# ============================================================================
# R/01_collect_osint.R
# Сбор OSINT-данных об инцидентах ИБ в космических системах
# ============================================================================
# Источники:
#   1. RSS: SpaceNews, Spaceflight Now, Space Safety Magazine
#   2. GDELT API: событийные запросы по паттернам
#   3. CISA Advisories: уведомления об уязвимостях
#   4. NASA Advisories: космические инциденты
#   5. PDF-отчёты: cybersecurity reports (опционально)
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(httr2)
  library(rvest)
  library(jsonlite)
  library(tidyRSS)
  library(lubridate)
  library(readr)
  library(purrr)
})

# ----------------------------------------------------------------------------
# 0. Конфигурация
# ----------------------------------------------------------------------------

# Дата среза — ключ воспроизводимости
SNAPSHOT_DATE <- Sys.Date()
SNAPSHOT_TS   <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Директории
RAW_DIR <- "data/raw"
dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(RAW_DIR, "rss"),      showWarnings = FALSE)
dir.create(file.path(RAW_DIR, "gdelt"),    showWarnings = FALSE)
dir.create(file.path(RAW_DIR, "cisa"),     showWarnings = FALSE)
dir.create(file.path(RAW_DIR, "nasa"),     showWarnings = FALSE)
dir.create(file.path(RAW_DIR, "pdf_reports"), showWarnings = FALSE)
dir.create(file.path(RAW_DIR, "meta"),     showWarnings = FALSE)

# Поисковые паттерны для GDELT и новостных фильтров
SPACE_CYBER_PATTERNS <- c(
  "satellite hack",
  "satellite cyber",
  "satellite cyberattack",
  "satellite jamming",
  "satellite spoofing",
  "ground station cyber",
  "space cyber incident",
  "space system breach",
  "GNSS spoofing",
  "GPS jamming"
)

# RSS-фиды
RSS_FEEDS <- tibble(
  source = c("SpaceNews", "SpaceflightNow", "SpaceSafetyMagazine"),
  url = c(
    "https://spacenews.com/feed/",
    "https://spaceflightnow.com/feed/",
    "https://www.spacesafetymagazine.com/feed/"
  ),
  category = "news"
)

# Лог-функция
log_step <- function(step, status = "info", msg = "") {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cli::cli_alert_info("[{ts}] {step}: {msg}")
}

# ----------------------------------------------------------------------------
# 1. Сбор RSS
# ----------------------------------------------------------------------------

collect_rss <- function(feeds_tbl, snapshot_date) {
  log_step("RSS", "start", "Парсинг RSS-фидов")
  
  # Вспомогательная функция для безопасного парсинга и нормализации типов
  parse_feed <- function(url) {
    res <- safely(tidyRSS::tidyfeed)(url)
    
    # Если ошибка или пустой результат — возвращаем пустой tibble с нужной схемой
    if (!is.null(res$error) || is.null(res$result) || nrow(res$result) == 0) {
      return(tibble(
        title = character(), 
        link = character(), 
        pub_date = character(), 
        description = character()
      ))
    }
    
    df <- res$result
    
    # Извлекаем только нужные колонки и жестко приводим к character
    tibble(
      title       = as.character(df$item_title %||% NA),
      link        = as.character(df$item_link %||% NA),
      pub_date    = as.character(df$item_pubDate %||% NA),
      description = as.character(df$item_description %||% NA)
    )
  }
  
  results <- feeds_tbl |>
    mutate(
      # Применяем нашу безопасную функцию парсинга
      data = map(url, parse_feed)
    ) |>
    unnest(data) |>
    mutate(
      snapshot_date = snapshot_date,
      source_feed = source
    )
  
  # Сохраняем
  out_path <- file.path(RAW_DIR, "rss", 
                        sprintf("rss_raw_%s.rds", SNAPSHOT_TS))
  write_rds(results, out_path)
  log_step("RSS", "done", sprintf("Сохранено %d записей → %s", 
                                  nrow(results), out_path))
  invisible(results)
}

# ----------------------------------------------------------------------------
# 2. GDELT API
# ----------------------------------------------------------------------------

collect_gdelt <- function(patterns, snapshot_date, max_records = 100) {
  log_step("GDELT", "start", sprintf("%d поисковых паттернов", length(patterns)))
  
  base_url <- "https://api.gdeltproject.org/api/v2/doc/doc"
  
  fetch_pattern <- function(pattern, idx, total) {
    cat(sprintf("  → Паттерн %d/%d: '%s'\n", idx, total, pattern))
    
    # Функция с retry-логикой
    try_fetch <- function(attempt = 1, max_attempts = 2) {
      req <- request(base_url) |>
        req_url_query(
          query      = pattern,
          mode       = "ArtList",
          format     = "json",
          maxrecords = max_records,
          startdate  = format(snapshot_date - days(365), "%Y%m%dT000000Z"),
          enddate    = format(snapshot_date, "%Y%m%dT235959Z"),
          sort       = "DateDesc"
        ) |>
        req_headers(
          `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36",
          `Accept`     = "application/json"
        ) |>
        req_timeout(30)
      
      resp <- tryCatch(
        req_perform(req),
        error = function(e) {
          if (attempt < max_attempts && grepl("429", e$message)) {
            cat(sprintf("     429 — повторная попытка через 30 сек (попытка %d)\n", attempt + 1))
            Sys.sleep(30)
            return(try_fetch(attempt + 1, max_attempts))
          }
          log_step("GDELT", "warn", sprintf("Ошибка для '%s': %s", pattern, e$message))
          return(NULL)
        }
      )
      resp
    }
    
    resp <- try_fetch()
    if (is.null(resp)) return(tibble())
    
    body <- resp_body_json(resp, simplifyVector = TRUE)
    articles <- body$articles
    
    if (is.null(articles) || length(articles) == 0) {
      cat(sprintf("    ✓ Найдено 0 статей\n"))
      return(tibble())
    }
    
    cat(sprintf("    ✓ Найдено %d статей\n", length(articles$title)))
    
    tibble(
      title      = articles$title %||% NA_character_,
      url        = articles$url %||% NA_character_,
      source     = articles$domain %||% NA_character_,
      date       = articles$seendate %||% NA_character_,
      language   = articles$language %||% NA_character_,
      country    = articles$sourcecountry %||% NA_character_,
      query      = pattern,
      snapshot   = snapshot_date
    )
  }
  
  # Запуск с задержкой 15 секунд между запросами
  results <- map2_dfr(
    patterns,
    seq_along(patterns),
    ~ {
      if (.y > 1) {
        cat(sprintf("  ⏳ Ожидание 15 секунд...\n"))
        Sys.sleep(15)
      }
      fetch_pattern(.x, .y, length(patterns))
    }
  ) |>
    distinct(url, .keep_all = TRUE)
  
  out_path <- file.path(RAW_DIR, "gdelt",
                        sprintf("gdelt_raw_%s.rds", SNAPSHOT_TS))
  write_rds(results, out_path)
  log_step("GDELT", "done", sprintf("Сохранено %d уникальных статей → %s",
                                    nrow(results), out_path))
  invisible(results)
}

# ----------------------------------------------------------------------------
# 3. CISA Advisories
# ----------------------------------------------------------------------------

collect_cisa <- function(snapshot_date) {
  log_step("CISA", "start", "Парсинг новостей CISA")
  
  url <- "https://www.cisa.gov/news-events/news"
  
  # 1. Настраиваем запрос с браузерным User-Agent
  req <- request(url) |>
    req_headers(
      `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36",
      `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
    ) |>
    req_timeout(20)
  
  # 2. Выполняем запрос с обработкой ошибок
  resp <- tryCatch(
    req_perform(req),
    error = function(e) {
      log_step("CISA", "warn", sprintf("Не удалось загрузить CISA: %s", e$message))
      return(NULL)
    }
  )
  
  if (is.null(resp)) return(tibble())
  
  # 3. Извлекаем HTML из ответа httr2
  page <- httr2::resp_body_html(resp)
  
  # 4. Парсинг узлов (добавили более универсальные селекторы)
  nodes <- html_elements(page, ".news-item, .views-row, article, .field--name-title a") |>
    html_elements("a")
  
  # 5. Создаём tibble
  items <- tibble(
    title = html_text2(nodes),
    url   = html_attr(nodes, "href")
  ) |>
    filter(!is.na(title), !is.na(url), str_length(title) > 5) |>
    mutate(
      url = ifelse(str_starts(url, "/"),
                   paste0("https://www.cisa.gov", url), url),
      source = "CISA",
      snapshot = snapshot_date
    ) |>
    # Фильтр по космическим/ICS ключевым словам
    filter(str_detect(tolower(title), 
                      regex("satellite|space|ics|scada|gps|gnss|aerospace|cyber", 
                            ignore_case = TRUE))) |>
    distinct(url, .keep_all = TRUE)
  
  out_path <- file.path(RAW_DIR, "cisa",
                        sprintf("cisa_raw_%s.rds", SNAPSHOT_TS))
  write_rds(items, out_path)
  log_step("CISA", "done", sprintf("Сохранено %d записей → %s", 
                                   nrow(items), out_path))
  invisible(items)
}

# ----------------------------------------------------------------------------
# 4. NASA Advisories
# ----------------------------------------------------------------------------

collect_nasa <- function(snapshot_date) {
  log_step("NASA", "start", "Парсинг NASA News")
  
  news_url <- "https://www.nasa.gov/news/all-news/"
  
  page <- tryCatch(
    read_html(news_url),
    error = function(e) {
      log_step("NASA", "warn", "Не удалось загрузить NASA news")
      return(NULL)
    }
  )
  
  if (is.null(page)) return(tibble())
  
  # Сначала извлекаем узлы
  nodes <- html_elements(page, "h3 a, .card-title a, article a")
  
  # Затем создаём tibble с векторными данными
  items <- tibble(
    title = html_text2(nodes),
    url   = html_attr(nodes, "href")
  ) |>
    filter(!is.na(title), str_length(title) > 10) |>
    mutate(
      url = ifelse(str_starts(url, "/"),
                   paste0("https://www.nasa.gov", url), url),
      source = "NASA",
      snapshot = snapshot_date
    ) |>
    filter(str_detect(tolower(title),
                      regex("cyber|hack|breach|vulnerab|security|satellite",
                            ignore_case = TRUE))) |>
    distinct(url, .keep_all = TRUE)
  
  out_path <- file.path(RAW_DIR, "nasa",
                        sprintf("nasa_raw_%s.rds", SNAPSHOT_TS))
  write_rds(items, out_path)
  log_step("NASA", "done", sprintf("Сохранено %d записей → %s",
                                   nrow(items), out_path))
  invisible(items)
}

# ----------------------------------------------------------------------------
# 5. Метаданные среза (для воспроизводимости)
# ----------------------------------------------------------------------------

save_snapshot_meta <- function() {
  meta <- list(
    snapshot_date = SNAPSHOT_DATE,
    snapshot_ts   = SNAPSHOT_TS,
    r_version     = R.version.string,
    packages      = c(
      tidyverse = as.character(packageVersion("tidyverse")),
      httr2     = as.character(packageVersion("httr2")),
      rvest     = as.character(packageVersion("rvest")),
      tidyRSS   = as.character(packageVersion("tidyRSS"))
    ),
    patterns      = SPACE_CYBER_PATTERNS,
    rss_feeds     = RSS_FEEDS$url
  )
  
  out_path <- file.path(RAW_DIR, "meta",
                        sprintf("snapshot_meta_%s.json", SNAPSHOT_TS))
  write_json(meta, out_path, pretty = TRUE, auto_unbox = TRUE)
  log_step("META", "done", sprintf("Метаданные → %s", out_path))
  invisible(meta)
}

# ----------------------------------------------------------------------------
# 6. Главный пайплайн
# ----------------------------------------------------------------------------

main <- function() {
  log_step("PIPELINE", "start", sprintf("Срез данных: %s", SNAPSHOT_DATE))
  
  rss_data   <- collect_rss(RSS_FEEDS, SNAPSHOT_DATE)
  gdelt_data <- collect_gdelt(SPACE_CYBER_PATTERNS, SNAPSHOT_DATE)
  cisa_data  <- collect_cisa(SNAPSHOT_DATE)
  nasa_data  <- collect_nasa(SNAPSHOT_DATE)
  meta       <- save_snapshot_meta()
  
  # Сводка
  summary_tbl <- tibble(
    source = c("RSS", "GDELT", "CISA", "NASA"),
    records = c(nrow(rss_data), nrow(gdelt_data),
                nrow(cisa_data), nrow(nasa_data))
  )
  
  cat("\n")
  cli::cli_h1("Сводка сбора данных")
  print(summary_tbl)
  cli::cli_alert_success("Пайплайн завершён. Сырые данные в: %s", RAW_DIR)
  
  invisible(list(
    rss = rss_data, gdelt = gdelt_data,
    cisa = cisa_data, nasa = nasa_data,
    meta = meta
  ))
}

results <- main()