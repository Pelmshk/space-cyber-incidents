# ============================================================================
# R/02_parse_and_clean.R
# Парсинг, стандартизация и очистка сырых OSINT-данных (v3.1)
# ============================================================================
# ИСПРАВЛЕНИЕ: добавлена дедупликация внутри каждого источника перед объединением

suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
  library(lubridate)
  library(fs)
  library(readr)
  library(digest)
  library(cli)
})

# ----------------------------------------------------------------------------
# 0. Конфигурация
# ----------------------------------------------------------------------------

RAW_DIR       <- "data/raw"
PROCESSED_DIR <- "data/processed"
dir.create(PROCESSED_DIR, recursive = TRUE, showWarnings = FALSE)

# ----------------------------------------------------------------------------
# 1. Вспомогательные функции
# ----------------------------------------------------------------------------

safe_col <- function(df, col_name, type = "character") {
  if (col_name %in% names(df)) {
    switch(type,
           "character" = as.character(df[[col_name]]),
           "Date"      = as.Date(df[[col_name]]),
           "POSIXct"   = as.POSIXct(df[[col_name]]),
           as.character(df[[col_name]]))
  } else {
    switch(type,
           "character" = rep(NA_character_, nrow(df)),
           "Date"      = as.Date(rep(NA_character_, nrow(df))),
           "POSIXct"   = as.POSIXct(rep(NA_character_, nrow(df))))
  }
}

clean_text <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x %>%
    as.character() %>%
    str_replace_all("<[^>]+>", " ") %>%
    str_replace_all("&nbsp;", " ") %>%
    str_replace_all("&amp;", "&") %>%
    str_replace_all("&quot;", "\"") %>%
    str_replace_all("&#39;", "'") %>%
    str_squish() %>%
    str_trunc(2000)
}

parse_date_robust <- function(x) {
  if (is.null(x) || length(x) == 0) return(as.POSIXct(NA, tz = "UTC"))
  x <- as.character(x)
  x <- str_trunc(x, 100)
  x <- str_squish(x)
  
  n <- length(x)
  result <- as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = "UTC")
  pending <- is.na(result) & !is.na(x) & str_length(x) > 0
  
  if (any(pending)) {
    iso <- str_extract(x[pending], "\\d{4}-\\d{2}-\\d{2}")
    parsed <- as.POSIXct(iso, format = "%Y-%m-%d", tz = "UTC")
    idx <- which(pending)
    ok <- !is.na(parsed)
    result[idx[ok]] <- parsed[ok]
    pending[idx[ok]] <- FALSE
  }
  
  if (any(pending)) {
    gdelt <- str_extract(x[pending], "\\d{8}T\\d{6}Z?")
    parsed <- as.POSIXct(gdelt, format = "%Y%m%dT%H%M%S", tz = "UTC")
    idx <- which(pending)
    ok <- !is.na(parsed)
    result[idx[ok]] <- parsed[ok]
    pending[idx[ok]] <- FALSE
  }
  
  if (any(pending)) {
    rfc <- x[pending]
    cleaned <- str_replace(rfc, "^[A-Za-z]{3},?\\s+", "")
    cleaned <- str_replace(cleaned, "\\s+[+-]\\d{4}\\s*$", "")
    cleaned <- str_replace(cleaned, "\\s+(GMT|UTC|EST|EDT|CST|PST|PDT|CEST|BST)\\s*$", "")
    parsed <- as.POSIXct(str_squish(cleaned), format = "%d %b %Y %H:%M:%S", tz = "UTC")
    idx <- which(pending)
    ok <- !is.na(parsed)
    result[idx[ok]] <- parsed[ok]
    pending[idx[ok]] <- FALSE
  }
  
  if (any(pending)) {
    parsed <- as.POSIXct(x[pending], format = "%b %d, %Y", tz = "UTC")
    idx <- which(pending)
    ok <- !is.na(parsed)
    result[idx[ok]] <- parsed[ok]
    pending[idx[ok]] <- FALSE
    
    parsed <- as.POSIXct(x[pending], format = "%B %d, %Y", tz = "UTC")
    idx <- which(pending)
    ok <- !is.na(parsed)
    result[idx[ok]] <- parsed[ok]
    pending[idx[ok]] <- FALSE
  }
  
  if (any(pending)) {
    parsed <- as.POSIXct(x[pending], format = "%d/%m/%Y", tz = "UTC")
    idx <- which(pending)
    ok <- !is.na(parsed)
    result[idx[ok]] <- parsed[ok]
    pending[idx[ok]] <- FALSE
  }
  
  if (any(pending)) {
    lub_parsed <- tryCatch({
      lubridate::parse_date_time(
        x[pending],
        orders = c("ymd", "Ymd", "dmy", "mdy", "ymd HMS", "dmy HMS"),
        quiet = TRUE, tz = "UTC"
      )
    }, error = function(e) as.POSIXct(rep(NA, sum(pending)), tz = "UTC"),
    warning = function(w) as.POSIXct(rep(NA, sum(pending)), tz = "UTC"))
    
    idx <- which(pending)
    ok <- !is.na(lub_parsed)
    result[idx[ok]] <- lub_parsed[ok]
  }
  
  result
}

extract_date_from_url <- function(urls) {
  if (is.null(urls) || length(urls) == 0) return(as.POSIXct(NA, tz = "UTC"))
  urls <- as.character(urls)
  
  d1 <- str_extract(urls, "\\d{4}[/\\-]\\d{2}[/\\-]\\d{2}")
  r1 <- as.POSIXct(str_replace_all(d1, "/", "-"), format = "%Y-%m-%d", tz = "UTC")
  
  d2 <- str_extract(urls, "\\d{8}")
  r2 <- as.POSIXct(d2, format = "%Y%m%d", tz = "UTC")
  
  d3 <- str_extract(urls, "\\d{4}[/\\-]\\d{2}(?![/\\-]\\d)")
  r3 <- as.POSIXct(paste0(str_replace_all(d3, "/", "-"), "-01"),
                   format = "%Y-%m-%d", tz = "UTC")
  
  result <- r1
  na_mask <- is.na(result) & !is.na(r2)
  result[na_mask] <- r2[na_mask]
  na_mask <- is.na(result) & !is.na(r3)
  result[na_mask] <- r3[na_mask]
  
  result
}

title_hash <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x %>%
    as.character() %>%
    str_to_lower() %>%
    str_squish() %>%
    str_replace_all("[^a-z0-9а-яё ]", "") %>%
    vapply(digest::digest, algo = "md5", FUN.VALUE = character(1))
}

infer_category <- function(raw_source, source_type, query_pattern) {
  n <- length(raw_source)
  category <- rep("mixed", n)
  category[raw_source == "NASA"] <- "space"
  
  rss_mask <- raw_source == "RSS"
  if (any(rss_mask)) {
    st <- tolower(source_type[rss_mask])
    category[rss_mask] <- case_when(
      str_detect(st, "hacker|bleeping|krebs|darkreading|securityweek|therecord|threatpost|cyberscoop") ~ "cyber",
      str_detect(st, "controlglobal|ics") ~ "ics",
      TRUE ~ "space"
    )
  }
  
  gdelt_mask <- raw_source == "GDELT"
  if (any(gdelt_mask)) {
    qp <- tolower(query_pattern[gdelt_mask])
    category[gdelt_mask] <- case_when(
      str_detect(qp, "cyber|hack|breach") ~ "cyber",
      str_detect(qp, "jamming|spoofing|interference") ~ "space",
      str_detect(qp, "supply.chain|counterfeit") ~ "supply_chain",
      TRUE ~ "mixed"
    )
  }
  
  cisa_mask <- raw_source == "CISA"
  if (any(cisa_mask)) {
    st <- tolower(source_type[cisa_mask])
    category[cisa_mask] <- case_when(
      str_detect(st, "ics") ~ "ics",
      str_detect(st, "kev") ~ "cyber",
      TRUE ~ "mixed"
    )
  }
  
  category
}

log_source_counts <- function(corpus, stage) {
  counts <- corpus %>% count(raw_source)
  n_rss   <- if ("RSS"   %in% counts$raw_source) counts$n[counts$raw_source == "RSS"]   else 0
  n_gdelt <- if ("GDELT" %in% counts$raw_source) counts$n[counts$raw_source == "GDELT"] else 0
  n_cisa  <- if ("CISA"  %in% counts$raw_source) counts$n[counts$raw_source == "CISA"]  else 0
  n_nasa  <- if ("NASA"  %in% counts$raw_source) counts$n[counts$raw_source == "NASA"]  else 0
  cli::cli_alert_info("{stage}: RSS={n_rss} | GDELT={n_gdelt} | CISA={n_cisa} | NASA={n_nasa} | Total={nrow(corpus)}")
  invisible(counts)
}

# ----------------------------------------------------------------------------
# 2. Загрузчики С ДЕДУПЛИКАЦИЕЙ ВНУТРИ КАЖДОГО ИСТОЧНИКА
# ----------------------------------------------------------------------------

empty_corpus <- function() {
  tibble(
    raw_source      = character(),
    source_type     = character(),
    query_pattern   = character(),
    category        = character(),
    title           = character(),
    source_url      = character(),
    published_date  = as.POSIXct(character()),
    description     = character(),
    snapshot_date   = as.Date(character()),
    title_hash      = character()
  )
}

load_rss <- function() {
  files <- dir_ls(path(RAW_DIR, "rss"), regexp = "\\.rds$")
  if (length(files) == 0) {
    cli::cli_alert_warning("RSS: файлы не найдены")
    return(empty_corpus())
  }
  cli::cli_alert_info("RSS: найдено {length(files)} файлов")
  
  all_rss <- map_dfr(files, ~ {
    df <- read_rds(.x)
    url_col <- if ("url" %in% names(df)) "url" else "link"
    date_col <- if ("pub_date" %in% names(df)) "pub_date" else
      if ("item_pubDate" %in% names(df)) "item_pubDate" else "pub_date"
    tibble(
      raw_source      = "RSS",
      source_type     = safe_col(df, "source_feed"),
      query_pattern   = NA_character_,
      title           = safe_col(df, "title"),
      source_url      = safe_col(df, url_col),
      published_date  = safe_col(df, date_col),
      description     = safe_col(df, "description"),
      snapshot_date   = as.Date(safe_col(df, "snapshot_date"))
    )
  })
  
  # ДЕДУПЛИКАЦИЯ ВНУТРИ RSS
  n_before <- nrow(all_rss)
  all_rss <- all_rss %>% distinct(source_url, .keep_all = TRUE)
  n_removed <- n_before - nrow(all_rss)
  if (n_removed > 0) {
    cli::cli_alert_success("RSS: удалено {n_removed} дубликатов по URL (внутри источника)")
  } else {
    cli::cli_alert_info("RSS: дубликатов по URL не найдено")
  }
  
  all_rss
}

load_gdelt <- function() {
  files <- dir_ls(path(RAW_DIR, "gdelt"), regexp = "\\.rds$")
  if (length(files) == 0) {
    cli::cli_alert_warning("GDELT: файлы не найдены")
    return(empty_corpus())
  }
  cli::cli_alert_info("GDELT: найдено {length(files)} файлов")
  
  all_gdelt <- map_dfr(files, ~ {
    df <- read_rds(.x)
    tibble(
      raw_source      = "GDELT",
      source_type     = safe_col(df, "source"),
      query_pattern   = safe_col(df, "query"),
      title           = safe_col(df, "title"),
      source_url      = safe_col(df, "url"),
      published_date  = safe_col(df, "date"),
      description     = paste0("[GDELT] lang=", safe_col(df, "language"),
                               "; country=", safe_col(df, "country")),
      snapshot_date   = as.Date(safe_col(df, "snapshot"))
    )
  })
  
  # ДЕДУПЛИКАЦИЯ ВНУТРИ GDELT
  n_before <- nrow(all_gdelt)
  all_gdelt <- all_gdelt %>% distinct(source_url, .keep_all = TRUE)
  n_removed <- n_before - nrow(all_gdelt)
  if (n_removed > 0) {
    cli::cli_alert_success("GDELT: удалено {n_removed} дубликатов по URL (внутри источника)")
  } else {
    cli::cli_alert_info("GDELT: дубликатов по URL не найдено")
  }
  
  all_gdelt
}

load_cisa <- function() {
  files <- dir_ls(path(RAW_DIR, "cisa"), regexp = "\\.rds$")
  if (length(files) == 0) {
    cli::cli_alert_warning("CISA: файлы не найдены")
    return(empty_corpus())
  }
  cli::cli_alert_info("CISA: найдено {length(files)} файлов")
  
  all_cisa <- map_dfr(files, ~ {
    df <- read_rds(.x)
    tibble(
      raw_source      = "CISA",
      source_type     = safe_col(df, "source"),
      query_pattern   = NA_character_,
      title           = safe_col(df, "title"),
      source_url      = safe_col(df, "url"),
      published_date  = safe_col(df, "pub_date"),
      description     = safe_col(df, "description"),
      snapshot_date   = as.Date(safe_col(df, "snapshot"))
    )
  })
  
  # ДЕДУПЛИКАЦИЯ ВНУТРИ CISA
  n_before <- nrow(all_cisa)
  all_cisa <- all_cisa %>% distinct(source_url, .keep_all = TRUE)
  n_removed <- n_before - nrow(all_cisa)
  if (n_removed > 0) {
    cli::cli_alert_success("CISA: удалено {n_removed} дубликатов по URL (внутри источника)")
  } else {
    cli::cli_alert_info("CISA: дубликатов по URL не найдено")
  }
  
  all_cisa
}

load_nasa <- function() {
  files <- dir_ls(path(RAW_DIR, "nasa"), regexp = "\\.rds$")
  if (length(files) == 0) {
    cli::cli_alert_warning("NASA: файлы не найдены")
    return(empty_corpus())
  }
  cli::cli_alert_info("NASA: найдено {length(files)} файлов")
  
  all_nasa <- map_dfr(files, ~ {
    df <- read_rds(.x)
    tibble(
      raw_source      = "NASA",
      source_type     = safe_col(df, "source"),
      query_pattern   = NA_character_,
      title           = safe_col(df, "title"),
      source_url      = safe_col(df, "url"),
      published_date  = safe_col(df, "pub_date"),
      description     = safe_col(df, "description"),
      snapshot_date   = as.Date(safe_col(df, "snapshot"))
    )
  })
  
  # ДЕДУПЛИКАЦИЯ ВНУТРИ NASA
  n_before <- nrow(all_nasa)
  all_nasa <- all_nasa %>% distinct(source_url, .keep_all = TRUE)
  n_removed <- n_before - nrow(all_nasa)
  if (n_removed > 0) {
    cli::cli_alert_success("NASA: удалено {n_removed} дубликатов по URL (внутри источника)")
  } else {
    cli::cli_alert_info("NASA: дубликатов по URL не найдено")
  }
  
  all_nasa
}

# ----------------------------------------------------------------------------
# 3. Главный пайплайн
# ----------------------------------------------------------------------------

main_clean <- function() {
  cli::cli_h1("Пайплайн очистки и стандартизации OSINT-корпуса (v3.1)")
  
  # --- Шаг 1. Загрузка с дедупликацией внутри источников ---
  cli::cli_h2("1. Загрузка сырых данных (с дедупликацией внутри источников)")
  df_rss   <- load_rss()
  df_gdelt <- load_gdelt()
  df_cisa  <- load_cisa()
  df_nasa  <- load_nasa()
  
  corpus <- bind_rows(df_rss, df_gdelt, df_cisa, df_nasa)
  cli::cli_alert_info("Всего записей после загрузки: {nrow(corpus)}")
  log_source_counts(corpus, "После загрузки")
  
  if (nrow(corpus) == 0) {
    cli::cli_abort("Корпус пуст. Проверьте, что шаг 01 отработал корректно.")
  }
  
  # --- Шаг 2. Очистка текста ---
  cli::cli_h2("2. Очистка текста")
  corpus <- corpus %>%
    mutate(
      title       = clean_text(title),
      description = clean_text(description),
      source_url  = str_trim(source_url)
    )
  
  # --- Шаг 3. Парсинг дат ---
  cli::cli_h2("3. Парсинг дат")
  corpus <- corpus %>%
    mutate(published_date = parse_date_robust(published_date))
  
  na_dates <- corpus %>% filter(is.na(published_date) & !is.na(title))
  if (nrow(na_dates) > 0) {
    cli::cli_alert_warning("Не распарсилось {nrow(na_dates)} дат")
  }
  
  # --- Шаг 3.1. Дозаполнение дат из URL ---
  cli::cli_h2("3.1. Дозаполнение дат из URL")
  n_before_na <- sum(is.na(corpus$published_date))
  corpus <- corpus %>%
    mutate(
      published_date = if_else(
        is.na(published_date),
        extract_date_from_url(source_url),
        published_date
      )
    )
  n_after_na <- sum(is.na(corpus$published_date))
  cli::cli_alert_success("Восстановлено дат из URL: {n_before_na - n_after_na}")
  cli::cli_alert_info("Осталось NA дат: {n_after_na}/{nrow(corpus)} ({round(100*n_after_na/nrow(corpus),1)}%)")
  
  na_by_source <- corpus %>%
    group_by(raw_source) %>%
    summarise(
      total = n(),
      with_date = sum(!is.na(published_date)),
      pct_with = round(100 * with_date / total, 1),
      .groups = "drop"
    )
  cli::cli_alert_info("Покрытие дат по источникам:")
  print(na_by_source)
  
  # --- Шаг 4. Эвристическая категоризация ---
  cli::cli_h2("4. Эвристическая категоризация")
  corpus <- corpus %>%
    mutate(category = infer_category(raw_source, source_type, query_pattern))
  
  # --- Шаг 5. Хэш заголовка ---
  cli::cli_h2("5. Вычисление хэшей заголовков")
  corpus <- corpus %>%
    mutate(title_hash = title_hash(title))
  
  # --- Шаг 6. Фильтрация мусора ---
  cli::cli_h2("6. Фильтрация некорректных записей")
  initial_count <- nrow(corpus)
  corpus <- corpus %>%
    filter(
      !is.na(source_url),
      !is.na(title),
      str_length(title) > 5,
      !str_detect(source_url, regex("example\\.com|test\\.com|localhost", ignore_case = TRUE))
    )
  cli::cli_alert_success("Удалено некорректных: {initial_count - nrow(corpus)}")
  
  # --- Шаг 7. Дедупликация между источниками ---
  cli::cli_h2("7. Дедупликация между источниками")
  n_before_url <- nrow(corpus)
  corpus <- corpus %>% distinct(source_url, .keep_all = TRUE)
  cli::cli_alert_success("Дубликатов по URL (между источниками): {n_before_url - nrow(corpus)}")
  
  n_before_hash <- nrow(corpus)
  corpus <- corpus %>% distinct(title_hash, snapshot_date, .keep_all = TRUE)
  cli::cli_alert_success("Семантических дубликатов: {n_before_hash - nrow(corpus)}")
  
  # --- Шаг 8. Сортировка ---
  corpus <- corpus %>% arrange(desc(published_date), raw_source)
  
  # --- Шаг 9. Сохранение ---
  cli::cli_h2("8. Сохранение корпуса")
  rds_path <- file.path(PROCESSED_DIR, "corpus_mentions_v1.rds")
  write_rds(corpus, rds_path)
  cli::cli_alert_success("RDS -> {rds_path}")
  
  csv_path <- file.path(PROCESSED_DIR, "corpus_mentions_v1.csv")
  write_csv(corpus, csv_path)
  cli::cli_alert_success("CSV -> {csv_path}")
  
  if (requireNamespace("arrow", quietly = TRUE)) {
    parquet_path <- file.path(PROCESSED_DIR, "corpus_mentions_v1.parquet")
    arrow::write_parquet(corpus, parquet_path)
    cli::cli_alert_success("Parquet -> {parquet_path}")
  }
  
  # --- Шаг 10. Финальная сводка ---
  cli::cli_h1("Сводка по источникам")
  by_source <- corpus %>%
    group_by(raw_source) %>%
    summarise(
      records    = n(),
      with_date  = sum(!is.na(published_date)),
      pct_date   = round(100 * with_date / records, 1),
      categories = paste(sort(unique(category)), collapse = ", "),
      .groups    = "drop"
    )
  print(by_source)
  
  cli::cli_h1("Временной диапазон")
  date_range <- corpus %>%
    summarise(
      min_date = format(min(published_date, na.rm = TRUE), "%Y-%m-%d"),
      max_date = format(max(published_date, na.rm = TRUE), "%Y-%m-%d")
    )
  print(date_range)
  
  cli::cli_alert_success("Пайплайн завершён. Файлы в: {PROCESSED_DIR}")
  invisible(corpus)
}

# ----------------------------------------------------------------------------
# 4. Запуск
# ----------------------------------------------------------------------------

results <- main_clean()