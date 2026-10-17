# ============================================================================
# R/02_parse_and_clean.R
# Парсинг, стандартизация и очистка сырых OSINT-данных
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
  library(lubridate)
  library(fs)
})

# ----------------------------------------------------------------------------
# 0. Конфигурация
# ----------------------------------------------------------------------------

RAW_DIR <- "data/raw"
PROCESSED_DIR <- "data/processed"
dir.create(PROCESSED_DIR, recursive = TRUE, showWarnings = FALSE)

# ----------------------------------------------------------------------------
# 1. Вспомогательные функции очистки
# ----------------------------------------------------------------------------

clean_text <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x %>%
    str_replace_all("<[^>]+>", "") %>%
    str_replace_all("&nbsp;", " ") %>%
    str_replace_all("&amp;", "&") %>%
    str_replace_all("&quot;", "\"") %>%
    str_squish() %>%
    str_trunc(2000)
}

parse_date_robust <- function(x) {
  if (is.null(x) || length(x) == 0) return(as.Date(NA))
  as_date(parse_date_time(x, orders = c(
    "ymd HMS", "ymd", "dmy HMS", "dmy", "mdy HMS", "mdy", "ymdHMS", "YmdHMS"
  ), quiet = TRUE))
}

# ----------------------------------------------------------------------------
# 2. Загрузчики (ВСЕ даты как character, чтобы bind_rows не падал)
# ----------------------------------------------------------------------------

load_rss <- function() {
  files <- dir_ls(path(RAW_DIR, "rss"), regexp = "\\.rds$")
  if (length(files) == 0) { cat("   ⚠ RSS: файлы не найдены\n"); return(tibble()) }
  map_dfr(files, ~ {
    read_rds(.x) %>%
      transmute(
        source_type    = source_feed,
        title          = title,
        source_url     = link,
        published_date = as.character(pub_date),
        description    = as.character(description),
        snapshot_date  = snapshot_date,
        query_pattern  = NA_character_,
        raw_source     = "RSS"
      )
  })
}

load_gdelt <- function() {
  files <- dir_ls(path(RAW_DIR, "gdelt"), regexp = "\\.rds$")
  if (length(files) == 0) { cat("   ⚠ GDELT: файлы не найдены\n"); return(tibble()) }
  map_dfr(files, ~ {
    read_rds(.x) %>%
      transmute(
        source_type    = source,
        title          = title,
        source_url     = url,
        published_date = as.character(date),
        description    = NA_character_,
        snapshot_date  = snapshot,
        query_pattern  = query,
        raw_source     = "GDELT"
      )
  })
}

load_cisa <- function() {
  files <- dir_ls(path(RAW_DIR, "cisa"), regexp = "\\.rds$")
  if (length(files) == 0) { cat("   ⚠ CISA: файлы не найдены\n"); return(tibble()) }
  map_dfr(files, ~ {
    read_rds(.x) %>%
      transmute(
        source_type    = source,
        title          = title,
        source_url     = url,
        published_date = NA_character_,
        description    = NA_character_,
        snapshot_date  = snapshot,
        query_pattern  = NA_character_,
        raw_source     = "CISA"
      )
  })
}

load_nasa <- function() {
  files <- dir_ls(path(RAW_DIR, "nasa"), regexp = "\\.rds$")
  if (length(files) == 0) { cat("   ⚠ NASA: файлы не найдены\n"); return(tibble()) }
  map_dfr(files, ~ {
    read_rds(.x) %>%
      transmute(
        source_type    = source,
        title          = title,
        source_url     = url,
        published_date = NA_character_,
        description    = NA_character_,
        snapshot_date  = snapshot,
        query_pattern  = NA_character_,
        raw_source     = "NASA"
      )
  })
}

# ----------------------------------------------------------------------------
# 3. Главный пайплайн очистки
# ----------------------------------------------------------------------------

main_clean <- function() {
  cat("=== Запуск пайплайна очистки и стандартизации ===\n")
  
  # 1. Загрузка и объединение
  cat("1. Загрузка сырых данных...\n")
  df_rss   <- load_rss()
  df_gdelt <- load_gdelt()
  df_cisa  <- load_cisa()
  df_nasa  <- load_nasa()
  
  corpus <- bind_rows(df_rss, df_gdelt, df_cisa, df_nasa)
  cat(sprintf("   Всего сырых записей: %d\n", nrow(corpus)))
  
  if (nrow(corpus) == 0) {
    stop("❌ Корпус пуст. Проверьте, что шаг 01 отработал и файлы .rds есть в data/raw/")
  }
  
  # 2. Очистка текста и парсинг дат (теперь типы совпадают — все character)
  cat("2. Очистка текста и парсинг дат...\n")
  corpus <- corpus %>%
    mutate(
      title          = clean_text(title),
      description    = clean_text(description),
      published_date = parse_date_robust(published_date),
      source_url     = str_trim(source_url)
    )
  
  # 3. Фильтрация "мусора"
  cat("3. Фильтрация некорректных записей...\n")
  initial_count <- nrow(corpus)
  corpus <- corpus %>%
    filter(
      !is.na(source_url),
      str_length(title) > 5,
      !str_detect(source_url, regex("example\\.com|test\\.com", ignore_case = TRUE))
    )
  cat(sprintf("   Удалено некорректных записей: %d\n", initial_count - nrow(corpus)))
  
  # 4. Дедупликация по URL
  cat("4. Дедупликация по URL...\n")
  initial_count <- nrow(corpus)
  corpus <- corpus %>%
    distinct(source_url, .keep_all = TRUE)
  cat(sprintf("   Удалено дубликатов: %d\n", initial_count - nrow(corpus)))
  
  # 5. Сохранение результата
  cat("5. Сохранение корпуса...\n")
  out_path <- file.path(PROCESSED_DIR, "corpus_mentions_v1.rds")
  write_rds(corpus, out_path)
  
  cat("\n=== Сводка по источникам ===\n")
  corpus %>%
    group_by(raw_source) %>%
    summarise(records = n(), .groups = "drop") %>%
    print()
  
  cat(sprintf("\n✔ Корпус сохранен: %s\n", out_path))
  invisible(corpus)
}

# ----------------------------------------------------------------------------
# Запуск
# ----------------------------------------------------------------------------

results <- main_clean()