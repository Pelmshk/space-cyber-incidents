# ============================================================================
# R/03_verify_and_merge.R (v15 — комбинированная верификация)
# ============================================================================
# Изменения относительно v14:
# 1. Комбинированная верификация с учётом качества источников:
#    - CISA/NASA advisories → автоматически HIGH
#    - Наличие CVE/компаний в тексте → повышает уверенность
#    - Сохранены уровни SINGLE_MULTI_MENTION и SINGLE_MULTI_TYPE
# 2. Двухэтапная классификация с парсингом полного текста
# 3. Кластеризация по (target, year_month, vector)
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
  library(lubridate)
  library(rvest)
  library(httr2)
  library(cli)
  library(magrittr)
})

# ----------------------------------------------------------------------------
# 0. Конфигурация
# ----------------------------------------------------------------------------

PROCESSED_DIR <- "data/processed"
corpus_path <- file.path(PROCESSED_DIR, "corpus_mentions_v1.rds")

MIN_INDEPENDENT_SOURCES <- 2L
MIN_MENTIONS_SINGLE <- 3L
MIN_TYPES_SINGLE <- 2L
MAX_TEXT_LENGTH <- 5000
URL_TIMEOUT <- 10
PROGRESS_EVERY <- 10

if (!file.exists(corpus_path)) {
  stop("Файл корпуса не найден. Сначала запустите R/02_parse_and_clean.R")
}

corpus <- read_rds(corpus_path)
cat(sprintf("Загружен корпус: %d упоминаний\n", nrow(corpus)))

# ----------------------------------------------------------------------------
# 1. Фильтрация шума
# ----------------------------------------------------------------------------

cat("\n=== 1. Фильтрация нерелевантных упоминаний ===\n")

NOISE_PATTERNS <- c(
  "drone", "uav", "quadcopter", "wingcopter",
  "robot", "armed robot", "battlebot",
  "report.*released", "report.*publish", "white paper",
  "proposal", "budget", "funding.*propos",
  "conference", "summit", "event.*announc",
  "job opening", "career opportunity", "hiring",
  "product launch", "new feature", "update available"
)

n_before_noise <- nrow(corpus)
corpus <- corpus %>%
  filter(!str_detect(tolower(title), str_c(NOISE_PATTERNS, collapse = "|")))

cat(sprintf("  До фильтрации: %d\n", n_before_noise))
cat(sprintf("  После фильтрации: %d\n", nrow(corpus)))
cat(sprintf("  Удалено шумовых: %d\n\n", n_before_noise - nrow(corpus)))

if (nrow(corpus) == 0) stop("Корпус пуст после фильтрации шума.")

# ----------------------------------------------------------------------------
# 2. СЛОВАРИ И КЛАССИФИКАТОРЫ
# ----------------------------------------------------------------------------

TARGETS_DICT <- c(
  "Viasat", "Starlink", "Iridium", "Inmarsat", "Intelsat", "SES", "OneWeb",
  "GPS", "GLONASS", "Galileo", "BeiDou", "GNSS",
  "SpaceX", "Thales", "Airbus", "Boeing", "Lockheed", "Northrop",
  "NASA", "Roscosmos", "ESA", "ISRO", "CNSA", "JAXA",
  "Raytheon", "L3Harris", "Arianespace", "Blue Origin", "ULA",
  "Orion", "JWST", "Hubble", "Artemis", "ISS",
  "Satellite", "Spacecraft", "Payload", "Transponder", "Uplink", "Downlink",
  "Ground Station", "Earth Station", "Control Center",
  "Network", "Telecom", "ISP", "Infrastructure", "Critical Infrastructure",
  "Data Center", "Server", "Control System",
  "Signal", "Communication", "Broadcast",
  "Maritime", "Aviation", "Shipping", "Ship", "Vessel", "Port",
  "Aircraft", "Plane", "Flight", "Airport",
  "Military", "Defense", "Government", "NATO",
  "Army", "Navy", "Air Force", "Marines",
  "Ministry", "Agency", "Department",
  "Ukraine", "Russia", "China", "Iran", "North Korea", "Syria",
  "Europe", "Asia", "Africa", "America",
  "Energy", "Finance", "Healthcare", "Banking", "Insurance",
  "Telecommunications", "Utilities", "Transport",
  "Operator", "Provider", "Carrier", "Service",
  "Company", "Firm", "Corporation", "Organization",
  "System", "Platform", "Application",
  "Data", "Information", "Database",
  "User", "Customer", "Client", "Subscriber"
)

extract_target <- function(text) {
  if (is.na(text) || str_length(text) == 0) return("Unknown")
  text <- str_to_upper(text)
  matches <- str_extract_all(
    text,
    str_c("\\b", str_to_upper(TARGETS_DICT), "\\b", collapse = "|")
  )[[1]]
  if (length(matches) == 0) return("Unknown")
  paste(unique(matches), collapse = ", ")
}

classify_vector <- function(text) {
  if (is.na(text) || str_length(text) == 0) return("Unclassified")
  text <- str_to_lower(text)
  
  if (str_detect(text, "jam|spoof|interfer|electronic warfare|signal loss|gnss disruption|gps disruption")) {
    return("Electronic Warfare (Jamming/Spoofing)")
  }
  if (str_detect(text, "hack|breach|ransom|malware|phish|cyberattack|cyber.?attack|intrusion|ddos|deface|exploit|compromise")) {
    return("Cyber Intrusion")
  }
  if (str_detect(text, "supply.?chain|vendor|backdoor|third.?party|trojan|counterfeit")) {
    return("Supply Chain Compromise")
  }
  if (str_detect(text, "disrupt|denial|outage|service.?down|blocked|access.?denied|offline|failure|malfunction")) {
    return("Service Disruption / DoS")
  }
  if (str_detect(text, "leak|exfiltrat|disclosure|unauthorized.?access|data.?breach")) {
    return("Data Exfiltration")
  }
  if (str_detect(text, "insider|employee.?threat|credential|password|privilege")) {
    return("Insider Threat / Credential Compromise")
  }
  if (str_detect(text, "debris|collision|kinetic|asat|anti.?satellite")) {
    return("Kinetic/Physical (Out of Scope)")
  }
  if (str_detect(text, "vulnerab|cve|zero.?day|patch|exploit")) {
    return("Vulnerability Disclosure")
  }
  if (str_detect(text, "attack|strike|threat|incident")) {
    return("Cyber Attack (General)")
  }
  if (str_detect(text, "security|cyber.?threat|cyber.?risk|concern|issue|problem|risk|danger|hazard")) {
    return("Security Incident (General)")
  }
  if (str_detect(text, "alert|warning|advisory|notice")) {
    return("Security Advisory")
  }
  if (str_detect(text, "report|suspect|allege|claim|announce")) {
    return("Reported Incident")
  }
  
  "Unclassified"
}

classify_segment <- function(text, target) {
  if (is.na(text) || str_length(text) == 0) return("Unknown Segment")
  text   <- str_to_lower(text)
  target <- str_to_lower(target)
  combined <- paste(text, target)
  
  if (str_detect(combined, "maritime|ship|vessel|port|naval|ocean|sea")) {
    return("Maritime Segment")
  }
  if (str_detect(combined, "aviation|aircraft|plane|flight|airport|airline")) {
    return("Aviation Segment")
  }
  if (str_detect(combined, "ground|earth station|control center|control system|mission control|network|server|data center|telecom|operator|provider|isp|infrastructure")) {
    return("Ground Segment")
  }
  if (str_detect(combined, "user|terminal|receiver|customer|subscriber|handset|device|modem|client")) {
    return("User Segment")
  }
  if (str_detect(combined, "satellite|spacecraft|orbit|payload|transponder|uplink|downlink|iridium|intelsat|ses|oneweb|viasat|inmarsat|starlink|space")) {
    return("Space Segment")
  }
  if (str_detect(combined, "supply|vendor|manufacturer|component|firmware|hardware")) {
    return("Supply Chain")
  }
  if (str_detect(combined, "military|defense|army|navy|air force|marines|weapon")) {
    return("Military Segment")
  }
  if (str_detect(combined, "government|ministry|agency|department|official")) {
    return("Government Segment")
  }
  if (str_detect(target, "ukraine|russia|china|iran|military|defense|government|nato|europe|asia|africa|america")) {
    if (str_detect(combined, "satellite|space|gps|gnss")) {
      return("Space Segment (Targeted)")
    }
    if (str_detect(combined, "ground|network|infrastructure")) {
      return("Ground Segment (Targeted)")
    }
    if (str_detect(combined, "maritime|ship|naval")) {
      return("Maritime Segment (Targeted)")
    }
    if (str_detect(combined, "aviation|aircraft|flight")) {
      return("Aviation Segment (Targeted)")
    }
    return("Unknown Segment (Geopolitical Target)")
  }
  
  "Unknown Segment"
}

# ----------------------------------------------------------------------------
# 3. ЭТАП 1: Первичная классификация (title + description)
# ----------------------------------------------------------------------------

cat("=== 2. ЭТАП 1: Первичная классификация (title + description) ===\n")

corpus <- corpus %>%
  mutate(
    rich_text = paste0(
      coalesce(title, ""), " ",
      coalesce(description, "")
    ) %>% str_squish(),
    target  = map_chr(rich_text, extract_target),
    vector  = map_chr(rich_text, classify_vector),
    segment = map2_chr(rich_text, target, classify_segment)
  )

n_unk_target_1 <- sum(corpus$target == "Unknown")
n_unc_vector_1 <- sum(corpus$vector == "Unclassified")
n_unk_seg_1    <- sum(str_detect(corpus$segment, "^Unknown"))

cat(sprintf("  Unknown целей: %d (%.1f%%)\n", n_unk_target_1, 100*n_unk_target_1/nrow(corpus)))
cat(sprintf("  Unclassified векторов: %d (%.1f%%)\n", n_unc_vector_1, 100*n_unc_vector_1/nrow(corpus)))
cat(sprintf("  Unknown сегментов: %d (%.1f%%)\n\n", n_unk_seg_1, 100*n_unk_seg_1/nrow(corpus)))

# ----------------------------------------------------------------------------
# 4. ЭТАП 2: Парсинг полного текста (только для нераспознанных)
# ----------------------------------------------------------------------------

cat("=== 3. ЭТАП 2: Парсинг полного текста (только для нераспознанных) ===\n")

idx_unknown <- which(
  corpus$target == "Unknown" | 
    corpus$vector == "Unclassified" | 
    str_detect(corpus$segment, "^Unknown")
)

n_needs_full_text <- length(idx_unknown)
cat(sprintf("  Записей для парсинга: %d\n", n_needs_full_text))

if (n_needs_full_text > 0) {
  
  USER_AGENTS <- c(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:125.0) Gecko/20100101 Firefox/125.0",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.4 Safari/605.1.15",
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"
  )
  
  extract_full_text_with_progress <- function(urls, max_length = MAX_TEXT_LENGTH, timeout = URL_TIMEOUT) {
    n_urls <- length(urls)
    results <- character(n_urls)
    success_count <- 0
    error_count <- 0
    blocked_count <- 0
    
    cat(sprintf("  Начинаем парсинг %d URL...\n", n_urls))
    cat(sprintf("  Таймаут на URL: %d сек\n", timeout))
    cat(sprintf("  Прогресс обновляется каждые %d URL\n", PROGRESS_EVERY))
    cat(strrep("-", 60), "\n")
    
    start_time <- Sys.time()
    
    for (i in seq_along(urls)) {
      url <- urls[i]
      results[i] <- NA_character_
      
      if (i %% PROGRESS_EVERY == 0 || i == n_urls) {
        elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        avg_time <- if (i > 0) elapsed / i else 0
        remaining <- avg_time * (n_urls - i)
        pct <- round(100 * i / n_urls, 1)
        
        progress_msg <- paste0(
          "  [", i, "/", n_urls, "] (", pct, "%) | ",
          "Успех: ", success_count, " | Ошибок: ", error_count, " | ",
          "Заблокировано: ", blocked_count, " | ",
          "Прошло: ", round(elapsed, 0), "с | Осталось: ~", round(remaining, 0), "с"
        )
        cat(progress_msg, "\n")
        flush.console()
      }
      
      if (is.na(url) || url == "") {
        next
      }
      
      ua <- USER_AGENTS[(i %% length(USER_AGENTS)) + 1]
      
      tryCatch({
        resp <- request(url) |>
          req_headers(
            `User-Agent` = ua,
            `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
            `Accept-Language` = "en-US,en;q=0.5"
          ) |>
          req_timeout(timeout) |>
          req_error(is_error = function(resp) FALSE) |>
          req_perform()
        
        status <- resp_status(resp)
        
        if (i <= 5) {
          cat(sprintf("    URL #%d: статус %d\n", i, status))
        }
        
        if (status == 403) {
          blocked_count <- blocked_count + 1
          next
        }
        if (status >= 400) {
          error_count <- error_count + 1
          next
        }
        
        page <- resp_body_html(resp)
        
        text <- page |>
          html_elements("article, .article-body, .post-content, main, .entry-content, .content, .story-body") |>
          html_text2() |>
          paste(collapse = " ") |>
          str_squish() |>
          str_trunc(max_length)
        
        if (str_length(text) < 100) {
          text <- page |>
            html_elements("body") |>
            html_text2() |>
            str_squish() |>
            str_trunc(max_length)
        }
        
        if (str_length(text) >= 100) {
          results[i] <- text
          success_count <- success_count + 1
        } else {
          error_count <- error_count + 1
        }
        
      }, error = function(e) {
        error_count <<- error_count + 1
        if (i <= 5) {
          cat(sprintf("    URL #%d ОШИБКА: %s\n", i, e$message))
        }
      })
      
      Sys.sleep(0.3)
    }
    
    cat(strrep("-", 60), "\n")
    cat(sprintf("  Парсинг завершён! Успешно: %d, Ошибок: %d, Заблокировано: %d, Всего: %d\n",
                success_count, error_count, blocked_count, n_urls))
    
    results
  }
  
  full_texts <- extract_full_text_with_progress(corpus$source_url[idx_unknown])
  
  corpus$full_text <- NA_character_
  corpus$full_text[idx_unknown] <- full_texts
  
  n_extracted <- sum(!is.na(full_texts))
  cat(sprintf("  Извлечено полных текстов: %d/%d\n\n", n_extracted, n_needs_full_text))
  
  cat("  Повторная классификация с полным текстом...\n")
  
  corpus <- corpus %>%
    mutate(
      rich_text = paste0(
        coalesce(title, ""), " ",
        coalesce(description, ""), " ",
        coalesce(full_text, "")
      ) %>% str_squish(),
      target  = map_chr(rich_text, extract_target),
      vector  = map_chr(rich_text, classify_vector),
      segment = map2_chr(rich_text, target, classify_segment)
    )
  
  n_unk_target_2 <- sum(corpus$target == "Unknown")
  n_unc_vector_2 <- sum(corpus$vector == "Unclassified")
  n_unk_seg_2    <- sum(str_detect(corpus$segment, "^Unknown"))
  
  cat(sprintf("  После этапа 2:\n"))
  cat(sprintf("    Unknown целей: %d (%.1f%%) [было %d]\n", n_unk_target_2, 100*n_unk_target_2/nrow(corpus), n_unk_target_1))
  cat(sprintf("    Unclassified векторов: %d (%.1f%%) [было %d]\n", n_unc_vector_2, 100*n_unc_vector_2/nrow(corpus), n_unc_vector_1))
  cat(sprintf("    Unknown сегментов: %d (%.1f%%) [было %d]\n\n", n_unk_seg_2, 100*n_unk_seg_2/nrow(corpus), n_unk_seg_1))
  
} else {
  cat("  Все записи распознаны на этапе 1. Парсинг не нужен.\n\n")
  corpus$full_text <- NA_character_
}

# ----------------------------------------------------------------------------
# 5. Кластеризация по (target, year_month, vector)
# ----------------------------------------------------------------------------

cat("=== 4. Кластеризация по (Target, Year-Month, Vector) ===\n")

corpus <- corpus %>% mutate(year_month = format(published_date, "%Y-%m"))

safe_min_date <- function(x) {
  if (all(is.na(x))) return(NA_POSIXct_)
  min(x, na.rm = TRUE)
}

incidents <- corpus %>%
  group_by(target, year_month, vector) %>%
  summarise(
    n_mentions   = n(),
    n_titles     = n_distinct(title_hash),
    n_sources    = n_distinct(source_url),
    n_source_types = n_distinct(source_type),
    sources_list = paste(sort(unique(source_url)), collapse = ", "),
    source_types_list = paste(sort(unique(source_type)), collapse = ", "),
    # НОВОЕ: признаки качества
    has_official = any(raw_source %in% c("CISA", "NASA")),
    has_cve = any(str_detect(rich_text, "CVE-\\d{4}-\\d{4,7}")),
    has_company = any(str_detect(rich_text, "Viasat|Starlink|SpaceX|Boeing|Lockheed|Raytheon|Northrop|Airbus|Thales")),
    segment      = first(segment),
    titles       = paste(unique(title), collapse = " | "),
    first_seen   = safe_min_date(published_date),
    .groups      = "drop"
  )

cat(sprintf("  Всего кластеров: %d\n\n", nrow(incidents)))

# ----------------------------------------------------------------------------
# 6. КОМБИНИРОВАННАЯ ВЕРИФИКАЦИЯ
# ----------------------------------------------------------------------------

cat("=== 5. Комбинированная верификация ===\n")
cat("  Критерии верификации:\n")
cat("    HIGH: официальный источник (CISA/NASA) ИЛИ >=5 URL\n")
cat("    MEDIUM: 2-4 URL ИЛИ официальный + 1 URL\n")
cat("    LOW: 1 URL + CVE/компании ИЛИ 1 URL + 3+ упоминаний\n")
cat("    SINGLE_MULTI_TYPE: 1 URL + 2+ типа источника\n")
cat("    SINGLE_SOURCE: всё остальное\n\n")

verified_incidents <- incidents %>%
  mutate(
    confidence = case_when(
      # HIGH: официальный источник или много URL
      has_official & n_sources >= 1 ~ "HIGH",
      n_sources >= 5 ~ "HIGH",
      
      # MEDIUM: 2-4 URL или официальный + 1 URL
      n_sources >= 2 & n_sources < 5 ~ "MEDIUM",
      has_official & n_sources == 1 ~ "MEDIUM",
      
      # LOW: 1 URL + детали в тексте или много упоминаний
      n_sources == 1 & (has_cve | has_company) ~ "LOW",
      n_sources == 1 & n_mentions >= MIN_MENTIONS_SINGLE ~ "LOW",
      
      # SINGLE_MULTI_TYPE: 1 URL + разные типы источников
      n_sources == 1 & n_source_types >= MIN_TYPES_SINGLE ~ "SINGLE_MULTI_TYPE",
      
      # SINGLE_SOURCE: всё остальное
      TRUE ~ "SINGLE_SOURCE"
    ),
    is_verified = confidence %in% c("HIGH", "MEDIUM", "LOW", "SINGLE_MULTI_TYPE"),
    status = if_else(is_verified, "VERIFIED", "UNVERIFIED")
  ) %>%
  arrange(desc(n_sources), desc(n_mentions))

cat("  Распределение по уровням достоверности:\n")
print(verified_incidents %>% count(confidence, sort = TRUE))

final_db <- verified_incidents %>%
  filter(is_verified) %>%
  select(
    target, vector, segment, year_month,
    n_mentions, n_titles, n_sources, n_source_types,
    sources_list, source_types_list, first_seen, confidence,
    has_official, has_cve, has_company
  )

cat(sprintf("\n  Верифицированных инцидентов: %d\n\n", nrow(final_db)))

# Сравнение с простой верификацией
n_simple_verified <- sum(incidents$n_sources >= MIN_INDEPENDENT_SOURCES)
n_new_verified <- nrow(final_db)
cat(sprintf("  Было (только >=2 URL): %d\n", n_simple_verified))
cat(sprintf("  Стало (комбинированная): %d\n", n_new_verified))
cat(sprintf("  Прирост: +%d инцидентов (+%.1f%%)\n\n", 
            n_new_verified - n_simple_verified,
            100 * (n_new_verified - n_simple_verified) / max(1, n_simple_verified)))

# ----------------------------------------------------------------------------
# 7. Диагностика
# ----------------------------------------------------------------------------

cat("=== 6. Диагностика и сводка ===\n")

if (nrow(final_db) > 0) {
  n_unk_target <- sum(final_db$target == "Unknown")
  n_unc_vector <- sum(final_db$vector == "Unclassified")
  n_unk_seg    <- sum(str_detect(final_db$segment, "^Unknown"))
  
  cat(sprintf("  Unknown целей: %d (%.1f%%)\n", n_unk_target, 100*n_unk_target/nrow(final_db)))
  cat(sprintf("  Unclassified векторов: %d (%.1f%%)\n", n_unc_vector, 100*n_unc_vector/nrow(final_db)))
  cat(sprintf("  Unknown сегментов: %d (%.1f%%)\n\n", n_unk_seg, 100*n_unk_seg/nrow(final_db)))
  
  cat("  Топ-5 инцидентов по числу уникальных ссылок:\n")
  print(
    final_db %>%
      select(target, vector, segment, year_month, n_sources, confidence, has_official) %>%
      arrange(desc(n_sources)) %>%
      head(5),
    n = 5, width = Inf
  )
  
  cat("\n  Топ-5 целей (без Unknown):\n")
  print(final_db %>% filter(target != "Unknown") %>% count(target, sort = TRUE) %>% head(5))
  cat(sprintf("  Примечание: %d инцидентов с нераспознанной целью исключены из топа\n", n_unk_target))
  
  cat("\n  Распределение по векторам (без Unclassified):\n")
  print(final_db %>% filter(vector != "Unclassified") %>% count(vector, sort = TRUE))
  cat(sprintf("  Примечание: %d инцидентов без классификации вектора исключены из топа\n", n_unc_vector))
  
  cat("\n  Распределение по сегментам (без Unknown):\n")
  print(final_db %>% filter(!str_detect(segment, "^Unknown")) %>% count(segment, sort = TRUE))
  cat(sprintf("  Примечание: %d инцидентов с нераспознанным сегментом исключены из топа\n", n_unk_seg))
  
  cat("\n  Распределение по уровням достоверности в финальной базе:\n")
  print(final_db %>% count(confidence, sort = TRUE))
  
  cat("\n  Статистика по признакам качества:\n")
  cat(sprintf("    С официальным источником (CISA/NASA): %d (%.1f%%)\n", 
              sum(final_db$has_official), 100*sum(final_db$has_official)/nrow(final_db)))
  cat(sprintf("    С CVE: %d (%.1f%%)\n", 
              sum(final_db$has_cve), 100*sum(final_db$has_cve)/nrow(final_db)))
  cat(sprintf("    С упоминанием компаний: %d (%.1f%%)\n", 
              sum(final_db$has_company), 100*sum(final_db$has_company)/nrow(final_db)))
  
} else {
  cat("  Верифицированных инцидентов не найдено.\n")
}

# ----------------------------------------------------------------------------
# 8. Сохранение
# ----------------------------------------------------------------------------

cat("\n=== 7. Сохранение результатов ===\n")

write_rds(verified_incidents, file.path(PROCESSED_DIR, "incidents_all_clusters.rds"))
cat("  Все кластеры -> incidents_all_clusters.rds\n")

write_rds(final_db, file.path(PROCESSED_DIR, "incidents_verified_v1.rds"))
cat("  Верифицированные -> incidents_verified_v1.rds\n")

cat(sprintf("\nПайплайн завершён. Данные в: %s\n", PROCESSED_DIR))