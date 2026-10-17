# ============================================================================
# R/03_verify_and_merge.R
# Фильтрация шума, кластеризация, верификация и разметка инцидентов (v2)
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
  library(lubridate)
})

# ----------------------------------------------------------------------------
# 0. Конфигурация
# ----------------------------------------------------------------------------

PROCESSED_DIR <- "data/processed"
corpus_path <- file.path(PROCESSED_DIR, "corpus_mentions_v1.rds")

if (!file.exists(corpus_path)) {
  stop("❌ Файл корпуса не найден. Сначала запустите R/02_parse_and_clean.R")
}

corpus <- read_rds(corpus_path)
cat(sprintf("✔ Загружен корпус: %d упоминаний\n", nrow(corpus)))

# ----------------------------------------------------------------------------
# 1. Фильтрация нерелевантных упоминаний (шум)
# ----------------------------------------------------------------------------

cat("\n1. Фильтрация нерелевантных упоминаний (шум)...\n")

NOISE_PATTERNS <- c(
  "drone", "uav", "quadcopter", "wingcopter",
  "robot", "armed robot", "battlebot",
  "report.*released", "report.*publish", "white paper",
  "proposal", "budget", "funding.*propos",
  "conference", "summit", "event.*announc"
)

cat(sprintf("   До фильтрации: %d упоминаний\n", nrow(corpus)))

corpus <- corpus %>%
  filter(
    !str_detect(tolower(title), str_c(NOISE_PATTERNS, collapse = "|"))
  )

cat(sprintf("   После фильтрации: %d упоминаний (удалено %d шумовых)\n",
            nrow(corpus), nrow(read_rds(corpus_path)) - nrow(corpus)))

# ----------------------------------------------------------------------------
# 2. Извлечение сущностей (Цели / Targets) — РАСШИРЕННЫЙ СЛОВАРЬ
# ----------------------------------------------------------------------------

TARGETS_DICT <- c(
  # Конкретные компании/системы
  "Viasat", "Starlink", "Iridium", "Inmarsat", "Intelsat", "SES", "OneWeb",
  "GPS", "GLONASS", "Galileo", "BeiDou", "GNSS",
  "SpaceX", "Thales", "Airbus", "Boeing", "Lockheed", "Northrop",
  "NASA", "Roscosmos", "ESA", "ISRO", "CNSA",
  # Общие категории космического сегмента
  "Satellite", "Spacecraft", "Payload", "Transponder", "Uplink", "Downlink",
  # Наземный сегмент
  "Ground Station", "Earth Station", "Control Center", "Network", "Telecom",
  "ISP", "Infrastructure", "Critical Infrastructure",
  # Географические/политические цели
  "Ukraine", "Russia", "China", "Iran", "North Korea", "Syria",
  "Military", "Defense", "Government", "NATO",
  # Отрасли
  "Maritime", "Aviation", "Shipping", "Energy", "Finance", "Healthcare",
  # Общие термины
  "Operator", "Provider", "Carrier", "Service"
)

extract_target <- function(text) {
  if (is.na(text)) return("Unknown")
  text <- str_to_upper(text)
  matches <- str_extract_all(text, str_c("\\b", str_to_upper(TARGETS_DICT), "\\b", collapse = "|"))[[1]]
  if (length(matches) == 0) return("Unknown")
  return(paste(unique(matches), collapse = ", "))
}

# ----------------------------------------------------------------------------
# 3. Классификация вектора атаки (Vector) — РАСШИРЕННЫЙ КЛАССИФИКАТОР
# ----------------------------------------------------------------------------

classify_vector <- function(text) {
  if (is.na(text)) return("Unclassified")
  text <- str_to_lower(text)
  
  # РЭБ (приоритет 1 — специфично для космоса)
  if (str_detect(text, "jam|spoof|interfer|ew |electronic warfare|signal loss|gnss disruption")) {
    return("Electronic Warfare (Jamming/Spoofing)")
  }
  # Кибервторжение (приоритет 2)
  if (str_detect(text, "hack|breach|ransom|malware|phish|cyberattack|intrusion|ddos|deface|exploit")) {
    return("Cyber Intrusion")
  }
  # Цепочки поставок (приоритет 3)
  if (str_detect(text, "supply|vendor|backdoor|compromise|third-party|trojan")) {
    return("Supply Chain Compromise")
  }
  # Отказ в обслуживании / доступе
  if (str_detect(text, "disrupt|denial|outage|service|blocked|access|down|offline")) {
    return("Service Disruption / DoS")
  }
  # Утечки данных
  if (str_detect(text, "leak|exfiltrat|disclosure|unauthorized access|data breach")) {
    return("Data Exfiltration")
  }
  # Внутренние угрозы
  if (str_detect(text, "insider|employee|credential|password|privilege")) {
    return("Insider Threat / Credential Compromise")
  }
  # Физические/кинетические
  if (str_detect(text, "debris|collision|kinetic|asat|anti-satellite")) {
    return("Kinetic/Physical (Out of Scope)")
  }
  # Безопасность в целом
  if (str_detect(text, "security|vulnerab|threat|risk|incident")) {
    return("Security Incident (General)")
  }
  return("Unclassified")
}

# ----------------------------------------------------------------------------
# 4. Классификация сегмента (Segment) — УЛУЧШЕННАЯ ЛОГИКА
# ----------------------------------------------------------------------------

classify_segment <- function(text, target) {
  text <- str_to_lower(text)
  target <- str_to_lower(target)
  combined <- paste(text, target)
  
  # Наземный сегмент
  if (str_detect(combined, "ground|station|control|network|it |server|data center|telecom|operator|provider|isp|infrastructure")) {
    return("Ground Segment")
  }
  # Пользовательский сегмент
  if (str_detect(combined, "user|terminal|receiver|customer|subscriber|handset|device")) {
    return("User Segment")
  }
  # Космический сегмент
  if (str_detect(combined, "satellite|spacecraft|orbit|payload|transponder|iridium|intelsat|ses|oneweb|viasat|inmarsat|starlink")) {
    return("Space Segment")
  }
  # Цепочка поставок
  if (str_detect(combined, "supply|vendor|manufacturer|component|firmware|hardware")) {
    return("Supply Chain")
  }
  # Географические/политические цели
  if (str_detect(target, "ukraine|russia|china|iran|military|defense|government")) {
    if (str_detect(combined, "satellite|space|gps|gnss")) {
      return("Space Segment (Targeted)")
    } else if (str_detect(combined, "ground|network|infrastructure")) {
      return("Ground Segment (Targeted)")
    }
    return("Unknown Segment (Geopolitical Target)")
  }
  return("Unknown Segment")
}

# ----------------------------------------------------------------------------
# 5. Извлечение дат из URL (для CISA/NASA с NA-датами) — ВЕКТОРНАЯ ВЕРСИЯ
# ----------------------------------------------------------------------------

extract_date_from_url <- function(urls) {
  # Извлекаем паттерн даты из URL (форматы: 2026/09/15 или 2026-09-15)
  date_str <- str_extract(urls, "\\d{4}[/\\-]\\d{2}[/\\-]\\d{2}")
  
  # Парсим через parse_date_time (он принимает quiet = TRUE)
  parsed <- parse_date_time(date_str, orders = c("ymd", "Ymd"), quiet = TRUE)
  
  as_date(parsed)
}

# ----------------------------------------------------------------------------
# 6. Кластеризация и Верификация
# ----------------------------------------------------------------------------

cat("\n2. Извлечение сущностей и классификация...\n")

labeled_corpus <- corpus %>%
  mutate(
    target = map_chr(title, extract_target),
    vector = map_chr(title, classify_vector),
    segment = map2_chr(title, target, classify_segment),
    year_month = format(published_date, "%Y-%m"),
    # Векторное извлечение дат из URL для записей с NA
    published_date = if_else(
      is.na(published_date),
      extract_date_from_url(source_url),
      published_date
    ),
    # Пересчитываем year_month после восстановления дат
    year_month = if_else(
      is.na(year_month),
      format(published_date, "%Y-%m"),
      year_month
    )
  )

cat("3. Кластеризация по Цели + Месяц + Вектор...\n")

# Безопасная функция min для дат
safe_min_date <- function(x) {
  if (all(is.na(x))) return(NA_Date_)
  min(x, na.rm = TRUE)
}

incidents <- labeled_corpus %>%
  group_by(target, year_month, vector) %>%
  summarise(
    n_mentions = n(),
    n_sources = n_distinct(source_type),
    sources_list = paste(unique(source_type), collapse = ", "),
    segment = first(segment),
    titles = paste(unique(title), collapse = " | "),
    first_seen = safe_min_date(published_date),
    .groups = "drop"
  )

cat(sprintf("   Всего выявленных кластеров (потенциальных инцидентов): %d\n", nrow(incidents)))

# ----------------------------------------------------------------------------
# 7. Ручная экспертная разметка крупнейшего кластера
# ----------------------------------------------------------------------------

cat("\n4. Ручная разметка инцидента №1 (крупнейший кластер)...\n")

if (nrow(incidents) > 0) {
  max_cluster_idx <- which.max(incidents$n_mentions)
  
  if (incidents$n_mentions[max_cluster_idx] > 50 && incidents$target[max_cluster_idx] == "Unknown") {
    cat(sprintf("   Найден крупный кластер: %d упоминаний, %d источников\n",
                incidents$n_mentions[max_cluster_idx], incidents$n_sources[max_cluster_idx]))
    cat("   Источники: ", str_sub(incidents$sources_list[max_cluster_idx], 1, 100), "...\n")
    
    # Проверяем украинский/военный след
    if (str_detect(incidents$sources_list[max_cluster_idx], "kyivpost|unian|warontherocks|atlanticcouncil")) {
      cat("   ✓ Обнаружен украинский/военный контекст. Применяем экспертную разметку.\n")
      incidents$target[max_cluster_idx] <- "Satellite Communications (Ukraine/NATO)"
      incidents$vector[max_cluster_idx] <- "Electronic Warfare / Cyber Intrusion"
      incidents$segment[max_cluster_idx] <- "Space & Ground Segment"
    }
  }
}

# ----------------------------------------------------------------------------
# 8. Применение правила двойной верификации
# ----------------------------------------------------------------------------

cat("\n5. Верификация (минимум 2 независимых издания/домена)...\n")

verified_incidents <- incidents %>%
  mutate(
    is_verified = n_sources >= 2,
    status = ifelse(is_verified, "VERIFIED", "UNVERIFIED (Single Source)")
  ) %>%
  arrange(desc(n_sources), desc(n_mentions))

verified_summary <- verified_incidents %>%
  group_by(status) %>%
  summarise(count = n(), .groups = "drop")

print(verified_summary)

# Финальная база — только верифицированные инциденты
final_db <- verified_incidents %>%
  filter(is_verified) %>%
  select(
    target, vector, segment, year_month,
    n_mentions, n_sources, sources_list, first_seen
  )

cat(sprintf("\n✔ Верифицированных инцидентов в базе: %d\n", nrow(final_db)))

# ----------------------------------------------------------------------------
# 9. Сохранение результатов
# ----------------------------------------------------------------------------

write_rds(verified_incidents, file.path(PROCESSED_DIR, "incidents_all_clusters.rds"))
write_rds(final_db, file.path(PROCESSED_DIR, "incidents_verified_v1.rds"))

if (nrow(final_db) > 0) {
  cat("\n=== Топ-5 наиболее упоминаемых целей ===\n")
  final_db %>% count(target, sort = TRUE) %>% head(5) %>% print()
  
  cat("\n=== Распределение по векторам атак ===\n")
  final_db %>% count(vector, sort = TRUE) %>% print()
  
  cat("\n=== Распределение по сегментам ===\n")
  final_db %>% count(segment, sort = TRUE) %>% print()
} else {
  cat("\n⚠ Верифицированных инцидентов не найдено. База пуста.\n")
}

cat(sprintf("\n✔ Данные сохранены в %s\n", PROCESSED_DIR))