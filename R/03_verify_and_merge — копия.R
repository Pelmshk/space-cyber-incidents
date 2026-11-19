# ============================================================================
# R/03_verify_and_merge.R
# Фильтрация шума, кластеризация, верификация и разметка инцидентов (v3)
# ============================================================================
# Изменения:
# 1. Адаптирован под схему корпуса из 02 (published_date = POSIXct)
# 2. Убрана ручная экспертная разметка крупнейшего кластера
# 3. Расширен словарь целей и классификаторы
# 4. Улучшена диагностика и вывод результатов
# 5. Убраны эмодзи
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
  library(lubridate)
  library(cli)
})

# ----------------------------------------------------------------------------
# 0. Конфигурация
# ----------------------------------------------------------------------------

PROCESSED_DIR <- "data/processed"
corpus_path <- file.path(PROCESSED_DIR, "corpus_mentions_v1.rds")

if (!file.exists(corpus_path)) {
  stop("Файл корпуса не найден. Сначала запустите R/02_parse_and_clean.R")
}

corpus <- read_rds(corpus_path)
cli::cli_alert_success("Загружен корпус: {nrow(corpus)} упоминаний")

# ----------------------------------------------------------------------------
# 1. Фильтрация нерелевантных упоминаний (шум)
# ----------------------------------------------------------------------------

cli::cli_h2("1. Фильтрация нерелевантных упоминаний (шум)")

NOISE_PATTERNS <- c(
  # Дроны и робототехника (не космические системы)
  "drone", "uav", "quadcopter", "wingcopter",
  "robot", "armed robot", "battlebot",
  # Документы и события (не инциденты)
  "report.*released", "report.*publish", "white paper",
  "proposal", "budget", "funding.*propos",
  "conference", "summit", "event.*announc",
  # Общие темы (не кибербезопасность)
  "job opening", "career opportunity", "hiring",
  "product launch", "new feature", "update available"
)

n_before_noise <- nrow(corpus)
corpus <- corpus %>%
  filter(
    !str_detect(tolower(title), str_c(NOISE_PATTERNS, collapse = "|"))
  )
n_removed_noise <- n_before_noise - nrow(corpus)

cli::cli_alert_info("До фильтрации: {n_before_noise} упоминаний")
cli::cli_alert_info("После фильтрации: {nrow(corpus)} упоминаний")
cli::cli_alert_success("Удалено шумовых: {n_removed_noise}")

if (nrow(corpus) == 0) {
  stop("Корпус пуст после фильтрации шума. Проверьте NOISE_PATTERNS.")
}

# ----------------------------------------------------------------------------
# 2. Извлечение сущностей (Цели / Targets)
# ----------------------------------------------------------------------------

cli::cli_h2("2. Извлечение целей (Targets)")

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

corpus <- corpus %>%
  mutate(target = map_chr(title, extract_target))

# Статистика по целям
target_stats <- corpus %>%
  count(target, sort = TRUE) %>%
  head(10)

cli::cli_alert_info("Топ-10 целей:")
print(target_stats)

# ----------------------------------------------------------------------------
# 3. Классификация вектора атаки (Vector)
# ----------------------------------------------------------------------------

cli::cli_h2("3. Классификация векторов атак")

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

corpus <- corpus %>%
  mutate(vector = map_chr(title, classify_vector))

# Статистика по векторам
vector_stats <- corpus %>%
  count(vector, sort = TRUE)

cli::cli_alert_info("Распределение векторов:")
print(vector_stats)

# ----------------------------------------------------------------------------
# 4. Классификация сегмента (Segment)
# ----------------------------------------------------------------------------

cli::cli_h2("4. Классификация сегментов")

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

corpus <- corpus %>%
  mutate(segment = map2_chr(title, target, classify_segment))

# Статистика по сегментам
segment_stats <- corpus %>%
  count(segment, sort = TRUE)

cli::cli_alert_info("Распределение сегментов:")
print(segment_stats)

# ----------------------------------------------------------------------------
# 5. Вычисление year_month и кластеризация
# ----------------------------------------------------------------------------

cli::cli_h2("5. Кластеризация по (Target, Year-Month, Vector)")

# published_date уже POSIXct из шага 02, просто форматируем
corpus <- corpus %>%
  mutate(
    year_month = format(published_date, "%Y-%m")
  )

# Статистика по year_month
ym_stats <- corpus %>%
  count(year_month, sort = TRUE)

cli::cli_alert_info("Распределение по месяцам:")
print(ym_stats)

# Кластеризация
safe_min_date <- function(x) {
  if (all(is.na(x))) return(NA_POSIXct_)
  min(x, na.rm = TRUE)
}

incidents <- corpus %>%
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

cli::cli_alert_success("Всего выявленных кластеров (потенциальных инцидентов): {nrow(incidents)}")

# ----------------------------------------------------------------------------
# 6. Верификация (правило двойного источника)
# ----------------------------------------------------------------------------

cli::cli_h2("6. Верификация (минимум 2 независимых издания/домена)")

verified_incidents <- incidents %>%
  mutate(
    is_verified = n_sources >= 2,
    status = ifelse(is_verified, "VERIFIED", "UNVERIFIED (Single Source)")
  ) %>%
  arrange(desc(n_sources), desc(n_mentions))

verified_summary <- verified_incidents %>%
  group_by(status) %>%
  summarise(count = n(), .groups = "drop")

cli::cli_alert_info("Результат верификации:")
print(verified_summary)

# Финальная база — только верифицированные инциденты
final_db <- verified_incidents %>%
  filter(is_verified) %>%
  select(
    target, vector, segment, year_month,
    n_mentions, n_sources, sources_list, first_seen
  )

cli::cli_alert_success("Верифицированных инцидентов в базе: {nrow(final_db)}")

# ----------------------------------------------------------------------------
# 7. Диагностика и вывод результатов
# ----------------------------------------------------------------------------

cli::cli_h2("7. Диагностика и сводка")

if (nrow(final_db) > 0) {
  # Топ-5 по медиа-охвату
  cli::cli_h3("Топ-5 инцидентов по охвату независимых СМИ")
  top5 <- final_db %>%
    select(target, vector, segment, year_month, n_sources, sources_list) %>%
    arrange(desc(n_sources)) %>%
    head(5) %>%
    mutate(
      target = str_wrap(target, width = 35),
      vector = str_wrap(vector, width = 30)
    )
  print(top5, n = 5, width = Inf)
  
  # Топ-5 целей
  cli::cli_h3("Топ-5 наиболее упоминаемых целей")
  final_db %>%
    count(target, sort = TRUE) %>%
    head(5) %>%
    print()
  
  # Распределение по векторам
  cli::cli_h3("Распределение по векторам атак")
  final_db %>%
    count(vector, sort = TRUE) %>%
    print()
  
  # Распределение по сегментам
  cli::cli_h3("Распределение по сегментам")
  final_db %>%
    count(segment, sort = TRUE) %>%
    print()
  
  # Временной диапазон
  cli::cli_h3("Временной диапазон")
  date_range <- final_db %>%
    summarise(
      min_date = format(min(first_seen, na.rm = TRUE), "%Y-%m-%d"),
      max_date = format(max(first_seen, na.rm = TRUE), "%Y-%m-%d")
    )
  print(date_range)
} else {
  cli::cli_alert_warning("Верифицированных инцидентов не найдено. База пуста.")
}

# ----------------------------------------------------------------------------
# 8. Сохранение результатов
# ----------------------------------------------------------------------------

cli::cli_h2("8. Сохранение результатов")

write_rds(verified_incidents, file.path(PROCESSED_DIR, "incidents_all_clusters.rds"))
cli::cli_alert_success("Все кластеры -> incidents_all_clusters.rds")

write_rds(final_db, file.path(PROCESSED_DIR, "incidents_verified_v1.rds"))
cli::cli_alert_success("Верифицированные инциденты -> incidents_verified_v1.rds")

cli::cli_alert_success("Данные сохранены в {PROCESSED_DIR}")