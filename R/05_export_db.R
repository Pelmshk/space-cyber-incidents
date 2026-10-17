# ============================================================================
# R/05_export_db.R
# Экспорт верифицированной базы инцидентов в SQLite, Parquet и CSV
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(DBI)
  library(RSQLite)
  library(arrow)
  library(fs)
})

# ----------------------------------------------------------------------------
# 0. Конфигурация
# ----------------------------------------------------------------------------

PROCESSED_DIR <- "data/processed"
OUTPUT_DIR    <- "output"
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

input_file <- file.path(PROCESSED_DIR, "incidents_verified_v1.rds")

if (!file.exists(input_file)) {
  stop("❌ Файл верифицированных инцидентов не найден. Запустите R/03_verify_and_merge.R")
}

db <- read_rds(input_file)
cat(sprintf("✔ Загружено %d верифицированных инцидентов\n", nrow(db)))

# ----------------------------------------------------------------------------
# 1. Подготовка схемы данных (Data Curation)
# ----------------------------------------------------------------------------

cat("1. Нормализация схемы для БД...\n")

# Приводим имена колонок к snake_case (стандарт для SQL)
db_sql <- db %>%
  rename_with(~ str_replace_all(.x, " ", "_")) %>%
  rename_with(~ str_to_lower(.x)) %>%
  mutate(
    incident_id = row_number(), # Генерируем уникальный ID
    first_seen  = as.character(first_seen) # SQLite плохо дружит с Date, конвертим в строку ISO
  ) %>%
  select(incident_id, everything()) # ID ставим первым

# ----------------------------------------------------------------------------
# 2. Экспорт в SQLite (Реляционная БД)
# ----------------------------------------------------------------------------

cat("2. Экспорт в SQLite...\n")
db_path <- file.path(OUTPUT_DIR, "space_cyber_db.sqlite")

# Удаляем старую БД, если есть
if (file.exists(db_path)) file.remove(db_path)

con <- dbConnect(SQLite(), dbname = db_path)

# Создаем основную таблицу инцидентов
dbWriteTable(con, "incidents", db_sql, overwrite = TRUE)

# Создаем справочную таблицу источников (нормализация)
sources_tbl <- db_sql %>%
  select(incident_id, sources_list, n_sources) %>%
  separate_rows(sources_list, sep = ", ") %>%
  distinct()

dbWriteTable(con, "incident_sources", sources_tbl, overwrite = TRUE)

dbDisconnect(con)
cat(sprintf("   ✔ База данных создана: %s\n", db_path))

# ----------------------------------------------------------------------------
# 3. Экспорт в Parquet (Колоночный формат для быстрого анализа)
# ----------------------------------------------------------------------------

cat("3. Экспорт в Parquet...\n")
parquet_path <- file.path(OUTPUT_DIR, "space_cyber_db.parquet")

# Возвращаем даты в формат Date для Parquet/Arrow
db_parquet <- db_sql %>%
  mutate(first_seen = as.Date(first_seen))

write_parquet(db_parquet, parquet_path)
cat(sprintf("   ✔ Parquet файл создан: %s\n", parquet_path))

# ----------------------------------------------------------------------------
# 4. Экспорт в CSV (Универсальный обмен)
# ----------------------------------------------------------------------------

cat("4. Экспорт в CSV...\n")
csv_path <- file.path(OUTPUT_DIR, "space_cyber_db.csv")
write_csv(db_parquet, csv_path)
cat(sprintf("   ✔ CSV файл создан: %s\n", csv_path))

# ----------------------------------------------------------------------------
# 5. Валидация и Сводка
# ----------------------------------------------------------------------------

cat("\n=== Схема базы данных (SQLite) ===\n")
con <- dbConnect(SQLite(), dbname = db_path)
tables <- dbListTables(con)
for (tbl in tables) {
  cat(sprintf("\nТаблица: %s\n", tbl))
  print(dbListFields(con, tbl))
}
dbDisconnect(con)

cat(sprintf("\n✔ Экспорт завершен. Файлы в директории: %s\n", OUTPUT_DIR))