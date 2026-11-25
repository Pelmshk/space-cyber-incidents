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
  stop("Файл верифицированных инцидентов не найден. Запустите R/03_verify_and_merge.R")
}

db <- read_rds(input_file)
cat(sprintf("Загружено %d верифицированных инцидентов\n", nrow(db)))

# ----------------------------------------------------------------------------
# 1. Подготовка схемы данных
# ----------------------------------------------------------------------------

cat("1. Нормализация схемы для БД...\n")

# Приводим имена колонок к snake_case
db_sql <- db %>%
  rename_with(~ str_replace_all(.x, " ", "_")) %>%
  rename_with(~ str_to_lower(.x)) %>%
  mutate(
    incident_id = row_number(),
    first_seen  = as.character(first_seen)
  ) %>%
  select(incident_id, everything())

# ----------------------------------------------------------------------------
# 2. Экспорт в SQLite
# ----------------------------------------------------------------------------

cat("2. Экспорт в SQLite...\n")
db_path <- file.path(OUTPUT_DIR, "space_cyber_db.sqlite")

if (file.exists(db_path)) file.remove(db_path)

con <- dbConnect(SQLite(), dbname = db_path)

# Основная таблица
dbWriteTable(con, "incidents", db_sql, overwrite = TRUE)

# Таблица источников (нормализация)
sources_tbl <- db_sql %>%
  select(incident_id, sources_list, n_sources) %>%
  separate_rows(sources_list, sep = ", ") %>%
  distinct()

dbWriteTable(con, "incident_sources", sources_tbl, overwrite = TRUE)

dbDisconnect(con)
cat(sprintf("   База данных создана: %s\n", db_path))

# ----------------------------------------------------------------------------
# 3. Экспорт в Parquet
# ----------------------------------------------------------------------------

cat("3. Экспорт в Parquet...\n")
parquet_path <- file.path(OUTPUT_DIR, "space_cyber_db.parquet")

db_parquet <- db_sql %>%
  mutate(first_seen = as.Date(first_seen))

write_parquet(db_parquet, parquet_path)
cat(sprintf("   Parquet файл создан: %s\n", parquet_path))

# ----------------------------------------------------------------------------
# 4. Экспорт в CSV
# ----------------------------------------------------------------------------

cat("4. Экспорт в CSV...\n")
csv_path <- file.path(OUTPUT_DIR, "space_cyber_db.csv")
write_csv(db_parquet, csv_path)
cat(sprintf("   CSV файл создан: %s\n", csv_path))

# ----------------------------------------------------------------------------
# 5. Валидация
# ----------------------------------------------------------------------------

cat("\n=== Схема базы данных (SQLite) ===\n")
con <- dbConnect(SQLite(), dbname = db_path)
tables <- dbListTables(con)
for (tbl in tables) {
  cat(sprintf("\nТаблица: %s\n", tbl))
  print(dbListFields(con, tbl))
}
dbDisconnect(con)

cat(sprintf("\nЭкспорт завершен. Файлы в директории: %s\n", OUTPUT_DIR))