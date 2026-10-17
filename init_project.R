# ============================================================================
# Скрипт инициализации структуры проекта Space Cyber Incidents
# ============================================================================

# Базовая директория проекта
project_root <- "space-cyber-incidents"

# ----------------------------------------------------------------------------
# 1. Создание структуры директорий
# ----------------------------------------------------------------------------

directories <- c(
  "docs",
  "data/raw",
  "data/processed", 
  "data/external",
  "R/functions",
  "reports",
  "manuscript/figures",
  "output"
)

# Создаем все директории
for (dir in directories) {
  full_path <- file.path(project_root, dir)
  dir.create(full_path, recursive = TRUE, showWarnings = FALSE)
}

cat("✓ Структура директорий создана\n")

# ----------------------------------------------------------------------------
# 2. Создание файлов с шаблонами
# ----------------------------------------------------------------------------

# README.md
readme_content <- c(
  "# Space Cyber Incidents Database",
  "",
  "## Описание проекта",
  "",
  "Систематизация инцидентов информационной безопасности в космических системах на основе открытых данных (OSINT).",
  "",
  "## Цели",
  "",
  "- Создать структурированную базу данных инцидентов ИБ в космосе",
  "- Разработать типологию инцидентов",
  "- Проанализировать динамику и паттерны атак",
  "- Подготовить научно-аналитическую статью",
  "",
  "## Структура проекта",
  "",
  "- `docs/` - методология и справочники",
  "- `data/` - данные (raw, processed, external)",
  "- `R/` - скрипты пайплайна",
  "- `reports/` - Quarto отчеты",
  "- `manuscript/` - научная статья",
  "- `output/` - финальные артефакты",
  "",
  "## Запуск",
  "",
  "```r",
  "# Установить зависимости",
  "renv::restore()",
  "",
  "# Запустить пайплайн",
  "source('R/01_collect_osint.R')",
  "source('R/02_parse_and_clean.R')",
  "# ... и так далее",
  "```",
  "",
  "## Контакты",
  "",
  "Лаборатория 'РАДАР'"
)

writeLines(readme_content, file.path(project_root, "README.md"))

# .Rprofile
rprofile_content <- c(
  "# Настройки R для проекта",
  "",
  "# Скрыть API ключи из .env",
  "if (file.exists('.env')) {",
  "  readRenviron('.env')",
  "}",
  "",
  "# Настройки опций",
  "options(",
  "  stringsAsFactors = FALSE,",
  "  scipen = 999",
  ")"
)

writeLines(rprofile_content, file.path(project_root, ".Rprofile"))

# .gitignore
gitignore_content <- c(
  "# Данные",
  "data/raw/",
  "data/processed/",
  "output/*.sqlite",
  "output/*.parquet",
  "",
  "# R",
  ".Rhistory",
  ".RData",
  ".Rproj.user",
  "renv/library/",
  "renv/staging/",
  "",
  "# Quarto",
  "reports/*.html",
  "manuscript/*.pdf",
  "",
  "# OS",
  ".DS_Store",
  "Thumbs.db"
)

writeLines(gitignore_content, file.path(project_root, ".gitignore"))

# ----------------------------------------------------------------------------
# 3. Шаблоны для docs/
# ----------------------------------------------------------------------------

# inclusion_criteria.md
inclusion_content <- c(
  "# Критерии включения инцидентов в базу",
  "",
  "## Что считаем инцидентом ИБ в космической системе",
  "",
  "### ВКЛЮЧАЕМ:",
  "",
  "- Кибератаки на спутники (перехват управления, вмешательство в работу)",
  "- Атаки на наземные станции управления (DDoS, компрометация)",
  "- Атаки на сегмент связи (глушение, спуфинг сигналов)",
  "- Компрометация цепочек поставок (встроенные бэкдоры в оборудование)",
  "- Утечки данных через космические системы",
  "- Фишинг/социальная инженерия сотрудников операторов, приведшая к компрометации",
  "",
  "### НЕ ВКЛЮЧАЕМ:",
  "",
  "- Технические сбои без признаков злонамеренного воздействия",
  "- Помехи из-за природных явлений (солнечная активность)",
  "- Космический мусор и физические столкновения",
  "- Общие кибератаки на IT-инфраструктуру компаний (не специфичные для космоса)",
  "",
  "## Правило верификации",
  "",
  "Инцидент включается в базу только при наличии минимум **двух независимых источников**.",
  "",
  "Примеры надежных источников:",
  "- Официальные пресс-релизы операторов",
  "- Уведомления CISA/NASA",
  "- Публикации в SpaceNews, Space Safety Magazine",
  "- Отчеты исследовательских центров"
)

writeLines(inclusion_content, file.path(project_root, "docs/inclusion_criteria.md"))

# taxonomy_v1.0.md
taxonomy_content <- c(
  "# Таксономия инцидентов ИБ в космических системах v1.0",
  "",
  "## Сегменты космической системы",
  "",
  "- **Space** - космический сегмент (спутники, орбитальные станции)",
  "- **Ground** - наземный сегмент (станции управления, центры обработки)",
  "- **User** - пользовательский сегмент (терминалы, приемники)",
  "- **Supply Chain** - цепочка поставок (производство, логистика)",
  "",
  "## Векторы атак (согласование с MITRE ATT&CK)",
  "",
  "- Initial Access",
  "- Execution",
  "- Persistence",
  "- Privilege Escalation",
  "- Defense Evasion",
  "- Credential Access",
  "- Discovery",
  "- Lateral Movement",
  "- Collection",
  "- Command and Control",
  "- Exfiltration",
  "- Impact",
  "",
  "## Последствия",
  "",
  "- Loss of Control - потеря управления",
  "- Data Exfiltration - утечка данных",
  "- Denial of Service - отказ в обслуживании",
  "- Data Manipulation - манипуляция данными",
  "- Service Degradation - деградация сервиса",
  "- Reputation Damage - репутационный ущерб",
  "",
  "## Типы целей",
  "",
  "- Government/Military - государственные/военные",
  "- Commercial - коммерческие",
  "- Scientific - научные",
  "- Navigation - навигационные (GPS, ГЛОНАСС)",
  "- Communication - связные",
  "- Earth Observation - дистанционного зондирования"
)

writeLines(taxonomy_content, file.path(project_root, "docs/taxonomy_v1.0.md"))

# ----------------------------------------------------------------------------
# 4. Шаблоны для R/ скриптов
# ----------------------------------------------------------------------------

# 01_collect_osint.R
collect_template <- c(
  "# ============================================================================",
  "# 01_collect_osint.R - Сбор OSINT данных",
  "# ============================================================================",
  "",
  "library(tidyverse)",
  "library(rvest)",
  "library(jsonlite)",
  "library(httr2)",
  "",
  "# Дата среза",
  "snapshot_date <- Sys.Date()",
  "",
  "# ----------------------------------------------------------------------------",
  "# 1. Парсинг RSS (SpaceNews, Space Safety Magazine)",
  "# ----------------------------------------------------------------------------",
  "",
  "# TODO: Реализовать парсинг RSS",
  "# rss_urls <- c(",
  "#   'https://spacenews.com/feed/',",
  "#   'https://spacesafetymagazine.com/feed/'",
  "# )",
  "",
  "# ----------------------------------------------------------------------------",
  "# 2. GDELT API запросы",
  "# ----------------------------------------------------------------------------",
  "",
  "# TODO: Реализовать запросы к GDELT",
  "# query <- 'satellite AND (hack OR cyber OR jam OR spoof)'",
  "",
  "# ----------------------------------------------------------------------------",
  "# 3. CISA/NASA advisories",
  "# ----------------------------------------------------------------------------",
  "",
  "# TODO: Парсинг официальных уведомлений",
  "",
  "# ----------------------------------------------------------------------------",
  "# Сохранение сырых данных",
  "# ----------------------------------------------------------------------------",
  "",
  "# dir.create('data/raw', showWarnings = FALSE)",
  "# write_rss(raw_data, 'data/raw/rss_snapshot.rds')",
  "",
  "cat('✓ Сбор данных завершен:', snapshot_date, '\\n')",
  ""
)

writeLines(collect_template, file.path(project_root, "R/01_collect_osint.R"))

# 02_parse_and_clean.R
clean_template <- c(
  "# ============================================================================",
  "# 02_parse_and_clean.R - Очистка и структурирование данных",
  "# ============================================================================",
  "",
  "library(tidyverse)",
  "library(stringr)",
  "",
  "# TODO: Реализовать очистку данных",
  "# - Удаление дубликатов",
  "# - Нормализация дат",
  "# - Очистка текста",
  "",
  "cat('✓ Очистка данных завершена\\n')",
  ""
)

writeLines(clean_template, file.path(project_root, "R/02_parse_and_clean.R"))

# 03_verify_and_merge.R
verify_template <- c(
  "# ============================================================================",
  "# 03_verify_and_merge.R - Верификация и слияние инцидентов",
  "# ============================================================================",
  "",
  "library(tidyverse)",
  "",
  "# Правило: минимум 2 независимых источника",
  "",
  "# TODO: Реализовать логику верификации",
  "",
  "cat('✓ Верификация завершена\\n')",
  ""
)

writeLines(verify_template, file.path(project_root, "R/03_verify_and_merge.R"))

# 04_taxonomy_label.R
label_template <- c(
  "# ============================================================================",
  "# 04_taxonomy_label.R - Разметка по таксономии",
  "# ============================================================================",
  "",
  "library(tidyverse)",
  "",
  "# TODO: Применить таксономию из docs/taxonomy_v1.0.md",
  "",
  "cat('✓ Разметка завершена\\n')",
  ""
)

writeLines(label_template, file.path(project_root, "R/04_taxonomy_label.R"))

# 05_export_db.R
export_template <- c(
  "# ============================================================================",
  "# 05_export_db.R - Экспорт в БД",
  "# ============================================================================",
  "",
  "library(tidyverse)",
  "library(DBI)",
  "library(RSQLite)",
  "library(arrow)",
  "",
  "# TODO: Экспорт в SQLite и Parquet",
  "",
  "cat('✓ Экспорт завершен\\n')",
  ""
)

writeLines(export_template, file.path(project_root, "R/05_export_db.R"))

# 06_eda_and_viz.R
eda_template <- c(
  "# ============================================================================",
  "# 06_eda_and_viz.R - Разведочный анализ и визуализация",
  "# ============================================================================",
  "",
  "library(tidyverse)",
  "library(plotly)",
  "library(leaflet)",
  "",
  "# TODO: EDA анализ",
  "# - Динамика инцидентов по годам",
  "# - География атак",
  "# - Профили векторов",
  "",
  "cat('✓ EDA завершен\\n')",
  ""
)

writeLines(eda_template, file.path(project_root, "R/06_eda_and_viz.R"))

# ----------------------------------------------------------------------------
# 5. Шаблоны для reports/
# ----------------------------------------------------------------------------

# report.qmd
report_template <- c(
  "---",
  "title: \"Space Cyber Incidents Database - Технический отчет\"",
  "author: \"Лаборатория РАДАР\"",
  "date: \"`r Sys.Date()`\"",
  "format: html",
  "---",
  "",
  "## Описание пайплайна",
  "",
  "Описание этапов обработки данных...",
  "",
  "## Методология",
  "",
  "Критерии включения, таксономия, верификация...",
  "",
  "## Результаты EDA",
  "",
  "Графики и анализ...",
  "",
  "## Выводы",
  "",
  "Основные findings...",
  ""
)

writeLines(report_template, file.path(project_root, "reports/report.qmd"))

# exec-summary.qmd
exec_template <- c(
  "---",
  "title: \"Executive Summary - Space Cyber Incidents\"",
  "author: \"Лаборатория РАДАР\"",
  "date: \"`r Sys.Date()`\"",
  "format: html",
  "---",
  "",
  "## Ключевые выводы",
  "",
  "1. ...",
  "2. ...",
  "3. ...",
  "",
  "## Рекомендации",
  "",
  "...",
  "",
  "[Полный отчет](report.html)",
  ""
)

writeLines(exec_template, file.path(project_root, "reports/exec-summary.qmd"))

# ----------------------------------------------------------------------------
# 6. Шаблоны для manuscript/
# ----------------------------------------------------------------------------

article_template <- c(
  "---",
  "title: \"Systematization of Cybersecurity Incidents in Space Systems: An OSINT-Based Analysis\"",
  "author: \"Laboratory RADAR Team\"",
  "date: \"`r Sys.Date()`\"",
  "format: pdf",
  "bibliography: bibliography.bib",
  "---",
  "",
  "## Abstract",
  "",
  "...",
  "",
  "## Introduction",
  "",
  "...",
  "",
  "## Methods",
  "",
  "...",
  "",
  "## Results",
  "",
  "...",
  "",
  "## Discussion",
  "",
  "...",
  "",
  "## Conclusion",
  "",
  "...",
  "",
  "## References",
  ""
)

writeLines(article_template, file.path(project_root, "manuscript/article.qmd"))

# bibliography.bib
bib_template <- c(
  "@article{example2024,",
  "  title={Example citation},",
  "  author={Author, A.},",
  "  journal={Journal},",
  "  year={2024}",
  "}",
  ""
)

writeLines(bib_template, file.path(project_root, "manuscript/bibliography.bib"))

# ----------------------------------------------------------------------------
# 7. renv инициализация (опционально)
# ----------------------------------------------------------------------------

cat("\n")
cat("============================================================================\n")
cat("✓ Структура проекта успешно создана!\n")
cat("============================================================================\n")
cat("\n")
cat("Созданные директории:\n")
cat("  - docs/\n")
cat("  - data/raw/, data/processed/, data/external/\n")
cat("  - R/, R/functions/\n")
cat("  - reports/\n")
cat("  - manuscript/, manuscript/figures/\n")
cat("  - output/\n")
cat("\n")
cat("Созданные файлы:\n")
cat("  - README.md\n")
cat("  - .Rprofile, .gitignore\n")
cat("  - docs/inclusion_criteria.md, docs/taxonomy_v1.0.md\n")
cat("  - R/01_collect_osint.R ... R/06_eda_and_viz.R\n")
cat("  - reports/report.qmd, reports/exec-summary.qmd\n")
cat("  - manuscript/article.qmd, manuscript/bibliography.bib\n")
cat("\n")
cat("Следующие шаги:\n")
cat("  1. cd space-cyber-incidents\n")
cat("  2. renv::init()  # Инициализация renv\n")
cat("  3. Начать работу с R/01_collect_osint.R\n")
cat("\n")