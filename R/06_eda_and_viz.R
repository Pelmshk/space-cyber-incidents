# ============================================================================
# R/06_eda_and_viz.R
# Разведочный анализ (EDA) и визуализация верифицированной базы инцидентов
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(ggplot2)
  library(stringr)
})

# ----------------------------------------------------------------------------
# 0. Конфигурация и загрузка данных
# ----------------------------------------------------------------------------

PROCESSED_DIR <- "data/processed"
OUTPUT_DIR    <- "output"
FIGURES_DIR   <- file.path(OUTPUT_DIR, "figures")
dir.create(FIGURES_DIR, recursive = TRUE, showWarnings = FALSE)

# Загружаем верифицированную базу (RDS сохраняет типы данных корректно)
db_path <- file.path(PROCESSED_DIR, "incidents_verified_v1.rds")

if (!file.exists(db_path)) {
  stop("❌ Файл incidents_verified_v1.rds не найден. Запустите R/03_verify_and_merge.R и R/05_export_db.R")
}

db <- read_rds(db_path)

cat(sprintf("✔ Загружено %d верифицированных инцидентов для анализа\n\n", nrow(db)))

# ----------------------------------------------------------------------------
# 1. Ключевые метрики (KPI) для Executive Summary
# ----------------------------------------------------------------------------

cat("=== КЛЮЧЕВЫЕ МЕТРИКИ (для Executive Summary) ===\n")
cat(sprintf("• Всего верифицированных инцидентов: %d\n", nrow(db)))
cat(sprintf("• Уникальных целей (Targets): %d\n", length(unique(db$target))))
cat(sprintf("• Уникальных векторов атак: %d\n", length(unique(db$vector))))
cat(sprintf("• Период наблюдения: %s — %s\n", min(db$year_month, na.rm=TRUE), max(db$year_month, na.rm=TRUE)))
cat(sprintf("• Максимальный медиа-охват одного инцидента: %d источников\n\n", max(db$n_sources)))

# ----------------------------------------------------------------------------
# 2. График 1: Динамика инцидентов по месяцам (Timeline)
# ----------------------------------------------------------------------------

cat("1. Генерация графика: Динамика инцидентов...\n")

p_timeline <- db %>%
  count(year_month, name = "count") %>%
  mutate(year_month = as.Date(paste0(year_month, "-01"))) %>% 
  ggplot(aes(x = year_month, y = count)) +
  geom_col(fill = "#2c3e50", width = 20, alpha = 0.9) +
  geom_text(aes(label = count), vjust = -0.5, size = 3.5, fontface = "bold") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month", expand = expansion(mult = c(0.05, 0.1))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal() +
  labs(
    title = "Рис. 1. Динамика верифицированных инцидентов ИБ в космических системах",
    subtitle = "Количество инцидентов по месяцам (на основе OSINT-корпуса)",
    x = "Месяц", y = "Количество инцидентов"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 12, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 10, color = "gray40", margin = margin(b = 15)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    panel.grid.major.x = element_blank()
  )

ggsave(file.path(FIGURES_DIR, "fig1_timeline.png"), p_timeline, width = 8, height = 4, dpi = 300)

# ----------------------------------------------------------------------------
# 3. График 2: Распределение векторов атак (Vector Distribution)
# ----------------------------------------------------------------------------

cat("2. Генерация графика: Векторы атак...\n")

p_vectors <- db %>%
  count(vector, sort = TRUE) %>%
  mutate(
    vector = fct_reorder(vector, n), 
    vector = str_wrap(vector, width = 30) # Перенос длинных названий
  ) %>%
  ggplot(aes(x = n, y = vector, fill = vector)) +
  geom_col(show.legend = FALSE, alpha = 0.85) +
  geom_text(aes(label = n), hjust = -0.3, size = 3.5, fontface = "bold") +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  theme_minimal() +
  labs(
    title = "Рис. 2. Распределение инцидентов по векторам атак",
    x = "Количество инцидентов", y = ""
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 12, margin = margin(b = 10)),
    axis.text.y = element_text(size = 9, color = "black"),
    panel.grid.major.y = element_blank()
  )

ggsave(file.path(FIGURES_DIR, "fig2_vectors.png"), p_vectors, width = 9, height = 6, dpi = 300)

# ----------------------------------------------------------------------------
# 4. График 3: Топ целей (Target Distribution)
# ----------------------------------------------------------------------------

cat("3. Генерация графика: Топ целей...\n")

p_targets <- db %>%
  count(target, sort = TRUE) %>%
  head(10) %>% 
  mutate(target = fct_reorder(target, n)) %>%
  ggplot(aes(x = n, y = target, fill = target)) +
  geom_col(show.legend = FALSE, alpha = 0.85) +
  geom_text(aes(label = n), hjust = -0.3, size = 3.5, fontface = "bold") +
  scale_fill_brewer(palette = "Blues") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal() +
  labs(
    title = "Рис. 3. Топ-10 наиболее упоминаемых целей инцидентов",
    x = "Количество инцидентов", y = ""
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 12, margin = margin(b = 10)),
    axis.text.y = element_text(size = 9, color = "black"),
    panel.grid.major.y = element_blank()
  )

ggsave(file.path(FIGURES_DIR, "fig3_targets.png"), p_targets, width = 9, height = 6, dpi = 300)

# ----------------------------------------------------------------------------
# 5. График 4: Распределение по сегментам (Segment Distribution)
# ----------------------------------------------------------------------------

cat("4. Генерация графика: Сегменты космических систем...\n")

p_segments <- db %>%
  count(segment, sort = TRUE) %>%
  mutate(
    segment = fct_reorder(segment, n),
    percent = n / sum(n)
  ) %>%
  ggplot(aes(x = n, y = segment, fill = segment)) +
  geom_col(show.legend = FALSE, alpha = 0.85) +
  geom_text(
    aes(label = paste0(n, " (", percent(percent, accuracy = 1), ")")), 
    hjust = -0.2, size = 3.5, fontface = "bold"
  ) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25))) + # Больше места для процентов
  theme_minimal() +
  labs(
    title = "Рис. 4. Распределение инцидентов по сегментам космической системы",
    subtitle = "Space (космический), Ground (наземный), User (пользовательский), Supply Chain",
    x = "Количество инцидентов", y = ""
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 12, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 9, color = "gray40", margin = margin(b = 15)),
    axis.text.y = element_text(size = 9, color = "black"),
    panel.grid.major.y = element_blank()
  )

ggsave(file.path(FIGURES_DIR, "fig4_segments.png"), p_segments, width = 9, height = 5, dpi = 300)

# ----------------------------------------------------------------------------
# 6. Вывод сводной таблицы для статьи (Топ-5 по медиа-охвату)
# ----------------------------------------------------------------------------

cat("\n=== СВОДНАЯ ТАБЛИЦА: Топ-5 инцидентов по охвату независимых СМИ ===\n")
db %>%
  select(target, vector, segment, year_month, n_sources, sources_list) %>%
  arrange(desc(n_sources)) %>%
  head(5) %>%
  mutate(
    target = str_wrap(target, width = 35),
    vector = str_wrap(vector, width = 30)
  ) %>%
  print(n = 5, width = Inf)

cat(sprintf("\n✔ Все графики успешно сохранены в директории: %s\n", FIGURES_DIR))
cat("✔ Готово к использованию в Quarto-отчёте и научной статье (IMRAD).\n")