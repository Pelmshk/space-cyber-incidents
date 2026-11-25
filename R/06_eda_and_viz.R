# ============================================================================
# R/06_eda_and_viz.R
# Разведочный анализ (EDA) и визуализация верифицированной базы инцидентов
# ============================================================================
# Изменения:
# 1. Нераспознанные значения (Unknown/Unclassified/Unknown Segment) убраны из топов
# 2. Добавлены примечания внизу графиков с количеством исключённых записей
# 3. KPI включает процент нераспознанных
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

db_path <- file.path(PROCESSED_DIR, "incidents_verified_v1.rds")

if (!file.exists(db_path)) {
  stop("Файл incidents_verified_v1.rds не найден. Запустите R/03_verify_and_merge.R и R/05_export_db.R")
}

db <- read_rds(db_path)

cat(sprintf("Загружено %d верифицированных инцидентов для анализа\n\n", nrow(db)))

# ----------------------------------------------------------------------------
# 0.1. Диагностика нераспознанных значений
# ----------------------------------------------------------------------------

n_total <- nrow(db)
n_unknown_target <- sum(db$target == "Unknown")
n_unclassified_vector <- sum(db$vector == "Unclassified")
n_unknown_segment <- sum(db$segment == "Unknown Segment" | db$segment == "Unknown Segment (Geopolitical Target)")

pct_unknown_target <- round(100 * n_unknown_target / n_total, 1)
pct_unclassified_vector <- round(100 * n_unclassified_vector / n_total, 1)
pct_unknown_segment <- round(100 * n_unknown_segment / n_total, 1)

cat("=== ДИАГНОСТИКА РАСПОЗНАВАНИЯ ===\n")
cat(sprintf("  Unknown целей: %d (%.1f%%)\n", n_unknown_target, pct_unknown_target))
cat(sprintf("  Unclassified векторов: %d (%.1f%%)\n", n_unclassified_vector, pct_unclassified_vector))
cat(sprintf("  Unknown сегментов: %d (%.1f%%)\n", n_unknown_segment, pct_unknown_segment))
cat(sprintf("  Полностью распознанных инцидентов: %d (%.1f%%)\n\n",
            sum(db$target != "Unknown" & db$vector != "Unclassified" &
                  !str_detect(db$segment, "^Unknown")),
            100 * sum(db$target != "Unknown" & db$vector != "Unclassified" &
                        !str_detect(db$segment, "^Unknown")) / n_total))

# ----------------------------------------------------------------------------
# 1. Ключевые метрики (KPI) для Executive Summary
# ----------------------------------------------------------------------------

cat("=== КЛЮЧЕВЫЕ МЕТРИКИ (для Executive Summary) ===\n")
cat(sprintf("- Всего верифицированных инцидентов: %d\n", n_total))
cat(sprintf("- Уникальных распознанных целей: %d\n",
            length(unique(db$target[db$target != "Unknown"]))))
cat(sprintf("- Уникальных распознанных векторов: %d\n",
            length(unique(db$vector[db$vector != "Unclassified"]))))
cat(sprintf("- Период наблюдения: %s - %s\n",
            min(db$year_month, na.rm = TRUE), max(db$year_month, na.rm = TRUE)))
cat(sprintf("- Максимальный медиа-охват одного инцидента: %d источников\n",
            max(db$n_sources)))
cat(sprintf("- Доля полностью распознанных инцидентов: %.1f%%\n\n",
            100 * sum(db$target != "Unknown" & db$vector != "Unclassified" &
                        !str_detect(db$segment, "^Unknown")) / n_total))

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
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month",
               expand = expansion(mult = c(0.05, 0.1))) +
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
# 3. График 2: Распределение векторов атак (без Unclassified)
# ----------------------------------------------------------------------------

cat("2. Генерация графика: Векторы атак...\n")

# Отдельно считаем Unclassified для примечания
n_unclass <- sum(db$vector == "Unclassified")
pct_unclass <- round(100 * n_unclass / n_total, 1)

# Основной график — только распознанные векторы
db_vectors <- db %>%
  filter(vector != "Unclassified") %>%
  count(vector, sort = TRUE)

caption_vectors <- sprintf(
  "Примечание: %d инцидентов (%.1f%%) не классифицированы по вектору (Unclassified)",
  n_unclass, pct_unclass
)

p_vectors <- db_vectors %>%
  mutate(
    vector = fct_reorder(vector, n),
    vector = str_wrap(vector, width = 30)
  ) %>%
  ggplot(aes(x = n, y = vector, fill = vector)) +
  geom_col(show.legend = FALSE, alpha = 0.85) +
  geom_text(aes(label = n), hjust = -0.3, size = 3.5, fontface = "bold") +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  theme_minimal() +
  labs(
    title = "Рис. 2. Распределение инцидентов по векторам атак",
    subtitle = "Только распознанные векторы (без Unclassified)",
    x = "Количество инцидентов", y = "",
    caption = caption_vectors
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 12, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 10, color = "gray40", margin = margin(b = 15)),
    plot.caption = element_text(size = 9, color = "gray50", face = "italic",
                                margin = margin(t = 10)),
    axis.text.y = element_text(size = 9, color = "black"),
    panel.grid.major.y = element_blank()
  )

if (nrow(db_vectors) > 0) {
  p_vectors <- p_vectors + xlim(0, max(db_vectors$n) * 1.15)
}

ggsave(file.path(FIGURES_DIR, "fig2_vectors.png"), p_vectors, width = 9, height = 6, dpi = 300)

# ----------------------------------------------------------------------------
# 4. График 3: Топ целей (без Unknown)
# ----------------------------------------------------------------------------

cat("3. Генерация графика: Топ целей...\n")

n_unk_target <- sum(db$target == "Unknown")
pct_unk_target <- round(100 * n_unk_target / n_total, 1)

db_targets <- db %>%
  filter(target != "Unknown") %>%
  count(target, sort = TRUE) %>%
  head(10)

caption_targets <- sprintf(
  "Примечание: %d инцидентов (%.1f%%) имеют нераспознанную цель (Unknown)",
  n_unk_target, pct_unk_target
)

p_targets <- db_targets %>%
  mutate(target = fct_reorder(target, n)) %>%
  ggplot(aes(x = n, y = target, fill = target)) +
  geom_col(show.legend = FALSE, alpha = 0.85) +
  geom_text(aes(label = n), hjust = -0.3, size = 3.5, fontface = "bold") +
  scale_fill_brewer(palette = "Blues") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal() +
  labs(
    title = "Рис. 3. Топ-10 наиболее упоминаемых целей инцидентов",
    subtitle = "Только распознанные цели (без Unknown)",
    x = "Количество инцидентов", y = "",
    caption = caption_targets
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 12, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 10, color = "gray40", margin = margin(b = 15)),
    plot.caption = element_text(size = 9, color = "gray50", face = "italic",
                                margin = margin(t = 10)),
    axis.text.y = element_text(size = 9, color = "black"),
    panel.grid.major.y = element_blank()
  )

if (nrow(db_targets) > 0) {
  p_targets <- p_targets + xlim(0, max(db_targets$n) * 1.15)
}

ggsave(file.path(FIGURES_DIR, "fig3_targets.png"), p_targets, width = 9, height = 6, dpi = 300)

# ----------------------------------------------------------------------------
# 5. График 4: Распределение по сегментам (без Unknown Segment)
# ----------------------------------------------------------------------------

cat("4. Генерация графика: Сегменты космических систем...\n")

n_unk_seg <- sum(str_detect(db$segment, "^Unknown"))
pct_unk_seg <- round(100 * n_unk_seg / n_total, 1)

db_segments <- db %>%
  filter(!str_detect(segment, "^Unknown")) %>%
  count(segment, sort = TRUE) %>%
  mutate(percent = n / sum(n))

caption_segments <- sprintf(
  "Примечание: %d инцидентов (%.1f%%) имеют нераспознанный сегмент (Unknown Segment)",
  n_unk_seg, pct_unk_seg
)

p_segments <- db_segments %>%
  mutate(segment = fct_reorder(segment, n)) %>%
  ggplot(aes(x = n, y = segment, fill = segment)) +
  geom_col(show.legend = FALSE, alpha = 0.85) +
  geom_text(
    aes(label = paste0(n, " (", percent(percent, accuracy = 1), ")")),
    hjust = -0.2, size = 3.5, fontface = "bold"
  ) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
  theme_minimal() +
  labs(
    title = "Рис. 4. Распределение инцидентов по сегментам космической системы",
    subtitle = "Space (космический), Ground (наземный), User (пользовательский), Supply Chain",
    x = "Количество инцидентов", y = "",
    caption = caption_segments
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 12, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 9, color = "gray40", margin = margin(b = 15)),
    plot.caption = element_text(size = 9, color = "gray50", face = "italic",
                                margin = margin(t = 10)),
    axis.text.y = element_text(size = 9, color = "black"),
    panel.grid.major.y = element_blank()
  )

if (nrow(db_segments) > 0) {
  p_segments <- p_segments + xlim(0, max(db_segments$n) * 1.3)
}

ggsave(file.path(FIGURES_DIR, "fig4_segments.png"), p_segments, width = 9, height = 5, dpi = 300)

# ----------------------------------------------------------------------------
# 6. Таблица Топ-5 инцидентов (без Unknown/Unclassified)
# ----------------------------------------------------------------------------

cat("\n=== СВОДНАЯ ТАБЛИЦА: Топ-5 распознанных инцидентов по охвату СМИ ===\n")

db_top <- db %>%
  filter(target != "Unknown", vector != "Unclassified") %>%
  select(target, vector, segment, year_month, n_sources, sources_list) %>%
  arrange(desc(n_sources)) %>%
  head(5) %>%
  mutate(
    target = str_wrap(target, width = 35),
    vector = str_wrap(vector, width = 30)
  )

if (nrow(db_top) > 0) {
  print(db_top, n = 5, width = Inf)
} else {
  cat("  (Нет полностью распознанных инцидентов для отображения)\n")
}

n_top_excluded <- n_total - nrow(db %>% filter(target != "Unknown", vector != "Unclassified"))
cat(sprintf("\nПримечание: из таблицы исключено %d инцидентов с Unknown/Unclassified\n", n_top_excluded))

# ----------------------------------------------------------------------------
# 7. Дополнительная таблица: нераспознанные инциденты (для справки)
# ----------------------------------------------------------------------------

cat("\n=== СПРАВКА: Нераспознанные инциденты (для экспертной разметки) ===\n")

db_unrecognized <- db %>%
  filter(target == "Unknown" | vector == "Unclassified") %>%
  select(target, vector, segment, year_month, n_mentions, n_sources) %>%
  arrange(desc(n_sources))

if (nrow(db_unrecognized) > 0) {
  cat(sprintf("  Всего нераспознанных инцидентов: %d\n", nrow(db_unrecognized)))
  cat("  Топ-10 по медиа-охвату:\n")
  print(head(db_unrecognized, 10), n = 10, width = Inf)
} else {
  cat("  Нераспознанных инцидентов не найдено.\n")
}

# ----------------------------------------------------------------------------
# 8. Финальная сводка
# ----------------------------------------------------------------------------

cat(sprintf("\nВсе графики сохранены в директории: %s\n", FIGURES_DIR))
cat("Готово к использованию в Quarto-отчёте и научной статье (IMRAD).\n")