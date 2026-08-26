library(ggplot2)
library(dplyr)
library(readxl)

input_file <- "input_data.xlsx"
output_prefix <- "forest_plot"

raw_data <- read_excel(input_file)

plot_data <- raw_data
colnames(plot_data) <- c(
  "feature", "panel", "estimate", "lower_ci", "upper_ci", "p_value", "model"
)

plot_data <- plot_data %>%
  mutate(
    p_value_numeric = suppressWarnings(as.numeric(p_value)),
    significance = case_when(
      p_value_numeric < 0.001 ~ "***",
      p_value_numeric < 0.01 ~ "**",
      p_value_numeric < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

panel_values <- unique(plot_data$panel)
reference_panel <- panel_values[1]
reference_model <- unique(plot_data$model)[1]

feature_order <- plot_data %>%
  filter(panel == reference_panel, model == reference_model) %>%
  arrange(desc(estimate)) %>%
  pull(feature) %>%
  unique()

plot_data$feature <- factor(plot_data$feature, levels = feature_order)

font_family <- "Arial"
base_size <- 12
title_size <- 14
model_values <- unique(plot_data$model)
model_colors <- c("#4E79A7", "#F28E2B", "#59A14F", "#E15759")

create_forest_plot <- function(data, panel_label) {
  data$feature <- factor(data$feature, levels = feature_order)
  data$model <- factor(data$model, levels = model_values)

  valid_lower <- data$lower_ci[is.finite(data$lower_ci) & !is.na(data$lower_ci)]
  valid_upper <- data$upper_ci[is.finite(data$upper_ci) & !is.na(data$upper_ci)]

  if (length(valid_lower) > 0 && length(valid_upper) > 0) {
    min_x <- max(min(valid_lower, na.rm = TRUE) * 0.8, 0.01)
    max_x <- min(max(valid_upper, na.rm = TRUE) * 1.5, 100)
  } else {
    min_x <- 0.05
    max_x <- 20
  }

  annotation_data <- data %>%
    mutate(
      annotation_x = max_x * 0.85,
      annotation_y = as.numeric(feature) +
        (as.numeric(model) - (length(model_values) + 1) / 2) * 0.18
    )

  ggplot(data, aes(x = estimate, y = feature)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
    geom_pointrange(
      aes(xmin = lower_ci, xmax = upper_ci, color = model),
      position = position_dodge(width = 0.6),
      linewidth = 0.6, fatten = 1.5, na.rm = TRUE
    ) +
    geom_text(
      data = annotation_data,
      aes(x = annotation_x, y = annotation_y, label = significance, color = model),
      size = base_size / 3, fontface = "bold", show.legend = FALSE,
      family = font_family, hjust = 0.5, vjust = 0.5, na.rm = TRUE
    ) +
    scale_x_continuous(
      trans = "log10",
      breaks = c(0.05, 0.1, 0.5, 1, 2, 4, 8, 16, 32),
      limits = c(min_x, max_x),
      labels = c("0.05", "0.1", "0.5", "1", "2", "4", "8", "16", "32")
    ) +
    scale_color_manual(values = model_colors[seq_along(model_values)]) +
    labs(x = "Effect estimate", y = "Feature", title = panel_label, color = NULL) +
    theme_minimal() +
    theme(
      text = element_text(family = font_family, size = base_size),
      plot.title = element_text(
        size = title_size, face = "bold", hjust = 0.5,
        margin = margin(b = 15), family = font_family
      ),
      axis.title = element_text(size = base_size, face = "bold", family = font_family),
      axis.title.x = element_text(margin = margin(t = 10), family = font_family),
      axis.title.y = element_text(margin = margin(r = 10), family = font_family),
      axis.text = element_text(size = base_size, family = font_family),
      axis.text.y = element_text(
        size = base_size, margin = margin(r = 10), face = "bold", family = font_family
      ),
      legend.title = element_blank(),
      legend.text = element_text(size = base_size, face = "bold", family = font_family),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.position = "bottom",
      legend.margin = margin(t = 10),
      plot.margin = margin(20, 20, 20, 20)
    ) +
    scale_y_discrete(expand = expansion(add = 1.0))
}

for (index in seq_along(panel_values)) {
  panel_data <- filter(plot_data, panel == panel_values[index])
  panel_plot <- create_forest_plot(panel_data, paste("Panel", index))
  print(panel_plot)
  ggsave(
    sprintf("%s_%02d.tiff", output_prefix, index),
    panel_plot, width = 12, height = 16, dpi = 300
  )
}
