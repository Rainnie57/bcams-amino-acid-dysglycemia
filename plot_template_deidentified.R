library(tidyverse)
library(readxl)
library(ggplot2)
library(patchwork)

# Read de-identified input tables. Do not upload the original input files.
raw_data <- read_excel("input_data.xlsx")
annotation_data <- read_excel("input_annotations.xlsx")

# Standardize the first six columns of the summary table.
clean_data <- raw_data %>%
  rename(
    feature = 1,
    group = 2,
    sample_size = 3,
    mean_value = 4,
    standard_deviation = 5,
    standard_error = 6
  )

# Merge display annotations, if applicable.
plot_data <- clean_data %>%
  left_join(annotation_data, by = "feature")

# Order features by their overall mean.
feature_ranking <- plot_data %>%
  group_by(feature) %>%
  summarise(overall_mean = mean(mean_value, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(overall_mean))

plot_data$feature <- factor(plot_data$feature, levels = feature_ranking$feature)
plot_data$group <- factor(plot_data$group)

# Use the lowest-ranked features in the inset.
inset_features <- tail(levels(plot_data$feature), 6)
main_data <- plot_data
inset_data <- plot_data %>% filter(feature %in% inset_features)

# Positions for optional significance annotations.
annotation_positions_main <- main_data %>%
  group_by(feature) %>%
  summarise(
    max_y = max(mean_value + standard_error, na.rm = TRUE),
    annotation = first(annotation),
    .groups = "drop"
  ) %>%
  filter(!is.na(annotation) & annotation != "")

annotation_positions_inset <- inset_data %>%
  group_by(feature) %>%
  summarise(
    max_y = max(mean_value + standard_error, na.rm = TRUE),
    annotation = first(annotation),
    .groups = "drop"
  ) %>%
  filter(!is.na(annotation) & annotation != "")

font_family <- "Arial"
base_size <- 12
annotation_size <- 10
group_colors <- c("#7FBF7B", "#FFD166", "#E74C3C")

main_plot <- ggplot(main_data, aes(x = feature, y = mean_value, fill = group)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(
    aes(
      ymin = mean_value - standard_error,
      ymax = mean_value + standard_error,
      color = group
    ),
    position = position_dodge(0.8), width = 0.25,
    linewidth = 0.6, show.legend = FALSE
  ) +
  geom_text(
    data = annotation_positions_main,
    aes(x = feature, y = max_y + 8, label = annotation),
    inherit.aes = FALSE,
    size = annotation_size / 2.83465,
    vjust = 0, fontface = "bold", family = font_family
  ) +
  scale_fill_manual(values = group_colors) +
  scale_color_manual(values = group_colors) +
  theme_classic() +
  labs(title = "Measurements", x = "", y = "Value (units)", fill = "") +
  theme(
    text = element_text(family = font_family, size = base_size),
    plot.title = element_text(
      size = base_size + 2, face = "bold", hjust = 0.5,
      margin = margin(b = 5), family = font_family
    ),
    axis.text.x = element_text(
      angle = 60, hjust = 1, size = base_size,
      face = "bold", family = font_family
    ),
    axis.text.y = element_text(size = base_size, face = "bold", family = font_family),
    axis.title.y = element_text(size = base_size, face = "bold", family = font_family),
    legend.position = c(0.95, 0.9),
    legend.text = element_text(face = "bold", size = base_size, family = font_family),
    legend.title = element_text(face = "bold", size = base_size, family = font_family),
    legend.justification = "top",
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.0)))

inset_plot <- ggplot(inset_data, aes(x = feature, y = mean_value, fill = group)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(
    aes(
      ymin = mean_value - standard_error,
      ymax = mean_value + standard_error,
      color = group
    ),
    position = position_dodge(0.8), width = 0.25,
    linewidth = 0.5, show.legend = FALSE
  ) +
  geom_text(
    data = annotation_positions_inset,
    aes(x = feature, y = max_y + 0.15, label = annotation),
    inherit.aes = FALSE,
    size = annotation_size / 2.83465,
    vjust = 0, fontface = "bold", family = font_family
  ) +
  scale_fill_manual(values = group_colors) +
  scale_color_manual(values = group_colors) +
  theme_classic() +
  labs(x = "", y = "Value (units)", fill = "") +
  theme(
    text = element_text(family = font_family, size = base_size - 1),
    axis.text.x = element_text(
      angle = 45, hjust = 1, size = base_size - 1,
      face = "bold", family = font_family
    ),
    axis.text.y = element_text(size = base_size - 1, face = "bold", family = font_family),
    axis.title.y = element_text(size = base_size - 1, face = "bold", family = font_family),
    legend.position = "none",
    axis.line = element_line(color = "black", linewidth = 0.3),
    axis.ticks = element_line(color = "black", linewidth = 0.3),
    plot.background = element_rect(color = "white", linewidth = 0.5)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)))

final_plot <- main_plot +
  inset_element(inset_plot, left = 0.50, bottom = 0.40, right = 0.85, top = 0.75)

print(final_plot)

ggsave(
  "figure.tiff", final_plot,
  width = 16, height = 10, dpi = 300, compression = "lzw"
)

cat("Figure saved as figure.tiff\n")
