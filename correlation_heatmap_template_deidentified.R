options(stringsAsFactors = FALSE)

ensure <- function(pkgs) {
  missing_pkgs <- setdiff(pkgs, rownames(installed.packages()))
  if (length(missing_pkgs)) install.packages(missing_pkgs, quiet = TRUE)
  invisible(lapply(pkgs, require, character.only = TRUE))
}

ensure(c("readxl", "dplyr", "ggplot2", "writexl"))

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(writexl)
})

input_file <- "input_workbook.xlsx"
sheet_one <- 1
sheet_two <- 2
long_output <- "long_format_data.xlsx"
figure_output <- "correlation_heatmap.png"
fill_limits <- c(-0.4, 0.4)

normalize_text <- function(x) {
  trimws(gsub("\\u00A0", " ", gsub("–", "-", x)))
}

is_p_value <- function(x) {
  numeric_x <- suppressWarnings(as.numeric(x))
  if (all(is.na(numeric_x))) return(FALSE)
  mean(numeric_x >= 0 & numeric_x <= 1, na.rm = TRUE) >= 0.9
}

is_ellipsis_name <- function(x) grepl("^\\.\\.\\.[0-9]+$", x)
remove_suffix <- function(x) sub("\\.\\.\\..*$", "", x)

get_feature_name <- function(first_name, second_name) {
  if (!is_ellipsis_name(first_name) && is_ellipsis_name(second_name)) {
    return(remove_suffix(first_name))
  }
  if (is_ellipsis_name(first_name) && !is_ellipsis_name(second_name)) {
    return(remove_suffix(second_name))
  }
  remove_suffix(first_name)
}

wide_to_long <- function(data, panel_name) {
  nonempty_columns <- which(colSums(!is.na(data)) > 0)
  data <- data[, nonempty_columns, drop = FALSE]
  column_names <- names(data)
  stopifnot(ncol(data) >= 3)

  value_columns <- 2:ncol(data)
  if (length(value_columns) %% 2 == 1) {
    value_columns <- value_columns[-length(value_columns)]
  }

  column_pairs <- split(value_columns, ceiling(seq_along(value_columns) / 2))
  rows <- lapply(column_pairs, function(pair_index) {
    first_index <- pair_index[1]
    second_index <- pair_index[2]
    first_value <- data[[first_index]]
    second_value <- data[[second_index]]

    if (is_p_value(first_value) && !is_p_value(second_value)) {
      correlation <- suppressWarnings(as.numeric(second_value))
      p_value <- suppressWarnings(as.numeric(first_value))
    } else {
      correlation <- suppressWarnings(as.numeric(first_value))
      p_value <- suppressWarnings(as.numeric(second_value))
    }

    data.frame(
      panel = panel_name,
      outcome = data[[1]],
      feature = as.character(get_feature_name(
        column_names[first_index], column_names[second_index]
      )),
      correlation = correlation,
      p_value = p_value,
      stringsAsFactors = FALSE
    )
  })
  bind_rows(rows)
}

significance_label <- function(p_value) {
  ifelse(
    is.na(p_value), "",
    ifelse(p_value < 0.001, "***", ifelse(p_value < 0.01, "**", ifelse(p_value < 0.05, "*", "")))
  )
}

data_one <- read_excel(input_file, sheet = sheet_one)
data_two <- read_excel(input_file, sheet = sheet_two)

long_data <- bind_rows(
  wide_to_long(data_one, "Panel 1"),
  wide_to_long(data_two, "Panel 2")
) %>%
  filter(!(is.na(correlation) & is.na(p_value))) %>%
  mutate(
    outcome = ifelse(
      is.na(outcome) | outcome %in% c("NA", "NaN", ""),
      " ", normalize_text(as.character(outcome))
    ),
    feature = normalize_text(feature)
  )

write_xlsx(list(data = long_data), long_output)

plot_data <- long_data %>%
  mutate(
    panel = factor(panel, levels = c("Panel 1", "Panel 2")),
    significance = significance_label(p_value),
    outcome_tag = paste0(outcome, " [", panel, "]"),
    feature = factor(feature, levels = unique(feature))
  )

panel_one_outcomes <- plot_data %>%
  filter(panel == "Panel 1") %>%
  distinct(outcome) %>%
  pull()
panel_two_outcomes <- plot_data %>%
  filter(panel == "Panel 2") %>%
  distinct(outcome) %>%
  pull()

outcome_levels <- c(
  paste0(panel_one_outcomes, " [Panel 1]"),
  paste0(panel_two_outcomes, " [Panel 2]")
)
plot_data$outcome_tag <- factor(plot_data$outcome_tag, levels = outcome_levels)

heatmap <- ggplot(
  plot_data,
  aes(x = feature, y = outcome_tag, fill = correlation)
) +
  geom_tile(color = "white", linewidth = 0.25) +
  geom_text(aes(label = significance), size = 3.1, na.rm = TRUE) +
  scale_fill_gradient2(
    low = "#6BAED6", mid = "white", high = "#FB6A4A",
    midpoint = 0, limits = fill_limits, name = "r",
    na.value = "white", oob = scales::squish
  ) +
  scale_y_discrete(
    labels = function(x) sub(" \\[Panel (1|2)\\]$", "", x),
    expand = c(0, 0)
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  coord_cartesian(expand = FALSE) +
  labs(x = NULL, y = NULL) +
  facet_grid(
    rows = vars(panel), scales = "free_y", space = "free_y", switch = "y"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    axis.title = element_text(face = "bold"),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(face = "bold", size = 12, margin = margin(r = 6)),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    plot.margin = margin(8, 12, 8, 8)
  )

print(heatmap)

number_of_rows <- length(unique(plot_data$outcome_tag))
ggsave(
  figure_output, heatmap,
  width = 14, height = max(7, 0.35 * number_of_rows + 2), dpi = 300
)

cat("Figure saved as ", figure_output, "\n", sep = "")
