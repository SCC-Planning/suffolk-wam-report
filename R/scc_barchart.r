
scc_barchart <- function(data, x, y,
                         # scc_logo,
                         title) {
  p1 <- data %>%
    ggplot(aes(x = x, y = y)) +
    geom_col(position = "identity", colour = "#333333", fill = "#2d6ca2") +
    geom_text(aes(
      y = mean(y * 0.5),
      label = paste(totals_formatting(y), "t")
    ),
    colour = "white"
    ) +
    geom_hline(yintercept = 0, size = 1, colour = "#333333") +
    labs(title = title) +
    scc_style()
  # Optional, adding scc logo to the plot
  # annotation_custom(scc_logo, xmin = 5.0, xmax = 7.5, ymin = -125000, ymax = -50000) +
  # coord_cartesian(clip = "off") +
  # theme(plot.margin = unit(c(1, 1, 4, 1), "lines"))
  return(p1)
}

scc_barchart_pct <- function(data, x, y,
                             # scc_logo,
                             title) {
  p1 <- data %>%
    ggplot(aes(x = x, y = y)) +
    geom_col(position = "identity", colour = "#333333", fill = "#2d6ca2") +
    geom_text(aes(
      y = mean(y * 0.5),
      label = paste(totals_formatting(y), "%")
    ),
    colour = "white"
    ) +
    geom_hline(yintercept = 0, size = 1, colour = "#333333") +
    labs(title = title) +
    scc_style()
  # Optional, adding scc logo to the plot
  # annotation_custom(scc_logo, xmin = 5.0, xmax = 7.5, ymin = -125000, ymax = -50000) +
  # coord_cartesian(clip = "off") +
  # theme(plot.margin = unit(c(1, 1, 4, 1), "lines"))
  return(p1)
}

scc_barchart_grouped <- function(data, x, y, group,
                                 # scc_logo,
                                 title) {
  p1 <- data %>%
    ggplot(aes(x = x, y = y, fill = as.factor(group))) +
    geom_col(position = "dodge", colour = "#333333") +
    geom_hline(yintercept = 0, size = 1, colour = "#333333") +
    labs(title = title) +
    scale_fill_manual(values = c("#e2eefa", "#2d6ca2", "#e8850c")) +
    scc_style()
  # Optional, adding scc logo to the plot
  # annotation_custom(scc_logo, xmin = 5.0, xmax = 7.5, ymin = -125000, ymax = -50000) +
  # coord_cartesian(clip = "off") +
  # theme(plot.margin = unit(c(1, 1, 4, 1), "lines"))
  return(p1)
}
