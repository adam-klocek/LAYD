# Shared vivid, accessible plotting style. Text accompanies color in every diagnostic.
PALETTE <- c("#7048F4", "#00A6A6", "#F43F5E", "#F59E0B", "#3282F6", "#D946EF")
layd_theme <- function() ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(
  panel.grid.minor = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_blank(),
  plot.title = ggplot2::element_text(face = "bold", color = "#17243B", size = 15),
  axis.title = ggplot2::element_text(color = "#536078"), legend.title = ggplot2::element_text(face = "bold"),
  plot.margin = ggplot2::margin(14, 18, 12, 12), plot.background = ggplot2::element_rect(fill = "white", color = NA))
blank_plot <- function(label) ggplot2::ggplot() + ggplot2::annotate("text", x = 0, y = 0, label = label, color = "#64748B", size = 4) + ggplot2::theme_void()
plot_family <- function(r) {
  z <- as.data.frame(table(factor(r$findings$family_label, levels = FAMILIES),
    factor(r$findings$status, levels = c("Review", "Clear", "Info", "Not assessed"))))
  names(z) <- c("family", "status", "n")
  ggplot2::ggplot(z, ggplot2::aes(x = family, y = n, fill = status)) + ggplot2::geom_col(width = .63) +
    ggplot2::coord_flip() + ggplot2::scale_fill_manual(values = c("Review" = "#F43F5E", "Clear" = "#00A6A6", "Info" = "#7048F4", "Not assessed" = "#CBD5E1"), drop = FALSE) +
    ggplot2::labs(x = NULL, y = "Number of checks", fill = NULL) + layd_theme() + ggplot2::theme(legend.position = "bottom")
}
plot_missing <- function(r, scale) {
  items <- r$summaries$item[r$summaries$scale == scale]
  # Show a deterministic spread, preserving original row numbers for inspection.
  idx <- unique(round(seq(1, nrow(r$prepared), length.out = min(500, nrow(r$prepared)))))
  m <- r$missing_kind[idx, items, drop = FALSE]
  z <- expand.grid(row = idx, item = items, KEEP.OUT.ATTRS = FALSE); z$state <- as.vector(m)
  z$item <- factor(z$item, levels = rev(items))
  ggplot2::ggplot(z, ggplot2::aes(x = row, y = item, fill = state)) + ggplot2::geom_tile() +
    ggplot2::scale_fill_manual(values = c("Observed" = "#D8D1FF", "Missing" = "#718096", "Missing code" = "#F59E0B", "Invalid" = "#F43F5E")) +
    ggplot2::labs(x = "Original row number (up to 500 evenly spaced rows)", y = NULL, fill = NULL) + layd_theme() +
    ggplot2::theme(legend.position = "bottom")
}
plot_missing_bars <- function(r) {
  z <- r$summaries
  ggplot2::ggplot(z, ggplot2::aes(x = reorder(item, missing_pct), y = missing_pct, fill = scale)) +
    ggplot2::geom_col(width = .68) + ggplot2::geom_hline(yintercept = r$settings$missing_threshold, linetype = 2, color = "#F43F5E") +
    ggplot2::coord_flip() + ggplot2::scale_fill_manual(values = rep(PALETTE, length.out = length(unique(z$scale)))) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(x * 100), "%"), limits = c(0, 1)) +
    ggplot2::labs(x = NULL, y = "Unavailable responses after validity checks", fill = "Scale") + layd_theme()
}
plot_likert <- function(r, scale) {
  items <- r$summaries$item[r$summaries$scale == scale]
  z <- do.call(rbind, lapply(items, function(item) {
    t <- table(r$prepared[[item]], useNA = "no")
    if (!length(t)) return(NULL)
    data.frame(item = item, response = names(t), percent = as.numeric(t) / sum(t))
  }))
  if (is.null(z) || !nrow(z)) return(blank_plot("No valid responses for this scale."))
  responses <- as.character(sort(unique(as.numeric(z$response))))
  if (length(responses) > 15) return(blank_plot("More than 15 response categories. Use the item histogram instead."))
  z$response <- factor(z$response, levels = responses); z$item <- factor(z$item, levels = rev(items))
  ggplot2::ggplot(z, ggplot2::aes(x = item, y = percent, fill = response)) + ggplot2::geom_col(width = .65, position = ggplot2::position_stack(reverse = TRUE)) + ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = grDevices::colorRampPalette(c("#F43F5E", "#F59E0B", "#C4B5FD", "#7048F4", "#009A9A"))(length(responses))) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
    ggplot2::labs(x = NULL, y = "Percent of valid, keyed responses", fill = "Response") + layd_theme() + ggplot2::theme(legend.position = "bottom")
}
plot_correlation <- function(m, title = "") {
  if (!length(m)) return(blank_plot("Select more items or scales."))
  z <- expand.grid(x = colnames(m), y = rownames(m), KEEP.OUT.ATTRS = FALSE); z$r <- as.vector(m)
  z$x <- factor(z$x, levels = colnames(m)); z$y <- factor(z$y, levels = rev(rownames(m)))
  p <- ggplot2::ggplot(z, ggplot2::aes(x, y, fill = r)) + ggplot2::geom_tile(color = "white", linewidth = .6) +
    ggplot2::scale_fill_gradient2(low = "#F43F5E", mid = "#F8FAFC", high = "#7048F4", midpoint = 0, limits = c(-1, 1), na.value = "#CBD5E1") +
    ggplot2::labs(x = NULL, y = NULL, fill = "r", title = title) + layd_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1))
  if (ncol(m) <= 15) p <- p + ggplot2::geom_text(ggplot2::aes(label = ifelse(is.finite(r), sprintf("%.2f", r), "NA")), size = 3.3)
  p
}
plot_item_rest <- function(r, scale) {
  z <- r$summaries[r$summaries$scale == scale, ]; z$flag <- ifelse(z$item_rest < 0, "Negative", ifelse(z$item_rest < r$settings$rit_threshold, "Weak", "Above threshold"))
  ggplot2::ggplot(z, ggplot2::aes(x = reorder(item, item_rest), y = item_rest, fill = flag)) +
    ggplot2::geom_col(width = .62, na.rm = TRUE) + ggplot2::geom_hline(yintercept = c(0, r$settings$rit_threshold), linetype = c(1, 2), color = "#718096") +
    ggplot2::coord_flip() + ggplot2::scale_fill_manual(values = c("Negative" = "#F43F5E", "Weak" = "#F59E0B", "Above threshold" = "#00A6A6")) +
    ggplot2::scale_y_continuous(limits = c(-1, 1)) + ggplot2::labs(x = NULL, y = paste(r$settings$corr_method, "corrected item-rest r"), fill = NULL) + layd_theme() +
    ggplot2::theme(legend.position = "bottom")
}
plot_scree <- function(r, scale) {
  p <- r$parallel[[scale]]
  if (is.null(p)) return(blank_plot("Parallel analysis unavailable: check sample size and constant items."))
  z <- p$table
  ggplot2::ggplot(z, ggplot2::aes(component, observed)) + ggplot2::geom_line(color = "#7048F4", linewidth = 1) +
    ggplot2::geom_point(color = "#7048F4", size = 3) +
    ggplot2::geom_line(ggplot2::aes(y = simulated_95), color = "#F43F5E", linetype = 2, linewidth = 1) +
    ggplot2::labs(x = "Component", y = "Eigenvalue", subtitle = "Purple: observed · Coral dashed: simulated 95th percentile") + layd_theme()
}
plot_loadings <- function(r, scale) {
  m <- r$models[[scale]]
  if (is.null(m$loading)) return(blank_plot(paste("CFA unavailable.", substr(m$reason, 1, 100))))
  z <- m$loading
  ggplot2::ggplot(z, ggplot2::aes(reorder(item, loading), loading, fill = loading < 0)) +
    ggplot2::geom_col(width = .62) + ggplot2::geom_hline(yintercept = 0) + ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("FALSE" = "#7048F4", "TRUE" = "#F43F5E"), guide = "none") +
    ggplot2::labs(x = NULL, y = "Standardized factor loading (median oriented positive)") + layd_theme()
}
plot_path <- function(r, scale) {
  m <- r$models[[scale]]
  if (is.null(m$loading)) return(blank_plot("A successful CFA is needed to draw the path diagram."))
  z <- m$loading; z$y <- seq(nrow(z), 1)
  mid <- mean(z$y)
  ggplot2::ggplot(z) + ggplot2::geom_segment(ggplot2::aes(x = .25, y = mid, xend = .82, yend = y, color = loading < 0),
    arrow = grid::arrow(length = grid::unit(.13, "inches")), linewidth = .8) +
    ggplot2::geom_text(ggplot2::aes(x = .57, y = (mid + y) / 2, label = sprintf("%.2f", loading)), size = 3, color = "#17243B") +
    ggplot2::geom_rect(ggplot2::aes(xmin = .82, xmax = 1.18, ymin = y - .24, ymax = y + .24), fill = "#EDE9FE", color = "#C4B5FD") +
    ggplot2::geom_text(ggplot2::aes(x = 1, y = y, label = item), color = "#49329E", size = 4) +
    ggplot2::annotate("point", x = .12, y = mid, size = 22, color = "#7048F4") +
    ggplot2::annotate("text", x = .12, y = mid, label = "Factor", color = "white", size = 4) +
    ggplot2::scale_color_manual(values = c("FALSE" = "#7048F4", "TRUE" = "#F43F5E"), guide = "none") +
    ggplot2::coord_cartesian(xlim = c(0, 1.2), clip = "off") + ggplot2::theme_void() +
    ggplot2::labs(caption = "Standardized loadings; residuals omitted. One-factor model per scale.")
}
plot_group <- function(r, scale, metric = "item_rest") {
  z <- r$groups[r$groups$scale == scale, ]
  if (!nrow(z)) return(blank_plot("Choose a grouping column with at most 30 levels and rerun."))
  z$value <- z[[metric]]
  ggplot2::ggplot(z, ggplot2::aes(item, value, group = group, color = group)) +
    ggplot2::geom_line(linewidth = .8, na.rm = TRUE) + ggplot2::geom_point(size = 2.8, na.rm = TRUE) +
    ggplot2::scale_color_manual(values = rep(PALETTE, length.out = length(unique(z$group)))) +
    ggplot2::labs(x = NULL, y = switch(metric, item_rest = "Corrected item-rest r", missing_pct = "Proportion unavailable", mean = "Mean keyed response"), color = "Group") +
    layd_theme() + ggplot2::theme(legend.position = "bottom")
}
