# spacecraft_theme.R
# Shared ggplot theme and colors (carried over from the automotive app's
# theme_sim): base size 16, near-black text, legend at the bottom in one row.

sc_variant_colors <- c(A = "#2c7fb8", B = "#d95f02", C = "#1b9e77")

theme_sc <- function(...) {
  ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(
      text = ggplot2::element_text(size = 16, colour = "#1a1a1a"),
      axis.text = ggplot2::element_text(size = 16, colour = "#1a1a1a"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      plot.title.position = "plot",
      ...
    )
}

# Legend in one row for every colour/fill scale.
sc_one_row_legend <- function() {
  ggplot2::guides(colour = ggplot2::guide_legend(nrow = 1), fill = ggplot2::guide_legend(nrow = 1))
}
