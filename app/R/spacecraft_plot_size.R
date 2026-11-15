# spacecraft_plot_size.R
# Guard ggplot PNG renders when a plotOutput is hidden, mid-resize, or has
# no usable client size. Shiny opens the graphics device before the
# renderPlot expression runs, so a req() inside the expr is too late to
# stop "invalid 'width'" and "figure margins too large". sc_render_plot()
# supplies width/height functions that req() a positive client size first.

sc_min_plot_px <- 100

sc_plot_width_ok <- function(w, min_px = sc_min_plot_px) {
  is.numeric(w) && length(w) >= 1 && is.finite(w[[1]]) && w[[1]] >= min_px
}

sc_client_output_size <- function(session, output_id, dim = c("width", "height"),
                                  min_px = sc_min_plot_px) {
  dim <- match.arg(dim)
  key <- paste0("output_", output_id, "_", dim)
  w <- session$clientData[[key]]
  req(sc_plot_width_ok(w, min_px))
  as.numeric(w[[1]])
}

sc_render_plot <- function(output_id, session, expr, res = 96, min_px = sc_min_plot_px,
                           env = parent.frame(), quoted = FALSE, ...) {
  stopifnot(is.character(output_id), length(output_id) == 1L, nzchar(output_id))
  if (!quoted) expr <- substitute(expr)
  shiny::renderPlot(
    expr,
    width = function() sc_client_output_size(session, output_id, "width", min_px),
    height = function() sc_client_output_size(session, output_id, "height", min_px),
    res = res,
    quoted = TRUE,
    env = env,
    ...
  )
}
