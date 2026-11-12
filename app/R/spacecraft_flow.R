# spacecraft_flow.R
# Inline SVG of how a unit moves through the line, used on Settings > Variants
# (including the full-screen card). The layout follows Doc's six-station flow;
# the look matches the approved flat "star map" mockup: nodes are points of
# light (filled circles with a four-point star) in the same language as the
# rest of the Shiny app — no glow, no dark background.
#
# Routes, by design:
#   Forward            solid cyan/blue, with arrowheads along the main line
#   Fail and Rework    dashed gold, one-way: Functional test fail and Review
#                      rework both enter Integration only (one shared arrow
#                      into Integration; none into Functional test or Review)
#   Env-test fail      dashed gold, into Review
#   Accepted           dashed green, the only Review -> Final acceptance path
#   Scrap              dashed red, ending in a hollow circle, no arrowhead

# Colours taken from the approved mockup, with the app primary as the
# forward/node blue so the diagram sits with the rest of the UI.
sc_flow_cols <- list(
  forward  = "#2c7fb8",
  halo     = "#e8f3fc",
  star     = "#ffffff",
  review   = "#e0a032",
  fail     = "#d2a44c",
  accepted = "#30894b",
  scrap    = "#ab3134",
  text     = "#222222",
  muted    = "#6c757d",
  paper    = "#ffffff"
)

# Four-point sparkle centred at (x, y). The arms sit on the axes; the
# control points pull the sides in so it reads as a twinkle, not a diamond.
sc_flow_star_d <- function(x, y, r) {
  k <- 0.16 * r
  sprintf(
    "M %.2f,%.2f C %.2f,%.2f %.2f,%.2f %.2f,%.2f C %.2f,%.2f %.2f,%.2f %.2f,%.2f C %.2f,%.2f %.2f,%.2f %.2f,%.2f C %.2f,%.2f %.2f,%.2f %.2f,%.2f Z",
    x, y - r,
    x + k, y - k, x + k, y - k, x + r, y,
    x + k, y + k, x + k, y + k, x, y + r,
    x - k, y + k, x - k, y + k, x - r, y,
    x - k, y - k, x - k, y - k, x, y - r
  )
}

# Axis-aligned path through pts (a two-column matrix / tibble of x, y),
# with rounded corners of radius `radius`.
sc_flow_ortho_d <- function(pts, radius = 18) {
  pts <- as.matrix(pts)
  n <- nrow(pts)
  if (n < 2) stop("Need at least two points")
  r_max <- function(a, b) {
    d <- sqrt((a[1] - b[1])^2 + (a[2] - b[2])^2)
    min(radius, d / 2 - 0.1)
  }
  pieces <- sprintf("M %.2f,%.2f", pts[1, 1], pts[1, 2])
  if (n == 2) return(paste(c(pieces, sprintf("L %.2f,%.2f", pts[2, 1], pts[2, 2])), collapse = " "))
  for (i in 2:(n - 1)) {
    prev <- pts[i - 1, ]; cur <- pts[i, ]; nxt <- pts[i + 1, ]
    r <- min(r_max(prev, cur), r_max(cur, nxt))
    v1 <- c(cur[1] - prev[1], cur[2] - prev[2]); v1 <- v1 / sqrt(sum(v1^2))
    v2 <- c(nxt[1] - cur[1], nxt[2] - cur[2]); v2 <- v2 / sqrt(sum(v2^2))
    enter <- cur - v1 * r
    leave <- cur + v2 * r
    pieces <- c(pieces,
                sprintf("L %.2f,%.2f", enter[1], enter[2]),
                sprintf("Q %.2f,%.2f %.2f,%.2f", cur[1], cur[2], leave[1], leave[2]))
  }
  paste(c(pieces, sprintf("L %.2f,%.2f", pts[n, 1], pts[n, 2])), collapse = " ")
}

sc_flow_marker <- function(id, colour) {
  htmltools::tags$marker(
    id = id, viewBox = "0 0 10 10", refX = "9", refY = "5",
    markerWidth = "8", markerHeight = "8", orient = "auto", markerUnits = "userSpaceOnUse",
    htmltools::tags$path(d = "M 0 1.2 L 9 5 L 0 8.8 Z", fill = colour)
  )
}

sc_flow_path <- function(d, colour, dashed = FALSE, marker_end = NULL, width = 2.8, id = NULL) {
  htmltools::tags$path(
    id = id, d = d, fill = "none", stroke = colour, `stroke-width` = width,
    `stroke-linecap` = if (dashed) "round" else "butt",
    `stroke-linejoin` = "round",
    `stroke-dasharray` = if (dashed) "7 6" else NULL,
    `marker-end` = if (!is.null(marker_end)) paste0("url(#", marker_end, ")") else NULL
  )
}

# A filled station: optional pale halo (Integration), circle, star.
sc_flow_node <- function(x, y, r, fill, halo_r = NULL, star_r = 9) {
  htmltools::tagList(
    if (!is.null(halo_r)) htmltools::tags$circle(cx = x, cy = y, r = halo_r, fill = sc_flow_cols$halo),
    htmltools::tags$circle(cx = x, cy = y, r = r, fill = fill),
    htmltools::tags$path(d = sc_flow_star_d(x, y, star_r), fill = sc_flow_cols$star)
  )
}

# Label block under (or otherwise near) a node. A white backdrop keeps
# route strokes from running through the words.
sc_flow_label <- function(x, y, lines, anchor = "middle", size = 16, weight = 700,
                          colour = sc_flow_cols$text, gap = 18, href = NULL) {
  # lines: character vector, or a list of lists(text, size, weight, colour, href)
  if (is.character(lines)) {
    lines <- purrr::imap(lines, function(text, i) {
      list(text = text, size = if (i == 1) size else size - 2,
           weight = if (i == 1) weight else 500,
           colour = if (i == 1) colour else sc_flow_cols$muted, href = NULL)
    })
  }
  # Backdrop size is an estimate; tspans stay centred / left / right with anchor.
  width <- max(purrr::map_dbl(lines, ~ max(6, nchar(.x$text)) * .x$size * 0.58))
  height <- sum(purrr::map_dbl(lines, "size")) + (length(lines) - 1) * 4 + 8
  x_left <- switch(anchor, start = x - 4, end = x - width + 4, x - width / 2)
  body <- purrr::imap(lines, function(ln, i) {
    node <- htmltools::tags$text(
      x = x, y = y + (i - 1) * (ln$size + 4),
      `text-anchor` = anchor, `font-size` = ln$size, `font-weight` = ln$weight,
      fill = ln$colour, `dominant-baseline` = "hanging", ln$text
    )
    if (!is.null(ln$href)) htmltools::tags$a(`xlink:href` = ln$href, href = ln$href, node) else node
  })
  htmltools::tagList(
    htmltools::tags$rect(x = x_left, y = y - 4, width = width, height = height,
                         fill = sc_flow_cols$paper, `pointer-events` = "none"),
    body
  )
}

# Mid-path word (Fail, Rework, Accepted) with a paper-coloured gap in the stroke.
sc_flow_path_label <- function(x, y, label, size = 14) {
  w <- nchar(label) * size * 0.72 + 22
  htmltools::tagList(
    htmltools::tags$rect(x = x - w / 2, y = y - size / 2 - 3, width = w, height = size + 8,
                         fill = sc_flow_cols$paper),
    htmltools::tags$text(x = x, y = y, `text-anchor` = "middle", `dominant-baseline` = "middle",
                         `font-size` = size, `font-weight` = 600, fill = sc_flow_cols$text, label)
  )
}

sc_flow_legend_item <- function(x, y, colour, dashed, label) {
  htmltools::tagList(
    sc_flow_path(sprintf("M %.1f,%.1f L %.1f,%.1f", x, y, x + 36, y), colour, dashed = dashed, width = 2.8),
    htmltools::tags$text(x = x + 44, y = y, `text-anchor` = "start", `dominant-baseline` = "middle",
                         `font-size` = 14, `font-weight` = 500, fill = sc_flow_cols$text, label)
  )
}

# The diagram itself. viewBox is 1100 x 540 so it scales cleanly in the card
# and in full-screen (viewBox 1100 x 560); a CSS min-width keeps type legible on a narrow phone.
sc_flow_svg <- function() {
  c <- sc_flow_cols
  r <- 26
  halo_r <- 46
  y_main <- 86
  x <- c(kit = 90, int = 310, func = 530, env = 750, acc = 980)
  # Vertical positions of the labelled rows, used to start and stop routes
  # in the gaps under those words rather than on the node itself.
  y_lab_main <- y_main + r + 14          # first line under a top-row node
  y_lab_int  <- y_lab_main               # same first-line height as the other top-row labels
  y_after_int <- y_lab_int + 56          # below "View settings", with a clear gap
  y_after_func <- y_lab_main + 28        # below "Functional test"
  y_after_env  <- y_lab_main + 46        # below the Environmental test subtitle
  y_after_acc  <- y_lab_main + 44        # below "+ ship"
  y_fail <- 242
  y_review <- 318
  y_after_review <- y_review + r + 34    # below "Review"
  y_rework <- 408
  y_scrap <- 478
  x_join <- unname(x[["int"]])          # shared vertical into Integration
  star_r <- 10

  # Forward arrows stop a little short of the next node so the tip just touches.
  tip <- 10
  fwd <- function(x1, x2, right_clearance, left_clearance) {
    sc_flow_path(
      sprintf("M %.2f,%.2f L %.2f,%.2f", x1 + right_clearance, y_main, x2 - left_clearance - tip, y_main),
      c$forward, marker_end = "sc-flow-arrow-forward"
    )
  }

  htmltools::tags$svg(
    class = "sc-flow-svg",
    viewBox = "0 0 1100 560",
    fill = "currentColor",
    role = "img",
    `aria-labelledby` = "sc-flow-title sc-flow-desc",
    xmlns = "http://www.w3.org/2000/svg",
    `xmlns:xlink` = "http://www.w3.org/1999/xlink",
    htmltools::tags$title(id = "sc-flow-title", "How a unit moves through the line"),
    htmltools::tags$desc(
      id = "sc-flow-desc",
      paste("Kitting to Integration to Functional test to Environmental test to Final acceptance and ship.",
            "A failed functional test returns to Integration. A failed environmental test goes to Review.",
            "From Review a unit may be accepted and sent to Final acceptance, sent back to Integration for rework,",
            "or scrapped. After two test failures a unit goes straight to Review and cannot be sent back for rework.")
    ),
    htmltools::tags$defs(
      sc_flow_marker("sc-flow-arrow-forward", c$forward),
      sc_flow_marker("sc-flow-arrow-fail", c$fail),
      sc_flow_marker("sc-flow-arrow-accepted", c$accepted)
    ),
    htmltools::tags$g(
      `font-family` = "Lato, 'Segoe UI', 'Helvetica Neue', Arial, sans-serif",
      # Halo first so routes and the Integration node sit on top of it.
      htmltools::tags$circle(cx = x[["int"]], cy = y_main, r = halo_r, fill = c$halo),
      # --- routes (under the nodes) ------------------------------------------
      # Main forward line.
      fwd(x[["kit"]], x[["int"]], r, halo_r),
      fwd(x[["int"]], x[["func"]], halo_r, r),
      fwd(x[["func"]], x[["env"]], r, r),
      fwd(x[["env"]], x[["acc"]], r, r),
      # Functional-test fail, left to the shared vertical (no arrowhead here).
      sc_flow_path(
        sc_flow_ortho_d(rbind(c(x[["func"]], y_after_func),
                              c(x[["func"]], y_fail),
                              c(x_join, y_fail)), radius = 18),
        c$fail, dashed = TRUE, id = "sc-flow-fail-func"
      ),
      # Rework: one-way out of Review to the shared vertical (no arrow into Review).
      sc_flow_path(
        sc_flow_ortho_d(rbind(c(x[["env"]] - r, y_review),
                              c(x[["env"]] - 36, y_review),
                              c(x[["env"]] - 36, y_rework),
                              c(x_join, y_rework)), radius = 18),
        c$fail, dashed = TRUE, id = "sc-flow-rework"
      ),
      # Shared vertical: the only arrowhead on the fail/rework loop, into Integration.
      sc_flow_path(
        sprintf("M %.2f,%.2f L %.2f,%.2f", x_join, y_rework, x_join, y_after_int + tip),
        c$fail, dashed = TRUE, marker_end = "sc-flow-arrow-fail", id = "sc-flow-into-integration"
      ),
      # Environmental-test fail, into Review only.
      sc_flow_path(
        sprintf("M %.2f,%.2f L %.2f,%.2f", x[["env"]], y_after_env, x[["env"]], y_review - r - tip),
        c$fail, dashed = TRUE, marker_end = "sc-flow-arrow-fail", id = "sc-flow-fail-env"
      ),
      # Accepted: the only path from Review to Final acceptance.
      sc_flow_path(
        sc_flow_ortho_d(rbind(c(x[["env"]] + r, y_review),
                              c(x[["acc"]], y_review),
                              c(x[["acc"]], y_after_acc + tip)), radius = 18),
        c$accepted, dashed = TRUE, marker_end = "sc-flow-arrow-accepted", id = "sc-flow-accepted"
      ),
      # Scrap: dashed red, ends at the hollow circle, no arrowhead anywhere.
      sc_flow_path(
        sprintf("M %.2f,%.2f L %.2f,%.2f", x[["env"]], y_after_review, x[["env"]], y_scrap - 16),
        c$scrap, dashed = TRUE, id = "sc-flow-scrap"
      ),
      # --- nodes --------------------------------------------------------------
      sc_flow_node(x[["kit"]],  y_main, r, c$forward, star_r = star_r),
      sc_flow_node(x[["int"]],  y_main, r, c$forward, star_r = star_r),
      sc_flow_node(x[["func"]], y_main, r, c$forward, star_r = star_r),
      sc_flow_node(x[["env"]],  y_main, r, c$forward, star_r = star_r),
      sc_flow_node(x[["acc"]],  y_main, r, c$forward, star_r = star_r),
      sc_flow_node(x[["env"]],  y_review, r, c$review, star_r = star_r),
      # Scrap is a hollow circle, not a star, and is the end of the red line.
      htmltools::tags$circle(cx = x[["env"]], cy = y_scrap, r = 15,
                             fill = c$paper, stroke = c$scrap, `stroke-width` = 2.8,
                             id = "sc-flow-scrap-node"),
      # --- labels -------------------------------------------------------------
      sc_flow_label(x[["kit"]],  y_main + r + 14, "Kitting"),
      sc_flow_label(x[["int"]],  y_lab_int, list(
        list(text = "Integration", size = 16, weight = 700, colour = c$text, href = NULL),
        list(text = "View settings", size = 13, weight = 600, colour = c$forward, href = "#sc-variant-settings")
      )),
      sc_flow_label(x[["func"]], y_main + r + 14, "Functional test"),
      sc_flow_label(x[["env"]],  y_main + r + 14, list(
        list(text = "Environmental test", size = 16, weight = 700, colour = c$text, href = NULL),
        list(text = "Vibration \u00b7 Thermal vacuum", size = 12, weight = 500, colour = c$muted, href = NULL)
      )),
      sc_flow_label(x[["acc"]],  y_main + r + 14, list(
        list(text = "Final acceptance", size = 16, weight = 700, colour = c$text, href = NULL),
        list(text = "+ ship", size = 15, weight = 700, colour = c$text, href = NULL)
      )),
      sc_flow_label(x[["env"]],  y_review + r + 12, "Review"),
      sc_flow_label(x[["env"]],  y_scrap + 18, "Scrap"),
      sc_flow_path_label((x[["func"]] + x_join) / 2, y_fail, "Fail"),
      sc_flow_path_label(x[["env"]], (y_after_env + y_review - r) / 2, "Fail"),
      sc_flow_path_label((x_join + x[["env"]]) / 2, y_rework, "Rework"),
      sc_flow_path_label((x[["env"]] + r + x[["acc"]]) / 2, y_review, "Accepted"),
      # --- legend -------------------------------------------------------------
      {
        items <- list(
          list(colour = c$forward,  dashed = FALSE, label = "Forward"),
          list(colour = c$accepted, dashed = TRUE,  label = "Accepted"),
          list(colour = c$fail,     dashed = TRUE,  label = "Rework"),
          list(colour = c$scrap,    dashed = TRUE,  label = "Scrap")
        )
        # Four items, centred as a group in the 1100-wide viewBox.
        widths <- c(108, 118, 108, 96)
        gap <- 48
        total <- sum(widths) + gap * (length(items) - 1)
        left <- (1100 - total) / 2
        xs <- left + c(0, cumsum(widths[-length(widths)] + gap))
        htmltools::tagList(
          purrr::pmap(list(xs, items), function(xi, it) sc_flow_legend_item(xi, 528, it$colour, it$dashed, it$label))
        )
      }
    )
  )
}

# The flow card body: responsive SVG, plus the caption that spells out the
# rule the picture leaves out (the two-failure cap).
sc_flow_ui <- function() {
  htmltools::tagList(
    htmltools::tags$style(htmltools::HTML("
      .sc-flow-wrap { width: 100%; overflow-x: auto; -webkit-overflow-scrolling: touch; }
      /* min-width keeps the 16px viewBox type around 12px on a phone; the
         card and full-screen view then scroll sideways rather than shrinking
         the labels to unreadable. */
      .sc-flow-svg { width: 100%; height: auto; min-width: 800px; display: block; }
      .card[data-full-screen='true'] .sc-flow-wrap { overflow: auto; }
      .card[data-full-screen='true'] .sc-flow-svg {
        min-width: 800px; width: auto; max-width: 100%; height: calc(100vh - 240px);
        margin: 0 auto;
      }
    ")),
    htmltools::div(class = "sc-flow-wrap", sc_flow_svg()),
    htmltools::tags$p(
      class = "mt-2 mb-0",
      "Every loop in the line returns to Integration. The picture leaves out one rule: once a unit has",
      "failed a test twice, it goes straight to the material review board. At that point the board can't",
      "send it back for rework, so the unit is either used as is or scrapped."
    )
  )
}
