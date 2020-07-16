#' Adds tags to facets
#'
#' @export
facet_tags <- function(position = "tl", tag_pool = letters, open = "", close = ")", stroke = 0.15, stroke.color = "white",
                 color = "black") {
  if (!is.list(position)) {
    position <- switch(position,
                       tl = list(x = 0.05, y = 0.95, hjust = 0, vjust = 1),
                       tr = list(x = 0.95, y = 0.95, hjust = 1, vjust = 1),
                       bl = list(x = 0.05, y = 0.05, hjust = 0, vjust = 0),
                       br = list(x = 0.95, y = 0.05, hjust = 1, vjust = 0),
                       position)
  }


  structure(list(tag_pool = tag_pool, open = open, close = close, position = position,
                 color = color,
                 stroke = stroke, stroke.color = stroke.color),
            class = "ggtags")
}

#' @export
ggplot_add.ggtags <- function(object, plot, object_name) {
  class(plot) <- c("ggtagged", class(plot))
  attr(plot, "tag_options") <- object
  plot
}

#' @export
print.ggtagged <- function(x, newpage = is.null(vp), vp = NULL, ...) {
  ggplot2::set_last_plot(x)
  if (newpage) grid::grid.newpage()

  # Record dependency on 'ggplot2' on the display list
  # (AFTER grid.newpage())
  grDevices::recordGraphics(
    requireNamespace("ggplot2", quietly = TRUE),
    list(),
    getNamespace("ggplot2")
  )

  tag_options <- attr(x, "tag_options")
  gb <- ggplot2::ggplot_build(x)
  lay <- gb$layout$layout

  x = tag_options$position$x
  y = tag_options$position$y

  hjust = tag_options$position$hjust
  vjust = tag_options$position$vjust

  gt <- ggplot2::ggplot_gtable(gb)
  panels <- which(grepl("panel", gt$layout$name))
  for (p in seq_along(panels)) {

    if (tag_options$stroke == 0) {
      tagGrob <- grid::textGrob(paste0(tag_options$open, tag_options$tag_pool[p], tag_options$close),
                                            x = x, y = y, gp = grid::gpar(col = tag_options$color),
                                            hjust = hjust, vjust = vjust)
    } else {
      tagGrob <- shadowtext::shadowtextGrob(paste0(tag_options$open, tag_options$tag_pool[p], tag_options$close),
                                            x = x, y = y, bg.r = tag_options$stroke, bg.colour = tag_options$stroke.color,
                                            gp = grid::gpar(col = tag_options$color),
                                            hjust = hjust, vjust = vjust)
    }

    this_panel <- gt$layout[panels[p], ]

    gt <- gtable::gtable_add_grob(gt, tagGrob, t = this_panel$t, l = this_panel$l, clip = "off")

  }


  if (is.null(vp)) {
    grid::grid.draw(gt)
  } else {
    if (is.character(vp)) grid::seekViewport(vp) else grid::pushViewport(vp)
    grid::grid.draw(gt)
    grid::upViewport()
  }

  invisible(x)
}

#' @export
plot.ggtagged <- print.ggtagged

