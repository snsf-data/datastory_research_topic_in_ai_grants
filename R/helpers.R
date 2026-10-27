#' Make a plot interactive with `ggiraph`
#'
#' @param x The ggplot to make interactive.
#' @param h The height of the SVG generated.
#' @param sw The width of the stroke.
#' @param fcolor The color (fill) to use when hovering on interactive parts.
#' @param scolor The color (stroke) to use when hovering on interactive parts.
#'
#' @returns An `ggiraph` interactive plot

make_ggiraph <- function(
  x,
  h = 4,
  w = 7,
  sw = 2,
  fcolor = "#f6685e",
  scolor = "#f6685e"
) {
  girafe(
    ggobj = x,
    height_svg = h,
    width_svg = w,
    options = list(
      opts_toolbar(hidden = c("saveaspng", "fullscreen")),
      opts_hover(
        css = girafe_css(
          # fmt: skip
          css = paste0(
            "fill:", fcolor, ";stroke:", scolor, ";stroke-width:", sw, ";"
          ),
          text = "stroke:none; color:blue;"
        )
      ),
      opts_tooltip(
        css = get_ggiraph_tooltip_css(family = font),
        opacity = 0.8,
        delay_mouseover = 0,
        delay_mouseout = 0
      )
    )
  )
}