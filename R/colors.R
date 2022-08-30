##############################################
# Valitsuse stiiliraamatust lähtuv kujundus ggplot2-le
#
# Eeskujusid on olnud mitmeid:
#
# https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2
#
# https://github.com/r4ecology/ggplot2-theme
# https://github.com/houseofcommonslibrary/clcharts
# Nimekirja teemadest leiab:
# https://github.com/jmcastagnetto/ggplot2_themes_in_github
#
# (c) 2021 Raoul Lättemäe
##############################################

# Colors
fpo_colors <- c(
  `red`        = "#3ecfe7",#"#d11141",
  `green`      = "#a2b969",#"#00b159",
  `blue`       = "#063951",#"#56B4E9",
  `orange`     = "#f36f13",#"#E69F00",
  `yellow`     = "#ebcb38",#"#ffc425",
  `light blue` = "#0d95bc",#"#ffc425",
  `light grey` = "#cccccc",
  `dark grey`  = "#8c8c8c")

#' Function to extract drsimonj colors as hex codes
#'
#' @param ... Character names of fpo_colors
#'
fpo_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (fpo_colors)

  fpo_colors[cols]
}

fpo_palettes <- list(
#  `main`  = fpo_cols("blue", "red", "light blue", "orange", "green", "yellow"),
  `main`  = fpo_cols("blue", "light blue", "yellow", "green", "red", "orange"),
  `cool`  = fpo_cols("blue", "light blue", "dark grey", "light grey", "green"),
  `hot`   = fpo_cols("red", "orange", "yellow", "green"),
  `mixed` = fpo_cols("blue", "light blue", "green", "yellow", "orange", "red", "dark grey"),
  `grey`  = fpo_cols("light grey", "dark grey")
)
#' Return function to interpolate a drsimonj color palette
#'
#' @param palette Character name of palette in fpo_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
fpo_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- fpo_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

#' Add Estonian Government theme to ggplot chart
#'
#' This function allows you to add the Estonian Government theme to your ggplotgraphics.
#' @keywords scale_color_fpo
#' @export
#' @examples
#' line <- ggplot(line_df, aes(x = year, y = lifeExp)) +
#' geom_line(colour = "#007f7f", size = 1) +
#' geom_hline(yintercept = 0, size = 1, colour="#333333") +
#' theme_fpo() + scale_color_fpo()
#' Color scale constructor for drsimonj colors
#'
#' @param palette Character name of palette in fpo_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_color_fpo <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- fpo_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("fpo_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Add Estonian Government theme to ggplot chart
#'
#' This function allows you to add the Estonian Government theme to your ggplotgraphics.
#' @keywords scale_fill_fpo
#' @export
#' @examples
#' line <- ggplot(line_df, aes(x = year, y = lifeExp)) +
#' geom_line(colour = "#007f7f", size = 1) +
#' geom_hline(yintercept = 0, size = 1, colour="#333333") +
#' theme_fpo() + scale_fill_fpo()
#' Fill scale constructor for drsimonj colors
#'
#' @param palette Character name of palette in fpo_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_fpo <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- fpo_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("fpo_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
