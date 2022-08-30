##############################################
# Valitsuse stiiliraamatust lähtuv kujundus ggplot2-le
#
# Eeskujusid on olnud mitmeid:
# https://github.com/r4ecology/ggplot2-theme
# https://github.com/houseofcommonslibrary/clcharts
# Nimekirja teemadest leiab:
# https://github.com/jmcastagnetto/ggplot2_themes_in_github
#
# (c) 2021 Raoul Lättemäe
##############################################

#' Add Estonian Government theme to ggplot chart
#'
#' This function allows you to add the Estonian Government theme to your ggplotgraphics.
#' @keywords theme_fpo
#' @export
#' @examples
#' line <- ggplot(line_df, aes(x = year, y = lifeExp)) +
#' geom_line(colour = "#007f7f", size = 1) +
#' geom_hline(yintercept = 0, size = 1, colour="#333333") +
#' theme_fpo() + scale_color_fpo()

theme_fpo <- function(
  base_size = 14,
  base_family = "Roboto Condensed",
  lines_lwd = 0.50,
  plot_grid = TRUE,
  axis_font = base_family,
  title_size = base_size*1.5,
  legend_size = base_size,
  bg_col = "white",
  title_font = base_family,
  title_col = "#006db5",
  base_col  = "black",
  axis_lines = TRUE,
  minor_grid = ifelse(plot_grid, TRUE, FALSE),
  vert_grid = ifelse(plot_grid, TRUE, FALSE),
  ticks_type = "outer",
  horz_grid = ifelse(plot_grid, TRUE, FALSE),
  alpha_leg = 0.1, bord_size = 0,
  legend_bg = "white",
  strip_bg = "white",
  grid_thick = 1,
  grid_type = "solid",
  ticks_xy  = "xy",
  grid_cols = c("#dad5d1", "#f2ede9")){
  theme_bw()+
    ggplot2::theme(
      plot.margin = grid::unit(c(1, 1, .5, .7), "cm"),
      text = ggplot2::element_text(family = base_family, size = base_size),
      axis.line =  element_line(size = ifelse(axis_lines, grid::unit(lines_lwd, "mm"),0), color = "black"),
      axis.ticks.length = grid::unit(ifelse(ticks_type == "outer", 0.15, -0.15), "cm"),
      axis.ticks.x =  element_line(size = ifelse(stringr::str_detect(ticks_xy, "x"), grid::unit(lines_lwd, "cm"),0), color = "black"),
      axis.ticks.y =  element_line(size = ifelse(stringr::str_detect(ticks_xy, "y"), grid::unit(lines_lwd, "cm") ,0), color = "black"),
      axis.text.x = ggplot2::element_text(size = base_size, colour = base_col , family = axis_font,margin=margin(ifelse(ticks_type == "inner", 11, 5),5,10,5,"pt")),
      axis.text.y = ggplot2::element_text(size = base_size, colour = base_col , family = axis_font, margin=margin(5,ifelse(ticks_type == "inner", 11, 5),10,5,"pt")),
      axis.title.y = ggplot2::element_text(size =  base_size, colour = base_col , vjust = 1.5, family = axis_font),
      axis.title.x = ggplot2::element_text(size = base_size,colour = base_col ,vjust = -.5, family = axis_font),
      panel.background = ggplot2::element_rect(fill = bg_col),
      plot.background = ggplot2::element_rect(fill = bg_col),
      panel.border = ggplot2::element_rect(colour = "black", fill=NA, size = bord_size),
      panel.grid.major.x = ggplot2::element_line(linetype = grid_type,colour = ifelse(vert_grid, grid_cols[1],bg_col), size = ifelse(vert_grid,0.25 * grid_thick, 0)),
#      panel.grid.minor.x = ggplot2::element_line(linetype = grid_type,colour = ifelse(vert_grid, ifelse(minor_grid, grid_cols[2 - (length(grid_cols) == 1)   ],bg_col),bg_col), size = ifelse(vert_grid,0.15* grid_thick, 0)),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(linetype = grid_type,colour = ifelse(horz_grid, grid_cols[1],bg_col), size = ifelse(horz_grid,0.25* grid_thick, 0)),
#      panel.grid.minor.y = ggplot2::element_line(linetype = grid_type,colour = ifelse(horz_grid, ifelse(minor_grid, grid_cols[2 - (length(grid_cols) == 1)  ],bg_col),bg_col), size = ifelse(horz_grid,0.15* grid_thick, 0)),
      plot.title = ggplot2::element_text(face="bold", vjust = 2, colour = title_col , size = title_size, family = title_font),
      plot.subtitle = ggplot2::element_text(vjust = 2, colour = title_col, size = base_size, family = title_font),
      plot.caption = ggplot2::element_text(hjust = 0, colour = "#999999", margin = ggplot2::margin(
        t = 18,
        r = 0,
        b = 0,
        l = 0, unit = "pt")),
      plot.caption.position = "plot",
      plot.title.position = "plot",
      plot.tag = ggplot2::element_blank(),
#      plot.tag = ggplot2::element_text(hjust = 1, colour = "#999999", size = base_size*0.8),
#      plot.tag.position = "bottomleft",
      legend.background = ggplot2::element_rect(fill = scales::alpha(legend_bg, alpha_leg)), legend.key = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = legend_size, family = base_family),
#      legend.position = "bottom",
#      legend.title = element_blank(),
      strip.background =  ggplot2::element_rect(colour = strip_bg, fill = strip_bg),
      strip.text.x = ggplot2::element_text(size = base_size + 1),
      strip.text.y = ggplot2::element_text(size = base_size + 1)
    )
}
