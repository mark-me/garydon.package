# Graydon theming ----
extrafont::font_import(pattern = "Roboto", prompt = FALSE)
extrafont::loadfonts(device="win")

#' #' A vector with the Graydon color palette
#' #' @export
#' col_graydon <- c("#00545D", "#EB6E08", "#858587",
#'                  "#8BB0DE", "#186FA7", "#474646",
#'                  "#BABFC1", "#000000", "#3AA2DF")
#' #' Color from the Graydon palette representing a low value
#' #' @export
#' col_graydon_low <- col_graydon[4]
#' #' Color from the Graydon palette representing a high value
#' #' @export
#' col_graydon_high <- col_graydon[2]
#' #' Color from the Graydon palette for the grid
#' #' @export
#' col_graydon_grid <- col_graydon[7]
#' #' Color from the Graydon palette for the axes
#' #' @export
#' col_graydon_axis <- col_graydon[6]
#' devtools::use_data(col_graydon_axis)

#' A function for applying a Graydon theme to a ggplot
#'
#' This function applies Graydon theme to a ggplot.
#' The first time it is used it will create a column specification file
#' allows you to create and or read a file which you can use to control a data file import
#' @param type The type of grid or background
#' @keywords ggplot2
#' @export
#' @examples
#' theme_graydon("grid")
theme_graydon <- function(type = c("grid", "horizontal", "vertical", "blank")) {

  graydon_theme <-
    ggthemes::theme_gdocs() +
    theme(axis.title = element_text(face = "plain"),
          panel.grid.major = element_line(colour = col_graydon_grid),
          plot.background = element_blank(),
          axis.line = element_line(colour = col_graydon_axis),
          text = element_text(family = "Roboto Medium",
                              color = col_graydon_axis)
    )

  if (type == "horizontal") {

    graydon_theme <- graydon_theme +
      theme(panel.grid.major.x = element_blank(),
            axis.line.y = element_blank()
      )

  } else if (type == "vertical") {

    graydon_theme <- graydon_theme +
      theme(panel.grid.major.y = element_blank(),
            axis.line.x = element_blank()
      )

  } else if (type == "blank") {

    graydon_theme <- ggthemes::theme_gdocs() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.line.x = element_blank(),
            axis.line.y = element_blank(),
            rect = element_blank(),
            text = element_text(family = "Roboto Medium",
                                col_graydon_axis)
      )

  }
  return(graydon_theme)
}

#' A function to include in a ggplot so the col aesthetics make use of the Graydon color palette
#'
#' @keywords ggplot2
#' @export
scale_color_graydon <- function(){
  return(scale_color_manual(values = col_graydon))
}

#' A function to include in a ggplot so the fill aesthetics make use of the Graydon color palette
#'
#' @keywords ggplot2
#' @export
scale_fill_graydon <- function(){
  return(scale_fill_manual(values = col_graydon))
}

#' A function to include in a ggplot so color and fill aesthetics make use of the Graydon color palette
#'
#' @keywords ggplot2
#' @export
scale_gradient_graydon <- function(){
  return(scale_fill_gradient(low = col_graydon_low, high = col_graydon_high) +
           scale_color_gradient(low = col_graydon_low, high = col_graydon_high))
}
