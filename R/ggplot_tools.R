# Graydon theming ----
extrafont::font_import(pattern = "Roboto", prompt = FALSE)
extrafont::loadfonts(device="win")

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

#' Saving a plot to a png, ready for use in a PowerPoint
#'
#' @param plot the ggplot, stored as a variable, which you want to save
#' @param file_name the filename, excluding the extension, you ant to save the plot to
#' @param squared logical indicating whether the plot is squared or landscape,
#' conform to side by side side slide (squared = TRUE) or
#' One graph in a slide (squared = FALSE)
#' @keywords ggplot2
#' @export
#' @examples
#' p < ggplot(data = mtcars,
#'            mapping = aes(x = wt, y = mpg, color = as.factor(cyl))) +
#'       geom_point()
#' save_plot_to_png(plot = p, file_name = "mtcars")
save_plot_to_png <- function(plot, file_name, squared = FALSE) {

  file_name <- paste0(file_name, ".png")

  if(squared) {

    png(
      file =  file_name,
      type = "cairo",
      bg = 'transparent',
      units = "cm",
      width = 14.39,
      height = 12.09,
      pointsize = 18,
      res = 300
    )

    invisible(print({plot}))

    invisible(dev.off())

  } else {

    png(
      file =  file_name,
      type = "cairo",
      bg = 'transparent',
      units = "cm",
      width = 29.21,
      height = 12.09,
      pointsize = 18,
      res = 300
    )

    print({plot})

    invisible(dev.off())

  }
}
