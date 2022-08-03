#' generate basic relief maps
#'
#' generate.map generates a ggplot map visualizing sf attributes over shaded relief.
#' It gives you a basic map so you don't have to deal with the nitty gritty and can just
#' focus on customizing the aesthetic.
#'
#' @name generate.map
#'
#' @param data output of overlay.relief
#' @param x name of the longitude variable in the overlay.relief output as character vector
#' @param y name of the latitude variable in the overlay.relief output as character vector
#' @param hillshade name of the hillshade variable in the overlay.relief output as character vector
#' @param variable name of the variable in the overlay.relief output you wish to map as character vector
#' @param axis_degrees if TRUE axis unit and labels will be in degrees (in accordance with the CRS of your data)
#' @return ggplot object
#' @export


generate.map <- function(data,
                         x,
                         y,
                         hillshade,
                         variable,
                         axis_degrees = TRUE) {
    cs <- data[["EPSG"]]
    data <- data[["relief"]]

    plot <- ggplot2::ggplot() +
      ggplot2::geom_raster(ggplot2::aes(
        x = data[, x],
        y = data[, y],
        alpha = data[, hillshade],
        fill = data[, variable]
      ),
      interpolate = TRUE) +
      ggplot2::labs(fill = variable) +
      ggplot2::scale_alpha(name = "",
                           range = c(1, 0.6),
                           guide = F) +
      ggplot2::theme(
        legend.position = "bottom",
        axis.title = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(colour = "black", fill = NA),
        text = ggplot2::element_text(size = 15, family = "serif")
      )

    if (axis_degrees == TRUE) {
      plot <- plot +
        ggplot2::coord_sf(crs = cs)
    }

    return(plot)
  }
