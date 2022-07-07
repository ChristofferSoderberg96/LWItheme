#' Learning and Work Institute colour palette (discrete)
#' Authored by Paul Bivand
#' The hues in the palette are oranges and blues
#'
#' @param fill Use the fill palette
#' @family colour lwi
#' @export

lwi_pal <-function (fill = TRUE)
{
  {
    #colors <- ggthemes_data$economist$fg
    if (fill) {
      function(n) {
        if (n == 1) {
          i <- "#ee7e3b"
        }
        else if (n == 2) {
          i <- c("#264c59", "#ee7e3b")
        }
        else if (n == 3) {
          i <- c("#f6c65b", "#ee7e3b", "#264c59")
        }
        else if (n == 4) {
          i <- c("#f6c65b", "#ee7e3b", "#264c59",
                 "#3fbfad")
        }
        else if (n %in% 5:6) {
          i <- c("#f6c65b", "#ee7e3b", "#5d3754",
                 "#264c59", "#3fbfad", "#a02140")
        }
        else if (n == 7) {
          i <- c("#f6c65b", "#ee7e3b", "#264c59",
                 "#5d3754", "#a02140", "#3fbfad", "#77B7D4")
        }
        else if (n >= 8) {
          i <- c("#f6c65b", "#ee7e3b", "#264c59",
                 "#5d3754", "#a02140",
                 "#FD9102", "#3fbfad", "#77B7D4", "#0086D4")
        }
        #        unname(colors[i][seq_len(n)])
      }
    }
    else {
      function(n) {
        if (n <= 3) {
          i <- c("#ee7e3b", "#264c59", "#5d3754")
        }
        else if (n %in% 4:5) {
          i <- c("#f6c65b", "#ee7e3b", "#5d3754",
                 "#264c59", "#3fbfad")
        }
        else if (n == 6) {
          i <- c("#f6c65b", "#ee7e3b", "#264c59",
                 "#5d3754", "#a02140", "#3fbfad")
        }
        else if (n > 6) {
          i <- c("#f6c65b", "#ee7e3b", "#264c59",
                 "#5d3754", "#a02140", "#3fbfad", "#FD9102","#77B7D4", "#0086D4")
        }
        #        unname(colors[i][seq_len(n)])
      }
    }
  }
}

#' Learning and Work Institute colour scales
#'
#' @inheritParams lwi_pal
#' @family colour lwi
#' @rdname scale_lwi
#' @export

scale_colour_lwi <- function (stata = FALSE, ...)
{
  discrete_scale("colour", "lwi", lwi_pal(), ...)
}

#' @rdname scale_lwi
#' @export

scale_fill_lwi <- function (stata = FALSE, ...)
{
  discrete_scale("fill", "lwi", lwi_pal(), ...)
}
