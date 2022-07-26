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


lwi_theme <- function(base_size = 12, base_family = "sans",
                      horizontal = TRUE, dkpanel = FALSE) {

  lwigrey <- "#4d4d4d"

  ## From measurements
  ## Ticks = 1 / 32 in, with margin about 1.5 / 32
  ## Title = 3 / 32 in (6 pt)
  ## Legend Labels = 2.5 / 32 in (5pt)
  ## Axis Labels = 2
  ## Axis Titles and other text ~ 2
  ## Margins: Top / Bottom = 6 / 32, sides = 5 / 32
  ret <-
    theme_foundation(base_size = base_size, base_family = base_family) +
    theme(line = element_line(colour = lwigrey),
          rect = element_rect(fill = "white", colour = NA,
                              linetype = 1),
          text = element_text(colour = "black"),
          ## Axis
          axis.line = element_line(size = rel(1)),
          axis.text = element_text(size = rel(1)),
          axis.text.x = element_text(vjust = 0,
                                     margin = margin(t = base_size,
                                                     unit = "pt")),
          axis.text.x.top = element_text(vjust = 0, margin = margin(b = base_size, unit = "pt")),
          axis.text.y = element_text(hjust = 0,
                                     margin = margin(r = base_size,
                                                     unit = "pt")),
          ## I cannot figure out how to get ggplot to do 2 levels of ticks
          ## axis.ticks.margin = unit(3 / 72, "in"),
          axis.ticks = element_line(),
          axis.title = element_text(size = rel(1)),
          axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"), face = "bold"),
          axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"), face = "bold",
                                      angle = 90),
          # axis.ticks.length = unit( -1/32, "in"),
          axis.ticks.length = unit(base_size * 0.5, "points"),
          legend.background = element_rect(linetype = 0),
          legend.spacing = unit(base_size * 1.25, "points"),
          legend.key = element_rect(linetype = 0),
          legend.key.size = unit(1.2, "lines"),
          legend.key.height = NULL,
          legend.key.width = NULL,
          legend.text = element_text(size = rel(1.25)),
          legend.text.align = NULL,
          legend.title = element_text(size = rel(1),  hjust = 0),
          legend.title.align = NULL,
          legend.position = "top",
          legend.direction = NULL,
          legend.justification = "center",
          ## legend.box = element_rect(fill = palette_economist['bgdk'],
          ## colour=NA, linetype=0),
          ## Economist only uses vertical lines
          panel.background = element_rect(fill = "white",
                                          colour = lwigrey, linetype = 1),
          panel.border = element_blank(),
          panel.grid.major = element_line(colour = lwigrey, size = rel(1)),
          panel.grid.minor = element_line(colour = lwigrey, size = rel(1), linetype = "dotted"),
          panel.spacing = unit(0.25, "lines"),
          strip.background = element_rect(fill = "white",
                                          colour = NA, linetype = 0),
          strip.text = element_text(size = rel(1.25)),
          strip.text.x = element_text(),
          strip.text.y = element_text(angle = -90),
          plot.background = element_rect(fill = "white",
                                         colour = NA),
          plot.title = element_text(size = rel(1.5),
                                    hjust = 0, face = "bold"),
          plot.margin = unit(c(6, 5, 6, 5) * 2, "points"),
          complete = TRUE)

  ret
}
