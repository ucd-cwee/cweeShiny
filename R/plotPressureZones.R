#' mapControls_ggplot
#' 
#' @import shiny
#' @import shinydashboard
#' @export

mapControls_ggplot <- function() {
    radioButtons("map_var", "Map Variable:",
                 c("Water Consumption (MG)" = "Consumption",,
                   "Energy Intensity (kWh/MG)" = "Energy_Intensity",
                   "Embedded Energy (kWh/year)" = "Embedded_Energy"))
}

#' pzPlot_ggplot
#' 
#' @import ggplot2
#' @import scales
#' @export

pzPlot_ggplot <- function(dat, var, label, clrs) {
  
  p <- ggplot(dat, aes(long, lat, group=group)) +
    coord_equal() +
    scale_x_continuous(expand = c(0.01, 0.01)) +
    scale_y_continuous(expand = c(0.01, 0.01)) +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.background = element_blank(),
          legend.title = element_text(size = 18, face = "bold"),
          legend.text = element_text(size = 16))
  
  if (!missing(var)) p <- p + geom_polygon(aes_string(fill=as.character(var)))
  lab <- ifelse(missing(label), ifelse(missing(var), NULL, as.character(var)), as.character(label))
  if (!missing(clrs)) p <- p + scale_fill_gradientn(name=lab, colours=clrs, na.value = 'gray50', guide = 'colourbar', labels=comma)
  
  p <- p + geom_path(color="black")
  
  p
}
