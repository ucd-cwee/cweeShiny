#' mapControls_ggplot
#' 
#' @import shiny
#' @import shinydashboard
#' @export

mapControls_ggplot <- function() {
    radioButtons("map_var", "Variable:",
                 c("Energy Intensity (kWh/MG)" = "EI_kWh_MG",
                   "Water Consumption (MG)" = "Consumption_MG",
                   "Energy Consumption (kWh)" = "Energy_kWh"))
}

#' pzPlot_ggplot
#' 
#' @import ggplot2
#' @export

pzPlot_ggplot <- function(dat, variable) {
  
  
  ggplot(dat, aes(long, lat, group=group)) +
    geom_path(color="black") +
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
          panel.background = element_blank()) +
    theme(legend.title = element_text(size = 18, face = "bold"),
          legend.text = element_text(size = 16))
}
