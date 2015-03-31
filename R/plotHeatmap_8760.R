
#' plotLoadProfile_ggplot
#' 
#' @param dat data...
#' @return \code{NULL}
#' @export

plotHeatmap_8760 <- function (dat) {
  
  p <- ggplot(dat, aes(hour, doy)) +
    geom_tile(aes(fill = kW), colour = "white") +
    scale_fill_gradientn(colours = c("#FFE97E", "#FF7800", "#700000")) +
    scale_x_discrete(expand = c(0,0), limits=0:23, breaks=seq(0,23,by=2)) +
    scale_y_continuous(expand = c(0,0), limits=c(366,1), trans='reverse', breaks=seq(10,360,by=10)) +
    ylab('day of year')
  
  print(p)
  
  invisible(NULL)
}
