
#' plotLoadProfile_ggplot
#' 
#' @param dat data...
#' @return \code{NULL}
#' @export

plotHeatmap_8760 <- function (dat, var, normVar, inclWeekends, zRng) {
  
  if(!inclWeekends) dat <- filter(dat, dow %in% 2:6)
  
  plot_dat <- dat %>%
    group_by(doy,hour) %>%
    summarise_(var = lazyeval::interp(~mean(var, na.rm = TRUE), var = as.name(var)))
  
  if (normVar != 'none') plot_dat <- plot_dat %>% group_by_(normVar) %>% mutate(var = var / max(var, na.rm=TRUE))
  
  p <- ggplot(dat, aes(hour, doy)) +
    geom_tile(aes_string(fill = var), colour = "white") +
    scale_fill_gradientn(colours = c("#FFE97E", "#FF7800", "#700000"), limits=zRng, oob = squish) +
    scale_x_discrete(expand = c(0,0), limits=0:23, breaks=seq(0,23,by=2)) +
    scale_y_continuous(expand = c(0,0), limits=c(366,1), trans='reverse', breaks=seq(10,360,by=10)) +
    ylab('day of year') +
    theme(legend.position = "top", legend.direction="horizontal")
  
  print(p)
  
  invisible(NULL)
}
