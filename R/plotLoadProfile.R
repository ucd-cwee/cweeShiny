
#' plotLoadProfile_ggplot
#' 
#' @param dat data...
#' @param type type...
#' @param color color...
#' @param labelMax labelMax...
#' @return \code{NULL}
#' @export

plotLoadProfile_ggplot <- function (dat, type, color, labelMax) {
  
  if (type == "Weekday") {
    days <- c(2,3,4,5,6)
  } else if (type == "Weekend") {
    days <- c(1,7)
  }
  
  plot_dat <- dat %>%
    filter(dow %in% days) %>%
    group_by(hour) %>%
    summarize(kW = mean(kW, na.rm=TRUE))
  
  plot_dat_all <- dat %>%
    filter(dow %in% days) %>%
    group_by(year,season,month,week,day,hour) %>%
    summarize(kW = mean(kW, na.rm=TRUE)) %>%
    mutate(id = paste0(year,week,day))
  
  maxVal <- ungroup(plot_dat_all) %>%
    filter(kW == max(kW, na.rm=TRUE)) %>%
    mutate(lab = format(ymd_hms(paste(year,month,day,hour,0,0, sep='-')), "%I %p - %b %d, %Y"))
  
  p <- ggplot(plot_dat, aes(hour, kW))
  if (color != 'none') p <- p + geom_line(data=plot_dat_all, aes_string("hour", "kW", group="id", color=color))
  if (color == 'none') p <- p + geom_line(data=plot_dat_all, aes(hour, kW, group=id), color="gray")
  p <- p + geom_line(size=2) +
    geom_point(size=4) +
    expand_limits(y=0) +
    ylab("kW") +
    ggtitle(paste(type, 'Average')) +
    theme(plot.title=element_text(face="bold", size=20))
  if (labelMax) p <- p + geom_point(data=maxVal, size=3) +
    geom_text(data=maxVal, aes(label=lab), vjust=1.2, size=6, fontface=2)
  
  print(p)
  
  invisible(NULL)
}

#' plotLoadProfile_dygraph.
#' 
#' @param dat data...
#' @param type type...
#' @return \code{NULL}
#' @export

plotLoadProfile_dygraph <- function (dat, type) {
  
  if (type == "Weekday") {
    days <- c(2,3,4,5,6)
  } else if (type == "Weekend") {
    days <- c(1,7)
  }
  
  plot_dat <- dat %>%
    filter(dow %in% days) %>%
    group_by(hour) %>%
    summarize(kW = mean(kW, na.rm=TRUE))
  
  plot_dat_all <- dat %>%
    filter(dow %in% days) %>%
    group_by(year,season,month,week,day,hour) %>%
    summarize(kW = mean(kW, na.rm=TRUE)) %>%
    mutate(id = paste0(year,week,day))
  
  plot_dat_ts <- setNames(xts(as.matrix(plot_dat$kW), order.by = as.POSIXct(strptime(paste(1,1,1,plot_dat$hour), format = "%Y %m %d %H"))), 'kW_mean')
  
  yrng <- ggplot_build(qplot(hour, kW, data=plot_dat_all))$panel$ranges[[1]]$y.range
  
  dygraph(plot_dat_ts, main = paste(type, 'Average'), group = "plotLoadProfile") %>%
    dyAxis(name='x', label='hour') %>%
    dyAxis(name='y', label='kW', valueRange=yrng) %>%
    dyOptions(fillGraph = TRUE, fillAlpha = 0.4, drawPoints = TRUE, pointSize = 2, axisLineWidth = 1.5)
  
}
