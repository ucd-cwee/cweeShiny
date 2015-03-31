
#' plotLoadProfile_ggplot
#' 
#' @param dat data...
#' @param type type...
#' @param color color...
#' @param labelMax labelMax...
#' @return \code{NULL}
#' @export

plotLoadProfile_ggplot <- function (dat, var, type, color, labelMax=FALSE, yRng) {
  
  if (type == "Weekday") {
    days <- c(2,3,4,5,6)
  } else if (type == "Weekend") {
    days <- c(1,7)
  }
  
  plot_dat <- dat %>%
    filter(dow %in% days) %>%
    group_by(hour) %>%
    summarise_(var = lazyeval::interp(~mean(var, na.rm = TRUE), var = as.name(var)))
  
  plot_dat_all <- dat %>%
    filter(dow %in% days) %>%
    group_by(year,season,month,week,day,hour) %>%
    summarise_(var = lazyeval::interp(~mean(var, na.rm = TRUE), var = as.name(var))) %>%
    mutate(id = paste0(year,week,day))
  
  maxVal <- ungroup(plot_dat_all) %>%
    filter(var == max(var, na.rm=TRUE)) %>%
    mutate(lab = format(ymd_hms(paste(year,month,day,hour,0,0, sep='-')), "%I %p - %b %d, %Y"))
  
  p <- ggplot(plot_dat, aes(hour, var))
  if (color != 'none') p <- p + geom_line(data=plot_dat_all, aes_string("hour", "var", group="id", color=color))
  if (color == 'none') p <- p + geom_line(data=plot_dat_all, aes(hour, var, group=id), color="gray")
  p <- p + geom_line(size=2) +
    geom_point(size=4) +
    ylab(var) +
    ggtitle(paste(type, 'Average')) +
    theme(plot.title=element_text(face="bold", size=20))
  if (labelMax) p <- p + geom_point(data=maxVal, size=3) +
    geom_text(data=maxVal, aes(label=lab), vjust=1.2, size=6, fontface=2)
  if (!missing(yRng)) p <- p + coord_cartesian(ylim=yRng)
  
  print(p)
  
  invisible(NULL)
}

#' plotLoadProfile_dygraph
#' 
#' @param dat data...
#' @param type type...
#' @return \code{NULL}
#' @export

plotLoadProfile_dygraph <- function (dat, var, type) {
  
  if (type == "Weekday") {
    days <- c(2,3,4,5,6)
  } else if (type == "Weekend") {
    days <- c(1,7)
  }
  
  plot_dat <- dat %>%
    filter(dow %in% days) %>%
    filter_(lazyeval::interp(~is.finite(var), var = as.name(var))) %>%
    group_by(hour) %>%
    summarise_(var = lazyeval::interp(~mean(var, na.rm = TRUE), var = as.name(var)))
  
  plot_dat_all <- dat %>%
    filter(dow %in% days) %>%
    filter_(lazyeval::interp(~is.finite(var), var = as.name(var))) %>%
    group_by(year,season,month,week,day,hour) %>%
    summarise_(var = lazyeval::interp(~mean(var, na.rm = TRUE), var = as.name(var))) %>%
    mutate(id = paste0(year,week,day))
  
  plot_dat_ts <- setNames(xts(as.matrix(plot_dat$var), order.by = as.POSIXct(strptime(paste(1,1,1,plot_dat$hour), format = "%Y %m %d %H"))), paste0(var,'_mean'))
  
  yrng <- c(0,round(qnorm(0.99, mean=mean(plot_dat_all$var, na.rm=TRUE), sd=sd(plot_dat_all$var, na.rm=TRUE))))
  
  dygraph(plot_dat_ts, main = paste(type, 'Average'), group = "plotLoadProfile") %>%
    dyAxis(name='x', label='hour') %>%
    dyAxis(name='y', label=var, valueRange=yrng) %>%
    dyOptions(fillGraph = TRUE, fillAlpha = 0.4, drawPoints = TRUE, pointSize = 2, axisLineWidth = 1.5)
  
}

#' plotFullTS_r
#' 
#' @param dat data...
#' @param inludeZero inludeZero...
#' @return \code{NULL}
#' @export

plotFullTS_r <- function(dat, inludeZero=TRUE) {
  
  p <- ggplot(dat, aes(ts, value, group=resource)) +
    geom_point(aes(color=resource)) +
    geom_line(aes(color=resource)) +
    #scale_x_date(labels = date_format("%b-%Y")) +
    xlab("") +
    facet_wrap(~ variable, ncol=1, scales='free_y') +
    theme(strip.text = element_text(size = 16))
  if (inludeZero) p <- p + expand_limits(y=0)
  
  print(p)
  
  invisible(NULL)
}

#' plotFullTS_js_EI
#' 
#' @param dat data...
#' @return \code{NULL}
#' @export

plotFullTS_js_EI <- function(dat) {
  dygraph(dat, main = 'EI', group = "plotFullTS") %>%
    dyOptions(fillGraph = TRUE, fillAlpha = 0.4, drawPoints = TRUE, pointSize = 2, axisLineWidth = 1.5)
}

#' plotFullTS_js_kW
#' 
#' @param dat data...
#' @return \code{NULL}
#' @export

plotFullTS_js_kW <- function(dat) {
  dygraph(dat, main = 'kW', group = "plotFullTS") %>%
    dyOptions(fillGraph = TRUE, fillAlpha = 0.4, drawPoints = TRUE, pointSize = 2, axisLineWidth = 1.5)
}

#' plotFullTS_js_Water
#' 
#' @param dat data...
#' @return \code{NULL}
#' @export

plotFullTS_js_Water <- function(dat) {
  dygraph(dat, main = 'Water', group = "plotFullTS") %>%
    dyOptions(fillGraph = TRUE, fillAlpha = 0.4, drawPoints = TRUE, pointSize = 2, axisLineWidth = 1.5) %>% 
    dyRangeSelector(height = 40)
}
