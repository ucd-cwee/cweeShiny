
#' menuItem_cwee
#' 
#' @import shiny
#' @import shinydashboard
#' @export

menuItem_cwee <- function (text, ..., icon = NULL, badgeLabel = NULL, badgeColor = "green",
                      tabName = NULL, href = NULL, newtab = TRUE, disable = FALSE) 
{
  subItems <- list(...)
  lapply(subItems, shinydashboard:::tagAssert, type = "li")
  if (!is.null(icon)) 
    shinydashboard:::tagAssert(icon, type = "i")
  if (!is.null(href) + (!is.null(tabName) + (length(subItems) > 
                                               0) != 1)) {
    stop("Must have either href, tabName, or sub-items (contained in ...).")
  }
  if (!is.null(badgeLabel) && length(subItems) != 0) {
    stop("Can't have both badge and subItems")
  }
  shinydashboard:::validateColor(badgeColor)
  isTabItem <- FALSE
  target <- NULL
  if (!is.null(tabName)) {
    isTabItem <- TRUE
    href <- paste0("#shiny-tab-", tabName)
  }
  else if (is.null(href)) {
    href <- "#"
  }
  else {
    if (newtab) 
      target <- "_blank"
  }
  if (!is.null(badgeLabel)) {
    badgeTag <- tags$small(class = paste0("badge pull-right bg-", 
                                          badgeColor), badgeLabel)
  }
  else {
    badgeTag <- NULL
  }
  if (length(subItems) == 0) {
    return(tags$li(a(href = if(!disable) href else '#', style = if(disable) 'color:#D0D0D0;',
                     `data-toggle` = if (isTabItem) "tab", `data-value` = if (!is.null(tabName)) tabName,
                     target = target, icon, span(text), badgeTag)))
  }
  tags$li(class = "treeview", a(href = href, icon, span(text), 
                                shiny::icon("angle-left", class = "pull-right")),
          tags$ul(class = "treeview-menu",
                  subItems))
}
