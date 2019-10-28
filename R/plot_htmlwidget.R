#' Export cooccurence tables as an image.
#'
#' @param widget \code{htmlwidget} object.
#' @param file The name of the file where the data will be saved. Default to \code{NULL}, no saving required.
#' @description
#' This function allow users to display cooccurence tables from the \code{cooccurExtra} in a word or pdf document.
#' Since by default htmlwidget objects cannot be displayed in word or pdf documents.
#' This function also allow users to save these tables as a PNG locally.
#' @details
#' To render the htmlwidget into an image. A headless browser is used to capture it, to do this the Chrome
#' Dev Tool is used, the \code{chromote} package provides this functionality.
#' @author
#' Yingjia J He, Charco Hui
#' @examples
#' # require packages
#' # devtools::install_github("rstudio/chromote")
#' # install.packages("showimage")
#' library(chromote)
#' library(showimage)
#' # ask for a "coocana" output model
#' data(ModelCA)
#' mytest = displaytable(mymod = modelca)
#' plot_htmlwidget(mytest[[3]])
#' plot_htmlwidget(mytest[[4]])


plot_htmlwidget = function(widget, file = NULL) {
  if(!inherits(widget, "formattable")) {
    stop("This is not an formattable htmlwidget object.")
  }
  tmp_file = paste0(tempfile(), ".html")
  widget = as.htmlwidget(widget)
  htmlwidgets::saveWidget(widget, tmp_file)
  b = ChromoteSession$new()
  b$Page$navigate(tmp_file)
  b$screenshot(selector = "#htmlwidget_container", show = TRUE,
               filename = file)
  b$close()
}
