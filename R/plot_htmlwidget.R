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
<<<<<<< HEAD

#####
# plot_htmlwidget
#####
# Export cooccurence tables as an image
# Allow users to display cooccurence tables from the \code{cooccurExtra} in a word or pdf document

plot_htmlwidget = function(widget, file = NULL){
  ###########################################################################################
  #####*1* "widget" is a htmlwidget object.
  #####*2* "file" is the name of the file where the data will be saved
  
  ###########################################################################################
  #0. turn down warnings and initialize
  ###########################################################################################
  oldw <- getOption("warn")
  options(warn = -1)
  ###########################################################################################
  #1. check class of the htmlwidget object
  ###########################################################################################
  # check the needed packages 
  if(!("chromote" %in% installed.packages()[,1])){
    writeLines(c("---------------------------------------------------------------------",
                 "The needed package 'chromote' hasn't been installed yet.",
                 "Then install it from github automatically.",
                 "PLease check the following code and install yourself, if any failure:",
                 '# devtools::install_github("rstudio/chromote", dependencies = TRUE,',
                 '#                          upgrade = "never", force = TRUE)',
                 "# library(chromote)",
                 "",
                 "---------------------------------------------------------------------"))
    if(!("devtools" %in% installed.packages()[,1])){
      writeLines(c("The needed package 'devtools' hasn't been installed yet.",
                   "Then install it from CRAN automatically.",
                   "",
                   "---------------------------------------------------------------------"))
      install.packages("devtools")
      library(devtools)
    }
    # save the session information
    SInf = sessionInfo()
    # update package "later"
    if("later" %in% names(SInf$loadedOnly)){
      if(as.numeric_version(SInf$loadedOnly$later$Version) < as.numeric_version("0.8.0.9003")){
        writeLines(c(paste("Namespace 'later'", SInf$loadedOnly$later$Version, 
                           "is being loaded, but version >= 0.8.0.9003 is required."),
                     "Then update it from CRAN automatically.",
                     "",
                     "---------------------------------------------------------------------"))
        install.packages("later")
        library(later)
      }
    }
    # update package "promises"
    if("promises" %in% names(SInf$loadedOnly)){
      if(as.numeric_version(SInf$loadedOnly$promises$Version) < as.numeric_version("1.0.1.9002")){
        writeLines(c(paste("Namespace 'promises'", SInf$loadedOnly$promises$Version, 
                           "is being loaded, but version >= 1.0.1.9002 is required."),
                     "Then update it from CRAN automatically.",
                     "",
                     "---------------------------------------------------------------------"))
        install.packages("promises")
        library(promises)
      }
    }
    devtools::install_github("rstudio/chromote", dependencies = TRUE, 
                             upgrade = "never", force = TRUE)
  }
  # check class of the htmlwidget object & stop if its not a formattable htmlwidget object
  if(!inherits(widget, "formattable")){
    writeLines(c("Error Message :",
                 "The input object 'widget' is not a formattable htmlwidget object."))
    stop()
=======


plot_htmlwidget = function(widget, file = NULL) {
  if(!inherits(widget, "formattable")) {
    stop("This is not an formattable htmlwidget object.")
>>>>>>> 12e515ebea018345fa66713da25c38975b2c3b1a
  }
  tmp_file = paste0(tempfile(), ".html")
  widget = as.htmlwidget(widget)
  htmlwidgets::saveWidget(widget, tmp_file)
  b = ChromoteSession$new()
  b$Page$navigate(tmp_file)
  b$screenshot(selector = "#htmlwidget_container", show = TRUE,
               filename = file)
  b$close()
<<<<<<< HEAD
  ###########################################################################################
  #3. turn back on warnings
  ###########################################################################################
  options(warn = oldw)
}

# plot_htmlwidget = function(widget, file = NULL) {
#   if(!inherits(widget, "formattable")) {
#     stop("This is not an formattable htmlwidget object.")
#   }
#   tmp_file = paste0(tempfile(), ".html")
#   widget = as.htmlwidget(widget)
#   htmlwidgets::saveWidget(widget, tmp_file)
#   b = ChromoteSession$new()
#   b$Page$navigate(tmp_file)
#   b$screenshot(selector = "#htmlwidget_container", show = TRUE,
#                filename = file)
#   b$close()
# }
=======
}
>>>>>>> 12e515ebea018345fa66713da25c38975b2c3b1a
