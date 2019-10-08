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
    writeLines(c("The needed package 'chromote' hasn't been installed yet.",
                 "Then install it from github automatically."))
    if(!("devtools" %in% installed.packages()[,1])){
      writeLines(c("The needed package 'devtools' hasn't been installed yet.",
                   "Then install it from CRAN automatically."))
      install.packages("devtools")
      library(devtools)
    }
    devtools::install_github("rstudio/chromote")
    library(chromote)
  }
  # check class of the htmlwidget object & stop if its not a formattable htmlwidget object
  if(!inherits(widget, "formattable")){
    writeLines(c("Error Message :"
                 "The input object 'widget' is not a formattable htmlwidget object."))
    stop()
  }
  ###########################################################################################
  #2. main functioning part
  ###########################################################################################
  # create a temp file to hold the htmlwidget object
  tmp_file = paste0(tempfile(), ".html")
  widget = as.htmlwidget(widget)
  htmlwidgets::saveWidget(widget, tmp_file)
  # create a temp html window for reflecting the htmlwidget object
  b = ChromoteSession$new()
  b$Page$navigate(tmp_file)
  # show an save the reflected object as a plot
  b$screenshot(selector = "#htmlwidget_container", show = TRUE,
               filename = file)
  # close the temp html window
  b$close()
  ###########################################################################################
  #3. turn back on warnings
  ###########################################################################################
  options(warn = oldw)
}