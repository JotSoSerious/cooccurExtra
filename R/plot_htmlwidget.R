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
    SInf = sessionInfo()
    if(as.numeric_version(SInf$loadedOnly$later$Version) < as.numeric_version("0.8.0.9003")){
      writeLines(c(paste("Namespace 'later'", SInf$loadedOnly$later$Version, 
                         "is being loaded, but version >= 0.8.0.9003 is required."),
                   "Then update it from CRAN automatically.",
                   "",
                   "---------------------------------------------------------------------"))
      install.packages("later")
      library(later)
    }
    if(as.numeric_version(SInf$loadedOnly$promises$Version) < as.numeric_version("1.0.1.9002")){
      writeLines(c(paste("Namespace 'promises'", SInf$loadedOnly$promises$Version, 
                         "is being loaded, but version >= 1.0.1.9002 is required."),
                   "Then update it from CRAN automatically.",
                   "",
                   "---------------------------------------------------------------------"))
      install.packages("promises")
      library(promises)
    }
    devtools::install_github("rstudio/chromote", dependencies = TRUE, 
                             upgrade = "never", force = TRUE)
    library(chromote)
  }
  # check class of the htmlwidget object & stop if its not a formattable htmlwidget object
  if(!inherits(widget, "formattable")){
    writeLines(c("Error Message :",
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
  # b$screenshot(selector = "#htmlwidget_container", show = TRUE,
  #              filename = file)
  b$screenshot(show = TRUE, filename = file)
  # close the temp html window
  b$close()
  ###########################################################################################
  #3. turn back on warnings
  ###########################################################################################
  options(warn = oldw)
}