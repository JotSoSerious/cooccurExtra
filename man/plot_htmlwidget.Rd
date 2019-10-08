\name{plot_htmlwidget}
\alias{cooccurExtra}
\title{
Export cooccurence tables as an image.
}
\description{
  This function allow users to display cooccurence tables from the \code{cooccurExtra} in a word or pdf document. Since by default htmlwidget objects cannot be displayed in word or pdf documents. This function also allow users to save these tables as a PNG locally.
}
\usage{
plot_table(widget, file = NULL)
}
\arguments{
  \item{widget}{
  \code{htmlwidget} object.
}
  \item{file}{
  The name of the file where the data will be saved. Default to \code{NULL}, no saving required.
}
}
\details{
To render the htmlwidget into an image. A headless browser is used to capture it, to do this the Chrome Dev Tool is used, the \code{chromote} package provides this functionality.
}
\author{
Yingjia J He, Charco Hui
}
\examples{
  # require packages
  # devtools::install_github("rstudio/chromote")
  # install.packages("showimage")
  library(chromote)
  library(showimage)
  
  # ask for a "coocana" output model
  data(ModelCA)
  
  # get the output from displaytable function
  mytest = displaytable(mymod = ModelCA)
  
  # get the plots from 
  plot_htmlwidget(mytest$STATSFT)
  plot_htmlwidget(mytest$SIGNFT)
}