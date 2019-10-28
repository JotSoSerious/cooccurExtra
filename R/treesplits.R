#' A function to detect break points of species occurance in different ingradients.
#'
#' @param mydata A data frame, which contains species and ingradients information from different regions.
#' @param species A list of charaters with names of the needed species.
#' @param ingradients A list of charaters with names of the needed ingradients where to be shown in the tables.
#' @param plot_title A logical value, which indicates showing plots with or without title.
#' @param first_split A logical value, which indicates showing scatter plots with only the first split or showing all the splits. By default, first_split = FALSE, which means only the first splits will be shown.
#' @param show.plots A choice of the showing plots. (show.plots = "splits") shows only the break points in the scatter plots. (show.plots = "dendrgram") shows only the tree dendrograms from the "rpart" models. (show.plots = "none") if no plot is needed. The default is (show.plots = "both"), which shows both the splits plots and the dendrograms.
#' @return Returns a list of break points in different ingradients and for different species.
#' @description
#' This is a function of the package "cooccurExtra". The main idea of this function is detecting break points of species occurance in different ingradients with the information of species and ingradients from different regions. It provides two kinds of plots to show the breaks points in different ingradients of each specie. Also it provides an output of break points (splits) of each ingradient and for each specie.
#' @author
#' Yingjia Jot He
#' @examples
#' # create a vector of charaters with all the needed species' names
#' Ten.Needed <- c("alpheus", "theora.lubrica", "aricidea",
#'                 "paradonidae.other", "austrovenus.stutchburyi", "macomona.liliana",
#'                 "maldanidae", "aonides", "prionospio.aucklandica", "orbiniidae")
#'
#' # create a vector of charaters with all the needed ingradients' names
#' Ing.Needed <- c("Log.mud.1.", "SST")
#'
#' # ask for the data frame to be analized
#' data(macrofauna)
#'
#' # the default with two kinds of plots
#' mytest <- treesplits(macrofauna, species = Ten.Needed, ingradients = Ing.Needed)
#'
#' # show the output
#' mytest
#'
#' # case with one specie that does not exist and show only the splits plots
#' mytest <- treesplits(macrofauna, species = c(Ten.Needed, "fake.specie"),
#'                      ingradients = Ing.Needed, show.plots = "splits")
#'
#' # case with one ingradient that does not exist and show only the dendrograms
#' mytest <- treesplits(macrofauna, species = Ten.Needed,
#'                      ingradients = c(Ing.Needed, "fake.specie"),
#'                      show.plots = "dendrogram")

# Function "treesplits"
#####
# Require Package "rpart" & "rattle"
#####
# A function to detect break points of species occurance in different ingradients
#####
treesplits = function(mydata, species, ingradients, first_split = FALSE,
                      plot_title = TRUE, #method = "tree",
                      show.plots = c("both", "splits",
                                     "dendrogram", "none")){
  #####*1* "mydata" is a data set with all the variables that we need
  #####*2* "methods" could be "tree" and some others, will update in the future
  #####*3* "species" can be any species that contain in data set
  #####*4* "ingradients" can be any species that contains in data set, no more than three types
  #####*5* "show.plots" gives the choices of plots
  #####*6* "plot_title" logical. indicates showing plots with or without title.
  #####*6* "first_split" logical. indicates showing scatter with only the first split.

  ###########################################################################################
  #0. turn down warnings
  ###########################################################################################
  oldw = getOption("warn")
  options(warn = -1)
  ###########################################################################################
  #1. check if any "species" or "ingradients" are not in "mydata"
  ###########################################################################################
  check.species = match(species, colnames(mydata))
  check.ingradients = match(ingradients, colnames(mydata))
  judge.species = is.na(check.species)
  judge.ingradients = is.na(check.ingradients)
  ## show warnings
  if(any(judge.species)){
    writeLines(c("Warning Message (1):",
                 paste(ifelse(sum(judge.species) == 1, "Specie", "Species"),
                       paste(paste0("'", species[judge.species], "'"),
                             collapse = ","),
                       ifelse(sum(judge.species) == 1, "is", "are"),
                       "not in the given data set, which",
                       ifelse(sum(judge.species) == 1, "has", "have"),
                       "been ignored.")))
  }
  if(any(judge.ingradients)){
    writeLines(c(paste("Warning Message", ifelse(any(judge.species), "(2):", "(1):")),
                 paste(ifelse(sum(judge.ingradients) == 1, "Ingradient", "Ingradients"),
                       paste(paste0("'", ingradients[judge.ingradients], "'"),
                             collapse = ","),
                       ifelse(sum(judge.ingradients) == 1, "is", "are"),
                       "not in the given data set, which",
                       ifelse(sum(judge.ingradients) == 1, "has", "have"),
                       "been ignored.")))
  }
  ## keep the rest
  keep.species = species[!judge.species]
  keep.ingradients = ingradients[!judge.ingradients]
  ## number of species & ingradients
  n.species = length(keep.species)
  n.ingradients = length(keep.ingradients)
  ###########################################################################################
  #2. vector for mfrow
  ###########################################################################################
  if(show.plots == c("both", "splits", "dendrogram", "none")) show.plots = "both" # default
  if(show.plots != "none") mf = c(ifelse(show.plots == "both", 2, 1), n.ingradients)
  ###########################################################################################
  #3. lsit for storing the splits
  ###########################################################################################
  temp = vector("list", n.species)
  Splits = vector("list", n.ingradients)
  for(i in 1:n.ingradients) Splits[[i]] = temp
  ###########################################################################################
  #4. do the plots and store the splits
  ###########################################################################################
  if(show.plots != "none"){
    op = par()
    par(mfrow = mf)
  }
  ## get column names of given data set
  col.names = colnames(mydata)
  ## set counters
  m = 0
  n = 0
  for(i in match(keep.species, col.names)){
    m = m + 1
    NameMacrofauna = keep.species[m]
    temp.models = vector("list", n.ingradients)
    for(j in match(keep.ingradients, col.names)){
      n = n + 1
      tempfit = rpart(mydata[, i] ~ mydata[, j], maxdepth = 2)
      temp.models[[n]] = tempfit
      tempsplit = tempfit$splits[, 4]
      Splits[[n]][[m]] = tempsplit
      if(show.plots == "both" | show.plots == "splits"){
        plot(x = mydata[, j], y = mydata[, i], type = "p",
             ylab = NameMacrofauna, xlab = keep.ingradients[n])
        if(plot_title){
          title(main = paste0(NameMacrofauna, " VS ", keep.ingradients[n]))
        }
        for(k in 1:ifelse(first_split, 1, length(tempsplit))){
          abline(v = tempsplit[k], lty = 2, col = 2, lwd = 2)
        }
      }
    }
    if(show.plots == "both" | show.plots == "dendrogram"){
      n = 0
      for(j in match(keep.ingradients, col.names)){
        n = n + 1
        model.tree = temp.models[[n]]
        fancyRpartPlot(prune(model.tree, cp = model.tree$cptable[model.tree$cptable[, "nsplit"] == 1, "CP"]),
                       sub = "", digits = 4)
        if(plot_title){
          title(main = paste0("Dendrogram of \n", NameMacrofauna, " VS ", keep.ingradients[n]))
        }
      }
    }
    n = 0
  }
  if(show.plots != "none") par(op)
  ###########################################################################################
  #5. turn back on warnings
  ###########################################################################################
  options(warn = oldw)
  ###########################################################################################
  #6. return values
  ###########################################################################################
  # add names to the output list
  names(Splits) = keep.ingradients
  for(i in 1:length(keep.ingradients)){
    names(Splits[[i]]) = keep.species
  }
  # return the list
  return(Splits)
}
