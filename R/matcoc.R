# Function "matcoc"
#####
# Require Package "cooccur"
#####
# a function to create a matrix for the use of "cooccur" function 
#####
matcoc = function(OMat, ID.Col = "es", species = NA){
  #####*1* "OMat" is a object of "coocana" or an original matrix/data frame for the use of "coocana"
  #####*2* "ID.Col" is the name of the column that distinguished esch observation
  #####*3* "species" can be any species that contain in data set
  
  ###########################################################################################
  #0. turn down warnings
  ###########################################################################################
  oldw = getOption("warn")
  options(warn = -1)
  ###########################################################################################
  #1. check everything and output error or warnings
  ###########################################################################################
  ## check if the input value is a "coocana" object
  if(class(OMat) == "list"){
    check.mod = match(names(OMat), c("Model", "Splits", "Stats", "Pairs", 
                                     "Cooc.Mat", "Used.Ingradients"))
    judge.mod = any(is.na(check.mod)) # "TRUE" if its not a "coocana" object
  }else{
    judge.mod = TRUE
  }
  ## check if the input value is a suitable 
  if(class(OMat) == "data.frame"){
    check.species = match(species, colnames(OMat))
    judge.species = all(is.na(check.species)) # "TRUE" if its not a suitable df
  }else{
    if(judge.mod){
      judge.species = TRUE
    }else{
      judge.species = FALSE
    }
  }
  ## return "error" if it's none of the above
  if(judge.mod & judge.species){
    #show()
    writeLines(c("Error Message:",
                 "The input object type is not the output of 'coocana' also its not in a suitable data_frame."))
    return()
  }
  ###########################################################################################
  #2. create a "coocana" object if the input "OMat" is a data frame
  ###########################################################################################
  if(judge.mod){
    OMat = coocana(mydata = OMat, ID.Col = ID.Col, 
                   species = species, pair.scatter = "none")
  }
  ###########################################################################################
  #3. turn back on warnings
  ###########################################################################################
  options(warn = oldw)
  ###########################################################################################
  #4. return the matrix for the use of "cooccur" function
  ###########################################################################################
  myoutput = OMat$Cooc.Mat 
  if(!any(is.na(species)) & !judge.mod){
    myoutput = myoutput[species, ]
  }
  return(myoutput)
}