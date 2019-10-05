# Function "CoocAna"
#####
# Require Package "cooccur"
#####
# # A function for "cooccur" analysis
#####
coocana= function(mydata, ID.Col = "es", species = NA, ingradients = NA,
                   type = c("byrow", "bycol", "all"),
                   with.splits = FALSE, show.tables = FALSE,
                   pair.scatter = c("scatter", "fit", "none"),
                   any.output = c("lite", "full", "none"),
                   show.split.plot = c("none", "both", "splits", "dendrogram")){

  ###########################################################################################
  #####*1* "mydata" is a data set with all the variables that we need
  #####*2* "ID.Col" is the name of the column that distinguished esch observation
  #####*3* "species" can be any species that contain in data set
  #####*4* "ingradients" can be any species that contains in data set, no more than three types
  #####*5* "type" gives the choices of species pairing type
  #####*6* "with.splits" gives the choices of output
  #####*7* "show.tables" logical, show tables of stats if "TRUE"
  #####*8* "pair.scatter" gives the choices of scatter plots ("fit" if fitted spline wanted)
  #####*9* "any.output" logical, show if the lite output is needed
  #####*10* "show.split.plot" gives the choices of splits plots

  ###########################################################################################
  #0. turn down warnings and initialize
  ###########################################################################################
  oldw <- getOption("warn")
  options(warn = -1)
  lite.output = any.output != "full" & any.output != "none"
  ###########################################################################################
  #1. check if any "species" or "ingradients" or "ID.Col" is not in "mydata"
  ###########################################################################################
  check.species = match(species, colnames(mydata))
  check.ingradients = match(ingradients, colnames(mydata))
  judge.species = is.na(check.species)
  judge.ingradients = is.na(check.ingradients)
  judge.ID = is.na(match(ID.Col, colnames(mydata)))
  mark = 1
  ## keep the rest
  keep.species = species[!judge.species]
  keep.ingradients = ingradients[!judge.ingradients]
  ## number of species & ingradients
  n.species = length(keep.species)
  n.ingradients = length(keep.ingradients)
  ## show warnings
  if(any(judge.species) & !is.na(species)){
    writeLines(c("Warning Message (1):",
                 paste(ifelse(sum(judge.species) == 1, "Specie", "Species"),
                       paste(paste0("'", species[judge.species], "'"),
                             collapse = ","),
                       ifelse(sum(judge.species) == 1, "is", "are"),
                       "not in the given data set, which",
                       ifelse(sum(judge.species) == 1, "has", "have"),
                       "been ignored.")))
    mark = mark + 1
  }
  if(any(judge.ingradients) & !is.na(ingradients)){
    writeLines(c(paste0("Warning Message (", mark, "):"),
                 paste(ifelse(sum(judge.ingradients) == 1, "Ingradient", "Ingradients"),
                       paste(paste0("'", ingradients[judge.ingradients], "'"),
                             collapse = ","),
                       ifelse(sum(judge.ingradients) == 1, "is", "are"),
                       "not in the given data set, which",
                       ifelse(sum(judge.ingradients) == 1, "has", "have"),
                       "been ignored.")))
    mark = mark + 1
  }
  if(any(judge.ID)){
    writeLines(c(paste0("Warning Message (",mark, "):"),
                 paste("'ID.Col' is not in the given data set, which",
                       "has been replaced by orders.")))
    mark = mark + 1
  }
  if(with.splits & (is.na(ingradients) | n.ingradients == 0)){
    writeLines(c(paste0("Warning Message (", mark, "):"),
                 paste("There is no",
                       ifelse(any(is.na(ingradients)), "input of ingradient,",
                              "existed input ingradients,"),
                       "so 'with.splits' cannot be true.")))
    with.splits = FALSE
    mark = mark + 1
  }
  if((is.na(species) | n.species == 0)){
    writeLines(c(paste0("Warning Message (", mark, "):"),
                 paste("There is no",
                       ifelse(is.na(species), "input of specie,",
                              "existed input species,"),
                       "and the work cannot be process.")))
    return()
  }
  ### check if the number of species is odd number
  judge.odd = (n.species %% 2) == 1
  ## add an index column if there is no exsisted index column
  if(any(judge.ID)){
    mydata$index = 1:nrow(mydata)
    ID.Col = "index"
  }
  ###########################################################################################
  #2. if splits are needed
  ###########################################################################################
  if(with.splits){
    Temp.Splits = treesplits(MyData.1, species = keep.species,
                             ingradients = keep.ingradients, show.plots = show.split.plot)
  }
  ###########################################################################################
  #3. create matrix to store pairs that are needed
  ###########################################################################################
  if(judge.odd){
    use.species = c(keep.species, keep.species[1])
  }else{
    use.species = keep.species
  }
  if(type != "all"){
    Pairs.Needed = matrix(use.species, ncol = 2, byrow = (type == "byrow"))
  }else{
    Pairs.Needed = t(combn(keep.species, 2))
  }
  ###########################################################################################
  #4. plot scatter plots by pairs
  ###########################################################################################
  if(pair.scatter != "none"){
    op = par()
    par(mfrow = c(1, 2))
    for(i in 1:nrow(Pairs.Needed)){
      temp.x = mydata[, Pairs.Needed[i, 2]]
      temp.y = mydata[, Pairs.Needed[i, 1]]
      plot(temp.x, temp.y, type = "p",
           xlab = Pairs.Needed[i, 2], ylab = Pairs.Needed[i, 1],
           main = paste("Scatter Plot of \n", Pairs.Needed[i, 1],
                        "VS", Pairs.Needed[i, 2]))
      if(pair.scatter == "fit"){
        temp.fit = lm(temp.y ~ temp.x + I(temp.x^2))
        temp.x.vect = seq(min(temp.x), max(temp.x), length.out = 200)
        temp.y.vect = predict(temp.fit, type = "response",
                              newdata = data.frame(temp.x = temp.x.vect))
        lines(temp.x.vect, temp.y.vect, lwd = 3, col = 2)
      }
    }
    par(op)
  }
  ###########################################################################################
  #5. modified data & create a matrix for "cooccur" function
  ###########################################################################################
  ## modified data
  mydata = mydata[, c(ID.Col, keep.species, keep.ingradients)]
  for(i in match(keep.species, colnames(mydata))){
    mydata[, i] = as.numeric(mydata[, i] > 1e-16)
  }
  ## then create a matrix for "cooccur"
  CO.Mat = t(mydata[, -match(c(ID.Col, keep.ingradients), colnames(mydata))])
  colnames(CO.Mat) = mydata[, ID.Col]
  #show(CO.Mat[, 1:5])
  ## get the locate information of each pair
  Pairs.Locate = matrix(match(drop(Pairs.Needed), row.names(CO.Mat)), ncol = 2)
  ###########################################################################################
  #6. cooccur if "with.splits" equals to FALSE
  ###########################################################################################
  ## fit a cooccur model to the whole data
  show("Full Data - All Pairs")
  CO.Fit.Pairs = cooccur(mat = CO.Mat, spp_names = T)
  ## get the needed pairs
  Temp = CO.Fit.Pairs$results
  # initial "COC.Pairs"
  COC.Pairs = Temp[1:nrow(Pairs.Locate), ]
  for(i in 1:nrow(Pairs.Locate)){
    COC.Pairs[i, ] = Temp[(Temp$sp1 == Pairs.Locate[i, 1] &
                             Temp$sp2 == Pairs.Locate[i, 2]) |
                            (Temp$sp1 == Pairs.Locate[i, 2] &
                               Temp$sp2 == Pairs.Locate[i, 1]), ]
  }
  ## create a lite output if chosen
  if(lite.output){
    COC.Pairs = COC.Pairs[, c("sp1_name", "sp2_name", "sp1_inc", "sp2_inc",
                              "obs_cooccur", "prob_cooccur", "exp_cooccur",
                              "p_lt", "p_gt")]
  }
  ###########################################################################################
  #7. some preperation if "splits" are needed
  ###########################################################################################
  if(with.splits){
    ##7-1. the splits part
    ### create storing place
    drop.PL = drop(Pairs.Locate)
    temp = rep(0, length(drop.PL))
    Splits = vector("list", n.ingradients)
    for(j in 1:n.ingradients) Splits[[j]] = temp
    ### splits for pairs
    for(i in 1:length(drop.PL)){
      for(j in 1:n.ingradients){
        Splits[[j]][i] = Temp.Splits[[j]][[drop.PL[i]]][1]
      }
    }
    ### turn into matrix
    for(j in 1:n.ingradients){
      Splits[[j]] = matrix(Splits[[j]], nrow = 2)
    }
    ### pair names
    Pairs.Names = paste(Pairs.Needed[, 1], Pairs.Needed[, 2], sep = ".")
    ### rename the former "Splits" list
    names(Splits) = keep.ingradients
    for(j in 1:n.ingradients){
      colnames(Splits[[j]]) = c("", "")
      rownames(Splits[[j]]) = Pairs.Names
    }
    ##7-2. the ranges part
    ### create list to store the results
    COC.ingradient = vector("list", n.ingradients)
    names(COC.ingradient) = keep.ingradients
    ### create sub-list for the "COC.ingradient"
    Temp.List = vector("list", length(Pairs.Names))
    names(Temp.List) = Pairs.Names
    ### put the sub-list into "COC.ingradient"
    for(i in 1:n.ingradients) COC.ingradient[[i]] = Temp.List
  }
  ###########################################################################################
  #8. cooccur if "with.splits" equals to TRUE
  ###########################################################################################
  if(with.splits){
    ## create a list to store all the models
    Cooccur.Models = vector("list", length(Pairs.Names) + 1)
    names(Cooccur.Models) = c(Pairs.Names, "Full Data")
    ## put the cooccur fitted model of full data into the list of models
    Cooccur.Models[[length(Pairs.Names) + 1]] = CO.Fit.Pairs
    ## sub list
    temp = vector("list", n.ingradients)
    names(temp) = keep.ingradients
    for(k in 1:length(Pairs.Names)) Cooccur.Models[[k]] = temp
    ## do cooccur in every splits
    for(i in 1:n.ingradients){
      # show(paste0("i=", i))
      show(keep.ingradients[i])
      for(k in 1:length(Pairs.Names)){
        show(Pairs.Names[k])
        Temp.ingradient = mydata[, keep.ingradients[i]]
        # show(paste0("k=", k))
        ## cut ingradients into groups by the splits we got from rpart models
        Temp.ingradient = cut(Temp.ingradient,
                              breaks = c(min(Temp.ingradient),
                                         if(Splits[[i]][k, 1] == Splits[[i]][k, 2]){
                                           Splits[[i]][k, 1]
                                         }
                                         else{sort(Splits[[i]][k, ],
                                                   decreasing = F)},
                                         max(Temp.ingradient)),
                              include.lowest = T)
        ## Create a matrix to store the results
        ### detect if the lited output are needed
        if(lite.output){
          COC.ingradient[[i]][[k]] = COC.Pairs[k, c("sp1_inc", "sp2_inc",
                                                    "obs_cooccur", "prob_cooccur",
                                                    "exp_cooccur", "p_lt", "p_gt")]
        }else{
          COC.ingradient[[i]][[k]] = COC.Pairs[k, ]
        }
        IDC = NULL
        ## create a sub list to tore all the splited levels
        Cooccur.Models[[k]][[i]] = vector("list", length(levels(Temp.ingradient)))
        names(Cooccur.Models[[k]][[i]]) = levels(Temp.ingradient)
        for(j in 1:length(levels(Temp.ingradient))){
          show(levels(Temp.ingradient)[j])
          # show(paste0("j=", j))
          ## fit cooccur models
          Temp.fit = cooccur(mat = CO.Mat[Pairs.Locate[k, ],
                                          Temp.ingradient == levels(Temp.ingradient)[j]],
                             spp_names = T, thresh = F)
          ## store the fitted model
          Cooccur.Models[[k]][[i]][[j]] = Temp.fit
          ## store the results (check if lited results are needed)
          if(lite.output){
            Temp.Add = Temp.fit$results[, c("sp1_inc", "sp2_inc",
                                            "obs_cooccur", "prob_cooccur",
                                            "exp_cooccur", "p_lt", "p_gt")]
          }else{
            Temp.Add = Temp.fit$results
          }
          if(nrow(Temp.Add) != 0){
            COC.ingradient[[i]][[k]] = rbind(COC.ingradient[[i]][[k]], Temp.Add)
            IDC = c(IDC, j)
          }
        }
        rownames(COC.ingradient[[i]][[k]]) = c("Full Set", levels(Temp.ingradient)[IDC])
      }
    }
  }
  ###########################################################################################
  #9. turn back on warnings
  ###########################################################################################
  options(warn = oldw)
  ###########################################################################################
  #10. show table
  ###########################################################################################
  if(with.splits){
    my.output = list(Model = Cooccur.Models, Splits = Splits,
                     Stats = COC.ingradient, Pairs = Pairs.Needed,
                     Cooc.Mat = CO.Mat, Used.Ingradients = keep.ingradients)
  }else{
    my.output = list(Model = CO.Fit.Pairs, Splits = NA, Stats = COC.Pairs,
                     Pairs = Pairs.Needed, Cooc.Mat = CO.Mat,
                     Used.Ingradients = keep.ingradients)
  }
  if(show.tables){ displaytable(mymod = my.output, with.splits = with.splits,
                                table.out = "stats")}
  ###########################################################################################
  #11. return values
  ###########################################################################################
  if(any.output == "none"){return()}
  else{return(my.output)}
}
