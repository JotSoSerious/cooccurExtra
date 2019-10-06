# Function "checkpnr"
#####
# Require Package "cooccur"
#####
# A function to distinct "positive", "negative", and "random"
#####
checkpnr = function(cooccur.mod){
  ##### "cooccur.mod" should be a cooccur model
  
  ###########################################################################################
  #0. turn down warnings & check if the input value is a "cooccur" object
  ###########################################################################################
  oldw = getOption("warn")
  options(warn = -1)
  ## check if the input value is a "cooccur" object
  ## return "error" if it's not
  if(class(cooccur.mod) != "cooccur"){
    writeLines(c("Error Message:",
                 "  The input model is not a 'cooccur' object."))
    return()
  }
  ###########################################################################################
  #1. create the table to show out
  ###########################################################################################
  table.out = prob.table(cooccur.mod)[, c("sp1", "sp2", "sp1_name", 
                                          "sp2_name", "p_gt")]
  num.row = nrow(table.out)
  table.out$PNR = ifelse(effect.sizes(cooccur.mod)$effects > 0, 
                         rep("Positive", num.row), 
                         rep("Negative", num.row))
  table.out$PNR = ifelse(table.out$p_gt > cooccur.mod$true_rand_classifier,
                         rep("Random", num.row), table.out$PNR)
  table.out = table.out[, c("sp1_name", "sp2_name", "p_gt", "PNR")]
  ###########################################################################################
  #2. turn back on warnings
  ###########################################################################################
  options(warn = oldw)
  ###########################################################################################
  #3. return the table with "Positive", "Negative", "Random"
  ###########################################################################################
  return(table.out)
}