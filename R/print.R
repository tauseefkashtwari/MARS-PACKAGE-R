#' Prints out a mars object
#'
#' @description PRINT METHOD MARS OBJECT
#' @return values of the coefficients for MARS
#'
#' @details Prints intercept and coefficient of mars object
#'
#' @examples mm <- mars(y~x1+x2,data=marstestdata)
#' print(mm)
#' @author Tauseef Kashtwari, Promit Chowdhury, Ibraheem Azad
print.mars <- function(marsobject,...) #marsobject
{
  #---------------------Coefficients separately--------------------#
  print(marsobject$call)
  print(coefficients(marsobject))
  print("NOW GONNA TRY TO PRINT THE MODEL\n")


  #---------------------MODEL PRINTED BELOW-----------------------#
  model = "Y = B0"
  for(i in 2:length(marsobject$coefficients))
  {
    #use paste
    model = paste(model, names(marsobject$coefficients)[i], sep="+") #take ith object separated by + e.g. Y = B0+B1X+B2X
  }

  print("The model is: ", model)

  for(k in 1:length(marsobject$coefficients)){
    cat(marsobject$coefficients[k], "is the coefficient of",names(marsobject$coefficients)[k],"\n" ) #coef followed by name
  }


}
