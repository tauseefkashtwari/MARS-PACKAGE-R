#' summary.mars
#'
#' @description prints a summary of the mars object with the function call. Also prints the summary of the hinges of each basis function.
#' @param object of class mars, which is obtained from calling mars()
#' @param digits the number of significant digits i.e. SIG FIG
#'
#' @return
#' @export
#'
#' @examples mm<-mars(y ~.,data=mars::marstestdata)
#' summary(mm)
#' @author Tauseef Kashtwari, Promit Chowdhury, Ibraheem Azad
summary.mars <- function(object,digits)
{
  for(i in 2:length(object$Bfuncs))
  {
    cat("Coefficient ",names(object$coefficients)[i],":\n")
    for(j in 1:nrow(object$Bfuncs[[i]]))
    {
      cat("covariates:",object$x_names[object$Bfuncs[[i]][j,2]],"split at value t:",object$Bfuncs[[i]][j,3],"\n")
    }
  }
}
