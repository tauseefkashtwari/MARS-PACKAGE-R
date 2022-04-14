#' Plot mars
#'
#' @param description plots the fitted basis function, made up of 1 to 2 hinge functions. Depends on main effects (1 exp variable) or two-way interactions (2 exp variables)
#' @param OBJECT a mars object
#' @param ... these are additional arguments for plotting mars object
#'
#' @export
#'
#' @examples mm <- mars(y~x1+x2,data=marstestdata,mars.control(Mmax=4))
#' @author Tauseef Kashtwari, Promit Chowdhury, Ibraheem Azad
plot.mars<-function(OBJECT,...)
{
  #THIS WAS GIVEN TO US---> SIMILAR TO PREDCICT LAB 10
  NEWDATA<-eval(OBJECT$call$data)

  #LAB 10 CODE
  tt<-terms(OBJECT$formula,data=NEWDATA)
  tt<-delete.response(tt)
  mf<-model.frame(tt,NEWDATA)
  mt<-attr(mf,"terms")
  X<-model.matrix(mt,mf)[,-1] #remove intercept
  Bf<-x$Bfuncs

  margin_1<-which(sapply(Bf,function(x) NROW(x)==1)) #x margin
  margin_2<-which(sapply(Bf,function(x) NROW(x)==2)) #y margin

  nn<-ceiling(sqrt(length(margin_1)+length(margin_2))) #marings

  #mar A numerical vector of the form c(bottom, left, top, right)
  opar<-graphics::par(mfrow=c(nn,nn),mar=c(2,2,2,2))

  #https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/on.exit
  on.exit(graphics::par(opar))#use exit handler to reset pars

  for(i in margin_1)
  {
    vv<-Bf[[i]][1,"v"];variable_name<-x$x_names[[vv]]

    #get x and y coordinate
    x_coord<-seq(from=min(X[,vv]),to=max(X[,vv]),length=100) #x coordinate
    y_coord<-h(x,Bf[[i]][1,"s"],Bf[[i]][1,"t"]) #y coordinate is the height (x,s,t)
    plot(x_coord,y_coord,type="l",xlab=variable_name,main=variable_name,...) #plot(x,y)
  }
}
