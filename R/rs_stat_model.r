

examples.as.rs.model = function() {
  reg <- glm(admit ~ gre + gpa + rank, family = binomial(link = "probit"), x = TRUE,
             data = mydata)
  as.rs.model(reg, show.mar.eff = TRUE) 
  
  reg <- lm(admit ~ gre + gpa + rank, x = TRUE,  data = mydata)
  as.rs.model(reg)  
}

as.rs.model = function(m, show.mar.eff= FALSE, robust = NULL,...) {
  restore.point("as.rs.model")
  
  rs = m
  sm = summary(m)
  
  class(rs) = c("rsModel", class(m))
  
  if ("glm" %in% class(m)) {
    rs$model.type = paste0(rs$family$family," ",rs$family$link)
  } else {
    rs$model.type = class(m)[1]
  }
  if (is.null(rs$formula))
    rs$formula = m$call$formula
  
  rs$coef.mat = sm$coef
  beta.stand = calculate.standardized.coef(m)
  rs$coef.mat =cbind(rs$coef.mat, "b*sx/sy"=beta.stand)
  
  rs$show.mar.eff = FALSE
  # Compute marginal effects
  if (show.mar.eff) {
    tryCatch({
      rs$mar.eff.mat <- as.matrix(maBina(m, digits=8)$out)
      cn = colnames(rs$coef.mat)
      cn[1] = "Mar. Effect"
      colnames(rs$mar.eff.mat) = cn
      rs$mar.eff <- rs$mar.eff.mat[,1]
      rs$show.mar.eff <- TRUE
    }, error = function(e) {print(e)})
  }
  rs
}

str.cols = function(..., len=Inf) {
  arg = list(...)
  restore.point("str.cols")
  
  ind = seq(1,length(arg),by=3)
  
  
  col.ind = c(unlist(arg[ind]),len)
  align = unlist(arg[ind+1])
  val = arg[ind+2]
  
  nc = length(val)
  
}

print.rsModel = function(rs, digits = max(3L, getOption("digits") - 3L), symbolic.cor = x$symbolic.cor, 
                         signif.stars = getOption("show.signif.stars"), ...) {
  
  cols = 60
  yvar = as.character(attr(rs$terms, "variables")[[2]])
  N = length(rs$residuals)
  
  str.cols(
    1, "l",paste0("Dep. var:", yvar),
    60,"l","Obs:",
    80,"r",N
  )
    
  
  str = str.place.left.right(paste0(rs$model.type,"model"),
                             paste0(" Obs:", N),cols)
  cat(paste0("\n",str,"\n"))
  str = paste0("Dependented variable: ", yvar)
  cat(paste0("\n",str,"\n\n"))
   
  if (is.true(rs$show.mar.eff)) {
    #cat("\nMarginal effects:\n")
    printCoefmat(rs$mar.eff.mat, digits = digits, signif.stars = signif.stars, na.print = "NA", ...)
  } else {  
    #cat("\nCoefficients:\n")
    printCoefmat(rs$coef.mat, digits = digits, signif.stars = signif.stars, na.print = "NA", ...)
  }  
}


calculate.standardized.coef = function (mod) 
{
  restore.point("calculate.standardized.coef")
  b <- mod$coefficient[-1]
  sx <- sapply(mod$model[-1],sd)
  sy <- sd(mod$model[[1]])
  beta <- b * sx/sy
  return(c(Intercept=NA,beta))
}