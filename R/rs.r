
examples.str.fill = function() {

  str.fill(paste0("Obs: 100"),80, "right")
  
  str.place.left.right("Hi", "Obs : 80", len=80)
}

str.place.left.right = function(left, right, len, fill = " ") {
  fill.len = len-nchar(left)-nchar(right)
  if (fill.len<1) {
    left = substring(left,1, nchar(left)+fill.len-3)
    return(paste0(left,"...",right))
  }
  paste0(left,paste0(rep(fill,fill.len),collapse=""),right)
}

str.fill = function(str, len, pos="left", fill = " ") {
  restore.point("str.fill")
  txt = paste0(rep(fill,len),collapse="")
  
  if (pos == "left") {
    return(substring(paste0(str,txt,1,len)))
  } else if (pos == "right") {
    return(paste0(substring(txt,1,len-nchar(str)),
                  substring(str,1,pmin(len,nchar(str)))
           ))
  }
  
}

str.single.space = function(str) {
  if (length(str)==0)
    return(str)
  while(any(grepl("  ",str, fixed=TRUE))) {
    str = gsub("    "," ",str)
    str = gsub("  "," ",str)
  }
  str
}

single.arg.commands = function() {
  c("gen","replace","sum")
}


get.rs.by = function() {
  mget(".RS.BY",envir=.GlobalEnv, ifnotfound=list(NULL))[[1]]
}

set.rs.by = function(val=NULL) {
  assign(".RS.BY",val,envir=.GlobalEnv)
}

set.rs.model = function(val=NULL) {
  assign("rs.model",val,envir=.GlobalEnv)
}

get.rs.model = function(val=NULL) {
  mget("rs.model",envir=.GlobalEnv, ifnotfound=list(NULL))[[1]]
}


get.rs.data = function(par=NULL) {
  restore.point("get.rs.data")
  dt = mget("rs.dt",envir=.GlobalEnv, ifnotfound=list(NULL))[[1]]
  if ("if" %in% par$opt.name) {
    if.str = par$opt.arg["if"]
    rows = eval(parse(text=if.str), dt)
    dt = dt[!rows,]
  }
  dt
  #.rs.env$data
}

set.rs.data = function(dt) {
  #.rs.env$data <<- dt
  assign("rs.dt",dt,envir=.GlobalEnv)
}

display = function(..., sep="", collapse="\n") {
  str = paste0(..., sep=sep, collapse=collapse)
  cat(paste0("\n",str,"\n"))
}


# Runs the string code on a data table
run.dt = function(dt,code,rows=NULL,by=NULL, with=TRUE, return.all = FALSE) {
  restore.point("run.dt")
  
  
  if (!is.null(by)) {
    by.str = paste0(",by=c(", paste0('"',by,'"',collapse=","),")")
  } else {
    by.str = ""
  }


  for (act.code in code) {
    com = paste0(
      "dt[",rows,",",act.code,by.str,",with=",with,"]"
    )
    dt = eval(parse(text=com))
  }
  dt
}


get._n.index = function(ind.str, dt=get.rs.data()) {
  restore.point(".n.index")
  len = NROW(dt)
  ind.str = gsub("._n",paste0("(1:",len,")"),ind.str, fixed = TRUE)
  ind = eval(parse(text=ind.str),dt)
  ind[ind<=0 | ind > len] = NA
  ind
}


make.rs.fun = function(sc, ...) {

}


# Like paste0 but returns an empty vector if some string is empty
sc = function(..., sep="", collapse=NULL) {
  str = list(...)
  restore.point("str.combine")
  len = sapply(str,length)
  if (any(len==0))
    return(vector("character",0))
  paste0(...,sep=sep,collapse=collapse)
}


# Compute marginal effects from probit or logit model
# The function is copied from the package erer
maBina = function (w, x.mean = TRUE, rev.dum = TRUE, digits = 4, ...) 
{
  restore.point("maBin")
  
  if (!inherits(w, "glm")) {
    stop("Please provide an object from 'glm()'.\n")
  }
  link <- w$family$link
  if (link != "probit" & link != "logit") {
    stop("Need a binary probit or logit model'.")
  }
  if (is.null(dim(w$x))) {
    stop("Please specify 'x = TRUE' in glm().\n")
  }
  x <- as.matrix(w$x)
  x.bar <- as.matrix(colMeans(w$x))
  b.est <- as.matrix(coef(w))
  K <- nrow(b.est)
  xb <- t(x.bar) %*% b.est
  if (link == "probit") 
    f.xb <- dnorm(xb)
  if (link == "logit") 
    f.xb <- dlogis(xb)
  if (!x.mean) {
    xb2 <- x %*% b.est
    if (link == "probit") 
      f.xb <- mean(dnorm(xb2))
    if (link == "logit") 
      f.xb <- mean(dlogis(xb2))
  }
  me <- f.xb * coef(w)
  bx <- b.est %*% t(x.bar)
  if (link == "probit") {
    dr <- diag(1, K, K) - as.numeric(xb) * bx
    va <- as.numeric(f.xb)^2 * dr %*% vcov(w) %*% t(dr)
  }  else {
    pg <- as.numeric(plogis(xb))
    dr <- diag(1, K, K) + (1 - 2 * pg) * bx
    va <- (pg * (1 - pg))^2 * dr %*% vcov(w) %*% t(dr)
  }
  se <- sqrt(diag(va))
  if (rev.dum) {
    for (i in 1:ncol(w$x)) {
      if (identical(unique(w$x[, i]), c(0, 1))) {
        x.d1 <- x.bar
        x.d1[i, 1] <- 1
        x.d0 <- x.bar
        x.d0[i, 1] <- 0
        if (link == "probit") {
          me[i] <- pnorm(t(x.d1) %*% b.est) - pnorm(t(x.d0) %*% 
                                                      b.est)
          dr2 <- dnorm(t(x.d1) %*% b.est) %*% t(x.d1) - 
            dnorm(t(x.d0) %*% b.est) %*% t(x.d0)
          va2 <- dr2 %*% vcov(w) %*% t(dr2)
          se[i] <- sqrt(as.numeric(va2))
        }
        if (link == "logit") {
          me[i] <- plogis(t(x.d1) %*% b.est) - plogis(t(x.d0) %*% 
                                                        b.est)
          dr2 <- dlogis(t(x.d1) %*% b.est) %*% t(x.d1) - 
            dlogis(t(x.d0) %*% b.est) %*% t(x.d0)
          va2 <- dr2 %*% vcov(w) %*% t(dr2)
          se[i] <- sqrt(as.numeric(va2))
        }
      }
    }
    
    
  }
  out <- data.frame(effect = me, error = se)
  out$t.value <- out$effect/out$error
  out$p.value <- 2 * (1 - pt(abs(out[, 3]), w$df.residual))
  out <- round(out, digits = digits)
  result <- nlist(link, f.xb, w, out)
  class(result) <- "maBina"
  return(result)
}

print.maBina = function (x, ...) 
{
  print(x$out)
}

example.rs.par = function() {
  .f = function(...) {
    rs.par(...)
  }
  .f (y= 5*4,x,  .r.cluster(clus),  .if(z==1))
}

examples.subst.double.dot = function() {
  X = 4
  str = ".gen (str1, d..X..=substr(days_op,..X..,1))"
  subst.double.dot(str)
  subst.double.dot("..X..Y..X..")
}

subst.double.dot = function(str, subst.env=parent.frame()) {
  restore.point("subst.double.dot")
  
  pattern = paste0("\\._[A-Za-z0-9_]_\\.")  
  
  var.dot = unique(unlist(str.extract.all(str, pattern, fixed=FALSE)))
  if (length(var.dot)==0)
    return(str)
  
  var = substring(var.dot,3, nchar(var.dot)-2)
  val = mget(var, envir=subst.env)
  
  for (i in seq_along(var)) {
    str = gsub(var.dot[i],val[i], str, fixed=TRUE) 
  }
  str
}

remove.quotes = function(str) {
  rows = (str.starts.with(str,'"') | str.starts.with(str,"'")) & 
         (str.ends.with(str,'"') | str.ends.with(str,"'"))
  if (any(rows))
    str[rows] = substring(str[rows],2,nchar(str[rows])-1)
  str
}

rs.par = function(..., .command = "", .subst.env=parent.frame()) {
  dots = named_dots(...)
  dots_val =  unlist(lapply(dots, deparse1))
  dots_name= names(dots)
  restore.point("rs.par")
  
  orows = str.starts.with(dots_name,".")
  arg.name = dots_name[!orows]
  arg = dots_val[!orows]
  
  arg.name = subst.double.dot(arg.name, .subst.env)
  arg = subst.double.dot(arg, .subst.env)
  
  
  opt.name = dots_name[orows]
  opt = dots_val[orows]
  if (length(opt)==0)
    opt = NULL
  opt.call = opt
  opt.arg = NULL
  
  if (!is.null(opt)) {
    opt.name = subst.double.dot(opt.name, .subst.env)
    opt = subst.double.dot(opt, .subst.env)
    
    
    opt.name = str.left.of(opt.name,"(")
    opt.name[is.na(opt.name)] = opt[is.na(opt.name)]
    opt.name = substring(opt.name,2)
    
    
    opt.arg = str.right.of(opt,"(")
    opt.arg = str.trim(substring(opt.arg,1,nchar(opt.arg)-1))
  
    
    opt.call = opt
    
    
    opt = lapply(seq_along(opt.call), function(i) list(name=opt.name[i], arg=opt.arg[i], call=opt.call[i]))
    names(opt.arg) = names(opt.call) = names(opt) = opt.name
  }
  
  list(com = .command,opt=opt, opt.arg=opt.arg, opt.call=opt.call, opt.name = opt.name,arg.name = arg.name, arg=arg)
}

known.options = function(par, ...) {
  ko = unlist(list(...))
  uo = setdiff(par$opt.name, ko)
  if (length(uo)>0) {
    str = paste0("Warning: currently the option(s) ", paste0(uo, collapse=","), " are not implemented.")
    display(str)
  }
  if (!is.null(get.rs.by()) & (!"by:" %in% ko) )
      display("Warning: Not yet implemented for use after by or sortby")
}

make.dot.functions = function() {
  rs.file="D:/libraries/rs/rs/R/rs_commands.r"
  dot.file = "D:/libraries/rs/rs/R/dot_commands.r"
  
  env = new.env()
  source(rs.file, env)
  fun = ls(env)  
  fun = fun[str.starts.with(fun,"rs.")]
  fun = substring(fun,4)
  
  code = paste0(
'.',fun, ' = function(...) {
  par = rs.par(..., .subst.env = parent.frame())
  rs.',fun,'(par)
}
', collapse="\n")

  cat(code)

  writeLines(code, dot.file)
}

examples.make.empty.commands = function() {
  setwd("D:/libraries/rs/adapation_vertical/programs/data_prep")
  translate.do.file("data_prep_1.do")
  r.file = "data_prep_1.do.r"
}

make.empty.commands = function(r.file) {
  r.file = "data_prep_1.do.r"
  
  txt = readLines(r.file)
  
  # Commands
  str = str.extract.first(txt,"\\.([A-Za-z0-9_]+) ")
  str = setdiff(unique(str),NA)
  str = substring(str,2,nchar(str)-1)
  
  used.commands = str
  
  env = new.env()
  package.dir = "D:/libraries/rs/rs/R/"
  source(paste0(package.dir,"rs_commands.r"), env)
  str = ls(env)
  str = str[str.starts.with(str,"rs.")]
  str = substring(str,4)
  
  com = setdiff(used.commands, str)
  com = sort(com)
  
  code = paste0(' 
rs.',com,' = function(par,check.options=TRUE, ...) {
  restore.point("rs.',com,'")
  if (check.options) known.options(par)
  
  display("Command .',com,' not yet implemented.")
}', collapse="\n")
  
  cat(code)
  writeClipboard(code)
  
  code = paste0(
'.',com, ' = function(...) {
  par = rs.par(..., .subst.env = parent.frame())
  rs.',com,'(par)
}
', collapse="\n")
  cat(code)
  writeClipboard(code)
  
  
  # Functions
  
  str = unique(str.extract.all(merge.lines(txt),"\\.\\.([A-Za-z0-9_]+)\\(")[[1]])
  str = substring(str,3,nchar(str)-1)
  used.fun = str
  
  env = new.env()
  package.dir = "D:/libraries/rs/rs/R/"
  source(paste0(package.dir,"dotdot_functions.r"), env)

  str = ls(env)
  str = str[str.starts.with(str,"..")]
  str = substring(str,3)

  com = setdiff(used.fun, str)
  com = sort(com)
  
  code = paste0(' 
..',com,' = function(...) {
  dots = named_dots(...)
  restore.point("..',com,'")
  
  display("Function ..',com,' not yet implemented.")
}', collapse="\n")
  cat(code)
  writeClipboard(code)
}


# From package psych
describe = function (x, na.rm = TRUE, interp = FALSE, skew = FALSE, ranges = TRUE, 
          trim = 0.1, type = 3, check = TRUE) 
{
  cl <- match.call()
  valid <- function(x) {
    sum(!is.na(x))
  }
  if (!na.rm) 
    x <- na.omit(x)
  if (is.null(dim(x)[2])) {
    len <- 1
    stats = matrix(rep(NA, 10), ncol = 10)
    stats[1, 1] <- valid(x)
    stats[1, 2] <- mean(x, na.rm = na.rm)
    stats[1, 10] <- sd(x, na.rm = na.rm)
    if (interp) {
      stats[1, 3] <- interp.median(x, na.rm = na.rm)
    }
    else {
      stats[1, 3] <- median(x, na.rm = na.rm)
    }
    stats[1, 9] <- mean(x, na.rm = na.rm, trim = trim)
    stats[1, 4] <- min(x, na.rm = na.rm)
    stats[1, 5] <- max(x, na.rm = na.rm)
    stats[1, 6] <- skew(x, na.rm = na.rm, type = type)
    stats[1, 7] <- mad(x, na.rm = na.rm)
    stats[1, 8] <- kurtosi(x, na.rm = na.rm, type = type)
    vars <- 1
  }
  else {
    stats = matrix(rep(NA, ncol(x) * 10), ncol = 10)
    rownames(stats) <- colnames(x)
    stats[, 1] <- apply(x, 2, valid)
    vars <- c(1:ncol(x))
    if (check) {
      for (i in 1:ncol(x)) {
        if (!is.numeric(x[[i]])) {
          x[[i]] <- as.numeric(x[[i]])
          rownames(stats)[i] <- paste(rownames(stats)[i], 
                                      "*", sep = "")
        }
        if (!is.numeric(unclass(x[[i]]))) 
          stop("non-numeric argument to 'describe'")
      }
    }
    x <- as.matrix(x)
    stats[, 2] <- apply(x, 2, mean, na.rm = na.rm)
    stats[, 10] <- apply(x, 2, sd, na.rm = na.rm)
    if (skew) {
      stats[, 6] <- skew(x, na.rm = na.rm, type = type)
      stats[, 8] <- kurtosi(x, na.rm = na.rm, type = type)
    }
    if (ranges) {
      stats[, 7] <- apply(x, 2, mad, na.rm = na.rm)
      stats[, 4] <- apply(x, 2, min, na.rm = na.rm)
      stats[, 5] <- apply(x, 2, max, na.rm = na.rm)
      stats[, 9] <- apply(x, 2, mean, na.rm = na.rm, trim = trim)
      if (interp) {
        stats[, 3] <- apply(x, 2, interp.median, na.rm = na.rm)
      }
      else {
        stats[, 3] <- apply(x, 2, median, na.rm = na.rm)
      }
    }
  }
  if (ranges) {
    if (skew) {
      answer <- data.frame(vars = vars, n = stats[, 1], 
                           mean = stats[, 2], sd = stats[, 10], median = stats[, 
                                                                               3], trimmed = stats[, 9], mad = stats[, 7], 
                           min = stats[, 4], max = stats[, 5], range = stats[, 
                                                                             5] - stats[, 4], skew = stats[, 6], kurtosis = stats[, 
                                                                                                                                  8], se = stats[, 10]/sqrt(stats[, 1]))
    }
    else {
      answer <- data.frame(vars = vars, n = stats[, 1], 
                           mean = stats[, 2], sd = stats[, 10], median = stats[, 
                                                                               3], trimmed = stats[, 9], mad = stats[, 7], 
                           min = stats[, 4], max = stats[, 5], range = stats[, 
                                                                             5] - stats[, 4], se = stats[, 10]/sqrt(stats[, 
                                                                                                                          1]))
    }
  }
  else {
    if (skew) {
      answer <- data.frame(vars, n = stats[, 1], mean = stats[, 
                                                              2], sd = stats[, 10], skew = stats[, 6], kurtosis = stats[, 
                                                                                                                        8], se = stats[, 10]/sqrt(stats[, 1]))
    }
    else {
      answer <- data.frame(vars = vars, n = stats[, 1], 
                           mean = stats[, 2], sd = stats[, 10], se = stats[, 
                                                                           10]/sqrt(stats[, 1]))
    }
  }
  class(answer) <- c("psych", "describe", "data.frame")
  return(answer)
}

# Taken from the web
mfx<-function(x){
  restore.point("mfx")
  
  bb<-data.frame(x$coefficients)
  bb2<-data.frame(mean(as.data.frame(x$data),na.rm=T))
  
  bbh<-as.character(rownames(bb))
  bbmeans<-bb2[bbh[2:nrow(bb)],]
  jkj <-bb[2:nrow(bb),]*bbmeans
  #
  M3<-sum(jkj) + bb[1,]
  #
  if (ll<-x$family$link=="probit"){
    probitmfx <- data.frame(mfx=dnorm(M3)*bb[2:nrow(bb),],row.names=bbh[2:nrow(bb)])
  }
  else{probitmfx <- data.frame(mfx=dlogis(M3)*bb[2:nrow(bb),],row.names=bbh[2:nrow(bb)])}
  #
  bbse<-bb/summary(x)$coef[,2]
  mfxse<-probitmfx/bbse[2:nrow(bb),]
  #
  probitmfxfull <- data.frame(mfx=probitmfx,SE=mfxse,bbmeans,summary(x)$coef[2:nrow(bb),3],summary(x)$coef[2:nrow(bb),4],row.names=bbh[2:nrow(bb)])
  colnames(probitmfxfull) <- c("mfx","SE","Mean Value","z","Pr(>|z|)")
  #
  logl <- 0.5*(-x$aic + 2*nrow(bb))
  #McFadden's R2
  depen <- x$data[,1]
  depenglm <- glm(depen ~ 1, family=binomial(link=x$family$link),data=x$data)
  logldepen <- 0.5*(-depenglm$aic + 2)
  psr2<- 1 - (logl/logldepen)
  #
  obs<-nrow(x$data)
  #SBC/BIC
  sbc<- -2*logl + log(obs)*nrow(bb)
  #HQIC
  HIC<- -2*logl + 2*log(log(obs))*nrow(bb)
  #Logit?
  if (qq<-x$family$link=="logit"){
  }
  else{}
  #CDF of Mean Model
  if (ll == TRUE){
    BigProb<-pnorm(M3)
  }
  else {BigProb<-plogis(M3)}
  #LR Test
  LRTest<- -2*(logl - logldepen)
  dfLR<-nrow(bb) - 1
  LRp<-dchisq(LRTest,df=dfLR)
  LRdata <- data.frame(LRTest, dfLR,LRp)
  colnames(LRdata) <- c("Test Statistic","DF","P-Value")
  rownames(LRdata) <- "LR Test"
  #
  tests<-rbind(BigProb,logl,psr2,x$aic,HIC,sbc)
  tests<-data.frame(tests)
  colnames(tests)<-""
  rownames(tests)<-c("CDF(Evaluated at the Mean):","Log-Likelihood:","McFadden's R2:","Akaike Information Criterion:","Hannan-Quinn Criterion:","Schwarz's Bayesian Criterion:")
  #
  cat("MFX Function for Logit and Probit", "\n")
  cat("", "\n")
  if (qq<-x$family$link=="logit"){ cat("This is a Logit Model","\n")
  }
  else if (qq<-x$family$link=="probit"){ cat("This is a Probit Model","\n")
  }
  else {cat("","\n")}
  cat("", "\n")
  cat("Reporting Marginal Effects, Evaluated at the Mean", "\n")
  cat("", "\n")
  printCoefmat(probitmfxfull, P.value=TRUE, has.Pvalue=TRUE)
  cat("", "\n")
  cat("Observations:", obs, "\n")
  cat("", "\n")
  printCoefmat(tests, P.value=F, has.Pvalue=F)
  cat("", "\n")
  cat("Likelihood-Ratio Test:", "\n")
  printCoefmat(LRdata, P.value=T, has.Pvalue=T)
  cat("", "\n")
}