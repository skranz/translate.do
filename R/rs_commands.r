
rs.drop = function(par,check.options=TRUE, ...) {
  restore.point("rs.drop")
  if (check.options) known.options(par,"if")

  dt = get.rs.data()
  
  if ("if" %in% par$opt.name) {
    rows = eval(parse(text=par$opt.arg["if"]), dt)
    dt = dt[!rows,]
  }
  
  for (col in par$arg) {
    suppressWarnings(dt[,col:=NULL, with=FALSE])
  }
  set.rs.data(dt)
}


rs.keep = function(par,check.options=TRUE, ...) {
  restore.point("rs.keep")
  if (check.options) known.options(par,"if")
  
  dt = get.rs.data()
  if ("if" %in% par$opt.name) {
    rows = eval(parse(text=par$opt.arg["if"]), dt)
    dt = dt[rows,]
  }
  
  if (length(par$arg)>0) {
    rm.cols = setdiff(colnames(dt),par$arg.name)
    for (col in rm.col) {
      suppressWarnings(dt[,col:=NULL, with=FALSE])
    }
  }
  set.rs.data(dt)
 }


rs.use = function(par,check.options=TRUE, ...) {
  restore.point("rs.use")
  if (check.options) known.options(par,"verbose")
  
  file = remove.quotes(par$arg[1])
  if (!has.substr(file,"."))
    file = paste0(file,".dta")
  
  
  if (require(rio)){
    df = rio::import(file)
  } else {
    cat("\nInstall rio for better data import.")
    library(foreign)
    df = read.dta(file)
  }
  
  dt = as.data.table(df)
  
  cn = colnames(dt)
  
  # Change names
  rold = cn[str.starts.with(cn,"_")]
  if (length(rold)>0) {
    rnew = sc(substring(rold,2),"_")
    display("Renamed the columns ", sc(rold, collapse=",")," to ",sc(rnew, collapse=","),".")
    setnames(dt,rold,rnew)
  }
  
  
  set.rs.data(dt)

  verbose = TRUE
  if (verbose)
    display("Loaded ", file, " with ", NROW(dt), " rows and ", NCOL(dt), " columns.")
}


rs.sum = function(par,check.options=TRUE, ...) {
  restore.point("rs.sum")
  if (check.options) known.options(par,"if","by:")

  dt = get.rs.data(par)
  rs.by = get.rs.by()
  if (!is.null(rs.by)) {
    #com = paste0("list(", paste0("sum_",names(dots),"= summary(",names(dots),")", collapse=","),")"
    #ret = run.dt(dt,com,by=rs.by)
    
    ret = dt[, describe(.SD), by=eval(rs.by), .SDcols=par$arg.name]
    set.rs.by(NULL)
    return(ret)
  } else {
    val = describe(dt[,par$arg.name, with=FALSE])
    #names(val) = names(dots)
  }
  
  #print(as.data.table(as.list(val)))
  val
}  


rs.Sum = function(par,check.options=TRUE, ...) {
  restore.point("rs.sum")
  if (check.options) known.options(par,"if","by:")
  
  dt = get.rs.data(par)
  rs.by = get.rs.by()
  if (!is.null(rs.by)) {
    com = paste0("list(", paste0("sum_",names(dots),"= sum(",names(dots),")", collapse=","),")")
    ret = run.dt(dt,com,by=rs.by)
    set.rs.by(NULL)
    return(ret)
  } else {
    val = sapply(par$arg.name, function(col) sum(dt[[col]], na.rm=TRUE))
    #names(val) = names(dots)
  }
  
  print(as.data.table(as.list(val)))
  invisible(val)
}  


rs.replace = function(par,check.options=TRUE, ...) {
  restore.point("rs.replace")
  if (check.options) known.options(par,"by:","if")

  dt = get.rs.data()
   
  i = length(par$arg.name)
  var = par$arg.name[i]
  attr(dt,".last.var") = var
  arg = par$arg[i]
  by = get.rs.by()
  
  if (!is.null(par$opt[["if"]])) {
    rows = which(eval(parse(text=par$opt[["if"]]$arg,srcfile=NULL),envir=dt))
    if (length(rows)==0) {
      display("Warning: No rows satisfy the if condition, no values replaced.")
      set.rs.by(NULL)
      return()
    }
  } else {
    rows = NULL
  }

  if (!is.null(by)) {
    restore.point("rs.replace.by")
    arg = gsub(".N()",".N",arg,fixed=TRUE)
    arg = gsub(".n()","rs.row.range(1,.N)",arg,fixed=TRUE)
    

    com = paste0(var," := ", arg)
    dt = run.dt(dt,rows=rows,com,by=by)
    set.rs.by(NULL)
  } else {  
    com = paste0(var," := ", par$arg[i])    
    dt = run.dt(dt, com,rows=rows, with=TRUE)
  }
  
  if (length(par$arg.name)==2) {
    type = par$arg[1]
    if (str.starts.with(type, "str")) {
      dt[[var]] = as.character(dt[[var]])
    } else if (type %in% c("byte", "int","long")) {
      dt[[var]] = as.integer(dt[[var]])
    } else if (type %in% c("float", "double")) {
      dt[[var]] = as.numeric(dt[[var]])      
    } else {
      warning("Unknown type '", type,"'. No type conversion took place.")
    }
  }
  
  set.rs.data(dt)
}  

examples.rs.gen = function() {
  .gen(str1,d1=5)
}

rs.gen = function(par,check.options=TRUE, ...) {
  restore.point("rs.gen")
  if (check.options) known.options(par,"by:")
  
  suppressWarnings(rs.replace(par, check.options=FALSE))
}  


rs.egen = function(par,check.options=TRUE, ...) {
  restore.point("rs.egen")
  if (check.options) known.options(par,"by:")
  rs.gen(par, check.options=FALSE)
}


rs.sort = function(par,check.options=TRUE, ...) {
  restore.point("rs.sort")
  if (check.options) known.options(par)
  
  dt = get.rs.data()
  setkeyv(dt, par$arg.name)
  set.rs.data(dt)  
}

rs.clear =  function(par,check.options=TRUE, ...) {
  restore.point("rs.clear")
  if (check.options) known.options(par)
  
  display(".clear not yet implemented")  
}

rs.cd =  function(par,check.options=TRUE, ...) {
  restore.point("rs.cd")
  if (check.options) known.options(par)
  dir = remove.quotes(par$arg[1])
  
  setwd(dir)
}

#.merge (gvkey,year, .using= PiM)# merges PiDifce and ab_ret 

rs.merge = function(par,check.options=TRUE, ...) {
  restore.point("rs.merge")
  if (check.options) known.options(par,"using")
  
  file = remove.quotes(par$opt.arg[["using"]])
  if (!has.substr(file,"."))
    file = paste0(file,".dta")
  
  library(foreign)
  dt = get.rs.data()
  dt2 = as.data.table(read.dta(file))
  
  old.keys = key(dt)
  cols = par$arg.name
  setkeyv(dt,cols)
  setkeyv(dt2,cols)
  
  mdt = merge(dt,dt2)
  
  set.rs.data(mdt)
}

rs.by = function(par,check.options=TRUE, ...) {
  restore.point("rs.by")
  if (check.options) known.options(par)
  
  set.rs.by(par$arg.name)
}


rs.bysort = function(par,check.options=TRUE, ...) {
  restore.point("rs.by")
  if (check.options) known.options(par)
  
  set.rs.by(par$arg.name)
}


rs.save = function(par, check.options=TRUE) { 
  restore.point("rs.save")
  if (check.options) known.options(par)
  
  file = par$arg[[1]]
  dt = get.rs.data()
  
  for (col in colnames(dt)) {
    if (is.character(dt[[col]])) {
      rows = dt[[col]] == ""
      dt[rows,col:=".", with=FALSE]
    }
      
  }
  write.dta(dt,file, version=8L)
  
  verbose= !is.false(par$opt[["verbose"]]$arg)
  if (verbose)
    display("Saved ", file, " with ", NROW(dt), " rows and ", NCOL(dt), " columns.")
}

rs.probit = function(par, check.options=TRUE,...) {
  restore.point("rs.probit")
  arg = par$arg;
  if (check.options) known.options(par,"if")
  
  yvar = arg[1]
  xvars = arg[-1]
  formula = paste0(yvar, "~", paste0(xvars, collapse="+"))
  
  dt = get.rs.data(par)
  
  mod = glm(formula = formula, family=binomial(link="probit"), data=dt, x=TRUE )
  set.rs.model(mod)
  as.rs.model(mod)
}

rs.dprobit = function(par,check.options=TRUE, ...) {
  restore.point("rs.dprobit")
  if (check.options) known.options(par,"if")
  rs.probit(par,check.options=FALSE)
  
  mod = get.rs.model()
  as.rs.model(mod, show.mar.eff = TRUE)
  #mfx(mod)
}

#.ttest (total1,.by(link_loan_rel_bf_fc1)) 
rs.ttest = function(par,check.options=TRUE, ...) {
  restore.point("rs.ttest")
  if (check.options) known.options(par,"if", "by", "unequal", "unpaired", "level")
  
  arg = par$arg.name[1]
  by = par$opt[["by"]]$arg
  var.equal = !("unequal" %in% par$opt.name)
  paired = !("unpaired" %in% par$opt.name) & (is.null(by))
  level = 0.95
  
  
  if ("level" %in% par$opt.name)
    level = as.numeric(par$opt.arg["level"])
  
  
  dt = get.rs.data(par)
  if (is.null(by)) {
    if (has.substr(arg,"==")) {
      x = str.trim(str.left.of(arg,"=="))
      y = str.trim(str.right.of(arg,"=="))
      mu = as.numeric(y)
      
      # One variable with mu given
      if (!is.na(mu)) {
        ret = t.test(x=dt[[x]],mu=mu, conf.level = level)        
      
      # Two variables
      } else {
        ret = t.test(x=dt[[x]],y=dt[[y]],paired=paired, var.equal=var.equal, conf.level = level)         
      }
    } else {
      x = arg
      ret = t.test(x=dt[[x]],conf.level = level)        
    }
    
  # By is given
  } else {
    form = as.formula(paste0(arg, "~", by))
    #t.test(formula=form, data=dt)
    ret =  t.test(formula=form,paired=paired, var.equal=var.equal, conf.level = level, data=dt)
  }
  ret
}


rs.capture = function(par,check.options=TRUE, ...) {
  restore.point("rs.capture")
  if (check.options) known.options(par)
  
  display("Command .capture not yet implemented.")
}

rs.collapse = function(par,check.options=TRUE, ...) {
  restore.point("rs.collapse")
  if (check.options) known.options(par)
  
  display("Command .collapse not yet implemented.")
}

rs.compress = function(par,check.options=TRUE, ...) {
  restore.point("rs.compress")
  if (check.options) known.options(par)
  
  display("Command .compress not yet implemented.")
}

rs.destring = function(par,check.options=TRUE, ...) {
  restore.point("rs.destring")
  if (check.options) known.options(par)

  dt = get.rs.data(par)
  var = par$arg.name[1]
  dt[[var]] = as.numeric(dt[[var]])
  set.rs.data(dt)
  return()
}

rs.dta = function(par,check.options=TRUE, ...) {
  restore.point("rs.dta")
  if (check.options) known.options(par)
  
  display("Command .dta not yet implemented.")
}

rs.erase = function(par,check.options=TRUE, ...) {
  restore.point("rs.erase")
  if (check.options) known.options(par)
  
  display("Command .erase not yet implemented.")
}

rs.expand = function(par,check.options=TRUE, ...) {
  restore.point("rs.expand")
  if (check.options) known.options(par)
  
  display("Command .expand not yet implemented.")
}

rs.label = function(par,check.options=TRUE, ...) {
  restore.point("rs.label")
  if (check.options) known.options(par)
  
  dt = get.rs.data(par)
  var = par$arg[[2]]
  label = par$arg[[3]]
  if (!var %in% colnames(dt)) {
    cat("\nNo variable ", var, " for labelling not found.")
    return()
  }
  attr(dt[[var]],"label") = label
  set.rs.data(dt)
}

rs.log = function(par,check.options=TRUE, ...) {
  restore.point("rs.log")
  if (check.options) known.options(par)
  
  display("Command .log not yet implemented.")
}

rs.nlogitgen = function(par,check.options=TRUE, ...) {
  restore.point("rs.nlogitgen")
  if (check.options) known.options(par)
  
  display("Command .nlogitgen not yet implemented.")
}

rs.nlogittree = function(par,check.options=TRUE, ...) {
  restore.point("rs.nlogittree")
  if (check.options) known.options(par)
  
  display("Command .nlogittree not yet implemented.")
}

rs.rename = function(par,check.options=TRUE, ...) {
  restore.point("rs.rename")
  if (check.options) known.options(par)
  
  display("Command .rename not yet implemented.")
}

rs.set = function(par,check.options=TRUE, ...) {
  restore.point("rs.set")
  if (check.options) known.options(par)
  
  display("Command .set not yet implemented.")
}

rs.tab = function(par,check.options=TRUE, ...) {
  restore.point("rs.tab")
  if (check.options) known.options(par)
  
  display("Command .tab not yet implemented.")
}

rs.tostring= function(par,check.options=TRUE, ...) {
  restore.point("rs.tostring")
  if (check.options) known.options(par,"gen","generate")
  
  oldvar = par$arg.name
  if (!is.null(par$opt$gen)) {
    newvar = as.character(par$opt$gen$arg)
  } else {
    newvar = oldvar
  }
  dt = get.rs.data(par)
  com = paste0(newvar,"= as.character(",oldvar,")", collapse=",")
  dt = s_mutate(dt,com)
  set.rs.data(dt)
}



