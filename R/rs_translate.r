
examples.translate.do.file = function() {
  library(stringtools)
  library(sktools)
  library(pryr)
  library(restorepoint)
  options(warn=2) 
  
  setwd("D:/libraries/translate.do/moral_hazard")
  translate.do.file("summaries_olds_news_g.do")
  
  setwd("D:/libraries/rs/adapation_vertical/programs/desc_stats")
  translate.do.file("desc_stats.do")
  
  setwd("D:/libraries/rs/adapation_vertical/programs/data_prep")
  translate.do.file("data_prep_1.do")
  .cd ("D:/libraries/rs/adapation_vertical")
  
  setwd("D:/libraries/translate.do/banking_network")
  translate.do.file("transaction.do")
  .use("transaction_account_data.dta")
  

  setwd("D:/libraries/translate.do/banking_network")
  translate.do.file("survey.do")
  .use("survey_data_file.dta")
  
  setwd("D:/libraries/translate.do/banking_network")
  translate.do.file("survey.do")
  .use("survey_data_file.dta")
 
  setwd("D:/libraries/translate.do/shrouded_fees/AER_figures_1and2")
  translate.do.file("amfi_monthly_netflows_hugh_AER.do")
  
  .use("survey_data_file.dta")

  setwd("D:/libraries/translate.do/shrouded_fees/AER_table2_SummaryStats")
  translate.do.file("summary_stats_final.do")

  
  library(foreign)
  file = "D:/libraries/translate.do/teachers_incentives_data/Incentives_FINAL/Raw Data/RandomCheck/Roster/enroll1.dta"
  read.dta(file)
  
  X = 5
  .gen ( d..X..= sl_no*2)
}

examples.translate_n =  function() {
  txt = c(".gen (x = _n)", ".gen (_num = ak_na + _n)")
}

translate_n = function(txt) { 
  restore.point("translate_n")
  regexp = "(?<![a-zA-Z0-9_])(_n)(?![a-zA-Z0-9_])"
  txt = gsub(regexp,"\\.n\\(\\)",txt,perl=TRUE)

  regexp = "(?<![a-zA-Z0-9_])(_N)(?![a-zA-Z0-9_])"
  txt = gsub(regexp,"\\.N\\(\\)",txt,perl=TRUE)
  
  txt
}

extract.by = function(txt) {
  restore.point("extract.by")
  com = str.left.of(txt, " ")
  
  rows = is.true(com=="by" | com == "bysort")
  str = txt[rows]
  com = com[rows]

  start.pos = nchar(com)+2
  end.pos = str.locate.first(str, ":")[,1,drop=FALSE]-1

  str = substring(str,start.pos,end.pos)
  str = gsub(" ",",",str)
  r.code = sc(".",com," (",str,");")
  
  remaining = txt
  remaining[rows] = str.trim(substring(txt[rows],end.pos+2,))
  
  all.r.code = rep("",length(txt))
  all.r.code[rows] = r.code
  
  list(rows = rows, r.code = all.r.code, remaining.code = remaining)
}

extract.for = function(txt) {
  restore.point("extract.for")
  com = str.left.of(txt, " ")
  
  remaining.code = txt
  
  rows = is.true(com=="for")
  str = txt[rows]
  com = com[rows]
  
  str = str.right.of(str, " ")
  # Type of for list var or num
  varnum = str.left.of(str," ")
  str = str.right.of(str, " ")
    
  start.pos = 1
  end.pos = str.locate.first(str, ":")[,1,drop=FALSE]-1
  
  li.str = str.trim(substring(str,start.pos,end.pos))
  
  li.str[varnum=="num"] = gsub("/",":",li.str[varnum=="num"])
  
  srows = varnum=="var"
  st = gsub(" ",'","',li.str[srows],fixed=TRUE)
  li.str[srows] = paste0('c("',st,'")')
  
  scom = str.trim(substring(str, end.pos+2))
  scom = gsub("X","._X_.", scom,fixed=TRUE)
  
  
  r.code = paste0('for (X in ', li.str,') {\n\t') 

  all.r.code = rep("",length(txt))
  all.r.code[rows] = r.code

  post.r.code = rep("",length(txt))
  post.r.code[rows] = "\n}"
  
  remaining.code[rows] = scom
  
  list(rows = rows, pre.r.code = all.r.code, post.r.code = post.r.code, remaining.code = remaining.code)
  
}

examples.extract.comments = function() {
  
  txt = '  
   /* Comment*/

    drop _all  /* Comment*/
    set mem 500m
  /*
    set matsize 800
    set more off      
    capture log close
  */  
    gen x = 5
* A star comment
  '
  txt = sep.lines(txt)
  
  extract.comments(txt)
}

extract.comments = function(txt) {
  
  str = merge.lines(txt)
  c.pos = str.blocks.pos(str,"/*","*/")$outer
  nl.pos = str.locate.all(str,"\n")[[1]][,1]
  
  cc.pos = pos.with.complement(c.pos)
  is.comment.pos = !attr(cc.pos,"complement")
  nl.int = findInterval(nl.pos,cc.pos[,1])
  
  rows = which(c(FALSE,is.comment.pos[nl.int])) 
  if (length(rows)>0)
    txt[rows] = paste0("#§#",txt[rows])
  
  rows = which(str.starts.with(txt,"*"))
  if (length(rows)>0)
    txt[rows] = paste0("#§#",substring(txt[rows],2))
  
  txt = gsub("/*","#§#",txt, fixed=TRUE)
  txt = gsub("*/","",txt, fixed=TRUE)
  
  comment = str.right.of(txt,"#§#")
  code = str.left.of(txt,"#§#")
  code[is.na(code)] = txt[is.na(code)]

  return(nlist(code, comment))
}


extract.using = function(txt) {
  restore.point("extract.using")
  pattern = "using "
  N = NROW(txt)
  rows = has.substr(txt,pattern)
  
  str = txt[rows]
  pos = str.locate.first(str, pattern)
  start.pos = pos[,1,drop=FALSE]
  
  sr = substring(str, pos[, 2] + 1, )
  end.pos = str.locate.first(sr, ",")[,1]-1
  end.pos[is.na(end.pos)] = nchar(sr)[is.na(end.pos)]
  
  path = substring(sr,1,end.pos)
  path = gsub('"','', path, fixed=TRUE)
  path = gsub("'",'', path, fixed=TRUE)
  r.code = paste0('.using("', path, '")')
  
  end.pos = end.pos + pos[, 2]
  
  remaining = txt
  remaining[rows] = paste0(substring(txt[rows],1, start.pos-1),"",substring(txt[rows],end.pos+1))
  
  all.r.code = rep("",N)
  all.r.code[rows] = r.code
    
  list(rows = rows, r.code = all.r.code, remaining.code = remaining)
}


extract.if = function(txt) {
  restore.point("extract.if")
  pattern = "if "
  N = NROW(txt)
  rows = has.substr(txt,pattern)
  
  str = txt[rows]
  pos = str.locate.first(str, pattern)
  start.pos = pos[,1,drop=FALSE]
  
  sr = substring(str, pos[, 2] + 1, )
  end.pos = str.locate.first(sr, ",")[,1]-1
  end.pos[is.na(end.pos)] = nchar(sr)[is.na(end.pos)]
  
  cond = substring(sr,1,end.pos)
  r.code = paste0('  .if (', cond,")")
  
  end.pos = end.pos + pos[, 2]
  
  remaining = txt
  remaining[rows] = str.trim(paste0(substring(txt[rows],1, start.pos-1),"",substring(txt[rows],end.pos+1)))
  
  all.r.code = rep("",length(txt))
  all.r.code[rows] = r.code
  
  list(rows = rows, r.code = all.r.code, remaining.code = remaining)
}


extract.com.arg.opt = function(txt) {
  restore.point("extract.com.arg.opt")

  txt = str.trim(txt)
    
  single.rows = !has.substr(txt," ")
  
  # Extract commands
  com = str.left.of(txt, " ")
  com[single.rows] = txt[single.rows]
  
  com.end.pos = nchar(com)
  
  # Options start with first comma outside braces
  #pos = str.locate.first(txt, ",")
  pos = do.call("rbind",lapply(seq_along(txt),function(row) {
    #restore.point("jfdhfkd")
    s = txt[row]
    pos = try(str.blocks.pos(s,start="(", end=")")$outer,silent = TRUE)
    if (is(pos,"try-error")) return(c(NA,NA))
    str.locate.first(s, ",", ignore.pos = pos)
  }))
      
  arg.end.pos = pos[,1,drop=FALSE]-1
  arg.end.pos[is.na(arg.end.pos)] = nchar(txt[is.na(arg.end.pos)])
  
  arg.str = str.trim(substring(txt,com.end.pos+2, arg.end.pos))
  arg.str[is.na(arg.str)] = ""
  arg.rows = nchar(arg.str)>0
  r.arg = arg.str
  r.arg[arg.rows] = translate.args(com[arg.rows], arg.str[arg.rows])
  
  opt.start.pos = pos[,1,drop=FALSE]+1
  opt.str = str.trim(substring(txt,opt.start.pos,))
  opt.str[is.na(opt.str)] = ""
  opt.rows = nchar(opt.str)>0
  r.opt = opt.str
  r.opt[opt.rows] = translate.opts(com[opt.rows], opt.str[opt.rows])
  
  list(com=list(rows = seq_along(txt),r.code=paste0(".",com), com=com),
       arg = list(rows = arg.rows, r.code = r.arg),
       opt = list(rows = opt.rows, r.code = r.opt)
  )
}

translate.args = function(com, args) {
  restore.point("translate.args")
  args = str.trim(args)
  
  # Replace =.  with =NA
  args = gsub("=.","=NA§",args, fixed=TRUE)
  args = gsub("=NA§_","=._",args, fixed=TRUE)
  args = gsub("=NA§","=NA",args, fixed=TRUE)
  #args
  
  # Add . before and after function name
  args = gsub("([a-zA-Z_]+)\\(","\\.\\.\\1\\(",args)
  
  r.args = gsub(" ",",",args)
  
  # Set paths into ""
  rows = com=="use" | com == "cd" | com =="save" | com=="erase"
  rows[rows] = ! (grepl('"', args[rows], fixed=TRUE) | grepl("'", args[rows], fixed=TRUE))
  r.args[rows] = sc('"',args[rows],'"')
  
  # Translate i. to factor
  r.args = gsub("i\\.([0-9a-zA-Z_]*)","factor(\\1)", r.args)
  
  
  r.args
}

translate.opts = function(com, opts) {
  restore.point("translate.opts")
  opts = str.trim(opts)
  

  opts = gsub(" (", "(", opts, fixed = TRUE)
  opts = gsub(" (", "(", opts, fixed = TRUE)
  opts = gsub("r cluster", "r.cluster", opts, fixed = TRUE)
  
  r.opts = unlist(lapply(opts, function(opt) {
    if (nchar(opt)==0)
      return("")
    sc(".",sep.lines(opt,","), collapse=",")
  }))
  r.opts
}

translate.do = function(txt) {
  restore.point("translate.do")  
  
  
  
  txt = gsub("\\","/",txt,fixed=TRUE) # Replace directories

  comment.info = extract.comments(txt)
  txt = comment.info$code
  
  
  # Command rows
  trim.txt = str.trim(str.single.space(txt))
  
  trim.txt = adjust.xi(trim.txt)
  
  start = substring(trim.txt,1,1)
  is.command = start %in% c(letters,LETTERS)
  
  # Commands with parameters
  rows = is.command
  str = trim.txt[rows]
  # Remove quietly
  rows = startsWith(str, "quietly ")
  str[rows] = trimws(str.right.of(str[rows],"quietly "))
  
  # Remove spaces and tabs around =
  str = gsub("[ \t]*=[ \t]*","=",str)
  
  
  for.info = extract.for(str)
  str = for.info$remaining.code
  
  by.info = extract.by(str)
  str = by.info$remaining.code
  
  using.info = extract.using(str)
  str = using.info$remaining.code
  
  if.info = extract.if(str)
  str = if.info$remaining.code
  
  ret = extract.com.arg.opt(str)
  com.info = ret$com; arg.info = ret$arg; opt.info=ret$opt
  
  
  # Combine the code
  code = sc(com.info$r.code," (", arg.info$r.code)
  right.comma = arg.info$rows
  
  for (info in list(using.info, if.info, opt.info)) {
    left.comma = info$rows
    comma = ifelse(right.comma & left.comma, ",","")
    code = sc(code,comma,info$r.code)
    right.comma = left.comma | right.comma
  }
  code = sc(code,")")
  code = sc(by.info$r.code, code)
  code = sc(for.info$pre.r.code, code, for.info$post.r.code)
  
  code = translate_n(code)
  
  # Replace variables starting with _
  # Remove initial _ and add _ to end
  regexp = "(?<![a-zA-Z0-9_])_([a-zA-Z0-9_]+)"
  code = gsub(regexp,"\\1_",code, perl=TRUE)
  
  code
  
  txt[is.command] = code
  
  rows = !is.na(comment.info$comment) 
  txt[rows] = sc(txt[rows], "#",comment.info$comment[rows])
  txt
}

adjust.xi = function(txt) {
  # remove xi:
  restore.point("adjust.xi")
  rows = str.starts.with(txt, "xi:") | str.starts.with(txt, "xi:")
  str = txt[rows]
  str = str.trim(str.right.of(str,":"))
  txt[rows] = str
  txt
}

#' Translate a do file into an R file
#' @param do.file The name of the do.file
#' @param r.file The name of the R file
#' @export
translate.do.file = function(do.file=NULL, r.file = sc(do.file,".r")) {
  restore.point("translate.do.file")
  
  txt = readLines(do.file)
  txt = translate.do(txt)
  writeLines(txt, r.file)
}