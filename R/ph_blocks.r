example = function() {
  str = "Hallo ((L2) Hier) und (noch) einer 'Mein Name lautet (Hi)'."
  start = "("
  end = ")"
  res = blocks.to.placeholder(str, start=c("'","("), end=c("'",")"))
  res
  
  res = blocks.to.placeholder(str, start, end)
  
  str = res$str; ph.df = res$ph.df  
  replace.placeholders(str, ph.df)
}



replace.placeholders = function(str, ph.df, recursive = TRUE, rows=NULL) {
  restore.point("ksjlfdlfjdfdfl")
  multiline = length(str)>1
  
  str = merge.lines(str)
  if (!is.null(rows))
    rows = which(stringi::stri_detect_fixed(str, ph.df$ph))
  for (row in rows) {
    str = gsub(ph.df$ph[row],ph.df$content[row],str,fixed=TRUE)
  }
  if (recursive) {
    rows = which(stringi::stri_detect_fixed(str, ph.df$ph))
    if (length(rows)>0) {
      restore.point("ksjlfdlfjl")
      str = replace.placeholders(str, ph.df, recursive = TRUE, rows=rows)  
    }
  }
  if (multiline)
    str = sep.lines(str)
  str
}

ignore.from.pattern = function(str, pattern, ...) {
  ignore.pos = str.find(str, pattern, ...)
  ignore = get.ignore(ignore.pos = ignore.pos, str = str)
  if (!any(ignore==TRUE)) ignore=NULL
  ignore
}

empty.ph.df = function() {
  data.frame(ph=character(0), ph.ind=integer(0), content=character(0))
}



lines.pos.to.placeholder = function(str, lines.pos, ph.prefix = "#~", ph.suffix="~#", ph.df = empty.ph.df()) {
  restore.point("lines.pos.to.placeholder")
  if (NROW(ph.df)>0) {
    ph.counter = max(ph.df$ph.ind)+1
  } else {
    ph.counter = 1
  }
  lines = which(!is.na(lines.pos[,1]) & !is.na(lines.pos[,2]))
  n = length(lines)
  if (n==0) return(list(str=str, ph.df=ph.df))
  lines.pos = lines.pos[lines,,drop=FALSE]
  
  content = substring(str[lines], lines.pos[,1], lines.pos[,2])
  ph.ind = ph.counter:(ph.counter+n-1)
  ph = paste0(ph.prefix, ph.ind, ph.suffix)  
  ph.df = rbind(ph.df, data.frame(ph=ph, ph.ind=ph.ind, content=content))
  str[lines] = paste0(
    substring(str[lines], 1, lines.pos[,1]-1),
    ph,
    substring(str[lines], lines.pos[,2]+1, nchar(str[lines]))
  )
    
  return(list(str=str, ph.df = ph.df))

}


lines.to.placeholder = function(str, lines, ph.prefix = "#~", ph.suffix="~#", ph.df = empty.ph.df()) {
  restore.point("lines.to.placeholder")
  if (NROW(ph.df)>0) {
    ph.counter = max(ph.df$ph.ind)+1
  } else {
    ph.counter = 1
  }
  n = length(lines)
  if (n==0) return(list(str=str, ph.df=ph.df))
  content = str[lines]
  ph.ind = ph.counter:(ph.counter+n-1)
  ph = paste0(ph.prefix, ph.ind, ph.suffix)  
  ph.df = rbind(ph.df, data.frame(ph=ph, ph.ind=ph.ind, content=content))
  str[lines] = ph
  return(list(str=str, ph.df = ph.df))

}

blocks.to.placeholder = function(str,start, end, ph.prefix = "#~", ph.suffix = "~#", ph.df = empty.ph.df(),ignore.pattern=NULL, ignore.start.pattern=ignore.pattern, ignore.end.pattern = ignore.pattern) {
  if (length(start)>1) {
    for (i in seq_along(start)) {
      res = blocks.to.placeholder(str, start[i], end[i], ph.prefix, ph.suffix, ph.df, ignore.start = ignore.start, ignore.end=ignore.end)
      str = res$str; ph.df = res$ph.df
    }
    return(res)
  }
  
  restore.point("blocks.to.placeholder")
  #stop()
  stopifnot(length(str)==1)
  if (NROW(ph.df)>0) {
    ph.counter = max(ph.df$ph.ind)+1
  } else {
    ph.counter = 1
  }
  old.ph.df = ph.df
  
  ignore.start = ignore.end = NULL
  if (!is.null(ignore.start.pattern)) {
    ignore.start = ignore.from.pattern(str, ignore.start.pattern)
  }
  if (!is.null(ignore.end.pattern)) {
    ignore.end = ignore.from.pattern(str, ignore.end.pattern)
  }
  
  pos = str.blocks.pos(str,start = start, end=end, ignore.start = ignore.start, ignore.end=ignore.end)
  n = NROW(pos$outer)
  if (n == 0) {
    return(list(str=str, ph.df = ph.df))
  }
  

  if (max(pos$levels)>1) {
    redo = TRUE
    use = which(pos$levels == max(pos$levels))
    pos$outer = pos$outer[use,,drop=FALSE]
    pos$inner = pos$inner[use,,drop=FALSE]
    pos$levels = pos$levels[use]
    n = NROW(pos$outer)
  } else {
    redo = FALSE
  }
  
  
  content = substring(str, first = pos$outer[,1],last=pos$outer[,2])
  ph.ind = ph.counter:(ph.counter+n-1)
  ph = paste0(ph.prefix, ph.ind, ph.suffix)  
  
  res.str = str.replace.at.pos(str, ph, pos = pos$outer)
  ph.df = rbind(old.ph.df, data.frame(ph=ph, ph.ind=ph.ind, content=content))
  
  if (redo) {
    restore.point("lknxlflkdfkldfj")
    res = blocks.to.placeholder(res.str,start = start, end=end,ph.prefix = ph.prefix,ph.suffix = ph.suffix, ph.df = ph.df, ignore.start = ignore.start, ignore.end=ignore.end)
    return(res)
  } else {
    return(list(str=res.str, ph.df = ph.df))
  }
  
}

#str =  '"this \\"name " *5'; stata.strings.to.ph(str)

stata.strings.to.ph = function(str, ...) {
  #res = blocks.to.placeholder(str, start='"', end='"', ignore.pattern='\\"',...)
  # problem a valid string is "C:\". I don't know how nested quotes
  # are done in Stata
  res = blocks.to.placeholder(str, start='"', end='"',...)

  res
}