replace.delimit.semicolon = function(txt) {
  restore.point("replace.delimit.semicolon")
  rows = which(has.substr(tolower(txt), "#delimit") | has.substr(tolower(txt), "#del"))
  if (length(rows)==0) return(txt)

  # Create alternating sequence of delimit; and delimit cr
  code = gsub(" ", "", tolower(txt[rows]), fixed=TRUE)
  semi.delimit = has.substr(code, "#delimit;") | has.substr(code, "#del;")
  if (sum(semi.delimit)==0) return(txt)
  
  use = rep(TRUE, length(rows))
  prev = FALSE
  for (i in seq_along(rows)) {
    use[i] = semi.delimit[i] != prev
    prev = use[i]
  }
  rows = rows[use]
  n = length(rows)
  
  start = rows[seq(1,n, by=2)]+1
  if (n > 1) {
    end = c(rows[seq(2,n,by=2)], NROW(txt))
  } else {
    end = NROW(txt)
  }

  remove.lines = rep(FALSE, NROW(txt))
  for (i in seq_along(start)) {
    str = inner.replace.delimit.semicolon(txt[start[i]:end[i]])
    txt[start[i]] = str
    if (end[i]-start[i]>1) {
      remove.lines[(start[i]+1):end[i]] = TRUE
    }
  }
  txt = txt[!remove.lines]
  trimws(sep.lines(txt))
}

inner.replace.delimit.semicolon = function(str) {
  # TO DO: Does not yet handle ; inside string constants
  restore.point("inner.replace.delimit.semicolon")
  str = merge.lines(str)
  
  # Put strings into blocks
  #res = blocks.to.placeholder(str, start='"', end='"', ignore.start = '\\"', ignore.end = '\\"',ph.prefix = "<.~",ph.suffix="~.>")
  #str = res$str; ph.df = res$ph.df 

  str = gsub("\n","",str, fixed=TRUE)
  str = gsub(";","\n", str, fixed=TRUE)
  #str = replace.placeholders(str, ph.df)
  str
}
