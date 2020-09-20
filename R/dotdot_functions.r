..monthly = function(...) {
  dots = named_dots(...)
  restore.point("..monthly")
  
  var = names(dots)[1]
  monthly(get.rs.data()[[var]], dots[[2]])
}


monthly = function(date_str, stata.format="mY",r.format=NULL, locale="C", as.Date=TRUE) {
  restore.point("monthly")
  library(lubridate)
  if (is.null(r.format)) {
    r.format = gsub("M","m", stata.format, fixed=TRUE)
  }
  r.format = paste0("d",r.format)
  date_str = paste0(01,date_str)
  res = parse_date_time(date_str,orders=r.format,locale=locale)
  if (as.Date) res = as.Date(res)
  res
}



..ends = function(...) {
  dots = named_dots(...)
  restore.point("..ends")
  
  display("Function ..ends not yet implemented.")
}

..floor = function(...) {
  dots = named_dots(...)
  restore.point("..floor")
  
  display("Function ..floor not yet implemented.")
}

..group = function(...) {
  dots = named_dots(...)
  restore.point("..group")
  
  display("Function ..group not yet implemented.")
}

..ln = function(...) {
  dots = named_dots(...)
  restore.point("..ln")
  
  display("Function ..ln not yet implemented.")
}

..max = function(..., na.rm=TRUE) {
  max(..., na.rm=na.rm)
}

..min = function(..., na.rm=TRUE) {
  min(..., na.rm=na.rm)
}

..org_form = function(...) {
  dots = named_dots(...)
  restore.point("..org_form")
  
  display("Function ..org_form not yet implemented.")
}

..rmax = function(...) {
  dots = named_dots(...)
  restore.point("..rmax")
  
  display("Function ..rmax not yet implemented.")
}

..rmin = function(...) {
  dots = named_dots(...)
  restore.point("..rmin")
  
  display("Function ..rmin not yet implemented.")
}

..substr = function(var, start, len=nchar(start)) {
  return(substring(var,start, start+len-1))
}

..sum = function(...) {
  sum(...)
}
