deparse1 = function (call, collapse = "") {
  paste0(deparse(call, width = 500), collapse = collapse)
}




#trimws_around("x \t  = 5\nk == 4","=")
trimws_leftof = function(str, around, whitespace="[\t ]*") {
  pattern = paste0(whitespace, around)
  str = gsub(pattern, around, str)
  str
}
trimws_rightof = function(str, around, whitespace="[\t ]*") {
  pattern = paste0(around,whitespace)
  str = gsub(pattern, around, str)
  str
}
trimws_around = function(str, around, whitespace="[\t ]*") {
  pattern = paste0(whitespace,around,whitespace)
  str = gsub(pattern, around, str)
  str
}