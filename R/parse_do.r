example.normalize.do = function() {
  txt = readLines("C:/libraries/translate.do/example2.do")
  txt = readLines("C:/libraries/translate.do/teachers_incentives_data/Incentives_FINAL/Programs/Table 9 Mid Panel BEF.do")  
  
  do.files = list.files("C:/libraries/translate.do/",glob2rx("*.do"),full.names = TRUE,recursive = TRUE)
  
  
  tab = lapply(do.files, function(file) {
    txt = readLines(file) 
    cat("\n", file)
    writeLines(txt,"temp.do")
    pho = normalize.do(txt)
    tab = do.to.table(pho=pho)
    tab$file = file
    tab
  }) %>% bind_rows() %>% filter(cmd != "")
  
  agg = tab %>%
    filter(cmd != "") %>%
    group_by(cmd) %>%
    summarize(
      count = n()
    ) %>%
    arrange(-count)
  
  
  file = "C:/libraries/translate.do/teachers_incentives_data/Incentives_FINAL/Programs/Table 9 Mid Panel BEF.do"
  txt = readLines(file)  
  writeLines(txt,"temp.do")
  pho = normalize.do(txt)
  writeLines(pho$str,"temp.do")
  
  tab = do.to.table(pho=pho)

    
  cat(merge.lines(pho.txt))
  pho
  org = replace.placeholders(merge.lines(pho$str), pho$ph.df)
  cat(org)
}

# Put do files in a normalized form
# with placeholders for comments and strings
# Also deal with #delimit commands

normalize.do = function(txt) {
  restore.point("normalize.do")  
  txt = trimws(sep.lines(txt))
  
  txt = merge.lines(txt)
  pho = blocks.to.placeholder(txt, start=c("/*"), end=c("*/"), ph.prefix = "#~co")
  txt = pho$str; ph.df = pho$ph.df
  
  # Remove line breaks from comments
  ph.df$content = gsub("\n","", ph.df$content, fixed=TRUE)
  
  # Transform quoted strings into placeholders
  # Problem if line comments contain unclosed ".
  
  # But if we first replace line comments, we may have a problem
  # in #delimit; code if strings contain ;
  pho = stata.strings.to.ph(txt,ph.df = ph.df,ph.prefix = "#~st")  
  txt = pho$str; ph.df = pho$ph.df
  pho
  
  # Transform #delimit ; code into normal line break code
  txt = replace.delimit.semicolon(sep.lines(txt))

  
  # Comment lines starting with * to ph
  # We first need to remove inline placeholders that might be put before
  # the *
  # otherwise we might not detect a * line
  ph.empty = ph.df %>% filter(startsWith(ph, "#~co")) %>% mutate(content="")
  temp.txt =  replace.placeholders(txt, ph.empty) %>% trimws()
  rows = which(startsWith(temp.txt, "*"))
  pho = lines.to.placeholder(txt,rows,ph.df=ph.df, ph.prefix = "#~cl")
  txt = pho$str; ph.df = pho$ph.df
  
  # End of line comments starting with // to ph
  lines.pos = str.find(txt, "//", first=TRUE)
  lines.pos[,2] = nchar(txt)
  pho = lines.pos.to.placeholder(txt, lines.pos,ph.df=ph.df, ph.prefix = "#~ce")

  txt = pho$str;
  
  # Do some additional normalizations  
  txt = trimws_around(txt, "=")
  txt = trimws_around(txt, ":")
  txt = gsub("#delimit", "#delimit ",txt)
  txt = trimws_around(txt, " ")
  
  pho$str = txt
  
  return(pho)
  #txt = pho$str; ph.df = pho$ph.df

}

# Takes a normalized do file pho object as argument
do.to.table = function(txt=pho$str, ph.df=pho$ph.df, pho) {
  restore.point("dopho.to.table")
  txt = merge.lines(txt)
  # Set brackets () into ph
  pho = blocks.to.placeholder(txt, start=c("("), end=c(")"), ph.prefix = "#~br")
  
  txt = pho$str
  # Remove comments from txt
  co.ph.df = ph.df %>%
    filter(startsWith(ph,"#~c")) %>%
    mutate(content = "")
  txt = replace.placeholders(txt, co.ph.df)
  
  #cat(txt)
  txt = sep.lines(txt)
  
  
  # change :\ ad :/ as this is part of file path
  
  txt =gsub(":\\","~;~\\", txt, fixed=TRUE)
  txt =gsub(":/","~;~\\", txt, fixed=TRUE)
  
  str = txt
  colon1 = str.left.of(str, ":",not.found = NA) %>% trimws()
  str = str.right.of(str, ":") 
  colon2 = str.left.of(str, ":",not.found = NA) %>% trimws()
  str = str.right.of(str, ":") 
  colon3 = str.left.of(str, ":",not.found = NA) %>% trimws()
  str = str.right.of(str, ":") %>% trimws()
  str = gsub("~;~\\",":\\", str, fixed=TRUE)
  
  # Some commands use : in a different way. Then don't store colon stuff
  no.colon = which(startsWith(txt, "merge"))
  if (length(no.colon) > 0) {
    colon1[no.colon] = colon2[no.colon] = colon3[no.colon] = NA
    str[no.colon] = txt[no.colon]
  }
  
  str = gsub(","," ,", str, fixed=TRUE)
  cmd = str.left.of(str," ")
  str = paste0(" ",str.right.of(str," "))
  opts = str.right.of(str,",",not.found=NA) %>% trimws()
  str = str.left.of(str,",")
  weight = str.between(str,"[","]", not.found=NA) %>% trimws()
  str = str.left.of(str,"[")
  using = str.right.of(str," using ",not.found=NA)  %>% trimws()
  str = str.left.of(str," using ")
  in_arg = str.right.of(str," in ",not.found=NA)  %>% trimws()
  str = str.left.of(str," in ") 
  if_arg = str.right.of(str," if ",not.found=NA)  %>% trimws()
  str = str.left.of(str," if ") 
  exp = str.right.of(str,"=",not.found=NA)  %>% trimws()
  str = str.left.of(str,"=") 
  arg_str = str
  
  tab = data.frame(cmd,arg_str, exp, if_arg, in_arg, using, opts, colon1, colon2,colon3, txt)
  tab
}