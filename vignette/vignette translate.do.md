# Vignete translate.do

## ** Preliminary Version: Don't distribute the package or this file without permission **

Version: 22.04.2014

Author: Sebastian Kranz (sebastian.kranz@uni-ulm.de)

Affiliation: Ulm University


## Motivation

The package is currently a research project that contains functions that help to translate .do files, which contain commands of the Stata language, to R. Especially in economics, .do files are very wide spread in empirical analysis. While Stata is a great product, it seems helpful for the advancement of open and reproducible research to be quickly able to translate .do files to R in order to reproduce an empirical analysis in an open source environment.

Unlike Octave, which fully reimplements the Matlab language, the package just helps to translate .do files, resulting in files with valid R syntax. While much translation should work automatically, there will still often be the need to adapt the resulting R file manually. Also, there will still be many features that are not yet implemented.

## Installation

Before installing translate.do from a local tar.gz file, you should run the following commands to install currently required packages:

```r

# Install packages from CRAN
install.packages("devtools")
install.packages("pryr")
install.packages("dplyr")
install.packages("data.table")
install.packages("lubridate")

# Install packages from github
library(devtools)
install_github(repo = "restorepoint", username = "skranz")
install_github(repo = "stringtools", username = "skranz")
install_github(repo = "sktools", username = "skranz")


# Now install translate.do from local .tar.gz file
```


## Usage

After you have installed all required packages, basic usage is simple. Assume you want to translate a file "mydo.do" in a folder "C:\mydofiles". You simply have to run:


```r
library(translate.do)
setwd("C:\mydofiles") # set to the directory with your do file
translate.do.file("mydo.do") # change filename
```


The function generates a file "mydo.do.r" that contains a translated version of the do file. Consider the following excerpt from a .do file:

```{}
clear
use table2_1
sort year month type_fund
merge year month type_fund using table2_2
```

This .do file code will be automatically translated to the following R code:

```r
.clear()
.use("table2_1")
.sort(year, month, type_fund)
.merge(year, month, type_fund, .using("table2_2"))
```


The functions .clear, .use, ... are implemented in the package translate.do and try to implement in R the intendet functionality of these commands. Currently, the scope of the package is quite limited:
  
  - only a small fraction of commands and options is implemented
  - some code will not yet be translated in valid R code

Nevertheless, frequent commands are already implemented and the package is structured to be easily extended.

## Internal structure and extending the package

Here are some points describing how the package works.

  - translate.do.file(...) generates an .r file with a valid R syntax that tries to look similar to the orginal .do file syntax.
  - The generated R commands have syntax like

```r
.merge(year, month, type_fund, .using("table2_2"))
```

  - A . is added to the original function name (to avoid confusion with similar named R functions), and options are added as a parameter starting also with a .
  - .merge is defined in the package translate.do in the file "dot_commands.r" and looks as follows:

```r
.merge = function(...) {
    par = rs.par(..., .subst.env = parent.frame())
    rs.merge(par)
}
```

  - The call to rs.par takes all arguments of .merge and yields a list "par"" that structures the arguments of .merge. Best is to take a look yourself at the structure of par (for example using restore.point debugging). 
  - The main work is done in the function rs.merge, which is defined in the file "rs_commands.r". It tries to implement the original functionality into R using the par object as a description of the function arguments and options. The function starts as following

```r

rs.merge = function(par, check.options = TRUE, ...) {
    restore.point("rs.merge")
    if (check.options) 
        known.options(par, "using")
    
    
    # ... ommited code
}
```

  
  - 
    - The call to restore.point is simply used for debugging purposes. (See the vignete of the package restore.point)
    - The call to known.options just specifies which options are already implemented by rs.merge. Here it is only the option "using". If the function is called with not-implemented options, a warning is automatically shown.
    - The remaining ommited code tries to implement the desired behavior in R.
  - I guess it is best to take a look at some rs. functions to get a feeling of how they work. 
  - While R typically stores data in user generated data frames, Stata has a single global data frame for the data set currently analysed. The package translate.do replicates this behavior by generating a data table named "rs.dt" in the global environment on which the different commands like .merge operate. (We use a data table instead of a data frame for speed purposes.)
  - The file dotdot_functions.r,  contains implementations of .do file functions substr, which are renamed ..substr and implement the desired transformation of their arguments in R.

### Extending the package
  - Main extensions would be to implement more commands and options. The main code would be added to "rs_commands.r". In the file "dot_commands.r", one just needs to generate a interface with the same structure than all the other interfaces in the function.
  - Similarly, there are also still a lot of function in "dotdot_functions.r" missing.
  - There are also some bugs and missing features in the direct translation, e.g. forach loops are not yet correctly converted.
  - Handling of several special variables, like "merge_" is not yet implemented.

## Disclaimer


Currently this package is a research project, studying how to facilitate conversion of Stata(TM)* .do files to R.  The package uses commands that are inspired by the syntax of .do file commands. At my university, I own a licensed copy of Stata and looked the documentation for .do file syntax. Yet, the package has been written without looking at source code of Stata.

Don't use this package if in your jurisdiction the syntax of a programming language, in particular the syntax of .do file commands, can be protected by copyright and your usage of this package would constitute an infringement of intellectual property of StataCorp. I will not take liability for any misuse of this package.

To the best of my knowledge, programming languages are NOT protected by copyright in the EU and the US. See for example:

http://www.out-law.com/en/articles/2013/january/computer-programming-languages-should-not-be-viewed-as-copyrightable-says-high-court-judge/

http://en.wikipedia.org/wiki/Clone_%28computing%29

*Stata is developed and trademarked by StatCorp

  
