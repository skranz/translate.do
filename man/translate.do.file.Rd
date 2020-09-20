\name{translate.do.file}
\alias{translate.do.file}
\title{Translate a do file into an R file}
\usage{
  translate.do.file(do.file = NULL,
    r.file = sc(do.file, ".r"))
}
\arguments{
  \item{do.file}{The name of the do.file}

  \item{r.file}{The name of the R file}
}
\description{
  Translate a do file into an R file
}

\examples{\dontrun{
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
}}
