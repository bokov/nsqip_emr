#' ---
#' title: "Generic R Project Configuration File"
#' author: "Alex F. Bokov, Ph.D."
#' date: "10/18/2018"
#' ---
#' This is just an example of what computer-specific variables should be set. 
#' Modify this file according to the needs of your project. For variables that
#' will vary from one computer to another, instruct your 
#' contributors/collaborators to create their own file called `config.local.R`
#' in the same directory as this one, and _not_ check that file into the repo.
#' 
#' This points to an example dataset for testing purposes. The real dataset 
#' should reside on individual collaborators' machines and the `inputdata`
#' variable set in their individual `config.local.R` will be used in preference 
#' to the path below.
inputdata <- 'data/example.csv';

#' For reproducible yet approximately random number generation (e.g. sampling)
project_seed <- 20191022;

#' If you want to override which strings will be treated as missing data, change
#' the variable below.
na.strings <- c('NA','','-','(null)');
#' Do you know what character is used to delimit your files? Usually it's either
#' comma or tab, but it could be anything. If you're not sure, leave this 
#' commented out, and the scripts will try to guess it for you.
#' (this setting not yet fully implemented)
#file_delim <- '\t';

#' Your data dictionary file (if/when you have one, uncomment this line)
#dctfile_raw <- 'WHERE_I_KEEP_MY_DATA/MY_DATA_DICTIONARY.csv';

#+ echo=F,eval=F
# Do not edit below this line
c()
