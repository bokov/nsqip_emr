#' ---
#' title: "Generic R Project Configuration File"
#' author: "Alex F. Bokov, Ph.D."
#' date: "10/18/2018"
#' ---
#' 
#' Please copy this file to `config.R`, edit that copy, and copy it over to
#' the working directory whenever you check out this project. This is just an
#' example of what computer-specific variables should be set. A file needs to be 
#' called `config.R` in order to be used as a source of configuration 
#' information by our scripts
#' 
inputdata <- 'data/example.csv';
project_seed <- 20191022;
#' 
#' Do you know what character is used to delimit your files? Usually it's either
#' comma or tab, but it could be anything. If you're not sure, leave this 
#' commented out, and the scripts will try to guess it for you.
#file_delim <- '\t';
#' Your data dictionary file (if/when you have one, uncomment this line)
#dctfile_raw <- 'WHERE_I_KEEP_MY_DATA/MY_DATA_DICTIONARY.csv';

#+ echo=F,eval=F
# Do not edit below this line
c()
