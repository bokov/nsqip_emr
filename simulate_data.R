#' ---
#' title: "Generating Random Sample Data"
#' author:
#' - "Alex F. Bokov"
#' - "Laura S. Manuel"
#' - "Meredith N. Zozus"
#' date: "10/22/2019"
#' ---
#' 
#+ init, message=FALSE,echo=FALSE
# init -----
# set to > 0 for verbose initialization
.debug <- 0;
# additional packages to install, if needed. If none needed, should be: ''
.projpackages <- c('dplyr','readr')
# name of this script
.currentscript <- "simulate_data.R"; 
# other scripts which need to run before this one. If none needed, shoule be: ''
.deps <- c( 'dictionary.R' ); 

# load stuff ----
# load project-wide settings, including your personalized config.R
if(.debug>0) source('./scripts/global.R',chdir=T) else {
  .junk<-capture.output(source('./scripts/global.R',chdir=T,echo=F))};

#+ header, echo=F, message=FALSE
#===========================================================#
# Your code goes below, content provided only as an example #
#===========================================================#

#' This script creates a redistributable simulated (not merely deidentified or
#' obfuscated) dataset with the same format and some similar statistical 
#' properties to the original dataset. Though it does not leak the original 
#' data, it needs access to the original data in order to extract the 
#' distributions from it.
#' 
#+ dosims, echo=F, message=FALSE
# create TIME_TO_EVENT with realistic distributions
simulated <- group_by(dat00,SOURCE,EVENT) %>% 
  mutate(CASE_DEID=sample(CASE_DEID,n(),rep=FALSE)
         ,TIME_TO_EVENT=quantile(TIME_TO_EVENT,runif(n()),na.rm = TRUE)) %>%
  mutate(TIME_TO_EVENT=if(unique(EVENT) %in% c('SurgEndTime','SurgStartTime')) {
    TIME_TO_EVENT} else round(TIME_TO_EVENT));

# scramble the already deidentified and randomly permuted CASE_DEIDs
simulated$CASE_DEID <- unique(simulated$CASE_DEID) %>% length %>% runif %>%
  `*`(1e18) %>% paste0('SU_',.) %>% factor(simulated$CASE_DEID,labels=.) %>%
  as.character;

# take a random sample
simulated_sample <- subset(simulated
                           ,CASE_DEID %in% sample(simulated$CASE_DEID
                                                  ,getOption('project.sample'
                                                             ,200)));
# save
write_csv(simulated_sample,'data/example.csv');


#+ echo=FALSE,warning=FALSE,message=FALSE
#===========================================================#
##### End of your code, start of boilerplate code ###########
#===========================================================#
knitr::opts_chunk$set(echo = FALSE,warning = FALSE,message=FALSE);

# save out with audit trail 
message('About to tsave');
tsave(file=paste0(.currentscript,'.rdata'),list=setdiff(ls(),.origfiles)
      ,verbose=FALSE);
message('Done tsaving');

#' ### Audit Trail
#+ echo=FALSE,results='hide'
.wt <- walktrail();
c()
