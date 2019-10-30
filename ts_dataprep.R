#' ---
#' title: "Visualizing Patient Events"
#' subtitle: "Preparing Data for Analysis by the Other Scripts"
#' author:
#' - "Alex F. Bokov^[UT Health San Antonio]"
#' - "Laura S. Manuel^[UT Health San Antonio]"
#' - "Meredith N. Zozus^[UT Health San Antonio]"
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' abstract: |
#'   The raw data has columns `CASE_DEID`, `TIME_TO_EVENT`, `SOURCE`, and
#'   `EVENT`. This script utilizes the reusable 
#'   [UT-Template framework](https://github.com/bokov/UT-Template) in `scripts`
#'   (initialized in the code preceding the line that says `Your code goes 
#'   below`) to read in the data. It then adds some derived variables and saves
#'   the data out. Normally there is no need to run or this script directly
#'   because it automatically gets invoked when needed. Presumably you compiled
#'   this report in order to understand how the data is processed from the raw
#'   file to `dat01` and `dat01a` (a random sample of `dat01`).
#' css: production.css
#' ---
#' 
#+ init, message=FALSE,echo=FALSE
# init -----
# set to > 0 for verbose initialization
.debug <- 1;
knitr::opts_chunk$set(echo = .debug>0,warning = .debug>1,message=.debug>2);
# additional packages to install, if needed. If none needed, should be: ''
.projpackages <- c('data.table','dplyr'); 
# name of this script
.currentscript <- "ts_dataprep.R"; 
# other scripts which need to run before this one. If none needed, shoule be: ''
.deps <- c( 'dictionary.R' ); 

# load stuff ----
# load project-wide settings, including your personalized config.R
if(.debug>1) source('./scripts/global.R',chdir=T) else {
  .junk<-capture.output(source('./scripts/global.R',chdir=T,echo=F))};

#+ startcode, echo=F, message=FALSE
#===========================================================#
# Your code goes below, content provided only as an example #
#===========================================================#

#' ### Preparing the Data
#' Get some aggregate columns for grouping similar timelines
dat01rank <- group_by(dat00,CASE_DEID) %>% 
  summarize(min=min(c(TIME_TO_EVENT,0),na.rm=TRUE)
            ,max=max(c(TIME_TO_EVENT,0),na.rm = TRUE)
            ,rng=max-min
            ,nn=n()
            ,adm=min(TIME_TO_EVENT[EVENT=='AdmitDt'])
            ,dsc=max(TIME_TO_EVENT[EVENT=='DischargeDt'&SOURCE=='NSQIP'])) %>% 
  # rmin amounts to a unique integer id for each case, helpful for tie breaking
  arrange(rng) %>% mutate(rmin=seq_len(n())) %>% as.data.table %>% 
  setkey(CASE_DEID);

#' Merge the aggregate columns on the full dataset
# dat01 ----
dat01 <- as.data.table(dat00) %>% setkey(CASE_DEID) %>% `[`(dat01rank) %>%
  # create a combined source-event variable
  {(.)[,`:=`(src_evt=paste0(SOURCE,'|',EVENT)
             # group various source-event combos into types
             ,evt_type=recode(EVENT,AdmitDt='Admit',DischargeDt='Discharge'
                              ,DischargeOrder='Discharge',SurgStartTime='Surg'
                              ,SurgEndTime='Surg',.default='Misc Orders')
             # variable by which to order the results for ggplot, first by
             # non-NSQIP admission date (if different from NSQIP's)
             ,order00=rank(rank(adm)*1e8 +
                             # then by earliest event preceding NSQIP admission
                             # date, if there are any such events
                             rank(min)*1e4 +
                             # then by NSQIP discharge date
                             rank(dsc) +
                             # then by the last recorded event of any type
                             rank(max)*1e-4))]}

# take a random sample 
dat01a <- subset(dat01,CASE_DEID %in% sample(dat01$CASE_DEID
                                        ,getOption('project.sample',1000))) %>%
  {(.)[,order01 := rank(order00)]};

if(file.exists('eventmap.csv')){
  dct0 <- try_import('eventmap.csv');
  .origfiles <- setdiff(.origfiles,'dct0');
  } else {
  stop("

You are missing the file 'eventmap.csv', which is needed for labeling plots.
You can generate a default one with the following command:

source('rebuild_eventmap.R')

       ");
}
#' 
#+ echo=FALSE,message=FALSE
#===========================================================#
##### End of your code, start of boilerplate code ###########
#===========================================================#
knitr::opts_chunk$set(echo = FALSE,warning = .debug>1,message=FALSE);

# save out with audit trail 
message('About to tsave');
tsave(file=paste0(.currentscript,'.rdata'),list=setdiff(ls(),.origfiles)
      ,verbose=FALSE);
message('Done tsaving');

#' ### Audit Trail
#+ echo=FALSE,results='hide'
.wt <- walktrail();
c()
