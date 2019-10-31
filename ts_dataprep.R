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
#' Get some aggregate columns for grouping similar timelines. All this chunk of
#' code below is not separate statements horribly formatted. It's technically 
#' one line of code, `data.table`'s equivalent of a pipeline. I'm not doing this
#' because I want to. If I did this with `dplyr` or base, it would take much 
#' longer than the `data.table` version.
#+ dat01rank
# dat01rank ----

dat01 <- as.data.table(dat00, key = 'CASE_DEID')[, `:=`(
  min = min(c(TIME_TO_EVENT, 0), na.rm = TRUE)
  ,max = max(c(TIME_TO_EVENT, 0), na.rm = TRUE)
  ,nn = .N ,adm = min(TIME_TO_EVENT[EVENT == 'AdmitDt'], na.rm = TRUE)
  ,dsc = max(TIME_TO_EVENT[EVENT == 'DischargeDt' &
                             SOURCE == 'NSQIP'], na.rm = TRUE)
  ,maxpre = max(TIME_TO_EVENT[TIME_TO_EVENT < 0 &
                                EVENT =='PreAdmit1|Discharge:INPATIENT']
                , na.rm =TRUE))
  , by = CASE_DEID][, `:=`(
    minpost = min(TIME_TO_EVENT[TIME_TO_EVENT > dsc &
                                  EVENT =='PostAdmit1|Admit:INPATIENT']
                  , na.rm =TRUE)
    ,rng = max - min), by = CASE_DEID][
      , rngprepost := diff(range(pmax(min, maxpre), pmin(max, minpost)))
      ,by =CASE_DEID][
        , `:=`(
          chk_readmovrlp = min(
            TIME_TO_EVENT[EVENT == 'PostAdmit1|Admit:INPATIENT']) < dsc
          ,chk_redscovrlp = min(
            TIME_TO_EVENT[EVENT == 'PostAdmit1|Discharge:INPATIENT']) <
            min(TIME_TO_EVENT[EVENT =='PostAdmit1|Admit:INPATIENT'])
          ,chk_dischovrlp = max(
            TIME_TO_EVENT[EVENT == 'PreAdmit1|Discharge:INPATIENT']) > 0
          ,chk_admitovrlp = max(
            TIME_TO_EVENT[EVENT == 'PreAdmit1|Admit:INPATIENT']) >
            max(TIME_TO_EVENT[EVENT =='PreAdmit1|Discharge:INPATIENT'])
          ,chk_missingdsc = is.infinite(dsc))
        ,by = CASE_DEID][, `:=`(
            src_evt = paste0(SOURCE, '|', EVENT)
            # group various source-event combos into types
            ,evt_type = recode(EVENT,AdmitDt = 'Admit'
                               ,DischargeDt = 'Discharge'
                               ,DischargeOrder = 'Discharge'
                               ,SurgStartTime = 'Surg',SurgEndTime = 'Surg'
                               ,.default = 'Misc Orders'))][
                                 , chk_total := chk_readmovrlp + 
                                     chk_redscovrlp + chk_dischovrlp + 
                                     chk_admitovrlp + chk_missingdsc
                                 ,by = CASE_DEID];
dat01$order00 <- frank(dat01,adm,min,dsc,max,rng,CASE_DEID,ties.method='dense');
dat01$order01 <- frank(dat01,adm,maxpre,dsc,minpost,rngprepost,CASE_DEID
                       ,ties.method='dense');
# [,order00 := frank(adm, min, dsc, max, rng, ties.method = 'random')
#        ,by = list(CASE_DEID)];
# 
#,order01 = frank(., adm, maxpre, dsc, minpost, rngprepost
#                 , ties.method ='random'))
#' 
#' #' Merge the aggregate columns on the full dataset
#' #+ dat01
#' # dat01 ----
#' dat01 <- as.data.table(dat00) %>% setkey(CASE_DEID) %>% `[`(dat01rank) %>%
#'   # data.table modifies values by reference, so have to pipe through copy in 
#'   # order to avoid messing up the original
#'   copy %>% 
#'   # create a combined source-event variable
#'   {(.)[,`:=`(src_evt=paste0(SOURCE,'|',EVENT)
#'              # group various source-event combos into types
#'              ,evt_type=recode(EVENT,AdmitDt='Admit',DischargeDt='Discharge'
#'                               ,DischargeOrder='Discharge',SurgStartTime='Surg'
#'                               ,SurgEndTime='Surg',.default='Misc Orders')
#'              # variable by which to order the results for ggplot, first by
#'              # non-NSQIP admission date (if different from NSQIP's)
#'              )]};
#' #' Validity tests for dat00 and dat01
stopifnot(identical(names(dat00)
                    ,c('CASE_DEID','TIME_TO_EVENT','SOURCE','EVENT')));
stopifnot(with(dat01,length(unique(CASE_DEID))==length(unique(order00))));
stopifnot(with(dat01,length(unique(CASE_DEID))==length(unique(order01))));

#' Keep only the events surrounding the NSQIP index stay that happen after the
#' previous discharge and before the next readmit
#+ dat02
# dat02 ----
dat02 <- dat01[TIME_TO_EVENT >= maxpre & TIME_TO_EVENT <= minpost,][
  ,order00:=order01];
stopifnot(with(dat02,length(unique(CASE_DEID))==length(unique(order00))));
stopifnot(with(dat02,all(order00==order01)));


#' Take a random sample 
#+ dat01a
# dat01a ----
dat01a <- dat01[CASE_DEID %in% sample(dat01$CASE_DEID
                                      ,getOption('project.sample',1000))
                ,][# collapse the gaps in the original ranking. Yes, clumsy,
                   # didn't say I was proud of this...
                   ,`:=`(order00 = as.numeric(ordered(order00))
                         ,order01 = as.numeric(ordered(order01)))];
stopifnot(with(dat01a
               ,length(union(seq_along(unique(CASE_DEID))
                             ,unique(order00)))==length(unique(CASE_DEID))));
stopifnot(with(dat01a
               ,length(union(seq_along(unique(CASE_DEID))
                             ,unique(order01)))==length(unique(CASE_DEID))));

# Keep only the events surrounding the NSQIP index stay that happen after the
# previous discharge and before the next readmit, for the random sample
#+ dat02a
# dat02a ----
dat02a <- dat01a[TIME_TO_EVENT >= maxpre & TIME_TO_EVENT <= minpost,][
  ,order00:=order01];
stopifnot(with(dat02a
               ,length(union(seq_along(unique(CASE_DEID))
                             ,unique(order00)))==length(unique(CASE_DEID))));
stopifnot(with(dat02a,all(order00==order01)));

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
