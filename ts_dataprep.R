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
#+ dat01
# dat01 ----

dat01 <- as.data.table(dat00, key = 'CASE_DEID')[, `:=`(
  # some global and case-level constants that will be needed by subsequent 
  # transformations
   min = min(c(TIME_TO_EVENT, 0), na.rm = TRUE)
  ,max = max(c(TIME_TO_EVENT, 0), na.rm = TRUE)
  ,nn = .N ,adm = min(TIME_TO_EVENT[EVENT == 'AdmitDt'], na.rm = TRUE)
  ,dsc = max(TIME_TO_EVENT[EVENT == 'DischargeDt' &
                             SOURCE == 'NSQIP'], na.rm = TRUE)
  ,maxpre = max(TIME_TO_EVENT[TIME_TO_EVENT < 0 &
                                EVENT =='PreAdmit1|Discharge:INPATIENT']
                , na.rm =TRUE)
  ,src_evt = paste0(SOURCE, '|', EVENT)
  ,evt_type = recode(EVENT,AdmitDt = 'Admit'
                     ,DischargeDt = 'Discharge'
                     ,DischargeOrder = 'Discharge'
                     ,SurgStartTime = 'Surg',SurgEndTime = 'Surg'
                     ,.default = 'Misc Orders')
  ), by = CASE_DEID][, `:=`(
     minpost = min(TIME_TO_EVENT[TIME_TO_EVENT > dsc &
                                  EVENT =='PostAdmit1|Admit:INPATIENT']
                  , na.rm =TRUE)
    ,rng = max - min), by = CASE_DEID][
      , rngprepost := diff(range(pmax(min, maxpre), pmin(max, minpost)))
      ,by =CASE_DEID][, `:=`(
        chk_readmovrlp = with_cm(min(
          TIME_TO_EVENT[EVENT == 'PostAdmit1|Admit:INPATIENT']) <= dsc
          ,'Post-NSQIP inpatient admits that happen on or before NSQIP discharge')
        ,chk_readscovrlp = with_cm(min(
          TIME_TO_EVENT[EVENT == 'PostAdmit1|Discharge:INPATIENT']) <
            min(TIME_TO_EVENT[EVENT =='PostAdmit1|Admit:INPATIENT'])
          ,'Post-NSQIP inpatient discharges that happen before their own admit dates')
        ,chk_dischovrlp = with_cm(max(
          TIME_TO_EVENT[EVENT == 'PreAdmit1|Discharge:INPATIENT']) >= 0
          ,'Pre-NSQIP inpatient discharges that happen on or after NSQIP admit')
        ,chk_admitovrlp = with_cm(max(
          TIME_TO_EVENT[EVENT == 'PreAdmit1|Admit:INPATIENT']) >
            max(TIME_TO_EVENT[EVENT =='PreAdmit1|Discharge:INPATIENT'])
          ,'Pre-NSQIP inpatient admits that happen after their own discharge dates')
        ,chk_missingdsc = with_cm(is.infinite(dsc)
                                  ,'Missing NSQIP discharge date')
        ,chk_noadm = with_cm(sum(src_evt=='CV3ClientVisit|AdmitDt')==0
                            ,'Non-unique EMR index admit date (or none?)')
        ,chk_nodsc = with_cm(sum(src_evt=='CV3ClientVisit|DischargeDt')==0
                            ,'Non-unique EMR index discharge date (or none?)')
        ,chk_multadm = with_cm(sum(src_evt=='CV3ClientVisit|AdmitDt')>1
                             ,'Non-unique EMR index admit date (or none?)')
        ,chk_multdsc = with_cm(sum(src_evt=='CV3ClientVisit|DischargeDt')>1
                             ,'Non-unique EMR index discharge date (or none?)')
        ,chk_admearly = with_cm(adm<0,'EMR admit date before NSQIP admit')
        ,chk_admlate = with_cm(adm>0,'EMR admit date after NSQIP admit')
        ,chk_sumpredsc = with_cm(
          sum(EVENT == 'PreAdmit1|Discharge:INPATIENT') >1
          ,'More than one pre-NSQIP inpatient discharge')
        ,chk_sumpreadm = with_cm(
          sum(EVENT == 'PreAdmit1|Admit:INPATIENT') >1
          ,'More than one pre-NSQIP inpatient admit')
        ,chk_sumpstdsc = with_cm(
          sum(EVENT == 'PostAdmit1|Discharge:INPATIENT') >1
          ,'More than one post-NSQIP inpatient discharge')
        ,chk_sumpstadm = with_cm(
          sum(EVENT == 'PostAdmit1|Admit:INPATIENT') >1
          ,'More than one post-NSQIP inpatient admit')
        ,chk_dsclate = with_cm(
          any(TIME_TO_EVENT[src_evt=='CV3ClientVisit|DischargeDt']>dsc)
          ,'EMR index discharge after NSQIP discharge')
        ,chk_dscearly = with_cm(
          any(TIME_TO_EVENT[src_evt=='CV3ClientVisit|DischargeDt']>dsc)
          ,'EMR index discharge before NSQIP discharge')
        ),by = CASE_DEID];
#' Column names of all the data checks
chk_cols <- grep('^chk_',names(dat01),val=T);
#' Total number of things wrong for each case.
dat01[,CHK_total := with_cm(rowSums(.SD)>0
                            ,'Cases with one or more of the above issues')
      ,.SDcols = chk_cols];
#' Sorting columns
dat01$order00 <- frank(dat01,adm,min,dsc,max,rng,CASE_DEID,ties.method='dense');
dat01$order01 <- frank(dat01,adm,maxpre,dsc,minpost,rngprepost,CASE_DEID
                       ,ties.method='dense');
dat01[,order.active := order00];

dat01qc <- unique(dat01[,c('CASE_DEID',chk_cols,'CHK_total')
                        ,with=FALSE])[,-1] %>% colSums;

#' Validity tests for dat00 and dat01
#' 
#' Make sure we didn't screw up the raw data with `data.table`'s 
#' pass-by-reference behavior.
stopifnot(identical(names(dat00)
                    ,c('CASE_DEID','TIME_TO_EVENT','SOURCE','EVENT')));
#' There must be a one-to-one relationship between CASE_DEID and order
#' And there must be a one-to-one relationship between CASE_DEID
#' and order
stopifnot(all(one2one(dat01,alist(CASE_DEID=order00,CASE_DEID=order01))));

#' Keep only the events surrounding the NSQIP index stay that happen after the
#' previous discharge and before the next readmit
#+ dat02
# dat02 ----
dat02 <- dat01[TIME_TO_EVENT >= maxpre & TIME_TO_EVENT <= minpost,][
  ,order.active := order01];

dat02qc <- unique(dat02[,c('CASE_DEID',chk_cols,'CHK_total')
                        ,with=FALSE])[,-1] %>% colSums;
#' Test `dat02` just as we did `dat01`
stopifnot(all(one2one(dat02,alist(CASE_DEID=order00,CASE_DEID=order01))));


#' Take a random sample 
#+ dat01a
# dat01a ----
dat01a <- dat01[CASE_DEID %in% sample(dat01$CASE_DEID
                                      ,getOption('project.sample',1000))
                ,][# collapse the gaps in the original ranking. Yes, clumsy,
                   # didn't say I was proud of this...
                   ,`:=`(order00 = as.numeric(ordered(order00))
                         ,order01 = as.numeric(ordered(order01)))][
                           ,order.active := order00];

#' In all the ordering variables, the unique values must be gap-less
#' integer sequences equal in length to the number of unique cases
stopifnot(with(dat01a,all.equal(seq_along(unique(CASE_DEID))
                                ,sort(unique(order00)))));
stopifnot(with(dat01a,all.equal(seq_along(unique(CASE_DEID))
                                ,sort(unique(order01)))));
#' And there must be a one-to-one relationship between CASE_DEID
#' and order
stopifnot(all(one2one(dat01a,alist(CASE_DEID=order00,CASE_DEID=order01))));


# Keep only the events surrounding the NSQIP index stay that happen after the
# previous discharge and before the next readmit, for the random sample
#+ dat02a
# dat02a ----
dat02a <- dat01a[TIME_TO_EVENT >= maxpre & TIME_TO_EVENT <= minpost,][
  ,order.active:=order01];
#' In all the ordering variables, the unique values must be gap-less
#' integer sequences equal in length to the number of unique cases
stopifnot(with(dat02a,all.equal(seq_along(unique(CASE_DEID))
                                ,sort(unique(order00)))));
stopifnot(with(dat02a,all.equal(seq_along(unique(CASE_DEID))
                                ,sort(unique(order01)))));
#' Insure one-to-one relationship as above
stopifnot(all(one2one(dat01,alist(CASE_DEID=order00,CASE_DEID=order01))));

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
