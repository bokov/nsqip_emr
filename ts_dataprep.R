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
#' 
#' ### Inactive Code
#' 
#' The code from here on down is temporarily kept here in commented-out form 
#' for convenience, in case I need to go back to using any of it. It will be
#' deleted in a future clean-up commit.
#' ***
#' 
#' All the distinct days:
# .start <- Sys.time();
# scaffold <- expand.grid(TIME_TO_EVENT=seq(min(dat01$TIME_TO_EVENT,na.rm=TRUE)
#                                 ,max(dat01$TIME_TO_EVENT,na.rm=TRUE))
#                         ,CASE_DEID=unique(dat01$CASE_DEID)
#                         ,stringsAsFactors = F) %>% as.data.table %>%
#   setkey(CASE_DEID,TIME_TO_EVENT) %>%
#   with_attrs(list(runtime=Sys.time()-.start));
# 
#' Another approach: create dummy variables upon which to cluster
#+ dat02, message=FALSE,cache=TRUE
# split the various source/event combos into separate columns
# .start <- Sys.time();
# dat02 <- model.matrix(~src_evt-1,dat01) %>% `!=`(0) %>%
#   cbind(dat01[,c('CASE_DEID','TIME_TO_EVENT')],.) %>% 
#   # use data.table because way too many rows for dplyr to run fast
#   mutate(TIME_TO_EVENT=round(TIME_TO_EVENT)) %>% as.data.table %>% 
#   setkey(CASE_DEID,TIME_TO_EVENT) %>% 
#   # squeeze down into one row per case-time
#   `[`(TRUE,lapply(.SD,any,na.rm=TRUE),by=list(CASE_DEID,TIME_TO_EVENT)) %>%
#   # expand so every combination of case and time is represented 
#   # (various analysis functions will need each to have the same number of 
#   # observations, and aligned in time)
#   `[`(scaffold) %>%
#   with_attrs(list(runtime=Sys.time()-.start));

# replace NAs created by the join with FALSE
# dat02[is.na(dat02)] <- FALSE;

# split out into numeric matrices
# dat03 <- split(select(dat02,-CASE_DEID,-TIME_TO_EVENT),dat02$CASE_DEID) %>% 
#   lapply(`+`,0);
#' 
#' 
# overall characteristics of event distributions
#dat04 <- Reduce(function(xx,yy) xx+yy,dat03);
# dat04 <- list();
# for(ii in names(dat03)) dat04[[ii]] <-  apply(dat03[[ii]][42:192,],1,paste0
#                                               ,collapse='.');
# dat04 <- do.call(rbind,dat04) %>% as.data.frame;
# dat04dst <- daisy(dat04); # < 2 min
# dat04hc <- hclust(dat04dst); 
# caseorder <- with(dat04hc,data.table(CASE_DEID=labels,order=order)) %>% 
#   setkey(CASE_DEID);
# supposedly 0.6 seconds
#' multivariate_cluster
# first strip out the day column and convert back to numeric
# runtimes: 100 = 15.02695s; 200 = 55.73224 s; 400 = 290.2607s; 800 = 1511.716s
# ...or 25 min
# .start <- Sys.time()
# dat03mvc <- tsclust(dat03a[1:400],k=4L, distance = "gak", seed = 390,
#                     args = tsclust_args(dist = list(sigma = 100))) %>% 
#   with_attrs(list(runtime=Sys.time()-.start));
# # Now turn it into a distance matrix and make a hierarchical cluster
# .start <- Sys.time();
# dat03hc <- hclust(dist(dat03mvc@distmat)) %>% 
#   with_attrs(list(runtime=Sys.time()-.start));

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
