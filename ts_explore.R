#' ---
#' title: "Visualizing Patient Events"
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
.projpackages <- c('GGally','tableone','pander','dplyr','ggplot2','readr'
                   ,'tidyr','dtwclust','data.table');
# name of this script
.currentscript <- "ts_explore.R"; 
# other scripts which need to run before this one. If none needed, shoule be: ''
.deps <- c( 'dictionary.R' ); 

# load stuff ----
# load project-wide settings, including your personalized config.R
if(.debug>0) source('./scripts/global.R',chdir=T) else {
  .junk<-capture.output(source('./scripts/global.R',chdir=T,echo=F))};

#+ startcode, echo=F, message=FALSE
#===========================================================#
# Your code goes below, content provided only as an example #
#===========================================================#

#' Get some aggregate columns for grouping similar timelines
dat01rank <- group_by(dat00,CASE_DEID) %>% 
  summarize(min=min(c(TIME_TO_EVENT,0),na.rm=TRUE)
            ,max=max(c(TIME_TO_EVENT,0),na.rm = TRUE),rng=max-min) %>% 
  arrange(rng) %>% mutate(rmin=seq_len(n()));

dat01 <- left_join(dat00,dat01rank) %>% 
  mutate(src_evt = paste0(SOURCE,'|',EVENT));

dat01a <- subset(dat01,CASE_DEID %in% sample(dat01$CASE_DEID
                                        ,getOption('project.sample'
                                                   ,1000)));
#' All the distinct days:
# scaffold <- data.frame(day=seq(min(dat01$TIME_TO_EVENT,na.rm=TRUE)
#                                ,max(dat01$TIME_TO_EVENT,na.rm=TRUE)));
.start <- Sys.time();
scaffold <- expand.grid(TIME_TO_EVENT=seq(min(dat01$TIME_TO_EVENT,na.rm=TRUE)
                                ,max(dat01$TIME_TO_EVENT,na.rm=TRUE))
                        ,CASE_DEID=unique(dat01$CASE_DEID)
                        ,stringsAsFactors = F) %>% as.data.table %>%
  setkey(CASE_DEID,TIME_TO_EVENT) %>%
  with_attrs(list(runtime=Sys.time()-.start));

#' Another approach: create dummy variables upon which to cluster
#+ dat02, message=FALSE

# split the various source/event combos into separate columns
.start <- Sys.time();
dat02 <- model.matrix(~src_evt-1,dat01) %>% `==`(0) %>%
  cbind(dat01[,c('CASE_DEID','TIME_TO_EVENT')],.) %>% 
  # use data.table because way too many rows for dplyr to run fast
  mutate(TIME_TO_EVENT=round(TIME_TO_EVENT)) %>% as.data.table %>% 
  setkey(CASE_DEID,TIME_TO_EVENT) %>% 
  # squeeze down into one row per case-time
  `[`(TRUE,lapply(.SD,any,na.rm=TRUE),by=list(CASE_DEID,TIME_TO_EVENT)) %>%
  # expand so every combination of case and time is represented 
  # (various analysis functions will need each to have the same number of 
  # observations, and aligned in time)
  `[`(scaffold) %>%
  with_attrs(list(runtime=Sys.time()-.start));

# replace NAs created by the join with FALSE
dat02[is.na(dat02)] <- FALSE;

# split out into numeric matrices
dat03 <- split(select(dat02,-CASE_DEID,-TIME_TO_EVENT),dat02$CASE_DEID) %>% 
  lapply(`+`,0);

#+ multivariate_cluster
# first strip out the day column and convert back to numeric
.start <- Sys.time()
dat03mvc <- tsclust(dat03[1:100],k=4L, distance = "gak", seed = 390,
                    args = tsclust_args(dist = list(sigma = 100))) %>% 
  with_attrs(dat03mvc,list(runtime=Sys.time()-.start));
#' Now turn it into a distance matrix and make a hierarchical cluster
.start <- Sys.time();
dat03hc <- hclust(dist(dat03mvc@distmat)) %>% 
  with_attrs(list(runtime=Sys.time()-.start));

#+ echo=FALSE,message=FALSE
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
