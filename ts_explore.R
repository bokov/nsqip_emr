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
.projpackages <- c('GGally','tableone','pander','dplyr','ggplot2','readr','pdc')
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
dat01 <- left_join(dat00,dat01rank);

dat01a <- subset(dat01,CASE_DEID %in% sample(dat01$CASE_DEID
                                        ,getOption('project.sample'
                                                   ,1000)));
#' Another approach: create dummy variables upon which to cluster
dat02 <- model.matrix(~src_evt-1,dat01a) %>% 
  cbind(dat01a[,c('CASE_DEID','TIME_TO_EVENT')],.) %>% 
  group_by(CASE_DEID,TIME_TO_EVENT) %>% mutate_all(as.logical) %>% 
  summarise_all(any) %>% rename(time=TIME_TO_EVENT) %>% split((.)$CASE_DEID) %>% 
  lapply(`[`,-1);

#' Now turn it into a similarity matrix
dat02sm <- matrix(0,length(dat02),length(dat02));
for(ii in seq_along(dat02)) for(jj in seq_along(dat02)){
  dat02sm[ii,jj] <- fmetric(dat02[[]])
}

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
