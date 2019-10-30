#' ---
#' title: "Rebuilding the Map File"
#' author:
#' - "Alex F. Bokov"
#' - "Laura S. Manuel"
#' - "Meredith N. Zozus"
#' date: "10/29/2019"
#' ---
#' 
#+ init, message=FALSE,echo=FALSE
# init -----
# set to > 0 for verbose initialization
.debug <- 0;
# additional packages to install, if needed. If none needed, should be: ''
.projpackages <- c('dplyr')
# name of this script
.currentscript <- "rebuild_eventmap.R"; 
# other scripts which need to run before this one. If none needed, shoule be: ''
.deps <- c( 'ts_dataprep.R' ); 

# load stuff ----
# load project-wide settings, including your personalized config.R
if(.debug>0) source('./scripts/global.R',chdir=T) else {
  .junk<-capture.output(source('./scripts/global.R',chdir=T,echo=F))};

#+ header, echo=F, message=FALSE
#===========================================================#
# Your code goes below, content provided only as an example #
#===========================================================#

dct0 <- unique(dat01[,c('SOURCE','EVENT','src_evt')]) %>% 
  mutate( color='gray',shape=20,size=1,alpha=1
         ,c_adm = EVENT=='AdmitDt'
         ,c_preadm = grepl('^preadmit1.admit',EVENT,ignore.case = TRUE)
         ,c_pstadm = grepl('^postadmit1.admit',EVENT,ignore.case = TRUE)
         ,c_dsc = grepl('^disch',EVENT,ignore.case=TRUE)
         ,c_predsc = grepl('^preadmit1.disch',EVENT,ignore.case = TRUE)
         ,c_pstdsc = grepl('^postadmit1.disch',EVENT,ignore.case = TRUE)
         ,c_srg = grepl('^surg',EVENT,ignore.case = TRUE)
         ,c_pre = c_predsc | c_preadm
         ,c_pst = c_pstdsc | c_psdadm
         ,c_prepst = c_pre | c_pst
         ,c_misc = pmax(c_adm,c_preadm,c_pstadm,c_dsc,c_predsc,c_pstdsc
                        ,c_srg) == 0
         );
write.csv(dct0,'eventmap.csv',row.names = FALSE);
c()