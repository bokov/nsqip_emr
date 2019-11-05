#' ---
#' title: "Visualizing Patient Events"
#' subtitle: "Comparing Dates Across NSQIP and the EMR"
#' author: "The CIRD Team"
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' fig_caption: yes
#' linkReferences: true
#' nameInLink: true
#' tblLabels: "roman"
#' tblPrefix: ["table","tables"]
#' output:
#'  html_document:
#'   keep_md: true
#'   pandoc_args: ["--filter", "pandoc-crossref"]
#'  word_document:
#'   reference_docx: 'nt_styletemplate.docx'
#'   keep_md: true
#'   pandoc_args: ["--filter", "pandoc-crossref", "publication.yaml"]
#'  pdf_document:
#'   keep_md: true
#'   pandoc_args: ["--filter", "pandoc-crossref"]
#' ---
#' 
#+ init, message=FALSE,echo=FALSE
# init -----
# set to > 0 for verbose initialization
.debug <- 0;
knitr::opts_chunk$set(echo = .debug>0,warning = .debug>1,message=.debug>2);
# additional packages to install, if needed. If none needed, should be: ''
.projpackages <- c('GGally','tableone','pander','dplyr','ggplot2'
                   ,'tidyr','data.table','scales');
# name of this script
.currentscript <- "ts_explore.R"; 
# other scripts which need to run before this one. If none needed, shoule be: ''
.deps <- c( 'ts_dataprep.R' ); 

# load stuff ----
# load project-wide settings, including your personalized config.R
if(.debug>1) source('./scripts/global.R',chdir=T) else {
  .junk<-capture.output(source('./scripts/global.R',chdir=T,echo=F))};

#+ startcode, echo=F, message=FALSE
#===========================================================#
# Your code goes below, content provided only as an example #
#===========================================================#
#+ simwarning,eval=basename(inputdata)=='example.csv',results='asis'
cat("

**WARNING: This instance of the report was generated using the default
synthetic dataset `example.csv`. It has the
correct column names in the correct format, and some statistical similarities
to the real data, but it is for demonstration and testing only. Do not rely
on this report to make decisions. If you have access to
the real data, create a  file named `config.local.R` and set the `inputdata`
variable to a path leading to the file containing real input data. Then this 
script _will_ generate a real report.**

...and before you rebuild this report using different `inputdata`, first clear
the caches like this: 

`unlink(c('*_cache','*_files'),recursive=T)`

***

");
#' # Discrepancies
#' 
#' The following is a table counting the number of cases where various issues
#' have been observed.
#' 
#+ dat01qc, results='asis', warning=.debug>1
# dat01qc ----
dat01qc %>% 
  {data.frame(Issue=unlist(comments(dat01[,names(.),with=F])),`N Cases`=.)} %>% 
  pander(justify='right');
#' 
#' # Event Distributions
#' 
#' The admission date of the index hospital stay reported in NSQIP was set to 0,
#' for each patient, and the times of all the other events are expressed in 
#' days before (negative values) and after (positive values) that admission 
#' date.
#' 
#' How are the various types of events distributed in time? If there are certain
#' ranges when few events occur, we could trim those off so the analysis can 
#' run faster.
#' 
#' 
#' ::::: {#fig:eventdist00 custom-style="Image Caption"}
#+ eventdist00, cache=TRUE, results='asis', warning=.debug>1, fig.width=10, fig.height=10
# eventdist00 ----
ggplot(na.omit(dat01)
       ,aes(x=round(TIME_TO_EVENT),fill=src_evt)) + 
  geom_bar(position=position_dodge()) + 
  scale_y_continuous(oob=squish,limits=c(0,125000)) + 
  xlim(-20,80) + labs(x='Days After Admission') + 
  with(dct0,scale_fill_manual(limits=src_evt,values = color)) +
  guides(fill=guide_legend(title="SOURCE|EVENT"
                           ,ncol = 2)) + 
  theme(legend.position = 'bottom',text=element_text(family="Times New Roman"));
cat('

Distribution of Events Relative to NSQIP Admission Date. Based on this we can 
trim off events past as early as 80 days-- most of the action seems to be within
that window. Please note that the bars are staggered so they can be visually 
distinguished, therefore a cluster of bars with no gaps separating them 
represent the same day. Bars only represent different days if there are gaps
between them.
    ');
#' :::::
#' 
#' ***
#' ###### blank
#' 
#' ::::: {#fig:eventdist01 custom-style="Image Caption"}
#+ eventdist01, cache=TRUE, results='asis', warning=.debug>1, fig.width=10, fig.height=10
# eventdist01 ----
subset(dat01,!src_evt %in% v(c_misc)) %>% na.omit %>% 
  ggplot(aes(x=round(TIME_TO_EVENT),fill=src_evt)) + 
  geom_bar(position=position_dodge()) + 
  scale_y_continuous(oob=squish,limits=c(0,3000)) + 
  xlim(-20,80) + labs(x='Days After Admission') + 
  guides(fill=guide_legend(title="SOURCE|EVENT"
                           ,ncol = 2)) + 
  with(subset(dct0,!src_evt %in% v(c_misc))
       ,scale_fill_manual(limits=src_evt,values = color)) +
  theme(legend.position = 'bottom',text=element_text(family="Times New Roman"));

cat('

Distribution of Events Relative to NSQIP Admission Date, omitting the most common events (orders)');
#' 
#' :::::
#' 
#' ***
#' # Time Lines
#' 
#' The plots below are an attempt to visually interpret patterns in patient 
#' histories by aligning them horizontally on NSQIP admission and vertically 
#' on the timing of the following events, in order of priority: admission date
#' for the index NSQIP stay as recorded in the EMR, the last inpatient discharge
#' before the index NSQIP stay (if any), the NSQIP discharge date (as recorded 
#' in NSQIP, if any), the first inpatient admission after the index NSQIP stay,
#' and the number of days between the last pre-NSQIP inpatient discharge (or, if 
#' there is none, then the first event of any type) and 
#' first post-NSQIP inpatient admission (or, if there is none, then the _last_
#' event of any type). The reasoning behind this is that if there are prior or
#' subsequent inpatient stays, the events during and beyond them belong to those
#' respective stays and not to the index NSQIP stay.
#' 
#' ::::: {#fig:allevents00 custom-style="Image Caption"}
#+ allevents00,cache=TRUE,results='asis',warning=.debug>1,fig.height=20,fig.width=10
# allevents00 ----
.xlim <- c(-200,800);
.input <- dat01; .input[,order.active:=order01];
source('snippet_ts_explore_allevents.R',local = TRUE);
cat('

Patient timelines, grouped by similarity with color/shape coded events 
superimposed. The events were grouped by patient. There are pulse-like patterns 
where timelines are longer and longer, then reset back to short timelines. These
"pulses" are cases that are tied for everything up to and including 
NSQIP `DischargeDt`, so within each `DischargeDt` they are sorted by last event.
Bright red points represent cases where the Sunrise
admit date _does not match_ the NSQIP admit date. Orange points are various 
types of orders that are neither discharges nor admits. Purple points represent 
surgery start (`x`) and end (`+`) times. Since this is on the scale of days, 
they usually coincide so they look like asterisks, but in a few cases they occur 
on different days, and distinct `x` and `+` symbols can be distinguished. 
Finally, discharge events (`CV3ClientVisit|DischargeDt`, 
`cv3Order|DischargeOrder`, and `NSQIP|DischargeDt`) are in various shades of 
green if they are believed to associated with the index stay. The NSQIP one is a 
solid dot, while the other two are hollow triangles. Therefore, when they 
coincide there should be dark green triangles with bright green centers. _When 
they do not coincide, the dark green triangles have a color other than green in 
their centers_. Pre-NSQIP discharges and admits are shades of pink and 
post-NSQIP discharges and admits are shades of blue. Admits are right-pointing 
triangles and discharges are left-pointing. For both pre- and post-NSQIP, 
specifically the inpatient admits and discharges use symbols that are larger 
than the others and solid instead of empty inside.
Note: the actual NSQIP admission date is not directly plotted here
because it exists for every case and in this dataset its value is always 0
(i.e. it cannot deviate from itself)

');
#' 
#' :::::
#' 
#' ###### blank
#' 
#' ***
#' 
#' ::::: {#fig:allevents01 custom-style="Image Caption"}
#+ allevents01,cache=TRUE,results='asis',warning=.debug>1,fig.height=20,fig.width=10
# allevents01 ----
.xlim <- c(-30,30);
source('snippet_ts_explore_allevents.R',local = TRUE);
cat('

Same data as [@fig:allevents00] but now the time-window narrowed to 30 days
before or after NSQIP admission date to better see fine detail
')
#' 
#' :::::
#' 
#' ###### blank
#' 
#' 
#' ***
#' 
#' # Full Timelines
#' 
#' From [@fig:allevents00] several trends can be noticed. It is common for
#' orders to precede admission. It is rare but possible for the Sunrise admit
#' date to deviate from the date recorded in NSQIP in either direction. When the
#' Sunrise date is earlier than NSQIP's, it almost always comes before any
#' orders. Sunrise discharge-related dates usually agree with NSQIP's, but when 
#' they deviate the Sunrise dates always come later. Surgeries trend closer to
#' admission than discharge. Usually, from admission to discharge there is a
#' dense stream of orders, close to daily, _and they can continue after
#' discharge_ though with a diminished frequency.
#' 
#' But these timelines can include multiple admissions and discharges to either 
#' side of the interval reported in NSQIP. What if the events before the 
#' last discharge before the NSQIP index stay and after the first admission 
#' after the NSQIP index stay were trimmed off? Will any further trends become
#' noticeable? The results are shown in [@fig:allevents02] and 
#' [@fig:allevents03]. Originally there had been `r nrow(dat01)` events and 
#' after trimming off the previous and subsequent stay events there were 
#' `r nrow(dat02)` events, so a total of `r nrow(dat01)-nrow(dat02)` were 
#' excluded.
#'
#' ###### blank
#' 
#' # Timelines only from Previous to Next Inpatient Stays
#' 
#' Some outpatient discharges can still be seen in [@fig:allevents02] and 
#' [@fig:allevents03]. This may be because when the next inpatient admit 
#' _coincides_ with the NSQIP discharge date, those events are counted as not 
#' having a post-NSQIP admission, so all subsequent events are included. There
#' are `r dat01qc['chk_readmovrlp']` such cases.
#' 
#' ::::: {#fig:allevents02 custom-style="Image Caption"}
#+ allevents02,cache=TRUE,results='asis',warning=.debug>1,fig.height=20,fig.width=10
# allevents02 ----
.xlim <- c(-200,800);
.input <- dat02;
source('snippet_ts_explore_allevents.R',local = TRUE);
cat('

Same data as [@fig:allevents00] but showing only the events in between the last
pre-NSQIP inpatient discharge and first post-NSQIP inpatient admission. In cases 
where one or both bounds do not exist, those timelines extend to the first or
last available event of any type
')
#' 
#' :::::
#' 
#' ###### blank
#' 
#' 
#' ***
#' 
#' ::::: {#fig:allevents03 custom-style="Image Caption"}
#+ allevents03,cache=TRUE,results='asis',warning=.debug>1,fig.height=20,fig.width=10
# allevents03 ----
.xlim <- c(-30,30 );
source('snippet_ts_explore_allevents.R',local = TRUE);
cat('

Same data as [@fig:allevents02] but now the time-window narrowed to 30 days
before or after NSQIP admission date to better see fine detail. Still excluding
everything before and after the previous and next inpatient admission
')
#' 
#' :::::
#' 
#' ###### blank
#' 
#' 
#' ***
#' 
#' ###### blank
#' 
#' 
#' ::::: {#fig:template custom-style="Image Caption"}
#+ template,cache=TRUE #,results='asis',warning=.debug>1,fig.height=20,fig.width=10
# template ----
#.xlim <- c(-200,800);
#.input <- dat02;
#source('snippet_ts_explore_allevents.R',local = TRUE);
# cat('
# 
# Figure caption
# ')
#' 
#' :::::
#' 
#' ###### blank
#' 
#' 
#' ***
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

# #' ### Audit Trail
#+ echo=FALSE,results='hide'
.wt <- walktrail();
c()
