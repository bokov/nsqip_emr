#' Determine which vectors have a one-to-one relationship.
#'
#' @param data Any object that can serve as an environment for \code{eval}. By
#'             default, the calling environment.
#' @param pairs An \code{alist}. The names and values will be converted into
#'              text and evaluated in the cops
#' @param .delimchar Names are created by pasting this string between the left
#'                   and right hand side of each name-value pair from 
#'                   \code{pairs}
#'
#' @return Named logical vector 
#'
#' @examples
#' 
#' one2one(mtcars,alist(wt=qsec,wt=drat,drat=qsec))
#' 
#' one2one(mtcars[2:5,],alist(wt=qsec,wt=drat,drat=qsec))
#' 
#' one2one(mtcars[2:5,],alist(wt='qsec',wt='drat',drat='qsec'))
#' 
#' # can handle expressions 
#' one2one(mtcars,alist(wt=qsec,wt=drat,`1:length(drat)`=rnorm(length(drat))))
#' 
#' # works on environments too
#' one2one(.GlobalEnv,letters,LETTERS);
#' 
#' one2one(,letters,LETTERS);
#' 
one2one <- function(data=parent.frame(),pairs=alist(),.delimchar='~'){
  sapply(seq_along(pairs),function(ii){
    i1<-names(pairs[ii]);
    i2 <- if(is.character(pairs[[ii]]) && length(pairs[[ii]])==1){
      pairs[[ii]]} else deparse(pairs[[ii]]);
    iiname <- paste0(i1,.delimchar,i2);
    if(eval(parse(text=sprintf('length(%s)!=length(%s)',i1,i2)),data)){
      return(setNames(FALSE,iiname))};
    setNames(eval(parse(text=sprintf('oo<-unique(data.frame(%s,%s));
                                      nrow(oo)==length(unique(oo[[1]])) &&
                                      nrow(oo)==length(unique(oo[[2]]))',i1,i2))
                  ,data),paste0(i1,.delimchar,i2))});
}

#' Vectorized version of the \code{comment} command.
#' 
#' Takes an iterable object (list or data.frame) and returns the comments if any
#'
#' @param xx       \code{list} or anything that inherits from it including 
#'                 \code{data.frame}
#' @param simplify Passed to \code{sapply}
#' @param ...      Currently ignored.
#'
#' @return Named character list or vector, depending on how \code{simplify} is 
#' set
comments <- function(xx,simplify=FALSE,...) {
  sapply(xx,comment,simplify=simplify)};

c()