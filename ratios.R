energy_burden_func <- function(g,s,se=NULL){
  if(is.null(se)){se<-s}
  s/g
}

eroi_func <- function(g,s,se=NULL){
  if(is.null(se)){se<-s}
  g/se
}

ner_func <- function(g,s,se=NULL){
  if(is.null(se)){se<-s}
  (g-s)/se
}

dear_func <- function(g,s,se=NULL){
  if(is.null(se)){se<-s}
  (g-s)/g
}