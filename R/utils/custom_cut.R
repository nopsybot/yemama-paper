custom_cut <- function(x,by=NULL,in_breaks=NULL,breaks=NULL){
  min.x <- floor(min(x,na.rm = TRUE))
  max.x <- ceiling(max(x,na.rm = TRUE))
  
  if(is.null(breaks)){
    if(is.null(in_breaks))
      in_breaks <- seq(min.x-min.x%%by+by,max.x-max.x%%by,by)
    
    breaks <- c(min.x, in_breaks, max.x) %>% unique()
  }
  
  lbs <- breaks[1:(length(breaks)-1)]
  ubs <- c(breaks[2:(length(breaks)-1)]-1,breaks[length(breaks)])
  ndigits <- max(floor(log10(c(lbs,ubs,max.x)) + 1))
  cut(x, breaks,
      labels = paste(str_pad(lbs,ndigits),"-",str_pad(ubs,ndigits)),
      right = FALSE,include.lowest = TRUE) %>% 
    setLabels(vlabels(x)) %>% 
    setLabels(attrn = "feature",vlabels(x,attrn="feature"))
}
