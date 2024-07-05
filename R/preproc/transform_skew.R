transform_skew <- function(data,vars) {
  
  right.skewed <- data %>% 
    gv(names(.) %in% vars) %>% 
    gv(sapply(.,\(x) is.numeric(x) & datawizard::skewness(x)[[1]]>3),"names")
  
  left.skewed <- data %>% 
    gv(names(.) %in% vars) %>% 
    gv(sapply(.,\(x) is.numeric(x) & datawizard::skewness(x)[[1]] < -3),"names")
  
  
  if(length(right.skewed)>0){
    data <- data %>% 
      mtt(
        across(
          right.skewed,
          \(x) log(x) %>%
            setLabels(
              ifelse(
                str_detect(vlabels(x),"\\)$"),
                str_replace(vlabels(x),"\\)$",", log scale\\)"),
                paste(vlabels(x),"(log scale)")
              )
            ),
          .names=\(c, f) paste0(c,".log")
        )
      )
  }
  if(length(left.skewed)>0){
    data <- data %>% 
      mtt(
        across(
          left.skewed,
          \(x) exp(x) %>% 
            setLabels(paste(attributes(x)$label,"(exp transformed)")),
          .names=\(c, f) paste0(c,".log")
        )
      )
  }
  
  return(data)
}
