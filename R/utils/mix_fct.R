mix_fct <- function(
    x,mix_level = "mixed"
){
  if (all(is.na(x))) 
    unique(x)
  else if(length(unique(x) %>% na_rm())>1) 
    factor(mix_level,levels=unique(c(levels(x),mix_level))) 
  else 
    unique(x) %>% na_rm()
}