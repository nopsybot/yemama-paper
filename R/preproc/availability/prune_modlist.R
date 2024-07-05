prune_modlist <- function(
    data = tar_read(meta_dat),
    modlist = tar_read(mod_shortlist),
    outcomes = paste0(c("compliance","retention","acceptance"), "_reported"),
    min_obs_var = 40,
    min_obs_cat = 10,
    return = c("pruned_list","lgl_table","modname_table","lgl"),
    drop_all_false = TRUE
) {
  
  prune_fun <- function(data,modlist,outcome){
    modlist %>% 
      as.list() %>% 
      rrapply(
        f = \(x) data[
          outcome==TRUE & !is.na(x),
          { if(is.logical(x)| is.character(x)) {x <- as.factor(x)};
            if (is.factor(x)) {
              fnobs(x)>min_obs_var & all(fnobs(x,g=as.factor(x))>min_obs_cat)
            }else if(is.numeric(x)){
              fnobs(x)>min_obs_var
            }},
          env=list(outcome = outcome, x = x)
        ]
      )
  }
  
  lgl_mat <- outcomes %>% 
    setNames(.,.) %>% 
    lapply(\(x) prune_fun(data,modlist,outcome = x) %>% unlist()) %>% 
    qM() %>% 
    setDimnames(modlist,1) %>% 
    {if (drop_all_false) .[rowSums(.)>0,] else .}
  
  if ("lgl_table" == return){
    return(lgl_mat %>% qDT(row.names.col = "moderator"))
  }
  if ("modname_table" == return){
    return(
      lgl_mat %>% 
        dapply(\(x) ifelse(x,row.names(.),NA)) %>% 
        qDT(row.names.col = "moderator")
    )
  }
  if ("pruned_list" == return) {
    return(lgl_mat %>% apply(2,\(x) row.names(.)[x==TRUE]))
  }
  if ("lgl" == return){
    return(lgl_mat %>% apply(1,any))
  }
}
