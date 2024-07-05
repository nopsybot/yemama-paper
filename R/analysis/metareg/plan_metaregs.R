plan_metaregs <- function(
    mod_plan = tar_read(mod_pruned_transf,store="pipelines/preproc/_targets"),
    add_filter_expr = NULL,
    tar_sfx = NULL
) {
  
  reg_plan <- mod_plan %>% 
    pivot(ids=c("moderator","feature"),names = list(variable = "sel_var")) %>%
    fsubset(!is.na(value),-value) %>% 
    mtt(
      prefix = sel_var %>% str_extract("compl|retent|accept"),
      name = paste0(prefix,"__",moderator) %>% 
        stringr::str_replace_all("\\.","_"),
      outcome = sel_var %>% str_remove("_reported"),
      reg_tarname = paste0("reg_",name),
      reg_tarsym = reg_tarname %>% lapply(as.name),
      
      sum_w_tarname = paste0("regsum_w_",name),
      sum_w_tarsym = sum_w_tarname %>% lapply(as.name),
      
      sum_l_tarname = paste0("regsum_l_",name),
      sum_l_tarsym = sum_l_tarname %>% lapply(as.name),
      
      yi_var = kit::iif(prefix=="compl","compl_m",paste0(prefix,".asin")),
      vi_var = paste0(prefix,"_vi"),
      ni_var = kit::iif(
        prefix=="compl","compl_sample_size",paste0(outcome,"_n")
      ),
      across(c(sel_var,moderator,yi_var,vi_var,ni_var),
             \(x) lapply(as.character(x),as.name)),
      mod_var = paste("~",moderator) %>% lapply(as.formula),
      i_expr = paste0(sapply(sel_var,deparse),"==TRUE") %>% lapply(str2lang)
    )
  
  if(!is.null(add_filter_expr)){
    add_filter_str = deparse(add_filter_expr) %>% 
      paste0(collapse = "")
    
    if(is.null(tar_sfx)){
      tar_sfx <- str_extract_all(add_filter_str,"^[^ =]+|fluence") %>% 
        unlist() %>% 
        paste0(collapse = "_")
    }
    
    reg_plan <- reg_plan %>% 
      mtt(
        i_expr = sapply(i_expr,deparse) %>% 
          paste(" & ",add_filter_str) %>% 
          lapply(str2lang),
        reg_tarname = paste0(reg_tarname,"__",tar_sfx), 
        reg_tarsym = reg_tarname %>% lapply(as.name),
        
        sum_w_tarname = paste0(sum_w_tarname,"__",tar_sfx), 
        sum_w_tarsym = sum_w_tarname %>% lapply(as.name),
        
        sum_l_tarname = paste0(sum_l_tarname,"__",tar_sfx),
        sum_l_tarsym = sum_l_tarname %>% lapply(as.name)
      )
  }
  
  return(reg_plan)
}
