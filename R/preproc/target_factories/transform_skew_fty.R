transform_skew_fty <- function(){
  
  tar_plan(
    
    dat_proc = meta_dat %>% transform_skew(
      vars = meta_dat %>% 
        gv(names(.) %in% mod_pruned$moderator) %>% 
        gv(is.numeric,"names")
    ),
    
    mod_pruned_transf = mod_pruned %>% 
      dapply(
        \(var)
        sapply(var,\(x) gvr(
          dat_proc, paste0("^",x,"\\.(log)|(exp)$"),"names"
        )) %>% 
          purrr::map2(.x = .,.y=names(.),\(x,y) ifelse(length(x)>0,x,y)) %>% 
          unlist() %>% 
          unname()
      )
  )
}
