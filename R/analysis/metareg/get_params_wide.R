get_params_wide <- function(metareg) {
  
  mod_var = paste(metareg$formula.mods,collapse="") %>% str_remove("~")
  
  metareg %$%
    data.table::data.table(
      moderator = mod_var,
      label = data[[mod_var]] %>% vlabels(),
      k = k,
      R2 = R2,
      class = class(data[[mod_var]])
    ) %>% 
    mtt(
      type = iif(class %in% c("integer","numeric"),"continuous","categorical")
    ) %>% 
    {if(.$class %in% c("integer","numeric")){
      mtt(.,
          b_mod = metareg$b[2],
          se_mod = metareg$se[2],
          ci.lb_mod = metareg$ci.lb[2],
          ci.ub_mod = metareg$ci.ub[2],
          pval_mod = metareg$pval[2]
      )
    } else if (.$class %in% c("factor","character","logical")){
      mtt(.,
          terms = list(rownames(metareg$b)),
          b = list(as.numeric(metareg$b)),
          se = list(as.numeric(metareg$se)),
          ci.lb = list(as.numeric(metareg$ci.lb)),
          ci.ub = list(as.numeric(metareg$ci.ub)),
          pval = list(as.numeric(metareg$pval)),
          QM = metareg$QM[[1]],
          QMdf = metareg$QMdf[[1]],
          QMp = metareg$QMp[[1]],
          across(
            c(terms,b,se,ci.lb,ci.ub,pval),
            \(x) lapply(x,unlist) %>% lapply(\(x) str_remove(x,mod_var))
          )
      )
    }} %>% 
    colorder(R2,pos = "end") 
  
  
}
