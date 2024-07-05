simplify_results_w <- function(
    combined_metaregsum_w = tar_read(combined_metaregsum_w)
) {
  combined_metaregsum_w %>% 
    mtt(
      across(
        c(b_mod,se_mod,ci.lb_mod,ci.ub_mod,pval_mod,QM,QMdf,QMp,R2),
        \(x) lapply(x,\(y) round(y,3))
      ),
      stat = iif(
       sapply(terms,is.null),
       glue::glue("b = {b_mod}; SE = {se_mod}, [{ci.lb_mod}; {ci.ub_mod}]"),
       glue::glue("Q({QMdf}) = {QM}")),
      pval = iif(is.na(QM),pval_mod,QMp) %>% as.numeric(),
      sig = iif(pval < 0.005,"*",""),
      R2 = R2
    ) %>% 
    fselect(
      outcome,moderator,k,stat,pval,sig,R2
    )
    
}
