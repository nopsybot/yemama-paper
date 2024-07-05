gt_style_sensitivity <- function(pooled_combisum) {
  pooled_combisum %>% 
  mtt(
    model_lab = nif(
      object=="rma.mv", "Three-level model",
      str_detect(model,"1unflue"), "Remove one influential sample",
      str_detect(model,"6unflue"), "Remove all six influential samples",
      default = "Inclusive"
    ),
    b = iif(
      outcome=="compliance",b*100,metafor::transf.iarcsin(b)*100
    ),
    se = iif(
      outcome=="compliance",se*100,metafor::transf.iarcsin(se)*100
    ),
    ci.lb = iif(
      outcome=="compliance",ci.lb*100,metafor::transf.iarcsin(ci.lb)*100
    ),
    ci.ub = iif(
      outcome=="compliance",ci.ub*100,metafor::transf.iarcsin(ci.ub)*100
    ),
    delta = iif(
      outcome=="compliance",delta*100,metafor::transf.iarcsin(delta)*100
    ),
    outcome = str_to_sentence(outcome)
  ) %>%
    fselect(-pval,-QEp,-QEdf) %>% 
    gt(groupname_col = "outcome") %>% 
    cols_move_to_start(model_lab) %>% 
    cols_move(delta,after = b) %>% 
    cols_move(g,after = k) %>% 
    fmt_number(columns = c(b,se,ci.lb,ci.ub),decimals = 1) %>% 
    fmt_number(columns = c(tau2,se.tau2,sigma2.1,sigma2.2),decimals = 3) %>% 
    fmt_integer(columns = c(k,g,QE)) %>% 
    fmt_number(columns = delta,n_sigfig = 1,force_sign = TRUE) %>% 
    sub_small_vals(columns = delta,threshold = 0.001) %>% 
    sub_missing(columns = g,  missing_text = "") %>% 
    fmt(columns = delta, rows = model_lab=="Inclusive", fns = \(x) "-") %>% 
    cols_merge_uncert(b,se) %>% 
    cols_merge_uncert(tau2,se.tau2) %>% 
    cols_merge(c(ci.lb,ci.ub),pattern="[{1}; {2}]") %>% 
    cols_merge(c(tau2,sigma2.1,sigma2.2), pattern="<<{1}>><<{2}; {3}>>") %>% 
    cols_label(
      b = "Estimate ± SE",
      ci.lb = "95% CI",
      tau2 = "τ2 ± SE<br>(σ2.1; σ2.1)",
      .fn = gt::md) %>% 
    tab_style(
      locations = cells_row_groups(
        groups = c("Retention","Acceptance")
        ),
        style = cell_borders(sides = "top",style = "hidden")
    ) %>% 
    cols_hide(columns = c(model,object)) %>% 
    tab_header(title = "Results from sensitivity analyses.") %>% 
    tab_options(
      row_group.border.bottom.style = "hidden",
      table_body.hlines.style = "hidden",
      heading.align = "left",
      column_labels.border.bottom.style = "solid",
      table.border.top.style = "hidden",
      row_group.font.weight = "bold",
      column_labels.font.weight = "bold"
    )
}
