gt_summarize_detail <- function(
    data = tar_read(meta_dat),
    outcomes = c("compliance_reported","retention_reported","acceptance_reported"),
    modvars = tar_read(mod_shortlist)) {

  sum_dt <- outcomes %>% 
    lapply(
      \(x) summarize_count_details(data,x,modvars) %>% 
        frename(value=x,.nse=FALSE)
    ) %>% 
    setNames(outcomes) %>% 
    Reduce(
      f=\(x,y) collapse::join(
        x,y[,],
        on=c("var","count_cat"),
        drop.dup.cols = TRUE,
        validate = "1:1",
        verbose = 0)
    ) %>%
    colorderv(c("label","count_cat",outcomes))
  
  sum_dt %>% gt_style_summary_detail()
}
