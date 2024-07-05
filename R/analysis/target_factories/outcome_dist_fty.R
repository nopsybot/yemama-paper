outcome_dist_fty <- function() {
  
  tar_plan(
    
    outcome_pairs = dat_arcsin[
      any_outcome==TRUE,
      .(compl_m,retention_rate_extr,acceptance_rate_extr)
    ] %>% GGally::ggpairs(columnLabels = vlabels(.)),
    
    outcome_pairs_file = "pipelines/analysis/figs/desc/outcomes_distribution.png" %T>% 
      ggsave(outcome_pairs,device=ragg::agg_png,scale = 1,
             width = 15,height = 15,units = "cm",dpi = 300),
    
    outcome_dist_plot = plot_dist_outcome_asin(dat_arcsin),
    
    outcome_dist_plot_file = "pipelines/analysis/figs/desc/outcome_dist.png" %T>% 
      ggsave(
        outcome_dist_plot,device = ragg::agg_png,
        width = 15, height = 17, units = "cm"
      )
  )
     

}