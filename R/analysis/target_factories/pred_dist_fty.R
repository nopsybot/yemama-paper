pred_dist_fty <- function() {
  
  tar_plan(
    pred_dist_plot = plot_dist_pred_log(
      dat_arcsin,
      mod_list,
      trans.rgx = "\\.(log)|(exp)$"
    ),
    pred_dist_plot_file = "pipelines/analysis/figs/desc/predicotr_dist.png" %T>% 
      ggsave(pred_dist_plot,device=ragg::agg_png,scale = 1.25,
             width = 15,height = 22,units = "cm",dpi = 300),
        
    all_pairs = plot_easycorr_mat(
      dat_arcsin,
      vars = c(mod_list$moderator,"year","acceptance_n")
    ),
    all_pairs_file = "pipelines/analysis/figs/desc/all_pair_cors.png" %T>% 
      ggsave(all_pairs,device=ragg::agg_png,scale = 1.25,
             width = 15,height = 15,units = "cm",dpi = 300),
    
    dayprompt_cor_plot = plot_log_corr(
      dat_imputed,
      xvar = "n_ema_days.log",
      yvar = "prompt_dfreq.log"
    ),
    dayprompt_cor_plot_file = "pipelines/analysis/figs/desc/dayprompt_cor_plot.png" %T>% 
      ggsave(dayprompt_cor_plot,device=ragg::agg_png,scale = 1,
             width = 15,height = 15,units = "cm",dpi = 300),
    
    intens_money_cor_plot = plot_log_corr(
      dat_imputed[,prompt_tot_n.log := log(prompt_tot_n)][],
      xvar = "prompt_tot_n.log",
      yvar = "inc_val_pot_max.usd.log"
    )
  )
  
}