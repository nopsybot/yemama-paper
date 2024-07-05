pooled_unfluenced_fty <- function(pooled_plan) {
  
  # Map out some plotting arguments we nee for the funnel plots later
  pooled_plan[
    ,.c(.x_breaks,.x_minorbreaks,.x_labels,.x_lab) := list(
      list(
        list(seq(0.2,1,0.2)),
        rep(list(seq(0.2,1,0.2) %>% metafor::transf.arcsin()),2)
      ) %>% purrr::flatten(),
      list(
        list(seq(0.2,1,0.2)),
        rep(list(seq(0.2,1,0.1) %>% metafor::transf.arcsin()),2)
      ) %>% purrr::flatten(),
      list(\(x) x*100,rep(list(metafor::transf.iarcsin),2)) %>% unlist(),
      c("Compliance (%)","Retention (arcsine scale)","Acceptance (arcsine scale)")
    )
  ]
  
  pooled_unfluenced_results <- tar_map(
    values = pooled_plan,
    names = "name",
    tar_plan(
      pooled_1unfluenced = metafor::rma(
        yi = yi_var, vi = vi_var, ni = ni_var, slab = record_id,
        method = "REML", 
        data = dat_arcsin[
          select_var==TRUE & 
            record_id %!in% allfluencers[str_detect(slab,'Tutelman'),slab],]
      ),
      pooled_2unfluenced = metafor::rma(
        yi = yi_var, vi = vi_var, ni = ni_var, slab = record_id,
        method = "REML", 
        data = dat_arcsin[
          select_var==TRUE & 
            record_id %!in% c(
              allfluencers[str_detect(slab,"Tutelman"),slab],
              "Achterhof_2021_259"
            ),
        ]
      ),
      pooled_5unfluenced = metafor::rma(
        yi = yi_var, vi = vi_var, ni = ni_var, slab = record_id,
        method = "REML", 
        data = dat_arcsin[
          select_var==TRUE & 
            record_id %!in% 
            allfluencers[!str_detect(slab,"Tutelman"),slab],]
      ),
      pooled_6unfluenced = metafor::rma(
        yi = yi_var, vi = vi_var, ni = ni_var, slab = record_id,
        method = "REML", 
        data = dat_arcsin[select_var==TRUE & record_id %!in% allfluencers$slab,]
      ),
      
      regtest_res = metafor::regtest(pooled_1unfluenced,predictor = "ni"),
      regtest_res_nosigma = metafor::regtest(pooled_2unfluenced,predictor = "ni"),
      
      funnel_plotdat = metafor::funnel(pooled_1unfluenced,yaxis = "ni"),
      
      funnel_ggplot = plot_funneldat(
        funnel_plotdat,
        regtest_res,
        x_breaks = .x_breaks,
        x_minorbreaks = .x_minorbreaks,
        x_labels = .x_labels,
        x_lab = .x_lab
      ),
      
      funnel_ggplot_file = fs::path(
        "pipelines/analysis/figs/pooled", paste0("funnel_plot_",name,".png")
      ) %T>% 
        ggsave(funnel_ggplot, device = ragg::agg_png, dpi = 300, scale = 1.5,
               height = 6, width = 10, units = "cm")
    )
  )
  
  funnel_fig_tars <- list(
    tar_combine(
      funnel_fig,
      pooled_unfluenced_results$funnel_ggplot,
      command = list(!!!.x) %>% 
        wrap_plots(nrow = 1) +
        plot_layout(axes = "collect")
    ),
    tar_file(
      funnel_fig_file,
      fs::path(
        "pipelines/analysis/figs/pooled","funnel_fig.png"
      ) %T>% 
        ggsave(funnel_fig, device = ragg::agg_png, dpi = 300, scale = 1.5,
               height = 5, width = 15, units = "cm")
    )
  )
  
  return(
    list(
      pooled_unfluenced_results,
      funnel_fig_tars
    ) %>% 
      purrr::flatten()
  )
}
