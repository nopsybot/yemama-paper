pooled_inclusive_fty <- function(pooled_plan) {
  
  tar_map(
    values = pooled_plan,
    names = "name",
    
    tar_plan(
      
      # Basic results
      pooled = metafor::rma(
        yi = yi_var, vi = vi_var, ni = ni_var, slab = record_id,
        method = "REML",
        data = dat_arcsin[select_var==TRUE,]
      ),
      tar_file(
        pooled_report_file,
        fs::path(here::here("pipelines/analysis/reports/pooled/"),name) %T>% 
          yemama_pooled_reporter(pooled, dir = .,funnel=c("yaxis = 'ni'"))
      ),
      
      #Metafor caterpillar
      caterpillar_plot = plot_rma_caterpillar(
        res = pooled,
        yi_var_str = .yi_var_str,
        yi_var_lab = .x_lab,
        xscale_trans_fun = .xscale_trans_fun,
        xlab_trans_fun =  .xlab_trans_fun
      ),
      # caterpillar_plot = plot_caterpillar(
      #   res = pooled,
      #   yi_var_str = .yi_var_str,
      #   yi_var_lab = .x_lab,
      #   xscale_trans_fun = .xscale_trans_fun,
      #   xlab_trans_fun =  .xlab_trans_fun
      # ),
      
      tar_file(
        caterpillar_plot_file,
        paste0(
          "pipelines/analysis/figs/pooled/caterpillar_",name,".png"
        ) %T>% 
          {ragg::agg_png(
            filename = .,res = 300, #scaling = 3, 
            width = 15, height = pooled$k/22, units = "cm"
          )
            par(mar = rep(2, 4))
            caterpillar_plot()
            invisible(dev.off())}),
      
      
      pooled_inf = metafor::influence.rma.uni(pooled),
      tar_file(
        pooled_inf_plot_file,
        paste0("pipelines/analysis/figs/pooled/influence/pooled_influence_",name,".png") %T>% 
          {ragg::agg_png(
            filename = .,
            res = 300,
            height = 6.2,width=18,units = "cm",
            pointsize = 10,
            scaling = 0.8
          )
            metafor::plot.infl.rma.uni(pooled_inf,c(1,3))
            dev.off()}
      ),
      influencers = pooled_inf$inf %>% 
        get_elem(1:10,recursive = F) %>% 
        qDT() %>% 
        mtt(
          is.inf_zi = abs(rstudent) >= qnorm(
            pooled$level/(2 * pooled$k), lower.tail = FALSE
          ),
          is.inf_cookd = cook.d > 
            (median(cook.d, na.rm = TRUE) + 6*IQR(cook.d, na.rm = TRUE))
        ),
      
      # 3-level meta-analysis comparison
      pooled_mv = metafor::rma.mv(
        yi = yi_var, vi_var,
        slab=study_id_dep,
        random = ~ 1|study_id_dep/record_id,
        test="t",
        data = dat_arcsin[select_var==TRUE,]
      ),
      
      pooled_mv_varcomp = dmetar::var.comp(pooled_mv),
      
      pooled_mv_nol3 = metafor::rma.mv(
        yi = yi_var, vi_var,
        data = dat_arcsin[select_var==TRUE,],
        slab=study_id_dep,
        random = ~ 1|study_id_dep/record_id, 
        sigma2 =  c(0, NA)
      ),
      
      anova_full_nol3 = anova(pooled_mv,pooled_mv_nol3)
      
    ) 
  )
}