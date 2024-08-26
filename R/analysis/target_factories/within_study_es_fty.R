within_study_es_fty <- function() {
  
  tar_plan(
    dat_within = esc_within_study(dat_imputed),
    
    # Gender ####
    # are mapped later
    # pooled_gender_diff = metafor::rma(
    #   data = dat_within[ diffgen_sig %in% c(
    #     "significant, stats reported 👇", "not significant, stats reported 👇"
    #   ),],
    #   yi = diffgen_esc, sei = diffgen_se^2,
    #   #random = ~ 1 | primary_record_id,
    #   ni = compl_sample_size,
    #   slab = authyear,
    #   measure = "GEN", method="REML"
    # ),
    
    pooled_gender_diff_forest = function(ffont = "Arial"){
      res <- pooled_gender_diff;
      metafor::forest.rma(
        res,
        addfit = TRUE,
        slab = NA,
        ilab = cbind(grplab,compl_sample_size) ,
        ilab.pos = c(2),
        ilab.xpos = c(-1.6,-1.1),
        mlab = "Random-Effects Model",
        header = TRUE,
        xlab = "Hedges' g",
        xlim = c(-3.8,3.8),
        at = c(-1,0,1,2),
        shade = TRUE,
        fonts = ffont
      )
      text(
        x = c(-2.9,-1.9,-1.3), y = res$k+2,
        labels = c("(k=25)","Group","N"),
        family = ffont, 
        font=2
      )
      text(
        x = -3.8, y = (1:res$k)-0.1,
        labels = rev(res$data$authyear),
        family = ffont, 
        font=1,
        pos = 4
      )
      text(
        x = c(0, 2.2), y = -2, 
        labels = c("Boys more compliant","Girls more compliant"), 
        col="grey20",
        pos=c(2,2), 
        #offset=-0.5,
        family = ffont
      )
    },
    
    pooled_gender_diff_forest_file = {
        "pipelines/analysis/figs/pooled/within/gender_diff_forest.png" %>% 
      (\(x) {ragg::agg_png(
        x, pointsize = 6, res = 330, width = 10, height = 12, units = "cm"
      )
      pooled_gender_diff_forest()
      invisible(dev.off())
      # re-crop image from undesired margin
      magick::image_read(x) %>% 
        magick::image_crop("1299x1400+0+140") %>% 
        magick::image_write(path =x)
      x})()
    },
    
    pooled_gender_diff_forest_jmir_file = {
        "pipelines/analysis/figs/pooled/within/gender_diff_forest_JMIR.png" %>% 
      (\(x) {ragg::agg_png(
        x, pointsize = 6, res = 300, width = 1200, height = 1200, units = "px",
        bitsize = 16
      )
      pooled_gender_diff_forest(ffont = "Times New Roman")
      invisible(dev.off())
      # re-crop image from undesired margin
      magick::image_read(x) %>% 
        magick::image_crop("1200x1050+0+120") %>% 
        magick::image_write(path =x)
      x})()
    },
    
    pooled_gender_diff_report_file = fs::path(
      here::here("pipelines/analysis/reports/pooled/gender_diff")
    ) %T>% yemama_pooled_reporter(pooled_gender_diff, dir = .),
    
    # pooled_age_corr = metafor::rma(
    #   data = dat_within[ corr_age_sig %in% c(
    #       "significant, stats reported 👇","not significant, stats reported 👇"
    #     ),],
    #   yi = corr_age_z, sei = corr_age_z_se,
    #   measure = "ZCOR", method="REML"
    # ),
    
    # AGE ####
    pooled_age_corr_forest = function(ffont = "Arial"){
      res <- pooled_age_corr
      metafor::forest.rma(
        res,
        addfit = TRUE,
        slab = authyear,
        ilab = cbind(compl_sample_size) ,
        ilab.pos = c(2),
        ilab.xpos = c(-1.1),
        header = TRUE,
        xlab = "Fisher's z",
        mlab = "Random-Effects Model",
        xlim = c(-3.8,3.8),
        shade = TRUE,
        fonts = ffont
      )
      text(
        x = c(-2.9,-1.3), 
        y = res$k+2,
        labels = c("(k=14)","N"), 
        cex=1, 
        family = ffont,
        font=2
      )
    },
    
    pooled_age_corr_forest_file = {
      "pipelines/analysis/figs/pooled/within/age_corr_forest.png" %>%
      (\(x) {ragg::agg_png(
        filename = x,
        pointsize = 6,res = 330,
        width = 10,
        height = 8,
        units = "cm"
      )
      pooled_age_corr_forest()
      invisible(dev.off())
      # re-crop image from undesired margin
      magick::image_read(x) %>% 
        magick::image_crop("1299x1400+0+140") %>% 
        magick::image_write(path =x)
      x})()
    },
    
    pooled_age_corr_forest_JMIR_file = {
      "pipelines/analysis/figs/pooled/within/age_corr_forest_JMIR.png" %>%
      (\(x) {ragg::agg_png(
        filename = x,
        pointsize = 6,
        res = 300,
        
        width = 1200,
        height = 800,
        units = "px"
      )
      pooled_age_corr_forest(ffont = "Times New Roman")
      invisible(dev.off())
      # re-crop image from undesired margin
      magick::image_read(x) %>% 
        magick::image_crop("1200x675+0+100") %>% 
        magick::image_write(path =x)
      x})()
    },
    
    
    pooled_age_corr_report_file = fs::path(
      here::here("pipelines/analysis/reports/pooled/age_corr")
    ) %T>% yemama_pooled_reporter(pooled_age_corr, dir = .),
    
    # pooled_base_corr = metafor::rma(
    #   data = dat_within[ corr_base_sig %in% c(
    #       "significant, stats reported 👇","not significant, stats reported 👇"
    #     ),],
    #   yi = corr_base_z, sei = corr_base_z_se,
    #   measure = "ZCOR", method="REML"
    # ),
    # 
    # pooled_base_corr_report_file = fs::path(
    #   here::here("pipelines/analysis/reports/pooled/base_corr")
    # ) %T>% yemama_pooled_reporter(pooled_base_corr, dir = .),
    
    # Into one fig ####
    pooled_gen_age_fig_file = {
      magick::image_read(
        c(pooled_gender_diff_forest_file,pooled_age_corr_forest_file)
      ) %>% 
        # Crop second image only if undesired top margin left
        # magick::image_crop(magick::geometry_area(y_off=150)) %>% 
        magick::image_append(stack = TRUE) %>% 
        magick::image_write(
          "pipelines/analysis/figs/pooled/within/gender_age_forest_magick.png"
        )
      "pipelines/analysis/figs/pooled/within/gender_age_forest_magick.png"
    },
    
    tar_map(
      values = data.table(
        name = c("gender_diff","age_corr"),
        .within_res = c("pooled_gender_diff","pooled_age_corr") %>% 
          rlang::syms(),
        sig_var_sym = c("diffgen_sig","corr_age_sig") %>% rlang::syms(),
        sei_lng = c("diffgen_se^2","corr_age_z_se") %>% lapply(str2lang),
        yi_var_sym = c("diffgen_esc","corr_age_z") %>% rlang::syms(),
        .measure = c("GEN","ZCOR")
      ),
      names = "name",
      tar_plan(
        pooled = metafor::rma(
          data = dat_within[ sig_var_sym %in% c(
            "significant, stats reported 👇", "not significant, stats reported 👇"
          ),],
          yi = yi_var_sym, sei = sei_lng,
          #random = ~ 1 | primary_record_id,
          ni = compl_sample_size, 
          slab = authyear,
          measure = .measure, method="REML"
        ),
        
        pooled_inf = metafor::influence.rma.uni(pooled),
        
        tar_file(
          pooled_inf_plot,
          paste0("pipelines/analysis/figs/pooled/within/pooled_influence_",name,".png") %T>% 
            {ragg::agg_png(
              filename = .,
              res = 300,
              height = 10,width=10,units = "cm",
              pointsize = 10,
              scaling = 0.8
            )
              metafor::plot.infl.rma.uni(pooled_inf,c(1,3))
              dev.off()}
        ),
        
        
        regtest_res = metafor::regtest(pooled,predictor = "ni"),
        
        funnel_plotdat = metafor::funnel(pooled,yaxis = "ni"),
        
        tar_file(
          funnel_plot_file,
          fs::path(
          "pipelines/analysis/figs/pooled/within", paste0("funnel_plot_",name,".png")
        ) %T>% 
          {ragg::agg_png(
            filename = .,
            res = 300,
            height = 10,width=16,units = "cm",
            pointsize = 10,
            scaling = 0.8
          )
            metafor::funnel.rma(pooled)
            dev.off()}
        )
      )        
    )
  )
}
