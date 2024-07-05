plot_imputation_simple <- function(
    model_frame = tar_read(compl_vi_model_frame),
    dat_imputed = tar_read(dat_imputed)
) {
  
  reg_lines <- model_frame %>% 
    mtt(
      pred_data = list(seq(0,1,0.005),cbind(seq(0,1,0.005),seq(0,1,0.005)^2)),
      pred_lines = purrr::pmap(
        list(result,pred_data),
        \(mod,pred_vals) metafor::predict.rma(
          mod,newmods = pred_vals,addx = TRUE
        ) %>% 
          collapse::qDT() %>% 
          collapse::mtt(X = X[,2]) %>% 
          collapse::ss(j=1:9)
      ) %>% setNames(name)
    ) %$% 
    data.table::rbindlist(pred_lines,idcol = "model") %>% 
    fsubset(X > min(dat_imputed[compliance_reported==TRUE]$compl_m)) %>% 
    mtt(labs = "Imputed via meta-regression")
  
  dat_scaled <- dat_imputed %>% 
    fsubset(compliance_reported==TRUE) %>% 
    mtt(
      labs = nif(
        str_detect(compl_sd_format,"^SD"),"SD reported",
        str_detect(compl_sd_format,"^Quantile"),"Imputed from quantile information",
        str_detect(compl_sd_format,"^quadratic"),"Imputed via quadratic meta-regression"
      ) %>% factor(
        levels = c(
          "SD reported",
          "Imputed from quantile information",
          "Imputed via quadratic meta-regression")
      ),
      sd_type = as.integer(labs),
      shapes = nif(
        sd_type == 1, 21,
        sd_type == 2, 16,
        sd_type == 3, 23
      ),
      colors = nif(
        sd_type == 1L, "black",
        sd_type == 2L, see::social_colors("teal"),
        sd_type == 3L, see::social_colors("deep orange")
      )
    )
  
  
  dat_scaled[
    compliance_reported==TRUE,
  ][] %>%
    ggplot2::ggplot(ggplot2::aes(x = compl_m, y = compl_ln_sd_corr_imp)) +
    ggplot2::geom_ribbon(
      data = reg_lines[model=="full"],
      inherit.aes = FALSE,
      ggplot2::aes(x = X, ymin = ci.lb, ymax = ci.ub),
      fill = see::social_colors("deep orange"),
      alpha = 0.1,
      show.legend = FALSE
    ) +
    ggplot2::guides(
      x = ggplot2::guide_axis(cap = "both"),
      y = ggplot2::guide_axis(cap = "both")
    ) +
    # ggplot2::scale_fill_identity() +
    ggplot2::geom_line(
      data = reg_lines[model=="full"],
      ggplot2::aes(x = X, y = pred),
      color = see::social_colors("deep orange"),
      linetype = "solid",
      show.legend = FALSE
    ) +
    ggplot2::geom_point(
      # inherit.aes = FALSE,
      ggplot2::aes(
        x = compl_m,
        y = compl_ln_sd_corr_imp,
        shape = labs,
        color = labs,
        fill = labs
      ),
      alpha = 1,
    ) +
    scale_color_manual(
      values = c(
        "SD reported" = "black",
        "Imputed from quantile information" = see::social_colors("teal") %>% unname(),
        "Imputed via quadratic meta-regression" = see::social_colors("deep orange") %>% unname()
      ),
      name = "Origin of SD-value"
    ) +
    scale_fill_manual(
      values = c(
        "SD reported" = "black",
        "Imputed from quantile information" = see::social_colors("teal") %>% unname(),
        "Imputed via quadratic meta-regression" = see::social_colors("deep orange") %>% unname()
      ),
      name = "Origin of SD-value"
    ) +
    scale_shape_manual(
      values = c(
        "SD reported" = 21,
        "Imputed from quantile information" = 16,
        "Imputed via quadratic meta-regression" = 24
      ),
      name = "Origin of SD-value"
    ) +
    ggplot2::labs(
      x = "Mean compliance",
      y = "ln(SD) - corrected"
    ) +
    papaja::theme_apa() + 
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.title.position = "left",
      # legend.spacing = ggplot2::unit(0.001,"lines"),
      # legend.key.spacing.y = ggplot2::unit(0.001,"lines"),
      # legend.key.spacing = ggplot2::unit(0.001,"lines")
    )
  
  
}
