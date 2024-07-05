plot_compl_sd_box <- function(dat_imputed) {
  
  scale_list <- dat_imputed[
    compliance_reported==TRUE,
    .(labs = compl_sd_format %>% unique() %>% sort(),
      shapes = c(
        20,
        compl_sd_format %>% 
          as.integer() %>% unique() %>% sort() %>% shift() %>% na_rm()
      ),
      colors = compl_m_format %>% fnunique() %>% 
        (\(x) {colos <- see::palette_material()(x);
        colos[1] <- "black"; colos[x] <- see::social_colors("deep orange"); colos})()
    )
  ]
  
  dat_imputed[compliance_reported ==TRUE,] %>% 
    ggplot2::ggplot(
      ggplot2::aes(x = compl_sd_format, y = compl_ln_sd_corr_imp)
    ) +
    ggplot2::geom_boxplot(
      ggplot2::aes(fill = compl_sd_format),
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(values = scale_list$colors) +
    ggplot2::labs(
      title = "",
      x = "",
      y = ""
    ) + 
    ggplot2::theme_minimal()
}
