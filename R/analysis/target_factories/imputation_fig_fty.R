imputation_fig_fty <- function() {
  
  tar_plan(
    
    imputation_scatter = plot_imputation_simple(compl_vi_model_frame,dat_imputed),
    
    tar_file(
      imputation_scatter_file,
      "pipelines/analysis/figs/desc/compl_sd_imputation.png" %T>%
        ggsave(
          imputation_scatter + labs(title = ""),
          device = ragg::agg_png, dpi = 300,scale = 1.2,
          width = 10, height = 15, units = "cm"
        )
    ),
    
    compl_sd_boxplot = plot_compl_sd_box(dat_imputed),
    
    compl_m_boxplot = plot_compl_m_box(dat_imputed),
    
    imputation_figure = patchwork::wrap_plots(
      list(
        p1 = imputation_scatter + labs(title =""),
        p2 = compl_sd_boxplot + 
          ggplot2::theme(axis.text = ggplot2::element_blank()),
        p3 = compl_m_boxplot +
          ggplot2::theme(axis.text.y.right = ggplot2::element_blank()),
        p4 = patchwork::guide_area()
      ),
      ncol = 2,
      heights = c(2,1),
      widths = c(2,1),
      guides = "collect",
      axes = "collect"
    ),
    
    imputation_figure_file = "pipelines/analysis/figs/desc/detailed_compl_sd_imputation.png" %T>%
      ggsave(
        imputation_figure,
        device = ragg::agg_png, dpi = 300,
        width = 15, height = 15, units = "cm"
      )
    
  )
}