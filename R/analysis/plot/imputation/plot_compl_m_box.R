plot_compl_m_box <- function(dat_imputed) {
  
  dat_imputed[compliance_reported ==TRUE,] %>% 
    ggplot2::ggplot(
      ggplot2::aes(x = compl_m, y = forcats::fct_rev(compl_m_format))
    ) +
    ggplot2::geom_boxplot() +
    ggplot2::scale_y_discrete(position = "right") +
    ggplot2::labs(
      title = "",
      x = "Mean compliance",
      y = ""
    ) + 
  ggplot2::theme_minimal()
    
}
