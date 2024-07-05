plot_easycorr_mat <- function(
    dat_arcsin = tar_read(dat_arcsin),
    vars = tar_read(mod_list)$moderator %>% c("year","acceptance_n")
){
  
  dat_arcsin %>% 
    fsubset(any_outcome==TRUE) %>% 
    gv(vars = vars) %>% 
    fselect(-c(compl_cutoff)) %>% 
    # GGally::ggpairs(columnLabels = vlabels(.))
    correlation::correlation() %>% 
    as.matrix() %>%
    # correlation::cor_sort() %>% 
    correlation::visualisation_recipe() %>% 
    plot() %>% 
    {. + scale_x_discrete(
      labels = gv(dat_arcsin,levels(.$data[[1]])) %>% 
        vlabels() %>% rev() %>% 
        {paste0(seq_along(.),". ",.)},
      position = "top",
      expand = c(0,0)
    ) +
        scale_y_discrete(
          labels = seq_along(levels(.$data[[1]])) %>% 
            rev() %>% 
            paste0("."),
          expand = c(0,0)
        )} +
    guides(
      fill = guide_colorbar(title = "Correlation"),
      x = guide_axis(cap = TRUE),
      y = guide_axis(cap = TRUE)
    ) +
    labs(title = "") +
    papaja::theme_apa() +
    theme(
      axis.text.x.top = element_text(angle = 45,hjust = 0),
      axis.line = element_blank()
    )
}
