plot_powercurve <- function(
    sim_res = tar_read(sim_res_cat4.k10)
){
  
  title_glue_pattern <- iif(
    any(str_detect(names(sim_res),"^g$")),
    "Categorical: g = {g}; b~1~ = {b1}",
    "Continuous: b~1~ = {b1}; M~pred~ = {x_m} (SD = {x_sd})"
  )
  
  title <- sim_res %>% 
    mtt(
      type = iif(any(str_detect(names(.),"^g$")),"Categorical","Continuous")
    ) %>% 
    purrr::map(\(x) unique(x) %>% paste(collapse = ", ")) %>% 
    glue::glue_data(title_glue_pattern)
  
  sim_res |> 
    roworder(alpha,k) %>% 
    mtt(alpha = factor(alpha, levels = c(.05,.005))) %>% 
    ggplot(aes(x = k, y = power)) +
    geom_hline(yintercept = 0.8, linetype = "dashed", color = "#CC79A7") +
    geom_line(aes(group = alpha, color = alpha)) +
    geom_point(
      aes(fill = alpha),
      size = 3, shape = 21, color = "white" , stroke = 1.5
    ) +
    see::scale_color_oi(name = "α-level") +
    see::scale_fill_oi(name = "α-level") +
    scale_x_continuous(
      limits = c(0,max(sim_res$k)),expand = c(0,0,0.05,0),
      breaks = seq(0,max(sim_res$k),40)
    ) +
    scale_y_continuous(
      limits = c(0,1),
      breaks = seq(0,1,.2),
      expand = c(0,0,0.05,0)
    ) +
    guides(
      x = guide_axis(cap = "upper"),
      y = guide_axis(cap = "upper")
    ) +
    labs(
      x = "Number of studies (k)",
      y = "Power",
      title = title
    ) +
    papaja::theme_apa() +
    theme(
      plot.title = ggtext::element_markdown(hjust = 0),
      # legend.position = "bottom",
      # legend.direction = "horizontal"
    )
}
