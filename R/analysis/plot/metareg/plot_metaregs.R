plot_metaregs <- function(
    data = tar_read(ALLreg_sum_l)[feature=="sample" & outcome=="compliance",],
    .drop_missing_mods = TRUE,
    ffont = "Arial"
){ 
  
  p <- data %>% 
  ggplot(aes(y = effect, x = b)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_linerange(
      aes(xmin = ci.lb,xmax = ci.ub), linewidth = 1) +
    geom_crossbar(
      aes(xmin = b-se,xmax = b+se), 
      fatten = 0.5,width = 0.2,fill = "grey75") +
    geom_point(
      aes(fill = sig, shape = sig),
      size = 2.5,stroke = 0.8,
      show.legend = TRUE
    ) +
    scale_y_discrete(drop = .drop_missing_mods) +
    scale_x_continuous(
      limits = c(
        min(data$ci.lb,na.rm = TRUE)-mean(data$se,na.rm=TRUE),
        max(data$ci.ub,na.rm = TRUE)+mean(data$se,na.rm=TRUE)
        )
      ) +
    scale_shape_manual(
      name = "",
      values = c("*P*<.005" = 23,"*P*<.05" = 24, "*P*≥.05" = 21),
      drop = FALSE
    ) +
    scale_fill_manual(
      name = "",
      #COLOR: values = c("p < .005" = "#6ACEEB","p < .05" = "#F0C40F", "p ≥ .05" = "white"),
      # BLACK-WHITE: 
      values = c("*P*<.005" = "white","*P*<.05" = "white", "*P*≥.05" = "white"),
      drop = FALSE
    ) +
    guides(x = guide_axis(cap = "both"), y = guide_axis(cap = "both")) +
    labs(
      x = "Estimate",
      y = "",
      caption = paste(
        "<strong>Figure 2. Metaregression estimates for sample characteristics</strong>.<br>",
        "<em>Note.</em> Undepicted estimates were not calculated due to low cell frequencies or overall missingness.",
        data$abbr_key %>% unlist() %>% unique() %>% na_rm() %>% 
          paste(collapse= "; ") %>% 
          {paste0("Abbreviaions: ",.,".")}
      )
    ) +
    
    papaja::theme_apa(base_size = 10, base_family = ffont) +
    theme(
      axis.text.y = ggtext::element_markdown(),
      legend.position = "bottom",
      legend.text.position = "right",
      legend.direction = "horizontal",
      legend.margin = margin(0.1,0.1,0.1,0.1,"lines"),
      legend.key.spacing.x = unit(0.5,"lines"),
      legend.text = ggtext::element_markdown(),
      # plot.margin = grid::unit(c(0,0,0,0),"cm"),
      plot.caption.position = "plot",
      plot.caption = ggtext::element_textbox(
        width = grid::unit(0.9, "npc"), 
        halign = 0,lineheight = 1.2
      )
    )
  
  # if(all(c("sample","design") %in% data$feature)){
  #   p + facet_grid(
  #     rows = vars(feature), space = "free_y"
  #   )
  # }
  
  return(p)
}
