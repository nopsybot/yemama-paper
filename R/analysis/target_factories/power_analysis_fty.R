power_analysis_fty <- function(){
  
  # Pipeline and functions were adapted from Gambarota, F., & Altoè, G. (2024). Understanding Meta-Analysis Through Data Simulation With Applications to Power Analysis. Advances in Methods and Practices in Psychological Science, 7(1), 25152459231209330. https://doi.org/10/gtjkbr
  # NOTE: This can easily take 40 minutes to simulate
  
  
  sim_plan <- data.table(
    .b1 = c(0.01,0.0035,0.2,0.15,0.07),
    .x_m = list(13,13,NULL,NULL,NULL),
    .x_sd = list(9,9, NULL, NULL,NULL),
    .g = list(NULL,NULL, 4, 2, 2),
    sim_fun = list(
      rep(list(do_sim_metareg),2),
      rep(list(do_sim_metaregcat),3)
    ) %>% purrr::flatten(),
    name = list("cont.b01","cont.b003","cat4.k10","cat2.K40","cat2.K200")
  )
    
  scenarios <- tar_map(
    values = sim_plan,
    names = "name",
    tar_plan(
      sim_grid = tidyr::expand_grid(
        k = c(12,20,40,80,120,160,200,240), 
        g = .g,
        b0 = 0.75,
        b1 = .b1,
        x_m = .x_m,
        x_sd = .x_sd,
        tau2r = 0.015,
        n = 60,
        nsim=1000,
        alpha = c(.05,.005)
      ),
      sim_res = sim_grid %>% 
        cbind(
          power = purrr::pmap(.,sim_fun,.progress = TRUE) %>% unlist()
        ),
      power_curve = sim_res %>% plot_powercurve(),
      tar_file(
        power_curve_file,
        paste0("pipelines/analysis/figs/power/power_curve_metareg",name,".png") %T>%
          ggsave(
            power_curve, device = ragg::agg_png,
            width = 10, height = 8, res = 300, units = "cm",
            scale = 1.5
          )
      )
    )
  )
  
  power_fig_tar <- list(
    tar_combine(
    power_figure,
    scenarios[["power_curve"]],
    command = list(!!!.x)[
      c("power_curve_cat4.k10","power_curve_cont.b01",
        "power_curve_cat2.K40","power_curve_cont.b003",
        "power_curve_cat2.K200"
        )
    ] %>% 
      purrr::map2(
        .y = LETTERS[1:5],
        \(x,y) x + ggplot2::labs(tag = y, x = "", y = "") + 
          theme(legend.position = "none", axis.text.x.bottom = ggtext::element_markdown())
      ) %>%
      purrr::modify_at(c(4),\(x) x + labs(x = "Number of samples (k)")) %>% 
      purrr::modify_at(c(5),\(x) x + labs(x = "Number of samples (per category)")) %>% 
      purrr::modify_at(1,\(x) x + scale_x_continuous(
        breaks = seq(0,240,40),
        labels = \(x) paste0(x/4,"/<br>",x))) %>% 
      purrr::modify_at(c(3,5),\(x) x + scale_x_continuous(
        breaks = seq(0,240,40),
        labels = \(x) paste0(x/2,"/<br>",x))) %>% 
      purrr::modify_at(c(1,3,5),\(x) x + labs(y = "Power")) %>% 
      Reduce(f = `+`) +
      wrap_plots(
        list(
          cowplot::get_legend(
            power_curve_cont.b01 + theme(legend.direction = "horizontal")
          ),
          {sim_grid_cont.b01 %>% 
              fselect(
                "τ<sup>2</sup><sub>Res</sub>" = tau2r,
                "n per sample" = n,
                "Monte Carlo draws per condition" = nsim
              ) %>% 
              unique() %>% as.vector() %>%  lapply(as.character) %>% 
              append(c("average sampling variance" = "0.001"),after = 1) %>% 
              {data.table(
                "Fixed parameters" = names(.), Value = as.character(.)
              )} %>% 
              gt() %>% 
              tab_style(
                locations=cells_column_labels(), 
                style = cell_text(weight = "bold")
              ) %>% 
              fmt_markdown() %>% 
              gt::as_gtable(text_grob = gridtext::richtext_grob)}
      ),nrow = 2,heights = c(1,10)) +
      plot_layout(nrow = 3) +
      plot_annotation(
        caption = paste(
          "Power curves for prototypical meta-regression scenarios.",
          "Panels **A**, **B**, and **C** show upper-bound effects covered by few samples",
          "Panels show **D** and **E** show lower-bound effects covered by many samples"
        ),
        theme = theme(plot.caption = ggtext::element_textbox_simple(size = 11))
      )
  ),
  tar_file(
    power_figure_file,
    "pipelines/analysis/figs/power/power_figure_metareg.png" %T>%
      ggsave(
        power_figure, device = ragg::agg_png,
        width = 10, height = 12, res = 300, units = "cm",
        scale = 2.5
      )
  )
  )
  
  return(list(scenarios,power_fig_tar))
  
}