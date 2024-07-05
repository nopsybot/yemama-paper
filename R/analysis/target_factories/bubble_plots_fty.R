bubble_plots_fty <- function() {
  
  tar_eval(
    values = data.table(
      outcome = c("compl","accept","retent"),
      mod = c("year","item_n_log__unflu1","n_ema_days_log__unflu1")) %>% 
      mtt(
        tar_reg_sym = glue::glue("reg_{outcome}__{mod}") %>% rlang::syms(),
        tar_plot_str = glue::glue("bubble_reg_{outcome}_{mod}"),
        tar_plot_sym = rlang::syms(tar_plot_str),
        tar_plot_file_str = glue::glue("bubble_reg_file__{outcome}_{mod}"),
        plot_file_str = glue::glue(
          "pipelines/analysis/figs/metaregs/bubble_metareg_{outcome}_{mod}.png"
        )
      ),
    expr = list(
      tar_target_raw(tar_plot_str, quote(plot_bubble_metareg(tar_reg_sym))),
      tar_target_raw(
        tar_plot_file_str,
        quote(plot_file_str %T>% ggsave(
          filename = ., plot = tar_plot_sym,
          width = 12, height = 6, units = "cm", dpi = 300,
          device = ragg::agg_png,scale = 1.6
        )) %>% as.expression()
      )
    )
  )
  
}