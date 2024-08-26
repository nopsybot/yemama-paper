metareg_plots_fty <- function() {
  
  tar_plan(
    tar_map(
      data.table(
        .fignum = c(2,3),
        .feature = c("sample","design"),
        .height = c(8,15)
      ),
      names = ".feature",
      tar_plan(
        ALLreg_sum_plot = plot_all_metaregs(
          ALLreg_sum_l__unflu1[feature==.feature,],
          fignum = .fignum
        ),
        tar_file(
          ALLreg_sum_plot_file,
          fs::path(
            paste0("pipelines/analysis/figs/metaregs/",.feature,"_metaregs.png")
          ) %T>% 
            ggsave(
              ALLreg_sum_plot, device = ragg::agg_png,
              width = 15, height = .height, res = 300, units = "cm",
              scale = 1.2
            )
        )
      )
    ),
    
    
    ALLregsum_plot__unflu1 = ALLreg_sum_l__unflu1 %>% 
      patchplot_all_metaregs(
        custom.asin.cartxlims = c(-.25,.2),
        custom.asin.breaks = c(-.2,0,.2),
      ),
    tar_file(
      ALLregsum_plot__unflu1_file,
      here::here("pipelines/analysis/figs/metaregs/all_metaregs.png") %T>% 
        ggsave(
          ALLregsum_plot__unflu1, device = ragg::agg_png,
          width = 15, height = 20,units = "cm", dpi = 300, scale = 1.1
        )
    ),
    ALLregsum_plot__unflu1_JMIR = ALLreg_sum_l__unflu1 %>% 
      patchplot_all_metaregs(
        custom.asin.cartxlims = c(-.25,.2),
        custom.asin.breaks = c(-.2,0,.2),
        ffont = "Times New Roman"
      ),
    tar_file(
      ALLregsum_plot__unflu1_JMIR_file,
      here::here("pipelines/analysis/figs/metaregs/all_metaregs_JMIR.png") %T>% 
        ggsave(
          ALLregsum_plot__unflu1_JMIR, device = ragg::agg_png,
          width = 900, height = 1200,units = "px", dpi = 300, scale = 1.8,
          bitsize = 16
        )
    ),
    
    ALLregsum_plot__unflu1_uncut = patchplot_all_metaregs(ALLreg_sum_l__unflu1_uncut),
    tar_file(
      ALLregsum_plot__unflu1_uncut_file,
      here::here("pipelines/analysis/figs/metaregs/all_metaregs_uncut.png") %>% 
        ggsave(
          ALLregsum_plot__unflu1_uncut, device = ragg::agg_png,
          width = 15, height = 20,units = "cm", dpi = 300, scale = 1
        ))
  )
  
}