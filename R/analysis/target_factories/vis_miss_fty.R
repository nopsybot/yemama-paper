vis_miss_fty <- function(){
  tar_map(
    values = data.table(
      .outcome = c("compl_m","retent.asin","accept.asin"),
      name = c("compl","retent","accept")
    ),
    names = "name",
    tar_target(
      vis_miss_mod, 
      outcome_mod_vis_miss(
        dat_arcsin, 
        outcome = .outcome,
        mod_list$moderator
      )
    ),
    tar_file(
      vis_miss_file,
      paste0("pipelines/analysis/figs/desc/miss_vis_",name,".png") %T>% 
        ggsave(vis_miss_mod,device = "png",scale = 1,
               width = 16,height = 12,units = "cm",dpi = 300)
    )
  )
}