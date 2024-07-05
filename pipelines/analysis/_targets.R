#   {targets} documentation see:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(crew)
library(fastverse)

# Set target options:
# packages that your targets need to run
tar_option_set(
  packages = c("glue","visdat","ggplot2","gridtext","ggtext","patchwork",#"quarto",
               "metafor","crayon","rlang","fastverse","gt","flextable"), 
  format = "rds"
)

# Whenever you add new packages to the pipeline!
# tar_renv(
#   script = "pipelines/analysis/_targets.R",
#   path = "pipelines/analysis/_targets_packages.R"
# )

# tar_make_clustermq() configuration (okay to leave alone):
# options(clustermq.scheduler = "multiprocess")

options(
  datatable.print.nrows = 10,
  datatable.print.trunc.cols = TRUE,
  datatable.print.class = TRUE,
  datatable.fread.na.strings = ""
)

tar_source()

mod_plan <- readRDS("pipelines/preproc/_targets/objects/mod_pruned_transf")

tar_plan(
  
  power_analysis_fty(),
  
  tar_rds(
    dat_file,
    "pipelines/preproc/_targets/objects/dat_proc"#,
    #cue = tar_cue_force(TRUE)
  ),
  tar_target(dat,readRDS(dat_file),cue = tar_cue_force(TRUE)),
  tar_rds(
    mod_list_file,
    "pipelines/preproc/_targets/objects/mod_pruned_transf"#,
    #cue = tar_cue_force(TRUE)
  ),
  tar_target(
    mod_list, readRDS(mod_list_file)#, cue = tar_cue_force(TRUE)
  ),
  
  descriptive_table_fty(),
  
  descriptive_vis_fty(),
  
  compl_vi_imputation_fty(),
   
  pooled_es_fty(),

  meta_regs_fty(mod_plan),

  within_study_es_fty()#,
  
  # NOTE: These reports don't render on some machines from within the pipeline,
  # if quarto.exe is restricted by the system.
  # But you can render them manually.
  # tar_quarto(
  #   results_chap,
  #   path = "pipelines/analysis/reports/results/results.qmd"
  # ),
  # tar_quarto(
  #   appendix,
  #   path = "pipelines/analysis/reports/appendix/appendix.qmd"
  # )
  
)
  


