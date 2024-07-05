#   {targets} documentation see:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(fastverse)

# Set target options:
tar_option_set(
  packages = c("rlang","ggplot2","glue","fastverse"), # packages that your targets need to run
  format = "rds"
)
# tar_renv(
#   script="pipelines/preproc/_targets.R",
#   path="pipelines/preproc/_targets_packages.R"
# ) # Whenever you add new packages to the pipeline!

getOption("datatable.na.strings","NA")

tar_source()

tar_plan(
  
  tar_file(extracts_file,"data/raw/Drexl2024_yemama_extracts.csv"),
    
  tar_file(dict_file,"data/raw/Drexl2024_yemama_extracts_DataDictionary.csv"),
  
  dict = dict_file %>% 
    data.table::fread(na.strings = "") %>% 
    rename_dict_vars() %>% 
    preproc_dict(),
  
  extracts = extracts_file %>% 
    fread(na.strings = "") %>% 
    dissect_ids() %>% 
    frename(retention_reported = retent_reported, 
            compliance_reported = compl_reported) %>% 
    mtt(
      first_pub = fmin(year,g = primary_record_id,TRA = "fill") %>% 
        setLabels("Year of earliest publication") %>% 
        setLabels(attrn = "feature","general"),
      
      n_pub = fnobs(primary_record_id,g = primary_record_id, TRA = "fill") %>% 
        setLabels(attrn = "feature","general")
    ),
  
  interrater_fty(),
   
  match_wen2017_fty(),
  
  meta_dat = preproc_extracts(extracts_wen_matched,dict),
  
  availability_fty(),
   
  transform_skew_fty(),
  
  processed_export_fty()
  
)
