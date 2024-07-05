exports_fty <- function() {
  tar_plan(
    
    tar_file(
      dict_raw_file,
      "data/preproc/YEMAMA_DataDictionary.csv",
      cue = tar_cue_force(TRUE)
    ),
    
    dict_raw = data.table::fread(file = dict_raw_file, na.strings = ""),
    
    tar_file(
      dict_raw_out_file,
      {dict_raw %>% 
        fsubset(`Variable / Field Name` %in% names(extracts)) %>% 
        data.table::fwrite(
          file = "data/preproc/Drexl2024_yemama_DataDictionary.csv"
        )
      "data/preproc/Drexl2024_yemama_DataDictionary.csv"},
      cue = tar_cue_force(TRUE)
    ),
    
    dict = dict_raw_out_file %>% 
      data.table::fread(na.strings = "") %>% 
      rename_dict_vars() %>% 
      preproc_dict(),
    
    tar_file(
      dict_export_file,
      "output/codebook_export.csv" %T>% data.table::fwrite(dict,file = .),
      cue = tar_cue_force(TRUE)
    ),
    
    extracts_export = preproc_for_students(extracts,dict),
    
    extracts_export_file = "output/YEMAMA_dat_for_students.csv" %T>% 
      data.table::fwrite(extracts_export,file = .),
    
    
    meta_dat = preproc_extracts(extracts_wen_matched,dict)#,
    
    # meta_dat_file = "output/YEMAMA_dat.csv" %T>% 
    #   data.table::fwrite(meta_dat,file = .)
  )
}