match_wen2017_fty <- function() {
  
  tar_plan(
    tar_file(wen2017_file,"data/raw/wen_check.csv"),
    wen2017 = fread(wen2017_file,na.strings = ""),
    extracts_wen_matched = extracts %>% 
      mtt(
        inWen2017 = iif(
          primary_record_id %in% wen2017$primary_record_id,
          "Wen 2017","new in Drexl 2024"
        ) %>% factor(levels = c("Wen 2017","new in Drexl 2024"))
      )
  )
  
}