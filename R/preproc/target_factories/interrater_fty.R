interrater_fty <- function() {
  
  tar_plan(
    
    tar_file(
      eligibility_data_file,
      "data/raw/Drexl2024_yemama_eligibility_biblio_data.csv"
    ),
    
    eligibility_data = fread(eligibility_data_file, na.strings = ""),
      
    
    kappa_abs = eligibility_data %$% 
      cbind(abs_pre_rater1,abs_pre_rater2) %>%
      na.omit() %>% 
      psych::cohen.kappa(),
    
    kappa_full = eligibility_data %$% 
      cbind(full_pre_rater1,full_pre_rater2) %>%
      na.omit() %>% 
      psych::cohen.kappa(),
    
    tar_file(comment_log_file, "data/raw/Drexl2024_yemama_CommentLog.csv"),
    
    comment_log_raw = fread(comment_log_file, na.strings = ""),
    
    comment_log = comment_log_raw %>% 
      fsubset(Field %in% dict$var) %>% 
      mtt(
        arm = str_extract(`Event Name`,"(?<=Arm )\\d(?=:)"),
        form = purrr::map_chr(Field,\(x) dict[var==x,form])
      ) %>%
      fsubset(
        # Revision rate concerns only other reviewers.
        User  %in%  c("drexkons", "ralivani") 
        & !(User=="drexkons" & arm==1)
        & !(User=="ralivani" & arm==6)
        # Exclude training set from revision rate
        & Record %!in% c(
          "Abadi_2021_427_aa", "Algheryafi_2023_8462_vr", 
          "Armstrong-Carter_2023_8450_vr", "Bakshi_2017_1376_sb", 
          "Balkaya-Ince_2020_555_sb", "Balkaya-Ince_2023_8446_vr", 
          "Beesdo-Baum_2020_690_sb", "Bentley_2023_8433_vr", 
          "Bickham_2015_1691_sb", "Boerner_2023_8416_vr", "Bourke_2021_12_sb", 
          "Bray_2017_1369_sb", "Bromberg_2014_1801_sb", "Browning_2023_8403_vr", 
          "Buhr_2022_79_sb", "Bulow_2022_44_fc", "Byrd_2022_170_sb", 
          "Caon_2022_97_sb", "Cheng_2020_284_aa", "Chin_2016_1951_aa", 
          "Collins_2016_1516_aa", "Comulada_2015_1635_vr", 
          "Connelly_2010_2138_vr", "Connelly_2010_2150_vr", 
          "Cordier_2016_1592_vr", "Corwin_2023_8377_dyn_vr", 
          "Cushing_2017_1335_fc", "Cushing_2019_808_fc"
        )
        # Exclude full retention comments that were given without revision.
        & !str_detect(Comment,"no.+dropout")
        & !str_detect(Comment,"no.+withdrawal")
        & !str_detect(Comment,"absence.+dropout")
        & !str_detect(Comment,"full.+retention")
      ),
    
    interrater_field = comment_log %>% 
      fselect(arm,Record,Field,User,Comment) %>% 
      funique() %>% 
      fsubset(Record %in% dat_proc$record_id) %>% 
      gby(Field) %>% 
      fcount() %>% 
      fungroup() %>% 
      mtt(
        prc = N/nrow(dat_proc)
      ) %$% 
      qsu(prc),
      
    interrater_form = comment_log %>%
      fselect(arm,Record,form,User) %>% 
      funique() %>% 
      fsubset(Record %in% dat_proc$record_id) %>% 
      gby(form) %>% 
      fcount() %>% 
      fungroup() %>% 
      mtt(
        prc = (N*100/nrow(dat_proc)) %>% round(2),
        form = form %>% 
          str_replace_all(pattern = c("^x[01]\\d_" = "", "_" = " ")) %>%
          {nif(
            .=="incentives","Incentives, feedback, training, parent involvement",
            str_detect(.,"recruitment"),"Recruitment, acceptance, retention",
            default = .
          )} %>% 
          str_to_sentence() %>% 
          forcats::fct_inorder()
      ) %>% 
      roworder(form)
    
    
  )
  
}