processed_export_fty <- function() {
  
  tar_plan(
    tar_rds(
      preprocessed_rds_file,
      "data/proc/Drexl2024_yemama_processed.rds" %T>%
        saveRDS(dat_proc, file = .)
    ),
    tar_file(
      preprocessed_csv_file,
      "data/proc/Drexl2024_yemama_processed.csv" %T>%
        fwrite(dat_proc, file = .)
    ),
    tar_rds(
      app_data_file,
      "app/Drexl2024_yemama_processed.rds" %T>%
        saveRDS(dat_proc, file = .)
    ),
    
    tar_target_raw(
    "preprocessed_cb",
    quote(
      codebookr::codebook(
        dat_proc,
        title = "YEMAMA Meta-analytic Data Codebook",
        subtitle = "Preprocessed data, ready for statistical analyses.",
        description = paste(
          "Each observation (row) shows extracted data for an eligible sample (sample ID), some of which are nested in identical studies (study ID).",
          "The primary_record_id can be used to match with publication-level data.",
          "Preprocessing steps included synthesis of checkbox variables to single-choice variables (suffixed with .mix),",
          "harmonization of diagnostic groups and clinical status variables,",
          "collapsing categories with low cell frequencies where sensible to reach categories with at least 10 observations,",
          "estimation of means and SDs from ranges, and imputation of missing values, and",
          "log-transformation of skewed moderator variables.",
          "Note: The meta-analytic pipeline was designed to use the .rds format for statistical analyses to keep variable attributes required for labelling in tables, plots etc.",
          "This codebook was autimatically generated using the {codebookr} r-package.",
          "Cannell B (2024). codebookr: Create Codebooks from Data Frames. R package version 0.1.8, https://brad-cannell.github.io/codebookr/, https://github.com/brad-cannell/codebookr."
        )
      ))
    ),
    tar_file(
      dat_proc_cb_file,
      "data/proc/Drexl2024_YEMAMA_processed_codebook.docx" %T>%
        officer:::print.rdocx(preprocessed_cb,target = .)
    )
  )
  
}
