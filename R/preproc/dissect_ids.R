#' @title Dissect RedCAP record IDs
#' @description Dissect RedCAP record IDs into their components
#'
#' @param preproc_raw data.table: The raw data
#'
#' @return data.table: returned with the dissected ID variables prepended.
#' @export
#'
#' @examples
dissect_ids <- function(preproc_raw) {
  preproc_raw %>% 
    collapse::mtt(
      sample_id = kit::iif(
        test = redcap_event_name=="extraction_arm_1",
        yes = record_id,
        no = record_id %>% 
          stringr::str_remove("_[:lower:]{2}$")
      ),
      study_id = sample_id %>% 
        stringr::str_remove("(?<=\\d{4}_\\d{1,4})_.+$"),
      recnum = study_id %>% stringr::str_extract("(?<=\\d{4}_)\\d{1,4}$")
    ) %>% 
    collapse::colorder(
      record_id,sample_id,study_id,recnum,primary_record_id
    ) %T>%
    (\(x) targets::tar_assert_true(
      !collapse::any_duplicated(x[,.(sample_id,redcap_event_name)]),
      "Duplicated sample IDs detected!"
    ))()
}
