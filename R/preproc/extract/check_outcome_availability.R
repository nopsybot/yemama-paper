check_outcome_availability <- function(extracts) {
  
  extracts[compliance_reported==TRUE,!is.na(compl_m)] %>% 
    collapse::qtab(na.exclude = FALSE) %>% addmargins() %>% as.list() %>%
    (\(x) if(is.null(x$`FALSE`)){
      cli::cli_alert_success(
        paste("All {.empg-br {x$`TRUE`}} sample compliances quantified.",sep = "\n")
      )
    }else if(x$`FALSE`>0){
      missing_vals <- extracts[compliance_reported==TRUE & is.na(compl_m), record_id]
      cli::cli_alert_info(
        paste("{.val-br {x$`FALSE`}} out of {.val-br {x$Sum}} (i.e., {.val-br {round(x$`FALSE`/x$Sum*100,1)}%}) compliance rates are missing, albeit labeled as reported!",
              "Check {.val-br {missing_vals}} !",sep = "\n")
      )})()
  
  extracts[compliance_reported==TRUE & !is.na(compl_m),!is.na(compl_sd)] %>% 
    collapse::qtab(na.exclude = FALSE) %>% addmargins() %>% as.list() %>% 
    (\(x) if(is.null(x$`FALSE`)){
      cli::cli_alert_success(
        paste("No sampling variences to be imputed.",sep = "\n")
      )
    }else if(x$`FALSE`>0){cli::cli_alert_info(
      "{.val-br {x$`FALSE`}} out of {.val-br {x$Sum}} (i.e., {.val-br {round(x$`FALSE`/x$Sum*100,1)}%}) sampling variances need to be imputed !")}
    )()
  
  extracts[compliance_reported==TRUE & !is.na(compl_m),!is.na(compl_sample_size)] %>% 
    collapse::qtab(na.exclude = FALSE) %>% addmargins() %>% as.list() %>% 
    (\(x) if(is.null(x$`FALSE`)){
      cli::cli_alert_success(
        paste("All {.empg-br {x$`TRUE`}} sample sizes available.",sep = "\n")
      )
    }else if(x$`FALSE`>0){
      missing_vals <- extracts[compliance_reported==TRUE & is.na(compl_sample_size), record_id]
      cli::cli_alert_info(
        paste("{.val-br {x$`FALSE`}} out of {.val-br {x$Sum}} (i.e., {.val-br {round(x$`FALSE`/x$Sum*100,1)}%}) sample sizes are missing, albeit labeled as reported!",
              "Check {.val-br {missing_vals}} !",sep = "\n")
      )})()
  
}
