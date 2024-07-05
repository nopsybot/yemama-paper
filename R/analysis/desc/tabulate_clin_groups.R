tabulate_clin_groups <- function(
    dat_imputed = tar_read(dat_imputed)
) {
  
  dat_imputed %>% 
    mtt(
      clin_sample.any_nonclin = iif(
        clin_sample.mix  %in% c("convenience","healthy"), "nonclinical",
        as.character(clin_sample.mix)
      ),
      clin_diag_sompsy_groups = nif(
        clin_sompsy.mixna == "somatic", diag_txt_harmonized,
        clin_sompsy.mixna == "psychiatric", clin_diag_groups.lump
      )) %>% 
    fselect(clin_sample.any_nonclin, clin_sompsy.mix, clin_diag_sompsy_groups, diag_txt) %>% 
    gby(clin_sample.any_nonclin, clin_sompsy.mix, clin_diag_sompsy_groups) %>% 
    fsummarise(
      N = fnobs(diag_txt),
      n_miss = sum(is.na(diag_txt)),
      diag_txt = list(unique(diag_txt))
    ) %>% 
    fungroup() %>% 
    mtt(
      N = iif(n_miss>0,n_miss,N), # because all missings apply to healhty
      n_miss = NULL
    ) %T>% 
    {tar_assert_true(!any(is.na(.$N)))} %>% 
    roworderv(c("clin_sompsy.mix","N"),decreasing = c(FALSE,TRUE)) %>% 
    pivot(
      ids = c("clin_sompsy.mix","clin_diag_sompsy_groups"),
      names = "clin_sample.any_nonclin",
      how = "wider"
    ) %>% 
    gvr(c("clin_","N_")) %>% 
    mtt(
      clin_sompsy.mix = iif(
        !is.na(N_nonclinical) | clin_sompsy.mix=="Mixed",
        "Healthy or convenience sample",
        as.character(clin_sompsy.mix)
      ) %>% 
        str_to_sentence() %>% 
        factor(
          levels = c(
            "Somatic","Psychiatric","Mixed","Healthy or convenience sample"
          )
        ),
      clin_diag_sompsy_groups = nif(
        clin_sompsy.mix == "Mixed", "transdiagnostic",
        clin_sompsy.mix == "Healthy or convenience sample", "N/A",
        default = clin_diag_sompsy_groups
      ),
      N_total = psum(gvr(.,"N_"), na.rm=TRUE),
      N_nonclinical = NULL
    ) %>% 
    colorderv(c("N_at risk","N_mixed"),pos="after") %>% 
    roworder(clin_sompsy.mix,-N_total) %>% 
    setLabels(
      c(vlabels(.)[1],
        "Harmonized diagnostic labels","Diagnosed","At-risk","Mixed","Total")
    )
  
}