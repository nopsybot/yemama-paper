preproc_incent_plan <- function(extracts) {
  
  extracts %>% 
    mtt(
      incentives = incentives==1 %>% 
        as.logical() %>% 
        iif("Incentivization","not reported") %>% 
        factor(levels = c("Incentivization","not reported")) %>% 
        setLabels("Availability of incentives") %>% 
        setLabels(attrn="feature","design"),
      
      incent_plan_n = psum(gvr(.,"incent_plan___[1-6]")) %>% 
        setLabels(attrn="feature","design"),
      
      incent_compl = psum(gvr(.,"incent_plan___[23]"))>0 %>% 
        setLabels(attrn="feature","design"),
      
      incent_uncompl = psum(gvr(.,"incent_plan___[1456]"))>0 %>% 
        setLabels(attrn="feature","design"),
      
      incent_compliance = nif(
        incent_plan_n==0,NA_character_,
        incent_plan___2 | incent_plan___3,"compliance incentivization",
        default = "no compliance incentivization"
      ) %>% 
        factor(
          levels = c("no compliance incentivization",
                     "compliance incentivization")
        ) %>% 
        setLabels("Compliance incentivization") %>%  
        setLabels("Ignoring unreported incentivization",attrn="note") %>% 
        setLabels("Compliance incentivization:",attrn="tick_title") %>% 
        set_attr("level_abbr",setNames(c("no","yes"),levels(.))) %>% 
        setLabels(attrn="feature","design"),
      
      incent_compliance_vs_nonrep = nif(
        incent_plan_n==0,"not reported",
        incent_plan___2==1 | incent_plan___3==1,"compliance incentivization",
        default = "unreported or unrelated to compliance"
      ) %>% 
        factor(
          levels=c("unrelated to compliance","compliance incentivization",
                   "not reported")
        ) %>% 
        setLabels("Compliance incentivization") %>% 
        setLabels(attrn="feature","design"),
      
      incent_plan_anyuncompl = nif(
        psum(gvr(.,"incent_plan___[23]"))>0 & 
          psum(gvr(.,"incent_plan___[1456]"))>0, 
        "combination of both",
        psum(gvr(.,"incent_plan___[23]"))>0,   "compliance incentivization",
        psum(gvr(.,"incent_plan___[1456]"))>0, "no compliance incentivization",
        incent_plan_n==0, NA_character_
      ) %>% 
        factor(
          levels = c("no compliance incenticization",
                     "compliance incentivization",
                     "combination of both")
        ) %>% 
        setLabels("Incenivization strategy") %>%
        setLabels(
          "Collapsing compliance threshold based incentivization and incremental with compliance into one category. Likewise, flat payment, incremental with time, lottery, and returning devices are gathered into one category as unrelated to compliance.",
          attrn="note"
        ) %>% 
        setLabels(attrn="feature","design"),
      
      incent_plan = nif(
        psum(gvr(.,"incent_plan___[1-6]"))>1,"multiple",
        incent_plan___1==1,"incremental with time",
        incent_plan___2==1,"incremental with compliance",
        incent_plan___3==1,"compliance threshold",
        incent_plan___4==1,"flat payment",
        incent_plan___5==1,"lottery",
        incent_plan___6==1,"returning devices",
        psum(gvr(.,"incent_plan___[1-6]"))==0,NA_character_
      ) %>% 
        factor(
          levels = c("multiple","incremental time","with compliance",
                     "compliance threshold","flat payment","lottery",
                     "returning devices")
        ) %>% 
        setLabels("Incentivization methods") %>% 
        setLabels(attrn="feature","design")
    )
}
