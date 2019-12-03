## PROJECT:  AGENCY SELF ASSESSMENT
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  table of PEPFAR 95s
## DATE:     2019-12-01
## UPDATED:  


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(ICPIutilities)
library(tameDP)
library(scales)

# IMPORT DATA -------------------------------------------------------------

df_mer <- list.files("~/ICPI/Data", "PSNU_IM", full.names = TRUE) %>% 
  read_rds()

df_twbx <- read_csv("data/cop19_dp_extraction_pop_v6.csv",
                    col_types = c(.default = "c"))


# MUNGE -------------------------------------------------------------------

  #limit tabeau file to just TZA and non-calculated fields
    df_twbx <- df_twbx %>% 
      filter(Operatingunit == "Tanzania") %>% 
      select(Operatingunit,
             Psnu, Psnuuid, 
             Age, Agecoarse,
             Sex, Indicator, 
             Val,
             Snu1) %>% 
      mutate(Val = as.double(Val)) %>% 
    rename_all(tolower)
  
  #extract data for VL from MSD
    df_vl <- df_mer %>% 
      filter(operatingunit == "Tanzania", 
             indicator %in% c("TX_CURR","TX_PVLS"),
             standardizeddisaggregate %in% c("Age Aggregated/Sex/HIVStatus",
                                             "Age/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus"),
             fiscal_year == 2019,
             sex != "Unknown Sex",
             ageasentered != "Unknown Age") %>% 
      mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>% 
      group_by(operatingunit, snu1, psnu, psnuuid, fiscal_year, indicator, ageasentered, trendscoarse, sex) %>% 
      summarise_if(is.double, sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      reshape_msd("long") 
  
  #clean up VL data
    df_vl <- df_vl %>% 
      filter((indicator == "TX_CURR" & period == "fy2019q2") |
             (indicator %in% c("TX_PVLS", "TX_PVLS_D") & period == "fy2019q4")) %>% 
      mutate(indicator = ifelse(indicator == "TX_CURR", "prior_ART", str_replace(indicator, "TX_PVLS", "VL"))) %>% 
      select(-period) %>% 
      rename(age = ageasentered, 
             agecoarse = trendscoarse)

  #bind dataset together
    df_tza <- bind_rows(df_twbx, df_vl) %>% 
      filter(!indicator %in% c("population", "prevalence"))
    
  #aggregat to ou level
    df_total_sex <-  df_tza %>% 
      group_by(indicator, age) %>% 
      summarise_at(vars(val), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      mutate(sex = "Total")
    
    df_agesex <- df_tza %>% 
      group_by(indicator, age, sex) %>% 
      summarise_at(vars(val), sum, na.rm = TRUE) %>% 
      ungroup() 
    
    df_tza_total <- df_total_sex %>%
      bind_rows(df_agesex) %>% 
      spread(indicator, val) %>% 
      rename(Current_ART = current_ART) %>% 
      mutate(ART_Coverage = Current_ART/PLHIV,
             VL_Coverage = VL_D/prior_ART,
             VL_Suppression = VL/VL_D) %>% 
      select(-c(prior_ART, VL, VL_D)) %>%
      gather(ind, val, -age, -sex) %>% 
      mutate(ind = str_replace(ind, "_", " ")) %>% 
           #val = ifelse(ind %in% c("PLHIV", "Current ART"), comma(val), percent(val, 1))) %>% 
      unite(ind, c(ind, sex), sep = " ") %>% 
      spread(ind, val) %>% 
      select(age, 
             `PLHIV Female`, `PLHIV Male`, `PLHIV Total`,
             `Current ART Female`, `Current ART Male`, `Current ART Total`,
             `ART Coverage Female`, `ART Coverage Male`, `ART Coverage Total`,
             `VL Coverage Female`, `VL Coverage Male`, `VL Coverage Total`,
             `VL Suppression Female`, `VL Suppression Male`, `VL Suppression Total`)
 
  #export
    write_csv(df_tza_total, "out/95s_table_tza.csv", na = "")

