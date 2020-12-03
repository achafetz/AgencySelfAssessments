## PROJECT:  AGENCY SELF ASSESSMENT
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  GEND_GBV
## DATE:     2020-12-02
## UPDATED:  


# DEPEDENCIES -------------------------------------------------------------

library(tidyverse)
library(ICPIutilities)
library(glue)
library(gt)
library(RColorBrewer)
library(extrafont)
library(scales)
library(glitr)
library(patchwork)


# IMPORT DATA -------------------------------------------------------------

df <-  list.files("~/Data", "OU_IM", full.names = TRUE) %>% 
  read_rds() %>% 
  filter(operatingunit == "Tanzania")  


df_mtmeru <- read_msd("data/Genie-SiteByIMs-Tanzania-Daily-2020-12-02.zip")

# MUNGE -------------------------------------------------------------------

df_gbv <- df %>% 
  filter(fundingagency == "USAID",
         indicator == "GEND_GBV", 
         standardizeddisaggregate == "Total Numerator",
         fiscal_year == 2020) %>% 
  group_by(fundingagency, primepartner, mech_name, indicator, fiscal_year) %>% 
  summarise(across(c(cumulative, targets), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(targets > 0 ) 

df_gbv <- df_gbv %>% 
  mutate(achv = cumulative/targets,
         partner = recode(primepartner, "DELOITTE CONSULTING LIMITED" = "Deloitte",
                          "Elizabeth Glaser Pediatric Aids Foundation" = "EGPAF"))

df_gbv %>% 
  ggplot(aes(partner, cumulative)) +
  geom_blank(aes(y = cumulative * 1.1)) +
  geom_col(fill = scooter) + 
  geom_errorbar(aes(y = targets, ymin = targets, ymax = targets), 
                color = burnt_sienna, size = 2, width = .8) +
  geom_point(aes(y = -800), size = 8, color = burnt_sienna) +
  geom_text(aes(y = -800, label = percent(achv, 1)),
            color = "white", size = 2.5,
            family = "Source Sans Pro") +
  geom_text(aes(label = comma(cumulative)),
            # color = "white", #size = 2.5,
            vjust = -1.5,
            family = "Source Sans Pro") +
  scale_y_continuous(label = comma) +
  labs(x = NULL, y = NULL,
       title = toupper("USAID IPs reached 40k\nsurvivors with post-\nviolence care"),
       subtitle = "USAID FY20 GEND_GBV",
       caption = "Source: FY20Q4i MSD") +
  si_style_ygrid() 


si_save("out/TZA_GEND_GBV.png", height = 4.25, width = 2.7)  
  


  
  df_mtmeru %>% 
    # filter(ageasentered == "<18") %>% 
    distinct(trendscoarse) %>% 
    arrange(trendscoarse) %>% 
    pull(trendscoarse)
  