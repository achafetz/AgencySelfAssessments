## PROJECT:  AGENCY SELF ASSESSMENT
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  trends in select testing modalities
## DATE:     2020-12-02
## UPDATED:  


# DEPEDENCIES -------------------------------------------------------------

library(tidyverse)
library(ICPIutilities)
library(glue)
library(gt)
library(RColorBrewer)
library(extrafont)
library(glitr)
library(scales)
library(ggtext)


# IMPORT DATA -------------------------------------------------------------

df <-  list.files("~/Data", "OU_IM", full.names = TRUE) %>% 
  read_rds() %>% 
  filter(operatingunit == "Tanzania")  



# MUNGE -------------------------------------------------------------------

df %>% 
  filter(fundingagency == "USAID",
         indicator %in% c("HTS_TST", "HTS_TST_POS"),
         fiscal_year == 2020) %>%
  distinct(primepartner)
  count(standardizeddisaggregate)
         
df_hts_mod <- df %>% 
  filter(fundingagency == "USAID",
         fiscal_year == 2020,
         indicator %in% c("HTS_TST", "HTS_TST_POS"),
         primepartner == "Elizabeth Glaser Pediatric Aids Foundation",
         standardizeddisaggregate == "Modality/Age/Sex/Result",
         modality %in% c("IndexMod", "Index", "OtherPITC")) %>% 
  group_by(indicator, fiscal_year, modality) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
  ungroup() 


df_hts_mod <- df_hts_mod %>%
  mutate(modality = recode(modality, 
                           Index = "Index (Facility)",
                           IndexMod = "Index (Community)",
                           OtherPITC = "OPD (Other PITC)"))

df_hts_mod <- df_hts_mod %>% 
  reshape_msd(clean = TRUE) %>% 
  spread(indicator, val) %>% 
  mutate(positivity = HTS_TST_POS/HTS_TST)

df_hts_mod %>% 
  ggplot(aes(period, HTS_TST_POS)) +
  geom_col() +
  facet_wrap(~modality) +
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = NULL,
       title = 'EGPAF OPTIMIZING FINDING PLHIV',
       subtitle = "FY20 HTS_TST_POS Quarterly Trends",
       caption = "Source: FY20Q4i MSD") +
  si_style_ygrid()

df_hts_mod %>% 
  ggplot(aes(period, positivity, group = modality)) +
  geom_path() +
  geom_point(size = 6) +
  facet_wrap(~modality) +
  expand_limits(y = 0) +
  scale_y_continuous(labels = percent_format(1)) +
  labs(x = NULL, y = NULL,
       caption = "Source: FY20Q4i MSD") +
  si_style_ygrid()
  
  
df_hts_mod %>% 
  mutate(period = str_remove(period, "FY20")) %>% 
  ggplot(aes(period, HTS_TST_POS)) +
  geom_col(fill = scooter) +
  geom_point(aes(y = -200), size = 6, color = burnt_sienna) +
  geom_text(aes(y = -200, label = percent(positivity, 1)),
            color = "white", size = 2.5,
            family = "Source Sans Pro") +
  facet_wrap(~modality) +
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = NULL,
       title = 'EGPAF OPTIMIZING FINDING PLHIV',
       subtitle = "FY20 HTS_TST_POS Quarterly Trends",
       caption = "Source: FY20Q4i MSD") +
  si_style_ygrid()

si_save("out/TZA_EGPAF_HTS.png", height = 4.2, width = 6.75)  
  