## PROJECT:  AGENCY SELF ASSESSMENT
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  DREAMS
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

df_psnu <-  list.files("~/Data", "PSNU_IM", full.names = TRUE) %>% 
  read_rds() %>% 
  filter(operatingunit == "Tanzania")  

# MUNGE -------------------------------------------------------------------

df_psnu %>% 
  filter(indicator == "AGYW_PREV",
         fiscal_year %in% c(2019, 2020)) %>% 
  count(standardizeddisaggregate, wt = cumulative)

df_dreams <- df_psnu %>% 
  filter(indicator == "AGYW_PREV",
         fiscal_year %in% c(2019, 2020),
         ageasentered == "10-14",
         standardizeddisaggregate == "Age/Sex/Time/Complete") %>% 
  count(psnu, fiscal_year, wt = cumulative, name = "cumulative")

df_dreams <- df_dreams %>% 
  mutate(order = ifelse(fiscal_year == max(fiscal_year), cumulative, 0),
         shape_col = ifelse(fiscal_year == max(fiscal_year), scooter, "#6d6d6d"))
  
df_dreams %>% 
  ggplot(aes(cumulative, fct_reorder(psnu, order, sum), group = psnu, color = shape_col)) +
  geom_path(size = .9) +
  geom_point(data = filter(df_dreams, fiscal_year == 2019), size = 5) +
  geom_text(data = filter(df_dreams, fiscal_year == 2020),
            label = "u", size = 6, vjust = .4, 
            family = "Wingdings 3") +
  scale_x_continuous(label = comma) +
  scale_color_identity() +
  labs(x = NULL, y = NULL,
       title = toupper("Impressive gains for reach of youth preventative services"),
       subtitle = "Tanzania AGYW_PREV 10-14",
       caption = "FY20Q4i MSD") +
  si_style_xgrid()



df_dreams %>% 
  ggplot(aes(cumulative, fct_reorder(psnu, order, sum), group = psnu)) +
  geom_path(size = 2, color = scooter) +
  geom_point(data = filter(df_dreams, fiscal_year == 2019), size = 7, color = scooter) +
  geom_text(data = filter(df_dreams, fiscal_year == 2020),
            label = "u", size = 10, vjust = .45, color = scooter, 
            family = "Wingdings 3") +
  geom_text(data = filter(df_dreams, fiscal_year == 2019),
            aes(label = comma(cumulative, 1)), color = "white", 
            family = "Source Sans Pro",
            vjust = .4, size = 2) +
  geom_text(data = filter(df_dreams, fiscal_year == 2020),
            aes(label = comma(cumulative, 1)), color = "white", 
            family = "Source Sans Pro",
            vjust = .1, hjust = .8, size = 2) +
  scale_x_continuous(label = comma) +
  labs(x = NULL, y = NULL,
       title = toupper("Impressive gains for youth preventative services"),
       subtitle = "Tanzania  AGYW_PREV  10-14",
       caption = "FY20Q4i MSD") +
  si_style_xgrid() +
  theme(axis.text.x = element_blank())

si_save("out/TZA_DREAMS.png", height = 3.4, width = 6)
