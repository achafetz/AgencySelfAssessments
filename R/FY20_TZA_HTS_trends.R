## PROJECT:  AGENCY SELF ASSESSMENT
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  HTS
## DATE:     2020-11-30
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

# GLOBAL VARIABLES --------------------------------------------------------


pal <- brewer.pal(5, "Spectral")[2:5]


# IMPORT DATA -------------------------------------------------------------

df <-  list.files("~/Data", "OU_IM", full.names = TRUE) %>% 
  read_rds() %>% 
  filter(operatingunit == "Tanzania")  



# MUNGE -------------------------------------------------------------------

df_hts <- df %>% 
  filter(fundingagency == "USAID",
         indicator %in% c("HTS_TST", "HTS_TST_POS"),
         standardizeddisaggregate == "Total Numerator",
         fiscal_year > 2018) %>% 
  group_by(fundingagency, indicator, fiscal_year) %>% 
  summarise(across(where(is.double), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  reshape_msd(clean = TRUE) %>% 
  filter(period_type == "results") %>% 
  spread(indicator, val) %>% 
  mutate(positivity = HTS_TST_POS/HTS_TST,
         fy = str_sub(period, start = 3, end = 4),
         fy_color = ifelse(fy == "20", scooter, "#7f7f7f"))

v_pos <- df_hts %>% 
  ggplot(aes(period, positivity, group = fy, color = fy_color, fill = fy_color)) +
  geom_path(size = 1) +
  geom_point(shape = 21, color = "white", size = 4, stroke = 1.1) +
  scale_y_continuous(label = percent_format(1)) +
  scale_x_discrete(label = c("FY19Q1", "" ,"FY19Q3", "",
                             "FY20Q1", "" ,"FY20Q3", "")) +
  scale_color_identity() +
  scale_fill_identity() +
  expand_limits(y = c(0, .08)) +
  labs(x = NULL, y = NULL, subtitle = "Positivity") +
  si_style_ygrid()
  

v_hts <- df_hts %>% 
  ggplot(aes(period, HTS_TST, fill = fy_color)) +
  geom_col() +
  scale_y_continuous(label = comma) +
  scale_x_discrete(label = c("", "" ,"", "",
                             "", "" ,"", "")) +
  scale_fill_identity() +
  labs(x = NULL, y = NULL, subtitle = "HTS_TST") +
  si_style_ygrid()

(v_hts / v_pos) +
  plot_annotation(
  title = 'USAID TESTING AND POSITIVITY REMAINED CONSTANT \nTHROUGH THE PANDEMIC',
  caption = 'Source: FY20Q4i MSD') & si_style_ygrid()

si_save("out/TZA_HTS_trends.png", height = 3.3, width = 5)
