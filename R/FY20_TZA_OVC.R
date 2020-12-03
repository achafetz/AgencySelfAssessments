## PROJECT:  AGENCY SELF ASSESSMENT
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  OVC
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

df_k2 <- read_csv("data/k2_data.csv")

# MUNGE -------------------------------------------------------------------

df_ovc <- df %>% 
  filter(fundingagency == "USAID",
         indicator == "OVC_HIVSTAT_POS",
         standardizeddisaggregate == "Total Numerator") %>% 
  group_by(fundingagency, fiscal_year) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  reshape_msd(clean = TRUE) %>% 
  rename(ovc_pos = val)

df_ovc_art <- df %>% 
  filter(fundingagency == "USAID",
         indicator == "OVC_HIVSTAT",
         otherdisaggregate == "Receiving ART") %>% 
  group_by(fundingagency, fiscal_year) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  reshape_msd(clean = TRUE) %>% 
  rename(ovc_art = val)

df_ovc <- df_ovc %>% 
  left_join(df_ovc_art) %>% 
  mutate(art_cov = ovc_art / ovc_pos)

df_ovc %>% 
  ggplot(aes(period, ovc_pos)) +
  geom_col(fill = scooter) +
  geom_point(aes(y = -1500), size = 7, color = burnt_sienna) +
  geom_text(aes(y = -1500, label = percent(art_cov, 1)), 
            family = "Source Sans Pro", size = 2.5, color = "white") +
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = NULL,
       title = toupper("Increasing # of C/ALHIV enrolled in the OVC Program"),
       subtitle = "USAID OVC_HIVSTAT",
       caption = "Source: FY20Q4i MSD") +
  si_style_ygrid()

si_save("out/TZA_OVC.png", width = 7, height= 5)


df_k2 %>% 
  mutate(category = fct_inorder(category)) %>% 
  ggplot(aes(value, fct_rev(category), label = comma(value))) +
  geom_blank(aes(value * 1.2)) +
  geom_col(fill = moody_blue) +
  geom_text(hjust = -.2, family = "Source Sans Pro", color = "gray30") +
  scale_x_continuous(expand = c(.005, .005)) + 
  labs(x = NULL, y = NULL,
       title = toupper("Kizazi Kipya is delivering an enhanced\nC/ALHIV service package"),
       subtitle = "FY20Q4",
       caption = "Source: Kizazi Kipya Viral Load Tracking") +
  si_style_nolines() +
  theme(axis.text.x = element_blank())

si_save("out/TZA_K2_VL.png", width = 5.28, height= 4.84)
