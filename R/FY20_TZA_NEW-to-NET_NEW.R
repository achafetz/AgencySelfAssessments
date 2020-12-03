## PROJECT:  AGENCY SELF ASSESSMENT
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  loss
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
library(ggtext)
library(ggrepel)


# IMPORT DATA -------------------------------------------------------------

df_psnu <-  list.files("~/Data", "PSNU_IM", full.names = TRUE) %>% 
  read_rds() %>% 
  filter(operatingunit == "Tanzania")  

df_nn <- read_csv("../right_size/Dataout/TX_CURR_NN_Calcs.csv") %>% 
  filter(operatingunit == "Tanzania")

snu_usaid <- df_psnu %>% 
  filter(fiscal_year == 2020,
         indicator == "TX_CURR",
         standardizeddisaggregate == "Total Numerator",
         snu1 != "_Military Tanzania",
         fundingagency != "Dedup") %>% 
  group_by(snu1, fundingagency) %>% 
  summarise(cumulative = sum(cumulative, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(snu1) %>% 
  mutate(share = cumulative/sum(cumulative)) %>% 
  ungroup() %>% 
  select(-cumulative) %>% 
  spread(fundingagency, share) %>% 
  arrange(desc(USAID)) %>% 
  filter(USAID > .5) %>% 
  pull(snu1)

df_tx <- df_psnu %>% 
  filter(#fundingagency == "USAID",
         fiscal_year == 2020,
         indicator %in% c("TX_NEW", "TX_NET_NEW", "TX_CURR"),
         standardizeddisaggregate == "Age/Sex/HIVStatus",
         ageasentered != "Unknown Age",
         snu1 != "_Military Tanzania") %>% 
  mutate(age = ifelse(trendscoarse == "<15", trendscoarse, ageasentered)) %>% 
  group_by(snu1, sex, age, indicator) %>% 
  summarise(cumulative = sum(cumulative, na.rm = TRUE)) %>% 
  ungroup()


df_tx <- df_tx %>% 
  spread(indicator, cumulative) %>%
  filter(TX_CURR > 0) 

df_viz <- df_tx %>% 
  mutate(ratio = abs(TX_NEW/TX_NET_NEW),
         lab = case_when(TX_NEW > 1000 & age != "40-44" ~ snu1))

df_viz %>% 
  filter(snu1 %in% snu_usaid) %>% 
  ggplot(aes(TX_NEW, TX_NET_NEW, size = TX_CURR, color = sex)) +
  geom_vline(xintercept = 0, color = "gray30") +
  geom_hline(yintercept = 0, color = "gray30") +
  geom_abline(slope = 1, color = "gray30", linetype = "dashed") +
  geom_point(alpha = .5) +
  geom_text(aes(label = lab), color = "gray50", hjust = -.3, 
            size = 2.5, na.rm = TRUE) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  scale_size_continuous(labels = comma) +
  scale_color_manual(values = c(scooter, moody_blue), name = "") +
  facet_wrap(~ age) +
  labs(title = "LARGEST LOSSES FOUND IN <span style='color:#1e87a5'>FEMALES 25-39</span>",
       subtitle = "FY20 | limited to USAID regions",
       caption = "Source: FY20Q4i MSD") +
  si_style() +
  theme(legend.title = element_text(color = "gray30", family = "Source Sans Pro"),
        plot.title = element_markdown(),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")
        )

si_save("out/TZA_losses.png", width = 9, height = 6.3)  



df_viz <- df_tx %>% 
  mutate(ratio = abs(TX_NEW/TX_NET_NEW),
         lab = case_when(TX_NEW > 500 & ratio > 1.5  ~ snu1))

df_viz %>% 
  filter(snu1 %in% snu_usaid) %>% 
  ggplot(aes(TX_NEW, TX_NET_NEW, size = TX_CURR, color = sex)) +
  geom_vline(xintercept = 0, color = "gray30") +
  geom_hline(yintercept = 0, color = "gray30") +
  geom_abline(slope = 1, color = "gray30", linetype = "dashed") +
  geom_text_repel(aes(label = lab), color = "gray50", hjust = -.3, size = 2.5, na.rm = TRUE) +
  geom_point(alpha = .5) +
  expand_limits(y = 2000, x = 2000) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  scale_size_continuous(labels = comma) +
  scale_color_manual(values = c(scooter, moody_blue), name = "") +
  facet_wrap(~ age) +
  labs(title = "LARGEST LOSSES FOUND IN <span style='color:#1e87a5'>FEMALES 25-39</span>",
       subtitle = "FY20 | limited to USAID regions",
       caption = "Note: Regions with more than 500 NEW and a 1.5 NEW to NET NEW ratio labeled
       Source: FY20Q4i MSD") +
  si_style() +
  theme(legend.title = element_text(color = "gray30", family = "Source Sans Pro"),
        plot.title = element_markdown(),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")
  )

si_save("out/TZA_losses.png", width = 9, height = 6.3)  
