## PROJECT:  AGENCY SELF ASSESSMENT
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  prep trend
## DATE:     2020-12-01
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
library(patchwork)


# IMPORT DATA -------------------------------------------------------------

df <-  list.files("~/Data", "OU_IM", full.names = TRUE) %>% 
  read_rds() %>% 
  filter(operatingunit == "Tanzania")  



# MUNGE -------------------------------------------------------------------

df_vmmc <- df %>% 
  filter(fundingagency == "USAID",
         indicator == "VMMC_CIRC",
         standardizeddisaggregate == "Age/Sex") %>% 
  group_by(indicator, trendscoarse, fiscal_year) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
  ungroup()

df_vmmc <- df_vmmc %>% 
  reshape_msd(clean = TRUE)

df_vmmc_20q3 <- tibble::tribble(
   ~indicator, ~trendscoarse,  ~period,
  "VMMC_CIRC",         "<15", "FY20Q3",
  "VMMC_CIRC",         "15+", "FY20Q3"
  )

df_vmmc <- df_vmmc %>% 
  bind_rows(df_vmmc_20q3) %>% 
  filter(trendscoarse != "Unknown Age") %>% 
  select(-period_type) %>% 
  arrange(period) %>% 
  group_by(period) %>% 
  mutate(share = val/sum(val)) %>% 
  ungroup()


df_vmmc <- df_vmmc %>% 
  mutate(age_col = ifelse(trendscoarse == "15+", moody_blue, "#cccccc"),
         age_col = fct_rev(age_col),
         lab_share = case_when(trendscoarse == "15+" ~ percent(share, 1)))

df_vmmc <- df_vmmc %>% 
  filter(str_detect(period, "19|20"))

v_vmmc <- df_vmmc %>%
  ggplot(aes(period, val)) +
  geom_col(fill = genoa) +
  # geom_col(fill = "#7f7f7f") +
  scale_y_continuous(label = comma) +
  labs(x = NULL, y = NULL) +
  si_style_ygrid()


v_vmmc_share <- df_vmmc %>% 
  ggplot(aes(period, share, fill = age_col, label = lab_share)) +
  geom_col() +
  # geom_text(family = "Source Sans Pro", color = "white",
  #           size = 4, vjust = 1.5) +
  geom_text(family = "Source Sans Pro", color = "white",
            size = 4, vjust = 1.2) +
  scale_y_continuous(label = percent) +
  scale_fill_identity() +
  labs(x = NULL, y = NULL) +
  si_style_nolines() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank())

(v_vmmc / v_vmmc_share) +
  plot_layout(heights = c(4, 1)) +
  plot_annotation(title = "DESPITE LOW VMMC, USAID ABLE TO SERVICE ONLY <span style='color:#8980cb'>15+</span> IN FY20Q4",
                  caption = "Source: FY20Q4i MSD") &
  theme(plot.title = element_markdown(size = 14, color = "#202020", face = "bold", family = "Source Sans Pro"),
        plot.caption = element_text(size = 9, color = "#909090", family =  "Source Sans Pro"))

si_save("out/TZA_VMMC.png", height = 6.3, width = 6.3)
