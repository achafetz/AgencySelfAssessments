## PROJECT:  AGENCY SELF ASSESSMENT
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  partner HTS_SELF
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
         indicator %in% c("HTS_SELF"),
         fiscal_year == 2020) %>%
  # distinct(primepartner)
count(standardizeddisaggregate)


df_self <- df %>% 
  filter(fundingagency == "USAID",
         indicator == "HTS_SELF",
         standardizeddisaggregate == "Total Numerator",
         fiscal_year == 2020) %>% 
  group_by(primepartner, fiscal_year) %>% 
  summarise(across(where(is.double), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  reshape_msd(clean = TRUE)

df_self_tot <- df_self %>%
  filter(period_type != "results") %>% 
  spread(period_type, val) %>% 
  mutate(partner = recode(primepartner,
                          "DELOITTE CONSULTING LIMITED" = "Deloitte",
                          "Family Health International" = "EpIC",
                          "JHPIEGO CORPORATION" = "Jhpiego"),
         achv = cumulative / targets,
         sub = glue("{partner}\n{percent(achv, 1)} Achv. [{comma(cumulative,1)}/{comma(targets,1)}]")) %>% 
  select(primepartner, partner, cumulative, sub)

df_self_viz <- df_self %>% 
  filter(period_type == "results") %>% 
  left_join(df_self_tot)


df_self_viz %>% 
  ggplot(aes(period, val, fill = sub)) +
  geom_col() +
  facet_wrap(~sub, ncol = 1) +
  scale_y_continuous(label = comma) +
  scale_fill_manual(values = c(scooter, moody_blue, genoa)) +
  labs(x = NULL, y = NULL,
       title = "EXPANSION IN SELF TESTING OVER FY20",
       subtitle = "FY20 USAID HTS_SELF",
       caption = "Source: FY20Q4i MSD") +
  si_style_ygrid() +
  theme(legend.position = "none")

df_self_viz %>% 
  ggplot(aes(period, val, fill = sub)) +
  geom_blank(aes(y = val * 1.3)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "gray30") +
  geom_text(aes(label = comma(val)), size = 3.5,
            family = "Source Sans Pro", color = "gray30",
            vjust = -1) +
  facet_wrap(~sub, ncol = 1) +
  scale_y_continuous(label = comma) +
  scale_fill_manual(values = c(scooter, moody_blue, genoa)) +
  labs(x = NULL, y = NULL,
       title = "EXPANSION IN SELF TESTING OVER FY20",
       subtitle = "FY20 USAID HTS_SELF",
       caption = "Source: FY20Q4i MSD") +
  si_style_nolines() +
  theme(legend.position = "none",
        axis.text.y = element_blank())

si_save("out/TZA_HTS_SELF.png", width = 5, height = 5.25)
