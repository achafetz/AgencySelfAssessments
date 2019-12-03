## PROJECT:  AGENCY SELF ASSESSMENT
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  clean up MMD graphic
## DATE:     2019-12-03
## UPDATED: 

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(readxl)
library(scales)
library(extrafont)
library(gridExtra)


# IMPORT DATA -------------------------------------------------------------

#MMD data from A.Amoah (2019-12-02)
  df_mmd <- read_excel("data/MMS _AT request 11.7.2019.xlsx")


# MUNGE -------------------------------------------------------------------

  df_mmd_share <- df_mmd %>% 
    mutate(MMS_u3 = MMS_1 + MMS_2) %>% 
    select(-MMS_1, -MMS_2) %>% 
    gather(mmd_type, val, -Month, -TX_CURR) %>% 
    rename(month = Month) %>% 
    mutate(share = val/TX_CURR,
           month = factor(month, c("June", "July", "August", "September")),
           mmd_type = recode(mmd_type, "MMS_u3" = "<3 months", "MMS_3" = "3-5 months", "MMS_6" = "6+ months"),
           lab_type = ifelse(month == "June", mmd_type, as.character(NA)),
           lab_start = ifelse(month == "June", percent(share, 1), NA),
           lab_end = ifelse(month == "September", percent(share, 1), NA)
           ) %>% 
    arrange(mmd_type, month)
  

# VIZ ---------------------------------------------------------------------

  df_mmd_share %>% 
    ggplot(aes(month, share, group = mmd_type, color = mmd_type)) +
    geom_hline(yintercept = 0)+
    geom_line(size = 1.1) +
    geom_point(size = 5) +
    geom_text(aes(label = lab_type), family = "Gill Sans MT", fontface = "bold", size = 4, vjust = -1, na.rm = TRUE) +
    geom_text(aes(label = lab_start), family = "Gill Sans MT", size = 4, hjust = 1.5, na.rm = TRUE) +
    geom_text(aes(label = lab_end), family = "Gill Sans MT", size = 4, hjust = -1, na.rm = TRUE) +
    scale_y_continuous(labels = percent_format(1)) +
    scale_color_manual(values = c("#335B8E", "#26456a", "#739bcc")) +
    expand_limits(y = .62) +
    labs(x = NULL, y = NULL,
         title = "USAID Monthly Trends in MMD",
         subtitle =  "share of current on Treatment",
         caption = "Note: USAID, all Tiers  Source: TZA Monthly Reporting Portal") +
    theme_minimal() +
    theme(plot.caption = element_text(color = "gray30", size = 10),
          text = element_text(family = "Gill Sans MT"),
          axis.text.y = element_blank(),
          legend.position = "none")

  ggsave("out/TZA_mmd_share2.png", dpi = 300, width = 4.1, height = 4.6)  
  