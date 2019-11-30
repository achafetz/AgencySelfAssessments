## PROJECT:  AGENCY SELF ASSESSMENT
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  review HRH data
## DATE:     2019-11-27
## UPDATED:  

#dependencies
  library(tidyverse)
  library(ICPIutilities)
  library(scales)
  library(extrafont)
  library(gridExtra)

#import
  path <- "../Downloads/PEPFAR-Data-Genie-SiteByIMs-2019-11-29.zip"
  df <- read_msd(path)
  
#select country
  ou_sel <- "Malawi"
  
#subset to HRH and TX_CURR and OU
  df_hrh <- df %>% 
    filter(operatingunit == ou_sel,
           sitetype %in% c("Facility", "Community"),
           fundingagency == "USAID",
           indicator %in% c("TX_CURR", "HRH_CURR"),
           standardizeddisaggregate %in% c("Total Numerator", "CadreCategory"),
           fiscal_year == 2019)
  
#adjust geographic focus
  df_hrh <- df_hrh %>% 
    mutate(snu_lvl = case_when(operatingunit == "Malawi" ~ psnu,
                              TRUE ~ snu1))
  
#aggregate
  df_hrh <- df_hrh %>% 
    group_by(operatingunit, snu_lvl, indicator, standardizeddisaggregate, otherdisaggregate) %>% 
    summarise_at(vars(cumulative), sum, na.rm = TRUE) %>% 
    ungroup()
  
#adjust names
  df_hrh <- df_hrh %>% 
    mutate(snu_lvl = str_remove(snu_lvl, " District"))

#HRH share
  df_hrh_share <- df_hrh %>% 
    filter(standardizeddisaggregate == "Total Numerator") %>% 
    group_by(operatingunit, indicator) %>% 
    mutate(reg_share = cumulative / sum(cumulative, na.rm = TRUE)) %>% 
    ungroup()

  (viz_share <- df_hrh_share %>% 
    select(-cumulative) %>% 
    spread(indicator, reg_share) %>% 
    filter(!is.na(HRH_CURR)) %>% 
    mutate(TX_CURR = ifelse(is.na(TX_CURR), 0, TX_CURR),
           snu_lvl = fct_reorder(snu_lvl, TX_CURR)) %>% 
    gather(indicator, reg_share, HRH_CURR, TX_CURR) %>% 
    arrange(snu_lvl, indicator) %>% 
    mutate(lab = case_when(snu_lvl %in% c("Iringa", "Mulanje") ~ indicator)) %>% 
    ggplot(aes(reg_share, snu_lvl, group = snu_lvl, color = indicator)) +
    geom_vline(xintercept = 0, color = "gray50") +
    geom_path(color = "gray70", size = 1, na.rm = TRUE) +
    geom_point(size = 6.5, na.rm = TRUE) +
    geom_text(aes(label = lab, 
                  hjust = ifelse(indicator == "TX_CURR", -.3, 1.2)), 
              family = "Gill Sans MT", fontface = "bold", 
              size = 3, na.rm = TRUE) +
    scale_x_continuous(labels = percent_format(1), expand = c(0.005, 0.005)) +
    scale_color_manual(values = c("#26456a", "gray60")) +
    labs(x = NULL, y = NULL, 
         subtitle = "Regional Share of HRH/Treatment",
         caption = " ") +
    expand_limits(x = .2) +
    facet_wrap(~ operatingunit) +
    theme_minimal() +
    theme(axis.ticks = element_blank(),
          text = element_text(family = "Gill Sans MT", size = 14),
          strip.text = element_text(color = "white"),
          legend.position = "none"
          ))
  
#HRH Cadre
  tx_order <- df_hrh %>% 
    filter(indicator == "TX_CURR") %>% 
    count(snu_lvl, wt = cumulative) %>% 
    arrange(n) %>% 
    pull(snu_lvl)
  
  if(ou_sel == "Malawi")
    tx_order <- c("Chiradzulu", "Thyolo", tx_order)
  
 df_viz_cadre <- df_hrh %>% 
    filter(standardizeddisaggregate == "CadreCategory") %>% 
    mutate(otherdisaggregate = fct_lump(otherdisaggregate, 3, w = cumulative, other_level = "All Other Cadres")) %>% 
    group_by(snu_lvl, otherdisaggregate) %>% 
    summarise_at(vars(cumulative), sum, na.rm = TRUE) %>% 
    ungroup() %>%
    group_by(snu_lvl) %>% 
    mutate(cadre_share = cumulative/sum(cumulative, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(snu_lvl = factor(snu_lvl, tx_order),
           otherdisaggregate = factor(otherdisaggregate, c("Lay", "Clinical", "Social Service", "All Other Cadres"))
           )
 
 (viz_cadre <- df_viz_cadre %>% 
    ggplot(aes(snu_lvl, cadre_share, fill = otherdisaggregate)) +
    geom_col(na.rm = TRUE) +
    geom_blank(aes(y = 1.2 * cadre_share), na.rm = TRUE) +
    geom_hline(yintercept = 0, color = "gray50") +
    geom_text(aes(label = percent(cadre_share, 1)), hjust = -.2,
              color = "gray30", family = "Gill Sans MT", na.rm = TRUE) +
    scale_y_continuous(labels = percent) +
    coord_flip() +
    scale_fill_manual(values = c("#335B8E", "#739bcc","#739bcc", "#D0DEEF")) +
    facet_wrap(. ~ otherdisaggregate, nrow = 1) +
    labs(x = NULL, y = NULL, 
         subtitle = "Main HRH Cadres",
         caption = "Source: FY19Q4i MSD") +
    theme_minimal() +
    theme(legend.position = "none",
          text = element_text(family = "Gill Sans MT", size = 14),
          axis.text.x = element_text(color = "white"),
          axis.text.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.caption = element_text(color = "gray30", size = 10)))

 viz_out <- grid.arrange(viz_share, viz_cadre, nrow = 1)

 ggsave("out/MWI_HRH_Distro.png", viz_out, dpi = 300,
        height = 5.6, width = 10, units = "in")     
  