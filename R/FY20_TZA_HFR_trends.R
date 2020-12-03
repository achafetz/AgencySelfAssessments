## PROJECT:  AGENCY SELF ASSESSMENT
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  HFR insights
## DATE:     2020-11-24
## UPDATED:  2020-11-30


# DEPEDENCIES -------------------------------------------------------------

  library(tidyverse)
  library(Wavelength)
  library(glitr)
  library(extrafont)
  library(scales)
  library(COVIDutilities)
  library(glue)
  library(svglite)


# GLOBAL VARIABLES --------------------------------------------------------

  path <- "../Wavelength/out/fy20_archive/HFR_Tableau_SQLview_FY20.csv"

  who_pandemic <- who_pandemic() %>% pull(date)

# IMPORT ------------------------------------------------------------------

  #read in FY20 periods
    df_hfr <- hfr_read(path)


# MUNGE -------------------------------------------------------------------

  #keep only TZA
    df_hfr <- df_hfr %>% 
      filter(operatingunit == "Tanzania")
    
  #keep only site where we expect reporting
    df_hfr <- df_hfr %>%
      filter(expect_reporting == TRUE)

  #id if there is reporting at the site (any disagg)
    df_hfr <- df_hfr %>% 
      mutate(has_reporting = !is.na(val))
    
  #convert to logical
    df_hfr <- df_hfr %>% 
      mutate(expect_reporting = as.logical(expect_reporting))
    
  #MMD
    # df_hfr %>% 
    #   filter(indicator == "TX_CURR") %>% 
    #   select(-val, -has_reporting) %>%
    #   mutate(indicator == "TX_MMD") %>% 
    #   glimpse()
  

# AGGREGATE ---------------------------------------------------------------

  #site totals (agg age/sex)
    df_hfr_site <- df_hfr %>% 
      group_by(date, indicator, mech_code, orgunituid, otherdisaggregate) %>% 
      summarise(hfr_results = sum(val, na.rm = TRUE),
                mer_targets = sum(mer_targets, na.rm = TRUE),
                has_reporting = max(has_reporting, na.rm = TRUE),
                expect_reporting = max(expect_reporting, na.rm = TRUE)) %>% 
      ungroup()


  #indicator x period totals
    df_hfr_agg <- df_hfr_site %>% 
      group_by(date, indicator, otherdisaggregate) %>% 
      summarise(across(c(hfr_results, has_reporting, expect_reporting), sum, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(completeness = has_reporting / expect_reporting)
  
  #mech x indicaticator x period totals
    df_hfr_agg_mech <- df_hfr_site %>% 
      group_by(date, mech_code, indicator, otherdisaggregate) %>% 
      summarise(across(c(hfr_results, has_reporting, expect_reporting), sum, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(completeness = has_reporting / expect_reporting)





sites <- df_hfr_agg %>% 
  filter(indicator == "HTS_TST") %>% 
  distinct(expect_reporting) %>% 
  pull()

df_hfr_agg %>% 
  filter(indicator == "HTS_TST") %>% 
  ggplot(aes(date, hfr_results)) +
  geom_vline(xintercept = who_pandemic) +
  geom_area(size = 1.1, alpha = .2, color = scooter, fill = scooter) +
  geom_point(aes(y = -300), size = 6, color = old_rose, alpha = .1) +
  geom_text(aes(y = -300, label = percent(completeness, 1)),
            size = 2, family = "Source Sans Pro", fontface = "bold",
            # color = "white",
            color = old_rose,
            vjust = .2) +
  expand_limits(y = 0) +
  scale_y_continuous(label = comma) +
  scale_x_date(expand = c(.01, .01)) +
  labs(x = NULL, y = NULL, 
       title = "IDENTIFICATION OF NEW POSITIVES FALLS AFTER PANDEMIC DECLARED",
       subtitle = "Tanzania | HTS_TST",
       caption = glue("Expected reporting across {comma(sites, 1)} org units x mechanisms\nSource: FY20 HFR")) + 
  si_style()


sites <- df_hfr_agg %>% 
  filter(indicator == "HTS_TST_POS") %>% 
  distinct(expect_reporting) %>% 
  pull()

df_hfr_agg %>% 
  filter(indicator == "HTS_TST_POS") %>% 
  ggplot(aes(date, hfr_results)) +
  geom_vline(xintercept = who_pandemic) +
  geom_area(size = 1.1, alpha = .2, color = scooter, fill = scooter) +
  geom_point(aes(y = -300), size = 6, color = old_rose, alpha = .1) +
  geom_text(aes(y = -300, label = percent(completeness, 1)),
            size = 2, family = "Source Sans Pro", fontface = "bold",
            # color = "white",
            color = old_rose,
            vjust = .2) +
  expand_limits(y = 0) +
  scale_y_continuous(label = comma) +
  scale_x_date(expand = c(.01, .01)) +
  labs(x = NULL, y = NULL, 
       title = "IDENTIFICATION OF NEW POSITIVES FALLS AFTER PANDEMIC DECLARED",
       subtitle = "Tanzania | HTS_TST_POS",
       caption = glue("Expected reporting across {comma(sites, 1)} org units x mechanisms\nSource: FY20 HFR")) + 
  si_style()
         

df_hfr_agg %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW")) %>% 
  ggplot(aes(date, hfr_results)) +
  geom_blank(aes(y = 7000)) +
  geom_vline(xintercept = who_pandemic) +
  geom_area(size = 1.1, alpha = .2, color = scooter, fill = scooter) +
  geom_point(aes(y = -300), size = 6, color = old_rose, alpha = .1) +
  geom_text(aes(y = -300, label = percent(completeness, 1)),
            size = 2, family = "Source Sans Pro", fontface = "bold",
            # color = "white",
            color = old_rose,
            vjust = .2) +
  expand_limits(y = 0) +
  facet_wrap(~ indicator, scales = "free_y") +
  scale_y_continuous(label = comma) +
  scale_x_date(expand = c(.01, .01)) +
  labs(x = NULL, y = NULL, 
       title = "IDENTIFICATION OF NEW POSITIVES FALLS AFTER PANDEMIC DECLARED",
       # subtitle = "Tanzania | HTS_TST",
       caption = glue("Expected reporting across {comma(sites, 1)} org units x mechanisms\nSource: FY20 HFR")) + 
  si_style()


si_save("out/TZA_HFR_trends.png")

df_hfr_agg %>% 
  filter(indicator %in% c("HTS_TST_POS", "HTS_TST")) %>% 
  select(date, indicator, hfr_results) %>% 
  spread(indicator, hfr_results) %>% 
  mutate(positivity = HTS_TST_POS/HTS_TST) %>% 
  ggplot(aes(date, positivity)) +
  geom_vline(xintercept = who_pandemic) +
  geom_area(size = 1.1, alpha = .2, color = scooter, fill = scooter) +
  # geom_point(aes(y = -300), size = 6, color = old_rose, alpha = .1) +
  # geom_text(aes(y = -300, label = percent(completeness, 1)),
  #           size = 2, family = "Source Sans Pro", fontface = "bold",
  #           # color = "white",
  #           color = old_rose,
  #           vjust = .2) +
  scale_y_continuous(label = percent) +
  scale_x_date(expand = c(.01, .01)) +
  labs(x = NULL, y = NULL, 
       title = "IDENTIFICATION OF NEW POSITIVES FALLS AFTER PANDEMIC DECLARED",
       subtitle = "Tanzania | HTS_TST_POS",
       caption = glue("Expected reporting across {comma(sites, 1)} org units x mechanisms\nSource: FY20 HFR")) + 
  si_style()    



df_hfr_agg %>% 
  filter(indicator == "HTS_TST") %>% 
  ggplot(aes(date, hfr_results)) +
  geom_vline(xintercept = who_pandemic) +
  geom_area(size = 1.1, alpha = .2, color = scooter, fill = scooter) +
  geom_point(aes(y = -300), size = 6, color = old_rose, alpha = .1) +
  geom_text(aes(y = -300, label = percent(completeness, 1)),
            size = 2, family = "Source Sans Pro", fontface = "bold",
            # color = "white",
            color = old_rose,
            vjust = .2) +
  expand_limits(y = 0) +
  scale_y_continuous(label = comma) +
  scale_x_date(expand = c(.01, .01)) +
  facet_wrap(~ mech_code, scales = "free_y") +
  labs(x = NULL, y = NULL, 
       title = "IDENTIFICATION OF NEW POSITIVES FALLS AFTER PANDEMIC DECLARED",
       subtitle = "Tanzania | HTS_TST",
       caption = glue("Expected reporting across {comma(sites, 1)} org units x mechanisms\nSource: FY20 HFR")) + 
  si_style()



sites <- df_hfr_agg %>% 
  filter(indicator == "TX_CURR") %>% 
  distinct(expect_reporting) %>% 
  pull()

df_hfr_agg %>% 
  filter(indicator == "TX_CURR",
         date != "2020-08-31") %>% 
  ggplot(aes(date, hfr_results)) +
  geom_vline(xintercept = who_pandemic, color = "gray30", linetype = "dashed") +
  geom_area(size = 1.1, alpha = .2, color = scooter, fill = scooter) +
  geom_point(aes(y = -20000), size = 6, color = old_rose, alpha = .1) +
  geom_text(aes(y = -20000, label = percent(completeness, 1)),
            size = 2, family = "Source Sans Pro", fontface = "bold",
            # color = "white",
            color = old_rose,
            vjust = .2) +
  expand_limits(y = 0) +
  scale_y_continuous(label = comma) +
  scale_x_date(expand = c(.02, .02)) +
  labs(x = NULL, y = "TX_CURR", 
       title = toupper("Sustained current clients and increased number of new \n clients on ART throughout the pandemic"),
       # subtitle = "Tanzania | TX_CURR",
       caption = glue("Expected reporting across {comma(sites, 1)} sites x mechanisms\nSource: FY20 HFR [USAID]")) + 
  si_style()

# si_save("out/TZA_HFR_TX_CURR_trends.png")
ggsave("out/TZA_HFR_TX_CURR_trends.svg", dpi = 300, height = 4, width = 6.75)


# LARGEST SITES -----------------------------------------------------------

df_hfr_site_share <- df_hfr_site %>%
  arrange(date, indicator, desc(mer_targets)) %>% 
  group_by(date, indicator) %>% 
  mutate(cumsum = cumsum(mer_targets),
         cumshare = cumsum/sum(mer_targets)) %>% 
  ungroup()

keep <- df_hfr_site_share %>% 
  filter(indicator == "HTS_TST_POS",
         date == min(date)) %>% 
  slice_max(order_by = mer_targets, n = 25) %>% 
  select(mech_code, orgunituid)

df_hfr_site_lrg_hts <- df_hfr_site_share %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS")) %>% 
  inner_join(keep)
