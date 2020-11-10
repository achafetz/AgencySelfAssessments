library(tidyverse)
library(Wavelength)
library(glitr)
library(extrafont)
library(scales)
library(glue)
library(ggtext)

path <- "C:/Users/achafetz/Documents/GitHub/Wavelength/out/joint/HFR_Tableau_SQLview.csv"

df_hfr <- hfr_read(path)

df_hfr <- df_hfr %>% 
  filter(operatingunit == "Tanzania")

df_hfr_lng <- df_hfr %>% 
  rename(hfr_results = val) %>% 
  gather(type, value, hfr_results, mer_results, mer_targets, na.rm = TRUE) %>% 
  mutate(value = as.double(value))

#remove time componets for aggregation
df_hfr_lng <- df_hfr_lng %>% 
  select(-date, -fy) %>% 
  mutate(hfr_pd = as.character(hfr_pd))

#create a period value for non-TX_CURR indicators
df_pd_sum <- df_hfr_lng %>% 
  filter(!indicator %in% c("TX_CURR", "TX_MMD"),
         type == "hfr_results") %>% 
  group_by_if(is.character) %>% 
  summarise(value = sum(value, na.rm = TRUE)) %>% 
  ungroup()

#create a period value for TX_CURR indicators
df_pd_max <- df_hfr_lng %>% 
  filter(indicator %in% c("TX_CURR", "TX_MMD") |
           type %in% c("mer_results", "mer_targets")) %>% 
  group_by_if(is.character) %>% 
  summarise_if(is.numeric, max, na.rm = TRUE) %>% 
  ungroup()

#join period dataset & respread
df_pd <- bind_rows(df_pd_sum, df_pd_max)  %>% 
  # spread(type, value, fill = 0)   #treat 0's as NA -> convert all NAs to 0
  spread(type, value)   #treat 0's as NA -> convert all NAs to 0

rm(df_hfr_lng, df_pd_max, df_pd_sum)

#reporting sites
  rep_sites <- df_hfr %>% 
    filter(!is.na(val)) %>% 
    distinct(orgunituid) %>% 
    pull()
  
  df_pd <- df_pd %>% 
    mutate(had_rptng = orgunituid %in% rep_sites) 

#clean up partner names
  df_pd <- df_pd %>% 
    mutate(primepartner = case_when(primepartner == "JSI Research And Training Institute, INC." ~ "JSI",
                                    primepartner == "Elizabeth Glaser Pediatric Aids Foundation" ~ "EGPAF",
                                    primepartner == "DELOITTE CONSULTING LIMITED" ~ "Deloitte",
                                    primepartner == "BAYLOR COLLEGE OF MEDICINE CH ILDREN FOUNDATION TANZANIA" ~ "Baylor",
                                    primepartner == "JHPIEGO CORPORATION" ~ "Jhpiego",
                                    primepartner == "ASSOCIATION OF PRIVATE HEALTH FACILITIES IN TANZANIA" ~ "USAID Tohara Salama",
                                    primepartner == "Family Health International" ~ "FHI360",
                                    primepartner == "NACOPHA" ~ "NACOPHA",
                                    primepartner == "TANZANIA HEALTH PROMOTION SUPP ORT (THPS)" ~ "THPS"
    ))
  

df_pd_agg <- df_pd %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS","TX_NEW"),
         had_rptng == TRUE
         ) %>%
  group_by(hfr_pd, primepartner, indicator) %>% 
  summarise(across(where(is.double), sum, na.rm = TRUE)) %>% 
  # summarise(across(where(is.double), .fns = list(sum = ~ sum(., na.rm = TRUE), max = ~ max(., na.rm = TRUE)), .names = "{.col}_{.fn}")) %>% 
  ungroup() %>% 
  # glimpse()
  mutate(hfr_pd = as.integer(hfr_pd)
         # hfr_results = ifelse(indicator == "TX_CURR", hfr_results_max, hfr_results_sum)
         )
  # select(hfr_pd, primepartner, indicator, hfr_results, mer_results = mer_results_sum, mer_targets = mer_targets_sum)
  

df_pd_agg <- df_pd_agg %>% 
  filter(hfr_pd == min(hfr_pd)) %>% 
  mutate(hfr_pd = 10,
         hfr_results = mer_results) %>% 
  bind_rows(df_pd_agg) %>% 
  group_by(primepartner, indicator) %>% 
  mutate(hfr_cum = cumsum(hfr_results)) %>% 
  ungroup() %>% 
  mutate(value_type = ifelse(hfr_pd == 10, "MER", "HFR")) %>% #hfr_cum = ifelse(indicator == "TX_CURR", hfr_results, hfr_cum),
  arrange(primepartner, indicator, hfr_pd)

df_pd_agg <- df_pd_agg %>% 
  complete(indicator, nesting(hfr_pd, primepartner))

df_pd_agg %>%
  filter(!primepartner %in% c("NACOPHA", "THPS", "Jhpiego", "FHI360")) %>% 
  ggplot(aes(hfr_pd, hfr_cum, group = indicator)) +
  geom_blank(aes(y = 15)) +
  geom_ribbon(aes(ymin = mer_targets *0.9, ymax = mer_targets * 1.1), fill = grey20k, alpha = .6) +
  geom_hline(aes(yintercept = mer_targets), linetype = "dashed") +
  geom_path(size = .9) +
  geom_point(aes(fill = value_type), shape = 21, size = 4, stroke = 1.4, color = "gray30") +
  expand_limits(y = 0) +
  facet_wrap(~ indicator + fct_reorder(primepartner, hfr_cum, na.rm = TRUE, .desc = TRUE), 
             scales = "free_y", ncol = 4, labeller = label_wrap_gen(multi_line = FALSE)) +
  scale_y_continuous(labels = comma_format(1)) +
  scale_x_continuous(breaks = c(10, 11, 12, 13),
                     labels = c("FY20Q3", "Jul", "Aug", "Sep")) +
  scale_fill_manual(values = c("gray30", "white")) +
  labs(x = NULL, y = NULL,
       title = "DID PARTNERS MEET THEIR FY20 TARGETS?",
       subtitle = "targets limited to sites where HFR was reported",
       caption = "Source: HFR [2020-11-05]") +
  si_style() +
  theme(legend.position = "none",
        panel.spacing = unit(.5, "lines"))



#EGPAF

df_pd_egpaf <- df_pd %>% 
  filter(indicator %in% c("HTS_TST_POS", "TX_NEW"),
         primepartner == "EGPAF"
         # had_rptng == TRUE
  ) %>%
  group_by(hfr_pd, snu1, primepartner, indicator) %>% 
  summarise(across(where(is.double), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(hfr_pd = as.integer(hfr_pd))


df_pd_egpaf <- df_pd_egpaf %>% 
  filter(hfr_pd == min(hfr_pd)) %>% 
  mutate(hfr_pd = 10,
         hfr_results = mer_results) %>% 
  bind_rows(df_pd_egpaf) %>% 
  group_by(primepartner, snu1, indicator) %>% 
  mutate(hfr_cum = cumsum(hfr_results)) %>% 
  ungroup() %>% 
  mutate(value_type = ifelse(hfr_pd == 10, "MER", "HFR")) %>% #hfr_cum = ifelse(indicator == "TX_CURR", hfr_results, hfr_cum),
  arrange(primepartner, snu1, indicator, hfr_pd)


df_pd_egpaf %>%
  ggplot(aes(hfr_pd, hfr_cum, group = snu1)) +
  geom_blank(aes(y = 15)) +
  geom_ribbon(aes(ymin = mer_targets *0.9, ymax = mer_targets * 1.1), fill = grey20k, alpha = .6) +
  geom_hline(aes(yintercept = mer_targets), linetype = "dashed") +
  geom_path(size = .9) +
  geom_point(aes(fill = value_type), shape = 21, size = 4, stroke = 1.4, color = "gray30") +
  expand_limits(y = 0) +
  facet_grid(fct_reorder(snu1, hfr_cum, na.rm = TRUE, .desc = TRUE) ~ indicator, 
             scales = "free_y") +
  scale_y_continuous(labels = comma_format(1)) +
  scale_x_continuous(breaks = c(10, 11, 12, 13),
                     labels = c("FY20Q3", "Jul", "Aug", "Sep")) +
  scale_fill_manual(values = c("gray30", "white")) +
  labs(x = NULL, y = NULL,
       title = "DID PARTNERS MEET THEIR FY20 TARGETS?",
       subtitle = "targets limited to sites where HFR was reported",
       caption = "Source: HFR [2020-11-05]") +
  si_style() +
  theme(legend.position = "none",
        panel.spacing = unit(.5, "lines"))




df_pd_egpaf <- df_pd %>% 
  filter(indicator %in% c("HTS_TST_POS"),
         primepartner == "EGPAF"
         # had_rptng == TRUE
  ) %>%
  group_by(hfr_pd, snu1,psnu, primepartner, indicator) %>% 
  summarise(across(where(is.double), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(hfr_pd = as.integer(hfr_pd))


df_pd_egpaf <- df_pd_egpaf %>% 
  filter(hfr_pd == min(hfr_pd)) %>% 
  mutate(hfr_pd = 10,
         hfr_results = mer_results) %>% 
  bind_rows(df_pd_egpaf) %>% 
  group_by(primepartner, snu1, psnu, indicator) %>% 
  mutate(hfr_cum = cumsum(hfr_results)) %>% 
  ungroup() %>% 
  mutate(value_type = ifelse(hfr_pd == 10, "MER", "HFR")) %>% #hfr_cum = ifelse(indicator == "TX_CURR", hfr_results, hfr_cum),
  arrange(primepartner, snu1, psnu, indicator, hfr_pd) %>%
  mutate(order = ifelse(hfr_pd == max(hfr_pd), hfr_cum, 0),
         achieved = case_when(hfr_pd == max(hfr_pd) & hfr_cum >= mer_targets ~ 1,
                              TRUE ~ 0)) %>%
  group_by(psnu) %>% 
  mutate(achieved = max(achieved)) %>% 
  ungroup() %>% 
  mutate(color = ifelse(achieved == 0, "#c43d4d","#202020"),
         label = glue("<span style='color:{color}'>{snu1}/{psnu}</span>"),
         label = fct_reorder(label, mer_targets, .desc = TRUE)
         )

df_pd_egpaf %>% 
  ggplot(aes(hfr_pd, hfr_cum, group = indicator)) +
  geom_blank(aes(y = 15)) +
  geom_ribbon(aes(ymin = mer_targets *0.9, ymax = mer_targets * 1.1), fill = grey20k, alpha = .6) +
  geom_hline(aes(yintercept = mer_targets), linetype = "dashed") +
  geom_path(size = .9) +
  geom_point(aes(fill = value_type), shape = 21, size = 4, stroke = 1.4, color = "gray30") +
  expand_limits(y = 0) +
  facet_wrap(~label, scales = "free_y") +
  scale_y_continuous(labels = comma_format(1)) +
  scale_x_continuous(breaks = c(10, 11, 12, 13),
                     labels = c("FY20Q3", "Jul", "Aug", "Sep")) +
  scale_fill_manual(values = c("gray30", "white")) +
  labs(x = NULL, y = NULL,
       title = "WHERE IS EGPAF MEETING THEIR FY20 TARGETS?",
       subtitle = "targets limited to sites where HFR was reported",
       caption = "Source: HFR [2020-11-05]") +
  si_style() +
  theme(legend.position = "none",
        panel.spacing = unit(.5, "lines"),
        strip.text = element_markdown())

