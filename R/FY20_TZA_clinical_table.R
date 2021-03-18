library(tidyverse)
library(ICPIutilities)
library(glue)
library(gt)
library(RColorBrewer)
library(extrafont)
library(glitr)
library(glamr)


# pal <- brewer.pal(5, "Spectral")[2:5]
pal <- c(old_rose_light, burnt_sienna_light, "#5BB5D5", "#BCBEC0")

df <-  si_path() %>% 
  return_latest("OU_IM") %>% 
  read_rds() %>% 
  filter(operatingunit == "Tanzania")

df_nn <- read_csv("../right_size/Dataout/TX_CURR_NN_Calcs.csv",
                  col_types = c(tx_curr = "d",
                                tx_net_new = "d",
                                tx_curr_lag_site = "d",
                                tx_net_new_adj = "d",
                                tx_net_new_adj_plus = "d",
                                tx_xfer = "d",
                                flag_loneobs = "l",
                                flag_multimech_site = "l",
                                flag_end_sitexmech = "l",
                                .default = "c"
                                )) %>% 
  filter(operatingunit == "Tanzania")


df_tza <- df %>% 
  filter(indicator %in% c("TX_CURR", "HTS_TST", "HTS_TST_POS", "TX_NEW"),
         fundingagency != "Dedup",
         standardizeddisaggregate == "Total Numerator"
         ) %>% 
  mutate(fundingagency = str_remove(fundingagency, "HHS/"))

df_agency <- df_tza %>% 
  group_by(fundingagency, indicator, fiscal_year) %>% 
  summarise(across(where(is.double), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  reshape_msd(clean = TRUE) %>% 
  filter(str_detect(period, "FY2"))

df_nn_agg <- df_nn %>% 
  filter(fundingagency != "Dedup",
         str_detect(period, "FY2")) %>% 
  mutate(fundingagency = str_remove(fundingagency, "HHS/")) %>% 
  group_by(fundingagency, period) %>% 
  summarise(value = sum(tx_net_new_adj_plus, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(indicator = "TX_NET_NEW (adjusted)",
         period_type = "results")

df_nn_agg <- df_nn_agg %>% 
  mutate(period = "FY20",
         period_type = "cumulative") %>% 
  group_by(fundingagency, period, period_type, indicator) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(df_nn_agg, .)



# df_nn <- df_agency %>% 
#   filter(indicator == "TX_CURR" & 
#          (str_detect(period, "4") | period_type == "targets")) %>% 
#   arrange(fundingagency, period) %>% 
#   group_by(fundingagency) %>% 
#   mutate(indicator = "TX_NET_NEW",
#          val = case_when(period_type == "targets" ~ val - lag(val))) %>% 
#   ungroup() %>% 
#   filter(period_type == "targets",
#          !is.na(val))
  


df_agency <- df_agency %>%
  bind_rows(df_nn_agg) %>% 
  mutate(pd = case_when(period_type == "results" ~ str_replace(period, "Q", "\nQ"),
                        period_type == "targets" ~ paste(period, "Targets", sep = "\n"),
                        period_type == "cumulative" ~  paste(period, "Total", sep = "\n")),
         fy = str_sub(period, 3,4),
         q = case_when(period_type == "results" ~ str_sub(period, -1),
                       period_type == "cumulative" ~ "5",
                       period_type == "targets" ~ "6"),
         order = as.numeric(fy) + (as.numeric(q)/100),
         pd = fct_reorder(pd, order),
         fundingagency = factor(fundingagency, c("USAID", "CDC", "DOD")),
         indicator = factor(indicator, c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_NET_NEW (adjusted)"))) %>% 
  select(-c(period, period_type, fy, q, order))


tbl <- df_agency %>%
  arrange(fundingagency, indicator, pd) %>% 
  pivot_wider(names_from = pd, values_from = value) %>% 
  mutate( `FY20\nAchieved` = `FY20\nTotal` / `FY20\nTargets`,
          `FY21\nAchieved` = `FY21\nTotal` / `FY21\nTargets`) %>% 
  select(-`FY21\nTotal`) %>% 
  relocate(`FY20\nAchieved`, .after = `FY20\nTargets`) %>% 
  gt(rowname_col = "indicator",
     groupname_col = "fundingagency")

tbl <- tbl %>% 
  tab_header(title = "Agency Performance") %>% 
  tab_options(table.font.names = "Source Sans Pro") %>% 
  cols_label(fundingagency = "",
             indicator = "") 

tbl <- tbl %>% 
  fmt_percent(vars(`FY20\nAchieved`, `FY21\nAchieved`), decimals = 0) %>% 
  fmt_number(vars(`FY20\nQ1`, `FY20\nQ2`, `FY20\nQ3`, `FY20\nQ4`, `FY20\nTotal`, `FY20\nTargets`,
                  `FY21\nQ1`, `FY21\nTargets`), decimals = 0) 

tbl <- tbl %>% 
  tab_style(
    style = cell_borders(
      sides = "right",
      weight = px(1.5),
    ),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    )) %>% 
  tab_style(style = cell_fill(color = pal[4]),
            locations = cells_body(
              columns = vars(`FY20\nAchieved`),
              rows = `FY20\nAchieved` >= 1.1)) %>%
  tab_style(style = cell_fill(color = pal[3]),
            locations = cells_body(
              columns = vars(`FY20\nAchieved`),
              rows = `FY20\nAchieved` >= .9)) %>% 
  tab_style(style = cell_fill(color = pal[2]),
            locations = cells_body(
              columns = vars(`FY20\nAchieved`),
              rows = `FY20\nAchieved` < .9)) %>% 
  tab_style(style = cell_fill(color = pal[1]),
            locations = cells_body(
              columns = vars(`FY20\nAchieved`),
              rows = `FY20\nAchieved` < .75)) %>% 
  tab_style(style = cell_fill(color = pal[4]),
            locations = cells_body(
              columns = vars(`FY21\nAchieved`),
              rows = `FY21\nAchieved` >= .35)) %>% 
  tab_style(style = cell_fill(color = pal[3]),
            locations = cells_body(
              columns = vars(`FY21\nAchieved`),
              rows = `FY21\nAchieved` >= .15)) %>% 
  tab_style(style = cell_fill(color = pal[2]),
            locations = cells_body(
              columns = vars(`FY21\nAchieved`),
              rows = `FY21\nAchieved` < .15))

tbl <- tbl %>% 
  tab_style(
    style = cell_borders(
      sides = "bottom", 
      color = "#FFFFFF",
      weight = px(1.5),
    ),
    locations = cells_body(
      columns = vars(`FY20\nAchieved`),
      rows = everything()
    )) %>% 
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_row_groups(groups = everything()))

tbl %>% 
  cols_width(
    vars(indicator) ~ px(180),
    everything() ~ px(80)
  ) %>%
  fmt_missing(columns = everything(), missing_text = "-")  %>% 
  tab_source_note(
    md("*TX_NET_NEW is adjusted to account for site tranfers <br>Source: FY21Q1i MSD*"))



#USAID only 
tbl <- df_agency %>%
  filter(fundingagency == "USAID") %>% 
  arrange(fundingagency, indicator, pd) %>% 
  pivot_wider(names_from = pd, values_from = val) %>% 
  mutate( `FY20\nAchieved` = `FY20\nTotal` / `FY20\nTargets`) %>% 
  relocate(`FY20\nAchieved`, .after = `FY20\nTargets`) %>% 
  gt(rowname_col = "indicator",
     groupname_col = "fundingagency")

tbl <- tbl %>% 
  tab_header(title = "Agency Performance") %>% 
  tab_options(table.font.names = "Source Sans Pro") %>% 
  cols_label(fundingagency = "",
             indicator = "") 

tbl <- tbl %>% 
  fmt_percent(vars(`FY20\nAchieved`), decimals = 0) %>% 
  fmt_number(vars(`FY20\nQ1`, `FY20\nQ2`, `FY20\nQ3`, `FY20\nQ4`, `FY20\nTotal`, `FY20\nTargets`,
                  `FY21\nTargets`), decimals = 0) 

tbl <- tbl %>% 
  tab_style(
    style = cell_borders(
      sides = "right",
      weight = px(1.5),
    ),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    )) %>% 
  # tab_style(style = cell_fill(color = pal[4]),
  #           locations = cells_body(
  #             columns = vars(`FY20\nAchieved`),
  #             rows = `FY20\nAchieved` >= 1.1)) %>% 
  tab_style(style = cell_fill(color = pal[3]),
            locations = cells_body(
              columns = vars(`FY20\nAchieved`),
              rows = `FY20\nAchieved` >= .9)) %>% 
  tab_style(style = cell_fill(color = pal[2]),
            locations = cells_body(
              columns = vars(`FY20\nAchieved`),
              rows = `FY20\nAchieved` < .9)) %>% 
  tab_style(style = cell_fill(color = pal[1]),
            locations = cells_body(
              columns = vars(`FY20\nAchieved`),
              rows = `FY20\nAchieved` < .75))

tbl <- tbl %>% 
  tab_style(
    style = cell_borders(
      sides = "bottom", 
      color = "#FFFFFF",
      weight = px(1.5),
    ),
    locations = cells_body(
      columns = vars(`FY20\nAchieved`),
      rows = everything()
    )) %>% 
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_row_groups(groups = everything()))

tbl %>% 
  cols_width(
    vars(indicator) ~ px(180),
    everything() ~ px(80)
  ) %>%
  fmt_missing(columns = everything(), missing_text = "-")  %>% 
  tab_source_note(
    md("*TX_NET_NEW is adjusted to account for site tranfers <br>Source: FY20Q4i MSD*"))


  


df_tza <- df_tza %>% 
  rename_official() 

df_ptnr <- df_tza %>%
  filter(fundingagency == "USAID") %>% 
  mutate(primepartner = recode(primepartner,
                          "BAYLOR COLLEGE OF MEDICINE CH ILDREN FOUNDATION TANZANIA" = "Baylor",
                          "DELOITTE CONSULTING LIMITED" = "Deloitte",
                          "Elizabeth Glaser Pediatric Aids Foundation" = "EGPAF",
                          "Elizabeth Glaser Pediatric AIDS Foundation" = "EGPAF",
                          "JHPIEGO CORPORATION" = "Jhpiego",
                          "Family Health International" = "EpIC (Sauti Q1)",
                          "Koninklijke Nederlandse Centrale Vereniging tot Bestrijding der Tuberculose (KNCV)" = "KNVC",
                          "TANZANIA HEALTH PROMOTION SUPP ORT (THPS)" = "THPS",
                          "JSI Research And Training Institute, INC." = "JSI",
                          "ASSOCIATION OF PRIVATE HEALTH FACILITIES IN TANZANIA" = "Tohara Salama"),
         primepartner = ifelse(mech_name == "Sauti za Watanzania", "EpIC (Sauti Q1)", primepartner)) %>% 
  group_by(fundingagency, primepartner, indicator, fiscal_year) %>% 
  summarise(across(where(is.double), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  reshape_msd(clean = TRUE) %>% 
  filter(str_detect(period, "FY2"))


df_nn_agg_ptnr <- df_nn %>% 
  filter(fundingagency != "Dedup",
         str_detect(period, "FY2")) %>% 
  mutate(fundingagency = str_remove(fundingagency, "HHS/")) %>% 
  mutate(primepartner = recode(primepartner,
                               "BAYLOR COLLEGE OF MEDICINE CH ILDREN FOUNDATION TANZANIA" = "Baylor",
                               "DELOITTE CONSULTING LIMITED" = "Deloitte",
                               "Elizabeth Glaser Pediatric Aids Foundation" = "EGPAF",
                               "Elizabeth Glaser Pediatric AIDS Foundation" = "EGPAF",
                               "JHPIEGO CORPORATION" = "Jhpiego",
                               "Family Health International" = "EpIC (Sauti Q1)",
                               "Koninklijke Nederlandse Centrale Vereniging tot Bestrijding der Tuberculose (KNCV)" = "KNVC",
                               "TANZANIA HEALTH PROMOTION SUPP ORT (THPS)" = "THPS",
                               "JSI Research And Training Institute, INC." = "JSI",
                               "ASSOCIATION OF PRIVATE HEALTH FACILITIES IN TANZANIA" = "Tohara Salama"),
         primepartner = ifelse(mech_name == "Sauti za Watanzania", "EpIC (Sauti Q1)", primepartner)) %>%
  group_by(fundingagency, primepartner, period) %>% 
  summarise(val = sum(tx_net_new_adj_plus, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(indicator = "TX_NET_NEW (adjusted)",
         period_type = "results")


df_nn_agg_ptnr <- df_nn_agg_ptnr %>% 
  mutate(period = "FY20",
         period_type = "cumulative") %>% 
  group_by(fundingagency, primepartner, period, period_type, indicator) %>% 
  summarise(across(c(val), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(df_nn_agg_ptnr, .)


df_ptnr <- df_ptnr %>%
  bind_rows(df_nn_agg_ptnr) %>% 
  mutate(pd = case_when(period_type == "results" ~ str_replace(period, "Q", "\nQ"),
                        period_type == "targets" ~ paste(period, "Targets", sep = "\n"),
                        period_type == "cumulative" ~  paste(period, "Total", sep = "\n")),
         fy = str_sub(period, 3,4),
         q = case_when(period_type == "results" ~ str_sub(period, -1),
                       period_type == "cumulative" ~ "5",
                       period_type == "targets" ~ "6"),
         order = as.numeric(fy) + (as.numeric(q)/100),
         pd = fct_reorder(pd, order),
         # fundingagency = factor(fundingagency, c("USAID", "CDC", "DOD")),
         indicator = factor(indicator, c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_NET_NEW (adjusted)"))) %>% 
  select(-c(period, period_type, fy, q, order))


df_ptnr_grp <- df_ptnr %>% 
  # filter(!primepartner %in% c("Baylor College of Medicine", "KNVC", "NACOPHA", "THPS", "Tohara Salama"))
  filter(primepartner %in% c("Baylor", "Deloitte", "EGPAF", "EpIC (Sauti Q1)"))
  # filter(primepartner %in% c("EpiC", "JSI"))

tbl_ptnr <- df_ptnr_grp %>% 
  select(-fundingagency) %>% 
  complete(pd, nesting(primepartner, indicator)) %>% 
  arrange(primepartner, indicator, pd) %>%
  pivot_wider(names_from = pd, values_from = val) %>% 
  mutate(`FY20\nAchieved` = `FY20\nTotal` / `FY20\nTargets`) %>% 
  relocate(`FY20\nAchieved`, .after = `FY20\nTargets`) %>% 
  gt(rowname_col = "indicator",
     groupname_col = "primepartner")

tbl_ptnr <- tbl_ptnr %>% 
  tab_header(title = "USAID Partner Performance") %>% 
  tab_options(table.font.names = "Source Sans Pro") %>% 
  cols_label(primepartner = "",
             indicator = "") 

tbl_ptnr <- tbl_ptnr %>% 
  fmt_percent(vars(`FY20\nAchieved`), decimals = 0) %>% 
  fmt_number(vars(`FY20\nQ1`, `FY20\nQ2`, `FY20\nQ3`, `FY20\nQ4`, `FY20\nTotal`, `FY20\nTargets`,
                  `FY21\nTargets`), decimals = 0) 

tbl_ptnr <- tbl_ptnr %>% 
  fmt_missing(columns = everything(),
              missing_text = "")

tbl_ptnr <- tbl_ptnr %>% 
  tab_style(
    style = cell_borders(
      sides = "right",
      weight = px(1.5),
    ),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    )) %>% 
  tab_style(style = cell_fill(color = "white"),
            locations = cells_body(
              columns = vars(`FY20\nAchieved`),
              rows = `FY20\nAchieved` == "")) %>%
  tab_style(style = cell_fill(color = pal[3]),
            locations = cells_body(
              columns = vars(`FY20\nAchieved`),
              rows = `FY20\nAchieved` >= .9)) %>% 
  tab_style(style = cell_fill(color = pal[2]),
            locations = cells_body(
              columns = vars(`FY20\nAchieved`),
              rows = `FY20\nAchieved` < .9)) %>% 
  tab_style(style = cell_fill(color = pal[1]),
            locations = cells_body(
              columns = vars(`FY20\nAchieved`),
              rows = `FY20\nAchieved` < .75)) %>% 
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_row_groups(groups = everything()))




tbl_ptnr <- tbl_ptnr %>% 
  cols_width(
    vars(indicator) ~ px(180),
    everything() ~ px(80)
  ) %>% 
  tab_source_note(
    md("*TX_NET_NEW is adjusted to account for site tranfers; EpIC took over for Sauti after Q1, results & targets combined <br>Source: FY20Q4i MSD*"))


gt::row
gtsave(tbl_ptnr, "out/TZA_PerformanceTable_USAID-Partners.png")

