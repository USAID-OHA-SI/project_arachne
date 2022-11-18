# PROJECT:  project_arachne
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  initial basic figures for POARTs
# REF ID:   4c86a407 
# LICENSE:  MIT
# DATE:     2022-11-17
# UPDATED: 
# tags: achievement, positivity trend, trends in treatment, iit trend

# DEPENDENCIES -----------------------------------------------------------------
  
  library(tidyverse)
  library(extrafont)
  library(gagglr)
  library(glue)
  library(scales)
  library(stringr)
  library(patchwork)
  library(ggtext)

# GLOBAL VARIABLES -------------------------------------------------------------
  
  ref_id <- "4c86a407"

# IMPORT -----------------------------------------------------------------------
  
  path <- "MER_Structured_Datasets_OU_IM_FY20-23"
  
  df <- si_path() %>%
    return_latest(path) %>%
    read_msd()
    
  df_filt <- df %>%
    filter(country %in% c("South Sudan"), 
           fiscal_year %in% c("2021", "2022"))
  
  # metadata
  si_path() %>% 
    return_latest(path) %>%
    get_metadata()

# MUNGE ------------------------------------------------------------------------
  
  peds <- c("<01", "01-04", "05-09", "10-14")
  adults <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
              "50-54", "55-59", "60-64", "65+")
  
  # Q: how do we want to characterize any "unknown age" data?
  # (standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered == "Unknown Age")
  
  # TX_CURR achievement 
  # re-work code to show quarterly OU level achievement using SIEI achievement 
  # color scheme, see slide 43, 147 in data viz library
  
  # df_tx <- df_filt %>%
  #   filter(
  #     indicator == "TX_CURR",
  #     funding_agency != "Dedup",
  #     (standardizeddisaggregate == "Age/Sex/HIVStatus" & is.na(ageasentered)) | 
  #       (standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered %in% peds) |
  #       (standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered %in% adults) |
  #       (standardizeddisaggregate == "Total Numerator")) %>%
  #   mutate(type = case_when(
  #     standardizeddisaggregate == "Total Numerator" & 
  #       is.na(ageasentered)~ "Total", 
  #     standardizeddisaggregate == "Age/Sex/HIVStatus" & 
  #       ageasentered %in% peds ~ "Children", 
  #     standardizeddisaggregate == "Age/Sex/HIVStatus" & 
  #       ageasentered %in% adults ~ "Adults", 
  #     standardizeddisaggregate == "Age/Sex/HIVStatus" & 
  #       ageasentered == "Unknown Age" ~ "Unknown")) %>%
  #   group_by(country, fiscal_year, indicator, type) %>%
  #   summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE),
  #             .groups = "drop") %>%
  #   reshape_msd("quarters") %>%
  #   select(-results_cumulative) %>%
  #   arrange(type, period)
  # 
  # df_tx <- df_tx %>%
  #   mutate(
  #     growth_rate_req =
  #       case_when(period == metadata$curr_pd ~
  #                   ((targets / results)^(1 / (4 - metadata$curr_qtr))) - 1)) %>%
  #   group_by(type) %>%
  #   fill(growth_rate_req, .direction = "updown") %>%
  #   mutate(
  #     growth_rate = (results / lag(results, order_by = period)) - 1,
  #     growth_rate = na_if(growth_rate, Inf)) %>%
  #   ungroup() %>%
  #   mutate(
  #     geo_gr_lab = case_when(
  #       is.infinite(growth_rate_req) ~ glue("{type}"),
  #       growth_rate_req < 0 ~ glue("{type}\nTarget achieved"),
  #       growth_rate_req < .1 ~ glue("{type} ({percent(growth_rate_req, 1)})"),
  #       TRUE ~ glue("{type} ({percent(growth_rate_req, 1)})")),
  #     gr_req_lab = case_when(fiscal_year == metadata$curr_fy ~ percent(growth_rate_req, 1)),
  #     gr_label_position = results + 10000,
  #     disp_targets = case_when(fiscal_year == metadata$curr_fy ~ targets))
  # 
  # df_focus <- df_tx %>%
  #   filter(period == metadata$curr_pd, 
  #          growth_rate_req == max(growth_rate_req)) %>%
  #   select(type) %>%
  #   pull()
  # 
  # pd <- df_tx %>%
  #   filter(period == metadata$curr_pd) %>%
  #   select(period) %>%
  #   distinct() %>%
  #   pull()
  # 
  # df_tx %>%
  #   filter(fiscal_year == metadata$curr_fy) %>%
  #   mutate(period_type = glue("{period}_{type}")) %>%
  #   ggplot(aes(x = period)) +
  #   geom_col(aes(y = results, fill = type),
  #            alpha = .55,
  #            position = position_dodge(width = -.75)) +
  #   geom_hline(aes(yintercept = disp_targets, color = type), 
  #              linetype = "dashed") +
  #   facet_wrap(~fct_reorder2(geo_gr_lab, fiscal_year, targets), scales = "free_y", 
  #              nrow = 1) +
  #   scale_y_continuous(limits = c(0, 65000), 
  #                      label = label_number(scale_cut = cut_short_scale())) +
  #   scale_x_discrete(breaks = pd) +
  #   scale_fill_manual(values = c("Adults" = usaid_medblue,
  #                                "Children" = usaid_lightblue,
  #                                "Total" = usaid_blue)) +
  #   scale_color_manual(values = c("Adults" = usaid_medblue,
  #                                 "Children" = usaid_lightblue,
  #                                 "Total" = usaid_blue)) +
  #   coord_flip() +
  #   labs(
  #     x = NULL, y = NULL,
  #     title = glue("{df_focus} Require Greatest Effort To Meet Quarterly Treatment Achievement Targets") %>% toupper(),
  #     subtitle = "Growth rate required (%) to meet TX_CURR targets for the current period",
  #     caption = glue("{metadata$caption} | US Agency for International Development")) +
  #   si_style_ygrid() +
  #   theme(
  #     legend.position = "none",
  #     panel.spacing = unit(.5, "picas"),
  #     axis.text.x = element_text(size = 8))
  
  
  # TX_CURR focus of efforts
  
  df_tx <- df_filt %>%
    filter(
      indicator == "TX_CURR",
      funding_agency != "Dedup",
      (standardizeddisaggregate == "Age/Sex/HIVStatus" & is.na(ageasentered)) | 
      (standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered %in% peds) |
      (standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered %in% adults) |
      (standardizeddisaggregate == "Total Numerator")) %>%
    mutate(type = case_when(
      standardizeddisaggregate == "Total Numerator" & 
        is.na(ageasentered)~ "Total", 
      standardizeddisaggregate == "Age/Sex/HIVStatus" & 
      ageasentered %in% peds ~ "Children", 
      standardizeddisaggregate == "Age/Sex/HIVStatus" & 
      ageasentered %in% adults ~ "Adults", 
      standardizeddisaggregate == "Age/Sex/HIVStatus" & 
        ageasentered == "Unknown Age" ~ "Unknown")) %>%
    group_by(country, fiscal_year, indicator, type) %>%
    summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE),
              .groups = "drop") %>%
    reshape_msd("quarters") %>%
    select(-results_cumulative) %>%
    arrange(type, period)
  
  df_tx <- df_tx %>%
    mutate(
      growth_rate_req =
        case_when(period == metadata$curr_pd ~
                    ((targets / results)^(1 / (4 - metadata$curr_qtr))) - 1)) %>%
    group_by(type) %>%
    fill(growth_rate_req, .direction = "updown") %>%
    mutate(
      growth_rate = (results / lag(results, order_by = period)) - 1,
      growth_rate = na_if(growth_rate, Inf)) %>%
    ungroup() %>%
    mutate(
      geo_gr_lab = case_when(
        is.infinite(growth_rate_req) ~ glue("{type}"),
        growth_rate_req < 0 ~ glue("{type}\nTarget achieved"),
        growth_rate_req < .1 ~ glue("{type} ({percent(growth_rate_req, 1)})"),
        TRUE ~ glue("{type} ({percent(growth_rate_req, 1)})")),
      gr_req_lab = case_when(fiscal_year == metadata$curr_fy ~ percent(growth_rate_req, 1)),
      gr_label_position = results + 10000,
      disp_targets = case_when(fiscal_year == metadata$curr_fy ~ targets))
  
  df_focus <- df_tx %>%
    filter(period == metadata$curr_pd, 
           growth_rate_req == max(growth_rate_req)) %>%
    select(type) %>%
    pull()
  
  pd <- df_tx %>%
    filter(period == metadata$curr_pd) %>%
    select(period) %>%
    distinct() %>%
    pull()
  
  df_tx %>%
    filter(fiscal_year == metadata$curr_fy) %>%
    mutate(period_type = glue("{period}_{type}")) %>%
    ggplot(aes(x = period)) +
    geom_col(aes(y = results, fill = type),
             alpha = .55,
             position = position_dodge(width = -.75)) +
    geom_hline(aes(yintercept = disp_targets, color = type), 
               linetype = "dashed") +
    facet_wrap(~fct_reorder2(geo_gr_lab, fiscal_year, targets), scales = "free_y", 
               nrow = 1) +
    scale_y_continuous(limits = c(0, 65000), 
                       label = label_number(scale_cut = cut_short_scale())) +
    scale_x_discrete(breaks = pd) +
    scale_fill_manual(values = c("Adults" = usaid_medblue,
                                 "Children" = usaid_lightblue,
                                 "Total" = usaid_blue)) +
    scale_color_manual(values = c("Adults" = usaid_medblue,
                                 "Children" = usaid_lightblue,
                                 "Total" = usaid_blue)) +
    coord_flip() +
     labs(
       x = NULL, y = NULL,
       title = glue("{df_focus} Require Greatest Effort To Meet Quarterly Treatment Achievement Targets") %>% toupper(),
       subtitle = "Growth rate required (%) to meet TX_CURR targets for the current period",
       caption = glue("{metadata$caption} | US Agency for International Development")) +
    si_style_ygrid() +
    theme(
      legend.position = "none",
      panel.spacing = unit(.5, "picas"),
      axis.text.x = element_text(size = 8))
  
  
  # Quarterly Trend in IIT and RTT at OU level
  
  df_iit <- df_filt %>% 
    filter(indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_NET_NEW", 
                            "TX_RTT"), 
           funding_agency != "Dedup") %>%
    pluck_totals() %>%
    group_by(country, fiscal_year, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>%
    rowwise() %>%
    # % of newly enrolled ART patients who experienced 
    #     interruptions in treatment during reporting period
    # iit = Number of ART patients who were on or initiated treatment during 
    #      the reporting period and then had no clinical contact since their 
    #     last expected contact /
    #      sum of (tx_curr_lag1 + # of patients newly enrolled into treatment)
    mutate(tx_curr_lag1 = tx_curr - tx_net_new,
           iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE),
           share_rtt_curr = tx_rtt/tx_curr,
           share_rtt_label = percent(share_rtt_curr),
           tx_curr_label = comma(round(tx_curr)), 
           iit_label = percent(iit), 
           fiscal_year = str_extract(period, "FY[0-2]{2}"),
           period_num = str_extract(period, "Q[1-4]"), 
           period_num = as.numeric(
             str_extract(period_num, "[1-4]"))) %>% 
    ungroup() %>%
    pivot_longer(c(iit, share_rtt_curr), 
                 names_to = "indicator") %>% 
    mutate(fill_color = ifelse(indicator == "iit", old_rose, denim),
           indicator = glue("{toupper(indicator)}"))
  
  df_iit %>%
    ggplot(aes(period, value, fill = fct_rev(indicator))) +
    geom_col(alpha = .55,
             position = position_dodge(width = -.55)) +
     facet_wrap(~fiscal_year, scales = "free_x", 
                ncol = 1) +
    scale_fill_manual(values = c("IIT" = old_rose,
                                 "SHARE_RTT_CURR" = denim)) +
    scale_y_continuous(limits = c(0, .1),
                       label = percent_format(1),
                       oob = oob_squish, 
                       breaks = c(0, 0.05, 0.1)) +
    scale_x_discrete(breaks = NULL) +
    si_style() +
    theme(panel.spacing = unit(.5, "line"),
          legend.position = "none",
          plot.title = element_markdown(),
          strip.text = element_markdown()) +
  labs(x = NULL, y = NULL, fill = NULL,
  title = glue("OU QUARTERLY <span style='color:{old_rose}'>IIT</span> AND 
               <span style='color:{denim}'>SHARE OF RTT</span>"),
          caption = glue(" Note: IIT = TX_ML / TX_CURR - TX_NET_NEW + TX_NEW; ITT capped to 10%
                                 Share of RTT = TX_RTT/TX_CURR
                         {metadata$caption} | US Agency for International Development"))

  

  
  
  