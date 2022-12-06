# PROJECT:  project_arachne
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  functions for the figures used for POART EDA
# REF ID:   cc2c736a 
# LICENSE:  MIT
# DATE:     2022-12-1
# UPDATED: 
# tags: arachne, achievement + adjusted achievement, 
#       positivity trends, trends in treatment, iit trend

# DEPENDENCIES ----------------------------------------------------------------

  library(tidyverse)
  library(extrafont)
  library(gagglr)
  library(glue)
  library(scales)
  library(stringr)
  library(janitor)

# Functions --------------------------------------------------------------------

# Calculate PLHIV in a given OU, returns a data frame containing OU level 
# TX_CURR_SUBNAT and an associated label, PLHIV and an associated label, 
# gap between TX_CURR_SUBNAT and PLHIV both as a number (gap) and a percent (gap_pct)

calculate_plhiv <- function(.nat_subnat_df) {
  
  df_nat <- .nat_subnat_df %>%
    filter(indicator %in% c("TX_CURR_SUBNAT", "PLHIV"),
           standardizeddisaggregate == "Age/Sex/HIVStatus") %>%
    group_by(fiscal_year, indicator) %>% 
    summarise(across(targets, sum, na.rm = TRUE)) %>% 
    pivot_wider(names_from = indicator, values_from = targets) %>% 
    mutate(gap = TX_CURR_SUBNAT/PLHIV, 
           gap_pct = percent(gap),
           need_label = comma(PLHIV - TX_CURR_SUBNAT), 
           PLHIV_label = comma(PLHIV), 
           TX_CURR_SUBNAT_label = comma(TX_CURR_SUBNAT))
  
  return(df_nat)
  
}

# Summarizes cumulative annual and quarterly progress on selected indicators
# in a faceted plot with two rows

ou_progress_summary <- function(.path, .df, .indicator, 
                                .ou, .fiscal_year, .funding_agency = NULL, ...){
  
  # all agencies
  if(is.null(.funding_agency)){
    
    df_new <- df %>%
      filter(
        indicator %in% .indicator,
        operatingunit == .ou,
        fiscal_year %in% .fiscal_year, 
        funding_agency != "Dedup") %>%
      pluck_totals() %>%
      group_by(country, indicator, fiscal_year) %>%
      summarise(across(c(starts_with("qtr"), "cumulative"),
                       sum,na.rm = TRUE), .groups = "keep") %>%
      reshape_msd(include_type = TRUE) %>%
      arrange(period, period_type) %>%
      mutate(period_type = if_else(period_type == "cumulative", 
                                   "Cumulative", "Quarterly"),
             period_num = row_number(period), 
             fiscal_year2 = str_extract(period, "FY[1-2][0-9]"),
             results_lab = case_when(period == metadata$curr_pd |
                                       fiscal_year2 == metadata$curr_fy_lab ~ 
                                       glue("{comma(value)}")), 
             ind_period = str_c(indicator, period, sep = "_")) %>%
      filter(if_else(period_type == "results",
                     period_num == max(period_num) |
                       period_num > max(period_num) - 6 |
                       period_num == max(period_num) - 6,
                     period_num == period_num))
    
    df_new %>%
      ggplot(aes(x = period, fill = fct_rev(indicator))) +
      geom_col(aes(y = value), alpha = .7,
               position = position_dodge(width = .65)) +
      facet_wrap(~period_type, scales = "free_x", ncol = 1) +
      geom_text(aes(label = results_lab, y = value, color = period_type), 
                position = position_dodge(width = 0.75),
                family = "Source Sans Pro", size = 12 / .pt, 
                vjust = -.5, na.rm = TRUE) +
      scale_x_discrete(breaks = unique(df_new$period)[grep("Q(2|4)", unique(df_new$period))]) +
      scale_y_continuous(limits = c(0, max(df_new$value) + 1000), 
                         label = label_number(scale_cut = cut_short_scale())) +
      scale_fill_manual(values = c(usaid_lightgrey, usaid_darkgrey)) +
      scale_color_manual(values = c(usaid_medgrey, usaid_darkgrey)) +
      labs(x = NULL, y = NULL, fill = NULL,
           subtitle = glue("Cumulative and Quarterly progress (Operating Unit)"),
           caption = glue("{metadata$caption} | US Agency for International Development")) +
      si_style_yline() +
      theme(panel.spacing = unit(.5, "line"),
            legend.position = "none",
            plot.title = element_markdown(),
            strip.text = element_markdown())
  }
  
  # just one agency
  else {
    
    df_new <- df %>%
      filter(
        funding_agency == .funding_agency,
        indicator %in% .indicator,
        operatingunit == .ou,
        fiscal_year %in% .fiscal_year) %>%
      pluck_totals() %>%
      group_by(country, indicator, fiscal_year) %>%
      summarise(across(c(starts_with("qtr"), "cumulative"),
                       sum,na.rm = TRUE), .groups = "keep") %>%
      reshape_msd(include_type = TRUE) %>%
      arrange(period, period_type) %>%
      mutate(period_type = if_else(period_type == "cumulative", 
                                   "Cumulative", "Quarterly"),
             period_num = row_number(period), 
             fiscal_year2 = str_extract(period, "FY[1-2][0-9]"),
             results_lab = case_when(period == metadata$curr_pd |
                                       fiscal_year2 == metadata$curr_fy_lab ~ 
                                       glue("{comma(value)}")), 
             ind_period = str_c(indicator, period, sep = "_")) %>%
      filter(if_else(period_type == "results",
                     period_num == max(period_num) |
                       period_num > max(period_num) - 6 |
                       period_num == max(period_num) - 6,
                     period_num == period_num))
    
    df_new %>%
      ggplot(aes(x = period, fill = fct_rev(indicator))) +
      geom_col(aes(y = value), alpha = .7, width = 1,
               position = position_dodge(width = .65)) +
      facet_wrap(~period_type, scales = "free_x", ncol = 1) +
      geom_text(aes(label = results_lab, y = value, color = period_type), 
                position = position_dodge(width = 0.75),
                family = "Source Sans Pro", size = 12 / .pt, 
                vjust = -.5, na.rm = TRUE) +
      scale_x_discrete(breaks = unique(df_new$period)[grep("Q(2|4)", unique(df_new$period))]) +
      scale_y_continuous(limits = c(0, max(df_new$value) + 1000), 
                         label = label_number(scale_cut = cut_short_scale())) +
      scale_fill_manual(values = c(usaid_lightgrey, usaid_darkgrey)) +
      scale_color_manual(values = c(usaid_medgrey, usaid_darkgrey)) +
      labs(x = NULL, y = NULL, fill = NULL,
           subtitle = glue("Cumulative and Quarterly progress (USAID)"),
           caption = glue("{metadata$caption} | US Agency for International Development")) +
      si_style_yline() +
      theme(panel.spacing = unit(.5, "line"),
            legend.position = "none",
            plot.title = element_markdown(),
            strip.text = element_markdown())
    
    
  }
  
}

# Summarizes only quarterly progress on selected indicators

ou_progress_qtr <- function(.path, .df, .indicator, 
                            .ou, .funding_agency = NULL, ...){
  
  # metadata
  si_path() %>% 
    return_latest(.path) %>%
    get_metadata()
  
  peds <- c("<01", "01-04", "05-09", "10-14")
  adults <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
              "50-54", "55-59", "60-64", "65+")
  
  if(is.null(.funding_agency)){
    
    df_new <-  .df %>%
      filter(
        funding_agency != "Dedup",
        operatingunit == .ou,
        indicator == .indicator,
        (standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered %in% peds) |
          (standardizeddisaggregate == "Total Numerator")) %>%
      mutate(type = ifelse(standardizeddisaggregate == "Total Numerator",
                           "Total", "Peds")) %>%
      group_by(fiscal_year, operatingunit, indicator, type) %>%
      summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE),
                .groups = "drop") %>%
      reshape_msd("quarters") %>%
      select(-results_cumulative) %>%
      arrange(type, operatingunit, period)
    
    df_new <- df_new %>%
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
          is.infinite(growth_rate_req) ~ glue("{toupper(operatingunit)}"),
          growth_rate_req < 0 ~ glue("{toupper(operatingunit)}\nTarget achieved"),
          growth_rate_req < .1 ~ glue("{toupper(operatingunit)}\n{percent(growth_rate_req, 1)}"),
          TRUE ~ glue("{toupper(operatingunit)}\n{percent(growth_rate_req, 1)}")),
        gr_lab = case_when(fiscal_year == metadata$curr_fy ~ 
                             glue("{percent(growth_rate, 1)}")),
        gr_lab = stringr::str_replace(gr_lab, "NA", "0"),
        gr_label_position = 1000,
        results_lab =    case_when(fiscal_year == metadata$curr_fy ~ 
                                     glue("{comma(results)}")),
        disp_targets = case_when(fiscal_year == metadata$curr_fy ~ targets), 
        amount_diff = targets - results, 
        pct_change = round_half_up((results - targets)/abs(targets) * 100),0)
    
    # percentage change from q1 to q4
    pct_change_new <- df_new %>%
      filter(type == "Total") %>%
      select(fiscal_year, pct_change) %>%
      filter(pct_change == max(as.numeric(pct_change))) %>%
      pull()
    
    df_new %>%
      filter(type == "Total") %>%
      ggplot(aes(period, results, fill = as.character(period))) +
      geom_col(aes(y = disp_targets), na.rm = TRUE, 
               fill = suva_grey, alpha = .2, width = 1) +
      geom_col(na.rm = TRUE, alpha = .7, width = 1) +
      geom_text(aes(label = results_lab, y = results), 
                family = "Source Sans Pro", color = usaid_darkgrey, size = 9 / .pt,
                vjust = -.5, na.rm = TRUE) +
      geom_text(aes(label = gr_lab, y = gr_label_position),
                family = "Source Sans Pro", color = "white", size = 9 / .pt,
                vjust = -.5, na.rm = TRUE) +
      scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
      scale_x_discrete(breaks = unique(df_new$period)[grep("Q(4)", unique(df_new$period))]) +
      scale_fill_manual(values = c(usaid_lightgrey, usaid_lightgrey, usaid_lightgrey, usaid_lightgrey,
                                   usaid_lightgrey, usaid_lightgrey, usaid_lightgrey, usaid_lightgrey,
                                   usaid_darkgrey, usaid_darkgrey, usaid_darkgrey, usaid_darkgrey)) +
      labs(
        x = NULL, y = NULL,
        subtitle = glue("{.indicator} Quarterly Trend (Operating Unit)"),
        caption = glue("{metadata$caption} | US Agency for International Development")) +
      si_style_ygrid() +
      theme(
        legend.position = "none",
        panel.spacing = unit(.5, "picas"),
        axis.text.x = element_text(size = 8))
    
  }
  
  else {
    
    # there's a better way to do this without re-writing the whole
    # chunk of code but i just haven't thought of it yet 
    # by Panic! At the Disco
    
    df_new <-  df %>%
      filter(
        operatingunit == .ou,
        funding_agency == .funding_agency,
        indicator == .indicator,
        (standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered %in% peds) |
          (standardizeddisaggregate == "Total Numerator")) %>%
      mutate(type = ifelse(standardizeddisaggregate == "Total Numerator",
                           "Total", "Peds")) %>%
      group_by(fiscal_year, operatingunit, indicator, type) %>%
      summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE),
                .groups = "drop") %>%
      reshape_msd("quarters") %>%
      select(-results_cumulative) %>%
      arrange(type, operatingunit, period)
    
    df_new <- df_new %>%
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
          is.infinite(growth_rate_req) ~ glue("{toupper(operatingunit)}"),
          growth_rate_req < 0 ~ glue("{toupper(operatingunit)}\nTarget achieved"),
          growth_rate_req < .1 ~ glue("{toupper(operatingunit)}\n{percent(growth_rate_req, 1)}"),
          TRUE ~ glue("{toupper(operatingunit)}\n{percent(growth_rate_req, 1)}")),
        gr_lab = case_when(fiscal_year == metadata$curr_fy ~ 
                             glue("{percent(growth_rate, 1)}")),
        gr_lab = stringr::str_replace(gr_lab, "NA", "0"),
        gr_label_position = 1000,
        results_lab =    case_when(fiscal_year == metadata$curr_fy ~ 
                                     glue("{comma(results)}")),
        disp_targets = case_when(fiscal_year == metadata$curr_fy ~ targets), 
        amount_diff = targets - results, 
        pct_change = round_half_up((results - targets)/abs(targets) * 100),0)
    
    # percentage change from q1 to q4
    pct_change_new <- df_new %>%
      filter(type == "Total") %>%
      select(fiscal_year, pct_change) %>%
      filter(pct_change == max(as.numeric(pct_change))) %>%
      pull()
    
    df_new %>%
      filter(type == "Total") %>%
      ggplot(aes(period, results, fill = as.character(period))) +
      geom_col(aes(y = disp_targets), na.rm = TRUE, 
               fill = suva_grey, alpha = .2, width = 1) +
      geom_col(na.rm = TRUE, alpha = .7, width = 1) +
      geom_text(aes(label = results_lab, y = results), 
                family = "Source Sans Pro", color = usaid_darkgrey, size = 9 / .pt,
                vjust = -.5, na.rm = TRUE) +
      geom_text(aes(label = gr_lab, y = gr_label_position),
                family = "Source Sans Pro", color = "white", size = 9 / .pt,
                vjust = -.5, na.rm = TRUE) +
      scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
      scale_x_discrete(breaks = unique(df_new$period)[grep("Q(4)", unique(df_new$period))]) +
      scale_fill_manual(values = c(usaid_lightgrey, usaid_lightgrey, usaid_lightgrey, usaid_lightgrey,
                                   usaid_lightgrey, usaid_lightgrey, usaid_lightgrey, usaid_lightgrey,
                                   usaid_darkgrey, usaid_darkgrey, usaid_darkgrey, usaid_darkgrey)) +
      labs(
        x = NULL, y = NULL,
        subtitle = glue("{.indicator} Quarterly Trend (USAID)"),
        caption = glue("{metadata$caption} | US Agency for International Development")) +
      si_style_ygrid() +
      theme(
        legend.position = "none",
        panel.spacing = unit(.5, "picas"),
        axis.text.x = element_text(size = 8))
    
  }
  
}