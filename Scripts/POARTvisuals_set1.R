# PROJECT:  project_arachne
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  initial basic figures for POARTs
# REF ID:   4c86a407 
# LICENSE:  MIT
# DATE:     2022-11-28
# UPDATED: 
# tags: achievement + adjusted achievement, positivity trends, trends in treatment, iit trend

# DEPENDENCIES -----------------------------------------------------------------
  
  library(tidyverse)
  library(extrafont)
  library(gagglr)
  library(glue)
  library(scales)
  library(stringr)
  library(patchwork)
  library(ggtext)
  library(tidytext)
  library(cowplot)
  library(janitor)

# GLOBAL VARIABLES -------------------------------------------------------------
  
  ref_id <- "4c86a407"

# IMPORT -----------------------------------------------------------------------
  
  path <- "MER_Structured_Datasets_OU_IM_FY20-23_20221114_v1_1"
  path_psnu <- "Genie_PSNU_IM_South_Sudan_Daily_2022-11-15"
  
  df <- si_path() %>%
    return_latest(path) %>%
    read_msd()
  
  df_psnu <- si_path() %>%
    return_latest(path_psnu) %>%
    read_msd()
    
  df_filt <- df %>%
    filter(country == "South Sudan", 
           fiscal_year %in% c("2021", "2022"))
  
  df_filt_psnu <- df_psnu %>%
    filter(fiscal_year %in% c("2021", "2022"))
  
  # # metadata
  # si_path() %>% 
  #   return_latest(path) %>%
  #   get_metadata()
  
# functions --------------------------------------------------------------------
  
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
        geom_col(aes(y = disp_targets), na.rm = TRUE, fill = suva_grey, alpha = .2) +
        geom_col(na.rm = TRUE, alpha = .7) +
        # want to only show dashed line for current FY
        # geom_errorbar(aes(ymin = targets, ymax = targets), 
        #               linetype = "dashed", width = .95, na.rm = TRUE) +
        geom_text(aes(label = results_lab, y = results), 
                  family = "Source Sans Pro", color = usaid_darkgrey, size = 9 / .pt,
                  vjust = -.5, na.rm = TRUE) +
        geom_text(aes(label = gr_lab, y = gr_label_position),
                  family = "Source Sans Pro", color = "white", size = 9 / .pt,
                  vjust = -.5, na.rm = TRUE) +
        scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
        scale_x_discrete(breaks = unique(ou_df_new$period)[grep("Q(4)", unique(ou_df_new$period))]) +
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
        geom_col(aes(y = disp_targets), na.rm = TRUE, fill = suva_grey, alpha = .2) +
        geom_col(na.rm = TRUE, alpha = .7) +
        # want to only show dashed line for current FY
        # geom_errorbar(aes(ymin = targets, ymax = targets), 
        #               linetype = "dashed", width = .95, na.rm = TRUE) +
        geom_text(aes(label = results_lab, y = results), 
                  family = "Source Sans Pro", color = usaid_darkgrey, size = 9 / .pt,
                  vjust = -.5, na.rm = TRUE) +
        geom_text(aes(label = gr_lab, y = gr_label_position),
                  family = "Source Sans Pro", color = "white", size = 9 / .pt,
                  vjust = -.5, na.rm = TRUE) +
        scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
        scale_x_discrete(breaks = unique(ou_df_new$period)[grep("Q(4)", unique(ou_df_new$period))]) +
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

# VIZ --------------------------------------------------------------------------
  
  # Q: how do we want to characterize any "unknown age" data?
  # (standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered == "Unknown Age")
  
  # OU level -------------------------------------------------------------------
    # Performance quarterly growth trend ---------------------------------------
  
  ou_progress_qtr(.path = path, 
                  .df = df, 
                  .indicator = "TX_NEW", 
                  .ou = "South Sudan")
  
  si_save(paste0(metadata$curr_pd, "TX_CURR_quarterly.png"),
          path = "Images",
          scale = 0.8)
   
  ou_progress_qtr(.path = path, 
                  .df = df, 
                  .indicator = "TX_NEW", 
                  .ou = "South Sudan")
   
   si_save(paste0(metadata$curr_pd, "TX_NEW_quarterly.png"),
           path = "Images",
           scale = 0.8)
   
    # TX_NEW/NET_NEW quarterly trend -------------------------------------------
   
   # df_nn <- ou_df %>% 
   #   filter(indicator %in% c("TX_CURR", "TX_NEW", "TX_NET_NEW"),
   #          fiscal_year == "2022", 
   #          funding_agency == "USAID") %>%
   #   pluck_totals() %>% 
   #   group_by(country, indicator, fiscal_year) %>% 
   #   summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
   #   reshape_msd(include_type = FALSE) %>% 
   #   pivot_wider(names_from = indicator,
   #               names_glue = "{tolower(indicator)}") %>%
   #   pivot_longer(c(tx_net_new, tx_new), 
   #                names_to = "indicator") %>% 
   #   mutate(fill_color = ifelse(indicator == "tx_net_new", scooter, scooter_light),
   #          indicator = glue("{toupper(indicator)}"),
   #          share = value / tx_curr)
   # 
   # df_nn %>%
   #   filter(tx_curr != 0) %>%
   #   ggplot(aes(period, value, fill = fct_rev(indicator))) +
   #   geom_col(alpha = .75,
   #            position = position_dodge(width = .5)) +
   #   geom_hline(yintercept = 0) +
   #   scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
   #   scale_fill_manual(values = c("TX_NEW" = scooter,
   #                                "TX_NET_NEW" = scooter_light)) +
   #   labs(x = NULL, y = NULL, fill = NULL,
   #        title = glue("<span style='color:{scooter_light}'>TX_NET_NEW</span> DROPPED IN Q3, REBOUNDED BY Q4"),
   #        subtitle = glue("TX_NEW stayed relatively constant"),
   #        caption = glue("{metadata$caption} | US Agency for International Development")) +
   #   si_style_ygrid() +
   #   theme(panel.spacing = unit(.5, "line"),
   #         legend.position = "none",
   #         plot.title = element_markdown(),
   #         strip.text = element_markdown())
   # 
   # si_save(glue("Images/{metadata$curr_pd}_SSD_tx-new-nn.png"),
   #         scale = 1.1)  
   
    # Quarterly Trend in IIT ---------------------------------
  
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
    filter(fiscal_year == "FY22") %>%
    ggplot(aes(period, value, fill = fct_rev(indicator))) +
    geom_col(alpha = .55,
             position = position_dodge(width = -.55)) +
     # facet_wrap(~fiscal_year, scales = "free_x", 
     #            ncol = 1) +
    scale_fill_manual(values = c("IIT" = old_rose,
                                 "SHARE_RTT_CURR" = denim)) +
    scale_y_continuous(limits = c(0, .1),
                       label = percent_format(1),
                       oob = oob_squish, 
                       breaks = c(0, 0.05, 0.1)) +
    scale_x_discrete(breaks = unique(ou_df_curr$period)[grep("Q(4)", unique(ou_df_curr$period))]) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          legend.position = "none",
          plot.title = element_markdown(),
          strip.text = element_markdown()) +
  labs(x = NULL, y = NULL, fill = NULL,
  # note that IIT cap will vary by OU, likely need to make this something 
  # the user can easily specify
  title = glue("OU QUARTERLY <span style='color:{old_rose}'>IIT</span> AND 
               <span style='color:{denim}'>SHARE OF RTT</span>"),
          caption = glue(" Note: IIT = TX_ML / TX_CURR - TX_NET_NEW + TX_NEW; ITT capped to 10%
                                 Share of RTT = TX_RTT/TX_CURR
                         {metadata$caption} | US Agency for International Development"))
  
  si_save(paste0(metadata$curr_pd, "IIT_RTT_quarterly.png"),
          path = "Images",
          scale = 0.8)
 -----------------------------------------------
  
  # df_iit_rtt <- ou_df %>% 
  #   filter(funding_agency == "USAID", 
  #          indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_NET_NEW", "TX_RTT"), 
  #          fiscal_year == "2022") %>%
  #   pluck_totals() %>%
  #   group_by(fiscal_year, country, indicator) %>% 
  #   summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
  #   reshape_msd(include_type = FALSE) %>% 
  #   pivot_wider(names_from = "indicator",
  #               names_glue = "{tolower(indicator)}")
  # 
  # df_iit_rtt <- df_iit_rtt %>%
  #   mutate(tx_curr_lag1 = tx_curr - tx_net_new) %>% 
  #   rowwise() %>% 
  #   mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE), 
  #          iit_label = percent(iit), 
  #          qtr_label = glue("{period} ({iit_label})")) %>% 
  #   ungroup()
  # 
  # vct_itt_cntry <- ou_df %>% 
  #   filter(funding_agency == "USAID", 
  #          indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_NET_NEW", "TX_RTT"), 
  #          fiscal_year == "2022") %>%
  #   pluck_totals() %>%
  #   group_by(fiscal_year, country, indicator) %>% 
  #   summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
  #   reshape_msd(include_type = FALSE) %>% 
  #   pivot_wider(names_from = "indicator",
  #               names_glue = "{tolower(indicator)}") %>% 
  #   filter(period == metadata$curr_pd) %>% 
  #   mutate(tx_curr_lag1 = tx_curr - tx_net_new) %>% 
  #   rowwise() %>% 
  #   mutate(
  #     iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
  #   ungroup() %>% 
  #   pull() %>% 
  #   percent()
  # 
  # df_iit_rtt %>%
  #   mutate(fiscal_year = str_sub(period, end = 4)) %>% 
  #   filter(tx_curr_lag1 != 0) %>%
  #   ggplot(aes(qtr_label, iit)) +
  #   geom_smooth(aes(weight = tx_curr_lag1, group = country),
  #               method = "loess",
  #               formula = "y ~ x", se = FALSE, na.rm = TRUE,
  #               linewidth = 1.5, color = old_rose_light) +
  #   scale_y_continuous(limits = c(0,.15),
  #                      label = NULL,
  #                      oob = oob_squish) +
  #   coord_cartesian(clip = "off") +
  #   labs(x = NULL, y = NULL,
  #        size = "Site TX_CURR (1 period prior)",
  #        title = glue("USAID MAINTAINED A RELATIVELY CONSTANT {vct_itt_cntry} IIT OVER FY22"),
  #        subtitle = glue("A 1% increase Occurred in Q3"),
  #        caption = glue("Note: IIT = TX_ML / TX_CURR - TX_NET_NEW + TX_NEW; ITT capped to 15%
  #                       {metadata$caption} | US Agency for International Development")) +
  #   si_style_ygrid() +
  #   theme(panel.spacing = unit(.5, "line"),
  #         axis.text = element_text(size = 8),
  #         plot.subtitle = element_markdown())
  # 
  # si_save(glue("Images/{metadata$curr_pd}_SSD_OU_iit.png"),
  #         scale = 0.8)  
  
  si_save(paste0(metadata$curr_pd, "TX_NEW_quarterly.png"),
          path = "Images",
          scale = 0.8)
  
  
  # USAID only -----------------------------------------------------------------
  
    # TX-CURR Performance quarterly growth trend 
  ou_progress_qtr(.path = path, 
                  .df = df, 
                  .indicator = "TX_CURR", 
                  .ou = "South Sudan", 
                  .funding_agency = "USAID")
  
  si_save(paste0(metadata$curr_pd, "TX_CURR_quarterly_USAID.png"),
          path = "Images",
          scale = 0.8)
  
    # TX_NEW Performance quarterly growth trend
  ou_progress_qtr(.path = path, 
                  .df = df, 
                  .indicator = "TX_NEW", 
                  .ou = "South Sudan", 
                  .funding_agency = "USAID")
  
  si_save(paste0(metadata$curr_pd, "TX_NEW_quarterly_USAID.png"),
          path = "Images",
          scale = 0.8)
                  
    # Quarterly Trend in IIT
  
    # Peds ---------------------------------------------------------------------
  
      # TX-CURR Performance quarterly growth trend 
      # TX_NEW Performance quarterly growth trend
      # Quarterly Trend in IIT
  
    # AGYW (15-24) -------------------------------------------------------------
    
      # TX-CURR Performance quarterly growth trend 
      # TX_NEW Performance quarterly growth trend
      # Quarterly Trend in IIT
      
  
  