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

# GLOBAL VARIABLES -------------------------------------------------------------
  
  ref_id <- "4c86a407"

# IMPORT -----------------------------------------------------------------------
  
  path <- "MER_Structured_Datasets_OU_IM_FY20-23"
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
  
  # TX_CURR achievement (Cascade)
  
  # Figures 2-6: 
  # Overall performance against 95-95-95 goals/remaining gap from 95-95-95 goals 
  # by agency
  # partner
  # SNU1
  # PSNU 
  
  # TX_CURR achievement + adjusted achievement, PSNU level
  # TODO: Add achievement color scheme
  
  # OU, SNU, PSNU 
  
  curr_df <- df_filt_psnu %>% 
    filter(indicator == "TX_CURR", 
           fiscal_year == metadata$curr_fy, 
           funding_agency == "USAID",
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(snu1, psnu, indicator, fiscal_year, funding_agency) %>% 
    summarize(across(matches("targets|cumu"), sum, na.rm = T)) %>% 
    ungroup()
  
  prep_adj <- curr_df %>% 
    group_by(snu1, psnu, indicator) %>% 
    mutate(rslt_capped = ifelse(cumulative > targets, targets, cumulative),
           rslt_gap = cumulative - targets,
           rslt_deficit = ifelse(cumulative < targets, cumulative - targets, NA_real_),
           rslt_surplus = ifelse(cumulative >= targets, cumulative - targets, NA_real_),
           achv = cumulative / targets,
           achv_adj = rslt_capped / targets, 
           snu_over_achv = cumulative > targets) %>% 
    ungroup() %>% 
    arrange(rslt_gap) 

  prep_adj <- 
    prep_adj %>% 
    group_by(snu_over_achv) %>% 
    mutate(rslt_gap_tot = sum(rslt_gap)) %>% 
    ungroup()%>% 
    mutate(snu_gap_sh = rslt_gap / rslt_gap_tot,
           gap_running_sh = cumsum(snu_gap_sh),
           gap_running_target = cumsum(rslt_gap))
  
  # Create SNU level rollups for achv and adjusted achv
  prep_adj <- 
    prep_adj %>% 
    group_by(snu1) %>% 
    mutate(across(c(targets:rslt_surplus), sum, na.rm = T, .names = "{.col}_snu")) %>% 
    ungroup() %>% 
    mutate(achv_snu = cumulative_snu / targets_snu,
           achv_adj_snu = rslt_capped_snu / targets_snu,
           deficit_sh = abs(rslt_deficit_snu)/targets_snu,
           surplus_sh = rslt_surplus_snu / targets_snu,
           psnu_order = fct_reorder(psnu, cumulative),
           facet_label = ifelse(snu_over_achv == TRUE, "Achieved Targets", "Unachieved"), 
           psnu_order2 = reorder_within(psnu, targets, facet_label), 
           achv_colors_snu = case_when(
             achv_adj_snu <= 0.25 ~ old_rose_light,
             (achv_adj_snu > 0.25 & achv_adj_snu < 0.5) ~ burnt_sienna_light,
             (achv_adj_snu > 0.5 & achv_adj_snu < 0.75) ~ scooter_med,
             achv_adj_snu > 0.75  ~ trolley_grey_light), 
           achv_colors_psnu = case_when(
             achv_adj <= 0.25 ~ old_rose_light,
             (achv_adj > 0.25 & achv_adj < 0.5) ~ burnt_sienna_light,
             (achv_adj > 0.5 & achv_adj < 0.75) ~ scooter_med,
             achv_adj > 0.75  ~ trolley_grey_light))

  # Pull global achv and global ach adj
  achv_lab  <- prep_adj %>% 
    pull(achv_snu)
  
  achv_adj_lab <- prep_adj %>% 
    pull(achv_adj_snu)
  
  fill_color_snu <- prep_adj$achv_colors_snu
  
  snu_count <- prep_adj %>% 
    summarise(snu_achv = sum(rslt_surplus_snu > 0, na.rm = T)) %>% 
    mutate(
      sing_plu = if_else(snu_achv == 1, "SNU", "SNUs"), 
      # TODO: make this more spefic for achieve vs exceed, is this 
      #       important to automate?
      achv_exceed_missed = if_else(snu_achv == 1.0,"ACHIEVED OR EXCEEDED", 
                                   "MISSED"))
  
  # Plots
  
  snu_plot <-
    prep_adj %>% 
    ggplot(aes(y = fct_reorder(snu1, achv_snu), fill = fill_color_snu)) +
    # What is target capped benchmark?
    geom_col(aes(x = targets_snu), fill = trolley_grey_light) +
    # What is target capped result?
    geom_col(aes(x = cumulative_snu), fill = fill_color_snu) +
    # Add label for achievement as a percent
    geom_text(aes(x = cumulative_snu, label = percent(achv_snu)), 
                size = 10/.pt, family = "Source Sans Pro") +
    # What is target capped gap/over-achievement?
    # geom_col(aes(x = rslt_gap_snu), fill = fill_color_snu) +
    # Add label for defecit as a percent
    # geom_text(aes(x = rslt_gap_snu, label = percent(defecit_sh)), 
    #            size = 10/.pt, family = "Source Sans Pro") +
    # Add label for surplus as a percent
    # geom_text(aes(x = rslt_gap_snu, label = percent(surplus_sh)), 
    #            size = 10/.pt, family = "Source Sans Pro") +
    scale_x_continuous(labels = scales::label_comma()) +
    si_style_xgrid() +
    coord_cartesian(clip = "off", expand = F) +
    labs(x = NULL, y = NULL, 
     title = glue("{snu_count$snu_achv[1]} {snu_count$sing_plu[1]} {snu_count$achv_exceed_missed[1]} THEIR {metadata$curr_fy} {unique(curr_df$indicator)} TARGETS"))
  
  fill_color_psnu <- prep_adj$achv_colors_psnu
  
  psnu_count <- prep_adj %>% 
    summarise(psnu_achv = sum(rslt_surplus > 0, na.rm = T)) %>% 
    mutate(
      sing_plu = if_else(psnu_achv == 1, 
                         "PSNU", "PSNUs"),
      # TODO: make this more specific for achieve vs exceed, is this 
      #       important to automate? may want to if we automate the 
      #       choice of the achievement or the gaps figures
      achv_exceed_missed = if_else(psnu_achv == 1.0,"ACHIEVED OR EXCEEDED", 
                                   "MISSED"))
  psnu_plot <-  
    prep_adj %>% 
    ggplot(aes(y = psnu_order)) +
    # What are the targets per psnu?
    geom_col(aes(x = targets), fill = trolley_grey_light) +
    # What are cumulative results?
    geom_col(aes(x = cumulative), fill = fill_color_psnu) +
    # Add label for achivement as a percent
    geom_text(aes(x = cumulative, label = percent(achv)), 
              size = 10/.pt, family = "Source Sans Pro") +
    # What is target capped gap/over-achievement?
    # geom_col(aes(x = rslt_gap), fill = fill_color_psnu) +
    # Add label for gap as a percent
    # geom_text(aes(x = rslt_gap, label = percent(achv)), 
    #            size = 10/.pt, family = "Source Sans Pro") +
    scale_x_continuous(labels = scales::label_comma()) +
    facet_wrap(~facet_label, scales = "free_y", ncol = 1) +
    si_style_xgrid(facet_space = 0.25) +
    coord_cartesian(clip = "off", expand = F) +
    labs(x = NULL, y = NULL, 
         title = glue("{psnu_count$psnu_achv[1]} {psnu_count$sing_plu[1]} {psnu_count$achv_exceed_missed[1]} THEIR {metadata$curr_fy} {unique(curr_df$indicator)} TARGETS"), 
         caption = 
           glue("Adjusted Achievement = Capped result* / Target
       
       *The value of the Capped result depends on whether or not targets were exceeded.
        If they were, the Capped result is equal to the Target, otherwise, it is equal to the 
        Cumulative result"))

  cowplot::plot_grid(snu_plot, psnu_plot,
                     ncol = 1, align = "hv", axis = "bt", 
                     rel_heights = c(2, 4))
 
  # TX_CURR focus of efforts ---------------------------------------------------
  
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

  

  
  
  