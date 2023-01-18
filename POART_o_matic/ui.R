# PROJECT:  project_arachne
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  UI settings for POART-l app
# REF ID:   092f65b1
# LICENSE:  MIT
# DATE:     2023-01-18
# UPDATED:

# theme <- bslib::bs_theme(
#   bg = "#FFFFFF",
#   fg = glitr::usaid_blue, 
#   primary = glitr::usaid_blue,
#   secondary = glitr::burnt_sienna, 
#   info = glitr::denim,
#   success = glitr::genoa, 
#   warning = glitr::golden_sand, 
#   danger = glitr::old_rose,
#   base_font = "Source Sans Pro")

# Setup user interface for app -------------------------------------------------
dashboardPage(
  # Theme
  #theme = theme,
  # Header ---------
  dashboardHeader(disable = TRUE),
  # Sidebar ----
  shinydashboardPlus::dashboardSidebar(
    glue("POART-al: Exploratory analysis for POART presentations"),
    width = 400,
    # Menu --------
    sidebarMenu(
      id = "select_options",
      # Select an OU ----
      pickerInput(
        inputId = "select_ou",
        label = "Select an operational unit (OU):",
        choices = unique(glamr::pepfar_country_list$operatingunit),
        options = list(size = 8)
      ),
      # select an indicator ----
      pickerInput(
        inputId = "select_indicator",
        label = "Select an indicator:",
        choices = c("TX_CURR", "TX_NEW"),
        options = list(size = 8),
        multiple = FALSE
      ),
      # select a year ----
      # pickerInput(
      #   inputId = "select_year",
      #   label = "Select a year(s):",
      #   choices = c("2023", "2022", "2021", "2020"),
      #   options = list(size = 8),
      #   multiple = TRUE),
      # select an agency ----
      pickerInput(
        inputId = "select_agency",
        label = "Select an agency/agencies:",
        choices = c("All", "USAID", "CDC", "DOD", "Peace Corps"),
        options = list(size = 8),
        multiple = FALSE
      ),
      # select a type ----
      pickerInput(
        inputId = "select_type",
        label = "Select a type of age data:",
        choices = c("Total", "Adults (15+)", "Children (<15)"),
        options = list(size = 8),
        multiple = FALSE
      ),
      # select a visual
      # select a type ----
      pickerInput(
        inputId = "select_template",
        label = "Select a visual to explore from the options in the tabs:",
        choices = c("Cumulative Achivement"),
        options = list(size = 8),
        multiple = FALSE
      ),
      # Figure Preview button
      actionButton(
        inputId = "figure_preview",
        label = "Preview Figure"
      ),
      # Generate slide button
      actionButton(
        inputId = "generate_slide",
        label = "Generate Slide"
      )
    ) # close sidebar Menu
  ), # close sidebar


  # Body ---------
  dashboardBody(
    tags$head(
      tags$style(HTML('.main-header .logo {
        font-family: "Source Sans Pro"; font-weight: bold;font-size: 24px;}
        .skin-blue .main-header .logo {
          background-color: #3c8dbc;}
        .skin-blue .main-header .logo:hover {
          background-color: #3c8dbc;}'))
    ),
    fluidRow(
      # progress spinner progress icon
      add_busy_spinner(
        color = usaid_lightgrey,
        spin = "fading-circle",
        timeout = 100,
        position = "top-left"
      )
    ),
    box(glue("This application reads in a MER Structured Dataset
              (MSD) and allows you to preview and generate slides of
              a range of exploratory visualizations developed from 
              themes common in POART presentations. 
              On the left are options to create each visual. 
              Each tab on the right features a different exploratory visualization
              you can preview in the application or export as a Powerpoint slide. 
                    
              For questions about or suggestions for this application 
              please contact sisupport@usaid.gov"),
      # "About the Application"
      title = "POART-l: Start Here",
      footer = glue("Developer/Maintainer: Jessica Hoehner, 
                Contributers: Aaron Chafetz"), 
      collapsible = TRUE),
    # Cumul Achv Box -----
    tabBox(
      id = "cumul_achv",
      # "Cumulative Achievement"
      tabPanel("Cumulative Achievement",
        glue("Cumulative achievement of quarterly targets for selected inputs.
                    Cumulative Results and Quarterly Achievement (%) labeled for previous 4 quarters"),
        width = 4,
        tags$br()
      )
    ), # close tab box
    # "Quarterly Achievement" ---------------------------------------------------
    #        tabPanel("Quarterly Achievement",
    #        width = 4,
    #        tags$br()), # close second tab panel
    # "Quarterly IIT and RTT" ---------------------------------------------------
    #      tabPanel("Quarterly IIT and RTT",
    #      width = 4,
    #      tags$br()), # close third tab panel
    # "Quarterly Patient Delta" ---------------------------------------------------
    #      tabPanel("Quarterly Patient Delta",
    #               width = 4,
    #               tags$br())),
    # Preview box ----
    box(title = "Figure Preview", htmlOutput("plot"), width = 12)
  ) # close Body
) # close DashboardPage
