pacman::p_load(
  "shiny", "officer", "tidyverse","forcats",
  "Cairo", "grid", "gridExtra", "gagglr", "scales",
  "stringr", "janitor", "rvg", "glue", "withr","writexl", 
  "shinyWidgets", "shinydashboard","shinydashboardPlus", 
  "plotly", "shinybusy", "flexdashboard","readxl", "ggtext", 
  "tidytext", "cowplot", "gagglr", "rmarkdown", "extrafont")

# List of inputs used in the app -----------------------------------------------

inputs <- list(
  
  # folder where data files will be read in from
  input_dir = "Data/",
  
  # list of templates
  templates = list(
    template_one = "POART-o-matic/templates/Template_01_Cumulative_Achievement.pptx",
    template_two = "POART-o-matic/templates/Template_02_Quarterly_Achievement.pptx",
    template_three = "POART-o-matic/templates/Template_03_Quarterly_IIT_RTT.pptx", 
    template_four = "POART-o-matic/templates/Template_04_Quarterly_Patient_Delta.pptx"),
  
  # list of functions used by app
  functions = list(
    clean = "POART-o-matic/functions/00_loom.R",
    preview_rmd = "POART-o-matic/preview_01.Rmd",
    generate_rmd = "POART-o-matic/generate_01.rmd")
)

# Lists of data files available to use in app ----------------------------------

# Create lists of templates available for data ---------------------------------

# Location of output slides
# output_path <- here::here("POART_o_matic/Images")

# Setup user interface for app -------------------------------------------------
ui <- function() {
  # Header ---------
  header <- dashboardHeader(disable = TRUE)
  
  
  # Sidebar ----
  sidebar <- shinydashboardPlus::dashboardSidebar(
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
        options = list(size = 8)),
      # select an indicator ----
      pickerInput(
        inputId = "select_indicator",
        label = "Select an indicator:",
        choices = c("TX_CURR", "TX_NEW"),
        options = list(size = 8), 
        multiple = FALSE),
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
        multiple = FALSE),
      # select a type ----
      pickerInput(
        inputId = "select_type",
        label = "Select a type of data:",
        choices = c("Total", "Adults (15+)", "Children (<15)"),
        options = list(size = 8), 
        multiple = FALSE),
      # Figure Preview button
      actionButton(
        inputId = "figure_preview",
        label = "Preview Figure"),
      # Generate slide button
      actionButton(
        inputId = "generate_slide",
        label = "Generate Slide")
    ) # close sidebar Menu
  ) # close sidebar
  
  
  # Body ---------
  body <- dashboardBody(
    tags$head(
      tags$style(HTML('.main-header .logo {
        font-family: "Source Sans Pro"; font-weight: bold;font-size: 24px;}
        .skin-blue .main-header .logo {
          background-color: #3c8dbc;}
        .skin-blue .main-header .logo:hover {
          background-color: #3c8dbc;}'))),
    fluidRow(
      # progress spinner progress icon
      add_busy_spinner(
        color = usaid_lightgrey,
        spin = "fading-circle", 
        timeout = 100, 
        position = "top-left")),
    # Cumul Achv Box -----
    tabBox(
      id = "cumul_achv",
      # "Cumulative Achievement"
      # Buttons show up correctly but don't currently impact the output,
      # as of now all parameters are being set in the preview_01.Rmd file
      # and I need to learn how to get the parameters indicated in the 
      # buttons into param_list that the preview file uses.
      tabPanel("Cumulative Achievement",
               glue("Cumulative achievement of quarterly targets for selected inputs."),
               width = 4,
               tags$br())
    ), #close tab box
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
  
  
  # Dashboard Page ----
ui <- dashboardPage(header, sidebar, body)
  
return(ui)
}

# Initialize connection to the R Shiny server ----------------------------------
server <- function(input, output, session) {
  
  # Update on figure preview ---------------------------------------------------
  
  # if the command is submitted
  # If the user selects to preview a figure
  observeEvent(input$figure_preview, {
    
    # get inputs
    selected_ou <-  as.character(input$select_ou)
    
    selected_indicator = as.character(input$select_indicator)
     
    # selected_years = as.list(input$select_year)
     
    selected_agency = as.character(input$select_agency)
     
    selected_type = as.character(input$select_type)
    #
    param_list <- list(
      selected_ou = selected_ou,
      selected_indicator = selected_indicator,
      # #selected_years = selected_years,
      selected_agency = selected_agency,
      selected_type = selected_type
      )
    
    dict <- list(
      in_file = here::here("POART_o_matic/Scripts/preview_01.Rmd"),
      out_file = "preview_01.html")

    # Output the plots requested
    output$plot <- renderUI({
      rmarkdown::render(
        input = dict$in_file,
        params = param_list,
        output_dir = "./www/",
        output_format = "flexdashboard::flex_dashboard",
        quiet = FALSE)
      tags$html(
        tags$iframe(
          seamless = "seamless",
          src = dict$out_file,
          width = "100%",
          height = "500px",
          id = "reportIframe"
        )
      )
    })
  }) # end slide preview loop
  
  # If the user selects to generate a slide,
  observeEvent(input$generate_slide, {
    temp <- isolate(input$select_template)
    
    template_dict <- list(
      # replace with user indicated name of template
      "Template_Cumulative_Achievement.pptx" =
        list(in_file_g = inputs$functions$generate_rmd))
    
    # generate the slides with user-provided parameters
    rmarkdown::render(
      input = template_dict[[temp]]$in_file_g,
      params = param_list,
      quiet = FALSE
    )
  })
}

# run complete App -------------------------------------------------------------
shinyApp(ui, server) # Shiny App