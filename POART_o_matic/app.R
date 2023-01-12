pacman::p_load(
  "shiny", "officer", "tidyverse","forcats",
  "Cairo", "grid", "gridExtra", "gagglr", "scales",
  "stringr", "janitor", "rvg", "glue", "withr","writexl", 
  "shinyWidgets", "shinydashboard","shinydashboardPlus", 
  "plotly", "shinybusy", "flexdashboard","readxl", "ggtext", 
  "tidytext", "cowplot", "gagglr")

# List of inputs used in the app -----------------------------------------------

inputs <- list(
  
  # folder where data files will be read in from
  input_dir = "Data/",
  
  # list of templates
  templates = list(
    template_one = "POART-o-matic/templates/Template_01_Cumulative_Achievement.pptx",
    template_two = "POART-o-matic/templates/Template_02_Quarterly_Achievement.pptx",
    template_three = "POART-o-matic/templates/Template_03_Quarterly_IIT_RTT.pptx", 
    template_four = "POART-o-matic/templates/Template_04_Quarterly_Patient_Delta.pptx" 
  ),
  
  # list of functions used by app
  functions = list(
    clean = "POART-o-matic/functions/00_loom.R",
    preview_rmd = "POART-o-matic/preview_01.Rmd",
    generate_rmd = "POART-o-matic/generate_01.rmd")
)

# Lists of data files available to use in app ----------------------------------

# Create lists of templates available for data ---------------------------------

# Location of output data

output_path <- here::here("POART_o_matic/Images")

# Setup user interface for app -------------------------------------------------

ui <- function() {
  
  ui <- dashboardPage(
    skin = "blue",
    # Title
    header = dashboardHeader(
      title = as.character("POART-al: 
                           For exploratory data analysis for POART presentations"),
      titleWidth = 1400),
    # Disable ugly sidebar
    sidebar = dashboardSidebar(disable = TRUE),
    # Create body of the page
    body = dashboardBody(
      fluidRow(
        # progress spinner progress icon
        add_busy_spinner(
          spin = "fading-circle",
          timeout = 100,
          position = "top-left"),
        # Initialize tabs
        # TODO: setup options for user to select an OU, indicator, years, 
        #       agency, and type
        #       remove option to select a template
        tabsetPanel(
          id = "select_options",
    # "Cumulative Achievement"
          tabPanel("Cumulative Achievement",
            # select OU
            shinyWidgets::pickerInput(
              inputId = "select_ou",
              label = "Select an operational unit (OU):",
              choices = unique(glamr::pepfar_country_list$operatingunit),
              options = list(size = 8)),
            # select an indicator
            pickerInput(
              inputId = "select_indicator",
              label = "Select an indicator:",
              choices = c("TX_CURR", "TX_NEW"),
              options = list(`actions-box` = TRUE, 
                             size = 8,
                             `selected-text-format` = "count > 3"), 
              multiple = FALSE),
            # select a year
            pickerInput(
              inputId = "select_year",
              label = "Select a year(s):",
              choices = c("2023", "2022", "2021", "2020"),
              options = list(`actions-box` = TRUE, 
                             size = 8,
                             `selected-text-format` = "count > 3"), 
              multiple = TRUE),
            # select an agency
            pickerInput(
              inputId = "select_agency",
              label = "Select an agency/agencies:",
              choices = c("USAID", "CDC", "DOD", "Peace Corps"),
              options = list(`actions-box` = TRUE, 
                             size = 8,
                             `selected-text-format` = "count > 3"), 
              multiple = TRUE),
            # select a type
            pickerInput(
              inputId = "select_type",
              label = "Select a type of data:",
              choices = c("Total", "Adult(15+)", "Pediatric (<15)"),
              options = list(`actions-box` = TRUE, 
                             size = 8,
                             `selected-text-format` = "count > 3"), 
              multiple = FALSE),
            width = 10,
            tags$br(),
            # Figure Preview button
            actionButton(
              inputId = "figure_preview",
              label = "Preview Figure"),
            # Generate slide button
            actionButton(
              inputId = "generate_slide",
              label = "Generate Slide"),
            width = 4,
            tags$br()), # close first tab
  # "Quarterly Achievement"
          tabPanel("Quarterly Achievement",
                   # select OU
                   shinyWidgets::pickerInput(
                     inputId = "select_ou",
                     label = "Select an operational unit (OU):",
                     choices = unique(glamr::pepfar_country_list$operatingunit),
                     options = list(size = 8)),
                   # select an indicator
                   pickerInput(
                     inputId = "select_indicator",
                     label = "Select an indicator:",
                     choices = c("TX_CURR", "TX_NEW"),
                     options = list(`actions-box` = TRUE, 
                                    size = 8,
                                    `selected-text-format` = "count > 3"), 
                     multiple = FALSE),
                   # select a year
                   pickerInput(
                     inputId = "select_year",
                     label = "Select a year(s):",
                     choices = c("2023", "2022", "2021", "2020"),
                     options = list(`actions-box` = TRUE, 
                                    size = 8,
                                    `selected-text-format` = "count > 3"), 
                     multiple = TRUE),
                   # select an agency
                   pickerInput(
                     inputId = "select_agency",
                     label = "Select an agency/agencies:",
                     choices = c("USAID", "Peace Corps", "CDC", "DOD"),
                     options = list(`actions-box` = TRUE, 
                                    size = 8,
                                    `selected-text-format` = "count > 3"), 
                     multiple = TRUE),
                   # select a type
                   pickerInput(
                     inputId = "select_type",
                     label = "Select a type of data:",
                     choices = c("Total", "Adult(15+)", "Pediatric (<15)"),
                     options = list(`actions-box` = TRUE, 
                                    size = 8,
                                    `selected-text-format` = "count > 3"), 
                     multiple = FALSE),
          width = 4,
          tags$br(),
          # Figure Preview button
          actionButton(
            inputId = "figure_preview",
            label = "Preview Figure"),
          # Generate slide button
          actionButton(
            inputId = "generate_slide",
            label = "Generate Slide"),
          width = 4,
          tags$br()), # close second tab panel
   # "Quarterly IIT and RTT"
        tabPanel("Quarterly IIT and RTT",
                 # select OU
                 shinyWidgets::pickerInput(
                   inputId = "select_ou",
                   label = "Select an operational unit (OU):",
                   choices = unique(glamr::pepfar_country_list$operatingunit),
                   options = list(size = 8)),
                 # select a year
                 pickerInput(
                   inputId = "select_year",
                   label = "Select a year(s):",
                   choices = c("2023", "2022", "2021", "2020"),
                   options = list(`actions-box` = TRUE, 
                                  size = 8,
                                  `selected-text-format` = "count > 3"), 
                   multiple = TRUE),
                 # select an agency
                 pickerInput(
                   inputId = "select_agency",
                   label = "Select an agency/agencies:",
                   choices = c("USAID", "Peace Corps", "CDC", "DOD"),
                   options = list(`actions-box` = TRUE, 
                                  size = 8,
                                  `selected-text-format` = "count > 3"), 
                   multiple = TRUE),
                 # select a type
                 pickerInput(
                   inputId = "select_type",
                   label = "Select a type of data:",
                   choices = c("Total", "Adult(15+)", "Pediatric (<15)"),
                   options = list(`actions-box` = TRUE, 
                                  size = 8,
                                  `selected-text-format` = "count > 3"), 
                   multiple = FALSE),
        width = 4,
        tags$br(),
        # Figure Preview button
        actionButton(
          inputId = "figure_preview",
          label = "Preview Figure"),
        # Generate slide button
        actionButton(
          inputId = "generate_slide",
          label = "Generate Slide"),
        width = 4,
        tags$br()), # close third tab panel
    # "Quarterly Patient Delta"
        tabPanel("Quarterly Patient Delta",
                 # select OU
                 shinyWidgets::pickerInput(
                   inputId = "select_ou",
                   label = "Select an operational unit (OU):",
                   choices = unique(glamr::pepfar_country_list$operatingunit),
                   options = list(size = 8)),
                 # select a year
                 pickerInput(
                   inputId = "select_year",
                   label = "Select a year(s):",
                   choices = c("2023", "2022", "2021", "2020"),
                   options = list(`actions-box` = TRUE, 
                                  size = 8,
                                  `selected-text-format` = "count > 3"), 
                   multiple = TRUE),
                 # select an agency
                 pickerInput(
                   inputId = "select_agency",
                   label = "Select an agency/agencies:",
                   choices = c("USAID", "Peace Corps", "CDC", "DOD"),
                   options = list(`actions-box` = TRUE, 
                                  size = 8,
                                  `selected-text-format` = "count > 3"), 
                   multiple = TRUE),
                 # select a type
                 pickerInput(
                   inputId = "select_type",
                   label = "Select a type of data:",
                   choices = c("Total", "Adult(15+)", "Pediatric (<15)"),
                   options = list(`actions-box` = TRUE, 
                                  size = 8,
                                  `selected-text-format` = "count > 3"), 
                   multiple = FALSE),
                 width = 4,
                 tags$br(),
                 # Figure Preview button
                 actionButton(
                   inputId = "figure_preview",
                   label = "Preview Figure"),
                 # Generate slide button
                 actionButton(
                   inputId = "generate_slide",
                   label = "Generate Slide"),
                 width = 4,
                 tags$br())), 
# tab box
        box(
          title = "Figure Preview",
          htmlOutput("plot"), width = 12)
      ) # fluid row
    ) # dashboard body
  ) # dashboard page
  return(ui)
} # ui function

# Initialize connection to the R Shiny server ----------------------------------
server <- function(input, output, session) {
  
  # Update on figure preview ---------------------------------------------------
  
  # if the command is submitted
  # If the user selects to preview a figure
  observeEvent(input$figure_preview, {
    temp <- isolate(input$select_template)
    
    template_dict <- list(
      # replace with user indicated name of template
      "Template_Cumulative_Achievement.pptx" =
        list(
          in_file_p = inputs$functions$preview_rmd,
          out_file = "template_01.html"
        )
    )
    
    # Output the plots requested
    output$plot <- renderUI({
      rmarkdown::render(
        input = template_dict[[temp]]$in_file_p,
        params = param_list,
        output_dir = "./www/",
        output_format = "flexdashboard::flex_dashboard",
        quiet = FALSE
      )
      tags$html(
        tags$iframe(
          seamless = "seamless",
          src = template_dict[[temp]]$out_file,
          width = "100%",
          height = "800px",
          id = "reportIframe"
        )
      )
    })
  }) # end slide preview loop
  
  # If the "Generate Slide button is pushed,
  observeEvent(input$generate_slide, {
    temp <- isolate(input$select_template)
    
    template_dict <- list(
      # replace with user indicated name of template
      "Template_Cumulative_Achievement.pptx" =
        list(in_file_g = inputs$functions$generate_rmd)
    )
    
    # generate the slides with user-provided parameters
    rmarkdown::render(
      input = template_dict[[temp]]$in_file_g,
      params = param_list,
      quiet = FALSE
    )
  })
} # server
# run complete App -------------------------------------------------------------
shinyApp(ui, server) # Shiny App