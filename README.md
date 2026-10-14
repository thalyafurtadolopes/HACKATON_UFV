# HACKATON_UFV
Prototype of a platform for women in STEM fields to understand how to bring their research to market.
library(shiny)
library(bslib)
library(bsicons)
library(dplyr)

# Modern UI theme with Biotech/AgTech color scheme
custom_theme <- bs_theme(
  bootswatch = "flatly",
  primary = "#1B4D3E",   # Dark green tone
  secondary = "#2A9D8F"
)

# --- USER INTERFACE (UI) ---
ui <- page_navbar(
  title = "Lab2Market STEM",
  theme = custom_theme,
  
  tabPanel(
    "Innovation Readiness Diagnosis",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h4("Project Profile"),
        textInput("researcher_name", "Lead Researcher", value = "Dr. Carolina"),
        selectInput("stem_field", "STEM Field", choices = c(
          "Agronomy / Plant Pathology", 
          "Biotechnology / Microbiology", 
          "Health / Pharmaceuticals", 
          "Engineering / Materials", 
          "Other"
        )),
        textInput("project_title", "Research Title", value = "Biofungicide based on Bacillus spp."),
        
        hr(),
        h4("Maturity Assessment (TRL & Market)"),
        radioButtons("q1", "1. Proof of concept in laboratory:", choices = c(
          "Not started" = 0, "In progress" = 1, "Successfully completed" = 2
        )),
        radioButtons("q2", "2. Validation in relevant environment:", choices = c(
          "Lab scale only" = 0, "Greenhouse / Pilot scale" = 1, "Field / Operational conditions" = 2
        )),
        radioButtons("q3", "3. Intellectual Property (IP):", choices = c(
          "No search / protection" = 0, "Prior art search completed" = 1, "Patent filed / granted" = 2
        )),
        radioButtons("q4", "4. Industry & Market engagement:", choices = c(
          "No industry contact" = 0, "Customer interviews conducted" = 1, "Letter of Intent / Partnership" = 2
        )),
        
        actionButton("calculate_btn", "Generate Diagnosis", class = "btn-success w-100 mt-2")
      ),
      
      mainPanel(
        width = 8,
        uiOutput("results_ui")
      )
    )
  ),
  
  tabPanel(
    "Academia-Industry Matchmaking",
    h3("Partnership & Funding Opportunities"),
    p("Curated opportunities, grants, and corporate partners filtered by your technology maturity and field:"),
    hr(),
    tableOutput("matches_table")
  )
)

# --- SERVER LOGIC ---
server <- function(input, output, session) {
  
  # Reactive assessment calculated on button click
  diagnosis_data <- eventReactive(input$calculate_btn, {
    score <- as.numeric(input$q1) + as.numeric(input$q2) + as.numeric(input$q3) + as.numeric(input$q4)
    
    # Technology Readiness Level (TRL) estimation
    trl_level <- case_when(
      score <= 2 ~ "TRL 2-3 (Concept & Proof of Concept)",
      score <= 5 ~ "TRL 4-5 (Validation / Pilot Testing)",
      TRUE       ~ "TRL 6-8 (Market Ready / Licensing)"
    )
    
    list(
      score = score,
      trl = trl_level,
      name = input$researcher_name,
      project = input$project_title,
      field = input$stem_field
    )
  }, ignoreNULL = FALSE) # Runs once on startup
  
  # Dynamically render results panel
  output$results_ui <- renderUI({
    res <- diagnosis_data()
    readiness_pct <- round((res$score / 8) * 100)
    
    tagList(
      h3(paste("Readiness Report:", res$project)),
      p(em(paste("Lead Researcher:", res$name, "| Field:", res$field))),
      br(),
      
      layout_columns(
        fill = FALSE,
        value_box(
          title = "Estimated TRL",
          value = res$trl,
          showcase = bsicons::bs_icon("graph-up-arrow"),
          theme = "primary"
        ),
        value_box(
          title = "Market Readiness Score",
          value = paste0(readiness_pct, "%"),
          showcase = bsicons::bs_icon("check2-circle"),
          theme = if(readiness_pct >= 50) "success" else "warning"
        )
      ),
      br(),
      card(
        card_header(h4("Recommended Action Plan")),
        card_body(
          if (res$score <= 2) {
            HTML("<b>1. Intellectual Property:</b> Contact your institution's Technology Transfer Office (TTO) to evaluate patentability.<br><b>2. Market Discovery:</b> Define your primary value proposition compared to current market products.")
          } else if (res$score <= 5) {
            HTML("<b>1. Field Validation:</b> Advance tests to real-world operational conditions.<br><b>2. Funding:</b> Apply for non-dilutive grants (e.g., SBIR/STTR, Horizon Europe, national innovation calls).<br><b>3. Industry Outreach:</b> Engage prospective corporate partners for joint validation trials.")
          } else {
            HTML("<b>1. Technology Transfer:</b> Start licensing discussions with industry players.<br><b>2. Academic Spin-off:</b> Explore founding a deep-tech startup based on your lab research.")
          }
        )
      )
    )
  })
  
  # Simulated industry matchmaking table
  output$matches_table <- renderTable({
    req(diagnosis_data())
    selected_field <- diagnosis_data()$field
    
    matches_df <- data.frame(
      Organization_or_Grant = c("AgroOpen Innovation Hub", "Global BioTech Ventures", "DeepTech Horizon Grant", "AgriScale Co-Development"),
      Field = c("Agronomy / Plant Pathology", "Biotechnology / Microbiology", "All STEM Fields", "Agronomy / Plant Pathology"),
      Support_Type = c("Acceleration & Field Testing", "Patent Licensing", "Non-Dilutive Grant", "B2B Co-Development"),
      Status = c("Open Applications", "Seeking Technologies", "Rolling Deadline", "Open Applications")
    )
    
    # Filter table by researcher's field + cross-cutting opportunities
    matches_df[matches_df$Field %in% c(selected_field, "All STEM Fields"), ]
  }, striped = TRUE, hover = TRUE)
}

# Run the Application
shinyApp(ui = ui, server = server)
