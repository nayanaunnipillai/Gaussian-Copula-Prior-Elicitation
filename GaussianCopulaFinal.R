library(shiny)
library(Rsolnp)
library(shinyjs)
library(mvtnorm)
library(extraDistr)
library(corrplot)
library(RColorBrewer)
library(plotly)
library(ggplot2)
library(GGally)
library(shinythemes)
library(shinyWidgets)
library(greybox)
library(remotes)
#library(rjqpd)
library(psych)


source("DistOptimizePebsi.R")


ui <- navbarPage(
  id = "main_nav",
  title = "Elicitation",
  theme = shinythemes::shinytheme("cosmo"),  
  
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
    
    tags$style(HTML('

/* --- GLOBAL --- */
body {
  min-height: 100vh;
  background: linear-gradient(to bottom right, #e0f2f1, #f1f8e9);
  font-family: "Segoe UI", sans-serif;
  color: #212121;
}


/* --- NAVBAR --- */
.navbar-default {
  background: linear-gradient(to right, #283593, #009688);
  border: none;
  box-shadow: 0 4px 8px rgba(0,0,0,0.1);
}
.navbar-default .navbar-brand,
.navbar-default .navbar-nav > li > a {
  color: white !important;
  font-weight: bold;
}
.navbar-default .navbar-nav > li > a:hover {
  background-color: rgba(255,255,255,0.1);
  color: #FFB300 !important;
}
.navbar-default .navbar-nav > .active > a {
  background-color: rgba(255,255,255,0.2) !important;
  color: #ffffff !important;
}

/* --- HOME SECTION --- */
.home-container {
  padding: 60px 20px;
  background: linear-gradient(to right, #e0f2f1, #f1f8e9);
  border-radius: 0 0 20px 20px;
  text-align: center;
}
.home-title {
  font-size: 40px;
  font-weight: bold;
  color: #283593;
  text-shadow: 2px 2px 6px rgba(0,0,0,0.1);
}
.home-subtitle {
  font-size: 22px;
  color: #37474F;
  margin-bottom: 30px;
}
.btn-get-started {
  background: linear-gradient(to right, #009688, #26A69A);
  color: white;
  font-size: 18px;
  font-weight: bold;
  border-radius: 50px;
  padding: 12px 30px;
  border: none;
  transition: 0.3s ease;
}
.btn-get-started:hover {
  background: linear-gradient(to right, #FFB300, #00796B);
}

/* --- FEATURE CARDS --- */
.feature-section {
  display: flex;
  justify-content: center;
  align-items: center;
  padding: 60px 20px;
  background-color: transparent;
}

.feature-row {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(260px, 1fr));
  gap: 40px;
  width: 100%;
  max-width: 1100px;
  margin: 0 auto;
  justify-items: center;
  align-items: stretch;
}

.feature-card {
  background: linear-gradient(to bottom right, #ffffff, #e0f7fa);
  border-radius: 16px;
  padding: 25px;
  transition: transform 0.3s ease, box-shadow 0.3s ease;
  box-shadow: 0 6px 18px rgba(0, 0, 0, 0.08);
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;
  text-align: center;
  min-height: 220px;
  width: 100%;
  max-width: 320px;
}

.feature-card:hover {
  transform: translateY(-5px);
  box-shadow: 0 12px 24px rgba(0, 0, 0, 0.15);
}

.feature-card i {
  font-size: 38px;
  margin-bottom: 12px;
  color: #26a69a;
  transition: transform 0.3s ease, color 0.3s ease;
}

.feature-card:hover i {
  transform: scale(1.2);
  color: #ff8f00;
}

.feature-card h4 {
  color: #00695c;
  font-weight: 700;
  font-size: 22px;
  margin-bottom: 10px;
}

.feature-card p {
  font-size: 15px;
  font-weight: 500;
  color: #37474f;
  line-height: 1.4;
}


/* --- SIDEBAR PANEL --- */
.well {
  background: linear-gradient(to bottom, #ffffff, #e0f7fa);
  border: none;
  border-radius: 18px;
  padding: 25px;
  box-shadow: 0 6px 20px rgba(0,0,0,0.1);
}

/* --- INPUT FIELDS --- */
.form-control {
  border: 2px solid;
  border-image: linear-gradient(90deg, #4A90E2, #50E3C2) 1;
  border-radius: 10px;
  font-weight: bold;
  color: #000000 !important;
  background-color: transparent;
  box-shadow: none;
}

.form-control:focus {
  border-image: linear-gradient(90deg, #007ACC, #26A69A) 1;
  outline: none;
}
label.control-label {
  color: #00695c;
  font-weight: 600;
}

/* --- BUTTONS --- */
.btn-custom {
  background: linear-gradient(to right, #4DA0B0, #D39D38);
  color: white;
  border: none;
  border-radius: 50px;
  padding: 10px;
  width: 100%;
  font-weight: bold;
  transition: 0.3s ease;
}
.btn-custom:hover {
  background: linear-gradient(to right, #26A69A, #1E88E5);
}

/* --- PANEL TITLES --- */
.panel-title {
  background: linear-gradient(to right, #26A69A, #1E88E5);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  font-size: 22px;
  font-weight: bold;
  text-align: center;
}

/* --- DIVIDERS --- */
hr {
  border: none;
  height: 3px;
  background: linear-gradient(to right, #26A69A, #42A5F5);
  box-shadow: 0 2px 6px rgba(0,0,0,0.1);
  margin: 30px 0;
}

/* --- CONTACT --- */
.contact-container {
  max-width: 600px;
  margin: 0 auto;
  background: radial-gradient(circle, #f0f8ff, #ffffff);
  padding: 30px;
  border-radius: 12px;
  box-shadow: 0 6px 16px rgba(0, 0, 0, 0.08);
  text-align: center;
}
.contact-box:hover {
  transform: scale(1.05);
  box-shadow: 0px 4px 12px rgba(0, 0, 0, 0.2);
}
.contact-name {
  color: #00695c;
  font-size: 25px;
  font-weight: bold;
}
.contact-email {
  font-size: 20px;
  color: #2c3e50;
  text-decoration: none;
}
.contact-email:hover {
  color: #e74c3c;
  text-decoration: underline;
}
.fas {
  color: #26a69a;
  margin-right: 5px;
}

/* --- OUTPUT BOXES --- */

.output-box {
  border: 2px solid;
  border-image: linear-gradient(90deg, #4A90E2, #50E3C2) 1;
  border-radius: 6px;
  padding: 10px 12px;
  font-weight: bold;
  color: #000000;
  font-family: "Segoe UI", sans-serif;
  background-color: transparent !important;
  box-shadow: none !important;
  white-space: pre-wrap;
  margin-bottom: 10px;
}

'))),
  
  # ---------------- Home ----------------
  
  tabPanel("Home",
           fluidPage(
             div(class = "home-container",
                 tags$h1("Eliciting a Gaussian Copula with Different Univariate Marginal Distributions", class = "home-title"),
                 br(), br(), br(),
                 div(class = "feature-row",
                     div(class = "feature-card",
                         tags$i(class = "fas fa-chart-line"),
                         tags$h4("Select Marginal Prior"),
                         tags$p("Choose from various unit-interval distributions.")
                     ),
                     div(class = "feature-card",
                         tags$i(class = "fas fa-sliders-h"),
                         tags$h4("Interactive Assessment"),
                         tags$p("Experts can assess quartiles and medians interactively.")
                     ),
                     div(class = "feature-card",
                         tags$i(class = "fas fa-project-diagram"),
                         tags$h4("Copula Correlation"),
                         tags$p("Visualise dependencies using correlation plots.")
                     )
                 ),
                 br(), br(),
                 actionButton("get_started", "Get Started", class = "btn-get-started")
             )
           )
  ),
  
  # ---------------- Gaussian Copula ----------------
  
  tabPanel("Gaussian Copula",
           sidebarLayout(
             sidebarPanel(
               div(class = "well",
                   selectInput("distributionG", "Select the Marginal Prior Distribution Family",
                               choices = c("Continuous-Bernoulli", "Alpha-Unit", "Topp-Leone", "Beta", "Kumaraswamy",
                                           "Unit-Weibull", "Beta Type 3", "Unit-Chen", "Logit-Normal", "Unit-Logistic",
                                           "Libby-Novick Beta", "Libby-Novick Kumaraswamy", "Unit-Exponential Pareto", "JQPDB"),
                               selected = "Kumaraswamy"
                   ),
                   numericInput("k", tags$strong("Number of Categories:"), value = 6, min = 4, max = 10),
                   uiOutput("category_inputs"),
                   actionButton("next_button1", "Next", class = "btn btn-custom"),
                   uiOutput("conditional_inputs"),
                   
                   conditionalPanel(
                     condition = "input.next_button1 > 0",
                     actionButton("calculate_button1", "Calculate", class = "btn btn-custom"),
                     br(), br(),
                     conditionalPanel(
                       condition = "input.calculate_button1 > 0",
                       tags$h4("Conditional Quartiles Assessments", class = "panel-title"),
                       br(),
                       uiOutput("conditional_quartiles"),
                       actionButton("next_button2", "Next", class = "btn btn-custom")
                     ),
                     br(), br(),
                     conditionalPanel(
                       condition = "input.next_button2 > 0",
                       tags$h4("Conditional Median Assessments", class = "panel-title"),
                       br(),
                       uiOutput("conditional_medians"),
                       actionButton("calculate_button2", "Calculate", class = "btn btn-custom")
                     )
                   )
               )
             ),
             mainPanel(
               div(class = "home-container",
                   uiOutput("distribution_heading"),
                   br(),
                   div(style = "text-align:center; font-weight:bold; font-size:16px; color:#0f0c0c; margin-top:10px;",
                       uiOutput("distribution_panel")
                   ),
                   conditionalPanel(
                     condition = "input.calculate_button2 > 0",
                     h3("Correlation Matrix", 
                        style = "color:#1E40AF; font-weight:800; font-size:20px; font-family:'Segoe UI', sans-serif; text-transform:uppercase; letter-spacing:1px; margin-bottom:20px;"),
                     div(class = "output-box",
                         verbatimTextOutput("correlationOutput")
                     ),
                     br(),
                     h3("Correlation Plot", 
                        style = "color:#1E40AF; font-weight:800; font-size:20px; font-family:'Segoe UI', sans-serif; text-transform:uppercase; letter-spacing:1px; margin-bottom:20px;"),
                     plotOutput("correlationPlot", height = "400px", width = "100%")
                   )
               )
             )
             
             
           )
  ),
  
  # ---------------- Contact ----------------
  
  tabPanel("Contact Us",
           div(class = "contact-container",
               tags$h3("Get in Touch"),
               br(),
               tags$h4("Nayana Unnipillai", class = "contact-name"),
               tags$p("Email: ", tags$a(href="mailto:nayana.unnipillai@open.ac.uk", class = "contact-email", "nayana.unnipillai@open.ac.uk")),
               br(),
               tags$h4("Fadlalla Elfadaly", class = "contact-name"),
               tags$p("Email: ", tags$a(href="mailto:fadlalla.elfadaly@open.ac.uk", class = "contact-email", "fadlalla.elfadaly@open.ac.uk"))
           )
  )
)


server <- function(input, output, session) {
  
  observeEvent(input$get_started, {
    updateNavbarPage(session, "main_nav", selected = "Gaussian Copula")
  })
  
  output$distribution_heading <- renderUI({
    req(input$distributionG)
    tags$h4(
      paste(input$distributionG, "Marginal Distribution for each category"),
      style = "color:#1E40AF; font-weight:800; font-size:25px; font-family:'Segoe UI', sans-serif; 
             margin-bottom:25px; text-transform:uppercase; letter-spacing:1px;"
    )
  })
  
  
  
  output$category_inputs <- renderUI({
    k <- as.numeric(input$k)
    category_inputs <- list()
    
    
    category_inputs[[1]] <- tagList(
      h4(tags$strong("Category 1", style = "font-family: Candara, Calibri, sans-serif; font-weight: bold; color:#8B1A1A")),
      fluidRow(
        column(12, h4(tags$strong("Quartile values for p1", style = "font-family: Garamond, Baskerville, 'Baskerville Old Face', 'Hoefler Text', 'Times New Roman', serif; font-weight: bold;color:#2F4F4F"))),
        column(4, numericInput("q1", "Q1:", value = 0.1, min = 0.1, max = 0.9, step = 0.1)),
        column(4, numericInput("q2", "Q2:", value = 0.2, min = 0.1, max = 0.9, step = 0.1)),  
        column(4, numericInput("q3", "Q3:", value = 0.3, min = 0.1, max = 0.9, step = 0.1))
      )
    )
    
    tagList(category_inputs)
  })
  
  value <- reactiveVal(FALSE)     
  
  observeEvent(input$next_button1, {
    value(TRUE)
  })
  
  output$conditional_inputs <- renderUI({
    req(value())
    k <- as.numeric(input$k)
    
    conditional_inputs <- lapply(2:(k-1), function(i) {
      fluidRow(
        h4(tags$strong(paste("Category ", i), style = "font-family: Candara, Calibri, sans-serif; font-weight: bold;color:#8B1A1A")),
        fluidRow(
          column(12, tags$h4(
            tags$strong(
              paste0("Quartile values for p", i, " (Assuming p", paste0(1:(i-1), collapse = ", p"), " are eliminated):")
            ),
            style = "font-family: Garamond, Baskerville, 'Baskerville Old Face', 'Hoefler Text', 'Times New Roman', serif; font-weight: bold; color:#2F4F4F"
          )),
          column(4, numericInput(paste("q1p", i), "Q1:", value = 0.1, step=0.1, min=0.1, max=0.9)),
          column(4, numericInput(paste("q2p", i), "Q2:", value = 0.2, step=0.1, min=0.1, max=0.9)),
          column(4, numericInput(paste("q3p", i), "Q3:", value = 0.3, step=0.1, min=0.1, max=0.9))
        ),
        
      )
    })
    
    
    tagList(conditional_inputs)
  })
  
  
  
  
  output$distribution_panel <- renderUI({
    #  dist <- input$distributionG
    k <- as.numeric(input$k)
    
    switch(input$distributionG,
           
           "Continuous-Bernoulli" = {
             tagList(
               lapply(1:(k-1), function(i) {
                 fluidRow(
                   column(2, verbatimTextOutput(paste0("category_output1", i))),
                   column(3, verbatimTextOutput(paste0("CB_output", i))),
                   column(10, plotOutput(paste0("plotCB", i), width = "100%", height = "300px"))
                 )
               }),
               hr(),
               h4(tags$strong("Error Value", style = "font-family: Candara, Calibri, sans-serif; color:#8B1A1A")),
               verbatimTextOutput("aggregate_error"),
               hr()
             )
           },
           
           
           
           "Alpha-Unit" = {
             tagList(
               lapply(1:(k-1), function(i) {
                 fluidRow(
                   column(2, verbatimTextOutput(paste0("category_output2", i))),
                   column(3, verbatimTextOutput(paste0("AU_output", i))),
                   column(10, plotOutput(paste0("plotAU", i), width = "100%", height = "300px"))
                 )
               }),
               hr(),
               h4(tags$strong("Error Value", style = "font-family: Candara, Calibri, sans-serif; color:#8B1A1A")),
               verbatimTextOutput("aggregate_error"),
               hr()
             )
           },
           
           "Topp-Leone" = {
             tagList(
               lapply(1:(k-1), function(i) {
                 fluidRow(
                   column(2, verbatimTextOutput(paste0("category_output3", i))),
                   column(3, verbatimTextOutput(paste0("TL_output", i))),
                   column(10, plotOutput(paste0("plotTL", i), width = "100%", height = "300px"))
                 )
               }),
               hr(),
               h4(tags$strong("Error Value", style = "font-family: Candara, Calibri, sans-serif; color:#8B1A1A")),
               verbatimTextOutput("aggregate_error"),
               hr()
             )
           },
           
           "Beta" = {
             tagList(
               lapply(1:(k-1), function(i) {
                 fluidRow(
                   column(2, verbatimTextOutput(paste0("category_output4", i))),
                   column(3, verbatimTextOutput(paste0("aB_output", i))),
                   column(3, verbatimTextOutput(paste0("bB_output", i))),
                   column(10, plotOutput(paste0("plotB", i), width = "100%", height = "300px"))
                 )
               }),
               hr(),
               h4(tags$strong("Error Value", style = "font-family: Candara, Calibri, sans-serif; color:#8B1A1A")),
               verbatimTextOutput("aggregate_error"),
               hr()
             )
           },
           
           "Kumaraswamy" = {
             tagList(
               lapply(1:(k-1), function(i) {
                 fluidRow(
                   column(2, verbatimTextOutput(paste0("category_output5", i))),
                   column(3, verbatimTextOutput(paste0("aK_output", i))),
                   column(3, verbatimTextOutput(paste0("bK_output", i))),
                   column(10, plotOutput(paste0("plotK", i), width = "100%", height = "300px"))
                 )
               }),
               hr(),
               h4(tags$strong("Error Value", style = "font-family: Candara, Calibri, sans-serif; color:#8B1A1A")),
               verbatimTextOutput("aggregate_error"),
               hr()
             )
           },
           
           "Unit-Weibull" = {
             tagList(
               lapply(1:(k-1), function(i) {
                 fluidRow(
                   column(2, verbatimTextOutput(paste0("category_output6", i))),
                   column(3, verbatimTextOutput(paste0("pUW_output", i))),
                   column(3, verbatimTextOutput(paste0("qUW_output", i))),
                   column(10, plotOutput(paste0("plotUW", i), width = "100%", height = "300px"))
                 )
               }),
               hr(),
               h4(tags$strong("Error Value", style = "font-family: Candara, Calibri, sans-serif; color:#8B1A1A")),
               verbatimTextOutput("aggregate_error"),
               hr()
             )
           },
           
           "Beta Type 3" = {
             tagList(
               lapply(1:(k-1), function(i) {
                 fluidRow(
                   column(2, verbatimTextOutput(paste0("category_output7", i))),
                   column(3, verbatimTextOutput(paste0("aB3_output", i))),
                   column(3, verbatimTextOutput(paste0("bB3_output", i))),
                   column(10, plotOutput(paste0("plotB3", i), width = "100%", height = "300px"))
                 )
               }),
               hr(),
               h4(tags$strong("Error Value", style = "font-family: Candara, Calibri, sans-serif; color:#8B1A1A")),
               verbatimTextOutput("aggregate_error"),
               hr()
             )
           },
           
           "Unit-Chen" = {
             tagList(
               lapply(1:(k-1), function(i) {
                 fluidRow(
                   column(2, verbatimTextOutput(paste0("category_output8", i))),
                   column(3, verbatimTextOutput(paste0("aCH_output", i))),
                   column(3, verbatimTextOutput(paste0("bCH_output", i))),
                   column(10, plotOutput(paste0("plotCH", i), width = "100%", height = "300px"))
                 )
               }),
               hr(),
               h4(tags$strong("Error Value", style = "font-family: Candara, Calibri, sans-serif; color:#8B1A1A")),
               verbatimTextOutput("aggregate_error"),
               hr()
             )
           },
           
           "Logit-Normal" = {
             tagList(
               lapply(1:(k-1), function(i) {
                 fluidRow(
                   column(2, verbatimTextOutput(paste0("category_output9", i))),
                   column(3, verbatimTextOutput(paste0("aLN_output", i))),
                   column(3, verbatimTextOutput(paste0("bLN_output", i))),
                   column(10, plotOutput(paste0("plotLN", i), width = "100%", height = "300px"))
                 )
               }),
               hr(),
               h4(tags$strong("Error Value", style = "font-family: Candara, Calibri, sans-serif; color:#8B1A1A")),
               verbatimTextOutput("aggregate_error"),
               hr()
             )
           },
           
           "Unit-Logistic" = {
             tagList(
               lapply(1:(k-1), function(i) {
                 fluidRow(
                   column(2, verbatimTextOutput(paste0("category_output10", i))),
                   column(3, verbatimTextOutput(paste0("aUL_output", i))),
                   column(3, verbatimTextOutput(paste0("bUL_output", i))),
                   column(10, plotOutput(paste0("plotUL", i), width = "100%", height = "300px"))
                 )
               }),
               hr(),
               h4(tags$strong("Error Value", style = "font-family: Candara, Calibri, sans-serif; color:#8B1A1A")),
               verbatimTextOutput("aggregate_error"),
               hr()
             )
           },
           
           "Libby-Novick Beta" = {
             tagList(
               lapply(1:(k-1), function(i) {
                 fluidRow(
                   column(2, verbatimTextOutput(paste0("category_output11", i))),
                   column(3, verbatimTextOutput(paste0("aLNB_output", i))),
                   column(3, verbatimTextOutput(paste0("bLNB_output", i))),
                   column(3, verbatimTextOutput(paste0("cLNB_output", i))),
                   column(10, plotOutput(paste0("plotLNB", i), width = "100%", height = "300px"))
                 )
               }),
               hr(),
               h4(tags$strong("Error Value", style = "font-family: Candara, Calibri, sans-serif; color:#8B1A1A")),
               verbatimTextOutput("aggregate_error"),
               hr()
             )
           },
           
           "Libby-Novick Kumaraswamy" = {
             tagList(
               lapply(1:(k-1), function(i) {
                 fluidRow(
                   column(2, verbatimTextOutput(paste0("category_output12", i))),
                   column(3, verbatimTextOutput(paste0("aLNK_output", i))),
                   column(3, verbatimTextOutput(paste0("bLNK_output", i))),
                   column(3, verbatimTextOutput(paste0("cLNK_output", i))),
                   column(10, plotOutput(paste0("plotLNK", i), width = "100%", height = "300px"))
                 )
               }),
               hr(),
               h4(tags$strong("Error Value", style = "font-family: Candara, Calibri, sans-serif; color:#8B1A1A")),
               verbatimTextOutput("aggregate_error"),
               hr()
             )
           },
           
           "Unit-Exponential Pareto" = {
             tagList(
               lapply(1:(k-1), function(i) {
                 fluidRow(
                   column(2, verbatimTextOutput(paste0("category_output13", i))),
                   column(3, verbatimTextOutput(paste0("aUP_output", i))),
                   column(3, verbatimTextOutput(paste0("bUP_output", i))),
                   column(3, verbatimTextOutput(paste0("cUP_output", i))),
                   column(10, plotOutput(paste0("plotUP", i), width = "100%", height = "300px"))
                 )
               }),
               hr(),
               h4(tags$strong("Error Value", style = "font-family: Candara, Calibri, sans-serif; color:#8B1A1A")),
               verbatimTextOutput("aggregate_error"),
               hr()
             )
           },
           
           "JQPDB" = {
             lapply(1:(k-1), function(i) {
               fluidRow(
                 column(2, verbatimTextOutput(paste0("category_output14", i))),
                 column(3, verbatimTextOutput(paste0("aJ_output", i))),
                 column(3, verbatimTextOutput(paste0("bJ_output", i))),
                 column(3, verbatimTextOutput(paste0("cJ_output", i))),
                 column(10, plotOutput(paste0("plotJ", i), width = "100%", height = "300px"))
               )
             })
           }
           
    )
  })
  
  # Calculations
  
  observeEvent(input$calculate_button1, {
    req(input$k)
    k <- as.numeric(input$k)
    
    lambdaC <<- rep(0, k); aB <<- rep(0, k); bB <<- rep(0, k); aK <<- rep(0, k); bK <<- rep(0, k); pW <<- rep(0, k); qW <<- rep(0, k); 
    aAU <<- rep(0, k); aTL <<- rep(0, k); aB3 <<- rep(0, k); bB3 <<- rep(0, k); aUC <<- rep(0, k); bUC <<- rep(0, k); 
    aCH <<- rep(0, k); bCH <<- rep(0, k); aLN <<- rep(0, k); bLN <<- rep(0, k); aUL <<- rep(0, k); bUL <<- rep(0, k); 
    aLNB <<- rep(0, k); bLNB <<- rep(0, k); cLNB <<- rep(0, k); aLNK <<- rep(0, k); bLNK <<- rep(0, k); cLNK <<- rep(0, k); 
    aUP <<- rep(0, k); bUP <<- rep(0, k); cUP <<- rep(0, k); aJ <<- rep(0, k); bJ <<- rep(0, k); cJ <<- rep(0, k)
    
    errorCB <<- rep(NA, k); errorAU <<- rep(NA, k); errorTL <<- rep(NA, k); errorB  <<- rep(NA, k); errorK  <<- rep(NA, k);
    errorW <<- rep(NA, k); errorB3 <<- rep(NA, k); errorCH <<- rep(NA, k); errorLN <<- rep(NA, k); errorUL <<- rep(NA, k);
    errorLNB <<- rep(NA, k); errorLNK <<- rep(NA, k); errorUP  <<- rep(NA, k);
    
    
    Q1 <<- numeric(k-1)
    Q2 <<- numeric(k)
    Q2[k] <<- 1
    Q3 <<- numeric(k-1)
    
    
    Q1[1] <<- as.numeric(input$q1)
    Q2[1] <<- as.numeric(input$q2)
    Q3[1] <<- as.numeric(input$q3)
    
    for (i in 1:(k-1)) {
      local({
        cat <- i
        
        for (j in 2:(k-1)) {
          if (as.numeric(input[[paste("q1p", j)]]) <= as.numeric(input[[paste("q2p", j)]])) {
            Q1[j] <<- as.numeric(input[[paste("q1p", j)]])
            Q2[j] <<- as.numeric(input[[paste("q2p", j)]])
            Q3[j] <<- as.numeric(input[[paste("q3p", j)]])
          }
        }
        
        # dist <- input$
        
        
        #    Q1 <<- c(0.349, 0.253, 0.330, 0.296, 0.557)
        
        #    Q2 <<- c(0.446, 0.358, 0.413, 0.366, 0.688,1)
        
        #    Q3 <<- c(0.545, 0.449, 0.495, 0.438, 0.802)
        
        switch(input$distributionG, 
               
               
               "Continuous-Bernoulli" = { 
                 parsC <- 0.2
                 LB <- 0.001
                 
                 qfun <- function(x, a) {
                   if (a != 0.5) {
                     y <- (log(1 + x * (2 * a - 1) / (1 - a))) / log(a / (1 - a))
                   } else {
                     y <- x
                   }
                   return(y)
                 }
                 
                 funC <- function(a) {
                   (Q1[cat] - qfun(0.25, a))^2 + (Q2[cat] - qfun(0.5, a))^2 + (Q3[cat] - qfun(0.75, a))^2
                 }
                 
                 solC <- solnp(pars = parsC, fun = funC, LB = LB, control = list(tol = 1e-20))
                 
                 lambdaC[cat] <<- solC$pars[1]
                 errorCB[cat] <<- funC(solC$pars[1])
                 
                 output[[paste0("category_output1", cat)]] <- renderPrint({
                   cat("Category", cat)
                 })
                 
                 output[[paste0("CB_output", cat)]] <- renderPrint({
                   cat("\u03BB", cat, ":", lambdaC[cat])
                 })
                 
                 mean_error <- geometric.mean(errorCB[errorCB > 0], na.rm = TRUE)
                 output$aggregate_error <- renderPrint({
                   cat("Optimisation Error: ", round(mean_error, 6), sep = "")
                 })
                 
                 
                 output[[paste0("plotCB", cat)]] <- renderPlot({
                   xCB <- seq(0, 1, by = 0.001)
                   B <- function(xCB, lambda) {
                     D <- ifelse(lambda != 0.5, 2 * atanh(1 - 2 * lambda) / (1 - 2 * lambda), 2)
                     y <- (lambda)^xCB * ((1 - lambda)^(1 - xCB)) * D
                     return(y)
                   }
                   yCB <- B(xCB, lambdaC[cat])
                   plot(xCB, yCB, type = "l", col = "purple", lwd = 4, main = "PDF of Continuous-Bernoulli Distribution", xlab = "p", ylim = c(0, 3), ylab = "Prior Density")
                   Q1C = qfun(0.25, lambdaC[cat])
                   Q2C = qfun(0.5, lambdaC[cat])
                   Q3C = qfun(0.75, lambdaC[cat])
                   segments(Q1[cat], 0, Q1[cat], B(Q1[cat], lambdaC[cat]), col = "green", lwd = 4)
                   segments(Q2[cat], 0, Q2[cat], B(Q2[cat], lambdaC[cat]), col = "green", lwd = 4)
                   segments(Q3[cat], 0, Q3[cat], B(Q3[cat], lambdaC[cat]), col = "green", lwd = 4)
                   segments(Q1C, 0, Q1C, B(Q1C, lambdaC[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   segments(Q2C, 0, Q2C, B(Q2C, lambdaC[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   segments(Q3C, 0, Q3C, B(Q3C, lambdaC[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   
                   legend("topright", legend = c("Assessed quartiles", "Calculated Quartiles"), col = c("green", "orange4"), lty = c("solid", "dotted"), lwd = 3)
                 })
               },
               
               
               
               "Alpha-Unit" = { 
                 parsAU <- 0.2
                 LB <- 0.001
                 
                 QAUP <- function(x, a, p) {
                   pnorm(log(x) / a) - (log(x) / a) * dnorm(log(x) / a) - p / 2
                 }
                 
                 QAU<- function(p, a) {
                   uniroot(QAUP, interval = c(0.01, 10), a = a, p = p)$root
                 }
                 
                 funAU <- function(a) {
                   (Q1[cat] - QAU(0.25, a))^2 + (Q2[cat] - QAU(0.5, a))^2 + (Q3[cat] - QAU(0.75, a))^2
                 }
                 
                 solAU <- solnp(pars = parsAU, fun = funAU, LB = LB, control = list(tol = 1e-20))
                 
                 aAU[cat] <<- solAU$pars[1]
                 errorAU[cat] <<- funAU(solAU$pars[1])
                 
                 output[[paste0("category_output2", cat)]] <- renderPrint({
                   cat("Category", cat)
                 })
                 
                 output[[paste0("AU_output", cat)]] <- renderPrint({
                   cat("\u03B1", cat, ":", aAU[cat])
                 })
                 
                mean_error <- geometric.mean(errorAU[errorAU > 0], na.rm = TRUE)
                 output$aggregate_error <- renderPrint({
                   cat("Optimisation Error: ", round(mean_error, 6), sep = "")
                 })
                 
                 output[[paste0("plotAU", cat)]] <- renderPlot({
                   xAU <- seq(0, 1, by = 0.001)
                   AU <- function(xAU, aAU) {
                     2 / (xAU * aAU) * (log(xAU) / aAU)^2 * dnorm(log(xAU) / aAU)             
                   }
                   yAU <- AU(xAU , aAU[cat])
                   plot(xAU, yAU, type = "l", col = "purple", lwd = 4, main = "PDF of Alpha-Unit Distribution", xlab = "p", ylim = c(0, min(max(yAU, na.rm = TRUE) * 1.1, 50)), ylab = "Prior Density")
                   Q1AU = QAU(0.25, aAU[cat])
                   Q2AU = QAU(0.5, aAU[cat])
                   Q3AU = QAU(0.75, aAU[cat])
                   segments(Q1[cat], 0, Q1[cat], AU(Q1[cat], aAU[cat]), col = "green", lwd = 4)
                   segments(Q2[cat], 0, Q2[cat], AU(Q2[cat], aAU[cat]), col = "green", lwd = 4)
                   segments(Q3[cat], 0, Q3[cat], AU(Q3[cat], aAU[cat]), col = "green", lwd = 4)
                   segments(Q1AU, 0, Q1AU, AU(Q1AU, aAU[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   segments(Q2AU, 0, Q2AU, AU(Q2AU, aAU[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   segments(Q3AU, 0, Q3AU, AU(Q3AU, aAU[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   
                   legend("topright", legend = c("Assessed quartiles", "Calculated Quartiles"), col = c("green", "orange4"), lty = c("solid", "dotted"), lwd = 3)
                 })
               },
               
               
               "Topp-Leone" = { 
                 parsTL <- 0.2
                 LB <- 0.001
                 
                 QTL <- function(u, a){
                   1 - sqrt(1 - u^(1 / a))
                 }
                 
                 funTL <- function(a) {
                   (Q1[cat] - QTL(0.25, a))^2 + (Q2[cat] - QTL(0.5, a))^2 + (Q3[cat] - QTL(0.75, a))^2
                 }
                 
                 solTL <- solnp(pars = parsTL, fun = funTL, LB = LB, control = list(tol = 1e-20))
                 
                 aTL[cat] <<- solTL$pars[1]
                 errorTL[cat] <<- funTL(solTL$pars[1])
                 
                 
                 output[[paste0("category_output3", cat)]] <- renderPrint({
                   cat("Category", cat)
                 })
                 
                 output[[paste0("TL_output", cat)]] <- renderPrint({
                   cat("\u03B1", cat, ":", aTL[cat])
                 })
                 
                 mean_error <- geometric.mean(errorTL[errorTL > 0], na.rm = TRUE)
                 output$aggregate_error <- renderPrint({
                   cat("Optimisation Error: ", round(mean_error, 6), sep = "")
                 })
                 
                 output[[paste0("plotTL", cat)]] <- renderPlot({
                   xTL <- seq(0, 1, by = 0.001)
                   TL <- function(xTL, aTL){
                     2 * aTL * xTL^(aTL - 1) * (1 - xTL) * (2 - xTL)^(aTL - 1)
                   }
                   yTL <- TL(xTL , aTL[cat])
                   plot(xTL, yTL, type = "l", col = "purple", lwd = 4, main = "PDF of Topp-Leone Distribution", xlab = "p", ylim = c(0, min(max(yTL, na.rm = TRUE) * 1.1, 50)), ylab = "Prior Density")
                   Q1TL = QTL(0.25, aTL[cat])
                   Q2TL = QTL(0.5, aTL[cat])
                   Q3TL = QTL(0.75, aTL[cat])
                   segments(Q1[cat], 0, Q1[cat], TL(Q1[cat], aTL[cat]), col = "green", lwd = 4)
                   segments(Q2[cat], 0, Q2[cat], TL(Q2[cat], aTL[cat]), col = "green", lwd = 4)
                   segments(Q3[cat], 0, Q3[cat], TL(Q3[cat], aTL[cat]), col = "green", lwd = 4)
                   segments(Q1TL, 0, Q1TL, TL(Q1TL, aTL[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   segments(Q2TL, 0, Q2TL, TL(Q2TL, aTL[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   segments(Q3TL, 0, Q3TL, TL(Q3TL, aTL[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   
                   legend("topright", legend = c("Assessed quartiles", "Calculated Quartiles"), col = c("green", "orange4"), lty = c("solid", "dotted"), lwd = 3)
                 })
               },
               
               
               
               "Beta" = {
                 parsB <- c(0.5, 0.5)
                 LB <- c(0, 0)
                 
                 funB <- function(x) {
                   (Q1[cat] - qbeta(0.25, x[1], x[2]))^2 +
                     (Q2[cat] - qbeta(0.5, x[1], x[2]))^2 +
                     (Q3[cat] - qbeta(0.75, x[1], x[2]))^2
                 }
                 solB <- solnp(pars = parsB, fun = funB, LB = LB, control = list(tol = 1e-10))
                 
                 aB[cat] <<- solB$pars[1]
                 bB[cat] <<- solB$pars[2]
                 errorB[cat] <<- funB(solB$pars)
                 
                 output[[paste0("category_output4", cat)]] <- renderPrint({
                   cat("Category", cat)
                 })
                 
                 output[[paste0("aB_output", cat)]] <- renderPrint({
                   cat("\u03B1", cat, ":", aB[cat])
                 })
                 
                 output[[paste0("bB_output", cat)]] <- renderPrint({
                   cat("\u03B2", cat, ":", bB[cat])
                 })
                 
                 mean_error <- geometric.mean(errorB[errorB > 0], na.rm = TRUE)
                 output$aggregate_error <- renderPrint({
                   cat("Optimisation Error: ", round(mean_error, 6), sep = "")
                 })
                 
                 output[[paste0("plotB", cat)]] <- renderPlot({
                   xB <- seq(0, 1, 0.001)
                   yB <- dbeta(xB, aB[cat], bB[cat])
                   plot(xB, yB, type = "l", col = "purple", lwd = 4, xlab = "p", ylab = "Prior density", ylim = c(0, min(max(yB, na.rm = TRUE) * 1.1, 50)), main = paste("PDF of Beta distribution - Category", cat))
                   Q1B <- qbeta(0.25, aB[cat], bB[cat], ncp = 0, log = FALSE)
                   Q2B <- qbeta(0.5,  aB[cat], bB[cat], ncp = 0, log = FALSE)
                   Q3B <- qbeta(0.75, aB[cat], bB[cat], ncp = 0, log = FALSE)
                   segments(Q1[cat], 0, Q1[cat], dbeta(Q1[cat], aB[cat], bB[cat]), col = "green", lwd = 4)
                   segments(Q2[cat], 0, Q2[cat], dbeta(Q2[cat], aB[cat], bB[cat]), col = "green", lwd = 4)
                   segments(Q3[cat], 0, Q3[cat], dbeta(Q3[cat], aB[cat], bB[cat]), col = "green", lwd = 4)
                   segments(Q1B, 0, Q1B, dbeta(Q1B, aB[cat], bB[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   segments(Q2B, 0, Q2B, dbeta(Q2B, aB[cat], bB[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   segments(Q3B, 0, Q3B, dbeta(Q3B, aB[cat], bB[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   legend("topright", legend = c("Assessed quartiles", "Calculated Quartiles"), col = c("green", "orange4"), lty = c("solid", "dotted"), lwd = 3)
                 })
               }, 
               
               
               "Kumaraswamy" = {
                 parsK <- c(0.5, 0.5)
                 LB <- c(0, 0)       
                 UB <- c(1000, 1000)       
                 
                 
                 funK<-function(x){(Q1[cat]-qkumar(0.25, x[1], x[2]))^2 +
                     (Q2[cat]-qkumar(0.5, x[1], x[2]))^2 +
                     (Q3[cat]-qkumar(0.75, x[1], x[2]))^2 }
                 
                 solK=solnp(pars=parsK, fun=funK, LB = LB, UB=UB, control=list(tol=1e-20))
                 
                 aK[cat]<<-solK$pars[1]
                 bK[cat]<<-solK$pars[2]
                 errorK[cat] <<- funK(solK$pars)
                 
                 output[[paste0("category_output5", cat)]] <- renderPrint({
                   cat("Category", cat)
                 })
                 
                 output[[paste0("aK_output", cat)]] <- renderPrint({
                   cat("a", cat, ":", aK[cat])
                 })
                 
                 output[[paste0("bK_output", cat)]] <- renderPrint({
                   cat("b", cat, ":", bK[cat])
                 })
                 
                 mean_error <- geometric.mean(errorK[errorK > 0], na.rm = TRUE)
                 output$aggregate_error <- renderPrint({
                   cat("Optimisation Error: ", round(mean_error, 6), sep = "")
                 })
                 
                 output[[paste0("plotK", cat)]] <- renderPlot({
                   xK <- seq(0, 1, 0.001)
                   yK <- dkumar(xK, aK[cat], bK[cat])
                   plot(xK, yK, type = "l", col = "purple", lwd = 4, xlab = "p", ylab = "Prior density",ylim = c(0, min(max(yK, na.rm = TRUE) * 1.1, 50)),  main = paste("PDF of Kumaraswamy distribution - Category", cat))
                   Q1K <- qkumar(0.25, aK[cat], bK[cat])
                   Q2K <- qkumar(0.5,  aK[cat], bK[cat])
                   Q3K <- qkumar(0.75, aK[cat], bK[cat])
                   segments(Q1[cat], 0, Q1[cat], dkumar(Q1[cat], aK[cat], bK[cat]), col = "green", lwd = 4)
                   segments(Q2[cat], 0, Q2[cat], dkumar(Q2[cat], aK[cat], bK[cat]), col = "green", lwd = 4)
                   segments(Q3[cat], 0, Q3[cat], dkumar(Q3[cat], aK[cat], bK[cat]), col = "green", lwd = 4)
                   segments(Q1K, 0, Q1K, dkumar(Q1K, aK[cat], bK[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   segments(Q2K, 0, Q2K, dkumar(Q2K, aK[cat], bK[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   segments(Q3K, 0, Q3K, dkumar(Q3K, aK[cat], bK[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   legend("topright", legend = c("Assessed quartiles", "Calculated Quartiles"), col = c("green", "orange4"), lty = c("solid", "dotted"), lwd = 3)
                 })
                 
               },
               
               
               "Unit-Weibull" = {
                 parsW <- c(0.5, 0.5)
                 LB <- c(0, 0)
                 
                 funW <- function(x) {
                   (Q1[cat] - exp(-(-log(0.25) / x[1])^(1 / x[2])))^2 +
                     (Q2[cat] - exp(-(-log(0.5) / x[1])^(1 / x[2])))^2 +
                     (Q3[cat] - exp(-(-log(0.75) / x[1])^(1 / x[2])))^2
                 }
                 
                 solW <- solnp(pars = parsW, fun = funW, LB = LB, control = list(tol = 1e-10))
                 
                 pW[cat] <<- solW$pars[1]
                 qW[cat] <<- solW$pars[2]
                 errorW[cat] <<- funW(solW$pars)
                 
                 output[[paste0("category_output6", cat)]] <- renderPrint({
                   cat("Category", cat)
                 })
                 
                 output[[paste0("pUW_output", cat)]] <- renderPrint({
                   cat("p", cat, ":", pW[cat])
                 })
                 
                 output[[paste0("qUW_output", cat)]] <- renderPrint({
                   cat("q", cat, ":", qW[cat])
                 })
                 
                 mean_error <- geometric.mean(errorW[errorW > 0], na.rm = TRUE)
                 output$aggregate_error <- renderPrint({
                   cat("Optimisation Error: ", round(mean_error, 6), sep = "")
                 })
                 
                 
                 
                 output[[paste0("plotUW", cat)]] <- renderPlot({
                   xW <- seq(0.001, 1, 0.001)
                   U <- function(xW, pW, qW) {
                     (pW * qW) / xW * (-log(xW))^(qW - 1) * exp(-pW * (-log(xW))^qW)
                   }
                   yW <- U(xW, pW[cat], qW[cat])
                   plot(xW, yW, type = "l", col = "purple", lwd = 4, main = "PDF of Unit Weibull Distribution", xlab = "p", ylim = c(0, min(max(yW, na.rm = TRUE) * 1.1, 50)), ylab = "Prior Density")
                   Q1W = exp(-((-log(0.25)) / pW[cat])^(1 / qW[cat]))
                   Q2W = exp(-((-log(0.5)) / pW[cat])^(1 / qW[cat]))
                   Q3W = exp(-((-log(0.75)) / pW[cat])^(1 / qW[cat]))
                   segments(Q1[cat], 0, Q1[cat], U(Q1[cat], pW[cat], qW[cat]), col = "green", lwd = 4)
                   segments(Q2[cat], 0, Q2[cat], U(Q2[cat], pW[cat], qW[cat]), col = "green", lwd = 4)
                   segments(Q3[cat], 0, Q3[cat], U(Q3[cat], pW[cat], qW[cat]), col = "green", lwd = 4)
                   segments(Q1W, 0, Q1W, U(Q1W, pW[cat], qW[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   segments(Q2W, 0, Q2W, U(Q2W, pW[cat], qW[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   segments(Q3W, 0, Q3W, U(Q3W, pW[cat], qW[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   legend("topright", legend = c("Assessed quartiles", "Calculated Quartiles"), col = c("green", "orange4"), lty = c("solid", "dotted"), lwd = 2)
                   
                 })
                 
               },
               
               
               "Beta Type 3" = {
                 parsB3 <- c(0.5, 0.5)
                 LB <- c(0, 0)       
                 
                 
                 QB3  <- function(u, a, b) {
                   qbeta(u, a, b)/(2-qbeta(u, a, b))
                 }
                 
                 funB3<-function(x)
                 {(Q1[cat]-QB3(0.25, x[1], x[2]))^2 +
                     (Q2[cat]-QB3(0.5, x[1], x[2]))^2 +
                     (Q3[cat]-QB3(0.75, x[1], x[2]))^2 
                 }
                 
                 solB3=solnp(pars=parsB3, fun=funB3, LB = LB, control=list(tol=1e-20))
                 
                 aB3[cat]<<-solB3$pars[1]
                 bB3[cat]<<-solB3$pars[2]
                 errorB3[cat] <<- funB3(solB3$pars)
                 
                 
                 output[[paste0("category_output7", cat)]] <- renderPrint({
                   cat("Category", cat)
                 })
                 
                 output[[paste0("aB3_output", cat)]] <- renderPrint({
                   cat("a", cat, ":", aB3[cat])
                 })
                 
                 output[[paste0("bB3_output", cat)]] <- renderPrint({
                   cat("b", cat, ":", bB3[cat])
                 })
                 
                 mean_error <- geometric.mean(errorB3[errorB3 > 0], na.rm = TRUE)
                 output$aggregate_error <- renderPrint({
                   cat("Optimisation Error: ", round(mean_error, 6), sep = "")
                 })
                 
                 output[[paste0("plotB3", cat)]] <- renderPlot({
                   xB3 <- seq(0, 1, 0.001)
                   
                   B3 <- function(xB3, aB3, bB3) {
                     (2/(xB3+1)^2) * dbeta((2*xB3)/(1+xB3), aB3, bB3)
                   }
                   
                   yB3 <- B3(xB3, aB3[cat], bB3[cat])
                   
                   plot(xB3, yB3, type = "l", col = "purple", lwd = 4, xlab = "p", ylab = "Prior density", ylim = c(0, min(max(yB3, na.rm = TRUE) * 1.1, 50)), main = paste("PDF of Beta Type 3 distribution - Category", cat))
                   Q1B3 <- QB3(0.25, aB3[cat], bB3[cat])
                   Q2B3 <- QB3(0.5,  aB3[cat], bB3[cat])
                   Q3B3 <- QB3(0.75, aB3[cat], bB3[cat])
                   segments(Q1[cat], 0, Q1[cat], B3(Q1[cat], aB3[cat], bB3[cat]), col = "green", lwd = 4)
                   segments(Q2[cat], 0, Q2[cat], B3(Q2[cat], aB3[cat], bB3[cat]), col = "green", lwd = 4)
                   segments(Q3[cat], 0, Q3[cat], B3(Q3[cat], aB3[cat], bB3[cat]), col = "green", lwd = 4)
                   segments(Q1B3, 0, Q1B3, B3(Q1B3, aB3[cat], bB3[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   segments(Q2B3, 0, Q2B3, B3(Q2B3, aB3[cat], bB3[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   segments(Q3B3, 0, Q3B3, B3(Q3B3, aB3[cat], bB3[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   legend("topright", legend = c("Assessed quartiles", "Calculated Quartiles"), col = c("green", "orange4"), lty = c("solid", "dotted"), lwd = 3)
                 })
                 
               },
               
               
               "Unit-Chen" = {
                 parsCH <- c(0.5, 0.5)
                 LB <- c(0, 0)       
                 QCH <- function(u, a, b) {
                   exp(-((log(1 - log(u)/a))^(1/b)))
                   
                 }
                 
                 funCH<-function(x) {(Q1[cat]-QCH(0.25, x[1], x[2]))^2 +
                     (Q2[cat]-QCH(0.5, x[1], x[2]))^2 +
                     (Q3[cat]-QCH(0.75, x[1], x[2]))^2 
                 }
                 
                 solCH <- solnp(pars=parsCH, fun=funCH, LB = LB, control=list(tol=1e-20))
                 
                 aCH[cat]<<-solCH$pars[1]
                 bCH[cat]<<-solCH$pars[2]
                 errorCH[cat] <<- funCH(solCH$pars)
                 
                 
                 output[[paste0("category_output8", cat)]] <- renderPrint({
                   cat("Category", cat)
                 })
                 
                 output[[paste0("aCH_output", cat)]] <- renderPrint({
                   cat("a", cat, ":", aCH[cat])
                 })
                 
                 output[[paste0("bCH_output", cat)]] <- renderPrint({
                   cat("b", cat, ":", bCH[cat])
                 })
                 
                 mean_error <- geometric.mean(errorCH[errorCH > 0], na.rm = TRUE)
                 output$aggregate_error <- renderPrint({
                   cat("Optimisation Error: ", round(mean_error, 6), sep = "")
                 })
                 
                 output[[paste0("plotCH", cat)]] <- renderPlot({
                   xCH <- seq(0, 1, 0.001)
                   
                   CH <- function(xCH, aCH, bCH) {
                     (aCH * bCH / xCH) * ((-log(xCH))^(bCH - 1)) * exp((-log(xCH))^bCH) * exp(aCH * (1 - exp((-log(xCH))^bCH)))
                   }
                   
                   yCH <- CH(xCH, aCH[cat], bCH[cat])
                   plot(xCH, yCH, type = "l", col = "purple", lwd = 4, xlab = "p", ylab = "Prior density", ylim = c(0, min(max(yCH, na.rm = TRUE) * 1.1, 50)), main = paste("PDF of Unit-Chen distribution - Category", cat))
                   Q1CH <- QCH(0.25, aCH[cat], bCH[cat])
                   Q2CH <- QCH(0.5,  aCH[cat], bCH[cat])
                   Q3CH <- QCH(0.75, aCH[cat], bCH[cat])
                   segments(Q1[cat], 0, Q1[cat], CH(Q1[cat], aCH[cat], bCH[cat]), col = "green", lwd = 4)
                   segments(Q2[cat], 0, Q2[cat], CH(Q2[cat], aCH[cat], bCH[cat]), col = "green", lwd = 4)
                   segments(Q3[cat], 0, Q3[cat], CH(Q3[cat], aCH[cat], bCH[cat]), col = "green", lwd = 4)
                   segments(Q1CH, 0, Q1CH, CH(Q1CH, aCH[cat], bCH[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   segments(Q2CH, 0, Q2CH, CH(Q2CH, aCH[cat], bCH[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   segments(Q3CH, 0, Q3CH, CH(Q3CH, aCH[cat], bCH[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   legend("topright", legend = c("Assessed quartiles", "Calculated Quartiles"), col = c("green", "orange4"), lty = c("solid", "dotted"), lwd = 3)
                 })
                 
               },
               
               "Logit-Normal" = {
                 parsLN <- c(0.5, 0.5)
                 LB1 <- c(-10, 0)       
                 
                 funLN<-function(x){(Q1[cat]-qlogitnorm(0.25, x[1], x[2]))^2 +
                     (Q2[cat]-qlogitnorm(0.5, x[1], x[2]))^2 +
                     (Q3[cat]-qlogitnorm(0.75, x[1], x[2]))^2 }
                 
                 solLN <- solnp(pars=parsLN, fun=funLN, LB = LB1, control=list(tol=1e-20))
                 
                 aLN[cat]<<-solLN$pars[1]
                 bLN[cat]<<-solLN$pars[2]
                 errorLN[cat] <<- funLN(solLN$pars)
                 
                 output[[paste0("category_output9", cat)]] <- renderPrint({
                   cat("Category", cat)
                 })
                 
                 output[[paste0("aLN_output", cat)]] <- renderPrint({
                   cat("a", cat, ":", aLN[cat])
                 })
                 
                 output[[paste0("bLN_output", cat)]] <- renderPrint({
                   cat("b", cat, ":", bLN[cat])
                 })
                 
                 mean_error <- geometric.mean(errorLN[errorLN > 0], na.rm = TRUE)
                 output$aggregate_error <- renderPrint({
                   cat("Optimisation Error: ", round(mean_error, 6), sep = "")
                 })
                 
                 
                 
                 output[[paste0("plotLN", cat)]] <- renderPlot({
                   xLN <- seq(0, 1, 0.001)
                   yLN <- dlogitnorm(xLN, aLN[cat], bLN[cat])
                   plot(xLN, yLN, type = "l", col = "purple", lwd = 4, xlab = "p", ylab = "Prior density", ylim = c(0, min(max(yLN, na.rm = TRUE) * 1.1, 50)), main = paste("PDF of Logit-Normal distribution - Category", cat))
                   Q1LN <- qlogitnorm(0.25, aLN[cat], bLN[cat])
                   Q2LN <- qlogitnorm(0.5,  aLN[cat], bLN[cat])
                   Q3LN <- qlogitnorm(0.75, aLN[cat], bLN[cat])
                   segments(Q1[cat], 0, Q1[cat], dlogitnorm(Q1[cat], aLN[cat], bLN[cat]), col = "green", lwd = 4)
                   segments(Q2[cat], 0, Q2[cat], dlogitnorm(Q2[cat], aLN[cat], bLN[cat]), col = "green", lwd = 4)
                   segments(Q3[cat], 0, Q3[cat], dlogitnorm(Q3[cat], aLN[cat], bLN[cat]), col = "green", lwd = 4)
                   segments(Q1LN, 0, Q1LN, dlogitnorm(Q1LN, aLN[cat], bLN[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   segments(Q2LN, 0, Q2LN, dlogitnorm(Q2LN, aLN[cat], bLN[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   segments(Q3LN, 0, Q3LN, dlogitnorm(Q3LN, aLN[cat], bLN[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   legend("topright", legend = c("Assessed quartiles", "Calculated Quartiles"), col = c("green", "orange4"), lty = c("solid", "dotted"), lwd = 3)
                 })
                 
               },
               
               
               "Unit-Logistic" = {
                 parsUL <- c(0.5, 0.5)
                 LB <- c(0, 0)       
                 
                 QUL <- function(u, a, b) {
                #   (a * u^(1/b)) / ((1 - a) * (1 - u)^(1/b) + a * u^(1/b))
                   exp(-mu/b) * (u/(1-u))^(1/b) / (1 + exp(-mu/b) * (u/(1-u))^(1/b))
                  
                 }
                 
                 funUL<-function(x)
                 {(Q1[cat]-QUL(0.25, x[1], x[2]))^2 +
                     (Q2[cat]-QUL(0.5, x[1], x[2]))^2 +
                     (Q3[cat]-QUL(0.75, x[1], x[2]))^2 
                 }
                 
                 solUL <- solnp(pars=parsUL, fun=funUL, LB = LB, control=list(tol=1e-20))
                 
                 aUL[cat]<<-solUL$pars[1]
                 bUL[cat]<<-solUL$pars[2]
                 errorUL[cat] <<- funUL(solUL$pars)
                 
                 output[[paste0("category_output10", cat)]] <- renderPrint({
                   cat("Category", cat)
                 })
                 
                 output[[paste0("aUL_output", cat)]] <- renderPrint({
                   cat("a", cat, ":", aUL[cat])
                 })
                 
                 output[[paste0("bUL_output", cat)]] <- renderPrint({
                   cat("b", cat, ":", bUL[cat])
                 })
                 
                 mean_error <- geometric.mean(errorUL[errorUL > 0], na.rm = TRUE)
                 output$aggregate_error <- renderPrint({
                   cat("Optimisation Error: ", round(mean_error, 6), sep = "")
                 })
                 
                 output[[paste0("plotUL", cat)]] <- renderPlot({
                   xUL <- seq(0, 1, 0.001)
                   
                   UL <- function(xUL, aUL, bUL) {
                     (bUL * aUL^bUL * xUL^(bUL - 1) * (1 - aUL)^bUL * (1 - xUL)^(bUL - 1)) / 
                       ((1 - aUL)^bUL * xUL^bUL + aUL^bUL * (1 - xUL)^bUL)^2
                   }
                   
                   yUL <- UL(xUL, aUL[cat], bUL[cat])
                   plot(xUL, yUL, type = "l", col = "purple", lwd = 4, xlab = "p", ylab = "Prior density",ylim = c(0, min(max(yUL, na.rm = TRUE) * 1.1, 50)), main = paste("PDF of Unit-Logistic distribution - Category", cat))
                   Q1UL <- QUL(0.25, aUL[cat], bUL[cat])
                   Q2UL <- QUL(0.5,  aUL[cat], bUL[cat])
                   Q3UL <- QUL(0.75, aUL[cat], bUL[cat])
                   segments(Q1[cat], 0, Q1[cat], UL(Q1[cat], aUL[cat], bUL[cat]), col = "green", lwd = 4)
                   segments(Q2[cat], 0, Q2[cat], UL(Q2[cat], aUL[cat], bUL[cat]), col = "green", lwd = 4)
                   segments(Q3[cat], 0, Q3[cat], UL(Q3[cat], aUL[cat], bUL[cat]), col = "green", lwd = 4)
                   segments(Q1UL, 0, Q1UL, UL(Q1UL, aUL[cat], bUL[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   segments(Q2UL, 0, Q2UL, UL(Q2UL, aUL[cat], bUL[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   segments(Q3UL, 0, Q3UL, UL(Q3UL, aUL[cat], bUL[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   legend("topright", legend = c("Assessed quartiles", "Calculated Quartiles"), col = c("green", "orange4"), lty = c("solid", "dotted"), lwd = 3)
                 })
                 
               },
               
               
               "Libby-Novick Beta" = {
                 parsLNB <- c(0.5, 0.5, 0.5)
                 LB <- c(0, 0, 0)       
                 
                 QLNB <- function(x, a, b, c) {
                   qbeta(x, a, b)/(c+(1-c)*qbeta(x, a, b))
                 }
                 
                 funLNB<-function(x)
                 {(Q1[cat]-QLNB(0.25, x[1], x[2], x[3]))^2 +
                     (Q2[cat]-QLNB(0.5, x[1], x[2], x[3]))^2 +
                     (Q3[cat]-QLNB(0.75, x[1], x[2], x[3]))^2 
                 }
                 
                 solLNB <- solnp(pars=parsLNB, fun=funLNB, LB = LB, control=list(tol=1e-20))
                 
                 aLNB[cat]<<-solLNB$pars[1]
                 bLNB[cat]<<-solLNB$pars[2]
                 cLNB[cat]<<-solLNB$pars[3]
                 errorLNB[cat] <<- funLNB(solLNB$pars)
                 
                 
                 output[[paste0("category_output11", cat)]] <- renderPrint({
                   cat("Category", cat)
                 })
                 
                 output[[paste0("aLNB_output", cat)]] <- renderPrint({
                   cat("a", cat, ":", aLNB[cat])
                 })
                 
                 output[[paste0("bLNB_output", cat)]] <- renderPrint({
                   cat("b", cat, ":", bLNB[cat])
                 })
                 
                 output[[paste0("cLNB_output", cat)]] <- renderPrint({
                   cat("c", cat, ":", cLNB[cat])
                 })
                 
                mean_error <- geometric.mean(errorLNB[errorLNB > 0], na.rm = TRUE)
                 output$aggregate_error <- renderPrint({
                   cat("Optimisation Error: ", round(mean_error, 6), sep = "")
                 })
                 
                 output[[paste0("plotLNB", cat)]] <- renderPlot({
                   xLNB <- seq(0, 1, 0.001)
                   
                   
                   LNB <- function(xLNB, aLNB, bLNB, cLNB) {
                     (cLNB/(1+xLNB*(cLNB-1))^2) * dbeta((cLNB*xLNB)/(1+(cLNB*xLNB)-xLNB), aLNB, bLNB)
                     
                   }
                   
                   yLNB <- LNB(xLNB, aLNB[cat], bLNB[cat], cLNB[cat])
                   plot(xLNB, yLNB, type = "l", col = "purple", lwd = 4, xlab = "p", ylab = "Prior density",ylim = c(0, min(max(yLNB, na.rm = TRUE) * 1.1, 50)), main = paste("PDF of Libby-Novick Beta distribution - Category", cat))
                   Q1LNB <- QLNB(0.25, aLNB[cat], bLNB[cat], cLNB[cat])
                   Q2LNB <- QLNB(0.5,  aLNB[cat], bLNB[cat], cLNB[cat])
                   Q3LNB <- QLNB(0.75, aLNB[cat], bLNB[cat], cLNB[cat])
                   segments(Q1[cat], 0, Q1[cat], LNB(Q1[cat], aLNB[cat], bLNB[cat], cLNB[cat]), col = "green", lwd = 4)
                   segments(Q2[cat], 0, Q2[cat], LNB(Q2[cat], aLNB[cat], bLNB[cat], cLNB[cat]), col = "green", lwd = 4)
                   segments(Q3[cat], 0, Q3[cat], LNB(Q3[cat], aLNB[cat], bLNB[cat], cLNB[cat]), col = "green", lwd = 4)
                   segments(Q1LNB, 0, Q1LNB, LNB(Q1LNB, aLNB[cat], bLNB[cat], cLNB[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   segments(Q2LNB, 0, Q2LNB, LNB(Q2LNB, aLNB[cat], bLNB[cat], cLNB[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   segments(Q3LNB, 0, Q3LNB, LNB(Q3LNB, aLNB[cat], bLNB[cat], cLNB[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   legend("topright", legend = c("Assessed quartiles", "Calculated Quartiles"), col = c("green", "orange4"), lty = c("solid", "dotted"), lwd = 3)
                 })
                 
               },
               
               
               
               "Libby-Novick Kumaraswamy" = {
                 parsLNK <- c(0.5, 0.5, 0.5)
                 LB <- c(0, 0, 0)       
                 
                 QLNK <-function(u, a, b, c) {
                   ((1 - (1 - u)^(1 / b)) / (1 - (1 - c) * (1 - u)^(1 / b)))^(1 / a)
                 }
                 
                 
                 funLNK<-function(x) {(Q1[cat]-QLNK(0.25, x[1], x[2], x[3]))^2 +
                     (Q2[cat]-QLNK(0.5, x[1], x[2], x[3]))^2 +
                     (Q3[cat]-QLNK(0.75, x[1], x[2], x[3]))^2 
                 }
                 
                 solLNK <- solnp(pars=parsLNK, fun=funLNK, LB = LB, control=list(tol=1e-20))
                 
                 aLNK[cat]<<-solLNK$pars[1]
                 bLNK[cat]<<-solLNK$pars[2]
                 cLNK[cat]<<-solLNK$pars[3]
                 errorLNK[cat] <<- funLNK(solLNK$pars)
                 
                 output[[paste0("category_output12", cat)]] <- renderPrint({
                   cat("Category", cat)
                 })
                 
                 output[[paste0("aLNK_output", cat)]] <- renderPrint({
                   cat("a", cat, ":", aLNK[cat])
                 })
                 
                 output[[paste0("bLNK_output", cat)]] <- renderPrint({
                   cat("b", cat, ":", bLNK[cat])
                 })
                 
                 output[[paste0("cLNK_output", cat)]] <- renderPrint({
                   cat("c", cat, ":", cLNK[cat])
                 })
                 
                 mean_error <- geometric.mean(errorLNK[errorLNK > 0], na.rm = TRUE)
                 output$aggregate_error <- renderPrint({
                   cat("Optimisation Error: ", round(mean_error, 6), sep = "")
                 })
                 
                 output[[paste0("plotLNK", cat)]] <- renderPlot({
                   xLNK <- seq(0, 1, 0.001)
                   
                   
                   LNK <- function(xLNK, aLNK, bLNK, cLNK) {
                     aLNK*bLNK*cLNK*xLNK^(aLNK-1)*(1+(cLNK*xLNK^aLNK)/(1-xLNK^aLNK))^(1-bLNK)/(1-(1-cLNK)*xLNK^aLNK)^2
                   }
                   
                   yLNK <- LNK(xLNK, aLNK[cat], bLNK[cat], cLNK[cat])
                   plot(xLNK, yLNK, type = "l", col = "purple", lwd = 4, xlab = "p",ylim = c(0, min(max(yLNK, na.rm = TRUE) * 1.1, 50)), ylab = "Prior density", main = paste("PDF of Libby-Novick Kumaraswamy distribution - Category", cat))
                   Q1LNK <- QLNK(0.25, aLNK[cat], bLNK[cat], cLNK[cat])
                   Q2LNK <- QLNK(0.5,  aLNK[cat], bLNK[cat], cLNK[cat])
                   Q3LNK <- QLNK(0.75, aLNK[cat], bLNK[cat], cLNK[cat])
                   segments(Q1[cat], 0, Q1[cat], LNK(Q1[cat], aLNK[cat], bLNK[cat], cLNK[cat]), col = "green", lwd = 4)
                   segments(Q2[cat], 0, Q2[cat], LNK(Q2[cat], aLNK[cat], bLNK[cat], cLNK[cat]), col = "green", lwd = 4)
                   segments(Q3[cat], 0, Q3[cat], LNK(Q3[cat], aLNK[cat], bLNK[cat], cLNK[cat]), col = "green", lwd = 4)
                   segments(Q1LNK, 0, Q1LNK, LNK(Q1LNK, aLNK[cat], bLNK[cat], cLNK[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   segments(Q2LNK, 0, Q2LNK, LNK(Q2LNK, aLNK[cat], bLNK[cat], cLNK[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   segments(Q3LNK, 0, Q3LNK, LNK(Q3LNK, aLNK[cat], bLNK[cat], cLNK[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   legend("topright", legend = c("Assessed quartiles", "Calculated Quartiles"), col = c("green", "orange4"), lty = c("solid", "dotted"), lwd = 3)
                 })
                 
               },
               
               
               "Unit-Exponential Pareto" = {
                 parsUP <- c(0.5, 0.5, 0.5)
                 LB <- c(0, 0, 0)       
                 
                 QUP <- function(u, a, b, c){
                   1 / (1 + (1/b) * ((log(1 - u))/(-c))^(-1/a))
                 }
                 
                 funUP<-function(x)
                 {(Q1[cat]-QUP(0.25, x[1], x[2], x[3]))^2 +
                     (Q2[cat]-QUP(0.5, x[1], x[2], x[3]))^2 +
                     (Q3[cat]-QUP(0.75, x[1], x[2], x[3]))^2 
                 }
                 
                 solUP <- solnp(pars=parsUP, fun=funUP, LB = LB, control=list(tol=1e-20))
                 
                 aUP[cat]<<-solUP$pars[1]
                 bUP[cat]<<-solUP$pars[2]
                 cUP[cat]<<-solUP$pars[3]
                 errorUP[cat] <<- funUP(solUP$pars)
                 
                 output[[paste0("category_output13", cat)]] <- renderPrint({
                   cat("Category", cat)
                 })
                 
                 output[[paste0("aUP_output", cat)]] <- renderPrint({
                   cat("a", cat, ":", aUP[cat])
                 })
                 
                 output[[paste0("bUP_output", cat)]] <- renderPrint({
                   cat("b", cat, ":", bUP[cat])
                 })
                 
                 output[[paste0("cUP_output", cat)]] <- renderPrint({
                   cat("c", cat, ":", cUP[cat])
                 })
                 
                 mean_error <- geometric.mean(errorUP[errorUP > 0], na.rm = TRUE)
                 output$aggregate_error <- renderPrint({
                   cat("Optimisation Error: ", round(mean_error, 6), sep = "")
                 })
                 print(mean_error)
                 
                 output[[paste0("plotUP", cat)]] <- renderPlot({
                   xUP <- seq(0, 1, 0.001)
                   
                   UP <- function(xUP, aUP, bUP, cUP){
                     aUP * cUP * (xUP / (bUP * (1 - xUP)))^(aUP- 1) * exp(-cUP * (xUP / (bUP * (1 - xUP)))^aUP) * (1 / (bUP * (1 - xUP)^2)) 
                   }
                   
                   yUP <- UP(xUP, aUP[cat], bUP[cat], cUP[cat])
                   plot(xUP, yUP, type = "l", col = "purple", lwd = 4, xlab = "p", ylab = "Prior density",ylim = c(0, min(max(yUP, na.rm = TRUE) * 1.1, 50)), main = paste("PDF of Unit-Exponential Pareto distribution - Category", cat))
                   Q1UP <- QUP(0.25, aUP[cat], bUP[cat], cUP[cat])
                   Q2UP <- QUP(0.5,  aUP[cat], bUP[cat], cUP[cat])
                   Q3UP <- QUP(0.75, aUP[cat], bUP[cat], cUP[cat])
                   segments(Q1[cat], 0, Q1[cat], UP(Q1[cat], aUP[cat], bUP[cat], cUP[cat]), col = "green", lwd = 4)
                   segments(Q2[cat], 0, Q2[cat], UP(Q2[cat], aUP[cat], bUP[cat], cUP[cat]), col = "green", lwd = 4)
                   segments(Q3[cat], 0, Q3[cat], UP(Q3[cat], aUP[cat], bUP[cat], cUP[cat]), col = "green", lwd = 4)
                   segments(Q1UP, 0, Q1UP, UP(Q1UP, aUP[cat], bUP[cat], cUP[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   segments(Q2UP, 0, Q2UP, UP(Q2UP, aUP[cat], bUP[cat], cUP[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   segments(Q3UP, 0, Q3UP, UP(Q3UP, aUP[cat], bUP[cat], cUP[cat]), col = "orange4", lwd = 4, lty = "dotted")
                   legend("topright", legend = c("Assessed quartiles", "Calculated Quartiles"), col = c("green", "orange4"), lty = c("solid", "dotted"), lwd = 3)
                 })
                 
               },
               
               
               "JQPDB" = {
                 
                 #   QJ <- qJQPDB(p, Q1, Q2, Q3, 0, 1, alpha = 0.25)
                 
                 aJ[cat]<<-Q1[cat]
                 bJ[cat]<<-Q2[cat]
                 cJ[cat]<<-Q3[cat]
                 
                 output[[paste0("category_output14", cat)]] <- renderPrint({
                   cat("Category", cat)
                 })
                 
                 output[[paste0("aJ_output", cat)]] <- renderPrint({
                   cat("a", cat, ":", aJ[cat])
                 })
                 
                 output[[paste0("bJ_output", cat)]] <- renderPrint({
                   cat("b", cat, ":", bJ[cat])
                 })
                 
                 output[[paste0("cJ_output", cat)]] <- renderPrint({
                   cat("c", cat, ":", cJ[cat])
                 })
                 
                 output[[paste0("plotJ", cat)]] <- renderPlot({
                   xJ <- seq(0, 1, 0.001)
                   
                   yJ <- dJQPDB(xJ, Q1[cat], Q2[cat], Q3[cat], lower = 0, upper = 1, alpha = 0.25)
                   
                   plot(xJ, yJ, type = "l", col = "purple", lwd = 4, xlab = "p", ylab = "Prior density",ylim = c(0, min(max(yJ, na.rm = TRUE) * 1.1, 50)), main = paste("PDF of J-QPD-B distribution - Category", cat))
                   
                   segments(Q1[cat], 0, Q1[cat], dJQPDB(Q1[cat], aJ[cat], bJ[cat], cJ[cat], lower = 0, upper = 1, alpha = 0.25), col = "orange4", lwd = 4)
                   segments(Q2[cat], 0, Q2[cat], dJQPDB(Q2[cat], aJ[cat], bJ[cat], cJ[cat], lower = 0, upper = 1, alpha = 0.25), col = "orange4", lwd = 4)
                   segments(Q3[cat], 0, Q3[cat], dJQPDB(Q3[cat], aJ[cat], bJ[cat], cJ[cat], lower = 0, upper = 1, alpha = 0.25), col = "orange4", lwd = 4)
                   
                   legend("topright", legend = "Assessed quartiles", col = "orange4", lty = "solid", lwd = 3)
                 })
                 
                 
               },
               
        ) # end of switch 
        
      })   # local
      
      
    }   # for loop
    
    
    #   aB <<- c(5.353, 3.906, 6.819, 7.838, 4.531)
    
    #    bB <<- c(6.577, 6.934, 9.607, 13.344, 2.227)
    
    
    # Conditional quartile assessments
    
    output$conditional_quartiles <- renderUI({
      k <- as.numeric(input$k)
      
      conditional_quartiles <- list()
      Q2star <<- numeric(k)
      Lmin <<- numeric(k)
      Umax <<- numeric(k)
      Q1Sstar <<- numeric(k-1)
      Q2Sstar <<- numeric(k-1)
      Q3Sstar <<- numeric(k-1)
      cond <<- numeric(k-1)
      Q1hash <<- numeric(k-1)
      Q3hash <<- numeric(k-1)
      etahash <<- numeric(k-1)
      etastar <<- numeric(k-1)
      
      # Conditional assumption values
      
      Q2star <<- sapply(1:(k), function(i) {
        if (i == 1) {
          return(Q2[i])
        } else {
          return(Q2[i] * prod(1 - Q2[1:(i-1)]))
        }
      })
      
      Lmin <<- sapply(1:(k), function(i) {
        if (i == 1) {
          return(Q1[i])
        } else {
          return(Q1[i] * prod(1 - Q2[1:(i-1)]))
        }
      })
      
      Umax <<- sapply(1:(k), function(i) {
        if (i == 1) {
          return(Q3[i])
        } else {
          return(Q3[i] * prod(1 - Q2[1:(i-1)]))
        }
      })
      
      
      for (i in 2:(k - 1)) {
        
        cond[i] <<- 1 - sum(Q2star[1:(i-1)])     ##HERE
        
      }
      
      
      
      
      for (i in 1:(k-1)) {
        if (i == 1) {
          Q1Sstar[i] <<- Q1[1]
        }
        else{
          #  Q1Sstar[i] <<- (Q2star[i]- (min(Q2star[i], cond[i] - Q2star[i]))/2)
          Q1Sstar[i] <<- (Q2star[i]- (Q2star[i]- Lmin[i])/2)
        }
      } 
      
      
      for (i in 1:(k-1)) {
        if (i == 1) {
          Q3Sstar[i] <<- Q3[1]
        }
        else{
          #  Q3Sstar[i] <<- (Q2star[i]+ (min(Q2star[i], cond[i] - Q2star[i]))/2)
          Q3Sstar[i] <<- (Q2star[i] + (min(cond[i], Umax[i]) - Q2star[i])/2)
        }
      }
      
      
      
      #    Q1Sstar <<- c(Q1Sstar[1], 0.163, 0.132, 0.067, 0.081)   ##HERE
      
      #    Q3Sstar <<- c(Q3Sstar[1], 0.230, 0.163, 0.086, 0.100)   ##HERE
      
      # Conditioned quartiles
      
      
      
      
      
      # Conditional heading for quartiles
      
      for (i in 2:(k - 1)) {
        
        conditional_value <- paste0("p", i, "| ")
        
        for (j in 1:(i - 1)) {
          if (j == 1) {
            conditional_value <- paste0(conditional_value, "p", j, "=", round(Q2star[j], 3))
          } else {
            conditional_value <- paste0(conditional_value, ", p", j, "=", round(Q2star[j], 3))
          }
        }
        
        conditional_quartiles[[i]] <- tagList(
          h4(tags$strong(paste("Quartile values for", conditional_value)), style = "font-family: Candara, Calibri, sans-serif; font-weight: bold;color:#8B1A1A"),
          fluidRow(
            column(5, numericInput(paste0("q1q", i), "Q1*:", value = round(Q1Sstar[i], 3),  step= 0.001, min= round(Lmin[i], 3), max = round(Q2star[i], 3))),
            column(5, numericInput(paste0("q3q", i), "Q3*:", value = round(Q3Sstar[i], 3), step= 0.001, min= round(Q2star[i], 3), max = round(min(cond[i], Umax[i]))))
          )
        )
      }
      
      
      
      tagList(conditional_quartiles)
    })
    
    #  Q1Sstar <- 
    
    
    
    # Conditional median headings
    
    output$conditional_medians <- renderUI({
      
      k <- as.numeric(input$k)
      
      
      
      conditional_medians <- list()
      
      mvalues <- rep(0, k-1)
      
      Q2Sstar <<- numeric(k)
      
      SS <<- numeric(k)
      
      m1 <- matrix(0, nrow = k-1, ncol = k-1)
      
      Q1Sstar <<- numeric(k-1)
      
      Q3Sstar <<- numeric(k-1)
      
      
      
      
      
      
      
      Q1Sstar[1] <<- Q1[1]
      
      Q3Sstar[1] <<- Q3[1]
      
      for (i in 2:(k-1)) {
        
        Q1Sstar[i] <<- as.numeric(input[[paste0("q1q", i)]])
        
        print(Q1Sstar[i])
        
        Q3Sstar[i] <<- as.numeric(input[[paste0("q3q", i)]])
        
        print("Q3Sstar")
        
        print(Q3Sstar[i])
        
      }
      
      
      
      #    Q1Sstar <<- c(Q1Sstar[1], 0.163, 0.132, 0.067, 0.081)
      
      #    Q3Sstar <<- c(Q3Sstar[1], 0.230, 0.163, 0.086, 0.100)
      
      etastar[1] <<- Q1Sstar[1] - Q2star[1]
      etahash[1] <<- etastar[1]
      for (i in 2:(k - 1)) {
        Q1hash[i] <<- Q1Sstar[i] / cond[i]
        etastar[i] <<- Q1Sstar[i] - Q2star[i]
        etahash[i] <<- (Q1Sstar[i] - Q2star[i]) / cond[i]    # Q1Sstar[i] - Q2star[i]) - etastar
        Q3hash[i] <<- Q3Sstar[i] / cond[i]
        
      }
      print("etahash")
      print(etahash)
      
      
      
      for (i in 1:(k - 2)) {
        median_block <- list()  # collect rows for this i-level conditioning
        
        for (j in (i + 1):k) {
          
          if (i == 1) {
            conditional_value <- paste("Median value for", paste0("p", j, "|p1=", round(Q1Sstar[1], 3), sep = " "))
            mvalues <- c(Q1Sstar[i])
          } else {
            mvalues <- c(Q2star[1:(i - 1)], Q1Sstar[i])
            conditional_value <- paste(
              "Median value for",
              paste0("p", j, "|", paste0("p", 1:i, "=", round(mvalues, 3), collapse = ", "), sep = " ")
            )
          }
          
          SS[i] <<- sum(mvalues)
          Q2Sstar[j] <<- (1 - SS[i]) * Q2star[j] / sum(Q2star[(i + 1):k])
          
          median_inputs <- tagList(
            h4(tags$strong(conditional_value),
               style = "font-family: Candara, Calibri, sans-serif; font-weight: bold;color:#8B1A1A"),
            fluidRow(
              column(5, numericInput(paste0("q2m", j, i), "Q2*:", value = round(Q2Sstar[j], 3), step = 0.1, min=0, max = 0.9))
            )
          )
          
          median_block <- c(median_block, list(median_inputs))
        }
        
        conditional_medians <- c(
          conditional_medians,
          median_block,
          list(tags$hr(style = "border: none; height: 2px; background: linear-gradient(to right, #d2691e, #ffdead); margin: 20px 0;"))
        )
      }
      
      
      
      
      tagList(conditional_medians)
      
    })
    
    
    # Q2Sstar[1]=0.876; 
    
    # Correlation martix Calculation
    
    
    observeEvent(input$calculate_button2, {
      
      
      
      S <<- numeric(k-1)
      
      m <<- matrix(0, nrow = k, ncol = k)
      
      ms <<- matrix(0, nrow = k, ncol = k)
      
      mh <<- matrix(0, nrow = k, ncol = k)
      
      msn <<- matrix(0, nrow = k, ncol = k)
      
      
      
      
      
      for (i in 1:(k-2)) {
        
        for (j in (i+1):k) {   
          
          
          
          print(as.numeric(input[[paste0("q2m", j,i)]]) )
          
          
          
          ms[j,i] <<- as.numeric(input[[paste0("q2m", j,i)]])          # conditional medians
          
        }
        
        
        
        S[i] <<- sum(ms[(i+1):k,i])
        
        
        
        for (j in (i+1):(k)) {   
          
          #   mh[j,i] <<- (1 -  SS[i]) * ms[j, i] / S[i]
          
          msn[j, i]<<- ms[j, i]*(1- sum(Q2star[1:i])- etastar[i])/S[i]
          
          if(j>i+1) mh[j,i] <<- msn[j, i]/(1- sum(Q2star[1:i])- etastar[i]-sum(msn[(i+1):(j-1), i]))    # lemma 2   ##HERE
          
          else mh[j,i] <<- msn[j, i]/(1- sum(Q2star[1:i])- etastar[i])    # lemma 2   ##HERE
          
          
          
        }
        
      }
      
      print("ms")
      
      
      
      print(t(ms))
      
      
      
      print("msn")
      
      
      
      print(t(msn))
      
      
      
      
      
      print("mh")
      
      
      
      print(t(mh))
      
      
      
      
      
      z<-rep(0,k-1)
      
      y <- rep(0, k-1)
      
      g <-  rep(0, k-1)
      
      #  y1 <- numeric(k - 1)
      
      #  y2 <- numeric(k - 1)
      
      #  y3 <- numeric(k - 1)
      
      
      
      L <<- rep(0, k-2);  U <<- rep(0, k-2);  V <<- matrix(0, k-2, k-2)
      
      
      
      
      
      u <- matrix(0, nrow = k-1, ncol = k-1)
      
      uT <- matrix(0, nrow = k-1, ncol = k-1)
      
      eta <-rep(0, k-1)
      
      
      
      
      
      switch(input$distributionG,
             
             
             "Beta" = {       
               
               
               
               L1 <- qnorm(pbeta(Q1Sstar[1], aB[1], bB[1]))
               
               U1 <- qnorm(pbeta(Q3Sstar[1], aB[1], bB[1]))
               
               V1 <- ((U1- L1) / 1.349)^2
               
               #  V1<-1                               ##HERE
               
               
               
               R <- matrix(V1, nrow = 1, ncol = 1)
               
               
               
               for (i in 2:(k - 1)) {
                 
                 L1 <- qnorm(pbeta(Q1hash[i], aB[i], bB[i]))
                 
                 U1 <- qnorm(pbeta(Q3hash[i], aB[i], bB[i]))
                 
                 V1 <- ((U1- L1) / 1.349)^2
                 
                 print(c(L1, U1, V1, Q1hash[i], Q3hash[i], Q1Sstar[i], Q3Sstar[i],  aB[i], bB[i]))
                 
                 Q <- matrix(0, nrow = i - 1, ncol = i - 1)
                 
                 u <- rep(0, i - 1)
                 
                 C <- matrix(0, nrow = i - 1, ncol = 1)
                 
                 
                 
                 for (s in 1:(i - 1)) {
                   
                   for (t in 1:(i - 1)) {
                     
                     if (s == t) {
                       
                       Q[s, t] <- qnorm(pbeta(Q2[s] + etahash[s] , aB[s], bB[s]))
                       
                       print(c(Q1Sstar[s], Q2star[s]))
                       
                     } else {
                       
                       if (s < t) {
                         
                         Q[s, t] = qnorm(pbeta(mh[t, s], aB[t], bB[t]))
                         
                       }
                       
                     }
                     
                   }
                   
                   
                   
                   u[s] = qnorm(pbeta(mh[i, s], aB[i], bB[i]))
                   
                 }
                 
                 
                 
                 print("etahash")
                 
                 print(etahash)
                 
                 print(Q)
                 
                 print(solve(Q))
                 
                 print(u)
                 
                 C <- R %*% solve(Q) %*% u
                 
                 sigma1 <- drop(V1 + drop(t(solve(Q) %*% u) %*% R %*% (solve(Q) %*% u)))
                 
                 R <- cbind(rbind(R, t(C)), rbind(C, sigma1))
                 
                 r <- solve(Q) %*% u
                 
                 #  A <- diag(c(1, 1/sqrt(diag(R)[-1])))
                 
                 A <- diag(c(1/sqrt(diag(R))))    ##HERE
                 
                 print("R")
                 
                 print(R)
                 
                 print("A")
                 
                 print(A)
                 
                 Rstar1 <- A %*% R %*% A                    
                 
                 print(Rstar1)
                 
               }
               
               
               
               rownames(R)<-NULL
               
               colnames(R)<-NULL
               
               
               
               
               
               output$correlationOutput <- renderPrint({
                 
                 cat("Correlation Matrix R of Gaussian Copula and Beta marginals:\n")
                 
                 print(Rstar1)
                 
               })
               
               
               
               
               
               output$correlationPlot <- renderPlot({
                 
                 corrplot(Rstar1, type = 'lower', order = 'hclust', tl.col = 'black',
                          
                          cl.ratio = 0.2, tl.srt = 45, col = COL2('PuOr', 10))
                 
               })
               
             },
             
             "Kumaraswamy" = {
               
               
               L2 <- qnorm(pkumar(Q1Sstar[1], aK[1], bK[1]))
               
               U2 <- qnorm(pkumar(Q3Sstar[1], aK[1], bK[1]))
               
               V2 <- ((U2- L2) / 1.349)^2
               
               R2 <- matrix(V2, nrow = 1, ncol = 1)
               
               for (i in 2:(k - 1)) {
                 L2 <- qnorm(pkumar(Q1hash[i], aK[i], bK[i]))
                 U2 <- qnorm(pkumar(Q3hash[i], aK[i], bK[i]))
                 V2 <- ((U2- L2) / 1.349)^2 
                 Q <- matrix(0, nrow = i - 1, ncol = i - 1)
                 u <- rep(0, i - 1)
                 C <- matrix(0, nrow = i - 1, ncol = 1)
                 
                 for (s in 1:(i - 1)) {
                   for (t in 1:(i - 1)) {
                     if (s == t) {
                       #  Q[s, t] = Q1Sstar[s] - Q2star[s]
                       Q[s, t] <- qnorm(pkumar(Q2[s] + etahash[s] , aK[s], bK[s]))
                       print(c(Q1Sstar[s], Q2star[s]))
                     } else {
                       if (s < t) {
                         #  Q[s, t] = m[t, s]
                         Q[s, t] = qnorm(pkumar(mh[t, s], aK[t], bK[t]))
                       }
                     }
                   }
                   
                   # u[s] = m[i, s]
                   u[s] = qnorm(pkumar(mh[i, s], aK[i], bK[i]))
                 }
                 print(c(Q1Sstar[s], Q2star[s]))
                 
                 C <- R2 %*% solve(Q) %*% u
                 sigma2 <- drop(V2 + drop(t(solve(Q) %*% u) %*% R2 %*% (solve(Q) %*% u)))
                 
                 R2 <- cbind(rbind(R2, t(C)), rbind(C, sigma2))
                 A <- diag(c(1/sqrt(diag(R2)))) 
                 
                 Rstar2 <- A %*% R2 %*% A              
                 
               }
               
               rownames(R2)<-NULL
               colnames(R2)<-NULL
               
               print(Rstar2) 
               
               output$correlationOutput <- renderPrint({
                 cat("Correlation Matrix R of Gaussian Copula and Kumaraswamy marginals:\n")
                 print(Rstar2)
               })
               
               output$correlationPlot<- renderPlot({
                 corrplot(Rstar2, type = 'lower', order = 'hclust', tl.col = 'black',
                          cl.ratio = 0.2, tl.srt = 45, col = COL2('BrBG', 10))
               })
               
             },
             
             
             
             "Unit-Weibull" = {
               
               pUW <- function(x, pW, qW) {
                 exp(-pW * (-log(x))^qW)
               }
               
               L3 <- qnorm(pUW(Q1Sstar[1], pW[1], qW[1]))
               
               U3 <- qnorm(pUW(Q3Sstar[1], pW[1], qW[1]))
               
               V3 <- ((U3- L3) / 1.349)^2
               
               R3 <- matrix(V3, nrow = 1, ncol = 1)
               
               
               
               for (i in 2:(k - 1)) {
                 L3 <- qnorm(pUW(Q1hash[i], pW[i], qW[i]))
                 U3 <- qnorm(pUW(Q3hash[i], pW[i], qW[i]))
                 V3 <- ((U3- L3) / 1.349)^2 
                 
                 Q <- matrix(0, nrow = i - 1, ncol = i - 1)
                 u <- rep(0, i - 1)
                 C <- matrix(0, nrow = i - 1, ncol = 1)
                 
                 for (s in 1:(i - 1)) {
                   for (t in 1:(i - 1)) {
                     if (s == t) {
                       #  Q[s, t] = Q1Sstar[s] - Q2star[s]
                       Q[s, t] <- qnorm(pUW(Q2[s] + etahash[s] , pW[s], qW[s]))
                       print(c(Q1Sstar[s], Q2star[s]))
                     } else {
                       if (s < t) {
                         #   Q[s, t] = m[t, s]
                         Q[s, t] = qnorm(pUW(mh[t, s], pW[t], qW[t]))
                       }
                     }
                   }
                   
                   # u[s] = m[i, s]
                   u[s] = qnorm(pUW(mh[i, s], pW[i], qW[i]))
                 }
                 
                 C <- R3 %*% solve(Q) %*% u
                 
                 sigma3 <- drop(V3 + drop(t(solve(Q) %*% u) %*% R3 %*% (solve(Q) %*% u)))
                 R3 <- cbind(rbind(R3, t(C)), rbind(C, sigma3))
                 r <- solve(Q) %*% u
                 A <- diag(c(1/sqrt(diag(R3))))
                 
                 print(R3)
                 Rstar3 <- A %*% R3 %*% A
                 
               }
               
               rownames(R3)<-NULL
               colnames(R3)<-NULL
               
               print(Rstar3) 
               
               output$correlationOutput <- renderPrint({
                 cat("Correlation Matrix R of Gaussian Copula and Unit-Weibull marginals:\n")
                 print(Rstar3)
               })
               
               
               output$correlationPlot<- renderPlot({
                 corrplot(Rstar3, type = 'lower', order = 'hclust', tl.col = 'black',
                          cl.ratio = 0.2, tl.srt = 45, col = COL2('PiYG', 10))
               })
             }, 
             
             
             # Gaussian Copula and Continous Bernoulli marginals
             
             "Continuous-Bernoulli" = {
               
               pCB <- function(x, lambdaC) {
                 if (lambdaC == 0.5) {
                   return(x)
                 } else {
                   return((lambdaC^x * (1 - lambdaC)^(1 - x) + lambdaC - 1) / (2 * lambdaC-1))
                 }
               }
               
               L4 <- qnorm(pCB(Q1Sstar[1], lambdaC[1]))
               
               U4 <- qnorm(pCB(Q3Sstar[1], lambdaC[1]))
               
               V4 <- ((U4- L4) / 1.349)^2
               
               R4 <- matrix(V4, nrow = 1, ncol = 1)
               
               
               for (i in 2:(k - 1)) {
                 L4 <- qnorm(pCB(Q1hash[i], lambdaC[i]))
                 U4 <- qnorm(pCB(Q3hash[i], lambdaC[i]))
                 V4 <- ((U4- L4) / 1.349)^2 
                 
                 Q <- matrix(0, nrow = i - 1, ncol = i - 1)
                 u <- rep(0, i - 1)
                 C <- matrix(0, nrow = i - 1, ncol = 1)
                 
                 for (s in 1:(i - 1)) {
                   for (t in 1:(i - 1)) {
                     if (s == t) {
                       #  Q[s, t] = Q1Sstar[s] - Q2star[s]
                       Q[s, t] <- qnorm(pCB(Q2[s] + etahash[s], lambdaC[s]))
                       print(c(Q1Sstar[s], Q2star[s]))
                     } else {
                       if (s < t) {
                         # Q[s, t] = m[t, s]
                         Q[s, t] = qnorm(pCB(mh[t, s], lambdaC[t]))
                       }
                     }
                   }
                   
                   #  u[s] = m[i, s]
                   u[s] = qnorm(pCB(mh[i, s], lambdaC[i]))
                 }
                 
                 C <- R4 %*% solve(Q) %*% u
                 
                 sigma4 <- drop(V4 + drop(t(solve(Q) %*% u) %*% R4 %*% (solve(Q) %*% u)))
                 
                 R4 <- cbind(rbind(R4, t(C)), rbind(C, sigma4))
                 r <- solve(Q) %*% u
                 A <- diag(c(1/sqrt(diag(R4))))
                 
                 print(R4)
                 Rstar4 <- A %*% R4 %*% A
                 
               }
               
               rownames(R4)<-NULL
               colnames(R4)<-NULL
               
               print(Rstar4) 
               
               output$correlationOutput <- renderPrint({
                 cat("Correlation Matrix R of Gaussian Copula and Continuous-Bernoulli marginals:\n")
                 print(Rstar4)
               })
               
               
               output$correlationPlot<- renderPlot({
                 corrplot(Rstar4, type = 'lower', order = 'hclust', tl.col = 'black', main='Correlation plot',
                          cl.ratio = 0.2, tl.srt = 45, col = COL2('PRGn', 10))
               })
             },
             
             
             
             
             
             "Alpha-Unit" = {
               
               pAU <- function(x, aAU) {
                 2 * pnorm(log(x)/aAU) - 2 * (log(x)/aAU) * dnorm(log(x)/aAU)
               }
               
               L5 <- qnorm(pAU(Q1Sstar[1], aAU[1]))
               
               U5 <- qnorm(pAU(Q3Sstar[1], aAU[1]))
               
               V5 <- ((U5- L5) / 1.349)^2
               
               R5 <- matrix(V5, nrow = 1, ncol = 1)
               
               
               
               for (i in 2:(k - 1)) {
                 L5 <- qnorm(pAU(Q1hash[i], aAU[i]))
                 U5 <- qnorm(pAU(Q3hash[i], aAU[i]))
                 V5 <- ((U5- L5) / 1.349)^2 
                 
                 Q <- matrix(0, nrow = i - 1, ncol = i - 1)
                 u <- rep(0, i - 1)
                 C <- matrix(0, nrow = i - 1, ncol = 1)
                 
                 for (s in 1:(i - 1)) {
                   for (t in 1:(i - 1)) {
                     if (s == t) {
                       #   Q[s, t] = Q1Sstar[s] - Q2star[s]
                       Q[s, t] <- qnorm(pAU(Q2[s] + etahash[s] , aAU[s]))
                       print(c(Q1Sstar[s], Q2star[s]))
                     } else {
                       if (s < t) {
                         #  Q[s, t] = m[t, s]
                         Q[s, t] = qnorm(pAU(mh[t, s], aAU[t]))
                       }
                     }
                   }
                   
                   # u[s] = m[i, s]
                   u[s] = qnorm(pAU(mh[i, s], aAU[i]))
                 }
                 print(c(Q1Sstar[s], Q2star[s]))
                 
                 C <- R5 %*% solve(Q) %*% u
                 
                 sigma5 <- drop(V5 + drop(t(solve(Q) %*% u) %*% R5 %*% (solve(Q) %*% u)))
                 print(sigma5)
                 
                 
                 R5 <- cbind(rbind(R5, t(C)), rbind(C, sigma5))
                 
                 r <- solve(Q) %*% u
                 A <- diag(c(1/sqrt(diag(R5))))
                 
                 print(R5)
                 Rstar5 <- A %*% R5 %*% A
                 
               }
               
               rownames(R5)<-NULL
               colnames(R5)<-NULL
               
               print(Rstar5) 
               
               output$correlationOutput <- renderPrint({
                 cat("Correlation Matrix R of Gaussian Copula and Alpha-Unit marginals:\n")
                 print(Rstar5)
               })
               
               
               output$correlationPlot<- renderPlot({
                 corrplot(Rstar5, type = 'lower', order = 'hclust', tl.col = 'black', main='Correlation plot',
                          cl.ratio = 0.2, tl.srt = 45, col = COL2('PRGn', 10))
               })
             },
             
             
             "Topp-Leone" = {
               
               pTL <- function(x, aTL) 1 - sqrt(1 - x^(1/aTL)) 
               
               L6 <- qnorm(pTL(Q1Sstar[1], aTL[1]))
               
               U6 <- qnorm(pTL(Q3Sstar[1], aTL[1]))
               
               V6 <- ((U6- L6) / 1.349)^2
               
               
               R6 <- matrix(V6, nrow = 1, ncol = 1)
               
               
               
               
               for (i in 2:(k - 1)) {
                 L6 <- qnorm(pTL(Q1hash[i], aTL[i]))
                 U6 <- qnorm(pTL(Q3hash[i], aTL[i]))
                 V6 <- ((U6- L6) / 1.349)^2 
                 
                 Q <- matrix(0, nrow = i - 1, ncol = i - 1)
                 u <- rep(0, i - 1)
                 C <- matrix(0, nrow = i - 1, ncol = 1)
                 
                 for (s in 1:(i - 1)) {
                   for (t in 1:(i - 1)) {
                     if (s == t) {
                       #  Q[s, t] = Q1Sstar[s] - Q2star[s]
                       Q[s, t] <- qnorm(pTL(Q2[s] + etahash[s] , aTL[s]))
                       print(c(Q1Sstar[s], Q2star[s]))
                     } else {
                       if (s < t) {
                         #  Q[s, t] = m[t, s]
                         Q[s, t] = qnorm(pTL(mh[t, s], aTL[t]))
                       }
                     }
                   }
                   
                   # u[s] = m[i, s]
                   u[s] = qnorm(pTL(mh[i, s], aTL[i]))
                 }
                 
                 C <- R6 %*% solve(Q) %*% u
                 
                 sigma6 <- drop(V6 + drop(t(solve(Q) %*% u) %*% R6 %*% (solve(Q) %*% u)))
                 
                 R6 <- cbind(rbind(R6, t(C)), rbind(C, sigma6))
                 
                 r <- solve(Q) %*% u
                 A <- diag(c(1/sqrt(diag(R6))))
                 
                 print(R6)
                 Rstar6 <- A %*% R6 %*% A
                 
               }
               
               rownames(R6)<-NULL
               colnames(R6)<-NULL
               
               output$correlationOutput <- renderPrint({
                 cat("Correlation Matrix R of Gaussian Copula and Topp-Leone marginals:\n")
                 print(Rstar6)
               })
               
               
               output$correlationPlot<- renderPlot({
                 corrplot(Rstar6, type = 'lower', order = 'hclust', tl.col = 'black', main='Correlation plot',
                          cl.ratio = 0.2, tl.srt = 45, col = COL2('PRGn', 10))
               })
             },
             
             
             "Beta Type 3" = {
               
               #   pB3 <- function(x, aB3, bB3) (2 / (x - 2)^2) * pbeta(2 * x / (1 + x), aB3, bB3)  # check
               
               pB3 <-  function(x, aB3, bB3) pbeta((2 * x) / (1 + x), aB3, bB3)
               
               
               L7 <- qnorm(pB3(Q1Sstar[1], aB3[1], bB3[1]))
               
               U7 <- qnorm(pB3(Q3Sstar[1], aB3[1], bB3[1]))
               
               V7 <- ((U7- L7) / 1.349)^2 
               
               R7 <- matrix(V7, nrow = 1, ncol = 1)
               
               
               for (i in 2:(k - 1)) {
                 L7 <- qnorm(pB3(Q1hash[i], aB3[i], bB3[i]))
                 U7 <- qnorm(pB3(Q3hash[i], aB3[i], bB3[i]))
                 V7 <- ((U7- L7) / 1.349)^2 
                 
                 Q <- matrix(0, nrow = i - 1, ncol = i - 1)
                 u <- rep(0, i - 1)
                 C <- matrix(0, nrow = i - 1, ncol = 1)
                 
                 for (s in 1:(i - 1)) {
                   for (t in 1:(i - 1)) {
                     if (s == t) {
                       #   Q[s, t] = Q1Sstar[s] - Q2star[s]
                       Q[s, t] <- qnorm(pB3(Q2[s] + etahash[s] , aB3[s], bB3[s]))
                       print(c(Q1Sstar[s], Q2star[s]))
                     } else {
                       if (s < t) {
                         #  Q[s, t] = m[t, s]
                         Q[s, t] = qnorm(pB3(mh[t, s], aB3[t], bB3[t]))
                         
                       }
                     }
                   }
                   
                   u[s] = qnorm(pB3(mh[i, s], aB3[i], bB3[i]))
                 }
                 
                 C <- R7 %*% solve(Q) %*% u
                 
                 sigma7 <- drop(V7 + drop(t(solve(Q) %*% u) %*% R7 %*% (solve(Q) %*% u)))
                 print(sigma7)
                 
                 R7 <- cbind(rbind(R7, t(C)), rbind(C, sigma7))
                 r <- solve(Q) %*% u
                 A <- diag(c(1/sqrt(diag(R7))))
                 
                 print(R7)
                 Rstar7 <- A %*% R7 %*% A
                 
               }
               
               rownames(R7)<-NULL
               colnames(R7)<-NULL
               
               print(Rstar7) 
               
               output$correlationOutput <- renderPrint({
                 cat("Correlation Matrix R of Gaussian Copula and Beta Type 3 marginals:\n")
                 print(Rstar7)
               })
               
               
               output$correlationPlot<- renderPlot({
                 corrplot(Rstar7, type = 'lower', order = 'hclust', tl.col = 'black',
                          cl.ratio = 0.2, tl.srt = 45, col = COL2('PiYG', 10))
               })
             }, 
             
             
             "Unit-Chen" = {
               
               #   pCH <-  function(x, aCH, bCH) exp(aCH * (1 - exp(x * (-log(x))^bCH)))
               pCH <-  function(x, aCH, bCH) exp(aCH * (1 - exp((-log(x))^bCH)))
               
               L8 <- qnorm(pCH(Q1Sstar[1], aCH[1], bCH[1]))
               U8 <- qnorm(pCH(Q3Sstar[1], aCH[1], bCH[1]))
               V8 <- ((U8- L8) / 1.349)^2 
               
               #  V8 <-1
               
               print("V8")
               
               print(V8)
               R8 <- matrix(V8, nrow = 1, ncol = 1)
               print(R8)
               
               
               
               for (i in 2:(k - 1)) {
                 L8 <- qnorm(pCH(Q1hash[i], aCH[i], bCH[i]))
                 U8 <- qnorm(pCH(Q3hash[i], aCH[i], bCH[i]))
                 V8 <- ((U8- L8) / 1.349)^2 
                 
                 Q <- matrix(0, nrow = i - 1, ncol = i - 1)
                 u <- rep(0, i - 1)
                 C <- matrix(0, nrow = i - 1, ncol = 1)
                 print("u")
                 print(u)
                 for (s in 1:(i - 1)) {
                   for (t in 1:(i - 1)) {
                     if (s == t) {
                       #  Q[s, t] = Q1Sstar[s] - Q2star[s]
                       Q[s, t] <- qnorm(pCH(Q2[s] + etahash[s] , aCH[s], bCH[s]))
                       print(c(Q1Sstar[s], Q2star[s]))
                     } else {
                       if (s < t) {
                         #  Q[s, t] = m[t, s]
                         Q[s, t] = qnorm(pCH(mh[t, s], aCH[t], bCH[t]))
                       }
                     }
                   }
                   
                   #   u[s] = m[i, s]
                   u[s] = qnorm(pCH(mh[i, s], aCH[i], bCH[i]))
                   print(c(u[s], mh[i, s], aCH[i], bCH[i]))
                 }
                 #  print(c(Q1Sstar[s], Q2star[s]))
                 print("hi")
                 print(Q)
                 print(R8)
                 print(V8)
                 print(u)
                 
                 C <- R8 %*% solve(Q) %*% u
                 
                 sigma8 <- drop(V8 + drop(t(solve(Q) %*% u) %*% R8 %*% (solve(Q) %*% u)))
                 print(sigma8)
                 
                 R8 <- cbind(rbind(R8, t(C)), rbind(C, sigma8))
                 r <- solve(Q) %*% u
                 A <- diag(c(1/sqrt(diag(R8))))
                 
                 print(R8)
                 Rstar8 <- A %*% R8 %*% A
                 
               }
               
               rownames(R8)<-NULL
               colnames(R8)<-NULL
               
               print(Rstar8) 
               
               output$correlationOutput <- renderPrint({
                 cat("Correlation Matrix R of Gaussian Copula and Unit-Chen marginals:\n")
                 print(Rstar8)
               })
               
               
               output$correlationPlot<- renderPlot({
                 corrplot(Rstar8, type = 'lower', order = 'hclust', tl.col = 'black',
                          cl.ratio = 0.2, tl.srt = 45, col = COL2('PiYG', 10))
               })
             }, 
             
             
             
             "Logit-Normal" = {
               
               
               L9 <- qnorm(plogitnorm(Q1Sstar[1], aLN[1], bLN[1]))
               
               U9 <- qnorm(plogitnorm(Q3Sstar[1], aLN[1], bLN[1]))
               
               V9 <- ((U9- L9) / 1.349)^2
               
               #  V9 <- 1
               R9 <- matrix(V9, nrow = 1, ncol = 1)
               
               for (i in 2:(k - 1)) {
                 L9 <- qnorm(plogitnorm(Q1hash[i], aLN[i], bLN[i]))
                 U9 <- qnorm(plogitnorm(Q3hash[i], aLN[i], bLN[i]))
                 V9 <- ((U9- L9) / 1.349)^2 
                 
                 Q <- matrix(0, nrow = i - 1, ncol = i - 1)
                 u <- rep(0, i - 1)
                 C <- matrix(0, nrow = i - 1, ncol = 1)
                 
                 for (s in 1:(i - 1)) {
                   for (t in 1:(i - 1)) {
                     if (s == t) {
                       #   Q[s, t] = Q1Sstar[s] - Q2star[s]
                       Q[s, t] <- qnorm(plogitnorm(Q2[s] + etahash[s] , aLN[s], bLN[s]))
                       print(c(Q1Sstar[s], Q2star[s]))
                     } else {
                       if (s < t) {
                         #  Q[s, t] = m[t, s]
                         Q[s, t] = qnorm(plogitnorm(mh[t, s], aLN[t], bLN[t]))
                       }
                     }
                   }
                   
                   u[s] = qnorm(plogitnorm(mh[i, s], aLN[i], bLN[i]))
                 }
                 print(c(Q1Sstar[s], Q2star[s]))
                 
                 C <- R9 %*% solve(Q) %*% u
                 
                 sigma9 <- drop(V9 + drop(t(solve(Q) %*% u) %*% R9 %*% (solve(Q) %*% u)))
                 
                 R9 <- cbind(rbind(R9, t(C)), rbind(C, sigma9))
                 r <- solve(Q) %*% u
                 A <- diag(c(1/sqrt(diag(R9))))
                 
                 print(R9)
                 Rstar9 <- A %*% R9 %*% A
                 
               }
               
               rownames(R9)<-NULL
               colnames(R9)<-NULL
               
               print(Rstar9) 
               
               output$correlationOutput <- renderPrint({
                 cat("Correlation Matrix R of Gaussian Copula and Logit-Normal marginals:\n")
                 print(Rstar9)
               })
               
               
               output$correlationPlot<- renderPlot({
                 corrplot(Rstar9, type = 'lower', order = 'hclust', tl.col = 'black',
                          cl.ratio = 0.2, tl.srt = 45, col = COL2('PiYG', 10))
               })
             }, 
             
             
             
             "Unit-Logistic" = {
               
               pUL <-  function(x, aUL, bUL) exp(aUL) * (x / (1 - x))^bUL / (1 + exp(aUL) * (x / (1 - x))^bUL)
               
               L10 <- qnorm(pUL(Q1Sstar[1], aUL[1], bUL[1]))
               
               U10 <- qnorm(pUL(Q3Sstar[1], aUL[1], bUL[1]))
               
               V10 <- ((U10- L10) / 1.349)^2 
               
               R10 <- matrix(V10, nrow = 1, ncol = 1)
               
               
               for (i in 2:(k - 1)) {
                 L10 <- qnorm(pUL(Q1hash[i], aUL[i], bUL[i]))
                 U10 <- qnorm(pUL(Q3hash[i], aUL[i], bUL[i]))
                 V10 <- ((U10- L10) / 1.349)^2 
                 
                 Q <- matrix(0, nrow = i - 1, ncol = i - 1)
                 u <- rep(0, i - 1)
                 C <- matrix(0, nrow = i - 1, ncol = 1)
                 
                 for (s in 1:(i - 1)) {
                   for (t in 1:(i - 1)) {
                     if (s == t) {
                       # Q[s, t] = Q1Sstar[s] - Q2star[s]
                       Q[s, t] <- qnorm(pUL(Q2[s] + etahash[s] , aUL[s], bUL[s]))
                       print(c(Q1Sstar[s], Q2star[s]))
                     } else {
                       if (s < t) {
                         Q[s, t] = qnorm(pUL(mh[t, s], aUL[t], bUL[t]))
                       }
                     }
                   }
                   
                   u[s] = qnorm(pUL(mh[i, s], aUL[i], bUL[i]))
                 }
                 print(c(Q1Sstar[s], Q2star[s]))
                 
                 C <- R10 %*% solve(Q) %*% u
                 
                 sigma10 <- drop(V10 + drop(t(solve(Q) %*% u) %*% R10 %*% (solve(Q) %*% u)))
                 print(sigma10)
                 
                 R10 <- cbind(rbind(R10, t(C)), rbind(C, sigma10))
                 r <- solve(Q) %*% u
                 A <- diag(c(1/sqrt(diag(R10))))
                 
                 print(R10)
                 Rstar10 <- A %*% R10 %*% A
                 
               }
               
               rownames(R10)<-NULL
               colnames(R10)<-NULL
               
               print(Rstar10) 
               
               output$correlationOutput <- renderPrint({
                 cat("Correlation Matrix R of Gaussian Copula and Unit-Logistic marginals:\n")
                 print(Rstar10)
               })
               
               
               output$correlationPlot<- renderPlot({
                 corrplot(Rstar10, type = 'lower', order = 'hclust', tl.col = 'black',
                          cl.ratio = 0.2, tl.srt = 45, col = COL2('PiYG', 10))
               })
             }, 
             
             
             "Libby-Novick Beta" = {
               
               pLNB <- function(x, aLNB, bLNB, cLNB) pbeta(cLNB * x / (1 - (1 - cLNB) * x), aLNB, bLNB)
               
               
               L11 <- qnorm(pLNB(Q1Sstar[1], aLNB[1], bLNB[1], cLNB[1]))
               
               U11 <- qnorm(pLNB(Q3Sstar[1], aLNB[1], bLNB[1], cLNB[1]))
               
               V11 <- ((U11- L11) / 1.349)^2 
               
               R11 <- matrix(V11, nrow = 1, ncol = 1)
               
               
               
               for (i in 2:(k - 1)) {
                 L11 <- qnorm(pLNB(Q1hash[i], aLNB[i], bLNB[i], cLNB[i]))
                 U11 <- qnorm(pLNB(Q3hash[i], aLNB[i], bLNB[i], cLNB[i]))
                 V11 <- ((U11- L11) / 1.349)^2 
                 
                 Q <- matrix(0, nrow = i - 1, ncol = i - 1)
                 u <- rep(0, i - 1)
                 C <- matrix(0, nrow = i - 1, ncol = 1)
                 
                 for (s in 1:(i - 1)) {
                   for (t in 1:(i - 1)) {
                     if (s == t) {
                       Q[s, t] <- qnorm(pLNB(Q2[s] + etahash[s], aLNB[s], bLNB[s], cLNB[s]))
                       print(c(Q1Sstar[s], Q2star[s]))
                     } else {
                       if (s < t) {
                         Q[s, t] = qnorm(pLNB(mh[t, s], aLNB[t], bLNB[t], cLNB[t]))
                       }
                     }
                   }
                   
                   u[s] = qnorm(pLNB(mh[i, s], aLNB[i], bLNB[i], cLNB[i]))
                 }
                 print(c(Q1Sstar[s], Q2star[s]))
                 
                 C <- R11 %*% solve(Q) %*% u
                 
                 sigma11 <- drop(V11 + drop(t(solve(Q) %*% u) %*% R11 %*% (solve(Q) %*% u)))
                 print(sigma11)
                 
                 R11 <- cbind(rbind(R11, t(C)), rbind(C, sigma11))
                 r <- solve(Q) %*% u
                 A <- diag(c(1/sqrt(diag(R11))))
                 
                 print(R11)
                 Rstar11 <- A %*% R11 %*% A
                 
               }
               
               rownames(R11)<-NULL
               colnames(R11)<-NULL
               
               print(Rstar11) 
               
               output$correlationOutput <- renderPrint({
                 cat("Correlation Matrix R of Gaussian Copula and Libby-Novick Beta marginals:\n")
                 print(Rstar11)
               })
               
               
               output$correlationPlot<- renderPlot({
                 corrplot(Rstar11, type = 'lower', order = 'hclust', tl.col = 'black',
                          cl.ratio = 0.2, tl.srt = 45, col = COL2('PiYG', 10))
               })
             }, 
             
             "Libby-Novick Kumaraswamy" = {
               
               pLNK <-  function(x, aLNK, bLNK, cLNK) 1 - ((1 - x^aLNK) / (1 - (1 - cLNK) * x^aLNK))^bLNK
               
             #  F <- function(x,a,b,c) 1 - exp(-cLNK * (x/(bLNK*(1-x)))^aLNK)
                
             #  function(x, a, b, c)  1 - ((1 - x^aLNK) / (1 - (1 - cLNK) * x^aLNK))^bLNK
               
               L12 <- qnorm(pLNK(Q1Sstar[1], aLNK[1], bLNK[1], cLNK[1]))
               
               U12 <- qnorm(pLNK(Q3Sstar[1], aLNK[1], bLNK[1], cLNK[1]))
               
               V12 <- ((U12- L12) / 1.349)^2 
               
               R12 <- matrix(V12, nrow = 1, ncol = 1)
               
               
               for (i in 2:(k - 1)) {
                 L12 <- qnorm(pLNK(Q1hash[i], aLNK[i], bLNK[i], cLNK[i]))
                 U12 <- qnorm(pLNK(Q3hash[i], aLNK[i], bLNK[i], cLNK[i]))
                 V12 <- ((U12- L12) / 1.349)^2 
                 
                 Q <- matrix(0, nrow = i - 1, ncol = i - 1)
                 u <- rep(0, i - 1)
                 C <- matrix(0, nrow = i - 1, ncol = 1)
                 
                 for (s in 1:(i - 1)) {
                   for (t in 1:(i - 1)) {
                     if (s == t) {
                       Q[s, t] <- qnorm(pLNK(Q2[s] + etahash[s] , aLNK[s], bLNK[s], cLNK[s]))
                       print(c(Q1Sstar[s], Q2star[s]))
                     } else {
                       if (s < t) {
                         Q[s, t] = qnorm(pbeta(mh[t, s], aLNK[t], bLNK[t], cLNK[t]))
                       }
                     }
                   }
                   
                   u[s] = qnorm(pLNK(mh[i, s], aLNK[i], bLNK[i], cLNK[i]))
                 }
                 print(c(Q1Sstar[s], Q2star[s]))
                 
                 C <- R12 %*% solve(Q) %*% u
                 
                 sigma12 <- drop(V12 + drop(t(solve(Q) %*% u) %*% R12 %*% (solve(Q) %*% u)))
                 print(sigma12)
                 
                 
                 R12 <- cbind(rbind(R12, t(C)), rbind(C, sigma12))
                 r <- solve(Q) %*% u
                 A <- diag(c(1/sqrt(diag(R12))))
                 
                 print(R12)
                 Rstar12 <- A %*% R12 %*% A
                 
               }
               
               rownames(R12)<-NULL
               colnames(R12)<-NULL
               
               print(Rstar12) 
               
               output$correlationOutput <- renderPrint({
                 cat("Correlation Matrix R of Gaussian Copula and Libby-Noivck Kumaraswamy marginals:\n")
                 print(Rstar12)
               })
               
               
               output$correlationPlot<- renderPlot({
                 corrplot(Rstar12, type = 'lower', order = 'hclust', tl.col = 'black',
                          cl.ratio = 0.2, tl.srt = 45, col = COL2('PiYG', 10))
               })
             }, 
             
             
             "Unit-Exponential Pareto" = {
               
               pUP <- function(x, aUP, bUP, cUP) 1 - exp(-cUP * (x / (bUP * (1 - x)))^aUP)
                 
               #  ((1 - x^aUP) / (1 - (1 - cUP) * x^aUP))^bUP
               
               L13 <- qnorm(pUP(Q1Sstar[1], aUP[1], bUP[1], cUP[1]))
               
               U13 <- qnorm(pUP(Q3Sstar[1], aUP[1], bUP[1], cUP[1]))
               
               V13 <- ((U13- L13) / 1.349)^2 
               
               R13 <- matrix(V13, nrow = 1, ncol = 1)
               
               
               for (i in 2:(k - 1)) {
                 L13 <- qnorm(pUP(Q1hash[i], aUP[i], bUP[i], cUP[i]))
                 U13 <- qnorm(pUP(Q3hash[i], aUP[i], bUP[i], cUP[i]))
                 V13 <- ((U13- L13) / 1.349)^2 
                 
                 Q <- matrix(0, nrow = i - 1, ncol = i - 1)
                 u <- rep(0, i - 1)
                 C <- matrix(0, nrow = i - 1, ncol = 1)
                 
                 for (s in 1:(i - 1)) {
                   for (t in 1:(i - 1)) {
                     if (s == t) {
                       Q[s, t] <- qnorm(pUP(Q2[s] + etahash[s] , aUP[s], bUP[s], cUP[s]))
                       print(c(Q1Sstar[s], Q2star[s]))
                     } else {
                       if (s < t) {
                         Q[s, t] = qnorm(pUP(mh[t, s], aUP[t], bUP[t], cUP[t]))
                       }
                     }
                   }
                   
                   u[s] = qnorm(pUP(mh[i, s], aUP[i], bUP[i], cUP[i]))
                 }
                 print(c(Q1Sstar[s], Q2star[s]))
                 
                 C <- R13 %*% solve(Q) %*% u
                 
                 sigma13 <- drop(V13 + drop(t(solve(Q) %*% u) %*% R13 %*% (solve(Q) %*% u)))
                 print(sigma13)
                 
                 R13 <- cbind(rbind(R13, t(C)), rbind(C, sigma13))
                 r <- solve(Q) %*% u
                 A <- diag(c(1/sqrt(diag(R13))))
                 
                 print(R13)
                 Rstar13 <- A %*% R13 %*% A
                 
               }
               
               rownames(R13)<-NULL
               colnames(R13)<-NULL
               
               print(Rstar13) 
               
               output$correlationOutput <- renderPrint({
                 cat("Correlation Matrix R of Gaussian Copula and Unit-Exponential Pareto marginals:\n")
                 print(Rstar13)
               })
               
               
               output$correlationPlot<- renderPlot({
                 corrplot(Rstar13, type = 'lower', order = 'hclust', tl.col = 'black',
                          cl.ratio = 0.2, tl.srt = 45, col = COL2('PiYG', 10))
               })
             },
             
             
             "JQPDB" = {
               
               L14 <- qnorm(pJQPDB(Q1Sstar[1], aJ[1], bJ[1], cJ[1], lower = 0, upper = 1, alpha = 0.25))
               U14 <- qnorm(pJQPDB(Q3Sstar[1], aJ[1], bJ[1], cJ[1], lower = 0, upper = 1, alpha = 0.25))
               V14 <- ((U14- L14) / 1.349)^2 
               
               R14 <- matrix(V14, nrow = 1, ncol = 1)
               
               for (i in 2:(k - 1)) {
                 L14 <- qnorm(pJQPDB(Q1hash[i], aJ[i], bJ[i], cJ[i], lower = 0, upper = 1, alpha = 0.25))
                 U14 <- qnorm(pJQPDB(Q3hash[i], aJ[i], bJ[i], cJ[i], lower = 0, upper = 1, alpha = 0.25))
                 V14 <- ((U14- L14) / 1.349)^2 
                 
                 Q <- matrix(0, nrow = i - 1, ncol = i - 1)
                 u <- rep(0, i - 1)
                 C <- matrix(0, nrow = i - 1, ncol = 1)
                 
                 for (s in 1:(i - 1)) {
                   for (t in 1:(i - 1)) {
                     if (s == t) {
                       Q[s, t] <- qnorm(pJQPDB(Q2[s] + etahash[s] , aJ[s], bJ[s], cJ[s], lower = 0, upper = 1, alpha = 0.25))
                       print(c(Q1Sstar[s], Q2star[s]))
                     } else {
                       if (s < t) {
                         Q[s, t] = qnorm(pJQPDB(mh[t, s], aJ[t], bJ[t], cJ[t], lower = 0, upper = 1, alpha = 0.25))
                       }
                     }
                   }
                   
                   u[s] = qnorm(pJQPDB(mh[i, s], aJ[i], bJ[i], cJ[i], lower = 0, upper = 1, alpha = 0.25))
                 }
                 print(c(Q1Sstar[s], Q2star[s]))
                 
                 C <- R14 %*% solve(Q) %*% u
                 
                 sigma14 <- drop(V14 + drop(t(solve(Q) %*% u) %*% R14 %*% (solve(Q) %*% u)))
                 print(sigma14)
                 
                 R14 <- cbind(rbind(R14, t(C)), rbind(C, sigma14))
                 r <- solve(Q) %*% u
                 A <- diag(c(1/sqrt(diag(R14))))
                 
                 print(R14)
                 Rstar14 <- A %*% R14 %*% A
                 
               }
               
               rownames(R14)<-NULL
               colnames(R14)<-NULL
               
               print(Rstar14) 
               
               output$correlationOutput <- renderPrint({
                 cat("Correlation Matrix R of Gaussian Copula and J-QPD-B marginals:\n")
                 print(Rstar14)
               })
               
               
               output$correlationPlot<- renderPlot({
                 corrplot(Rstar14, type = 'lower', order = 'hclust', tl.col = 'black',
                          cl.ratio = 0.2, tl.srt = 45, col = COL2('PiYG', 10))
               })
             },
             
             
             
             
             
             
      ) # switch
      
    })
    
  })
}

shinyApp(ui, server)
