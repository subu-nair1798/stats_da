library(shiny) 
library(shinydashboard)
library(DT)
library(data.table)
library(datasets)
library(shinyalert)
library(shinyjs)
library(pdfetch)

dataFrameList <- c("0")
datasetList <- sort(gsub("\\ .*","",data(package = "datasets")$results[, "Item"]))

for(i in datasetList) {
  if(is.data.frame(get(i))) {
    dataFrameList <- c(dataFrameList, i)
  }
}
dataFrameList <- dataFrameList[-1]

shinyUI(
    dashboardPage(skin = "black",
    dashboardHeader(title = span(tagList(icon("chart-bar"), tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))), 
                                                            "Statistics for DA", 
                                                            tags$head(tags$style(HTML("#mnom_xp { height : 200px; overflow-y : scroll }"))), 
                                                            tags$head(tags$style(HTML("#cpm_url_btn { margin-top : 25px }"))),
                                                            tags$head(tags$style(HTML("#mean_url_btn { margin-top : 25px }"))),
                                                            tags$head(tags$style(HTML("#mean_yfin_btn { margin-top : 25px }")))
                                         ))
                    ),       
    dashboardSidebar(
      
      sidebarMenu(
        menuItem("Discrete Probability Models", tabName = "dpm",
                 menuSubItem("Generate Data", tabName = "dpm_gen", icon = icon("minus"))
                 ),
        menuItem("Continuous Probability Models", tabName = "cpm",
                 menuSubItem("Import Dataset", tabName = "cpm_imp", icon = icon("minus")),
                 menuSubItem("Generate Data", tabName = "cpm_gen", icon = icon("minus"))
                 ),
        menuItem("Hypothesis Testing", tabName = "ht",
                 menuSubItem("Test of Mean", tabName = "ht_mean", icon = icon("minus")),
                 menuSubItem("Test of Variance", tabName = "ht_var", icon = icon("minus"))
                 ),
        menuItem("Generalized Linear Models", tabName = "glm",
                 menuSubItem("Linear Regression", tabName = "glm_lin", icon = icon("minus")),
                 menuSubItem("Logistic Regression", tabName = "glm_log", icon = icon("minus"))
                 )
      )
    ),
    dashboardBody(
      useShinyalert(),
      useShinyjs(),
      tabItems(
        tabItem(tabName = "dpm_gen",
                sidebarPanel(
                  selectInput("dpm_gen_input", "Select Model", c("Bernoulli", "Binomial", "Multinomial", "Poisson", "Geometric", "Hypergeometric"), selectize = TRUE),
                  
                  numericInput("dpm_gen_s", "No. of simulated data", min = 1, max = 1000, value = 10),
                  conditionalPanel(condition = "input.dpm_gen_input == 'Bernoulli'",
                                    numericInput("bern_p", "Probability : p", value = 0.5),
                                    numericInput("bern_j", "Enter value of j∈{0,1}", value = 1)
                                    ),
                  conditionalPanel(condition = "input.dpm_gen_input == 'Binomial'",
                                    numericInput("binom_t", "Number of Trials : t", value = 10),
                                    numericInput("binom_p", "Probability : p", value = 0.5),
                                    numericInput("binom_j", "Enter value of j∈[0,n]", value = 1)
                                    ),
                  conditionalPanel(condition = "input.dpm_gen_input == 'Multinomial'", 
                                    numericInput("mnom_k", "Value of k", value = 1),
                                    uiOutput("mnom_xp")
                                    ),
                  conditionalPanel(condition = "input.dpm_gen_input == 'Poisson'", 
                                    numericInput("pois_lambda", "Parameter Lambda", value = 1),
                                    numericInput("pois_end", "Ending value for x", value = 10),
                                    numericInput("pois_j", "Enter value of j∈[0,n]", value = 1)
                                    ),
                  conditionalPanel(condition = "input.dpm_gen_input == 'Geometric'",
                                    numericInput("geom_p", "Probability of success in each trial : p", value = 0.5),
                                    numericInput("geom_end", "Ending value of x", value = 10),
                                    numericInput("geom_j", "No. of failures before success : j∈[1,n]", value = 1)
                                    ),
                  conditionalPanel(condition = "input.dpm_gen_input == 'Hypergeometric'", 
                                    numericInput("hyper_ncap", "Total number of items: N", value = 20),
                                    numericInput("hyper_k", "Number of items of Type 1: k", value = 10),
                                    numericInput("hyper_n", "Total number of items drawn: n", value = 5),
                                    numericInput("hyper_j", "Number of items drawn of Type 1: max{0, n - N + k} <= j <= {n, k}", value = 2)
                                    ),
                  actionButton("dpm_gen_btn", "Submit")
                ),mainPanel(
                  hr(),
                  tabsetPanel(type = "pills",
                              tabPanel("Plot", hr(), plotOutput("dpm_gen_plot")),
                              tabPanel("Expected Probability", hr(), verbatimTextOutput("dpm_gen_prob")),
                              tabPanel("Table", hr(), DT::dataTableOutput("dpm_gen_tab")),
                              tabPanel("Expectation", hr(), verbatimTextOutput("dpm_gen_exp")),
                              tabPanel("Variance", hr(), verbatimTextOutput("dpm_gen_var"))
                  )
                )
                ),
        tabItem(tabName = "cpm_gen",
                sidebarPanel(
                  selectInput("cpm_gen_input", "Select Model", c("Uniform", "Normal", "Exponential", "Gamma", "Chi-Squared"), selectize = TRUE),
                  
                  numericInput("cpm_gen_s", "No. of simulated data", min = 1, max = 1000, value = 10),
                  conditionalPanel(condition = "input.cpm_gen_input == 'Uniform'",
                                   numericInput("unif_a", "Parameter a", value = 5),
                                   numericInput("unif_b", "Parameter b", value = 10),
                                   numericInput("unif_x", "Value of x∈[a,b] ", value = 7)
                                   ),
                  conditionalPanel(condition = "input.cpm_gen_input == 'Normal'",
                                   numericInput("norm_mu", "Parameter u ∈ R", value = 0),
                                   numericInput("norm_sigma", "Parameter sigma (σ^2 > 0)", value = 1),
                                   numericInput("norm_j", "Value of j ∈ R", value = 0),
                                   numericInput("norm_i", "Support : i", value = 2)
                                   ),
                  conditionalPanel(condition = "input.cpm_gen_input == 'Exponential'", 
                                   numericInput("exp_lambda", "Parameter Lambda > 0", value = 1),
                                   numericInput("exp_j", "Value of j >= 0", value = 0),
                                   numericInput("exp_i", "Support : i", value = 2)
                                   ),
                  conditionalPanel(condition = "input.cpm_gen_input == 'Gamma'", 
                                   numericInput("gamma_alpha", "Parameter Alpha > 1", value = 2),
                                   numericInput("gamma_lambda", "Parameter Lambda > 0", value = 1),
                                   numericInput("gamma_j", "Value of j >= 0", value = 0),
                                   numericInput("gamma_i", "Support : i", value = 2)
                                   ),
                  conditionalPanel(condition = "input.cpm_gen_input == 'Chi-Squared'", 
                                   numericInput("chisq_k", "Parameter k (Degree of Freedom) ∈ N", value = 1),
                                   numericInput("chisq_j", "Value of j >= 0", value = 0),
                                   numericInput("chisq_i", "Support : i", value = 2)
                                   ),
                  actionButton("cpm_gen_btn", "Submit")
                ), mainPanel(
                   hr(),
                   tabsetPanel(type = "pills",
                              tabPanel("Plot", hr(), plotOutput("cpm_gen_plot")),
                              tabPanel("Expected Probability", hr(), verbatimTextOutput("cpm_gen_prob")),
                              tabPanel("Table", hr(), DT::dataTableOutput("cpm_gen_tab")),
                              tabPanel("Expectation", hr(), verbatimTextOutput("cpm_gen_exp")),
                              tabPanel("Variance", hr(), verbatimTextOutput("cpm_gen_var"))
                   )
                )
                ),
        tabItem(tabName = "cpm_imp",
                sidebarPanel(
                  selectInput("cpm_imp_source", "Select Data Source", choices = c("File" = "file", "URL" = "url", "In-Built" = "inBuilt")),
                  conditionalPanel(condition = "input.cpm_imp_source == 'file'",
                                   fileInput("data_file", "Choose CSV File", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                   tags$hr(),
                                   checkboxInput("cpm_imp_header", "Header", TRUE),
                                   radioButtons("cpm_imp_sep", "Separator",
                                                choices = c(Comma = ",",
                                                            Semicolon = ";",
                                                            Tab = "\t"),
                                                selected = ","),
                                   tags$hr()
                                   ),
                  conditionalPanel(condition = "input.cpm_imp_source == 'url'",
                                   splitLayout(cellWidths = c("83.5%", "16.5%"),
                                                 textInput("url_input", label = "Enter URL"),
                                                 actionButton("cpm_url_btn", "URL")
                                               )
                                   ),
                  conditionalPanel(condition = "input.cpm_imp_source == 'inBuilt'",
                                   selectInput("cpm_ibds", "Select Dataset", dataFrameList)
                                   ),
                  selectInput("cpm_imp_cols", "Select a Column", choices = ""),
                  selectInput("cpm_imp_input", "Select Model", c("Uniform", "Normal", "Exponential", "Gamma"), selectize = TRUE),
                  conditionalPanel(condition = "input.cpm_imp_input == 'Gamma'",
                                   numericInput("imp_gamma_alpha", "Parameter Alpha > 1", value = 2)
                  ),
                  numericInput("cpm_imp_s", "No. of simulated data", min = 1, max = 1000, value = 10),
                  actionButton("cpm_imp_btn", "Submit")
                ), mainPanel(
                   hr(),
                   tabsetPanel(type = "pills",
                               tabPanel("Table", hr(), DT::dataTableOutput("cpm_imp_tab")),
                               tabPanel("Predicted Value", hr(), verbatimTextOutput("cpm_imp_pred"))
                               )
                )
                ),
        tabItem(tabName = "ht_mean",
                sidebarPanel(
                  selectInput("ht_source", "Select Data Source", choices = c("User Input" = "mean_input", "File" = "mean_file", "URL" = "mean_url", "In-Built" = "mean_inBuilt", "Yahoo Finance" = "mean_yfin")),
                  conditionalPanel(condition = "input.ht_source == 'mean_input'",
                                   numericInput("mean_xbar", "Sample mean : x̅ ", value = 0),
                                   numericInput("mean_sigma", "Population SD : σ", value = 1),
                                   numericInput("mean_n", "Sample size : n", value = 30)
                                   ),
                  conditionalPanel(condition = "input.ht_source == 'mean_file'",
                                   fileInput("mean_fileInput", "Choose CSV File", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                   checkboxInput("mean_header", "Header", TRUE),
                                   radioButtons("mean_sep", "Separator",
                                                choices = c(Comma = ",",
                                                            Semicolon = ";",
                                                            Tab = "\t"),
                                                selected = ","),
                                   selectInput("mean_file_cols", "Select a Column", choices = "")
                                   ),
                  conditionalPanel(condition = "input.ht_source == 'mean_url'",
                                   splitLayout(cellWidths = c("83.5%", "16.5%"),
                                               textInput("mean_urlInput", label = "Enter URL"),
                                               actionButton("mean_url_btn", "URL")
                                   ),
                                   selectInput("mean_url_cols", "Select a Column", choices = "")
                                   ),
                  conditionalPanel(condition = "input.ht_source == 'mean_inBuilt'",
                                   selectInput("mean_inBuiltInput", "Select Dataset", dataFrameList),
                                   selectInput("mean_ibds_cols", "Select a Column", choices = "")
                                   ),
                  conditionalPanel(condition = "input.ht_source == 'mean_yfin'",
                                   splitLayout(cellWidths = c("82%", "18%"),
                                               textInput("mean_tickerInput", "Enter ticker"),
                                               actionButton("mean_yfin_btn", "Fetch")
                                   ),
                                   selectInput("mean_freqInput", "Enter Frequency", choices = c("1d", "1wk", "1mo")),
                                   selectInput("mean_yfin_cols", "Select a Column", choices = "")
                                   ),
                  numericInput("mean_mu", "Population mean : μ", value = 0),
                  radioButtons("mean_rb", "Test type : ",
                               c("Lower Tailed" = "meanInput_left",
                                 "Upper Tailed" = "meanInput_right",
                                 "Two Tailed" = "meanInput_two")),
                  sliderInput("mean_alpha", "Significance level : α", value = 0.05, min = 0, max = 0.25, step = 0.001),
                  actionButton("mean_btn", "Submit")
                ), mainPanel(
                  hr(),
                  tabsetPanel(type = "pills",
                              tabPanel("Plot", hr(), plotOutput("ht_mean_plot")),
                              tabPanel("Result", hr(), verbatimTextOutput("ht_mean_result")),
                              tabPanel("Decision", hr(), verbatimTextOutput("ht_mean_decision")),
                              tabPanel("Data Table", hr(), DT::dataTableOutput("ht_mean_tab"), textOutput("ht_mean_tab_ui")),
                              tabPanel("Reference Table", hr(), uiOutput("mean_ref_tab"))
                  )
                )
                )
      )
    )
  )
)