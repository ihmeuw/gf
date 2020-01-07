#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("PCE File Finder"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Country:", choices=c('All'='All', "DRC"="cod", "Guatemala"="gtm", "Senegal"="sen", "Uganda"="uga")),
      selectInput("grant", "Grant:",
                  c('All', 'COD-C-CORDAID', 'COD-H-CORDAID', 'COD-H-MOH', 'COD-H-SANRU', 'COD-M-MOH', 'COD-M-PSI',
                    'COD-M-SANRU', 'COD-T-CARITAS', 'COD-T-MOH', 'GTM-610-G04-T', 'GTM-H-HIVOS', 'GTM-H-INCAP', 'GTM-M-MSPAS', 
                    'GTM-T-MSPAS', 'GUA-311-G05-H', 'GUA-311-G06-H', 'NA', 'SEN-H-ANCS', 'SEN-H-CNLS', 'SEN-M-IntraH', 'SEN-M-PNLP',
                    'SEN-S-MOH', 'SEN-Z-MOH', 'SNG-T-PLAN', 'SNG-T-PNT', 'UGA-011-G09-S', 'UGA-708-G13-H', 'UGA-C-TASO', 'UGA-H-MoFPED',
                    'UGA-M-MoFPED', 'UGA-M-TASO', 'UGA-S-MoFPED', 'UGA-S-TASO', 'UGA-T-MoFPED', 'UGD-011-G10-S', 'UGD-011-G11-M', 'UGD-011-G12-M',
                    'UGD-708-G07-H', 'UGD-708-G08-M', 'ZAR-506-G04-T', 'ZAR-708-G06-H', 'ZAR-809-G10-H', "unknown")),
      selectInput("grant_period", "Grant Period", c('All', "2018-2020", '2006-2011', '2008-2013', '2009-2010', '2010-2012',
                                                    '2010-2014', '2011-2014', '2011-2015', '2012-2014', '2012-2015',
                                                    '2013-2017', '2014-2015', '2014-2017', '2014-2018', '2015-2017',
                                                    '2016-2017', '2016-2019', '2018-2018', '2019-2021', '2019-2022', 'NA')),
      selectInput("grant_status", "Grant Status:", c("All"="All", "Active"="active", "Not Active"="not_active")),
      selectInput("data_source", "Data Source:", c("All"="All", 'Budget'="budget", 'PUDR'="pudr")),
      selectInput("file_iteration", "File Iteration:", c("All"="All", "Initial (during grant-making)"="initial", "Final, approved"="final",
                                                         "Revision"="revision"))
    ), 
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("All Files", DT::dataTableOutput("filesAvailable")), 
                  tabPanel("Final, approved budgets", DT::dataTableOutput("finalApproved")), 
                  tabPanel("Most recent budget", DT::dataTableOutput("mostRecentRevision"))
      )
    ) 
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  require(data.table)
  require(readxl)
  file_list = fread("file_list.csv")
  
  # Dynamically change menu options 
  # observe({
  #   shortFileList = file_list[(loc_name%in%input$country | loc_name=="All") & 
  #                             (grant%in%input$grant | grant=="All") & 
  #                             (grant_period%in%input$grant_period | grant_period=="All")]
  #   updateSelectInput(session, "grant",
  #                     choices = unique(shortFileList$grant))
  # 
  #   updateSelectInput(session, "grant_period",
  #                     choices=unique(shortFileList$grant_period))
  # })
  
  
  display <- reactive({
    table = copy(file_list)
    if (!input$country=="All") table = table[loc_name%in%input$country]
    if (!input$grant=="All") table = table[grant%in%input$grant]
    if (!input$grant_period=="All") table = table[grant_period%in%input$grant_period]
    if (!input$grant_status=="All") table = table[grant_status%in%input$grant_status]
    if (!input$data_source=="All") table = table[data_source%in%input$data_source]
    if (!input$file_iteration=="All") table = table[file_iteration%in%input$file_iteration]
    
    table = table[, .(loc_name, grant, grant_period, file_name, sheet_financial, data_source, file_iteration, start_date_financial, 
                          pudr_semester_financial, update_date)]
    table[start_date_financial=="NA", start_date_financial:=NA]
    table[, start_date_financial:=as.numeric(start_date_financial)]
    table[, start_date_financial:=as.Date(start_date_financial, origin = "1899-12-30")]
    table[, update_date:=substr(update_date, 1, 10)]
    table[, update_date:=as.Date(update_date, format="%Y-%m-%d")]
    
    table
  })
  
  #Display the whole file list 
  output$filesAvailable <- DT::renderDataTable({ 
    table = display()
    names(table) = c('Country', 'Grant', 'Grant Period', 'File', 'Excel Sheet', 'Data Source', 'File Iteration', 'Start Date', 
                       'PUDR Semester', 'Update Date')
    table
  })
  
  # Display the final, approved budgets 
  output$finalApproved <- DT::renderDataTable({
    table = display()
    table = table[file_iteration=="final" & data_source=="budget"]
    table = table[, .(grant, grant_period, file_name, sheet_financial, update_date)]
    names(table) = c('Grant', 'Grant Period', 'File', 'Excel Sheet', 'Update Date')
    table
  })
  
  # Display the most recent revision
  output$mostRecentRevision <- DT::renderDataTable({
    table = display()
    table = table[data_source=="budget" & file_iteration=="revision"]
    table = table[order(grant, grant_period, -update_date)]
    table[, seq:=seq(0, 10, by=1), by=c('grant', 'grant_period')]
    stopifnot(any(table$seq)<10)
    table = table[seq==0]
    table$seq <- NULL
    
    table = table[, .(grant, grant_period, file_name, sheet_financial, update_date)]
    names(table) = c('Grant', 'Grant Period', 'File', 'Excel Sheet', 'Update Date')
    table
  })
}

# Run the application 
shinyApp(ui, server)

