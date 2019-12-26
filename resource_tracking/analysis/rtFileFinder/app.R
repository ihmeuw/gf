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
  
  selectInput("country", "Country:", c("DRC"="cod", "Guatemala"="gtm", "Senegal"="sen", "Uganda"="uga")),
  
  selectInput("grant", "Grant:",
              c('COD-C-CORDAID', 'COD-H-CORDAID', 'COD-H-MOH', 'COD-H-SANRU', 'COD-M-MOH', 'COD-M-PSI',
                'COD-M-SANRU', 'COD-T-CARITAS', 'COD-T-MOH', 'GTM-610-G04-T', 'GTM-H-HIVOS', 'GTM-H-INCAP', 'GTM-M-MSPAS', 
                'GTM-T-MSPAS', 'GUA-311-G05-H', 'GUA-311-G06-H', 'NA', 'SEN-H-ANCS', 'SEN-H-CNLS', 'SEN-M-IntraH', 'SEN-M-PNLP',
                'SEN-S-MOH', 'SEN-Z-MOH', 'SNG-T-PLAN', 'SNG-T-PNT', 'UGA-011-G09-S', 'UGA-708-G13-H', 'UGA-C-TASO', 'UGA-H-MoFPED',
                'UGA-M-MoFPED', 'UGA-M-TASO', 'UGA-S-MoFPED', 'UGA-S-TASO', 'UGA-T-MoFPED', 'UGD-011-G10-S', 'UGD-011-G11-M', 'UGD-011-G12-M',
                'UGD-708-G07-H', 'UGD-708-G08-M', 'ZAR-506-G04-T', 'ZAR-708-G06-H', 'ZAR-809-G10-H', "unknown")),
  
  selectInput("grant_period", "Grant Period", c("2018-2020", '2006-2011', '2008-2013', '2009-2010', '2010-2012',
                                                '2010-2014', '2011-2014', '2011-2015', '2012-2014', '2012-2015',
                                                '2013-2017', '2014-2015', '2014-2017', '2014-2018', '2015-2017',
                                                '2016-2017', '2016-2019', '2018-2018', '2019-2021', '2019-2022', 'NA')),
  # selectInput("grant_status", "Grant Status:", c("Active"="active", "Not Active"="not_active")),
  # selectInput("data_source", "Data Source:", c('Budget'="budget", 'PUDR'="pudr")), 
  # selectInput("file_iteration", "File Iteration:", c("Initial (during grant-making)"="initial", "Final, approved"="final", 
  #                                                    "Revision"="revision")), 
  
  # Show a plot of the generated distribution
  DT::dataTableOutput("filesAvailable")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    require(data.table)
    require(readxl)
    info = data.table(Sys.info())
    user = as.character(info[7])
    file_list = data.table(read_xlsx(paste0("C:/Users/", user, "/Box Sync/Global Fund Files/master_file_list.xlsx")))
    
    # Dynamically change menu options 
    observe({
      updateSelectInput(session, "grant",
                        choices = file_list[loc_name%in%input$country & 
                                              grant_period%in%input$grant_period, grant])
    }) 
    observe({
      updateSelectInput(session, "grant_period", 
                        choices=file_list[loc_name%in%input$country & 
                                            grant%in%input$grant, grant_period])
    })
    
    #Make it pretty 
    output$filesAvailable <- DT::renderDataTable({
      # Subset data first 
      display = file_list[loc_name%in%input$country & 
                            grant%in%input$grant & 
                            grant_period%in%input$grant_period]
      display = display[, .(loc_name, grant, grant_period, file_name, sheet_financial, data_source, file_iteration, start_date_financial, 
                            pudr_semester_financial, update_date)]
      display[start_date_financial=="NA", start_date_financial:=NA]
      display[, start_date_financial:=as.numeric(start_date_financial)]
      display[, start_date_financial:=as.Date(start_date_financial, origin = "1899-12-30")]
      display[, update_date:=substr(update_date, 1, 10)]
      display[, update_date:=as.Date(update_date, format="%Y-%m-%d")]
      names(display) = c('Country', 'Grant', 'Grant Period', 'File', 'Excel Sheet', 'Data Source', 'File Iteration', 'Start Date', 
                         'PUDR Semester', 'Update Date')
      display
  })
}

# Run the application 
shinyApp(ui, server)

