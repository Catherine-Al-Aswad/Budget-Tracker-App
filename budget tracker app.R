#    http://shiny.rstudio.com/

library(shiny)  #rshiny
library(tidyverse)  #view and glimpse
library(readxl) # read_excel
library(janitor) #clean_names
library(here) # here() for file path
library(ggplot2) # ggplot
library(plotly)  #interactive graphs
library(lubridate) # date formating
library(RColorBrewer)  # brewer color palette
library(shinyWidgets)  # for calendar widget
library(writexl)  # write to excel
library(vroom)   # write to excel
library(openxlsx)    # write to excel

# # bubble graph libraries
# devtools::install_github("jeromefroe/circlepackeR")
# install.packages("data.tree")
# install.packages("htmlwidgets")
library(circlepackeR) 
library(data.tree)
library(htmlwidgets)


setwd("path to the excel sheet containg your data")

#### OG data reading
# data <- read_excel("Budget_OG.xlsx", sheet = 1) %>% clean_names() %>%
#   subset(select = -c(9:11, 13, 15)) %>%
#   mutate(date_of_transaction = as.Date(date_of_transaction, format = "%Y-%m-%d"),
#          day_cde = as.numeric(format(date_of_transaction, format = "%d")),
#          day_nme = weekdays(date_of_transaction),
#          month_cde = as.numeric(format(date_of_transaction, format = "%m")),
#          month_nme = months(date_of_transaction),
#          month_nme = factor(month_nme),
#          year_cde = as.numeric(format(date_of_transaction, format = "%Y")))


#### writing to xlsx
# r = nrow(data) + 2
# r
# 
# wb <- loadWorkbook(data, "Budget_test.xlsx")
# writeData(wb, sheet = "Sheet1",data, startRow = r)
# saveWorkbook(wb,"Budget_test.xlsx",overwrite = T)


# ########### other data option
# data2 <- read_excel("Budget_test.xlsx", sheet = 1) %>% clean_names() %>%
#   # subset(select = -c(8,10,16)) %>%
#   mutate(date_of_transaction = as.Date(date_of_transaction, format = "%Y-%m-%d"),
#          day_cde = as.numeric(format(date_of_transaction, format = "%d")),
#          day_nme = weekdays(date_of_transaction),
#          month_cde = as.numeric(format(date_of_transaction, format = "%m")),
#          month_nme = months(date_of_transaction),
#          month_nme = factor(month_nme),
#          year_cde = as.numeric(format(date_of_transaction, format = "%Y")))


data <- read_excel("Budget.xlsx", sheet = 1) %>% clean_names() %>%
  mutate(month_nme = factor(month_nme))


glimpse(data)


budget_data <- read_excel("Budget.xlsx", sheet = 2) %>% clean_names() %>%
  mutate(month = factor(month))





# Define UI for application that draws a histogram
ui <- fluidPage(
  
  mainPanel(
    tabsetPanel(
      tabPanel("Monthly Summary", 
               fluidRow(
                 column(width = 3, 
                        fluidRow(style = "height:300px",
                                 wellPanel( titlePanel("Monthly Budget Tracking"),
                                            selectInput(inputId = "year", 
                                                        label = "Choose a Year", 
                                                        choices = unique(data$year_cde)),
                                            
                                            selectInput(inputId = "category", 
                                                        label = "Choose a Category", 
                                                        choices = sort(unique(data$overall_category)))
                                            ),
                                 width = 3)),
                 column(width = 3,
                        fluidRow(style = "height:300px",
                                 plotlyOutput("monthlytotals",
                                              width = 500,
                                              height = 300),
                                 height = div(style = "height:300px;")))),
                 fluidRow(
                   column(width=12,
                        fluidRow(style = "height:1000px",
                                 plotlyOutput("monthlyplot",
                                              width = 1900,
                                              height = 800),
                                 height = div(style = "height:1000px;")),
                        fluidRow(style = "height:1500px",
                                 plotlyOutput("monthlybreakdown",
                                              width = 1900,
                                              height = 2000),
                                 height = div(style = "height:1500px;"))
                        )
                 )
               ),
    
    tabPanel("Yearly Summary",
             fluidRow(
               column(width = 3, 
                      fluidRow(style = "height:300px",
                               wellPanel( titlePanel("Yearly Budget Tracking"),
                                          selectInput(inputId = "categoryYear", 
                                                      label = "Choose a Category", 
                                                      choices = sort(unique(data$overall_category)))
                                          ),
                               width = 3)),
               column(width=12,
                      fluidRow(style = "height:1000px",
                               plotlyOutput("yearlyplot",
                                            width = 1700,
                                            height = 800),
                               height = div(style = "height:1000px;")),
                      fluidRow(style = "height:1000px",
                               plotlyOutput("yearlyplotttl",
                                            width = 1900,
                                            height = 800),
                               height = div(style = "height:1000px;"))
                      )
               )
             ),
    
    tabPanel("Item Breakdwon", 
             fluidRow(
               column(width = 3, 
                      fluidRow(style = "height:300px",
                               wellPanel( titlePanel("Yearly Item Budget Tracking"),
                                          selectInput(inputId = "itmyear", 
                                                      label = "Choose a Year", 
                                                      choices = unique(data$year_cde)),
                                          
                                          selectInput(inputId = "itmcategory", 
                                                      label = "Choose a Category", 
                                                      choices = sort(unique(data$overall_category)))
                               ),
                               width = 3)),
               column(width=12,
                      fluidRow(style = "height:1500px",
                               circlepackeROutput("itmplt",
                                            width = 1500,
                                            height = 1500),
                               height = div(style = "height:1500px;")),
                      fluidRow(style = "height:1500px",
                               circlepackeROutput("itmplt2",
                                                  width = 1500,
                                                  height = 1500),
                               height = div(style = "height:1500px;"))
               )
               )
             ),
    
    tabPanel("Input Data",
             fluidRow(
               column(width = 3, 
                      fluidRow(style = "height:300px",
                               wellPanel( titlePanel("Input Data"),
                                          airDatepickerInput(inputId = "itmdteinpt",
                                                             label = "Select a date",
                                                             value = Sys.Date()),
                                          
                                          checkboxInput(inputId = "categoryinpt", 
                                                        label = "New Overall Category?", 
                                                        value = FALSE),
                                          
                                          conditionalPanel("input.categoryinpt == 1",
                                                           textInput(inputId = "categoryinpt_new", 
                                                                     label = "Type new Overall Category", 
                                                                     value = "")),
                                          
                                          conditionalPanel("input.categoryinpt == 0",
                                                           selectInput(inputId = "categoryinpt_old",
                                                                       label = "Choose an Overall Category",
                                                                       choices = sort(unique(data$overall_category)))),
                                        
                                        
                                          checkboxInput(inputId = "itmcategoryinpt", 
                                                        label = "New Item Category?", 
                                                        value = FALSE),
                                          
                                          conditionalPanel("input.itmcategoryinpt == 1",
                                                           textInput(inputId = "itmcategoryinpt_new", 
                                                                     label = "Type new Item Category", 
                                                                     value = "")),
                                          
                                          conditionalPanel("input.itmcategoryinpt == 0",
                                                           selectInput(inputId = "itmcategoryinpt_old", 
                                                                       label = "Choose an Item Category", 
                                                                       choices = sort(unique(data$item_category)))),
                                          
                                          checkboxInput(inputId = "itmdescinpt", 
                                                        label = "New Item Description?", 
                                                        value = FALSE),
                                          
                                          conditionalPanel("input.itmdescinpt == 1",
                                                           textInput(inputId = "itmdescinpt_new", 
                                                                     label = "Type new Item Description", 
                                                                     value = "")),
                                          
                                          conditionalPanel("input.itmdescinpt == 0",
                                                           selectInput(inputId = "itmdescinpt_old",
                                                                       label = "Choose an Item Description",
                                                                       choices = sort(unique(data$item_description)))),
                                          
                                          numericInput(inputId = "cost_inpt", 
                                                    label = "Original Cost", 
                                                    value = 0),
                                          
                                          radioButtons(inputId = "cost_dscnt", 
                                                      label = "Cost Discount Type", 
                                                      choices = list("Amount" = 1, 
                                                                     "Percent/Ratio" = 2,
                                                                     "None" = 3),
                                                      selected = 3,
                                                      inline = TRUE),
                                          
                                          conditionalPanel("input.cost_dscnt < 3",
                                                           numericInput(inputId = "cost_dscnt_yes",
                                                                        label = "Enter Cost Discount",
                                                                        value = 0)),
                                          
                                          sliderInput(inputId = "tax_percnt", 
                                                      label = "Percent Tax", 
                                                      min = 0,
                                                      max = 1,
                                                      value = 0,
                                                      step = 0.05),
                                          
                                          radioButtons(inputId = "tipOfTotal", 
                                                       label = "Tip Type", 
                                                       choices = list("Amount" = 1, 
                                                                      "Percent/Ratio" = 2,
                                                                      "None" = 3),
                                                       selected = 3,
                                                       inline = TRUE),
                                          
                                          conditionalPanel("input.tipOfTotal < 3",
                                                           numericInput(inputId = "tipOfTotal_yes",
                                                                        label = "Enter Tip", 
                                                                        value = 0)),
                                          
                                          radioButtons(inputId = "total_dscnt", 
                                                       label = "Discount Type of Total", 
                                                       choices = list("Amount" = 1, 
                                                                      "Percent/Ratio" = 2,
                                                                      "None" = 3),
                                                       selected = 3,
                                                       inline = TRUE),
                                          
                                          conditionalPanel("input.total_dscnt < 3",
                                                           numericInput(inputId = "total_dscnt_yes", 
                                                                        label = "Enter Discount of Total", 
                                                                        value = 0)),
                                          
                                          numericInput(inputId = "additional_fees", 
                                                    label = "Enter any additional fees", 
                                                    value = 0),
                                          
                                          textInput(inputId = "comment_inpt", 
                                                    label = "Comments:", 
                                                    value = ""),
                                          
                                          actionButton("itemtable_add", "Add Item")
                                      
                                          
                               ),
                               width = 3)),
               column(width=5,
                      fluidRow(
                        dataTableOutput('checktable')),
                      
                      fluidRow(
                        dataTableOutput('checktotaltable')),
                      
                      fluidRow(
                        numericInput(inputId = "remove_row", 
                                     label = "Remove Row",
                                     value = 0),
                        actionButton("remove_row_okay", "Delete Row")),
                      fluidRow(
                        actionButton("approve_check", "Approve Check"),
                        downloadButton("downloadData", "Done"))
                        
                        )
             ))
    )
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$monthlytotals <- renderPlotly({
    
    # bar data
    data_filtered = data %>% filter(year_cde == input$year)
    
    # summary lines data
    data_filtered_avg = data_filtered %>% 
      group_by(month_nme) %>%
      summarize(cost_ttl = sum(monthly_cost)) %>%
      ungroup() %>%
      summarize(cost_mean = mean(cost_ttl),
                cost_median = median(cost_ttl))
    
    # monthly totals
    data_filtered_ttl = data_filtered %>% 
      group_by(month_nme) %>%
      summarize(cost_ttl = sum(monthly_cost))
    
    
    ggplotly(
      ggplot() +
        geom_line(data_filtered_ttl, 
                  mapping = aes(x=month_nme, y=cost_ttl, group = 1), 
                  color = "gold")+
        geom_point(data_filtered_ttl, 
                   mapping = aes(x=month_nme, y=cost_ttl, group = 1), 
                   color = "gold",
                   shape = 23)+
        geom_hline(data_filtered_avg, 
                   mapping = aes(yintercept = cost_mean),
                   color = "blue") +
        geom_hline(data_filtered_avg, 
                   mapping = aes(yintercept = cost_median),
                   color = "green") +
        scale_y_continuous("Total Monthly Cost", 
                           labels = scales::dollar,
                           breaks = scales::pretty_breaks(n=15)) +
        scale_x_discrete("Month", limits = unique(data_filtered$month_nme)) +
        labs(title = "Total Monthly Spending, After Tax") +
        theme_bw() +
        theme(plot.title = element_text(size = 14, face="bold", hjust = 0.5))
      
    )
  })
  
  output$monthlyplot <- renderPlotly({
      
      # bar data
      data_filtered = data %>% filter(year_cde == input$year,
                                      overall_category == input$category)
      
      # summary lines data
      data_filtered_avg = data_filtered %>% 
        group_by(month_nme) %>%
        summarize(cost_ttl = sum(monthly_cost)) %>%
        ungroup() %>%
        summarize(cost_mean = mean(cost_ttl),
                  cost_median = median(cost_ttl))
      

      # monthly totals
      data_filtered_ttl = data_filtered %>% 
        group_by(month_nme) %>%
        summarize(cost_ttl = sum(monthly_cost))
      
      # budget data
      budget_data_filtered = budget_data %>% filter(year == input$year,
                                                    overall_category == input$category)
      
      # get colors
      colourCount = length(unique(data_filtered$item_category))
      getPalette = colorRampPalette(brewer.pal(12, "Paired"))
      
      
      ggplotly(
        ggplot() +
          geom_bar(data_filtered,
                   stat = "identity", 
                   mapping = aes(x = month_nme, y = monthly_cost, fill = item_category, 
                       text = paste("Item: ", item_description)),
                   colour = "black",
                   size = 0.25) +
          scale_y_continuous("Monthly Cost", 
                             labels = scales::dollar,
                             breaks = scales::pretty_breaks(n=15)) +
          scale_x_discrete("Month", limits = unique(data_filtered$month_nme)) +
          geom_line(budget_data_filtered, 
                    mapping = aes(x=month, y=budget, group = 1), 
                    color = "red")+
          geom_point(budget_data_filtered, 
                    mapping = aes(x=month, y=budget, group = 1), 
                    color = "red")+
          geom_point(data_filtered_ttl, 
                     mapping = aes(x=month_nme, y=cost_ttl, group = 1), 
                     color = "gold",
                     shape = 23)+
          geom_hline(data_filtered_avg, 
                     mapping = aes(yintercept = cost_mean),
                     color = "blue") +
          geom_hline(data_filtered_avg, 
                     mapping = aes(yintercept = cost_median),
                     color = "green") +
          labs(title = "Monthly Spending, After Tax") +
          theme_bw() +
          theme(plot.title = element_text(size = 14, face="bold", hjust = 0.5)) +
          scale_fill_manual(values = getPalette(colourCount))
       
           
      )
      })
    
  output$monthlybreakdown <- renderPlotly({
      
      # bar data
      data_filtered = data %>% filter(year_cde == input$year,
                                      overall_category == input$category)
      
      # summary lines data
      data_filtered_avg = data_filtered %>% 
        group_by(month_nme,item_category) %>%
        summarize(cost_ttl = sum(monthly_cost)) %>%
        ungroup() %>%
        group_by(item_category) %>%
        summarize(cost_mean = mean(cost_ttl),
                  cost_median = median(cost_ttl))
      # monthly totals
      data_filtered_ttl = data_filtered %>% 
        group_by(month_nme,item_category) %>%
        summarize(cost_ttl = sum(monthly_cost))
      
      # get colors
      colourCount = length(unique(data_filtered$item_category))
      getPalette = colorRampPalette(brewer.pal(12, "Paired"))
      
      ggplotly(
        ggplot(data_filtered) +
          geom_bar(stat = "identity", 
                   aes(x = month_nme, y = monthly_cost, fill = item_category,
                       text = paste("Item: ", item_description)),
                   colour = "black",
                   size = 0.25) +
          scale_y_continuous("Monthly Cost", 
                             labels = scales::dollar,
                             breaks = scales::pretty_breaks(n=4)) +
          scale_x_discrete("Month", limits = unique(data_filtered$month_nme)) +
          geom_hline(data_filtered_avg, 
                     mapping = aes(yintercept = cost_mean),
                     color = "blue") +
          geom_hline(data_filtered_avg, 
                     mapping = aes(yintercept = cost_median, fill = item_category),
                     color = "green") +
          geom_point(data_filtered_ttl, 
                     mapping = aes(x=month_nme, y=cost_ttl, group = 1), 
                     color = "gold",
                     shape = 23)+
          labs(title = "Monthly Spending by Item Category, After Tax") +
          theme_bw() +
          theme(plot.title = element_text(size = 14, face="bold", hjust = 0.5)) +
          facet_wrap(~item_category, scales = "free_y", ncol=1)  +
          scale_fill_manual(values = getPalette(colourCount))
        
      )
      
      
    })
    
    
  output$yearlyplot <- renderPlotly({
      
      # line graph data
      data_filtered = data %>% filter(overall_category == input$categoryYear)
      
      # summary lines data
      data_filtered_avg = data_filtered %>% 
        group_by(year_cde, month_nme) %>%
        summarize(cost_ttl = sum(monthly_cost)) %>%
        ungroup() %>%
        group_by(year_cde) %>%
        summarize(cost_mean = mean(cost_ttl),
                  cost_median = median(cost_ttl))
      
      # budget data
      budget_data_filtered = budget_data %>%
        filter(overall_category == input$categoryYear) %>%
        group_by(year) %>%
        summarize(avrg_budget = mean(budget))

        
      # get colors
      colourCount = length(unique(data_filtered$overall_category))
      getPalette = colorRampPalette(brewer.pal(12, "Paired"))
      
      ggplotly(
        ggplot() +
          geom_line(data_filtered_avg, 
                     mapping = aes(x = year_cde, y = cost_mean),
                     color = "blue") +
          geom_point(data_filtered_avg, 
                     mapping = aes(x = year_cde, y = cost_mean, group = 1), 
                     color = "blue")+
          geom_line(data_filtered_avg, 
                     mapping = aes(x = year_cde, y = cost_median),
                     color = "green") +
          geom_point(data_filtered_avg, 
                     mapping = aes(x = year_cde, y = cost_median, group = 1), 
                     color = "green")+
          geom_line(budget_data_filtered,
                    mapping = aes(x=year, y=avrg_budget),
                    color = "red") +
          geom_point(budget_data_filtered,
                     mapping = aes(x=year, y=avrg_budget, group = 1),
                     color = "red")+
          scale_y_continuous("Yearly Cost", 
                             labels = scales::dollar,
                             breaks = scales::pretty_breaks(n=25)) +
          scale_x_discrete("Year",limits = unique(data_filtered_avg$year_cde)) +
          labs(title = "Monthly Spending per Year, After Tax") +
          theme_bw() +
          theme(plot.title = element_text(size = 14, face="bold", hjust = 0.5)) +
          scale_fill_manual(values = getPalette(colourCount))
        )
      
    })

  output$yearlyplotttl <- renderPlotly({
      
      # yearly totals
      data_filtered_ttl = data %>% 
        group_by(year_cde, overall_category) %>%
        summarize(cost_ttl = sum(monthly_cost))
      
      # get colors
      colourCount = length(unique(data_filtered_ttl$overall_category))
      getPalette = colorRampPalette(brewer.pal(12, "Paired"))
      
      ggplotly(
        ggplot(data_filtered_ttl, 
               mapping = aes(x = year_cde, 
                             y = cost_ttl, 
                             color = overall_category,
                             shape = overall_category)) +
          geom_line() +
          geom_point()+
          scale_y_continuous("Yearly Cost", 
                             labels = scales::dollar,
                             breaks = scales::pretty_breaks(n=25)) +
          scale_x_discrete("Year",limits = unique(data_filtered_ttl$year_cde)) +
          labs(title = "Yearly Total Spending, After Tax") +
          theme_bw() +
          theme(plot.title = element_text(size = 14, face="bold", hjust = 0.5)) +
          scale_color_manual(values = getPalette(colourCount))
      )
      
    })   
    
    
  output$itmplt <- renderCirclepackeR({
      
      # data
      data_filtered = data %>% filter(year_cde == input$itmyear,
                                      overall_category == input$itmcategory) %>%
        group_by(item_category,item_description) %>%
        summarize(cost_ttl = sum(monthly_cost)*(-1)) %>%
        mutate(pathString = paste("world", item_category, item_description, cost_ttl, sep = "/")) %>%
        as.Node()
      
      #create circlepacker graph
      circlepackeR(data_filtered, 
                        size="cost_ttl", 
                        color_min=rgb(233/255,237/255,104/255), 
                        color_max=rgb(84/255,204/255,202/255), 
                        width=1000, 
                        height=1000)
      
      # #save the circle graph
      # saveWidget(p, file="circles.html")
      
    })   
    
  output$itmplt2 <- renderCirclepackeR({
      
      # data
      data_filtered = data %>% filter(year_cde == input$itmyear,
                                      overall_category == input$itmcategory) %>%
        group_by(item_category) %>%
        summarize(cost_ttl = sum(monthly_cost)*(-1)) %>%
        mutate(pathString = paste("world", item_category, cost_ttl, sep = "/")) %>%
        as.Node()
      
      #create circlepacker graph
      circlepackeR(data_filtered, 
                   size="cost_ttl", 
                   color_min=rgb(233/255,237/255,104/255), 
                   color_max=rgb(84/255,204/255,202/255), 
                   width=1000, 
                   height=1000)
      
    }) 
  
  

  itm_catg_fltr <- reactive({
    if(input$categoryinpt == FALSE && input$itmcategoryinpt == FALSE) {data %>% filter(overall_category == input$categoryinpt_old)}
  })
  
  observeEvent(itm_catg_fltr(), {
    
    if(input$itmcategoryinpt == FALSE) {updateSelectizeInput(session, 
                                                             "itmcategoryinpt_old", 
                                                             choices = sort(unique(itm_catg_fltr()$item_category)))}
  })
  
  itm_desc_fltr <- reactive({
    if(input$itmcategoryinpt == FALSE && input$itmdescinpt == FALSE) {data %>% filter(item_category == input$itmcategoryinpt_old)}
  })
  
  observeEvent(itm_desc_fltr(), {
    
    if(input$itmdescinpt == FALSE) {updateSelectizeInput(session,
                                                         "itmdescinpt_old", 
                                                         choices = sort(unique(itm_desc_fltr()$item_description)))}
  })
  
  
  
  
  table_all <- reactiveVal(data[0,])
  check_table <- reactiveVal(data[0,])
  
  observeEvent(input$itemtable_add, {
    overall_category_i = " "
    if(input$categoryinpt == FALSE) {overall_category_i = input$categoryinpt_old}
    else (overall_category_i = input$categoryinpt_new)
    
    itmcategoryinpt_i = " "
    if(input$itmcategoryinpt == FALSE) {itmcategoryinpt_i = input$itmcategoryinpt_old}
    else (itmcategoryinpt_i = input$itmcategoryinpt_new)
    
    itmdescinpt_i = " "
    if(input$itmdescinpt == FALSE) {itmdescinpt_i = input$itmdescinpt_old}
    else (itmdescinpt_i = input$itmdescinpt_new)
    
    cost_discount_amnt_i = 0
    cost_discount_prcnt_i = 0
    if(input$cost_dscnt == 1) {cost_discount_amnt_i = input$cost_dscnt_yes}
    else if(input$cost_dscnt == 2) {cost_discount_prcnt_i = input$cost_dscnt_yes}
    else {cost_discount_amnt_i = 0}
    
    tip_prcnt_i = 0
    tip_amnt_i = 0
    if(input$tipOfTotal == 1) {tip_amnt_i = input$tipOfTotal_yes}
    else if(input$tipOfTotal == 2) {tip_prcnt_i = input$tipOfTotal_yes}
    else {tip_amnt_i = 0}
    
    total_discount_prcnt_i = 0
    total_discount_amnt_i = 0
    if(input$total_dscnt == 1) {total_discount_amnt_i = input$total_dscnt_yes}
    else if(input$total_dscnt == 2) {total_discount_prcnt_i = input$total_dscnt_yes}
    else {total_discount_amnt_i = 0}
    
    
    check_table() %>% add_row(
      overall_category = overall_category_i,
      item_category = itmcategoryinpt_i ,
      item_description = itmdescinpt_i,
      cost = -(input$cost_inpt), 
      cost_discount_amnt = cost_discount_amnt_i,
      cost_discount_prcnt = cost_discount_prcnt_i,
      hst = input$tax_percnt,
      additional_total_fee = input$additional_fees,
      tip_prcnt = tip_prcnt_i,
      tip_amnt = tip_amnt_i,
      total_discount_prcnt = total_discount_prcnt_i,
      total_discount_amnt = total_discount_amnt_i,
      monthly_cost = (cost - (cost*cost_discount_prcnt) + cost_discount_amnt)+ 
        ((cost - (cost*cost_discount_prcnt) + cost_discount_amnt)*hst) - 
        additional_total_fee - tip_amnt + total_discount_amnt + 
        (((cost - (cost*cost_discount_prcnt) + cost_discount_amnt)+ 
           ((cost - (cost*cost_discount_prcnt) + cost_discount_amnt)*hst))*tip_prcnt) - 
        (((cost - (cost*cost_discount_prcnt) + cost_discount_amnt)+ 
            ((cost - (cost*cost_discount_prcnt) + cost_discount_amnt)*hst))*total_discount_prcnt),
      
        
        date_of_transaction = as.Date(input$itmdteinpt, format = "%Y-%m-%d"),
      comments = input$comment_inpt,
      day_cde = as.numeric(format(date_of_transaction, format = "%d")),
      day_nme = weekdays(date_of_transaction),
      month_cde = as.numeric(format(date_of_transaction, format = "%m")),
      month_nme = factor(months(date_of_transaction)),
      year_cde = as.numeric(format(date_of_transaction, format = "%Y"))) %>%
      check_table()
    
  })
  
  observeEvent(input$remove_row_okay, {
    
    df <- check_table()
    df <- df[-input$remove_row,]
    check_table(df)
  })
  
  observeEvent(input$approve_check, {
    
    df1 <- table_all()
    df2 <- check_table()
    df <- rbind(df1,df2)
    table_all(df)
    
    df2 <- df[0,]
    check_table(df2)
  })
  
  output$checktable <- renderDataTable(check_table())
  
  
  
  output$checktotaltable = renderDataTable({

    check_table() %>%
      summarize(FirstCost = sum(cost),
                FinalCost = sum(monthly_cost))
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {'Budget.xlsx'},
    content = function(file) {
      # vroom::vroom_write(check_table(), file, delim = ',', append = TRUE          ### writes to tsv but I wanted xlsx
      pathfile = 'C:/Users/cathe/OneDrive/Documents/Personal/Formal/Budget/Budget.xlsx'
      rw <- nrow(data) + 2
      wb <- loadWorkbook(file = pathfile)
      writeData(wb, sheet = 1,table_all(), startRow = rw, colNames = FALSE)
      saveWorkbook(wb,pathfile ,overwrite = TRUE)
      
      })
  
   
    
}

# Run the application 
shinyApp(ui = ui, server = server)
