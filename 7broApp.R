
library(shiny)
library(tidyverse)
library(DT)
library(lubridate)
library(shinythemes)
library(shinydashboard)
library(shinyalert)
library(rlang)
library(scales)

library(leaflet)
library(rgdal)
library(ggmap)
library(htmltools)

source(file = 'read_data_shiny.R')
#source(file = 'Read_trips.R')
address_n_codes <- read_csv(file = 'Address_Code.csv')

groubyid <- c('Year'='year','Month'='month','Week'='week')

sidebar <- dashboardSidebar(
  br(),
  br(),
  
  h4('Parameters'),
  
  br(),
  
  sidebarMenu(
    menuItem('Dashboard', tabName = 'dashboard', icon = icon('dashboard')),
    menuItem('IFTA', tabName = 'ifta', icon = icon('truck-moving')),
    
    br(),
    
    dateRangeInput(inputId = 'datein', 
                   label = h4('Date Range'), 
                   start = '2020-01-01', end = Sys.Date()),
    checkboxGroupInput(inputId = 'groupby',
                       label = h4('Summarize By'),
                       choices = groubyid, selected = groubyid),
    checkboxGroupInput(inputId = 'drvr',
                       label = h4('Driver'),
                       choices = "")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'dashboard',
            fluidRow(
              infoBoxOutput("rev", width = 2),
              infoBoxOutput("exp", width = 2),
              infoBoxOutput("profloss", width = 2),
              infoBoxOutput("cpm", width = 2),
              infoBoxOutput("rpm", width = 2),
              infoBoxOutput("mpg", width = 2)
            ),
            br(),
            fluidRow(
              box(
                status = "primary",
                headerPanel("Agg Data"),
                br(),
                br(),
                solidHeader = T,
                br(),
                DT::dataTableOutput(outputId = 'calendar'),
                width = 6,
                height = "600px"
              ),
              box(
                status = "primary",
                headerPanel("Activity Map"),
                br(),
                br(),
                solidHeader = T,
                br(),
                leaflet::leafletOutput(outputId = "mymap", height = '500px'),
                width = 6,
                height = "600px"
              )
            )
    ),
    tabItem(tabName = 'ifta',
            #leaflet::leafletOutput(outputId = "mymap", width = '400px')
    )
  )
)

ui <- dashboardPage(skin = 'red',
  dashboardHeader(title = '7 BROTHERS LOGISTICS LLC'),
  sidebar,
  body
)

server <- function(input, output, session) {
  
  day1 <- reactive({ymd(input$datein[1])})
  day2 <- reactive({ymd(input$datein[2])})
  
  cal <- reactive({
    tibble(date = seq(from=ymd(day1()), to=ymd(day2()), by=1),
           week = lubridate::week(date),
           month = month(date, label = T),
           year = year(date)) %>%
      select(!!!syms(input$groupby)) %>% unique()
  })
  ##############################################################
  revenues <- reactive({
    RateCon %>% 
      filter(fromDate >= day1(), fromDate <= day2()) %>% #,
             #`Driver Name` %in% input$drvr) %>%
      mutate(week = lubridate::week(fromDate),
             month = month(fromDate, label = T),
             year = year(fromDate)) %>% 
      group_by(!!!syms(input$groupby)) %>% 
      summarise(Revenues = sum(total, na.rm = T)) %>% 
      right_join(cal())
  })
  ##############################################################
  exp <- reactive({
    expenses %>% 
      filter(date >= day1(), date <= day2()) %>%
      mutate(week = lubridate::week(date),
             month = month(date, label = T),
             year = year(date)) %>% 
      group_by(!!!syms(input$groupby)) %>% 
      summarise(Expenses = sum(amount, na.rm = T)) %>% 
      right_join(cal()) 
  })
  ##############################################################
  drv <- reactive({
    driver %>% 
      filter(date >= day1(), date <= day2()) %>%
      mutate(week = lubridate::week(date),
             month = month(date, label = T),
             year = year(date)) %>% 
      group_by(!!!syms(input$groupby), driver) %>% 
      summarise(`Driver Pay` = sum(pay, na.rm = T)) %>% 
      right_join(cal())
  })
  ##############################################################
  ins <- reactive({
    insurance %>% 
      filter(date >= day1(), date <= day2()) %>%
      mutate(week = lubridate::week(date),
             month = month(date, label = T),
             year = year(date)) %>% 
      group_by(!!!syms(input$groupby)) %>% 
      summarise(Insurance = sum(amount, na.rm = T)) %>% 
      right_join(cal())
  })
  ##############################################################
  gas <- reactive({
    fuel %>% 
      filter(date >= day1(), date <= day2(), 
             !Item %in% c("SCLE","CADV")) %>%
      mutate(week = lubridate::week(date),
             month = month(date, label = T),
             year = year(date)) %>% 
      group_by(!!!syms(input$groupby)) %>% 
      summarise(Fuel = sum(Amt, na.rm = T),
                `Fuel Quantity` = sum(Qty, na.rm = T)) %>% 
      mutate(`Fuel Price` = round(Fuel/`Fuel Quantity`,2)) %>%
      right_join(cal())
  })
  ##############################################################
  other_exp  <- reactive({
    fuel %>% 
      filter(date >= day1(), date <= day2(), 
             Item %in% c("SCLE","CADV")) %>%
      mutate(week = lubridate::week(date),
             month = month(date, label = T),
             year = year(date)) %>% 
      group_by(!!!syms(input$groupby)) %>% 
      summarise(`Other Exp` = sum(Amt, na.rm = T)) %>%
      right_join(cal())
  })
  ##############################################################
  temp_dist <- reactive({
    vt %>% 
      filter(date >= day1(), date <= day2()) %>%
      mutate(week = lubridate::week(date),
             month = month(date, label = T),
             year = year(date)) %>% 
      arrange(date) %>% 
      group_by(!!!syms(input$groupby)) %>% 
      summarise(Distance = round(sum(distance, na.rm = T),0)) %>% 
      right_join(cal())
  })
  ##############################################################
  tol <- reactive({
    tolls %>% 
      filter(date >= day1(), date <= day2()) %>%
      mutate(week = lubridate::week(date),
             month = month(date, label = T),
             year = year(date)) %>% 
      group_by(!!!syms(input$groupby)) %>% 
      summarise(Tolls = sum(Amount, na.rm = T)) %>% 
      right_join(cal())
  })
  ##############################################################
  note <- reactive({
    truck_note %>% 
      filter(date >= day1(), date <= day2()) %>%
      mutate(week = lubridate::week(date),
             month = month(date, label = T),
             year = year(date)) %>% 
      group_by(!!!syms(input$groupby)) %>% 
      summarise(`Truck Note` = sum(amount, na.rm = T)) %>% 
      right_join(cal())
  })
  ##############################################################
  all_in_list <- reactive({
    list(revenues(), exp(), drv(), tol(), note(),
         ins(), gas(), other_exp(), temp_dist())
  })
  
  #find unique drivers to fill the driver selection box
  observeEvent(input$datein, {
    sel_driver <- reactive({
      drv() %>%
        filter(!is.na(driver)) %>%
        select(driver) %>%
        pull %>% unique()
    })
    updateCheckboxGroupInput(session = session,
                             inputId = 'drvr',
                             choices = sel_driver(),
                             selected = sel_driver())
  })
  
  all_dat <- reactive({
    all_in_list() %>% 
      reduce(full_join) %>% ungroup() %>% 
      #filter(driver %in% input$drvr) %>% 
      replace_na(
        list(`Driver Pay`=0, Tolls=0, `Truck Note`=0, Insurance=0,
             Fuel=0, `Other Exp`=0, Expenses=0, `Fuel Quantity`=0,
             Distance=0)
      ) %>% 
      mutate(`All Expenses` = Expenses+`Driver Pay`+Tolls+
               `Truck Note`+Insurance+Fuel+`Other Exp`,
             Expenses = Expenses + `Other Exp`,
             `Miles/Gal` = round(Distance/`Fuel Quantity`,2),
             `Prof/Loss` = round(Revenues - `All Expenses`,2)) %>%
       select(!!!syms(input$groupby), Revenues, `All Expenses`,`Prof/Loss`,
              `Driver Pay`, Fuel, Tolls, `Truck Note`, Insurance,
              Expenses, Distance, `Fuel Quantity`, `Miles/Gal`)
  })
  
  # output$calendar <- DT::renderDataTable(
  #   all_dat() %>% datatable()
  #)
  ############## tot rev, tot exp, Cost per mile, rate per mile etc ###########
  
  agg_dat <- reactive({
    all_dat() %>% 
      summarise(Distance = sum(Distance, na.rm = T),
                `All Expenses` = sum(`All Expenses`, na.rm = T),
                Revenues = sum(Revenues, na.rm = T),
                `Fuel Quantity` = sum(`Fuel Quantity`, na.rm = T))
  })
  ### all revenue calc and render ###
  all_rev <- reactive({
    agg_dat() %>% .$Revenues %>% round()
  })
  output$rev <- renderInfoBox(
    infoBox(
      h4('Revenues'),
      all_rev() %>% scales::dollar(),
      icon = icon('file-invoice-dollar'),
      color = 'green',
      fill = F
    )
  )
  ### all Expenses calc and render ###
  all_exp <- reactive({
    agg_dat() %>% .$`All Expenses` %>% round()
  })
  output$exp <- renderInfoBox(
    infoBox(
      h4('All Expenses'),
      all_exp() %>% scales::dollar(),
      icon = icon('money-bill-alt'),
      color = 'orange',
      fill = F
    )
  )
  ### Profits vs Losses and render ###
  prof_loss <- reactive({
    agg_dat() %>% 
      mutate(`Profit/Loss` = Revenues - `All Expenses`) %>% 
      select(`Profit/Loss`) %>% round()
  })
  output$profloss <- renderInfoBox(
    infoBox(
      h4('Profit / Loss'),
      prof_loss(),# %>% scales::dollar(),
      icon = icon('balance-scale'),
      color = 'red',
      fill = F
    )
  )
  ### cost per mile calc and render ###
  cpm <- reactive({
    agg_dat() %>% 
      mutate(cpm = ifelse(test = !is.na(Distance|Distance == 0),
                          yes = round(`All Expenses`/Distance,2), no = NA)) %>% 
      select(cpm) %>% round(2)
  })
  output$cpm <- renderInfoBox(
    infoBox(
      h4('cpm'),
      cpm(),
      icon = icon('percentage'),
      color = 'maroon',
      fill = F
    )
  )
  ### rate per mile calc and render ###
  rpm <- reactive({
    agg_dat() %>% 
      mutate(rpm = ifelse(test = !is.na(Distance|Distance == 0),
                          round(Revenues/Distance,2), NA)) %>% 
      select(rpm) %>% round(2)
  })
  output$rpm <- renderInfoBox(
    infoBox(
      h4('rpm'),
      rpm(),
      icon = icon('divide'),
      color = 'purple',
      fill = F
    )
  )
  ### mile per gallon calc and render ###
  mpgal <- reactive({
    agg_dat() %>% 
      mutate(mpg = ifelse(test = !is.na(`Fuel Quantity`|`Fuel Quantity` == 0),
                        yes = round(Distance/`Fuel Quantity`,2), no = NA)) %>% 
      select(mpg) %>% round(2)
  })
  output$mpg <- renderInfoBox(
    infoBox(
      h4('mpg'),
      mpgal(),
      icon = icon('gas-pump'),
      color = 'blue',
      fill = F
    )
  )
  
  output$calendar <- DT::renderDataTable(
    all_dat() %>% 
      select(!!!syms(input$groupby), Revenues, `All Expenses`, `Prof/Loss`, Fuel,
             `Driver Pay`, `Miles/Gal`) %>% 
      datatable()
  )
  ############## map data and renderleaflet ##################
  mdata <- reactive({
    RateCon %>%
      select(toDate, toLocation, driver=`Driver Name`, total) %>% 
      left_join(address_n_codes, by=c('toLocation'='address')) %>%
      filter(toDate >= day1(), toDate <= day2(),
             driver %in% input$drvr)
  })
  
  output$mymap <- renderLeaflet(
    leaflet::leaflet(data = mdata()) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>% 
      setView(lng = -88.833530, lat = 36.843707, zoom = 5) %>% 
      addMarkers(lng = ~lon, lat = ~lat,
                 clusterOptions = markerClusterOptions())
  )
  ############################################################
  
  #### IFTA Page ############
  cnames <- c("cardNumber","date","invoiceId","unit","driver","odometer","locationName",
              "city","state","fee","item","unitPrice","discountePrice","discount","quantity",
              "discountAmount","discountType","amount","db","currency")
}

shinyApp(ui = ui, server = server)



