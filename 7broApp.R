
library(shiny)
library(tidyverse)
library(DT)
library(lubridate)
library(shinythemes)
library(shinydashboard)
library(shinyalert)
library(rlang)
library(scales)
library(plotly)

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
                   start = Sys.Date()-300, end = Sys.Date()),
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
                headerPanel("Aggregate Data"),
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
            fluidRow(
              infoBoxOutput(outputId = "miles", width = 3),
              infoBoxOutput("gallon", width = 3),
              infoBoxOutput("cost", width = 3),
              infoBoxOutput("mpg2", width = 3)
            ),
            br(),
            
            fluidRow(
              box(
                status = "primary",
                headerPanel("IFTA DATA"),
                br(),
                br(),
                solidHeader = T,
                br(),
                DT::dataTableOutput(outputId = 'iftadata'),
                width = 4,
                height = "600px"
              ),
              box(
                status = "primary",
                headerPanel("Fuel"),
                br(),
                br(),
                solidHeader = T,
                br(),
                plotOutput(outputId = 'fuelchart'),
                width = 8,
                height = "600px"
              )
            )
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
    RateCon %>% mutate(date = toDate) %>% 
      filter(date >= day1(), date <= day2()) %>% #,
             #`Driver Name` %in% input$drvr) %>%
      mutate(week = lubridate::week(date),
             month = month(date, label = T),
             year = year(date)) %>% 
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
      group_by(!!!syms(input$groupby)) %>% 
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
      driver %>%
        filter(date >= day1(), date <= day2()) %>%
        mutate(week = lubridate::week(date),
               month = month(date, label = T),
               year = year(date)) %>%
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
      datatable() %>% 
      formatCurrency(columns = c("Revenues","All Expenses","Prof/Loss",
                                 "Fuel","Driver Pay"))
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
  trips <- as_tibble(vt) %>% 
    mutate(date = ymd(date))
  
  ifta_trips <- reactive({
    trips %>% 
      filter(date >= day1(), date <= day2()) %>% 
      group_by(jurisdiction) %>% 
      summarise(distance = sum(distance))
  })
  
  tst <- reactive({
    fuel %>% 
      mutate(date = mdy(`Tran Date`)) %>% 
      filter(Item == "ULSD",
             date >= day1(), date <= day2()) %>% 
      group_by(`State/ Prov`) %>% 
      summarize(quantity = sum(Qty, na.rm = T),
                amount = sum(Amt, na.rm = T)) %>% 
      ungroup() %>% 
      rename(state = `State/ Prov`)
  })
  
  myIfta <- reactive({
    ifta_trips() %>% 
      left_join(tst(), by = c("jurisdiction" = "state")) %>% 
      arrange(jurisdiction) %>% 
      mutate(mpg = round(distance/quantity,2))
  })
  
  output$iftadata <- renderDataTable(
    myIfta() %>% 
      datatable() %>% 
      formatRound(columns = c("distance", "quantity"), digits = 0)
  )
  
  ### total miles and render ###
  totmiles <- reactive({
    myIfta() %>%
      summarise(distance = sum(distance, na.rm = T)) %>%
      pull() %>% 
      round()
  })
  output$miles <- renderInfoBox(
    infoBox(
      h4('Mileage'),
      totmiles() %>% scales::comma(),
      icon = icon('road'),
      color = 'maroon',
      fill = F
    )
  )
  ### total gallons and render ###
  totqty <- reactive({
    myIfta() %>%
      summarise(quantity = sum(quantity, na.rm = T)) %>%
      pull() %>% 
      round(0)
  })
  output$gallon <- renderInfoBox(
    infoBox(
      h4('Gallons'),
      totqty() %>% scales::comma(),
      icon = icon('gas-pump'),
      color = 'orange',
      fill = F
    )
  )
  ### Fuel Cost and render ###
  totcost <- reactive({
    myIfta() %>%
      summarise(amount = sum(amount, na.rm = T)) %>%
      pull() %>% 
      round(0)
  })
  output$cost <- renderInfoBox(
    infoBox(
      h4('Total Fuel Cost'),
      totcost() %>% scales::dollar(),
      icon = icon('dollar'),
      color = 'red',
      fill = F
    )
  )
  
  ### Miles per gallon and render ###
  mpg <- reactive({
    myIfta() %>%
      summarise(distance = sum(distance, na.rm = T),
                quantity = sum(quantity, na.rm = T)) %>%
      mutate(mpg = distance/quantity) %>% 
      pull(mpg) %>% 
      round(2)
  })
    
  output$mpg2 <- renderInfoBox(
    infoBox(
      h4('Miles per Gallon'),
      mpg(),
      icon = icon('funnel-dollar'),
      color = 'red',
      fill = F
    )
  )
  
  output$fuelchart <- renderPlot(
    myIfta() %>% 
      pivot_longer(cols = distance:mpg, names_to = "stat", values_to = 'value') %>% 
      mutate(stat = case_when(
        stat == 'distance' ~ 'Distance (mile)',
        stat == 'quantity' ~ 'Quantity (gal)',
        stat == 'amount' ~ 'Fuel Value ($)',
        TRUE ~ 'MPG (miles/gal)'
      )) %>% 
      ggplot(data = ., mapping = aes(x = jurisdiction, y = value, fill = stat)) +
      geom_col(show.legend = F) +
      facet_wrap(~stat, scales = 'free') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13))
  )
  
}

shinyApp(ui = ui, server = server)



