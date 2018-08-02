#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(jsonlite)
library(mapview)
library(shinyBS)
library(formattable)
library(shinyjs)

shinyServer(function(input, output, session) {
  
  IP <- reactive({ input$getIP })
  
  # observe({
  #   cat(capture.output(str(IP()), split=TRUE))
  # })
  # 
  formData_user <- reactive({
    data <- sapply(fields_user, function(x) input[[x]])
    data
  })
  
  observe({
    if(!input$city %in% (world.cities %>% filter(country.etc == input$country) %>% select(name))[[1]]) {
      updateSelectizeInput(session, inputId = "city",
                           choices = c("", (world.cities %>% filter(country.etc == input$country) %>%
                                        rearrange(n = 10, wt = pop) %>%
                                        select(name) %>% unique())[[1]]))
    }
  })
  
  
  Dates <- reactiveValues()
  observe({
    Dates$SelectedDates <- 
      c(as.character(format(input$dateRange[1],format = "%m/%Y")),as.character(format(input$dateRange[2],format = "%m/%Y")))
  })
  
  traj <- reactiveValues()
  
  traj$data <- data.frame(city = character(), 
                     country = character(),
                     lat = numeric(), 
                     lng = numeric(), 
                     pop = numeric(), 
                     startTime = character(), 
                     endTime = character(), 
                     elapsedTime = numeric(),
                     inputTime = character(), 
                     reason = character(), 
                     stringsAsFactors = F)
  
  traj$firstSubmit <- T
  traj$ID_tmp <- 0
  traj$showCountries <- F
  
  # to restore the current city the user inputs or clicks
  curCity <- reactive({
      curCity <- world.cities %>% 
        filter(country.etc == input$country, name == input$city) 
      if(nrow(curCity) >= 2) {
        traj$warningMes1 <- paste0("Please be cautious that there are more than one <b>", input$city, 
                                 "</b> cities in <b>", input$country, "</b>. ")
        curCity <- curCity %>% arrange(desc(pop)) %>% filter(!duplicated(name))
      } else traj$warningMes1 <- NULL
      curCity
  })
  
  observe({
    if(nrow(curCity()) > 0) { 
      traj$curPos <- curCity()
    } else {traj$curPos <- NULL}
  })
  
  # draw the map according to the trajectory
  traj_map <- reactive({
    if(nrow(traj$data) == 0) {
      leaflet() %>% addProviderTiles(providers$Esri) %>% setView(lng = 0, lat = 30, zoom = 1.5)
    } else if(nrow(traj$data) == 1) {
      leaflet() %>% addProviderTiles(providers$Esri) %>% setView(lng = traj$data$lng, lat = traj$data$lat, 
                                           zoom = 13 - log10(traj$data$pop)) %>% 
        addPopups(lng = traj$data$lng, lat = traj$data$lat, popup = paste0(traj$data$city, ", ", traj$data$country))
    } else {
      map <- leaflet() %>% addProviderTiles(providers$Esri) %>% fitBounds(lng1 = min(traj$data$lng) - 2, 
                                                    lat1 = min(traj$data$lat) - 2, 
                                                    lng2 = max(traj$data$lng) + 2, 
                                                    lat2 = max(traj$data$lat) + 2) %>%
        # addPopups(lng = traj$data$lng, lat = traj$data$lat, 
        #           popup = toJSON(apply(traj$data[, c("city", "country")], 1, paste, collapse = ", "))) %>%
        # addCircles(lng = traj$data$lng, lat = traj$data$lat, radius = traj$data$elapsedTime) %>% 
        addPolylines(lng = traj$data$lng, lat = traj$data$lat)
      for(i in 1 : (nrow(traj$data)-1)) {
        # print(traj$data)
        # print(get_arrowhead(traj$data[i, c("long", "lat")], traj$data[i+1, c("long", "lat")]))
        map <- map %>% addPolylines(lng = get_arrowhead(traj$data[i, c("lng", "lat")], traj$data[i+1, c("lng", "lat")])$lng, 
                                    lat = get_arrowhead(traj$data[i, c("lng", "lat")], traj$data[i+1, c("lng", "lat")])$lat)
      }
      map
    }
  })
  
  # output our map
  output$map <- renderLeaflet({
    # print(curCity())
    # print(traj$data)
    tmp <<- traj$data
    traj_map()
  })
  
  # responses on clicks
  observeEvent(input$map_click, {
    click <- input$map_click
    clickCity <- world.cities %>% mutate(lat = abs(lat - click$lat), long = abs(long - click$lng)) %>%
      mutate(dist = lat + long) %>% arrange(dist) %>% filter(dist <= 2) %>% slice(1)
    if(nrow(clickCity) > 0) {
      updateSelectizeInput(session, inputId = "country", selected = clickCity$country.etc)
      updateSelectizeInput(session, inputId = "city",
                           choices = (world.cities %>% filter(country.etc == clickCity$country.etc) %>%
                                                                   # arrange(desc(pop)) %>%
                                                                   select(name) %>% unique())[[1]],
                           selected = clickCity$name)

      proxy <- leafletProxy(mapId = "map")
      proxy %>% clearPopups() %>% addPopups(lng = click$lng, lat = click$lat, 
        popup = paste(clickCity$name, clickCity$country.etc, sep = ", "))
      traj$curPos <- clickCity
    } else {
      traj$curPos <- NULL
    }
  })
  
  observeEvent(input$showCountries, {
    dataCountries <- traj$data %>% group_by(country) %>% summarise(lat = mean(lat), lng = mean(lng))
    if(!traj$showCountries) {
      proxy <- leafletProxy(mapId = "map")
      proxy %>% clearPopups() %>% addPopups(lng = dataCountries$lng, lat = dataCountries$lat,
                                            popup = dataCountries$country)
      traj$showCountries <- T
      updateActionButton(session, inputId = "showCountries", label = "Hide country names")
    } else {
      proxy <- leafletProxy(mapId = "map")
      proxy %>% clearPopups()
      if(nrow(traj$data) == 1) {
        proxy %>% 
          addPopups(lng = traj$data$lng, lat = traj$data$lat, 
                    popup = paste0(traj$data$city, ", ", traj$data$country))
      }
      traj$showCountries <- F
      updateActionButton(session, inputId = "showCountries", label = "Show country names")
    }
  })
  
  
  
  
  # add the city into trajectory if applicable
  observeEvent(input$add, {
    if(input$city == "" || input$country == "") {
      traj$warningMes2 <- paste0("Error: Please input <b>city</b> and <b>country</b>. ")
    } else if(is.na(input$dateRange[1]) || is.na(input$dateRange[2])) {
      traj$warningMes2 <- paste0("Error: Please input <b>start time</b> and <b>end time</b>. ")
    } else if(input$reason == "") {
      traj$warningMes2 <- paste0("Error: Please input the <b>reason</b>. ")
    } else {
      isValidInput <- T
      curStartTime <- format(input$dateRange[1], "%Y-%m")
      curEndTime <- format(input$dateRange[2], "%Y-%m")
      if(curEndTime < curStartTime) {
        traj$warningMes2 <- 
          paste0("Error: The end time in <b>", input$city, "</b> is earlier than the start time. ")
        isValidInput <- F
      } else if(nrow(traj$data) > 0) {
        for(i in 1 : nrow(traj$data)) {
          if(!(curEndTime <= traj$data[i, "startTime"] | curStartTime >= traj$data[i, "endTime"])) {
            traj$warningMes2 <- 
              paste0("Error: Time overlapped in <b>",
                     input$city,
                     "</b> and <b>",
                     traj$data[i, "city"],
                     "</b> from ",
                     max(curStartTime, traj$data[i, "startTime"]), 
                     " to ", 
                     min(curEndTime, traj$data[i, "endTime"]), 
                     ". ")
            isValidInput <- F
          }
        }
      }
      if(isValidInput) {
        # print(traj$curPos)
        updateSelectizeInput(session, inputId = "country", selected = "")
        updateSelectizeInput(session, inputId = "city", selected = "")
        traj$data[nrow(traj$data)+1, ] <- 
          data.frame(traj$curPos$name, traj$curPos$country.etc, 
                     as.numeric(traj$curPos$lat), as.numeric(traj$curPos$long), 
                     as.numeric(traj$curPos$pop), 
                     curStartTime, curEndTime,
                     sqrt(interval(paste0(curStartTime, "-01"), paste0(curEndTime, "-01")) / months(1)) * 3e4,
                     format(Sys.time(), "%Y%m%d%H%M%S", tz = "EST"), 
                     input$reason, 
                     stringsAsFactors = F)
        traj$data <- traj$data %>% arrange(startTime, endTime)
        updateDateRangeInput(session, inputId = "dateRange", 
                             start = max(traj$data$endTime), end = NA)
        updateSelectInput(session, inputId = "reason", selected = "")
        traj$warningMes2 <- NULL
        output$nextInput <- renderText("<span style=\"color:#0000FF;font-weight:bold;\">Please input your next city. </span>")
      } else {
        output$nextInput <- renderText("Please check your input. ")
      }
    }
  })
  
  output$main <- renderUI({
    fluidPage(
      column(12, dataTableOutput(outputId = "trajec")), 
      tags$script("$(document).on('click', '#trajec button', function () {
                      Shiny.onInputChange('lastClickId',this.id);
                  Shiny.onInputChange('lastClick', Math.random())
      });")
    )
  })

  # Reference: https://antoineguillot.wordpress.com/2017/03/01/three-r-shiny-tricks-to-make-your-shiny-app-shines-33-buttons-to-delete-edit-and-compare-datatable-rows/
  output$trajec <- renderDataTable({
    
    DT <- traj$data
    
    if(nrow(traj$data) > 0) {
      DT[["Action"]]<-
        paste0('
               <div class="btn-group" role="group" aria-label="Basic example">
               <button type="button" class="btn btn-secondary delete" id=delete_',1:nrow(traj$data),'>Delete</button>
               </div>
               ')
      datatable(DT[, c("country", "city", "startTime", "endTime", "reason", "Action")]
                %>% setnames(c("Country", "City", "Start Time", "End Time", "Reason", "Actions")), escape = F, 
                options = list(paging = F, searching = F))
    }
  })
  
  ##Managing in row deletion


  observeEvent(input$lastClick,
               {
                 if (input$lastClickId%like%"delete")
                 {
                   row_to_del=as.numeric(gsub("delete_","",input$lastClickId))
                   traj$data <- traj$data[-row_to_del, ]
                 }
               }
  )

  output$warning <- renderText({
    paste0("<font color=\"red\">", 
      paste(traj$warningMes1, traj$warningMes2, collapse = "<br>"), 
      "</font>")
  })
  
  nCountries <- reactive({
    length(unique(traj$data$country))
  })
  
  output$downloadMap <- downloadHandler(
    filename = "Trajectory_Map.pdf", 
    content = function(file) {
      mapshot(x = traj_map() %>% addLabelOnlyMarkers(lng = min(traj$data$lng) * .5 + max(traj$data$lng) * .5, 
                                            lat = max(traj$data$lat), 
                                            label = HTML(paste0("<span style=\"color:#FF0000;font-weight:bold;font-size:20px;
                                                                font-family:Arial\">You have been to ", 
                                                                length(unique(traj$data$country)), 
                                                                " countries. <br>You are better traveled than ", 
                                                                percent(sum(dbGetQuery(conn, "select nCountries from user_info;")[[1]] < nCountries()) / length(dbGetQuery(conn, "select nCountries from user_info;")[[1]]) + 
                                                                  sum(dbGetQuery(conn, "select nCountries from user_info;")[[1]] == nCountries()) / length(dbGetQuery(conn, "select nCountries from user_info;")[[1]]) / 2, digits = 2), 
                                                                " of the visitors. </span>")), 
                                            labelOptions = labelOptions(noHide = T, direction = 'top', offset = c(0, -160), textOnly = T)), 
              file = file, 
              cliprect = "viewport", 
              selfcontained = F)
      # write.csv(traj$data, file = file, row.names = F)
    }
  )
  
  # output$downloadText <- downloadHandler(
  #   filename = "Trajectory_text.csv", 
  #   content = function(file) {
  #     write.csv(traj$data, file = file, row.names = F)
  #   }
  # )
  
  observe({
    if(nrow(traj$data) == 0) {
      hide(id = "showCountries")
      hide(id = "qnaire")
    } else {
      shinyjs::show(id = "showCountries")
      shinyjs::show(id = "qnaire")
    }
  })
  
  observeEvent(input$qnaire, {
    if(nrow(traj$data) > 0) {
      toggleModal(session, modalId = "modalExample", toggle = "open")
    }
  })
  
  output$popup <- renderUI({
    bsModal("modalExample", "To generate your map, please complete the following anonymous survey: ", "", size = "large",
            column(12,                   
                   selectInput(inputId = "gender", label = "Gender", 
                               choices = c("", "Male", "Female", "Other")), 
                   selectInput(inputId = "age", label = "Age", 
                               choices = c("", "0 - 18", "18 - 70", "70+")), 
                   actionButton(inputId = "qSubmit", label = "Submit"))
    )
  })
  
  observeEvent(input$qSubmit, {
    if(traj$firstSubmit) {
      traj$ID_tmp <- nrow(dbGetQuery(conn, "select * from user_info;")) + 1
      traj$firstSubmit <- F
    } else {
      dbGetQuery(conn, paste0("delete from traj_info where ID = ", traj$ID_tmp, "; "))
      dbGetQuery(conn, paste0("delete from user_info where ID = ", traj$ID_tmp, "; "))
    }

    # print(traj$ID_tmp)
    # print(traj$firstSubmit)
    data_traj <- cbind(ID = traj$ID_tmp, traj$data, stringsAsFactors = F)
    # print(data_traj[, fields_traj])
    # print(t(as.matrix(c(ID = traj$ID_tmp, formData_user(), 
    #                     submitTime = format(Sys.time(), "%Y%m%d%H%M%S")))))
    saveData(data_traj[, fields_traj], table_traj)
    saveData(t(as.matrix(c(ID = traj$ID_tmp, formData_user(), nCountries = nCountries(),
                           submitTime = format(Sys.time(), "%Y%m%d%H%M%S", tz = "EST")))), table_user)
    updateActionButton(session, inputId = "qnaire", label = "Update your responses")
    toggleModal(session, modalId = "modalExample", toggle = "close")
    
    
  })
  
  # output$test <- renderText({
  #   paste(options()$mysql) 
  # })
  # session$onSessionEnded(function() {
  #   dbDisconnect(conn)
  # })
  hide(id = c("downloadMap"))
  hide(id = c("gShare"))
  hide(id = c("tShare"))
  hide(id = c("fShare"))
  observeEvent(input$qSubmit, {
    if(input$gender != '' && input$age != '')
      shinyjs::show(id = "downloadMap")
      shinyjs::show(id = "gShare")
      shinyjs::show(id = "tShare")
      shinyjs::show(id = "fShare")
  })
})
