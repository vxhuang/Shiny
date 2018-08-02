#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(dplyr)
library(data.table)
library(maps)
library(lubridate)
library(DT)
library(shinyjs)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tags$title("The Life Path Project"), 
  
  tags$head(
    includeScript("www/app.js")
  ),
  
  tags$head(
    # tags$link(rel = "shortcut icon", type="image/x-icon", href="http://daattali.com/shiny/img/favicon.ico"),
    # Facebook OpenGraph tags
    tags$meta(property = "fb:app_id", content = "298487377558708"), 
    tags$meta(property = "og:title", content = share$title),
    tags$meta(property = "og:type", content = "website"),
    tags$meta(property = "og:url", content = share$url),
    tags$meta(property = "og:image", content = share$image),
    tags$meta(property = "og:description", content = share$description),
    
    # Twitter summary cards
    tags$meta(name = "twitter:card", content = "summary"),
    tags$meta(name = "twitter:site", content = paste0("@", share$twitter_user)),
    tags$meta(name = "twitter:creator", content = paste0("@", share$twitter_user)),
    tags$meta(name = "twitter:title", content = share$title),
    tags$meta(name = "twitter:description", content = share$description),
    tags$meta(name = "twitter:image", content = share$image)
  ),
  useShinyjs(),
  tags$head(
    tags$script(src="getIP.js")
  ),
  # textOutput("test"), 
  h2(HTML("The Life Path Project")), 
  br(), 
  sidebarLayout(
    sidebarPanel(width = 4, 
      HTML("<p style=\"font-size:16px;\">This website generates a map of your life path. 
             Please report how long you spent in each city.</p>"),
      HTML("<p style=\"font-size:16px;\"> 
            You can either use the <b>dropdown menu</b> or <b>type in</b> the city names.
            </p>"),
      br(),
      HTML("<p style=\"font-size:16px;\"> 
            Please start with your birth city. 
            </p>"),
      br(),
      br(), 
      fluidRow(
        column(width = 6,
               selectizeInput(inputId = "country", label = "Country", 
                              choices = c("", (world.cities %>% select(country.etc) %>% 
                                                arrange(country.etc) %>% unique())[[1]]), 
                              selected = "")
               ), 
        column(width = 6, 
               selectizeInput(inputId = "city", label = "City", 
                              choices = "", 
                              selected = "")
               )
      ), 
      
      br(),
      br(),
      dateRangeMonthsInput('dateRange',label = "Duration in selected city", 
                     format = "mm/yyyy", start = "", end = "", max = Sys.Date(), 
                     startview = "year"), 
      br(),
      br(),
      selectInput(inputId = "reason", label = "Reason", 
                  choices = c("", "Residence", "Study", "Tourism", "Work", "Other")), 
      conditionalPanel(condition = "input.reason == 'Other'", {
        textInput(inputId = "reasonOther", label = "Specify: ")
      }),
      br(),
      br(), 
      actionButton(inputId = "add", label = "Add to map"), 
      htmlOutput(outputId = "nextInput"), 
      htmlOutput(outputId = "warning"), 
      br(), 
      br(), 
      br(),
      br()
    ), 
    mainPanel(
      leafletOutput(outputId = "map"), 
      uiOutput(outputId = "main"),
      div(
        fluidRow(
          actionButton(inputId = "showCountries", label = "Show country names"), 
          actionButton(inputId = "qnaire", label = "Generate your map")
        ), 
        style="float:right"),
      br(), 
      br(), 
      # conditionalPanel("input.gender != '' && input.age != ''", 
      absolutePanel(right = "2%", 
                       downloadButton(outputId = "downloadMap", label = "Download your life trajectory map"), 
                       br(),
                       br(), 
                       # downloadButton(outputId = "downloadText", label = "Download your trajectory in .csv file")
                       # div(HTML("&nbsp<a id='gShare' href=\"https://plus.google.com/share?url=http://54.210.79.190:3838/traj_map\" target=\"_blank\">
                       #          <img src=\"google.png\" width=\"50px\" height=\"50px\" alt=\"Google\" />
                       #          </a>"), style = "float:right"),
                       # div(HTML("&nbsp<a id='tShare' href=\"https://twitter.com/share?url=http://54.210.79.190:3838/traj_map&amp;text=Create%20my%20life%20path&amp;hashtags=lifepath\" target=\"_blank\">
                       #          <img src=\"twitter.png\" width=\"50px\" height=\"50px\" alt=\"Twitter\" />
                       #          </a>"), style = "float:right"),
                       # div(HTML("&nbsp<a id='fShare' href='http://www.facebook.com/sharer.php?u=http://54.210.79.190:3838/traj_map' target='_blank'>
                       #          <img src='facebook.png' width=\"50px\" height=\"50px\" alt='Facebook' />
                       #          </a>"), style = "float:right"),
                       actionButton(inputId = "fbShareBtn", label = "Facebook Share", icon = icon("calendar")), 
                       style = "float:right"
                       ),
      uiOutput(outputId = "popup")
    )
  )
))


