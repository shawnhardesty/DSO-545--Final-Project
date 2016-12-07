
library(shiny)
library(png)

ui = navbarPage("LA 3-1-1 Data",
                navbarMenu("311 Call Center Tracking Data",
                           tabPanel("Departments",
                                    sidebarLayout(
                                      sidebarPanel(
                                        radioButtons(inputId = "Plot1", label = "Choose a Plot:",
                                                     choices = c("Number of Calls by Department",
                                                                 "Number of Calls by Department and Resolution Category"))),
                                      mainPanel(imageOutput("Plot1")))),
                           tabPanel("Services",
                                    sidebarLayout(
                                      sidebarPanel(
                                        radioButtons(inputId = "Plot2", label = "Choose a Plot:",
                                                     choices = c("Number of Calls by Service",
                                                                 "Number of Calls by Service and Resolution Category"))),
                                      mainPanel(
                                        imageOutput("Plot2")))),
                           tabPanel("Call Resolution",
                                    sidebarLayout(
                                      sidebarPanel(
                                        radioButtons(inputId = "Plot3", label = "Choose a Plot:",
                                                     choices = c("Percentage of Resolution Category by Year",
                                                                 "Service Failure"))),
                                      mainPanel(
                                        imageOutput("Plot3")))),
                           tabPanel("Zip Code",
                                    sidebarLayout(
                                      sidebarPanel(
                                        radioButtons(inputId = "Plot4", label = "Choose a Plot:",
                                                     choices = c("Top20 Zip Codes and Corresponding Top10 Services (a)",
                                                                 "Top20 Zip Codes and Corresponding Top10 Services (b)"))),
                                      mainPanel(
                                        imageOutput("Plot4")))),
                           tabPanel("Time",
                                    sidebarLayout(
                                      sidebarPanel(
                                        radioButtons(inputId = "Plot5", label = "Choose a Plot:",
                                                     choices = c("Number of Calls by Hour and Weekday",
                                                                 "Number of Calls by Hour, Weekday and Year",
                                                                 "Number of Calls by Weekday and Month",
                                                                 "Number of Calls by Weekday, Month and Year"))),
                                      mainPanel(
                                        imageOutput("Plot5")))),
                           tabPanel("Map",
                                    sidebarLayout(
                                      sidebarPanel(
                                        radioButtons(inputId = "Plot6", label = "Choose a Plot:",
                                                     choices = c("Number of Calls vs. Population Density at Zip Code Level"))),
                                      mainPanel(
                                        imageOutput("Plot6"))))
                ),
                navbarMenu("My LA 311 Service Request Data",
                           tabPanel("Request Type",
                                    sidebarLayout(
                                      sidebarPanel(
                                        radioButtons(inputId = "Plot7", label = "Choose a Plot:",
                                                     choices = c("Sources of Request Date",
                                                                 "Type of Request from Different Sources",
                                                                 "Type of Request in Different Status",
                                                                 "Number of Request by Day of Week and Request Type"))),
                                      mainPanel(imageOutput("Plot7")))),
                           tabPanel("Request Owner",
                                    sidebarLayout(
                                      sidebarPanel(
                                        radioButtons(inputId = "Plot8", label = "Choose a Plot:",
                                                     choices = c("Request for Different Owner"))),
                                      mainPanel(
                                        imageOutput("Plot8")))),
                           tabPanel("Request Status",
                                    sidebarLayout(
                                      sidebarPanel(
                                        radioButtons(inputId = "Plot9", label = "Choose a Plot:",
                                                     choices = c("Request in Different Status",
                                                                 "Request Type of Cancelled Requests",
                                                                 "Request Status by Day of Week and Status"))),
                                      mainPanel(
                                        imageOutput("Plot9")))),
                           tabPanel("Map",
                                    sidebarLayout(
                                      sidebarPanel(
                                        radioButtons(inputId = "Plot10", label = "Choose a Plot:",
                                                     choices = c("Requests in Different Council Districts",
                                                                 "City of Los Angeles Council Districts",
                                                                 "Heatmap of Request Amount",
                                                                 "Population Density  VS. Request Count"))),
                                      mainPanel(
                                        imageOutput("Plot10")))),
                           tabPanel("ZIP Code",
                                    sidebarLayout(
                                      sidebarPanel(
                                        radioButtons(inputId = "Plot11", label = "Choose a Plot:",
                                                     choices = c("Correspondence between Top20 Zip Code Areas and Top 10 Request Types",
                                                                 "Top 20 Zip Area with Different Request Type"))),
                                      mainPanel(
                                        imageOutput("Plot11"))))
                ))