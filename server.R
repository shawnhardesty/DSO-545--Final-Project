
library(shiny)
library(png)


server = function(input, output) {
  
  output$Plot1 <- renderImage({
    if (input$Plot1 == "Number of Calls by Department") {
      return(list(src = normalizePath(file.path('./images', 'Picture1.png'))))
      
    } else if (input$Plot1 == "Number of Calls by Department and Resolution Category") {
      return(list(src = normalizePath(file.path('./images', 'Picture2.png'))))
      
    } 
  }, deleteFile = FALSE)
  
  output$Plot2 <- renderImage({
    if (input$Plot2 == "Number of Calls by Service") {
      return(list(src = normalizePath(file.path('./images', 'Picture3.png'))))
      
    } else if (input$Plot2 == "Number of Calls by Service and Resolution Category") {
      return(list(src = normalizePath(file.path('./images', 'Picture4.png'))))
      
    } 
  }, deleteFile = FALSE)
  
  output$Plot3 <- renderImage({
    if (input$Plot3 == "Percentage of Resolution Category by Year") {
      return(list(src = normalizePath(file.path('./images', 'Picture6.png'))))
      
    } else if (input$Plot3 == "Service Failure") {
      return(list(src = normalizePath(file.path('./images', 'Picture7.png'))))
      
    } 
  }, deleteFile = FALSE)
  
  output$Plot4 <- renderImage({
    if (input$Plot4 == "Top20 Zip Codes and Corresponding Top10 Services (a)") {
      return(list(src = normalizePath(file.path('./images', 'Picture13.png'))))
      
    } else if (input$Plot4 == "Top20 Zip Codes and Corresponding Top10 Services (b)") {
      return(list(src = normalizePath(file.path('./images', 'Picture14.png'))))
      
    } 
  }, deleteFile = FALSE)

  output$Plot5 <- renderImage({
    if (input$Plot5 == "Number of Calls by Hour and Weekday") {
      return(list(src = normalizePath(file.path('./images', 'Picture8.png'))))
      
    } else if (input$Plot5 == "Number of Calls by Hour, Weekday and Year") {
      return(list(src = normalizePath(file.path('./images', 'Picture9.png'))))
      
    } else if (input$Plot5 == "Number of Calls by Weekday and Month") {
      return(list(src = normalizePath(file.path('./images', 'Picture10.png'))))
      
    } else if (input$Plot5 == "Number of Calls by Weekday, Month and Year") {
      return(list(src = normalizePath(file.path('./images', 'Picture11.png'))))
      
    }
  }, deleteFile = FALSE)  
  
  output$Plot6 <- renderImage({
    if (input$Plot6 == "Number of Calls vs. Population Density at Zip Code Level") {
      return(list(src = normalizePath(file.path('./images', 'Picture12.png'))))
    }
  }, deleteFile = FALSE)  

  output$Plot7 <- renderImage({
    if (input$Plot7 == "Sources of Request Date") {
      return(list(src = normalizePath(file.path('./images', 'Sources of Request Date.png'))))
      
    } else if (input$Plot7 == "Type of Request from Different Sources") {
      return(list(src = normalizePath(file.path('./images', 'Type of Request from Different Sources.png'))))
    } else if (input$Plot7 == "Type of Request in Different Status") {
      return(list(src = normalizePath(file.path('./images', 'Type of Request in Different Status.png'))))
    } else if (input$Plot7 == "Number of Request by Day of Week and Request Type") {
      return(list(src = normalizePath(file.path('./images', 'Number of Request by Day of Week and Request Type.png'))))
      
    } 
  }, deleteFile = FALSE)
  
  output$Plot8 <- renderImage({
    if (input$Plot8 == "Request for Different Owner") {
      return(list(src = normalizePath(file.path('./images', 'Request for Different Owner.png'))))

    } 
  }, deleteFile = FALSE)
  
  output$Plot9 <- renderImage({
    if (input$Plot9 == "Request in Different Status") {
      return(list(src = normalizePath(file.path('./images', 'Request in Different Status.png'))))
    } else if (input$Plot9 == "Request Type of Cancelled Requests") {
      return(list(src = normalizePath(file.path('./images', 'Request Type of Cancelled Requests.png'))))
    } else if (input$Plot9 == "Request Status by Day of Week and Status") {
      return(list(src = normalizePath(file.path('./images', 'Request Status by Day of Week and Status.png'))))
      
    } 
  }, deleteFile = FALSE)
  
  output$Plot10 <- renderImage({
    if (input$Plot10 == "Requests in Different Council Districts") {
      return(list(src = normalizePath(file.path('./images', 'Requests in Different Council Districts.png'))))
    } else if (input$Plot10 == "City of Los Angeles Council Districts") {
      return(list(src = normalizePath(file.path('./images', 'City of Los Angeles Council Districts.png'))))
    } else if (input$Plot10 == "Heatmap of Request Amount") {
      return(list(src = normalizePath(file.path('./images', 'Heatmap of Request Amount.png'))))
    } else if (input$Plot10 == "Population Density  VS. Request Count") {
      return(list(src = normalizePath(file.path('./images', 'Population Density  VS. Request Count.png'))))
    } 
  }, deleteFile = FALSE)
  
  output$Plot11 <- renderImage({
    if (input$Plot11 == "Correspondence between Top20 Zip Code Areas and Top 10 Request Types") {
      return(list(src = normalizePath(file.path('./images', 'Correspondence between Top20 Zip Code Areas and Top 10 Request Types.png'))))
      
    } else if (input$Plot11 == "Top 20 Zip Area with Different Request Type") {
      return(list(src = normalizePath(file.path('./images', 'Top 20 Zip Area with Different Request Type.png'))))
      
    }
  }, deleteFile = FALSE)  
  
  }




