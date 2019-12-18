load("Baseball.rdata")
load("Shiny.rdata")
library(plotly)
library(shiny)



##Created UI
ui<-fluidPage(
  titlePanel(title = "Ultimate Baseball Explorer by: Paul Gunsalus"),
  
  sidebarPanel(
    ## Conditional Panel for MLB over time
    conditionalPanel(condition = "input.tabs =='MLB over Time'",
                     ## Yvar Input
                     selectInput(inputId="yvar", label="Select a Statistic",
                                 choices=mlb.stat.choices),
                     ## Yvar Input for a second plot
                     uiOutput(outputId = "y2var"),
                     ## Slider for Custom Year Range
                     sliderInput("years", label = ("Year Range"), sep = "",min = 1871, 
                                 max = 2018, value = c(1871, 2018))
                     
    ),
    ## Conditional Panel for Career's Range
    conditionalPanel(condition = "input.tabs=='Careers'",
                     ## Input for Y-Variable
                     selectInput(inputId = "careervar", label = "Select a X Variable",
                                 choices=stat.choices),
                     ## Input for X-Variable
                     uiOutput(outputId = "careervar2"),
                     ## Paragraph explaining data on this page
                     p("For an even comparison this data only includes players who
                       began their careers after 1985 and finished before 2016.
                       The Player had to play in at least 500 game, have at least
                       500 At-Bats. This data does include pitchers.")),
    
    ## Conditional Panel for Player Career Evaluation
    conditionalPanel(condition = "input.tabs=='Evaluation'",
                     #Paragraph Description
                     p("Click on Lasso and Use to Select Player(s).
                        Career Dollars per Base is the total number
                        bases produced by a played over their career divided
                        by their Career Earnings Adjusted for Inflation. 
                       The Red Line displays the Median DPB value during this 
                      32 season span $10,600. This means owners on average 
                      paid about $10,600 for each base a player produced
                       during this time period."))
    
                     ),
  ## Main Panel and Tabs
  mainPanel(tabsetPanel(id="tabs",
                        tabPanel("MLB over Time", plotlyOutput("timeplot"),
                                 plotlyOutput("timeplot2")),
                        tabPanel("Careers", plotlyOutput("scatterplot")),
                        tabPanel("Evaluation",plotlyOutput("dotplot"),
                                 plotlyOutput("lineplot")))),
  ## Keeps Errors from Popping UP
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  )
  
    )

## Created server with added session to make plot interactive
server<-function(input, output, session){
  ## Created outputs for the added variable selections
  output$y2var<-renderUI({
    selectInput(inputId="y2var", label="Select Statistic (Plot 2)",
                choices = mlb.stat.choices[mlb.stat.choices != input$yvar],
                selected = input$y2var)
  })
  
  output$careervar2<-renderUI({
    selectInput(inputId = "careervar2", label = "Select a Y Variable",
                choices = stat.choices[stat.choices!= input$careervar],
                selected = input$careervar2)
  })
  
  ## Builind plot for MLB over Time
  output$timeplot<-renderPlotly({
    
    ## Selected Years
    
    selectedMLByears<-filter(MLB.totals, yearID %in% min(input$years):max(input$years))
    
    ##Storing data for hover labels
    p1data<-event_data("plotly_hover")
    
    ## If null show no hover labels
    if(is.null(p1data)){
      p1<-plot_ly(data=selectedMLByears,
                  x=~yearID, y=~get(input$yvar), 
                  ## Custom MLB BLue Colors
                  color=I("#002D72"), hoverinfo='none') %>% 
        add_lines() %>% 
        ## labels for axes and captions
        layout(yaxis=list(title=names(yvars[which(yvars == input$yvar)])), xaxis=list(title="Year"),
               annotations=list(x = 1, y = -0.1, text = "Source: Lahman's Baseball Database", 
                                showarrow = F, xref='paper', yref='paper', 
                                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                font=list(size=8, color="black"))) %>% 
        
        ## Zoom is disabled
        layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
      
      
    }else{
      ## I hover data selected show custom labels
      ## Other annotations are the same
      p1<-plot_ly(data=selectedMLByears,
                  x=~yearID, y=~get(input$yvar), color=I("#002D72"),
                  hoverinfo='text',
                  text=~paste("Year", p1data$x,"<br>",
                              names(yvars[which(yvars == input$yvar)]),
                              round(p1data$y,3), sep=" ")) %>% 
        add_lines() %>% 
        layout(yaxis=list(title=names(yvars[which(yvars == input$yvar)])), xaxis=list(title="Year"),
               annotations=list(x = 1, y = -0.1, text = "Source: Lahman's Baseball Database", 
                                showarrow = F, xref='paper', yref='paper', 
                                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                font=list(size=8, color="black"))) %>% 
        layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    }
    
    p1
  })
   ## Built Second Time Plot for Statistic Comparison
  output$timeplot2<-renderPlotly({
    ## Filtered data for selected MLB Years
    selectedMLByears<-filter(MLB.totals, yearID %in% min(input$years):max(input$years))
    ## Event data for this plot
    p2data<-event_data("plotly_hover")
    ## IF no event data, display no customs hover info
    if(is.null(p2data)){
      p2<-plot_ly(data=selectedMLByears,
                  ## Added MLB Blue dots
                  x=~yearID, y=~get(input$y2var), color=I("#002D72"), hoverinfo='none') %>% 
        add_lines() %>% 
        ## Axes labels and caption added
        layout(yaxis=list(title=names(yvars[which(yvars == input$y2var)])), xaxis=list(title="Year"),
               annotations=list(x = 1, y = -0.1, text = "Source: Lahman's Baseball Database", 
                                showarrow = F, xref='paper', yref='paper', 
                                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                font=list(size=8, color="black"))) %>% 
        
        ##Zoom disabled
        layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    }else{
      
      ##  CUstoms hover displays from eevent data
      p2<-plot_ly(data=selectedMLByears,
                  x=~yearID, y=~get(input$y2var), color=I("#002D72"),hoverinfo='text',
                  text=~paste("Year", p2data$x,"<br>",
                              names(yvars[which(yvars == input$y2var)]),
                              round(p2data$y,3), sep=" ")) %>% 
        add_lines() %>% 
        ## Axes Labels and Caption Added
        layout(yaxis=list(title=names(yvars[which(yvars == input$y2var)])),
               xaxis=list(title="Year"),
               annotations=list(x = 1, y = -0.1, text = "Source: Lahman's Baseball Database", 
                                showarrow = F, xref='paper', yref='paper', 
                                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                font=list(size=8, color="black"))) %>% 
        ## Zoom Disabled
        layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    }
    
    p2
    
  })
  ## Created Scatter Plot for Career Comparison
  output$scatterplot<-renderPlotly({
    ## Event data for this plot
    p4data<-event_data("plotly_hover")
    ## No Custom  hover labels again if not event data
    if(is.null(p4data)){
      p4<-plot_ly(data=selected.careers,
                  x=~get(input$careervar), y=~get(input$careervar2), color=I("#002D72"),
                  type="scatter", hoverinfo='none') %>% 
        ## Axes Labels and Caption Added
        layout(yaxis=list(title=names(yvars[which(yvars == input$careervar2)])),
               xaxis=list(title=names(yvars[which(yvars == input$careervar)])),
               annotations=list(x = 1, y = -0.1, text = "Source: Lahman's Baseball Database", 
                                showarrow = F, xref='paper', yref='paper', 
                                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                font=list(size=8, color="black"))) %>% 
        ## Zoom Disabled
        layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
      
    }else{
      ## Custom labels displayed
      ## Other annotationas are the same
      p4<-plot_ly(data=selected.careers,
                  x=~get(input$careervar), y=~get(input$careervar2), color=I("#002D72"),
                  type="scatter", hoverinfo='text',
                  text=~paste(full.name,"<br>",names(yvars[which(yvars == input$careervar)]),round(p4data$x,3),
                              "<br>",names(yvars[which(yvars == input$careervar2)]),round(p4data$y,3),sep = " ")) %>% 
        layout(yaxis=list(title=names(yvars[which(yvars == input$careervar2)])),
               xaxis=list(title=names(yvars[which(yvars == input$careervar)])),
               annotations=list(x = 1, y = -0.1, text = "Source: Lahman's Baseball Database", 
                                showarrow = F, xref='paper', yref='paper', 
                                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                font=list(size=8, color="black"))) %>% 
        layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    }
    
    p4
    
  })
  
  ## Scatter Plot for Selected story
  output$dotplot<-renderPlotly({
    
    p5<-plot_ly(data=selected.careers,
                x=~total_bases, y=~Car.DPB, color=I("#002D72"), type="scatter",
                hoverinfo= 'text',
                ## Custom labels
                text=~paste0(full.name,
                             '<br>$', round(Car.DPB,2),
                             '<br>Total Bases:', total_bases)) %>% 
      ## Added Median line Custom MLB Red
      add_segments(x=0, xend = 10000, y=Median, yend=Median, name="Median", color=I("#D50032")) %>% 
      #Axes labels and caption
      layout(layout.dragmode="lasso", showlegend=FALSE, title="Career Production",
             yaxis=list(title="Career Dollars Per Base"),
             xaxis=list(title="Total Bases"),
             annotations=list(x = 1, y = -0.1, text = "Source: Lahman's Baseball Database", 
                              showarrow = F, xref='paper', yref='paper', 
                              xanchor='right', yanchor='auto', xshift=0, yshift=0,
                              font=list(size=8, color="black")))
    ## Event data
    s<-event_data("plotly_selected")
    
    p5
    
    
  })
  
  ## Added lineplot for selected story
  output$lineplot<-renderPlotly({
    ## storing event renaming and making a short window for easier filtering
    s<-event_data("plotly_selected")
    names(s)<-c("x","y","total_bases","Car.DPB")
    q<-s$Car.DPB
    r<-(q+0.1)
    t<-(q-0.1)
    ## if no event data do not display a plot
    if(is.null(q)) "Nothing" else 
      sel.data<-filter(selected.players2, Car.DPB <r & Car.DPB>t)
    ## Building plot of all seasons of all players in data set
    p2<-plot_ly(data = selected.players2,
                x=~yearID,y=~total_bases, color=I("gray"),
                hoverinfo='none', showlegend=FALSE) %>% 
      group_by(playerID) %>% 
      add_lines() %>% 
      ## tracing lines of selected data
      filter(full.name %in% sel.data$full.name) %>% 
      add_lines(color=~full.name,
                ## Custom labels
                hoverinfo= 'text', text=~paste0(full.name,"<br>","Total Bases: ", total_bases,
                                                "<br>","Salary: ","$",round(adj2016,2),"<br>","DPB: ",round(DPB,2)), showlegend=TRUE) %>% 
      layout(yaxis=list(title="Total Bases"),
             xaxis= list(title="Year"),
             title="Production Over Time", legend = list(x=0.8, y=0.95),
             annotations=list(x = 1, y = -0.1, text = "Source: Lahman's Baseball Database", 
                              showarrow = F, xref='paper', yref='paper', 
                              xanchor='right', yanchor='auto', xshift=0, yshift=0,
                              font=list(size=8, color="black"))) %>% 
      layout(hovermode="closest")
    
    
    p2
    
  })
  
  
  
  
}
## Display App
shinyApp(ui=ui, server=server)
