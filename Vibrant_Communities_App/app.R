library(leaflet)
library(stringi)
library(readxl)
library(tidyverse)
library(shiny)
library(rgdal)
library(readr)
library(plotly)
library(sp)
library(shinydashboard)
library(listviewer)
#reading in functions for drawing graphics
source('helpers.R')

#data sources

opportunity <- read_excel("Opportunity Indicators.xlsx", skip =3)

opportunity <- opportunity[1:(nrow(opportunity)-3),]


standardize_data = function (v){
  data_viable <- as.numeric(v[which(is.na(v)==0 & v!='-')])
  
  max_vector = max(data_viable)
  min_vector = min(data_viable)
  standardize_action = function (d){
    
    point = as.numeric(d)

    if(is.na(point)){
      return (NA)
    }
    return((point-min_vector)/(max_vector-min_vector))
  }
  sapply(v, standardize_action)
}

get_third = function(l){
  return(l[[3]])
}
sf = readOGR(dsn = ".//data", 
             layer = "cb_2017_42_tract_500k")

alleg_county_tract_ids = sf$TRACTCE[which(sf$COUNTYFP=='003')]

alleg_county_tract_ids = order(alleg_county_tract_ids)


ind_var = 'Opportunity Composite'
sub_ind_var = "1"
hist_choice_list = vector()
ui <- fluidPage(
   # Application title
   titlePanel(strong("Allegheny County Vibrant Communities")),
   navbarPage(strong('Vibrancy Areas'),
              tabPanel('Opportunity',
                       sidebarLayout(
                         sidebarPanel(
                            p("Made with", a("Shiny", href = "http://shiny.rstudio.com"), "."),
        
                            verbatimTextOutput("Info"),
        
                            selectInput("variable", h5("Sustainable Development Goal:"),
                                c("Opportunity Index", "Poverty Level",
                                "Education",
                                'Economic Participation',
                                'Industry & Infrastructure',
                                'Housing')),
        br(),
        uiOutput('sub_inds'),
        br(),
        br(),

        plotlyOutput('county_com_map', height = 300)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         leafletOutput('testmap', height = 640)
      )
   )
  ),
  tabPanel('Sustainability'),
  tabPanel('Culture'),
  tabPanel('Overall Vibrancy')
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$sub_inds=renderUI({
    if(input$variable == "Opportunity Index"){
      choice_list = c( input$variable) 
      hist_choice_list <<- choice_list
    }
    if(input$variable == "Poverty Level"){
      choice_list = c( paste(input$variable, 'Composite'), '% of Families in Poverty',
                       '% of Females in Poverty',
                       '% of Households on Food Stamps' )
      hist_choice_list <<- choice_list
    }
    if(input$variable == "Education"){
      choice_list = c( paste(input$variable, 'Composite'), 'Pre-K Participation Rate',
                       'Adult K-12 Completion Rate')
      hist_choice_list <<- choice_list
    }
    if(input$variable == "Economic Participation"){
      choice_list = c( paste(input$variable, 'Composite'), 'Youth Unemployment Rate',
                       'K-12 Completion Rate')
      hist_choice_list <<- choice_list
    }
    if(input$variable == "Industry & Infrastructure"){
      choice_list = c( paste(input$variable, 'Composite'), 'Ratio of Manuf. Jobs to All Jobs',
                       'Broadband Access Rate %')
      hist_choice_list <<- choice_list
    }
    if(input$variable == 'Housing'){
        choice_list = c(paste(input$variable, 'Composite'), 'Gross Rent as a % of Household Income')
        hist_choice_list <<- choice_list
        
    }
      
  
    radioButtons("sub_ind_selected", label = h3("Sustainable Development Goal & Indicators"),
                 choiceNames = choice_list, 
                 choiceValues = seq(1, length(choice_list)), 
                 selected = 1)
  })
  
  
   data <- reactive({
     validate(
       need(input$sub_ind_selected, message=FALSE),
       need(input$variable, message = FALSE)
     )
      if(input$variable=="Opportunity Index"){
        sub_inds_selected = input$sub_ind_selected
        
        fam_pov = standardize_data(opportunity$`% Family Poverty`)
        fem_pov = standardize_data(opportunity$`% Female Poverty`)
        hshld_pov = standardize_data(opportunity$`% Hshld Foodstamps`)
        goal_1_composite = apply(cbind((1-fam_pov), (1-fem_pov), (1-hshld_pov)), 1, mean)
        
        pre_k = standardize_data(opportunity$`Percent of kids in Nursery school, preschool`)
        k_12_complet = standardize_data(100 - opportunity$`Percent;  Population 25 years and over  9th to 12th grade, no diploma`)
        goal_4_composite = apply(cbind(pre_k, k_12_complet), 1, mean)
        
        youth_unemp = standardize_data(opportunity$`Unemployment rate; Estimate; AGE - 16 to 19 years`)
        k_12_complet_2 = standardize_data(opportunity$`Percent; Estimate; Percent high school graduate or higher`)
        goal_8_composite = apply(cbind(1-youth_unemp, k_12_complet_2), 1, mean)
        
        
        manuf_jobs = standardize_data(opportunity$`Service occupations; Estimate; Manufacturing`)
        broadband_access = standardize_data(opportunity$`Estimate; Has a computer: - With a broadband Internet subscription (Divided by Total Number of HH per Census Tract to find rate)`)
        goal_9_composite = apply(cbind(manuf_jobs, broadband_access), 1, mean)
        
        goal_11_composite =  1-standardize_data(opportunity$`Percent; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME  35.0 percent or more`)
        
        
        data_used = as.numeric(apply(cbind(goal_1_composite, goal_4_composite, goal_8_composite,
                                goal_9_composite, goal_11_composite), 1, mean))
        
        tracts = strsplit(sapply(strsplit(opportunity$`Census Tract`, 
                                          split = ' '), get_third),
                          split = ',')
        lab_additions = " "
      }
     if(input$variable=="Poverty Level"){
       sub_inds_selected = input$sub_ind_selected
       
       if(1 %in% sub_inds_selected){
         fam_pov = standardize_data(opportunity$`% Family Poverty`)
         fem_pov = standardize_data(opportunity$`% Female Poverty`)
         hshld_pov = standardize_data(opportunity$`% Hshld Foodstamps`)
         data_used = apply(cbind(fam_pov, fem_pov, hshld_pov), 1, mean)
       }
       if(2 %in% sub_inds_selected){
         data_used = opportunity$`% Family Poverty`
       }
       if(3 %in% sub_inds_selected){
         data_used = opportunity$`% Female Poverty`
       }
       if(4 %in% sub_inds_selected){
         data_used =  opportunity$`% Hshld Foodstamps`
       }
       tracts = strsplit(sapply(strsplit(opportunity$`Census Tract`, 
                                         split = ' '), get_third),
                         split = ',')
       lab_additions = " "
     }
     if(input$variable=="Education"){
       sub_inds_selected = input$sub_ind_selected
       
       if(1 %in% sub_inds_selected){
         pre_k = standardize_data(opportunity$`Percent of kids in Nursery school, preschool`)
         k_12_complet = standardize_data(100 - opportunity$`Percent;  Population 25 years and over  9th to 12th grade, no diploma`)
         data_used = apply(cbind(pre_k, k_12_complet), 1, mean)
         
       }
       if(2 %in% sub_inds_selected){
         data_used = opportunity$`Percent of kids in Nursery school, preschool`
       }
       if(3 %in% sub_inds_selected){
         data_used = 100 - opportunity$`Percent;  Population 25 years and over  9th to 12th grade, no diploma`
       }
       tracts = strsplit(sapply(strsplit(opportunity$`Census Tract`, 
                                         split = ' '), get_third),
                         split = ',')
       lab_additions = " "
       
     }
      if(input$variable=="Economic Participation"){
       sub_inds_selected = input$sub_ind_selected
       
         if(1 %in% sub_inds_selected){
           youth_unemp = standardize_data(opportunity$`Unemployment rate; Estimate; AGE - 16 to 19 years`)
           k_12_complet_2 = standardize_data(opportunity$`Percent; Estimate; Percent high school graduate or higher`)
           data_used = apply(cbind(youth_unemp, k_12_complet_2), 1, mean)
         }
         if(2 %in% sub_inds_selected){
           data_used = opportunity$`Unemployment rate; Estimate; AGE - 16 to 19 years` 
         }
         if(3 %in% sub_inds_selected){
           data_used = opportunity$`Percent; Estimate; Percent high school graduate or higher`
         }
       tracts = strsplit(sapply(strsplit(opportunity$`Census Tract`, 
                                         split = ' '), get_third),
                         split = ',')
       lab_additions = " "
       
       }
     
     if(input$variable=="Industry & Infrastructure"){
       sub_inds_selected = input$sub_ind_selected
       
       if(1 %in% sub_inds_selected){
         manuf_jobs = standardize_data(opportunity$`Service occupations; Estimate; Manufacturing`)
         broadband_access = standardize_data(opportunity$`Estimate; Has a computer: - With a broadband Internet subscription (Divided by Total Number of HH per Census Tract to find rate)`)
         data_used = apply(cbind(manuf_jobs, broadband_access), 1, mean)
       }
       if(2 %in% sub_inds_selected){
         data_used = opportunity$`Service occupations; Estimate; Manufacturing`
       }
       if(3 %in% sub_inds_selected){
         data_used = opportunity$`Estimate; Has a computer: - With a broadband Internet subscription (Divided by Total Number of HH per Census Tract to find rate)`
       }
       tracts = strsplit(sapply(strsplit(opportunity$`Census Tract`, 
                                         split = ' '), get_third),
                         split = ',')
       lab_additions = " "
       
     }
     
     else{
       if(input$variable == "Housing"){
         sub_inds_selected = input$sub_ind_selected
         if(1 %in% sub_inds_selected){
           data_used =  standardize_data(opportunity$`Percent; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME  35.0 percent or more`)
            }
         if(2 %in% sub_inds_selected){
           data_used = opportunity$`Percent; GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME  35.0 percent or more` 
          }

         
         tracts = strsplit(sapply(strsplit(opportunity$`Census Tract`, 
                                           split = ' '), get_third),
                           split = ',')
         lab_additions = " "
         
       
       }
     }

    
     data_used_labels = as.numeric(data_used)


     tract_labels <- sprintf(
       "<strong>Allegheny County Census Tract %s</strong><br/>%s %g",
       paste(tracts, ":", sep = ''), lab_additions, data_used_labels
     ) %>% lapply(htmltools::HTML)
     
     return(list(as.numeric(data_used), tract_labels,tracts))

   })
   
   
   
   breaks <- reactive({
     data_used <-data()[[1]]
      
     summ_used = summary(data_used)[c(1, 2, 3, 5, 6)]
     if(summ_used[1] == summ_used[2]){
       summ_used[2] = summ_used[1] + (summ_used[3]-summ_used[1])/2
     }
     if(summ_used[4] == summ_used[5]){
       summ_used[4] = summ_used[3] + (summ_used[5]-summ_used[3])/2
     }
     if(summ_used[1] == summ_used[2] & summ_used[2] == summ_used[3] &
        summ_used[3] == summ_used[4]){
       summ_used[2] = summ_used[5]/4
       summ_used[3] = summ_used[5]/2
       summ_used[4] = 3* summ_used[5]/4
     }
     

     summ_used 
   })
   
   palettes_used <- reactive({
     if(input$variable=="Opportunity Index"){
       pal_used = "Greens"
       
     }
     if(input$variable == "Poverty Level"){
       pal_used = "Reds"
     }
     if(input$variable == "Education"){
         pal_used = "Blues"
     }
     if(input$variable == "Economic Participation"){
       pal_used = "PuRd"
     }
     
     if(input$variable == "Industry & Infrastructure"){
       pal_used = "Greys"
     }
     
     if(input$variable == "Housing"){
       pal_used = "YlOrRd"
     }
     

     pal_used 
   })
   
   
   data_percentile = function(x, data){
       
       paste(round(sum(data<x)/length(data), 2)*100, '%', sep = '')
   }
   
   
   
   output$testmap <- renderLeaflet(({
     validate(
       need(input$variable, message=FALSE),
       need(input$sub_ind_selected, message=FALSE)
       )
     geo_data <- data()
     
     bin_breaks <- breaks()
     pal_used <- palettes_used()
     palette_used = colorBin(pal_used, domain = geo_data[[1]], bins = bin_breaks)
      
       m<- leaflet(sf[which(sf$COUNTYFP=='003')[alleg_county_tract_ids],]) %>%
       addTiles() %>%
       addPolygons(color = "#444444", fillColor = ~palette_used(geo_data[[1]]), 
                   weight = 1.5, smoothFactor = 1, label = ~geo_data[[2]],
                   opacity = .8, layerId = paste(geo_data[[1]], geo_data[[3]]),
                   fillOpacity = 0.75, highlightOptions = highlightOptions(color = "chartreuse4",
                                                                                         weight = 5,
                                                                                         bringToFront = TRUE,
                                                                                         sendToBack = TRUE))
      m<- addLegend(m,"bottomright", pal = palette_used, values = geo_data[[1]],
                 title = input$variable,
                 
                 opacity = 1)
     m
     
   }))
  
   clicks <- reactive({
     if(is.null(input$testmap_shape_click$id)==0){
       strsplit(as.character(input$testmap_shape_click$id), split = ' ')[[1]]
     }
    })
   
   
   
   
   output$county_com_map <- renderPlotly({
     validate(
       need(input$variable, message=FALSE),
       need(input$sub_ind_selected, message=FALSE)
      )

     
     plot_geo_data <- data()
     max_geo_data = max(plot_geo_data[[1]][which(is.na(plot_geo_data[[1]])==0)])
     geo_data <- plot_geo_data[[1]][which(is.na(plot_geo_data[[1]])==0)]

     hist_max <- max(table(cut(geo_data, seq(min(geo_data), max(geo_data), 
                                             length.out = 30))))
      
      click_parts <- clicks()

      if(is.null(click_parts)==0 & input$sub_ind_selected==sub_ind_var & input$variable == ind_var ){
    
        a<-  ggplot()+
             geom_segment(aes(x =min(geo_data), xend = max(geo_data),  y = hist_max*1.1 , 
                              yend = hist_max * 1.1))+
             geom_point(aes(x = as.numeric(click_parts[[1]]), 
                            y = max(hist_max) * 1.1,
                            text = paste("Percentile:", data_percentile(as.numeric(click_parts[[1]]),
                                                                        geo_data))),
                        col = 'coral') +
             
             geom_histogram(aes(x = geo_data), fill = 'dodgerblue4', bins = 30 )+
             labs(title = paste('Relative Position of\nTract',
                                click_parts[[2]],
                                'in Allegheny County'),
                  x = hist_choice_list[(as.numeric(input$sub_ind_selected))],
                  y = 'Frequency Count')+
           theme(plot.title = element_text(size = 8))
      
        
        a <- ggplotly(a, tooltip =c("text"))
        
        a <- config(a, displayModeBar = FALSE)
        
        a <-layout(a, xaxis = list(fixedrange = TRUE),
                   yaxis = list(fixedrange = TRUE))
        if(is.na(as.numeric(click_parts[[1]]))){
             b<- ggplot() +
               geom_text(aes(x = 50, y = 50, label = "NO DATA AVAILABLE"), col = 'red')+
               theme(axis.text = element_blank())
             
             b
           }
           else{
            a 
          
           }
      }
      else{
        c<-  ggplot()+
          geom_histogram(aes(x = geo_data), fill = 'dodgerblue4', bins = 30 )+
          labs(title = paste('Relative Position of Tracts by\n',
                             ifelse(input$sub_ind_selected == "1", input$variable,
                                    paste(input$variable, 'Indicator',
                                          as.numeric(input$sub_ind_selected)-1)),
                                          'in Allegheny County'), 
               x = hist_choice_list[(as.numeric(input$sub_ind_selected))],
               y = 'Frequency Count')+
          theme(plot.title = element_text(size = 8))
        
        ind_var <<- input$variable
        sub_ind_var <<- input$sub_ind_selected
        
        c <- ggplotly(c)
        c <- config(c, displayModeBar = FALSE)
  
        c <- style(c, hoverinfo='none',traces=c(1))
        c
      
      }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

