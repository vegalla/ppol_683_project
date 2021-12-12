
## Setup -----------------------------------------------------------------------

library(sf)
library(tmap)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(scales)
library(rsconnect)

## Data ------------------------------------------------------------------------

# Read in processed shape file data:

# Set WD to file directory (assuming that this file is opened in Rstudio)

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load processed city files

dc <-
  st_read('_data/processed/dc_processed.shp')

# user interface ----------------------------------------------------------

ui <- 
  dashboardPage(
    
    dashboardHeader(title = 'Feature Exploration'),
    
    dashboardSidebar(
      
      selectInput(
        inputId = 'year',
        label = 'Select a year:',
        choices = 2010:2017,
        selected = 2017),
      
      radioButtons(
        inputId = 'city',
        label = 'View city:',
        
        # Filter choices for all offenses or specific offense
        
        choiceNames = c('Washington, DC', 
                        'Norfolk, VA'),
        choiceValues = c("dc", 
                         "norfolk")),
      
      checkboxGroupInput(
        
        inputId = 'features',
        label = 'Select trend features:',
        choiceNames = c('Income',
                        'House Value',
                        'Single Father %',
                        'Single Mother %',
                        'Fertility',
                        'College Educated %',
                        'Born in US %',
                        'Only Speaks English %'),
       choiceValues = c('median_income_hh',
                        'median_house_value',
                        'pct_single_father', 
                        'pct_single_mother', 
                        'fertility', 
                        'pct_bachelors',
                        'pct_born_us',
                        'pct_only_speaks_english')),
      
            
      sidebarMenu(
        menuItem('Maps',
                 icon = icon('map'),
                 tabName = 'maps'),
        
        menuItem('Summary Table',
                 icon = icon('table'),
                 tabName = 'tables01'),
        
        menuItem('Trend',
                 icon = icon('chart-line'),
                 tabName = 'charts'),
        
        menuItem('Data Table',
                 icon = icon('table'),
                 tabName = 'tables02')
      )
    ),
    
    dashboardBody(
      tags$head(
        tags$link(
          rel = 'stylesheet',
          type = 'text/css',
          href = 'dashboard_styles.css'
        )
      ),
      
      tabItems(
        
        tabItem(
          tabName = 'maps',
          h2('Map of Economic Features'),
          tmapOutput(outputId = 'map01'),
          h2('Map of Education'),
          tmapOutput(outputId = 'map02'),
          h2('Map of Social Features'),
          tmapOutput(outputId = 'map03')),
        
        tabItem(
          tabName = 'tables01',
          h2('Summary Table of Features (Mean), by GEOID'),
          dataTableOutput(outputId = 'summary_table')),
        
        tabItem(
          tabName = 'charts',
          h2('Trend of Features per Year'),
          plotOutput(outputId = 'plot_trend')),
        
        tabItem(
          tabName = 'tables02',
          h2('Table of Data'),
          dataTableOutput(outputId = 'data_table'))
      )
    )
  )

# server ------------------------------------------------------------------

server <- 
  function(input, output) { 
    
    # Data subsetting and summarizing -------------------------------------
    
    # City selection
    
    city <-
      reactive({
        if (input$city == 'dc') {
          dc
        }
      })
    
    city_tibble <-
      reactive({
        city() %>%
          as_tibble()
      })
    
    # Filtered to year
    
    filtered <-
      reactive({
        city() %>%
          filter(
            
            # Filter to specified year
            
            year == input$year)
        
      })
    
    summarized <-
      reactive({
        filtered() %>%
          as_tibble() %>%
          group_by(GEOID) %>%
          mutate(
            income = mean(mdn_nc_, na.rm = TRUE),
            house_value = mean(mdn_hs_, na.rm = TRUE),
            pct_college_edu = mean(pct_bch, na.rm = TRUE),
            pct_single_father = mean(pct_sngl_f, na.rm = TRUE),
            pct_single_mother = mean(pct_sngl_m, na.rm = TRUE),
            fertility = mean(fertlty, na.rm = TRUE),
            pct_born_us = mean(pct_br_, na.rm = TRUE),
            pct_speaks_eng = mean(pct_n__)) %>%
          select(
            GEOID, 
            income,
            house_value,
            pct_college_edu,
            pct_single_father,
            pct_single_mother,
            fertility,
            pct_born_us,
            pct_speaks_eng)
      })
    
    # Outputs -------------------------------------------------------------
    
    # Map:
    
    output$map01 <-
      renderTmap(
        filtered() %>% 
          
          # Census Tracts, Income
          
          tm_shape() + 
          tm_polygons(
            col = 'mdn_nc_',
            title = 'Income',
            group = 'Income') +
          
          # Census Tracts, House Value
          
          tm_shape(filtered()) + 
          tm_polygons(
            col = 'mdn_hs_',
            title = 'House Value',
            group = 'House Value',
            palette=c("red","blue")) +
          
          tm_layout(legend.outside = TRUE) +
          
          tm_view(bbox = st_bbox(dc))
      )
    
    output$map02 <-
      renderTmap(
        filtered() %>% 
          
          # Census Tracts, % College Educated
          
          tm_shape() +
          tm_polygons(
            col = 'pct_bch',
            title = '% College Educated',
            group = '% College Educated',
            style = "fixed",
            breaks = c(10, 20, 30, 40, 50)) +
          
          tm_layout(legend.outside = TRUE) +
          
          tm_view(bbox = st_bbox(dc))
      )
    
    output$map03 <-
      renderTmap(
        filtered() %>% 
          
          # Census Tracts, % Single Parentage
          
          tm_shape() +
          tm_polygons(
            col = 'pct_sngl_f',
            title = '% Single Fathers',
            group = '% Single Fathers',
            style = "fixed",
            breaks = c(2, 4, 6, 8, 10)) +
          
          tm_shape(filtered()) +
          tm_polygons(
            col = 'pct_sngl_m',
            title = '% Single Mothers',
            group = '% Single Mothers',
            style = "fixed",
            breaks = c(2, 4, 6, 8, 10),
            palette=c("red","blue")) +
          
          tm_layout(legend.outside = TRUE) +
          
          tm_view(bbox = st_bbox(dc))
      )
    
    # Summary table:
    
    output$summary_table <-
      renderDataTable(
        summarized())
    
    # Plot:

    output$plot_trend <-
      renderPlot(
        city() %>%
          as_tibble() %>%
          group_by(year) %>%
          summarize(
            mean_income = mean(
              mdn_nc_, na.rm = TRUE),
            mean_house_value = mean(
              mdn_hs_, na.rm = TRUE),
            mean_pct_edu = mean(
              pct_bch, na.rm = TRUE),
            mean_fertility = mean(
              fertlty, na.rm = TRUE),
            mean_house_value = mean(
              pct_sngl_f, na.rm = TRUE)) %>%
          mutate(
            rescale_income = rescale(
              mean_income, c(0,1)),
            rescale_house_value = rescale(
              mean_house_value, c(0,1)),
            rescale_pct_edu = rescale(
              mean_pct_edu, c(0,1)),
            rescale_fertility = rescale(
              mean_fertility, c(0,1))) %>%
          ggplot(aes(x = year)) +
            geom_line(
              aes(y = rescale_income,
              color = 'red')) +
            geom_line(
              aes(y = rescale_house_value,
              color = 'black')) +
            geom_line(
              aes(y = rescale_pct_edu,
              color = 'blue')) +
            geom_line(
              aes(y = rescale_fertility,
              color = 'pink')) +
            labs(y = "",
                 x = "") +
            ylim(0, 1) +
            scale_x_continuous(breaks = (2010:2017)) +
            scale_color_manual(
              name = 'Feature', 
              values =c(
                'red' = 'red',
                'black' = 'black',
                'blue' = 'blue',
                'pink' = 'pink'), 
              labels = c(
                'Income',
                'House Value',
                '% College Educated',
                'Fertility')) +
            theme_minimal() +
            theme(axis.text.y=element_blank(),
                  axis.ticks.y=element_blank())
      )
    
    # Data Table
    
    output$data_table <-
      renderDataTable(
        city_tibble()
      )
  }

# knit and run app --------------------------------------------------------


shinyApp(ui, server)
