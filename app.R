
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
  st_read('data/processed/dc_processed.shp')

va <-
  st_read('data/processed/va_processed.shp')

facet_labels <-
  as_labeller(c(
    'mdn_nc_' = 'Median Income', 
    'mdn_hs_' = 'Median House Value',
    'pct_bch' = '% College Educated',
    'pct_sngl_f' = '% Single Father Household',
    'pct_sngl_m' = '% Single Mother Household',
    'fertlty' = 'Fertility', 
    'pct_br_' = '% Born in US', 
    'pct_n__' = '% Speaks only English'))

# user interface ----------------------------------------------------------

ui <- 
  dashboardPage(
    
    dashboardHeader(title = 'Feature Exploration'),
    
    dashboardSidebar(
      
      sidebarMenu(
        menuItem('Home',
                 icon = icon('home'),
                 tabName = 'home'),
        
        menuItem('Maps',
                 icon = icon('map'),
                 tabName = 'maps')),
      
      selectInput(
        inputId = 'year',
        label = 'Select a year:',
        choices = 2010:2017,
        selected = 2017),
      
      radioButtons(
        inputId = 'city',
        label = 'View city:',
        
        # Filter choices for all offenses or specific offense
        
        choiceNames = c(
          'Washington, DC', 
          'Norfolk, VA'),
        choiceValues = c(
          "dc", 
          "norfolk")),
      
      radioButtons(
        
        inputId = 'features',
        label = 'Select feature to map:',
        choiceNames = c(
          'Income',
          'House Value',
          'College Educated %',
          'Single Father %',
          'Single Mother %',
          'Fertility',
          'Born in US %',
          'Only Speaks English %'),
        
        choiceValues = c(
          'median_income_hh',
          'median_house_value',
          'pct_bachelors',
          'pct_single_father', 
          'pct_single_mother', 
          'fertility',
          'pct_born_us',
          'pct_only_speaks_english')),
            
      sidebarMenu(
        
        menuItem('Trend',
                 icon = icon('chart-line'),
                 tabName = 'charts'),
        
        menuItem('Summary Table',
                 icon = icon('table'),
                 tabName = 'tables01'),
        
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
          tabName = 'home',
          h2('Overview'),
          p("This application is intended to assist with the feature exploration
             of the following analysis: Causal Explanation of Gentrification in
             Washington, DC and Norfolk, VA. The analysis implements a two-way 
             fixed effects linear model on both cities individually. "),
          br(),
          p("Featured below are two maps of the cities. The highlighted 
             census tracts (neighborhoods) have been gentrified, according to 
             the National Community Reinvestment Coalition. Over time, these 
             neighborhoods experienced increases in income, house value, and 
             proportion of college educated residents."),
          img(
            src='dc_ncrc_gent.png',
            width = '50%'),
          p("From 2010 - 2017, Washington, DC experienced  a relatively high
             amount of gentrification with 55 of 179 census tracts classified
             in either 2012 or 2017."),
          img(
            src='va_ncrc_gent.png',
            width = '50%'),
          p("By comparison, the city of Norfolk, VA doesn't seem to gentrify to
             the same degree. However, this is only in accordance with NCRC's
             definition. Personal, anecdotal experience considers this city 
             as continuing to gentrify regardless of the NCRC labels.")),
        
        tabItem(
          tabName = 'maps',
          h2('Map of Selected Feature'),
          tmapOutput(outputId = 'map')),
        
        tabItem(
          tabName = 'tables01',
          h2('Summary (Mean) Table of Features, by GEOID'),
          dataTableOutput(outputId = 'summary_table')),
        
        tabItem(
          tabName = 'charts',
          h2('Trend of Features per Year'),
          
          # List Census Tracts
          
          selectInput(
              inputId = 'geoid_select',
              label = NULL,
              choices = c('init')),
          
          plotOutput(outputId = 'plot_trend')
        ),
        
        tabItem(
          tabName = 'tables02',
          h2('Table of Data'),
          dataTableOutput(outputId = 'data_table'))
      )
    )
  )

# server ------------------------------------------------------------------

server <- 
  function(input, output, session) { 
    
    observeEvent(input$city, {
      if(input$city == 'dc'){
        updateSelectInput(
          session, 
          inputId = 'geoid_select', 
          label = 'DC GEOID', 
          choices = c(sort(dc$GEOID)))
      }
      if(input$city == 'norfolk'){
        updateSelectInput(
          session, 
          inputId = 'geoid_select', 
          label = 'VA GEOID', 
          choices = c(sort(va$GEOID)))
      }
    })
    
    observeEvent(input$go,{
      print(input$geoid_select)
      print(input$city)
    })
    # City selection
    
    city <-
      reactive({
        if (input$city == 'dc') {
          dc
        }
        else {
          va
        }
      })
    
    # Convert to tibble
    
    city_tibble <-
      reactive({
        city() %>%
          st_drop_geometry() %>%
          as_tibble()
      })
    
    # Filter to year
    
    filtered <-
      reactive({
        city() %>%
          
          # Filter to input year
          
          filter(
            year == input$year)
        
      })
    
    # Specify 'visualize' variable as input feature
    
    map_filtered <-
      reactive({
        if (input$features == 'median_income_hh') {
          filtered() %>%
            mutate(visualize = mdn_nc_)
        } else if (input$features == 'median_house_value') {
          filtered() %>%
            mutate(visualize = mdn_hs_)
        } else if (input$features == 'pct_bachelors') {
          filtered() %>%
            mutate(visualize = pct_bch)
        } else if (input$features == 'pct_single_father') {
          filtered() %>%
            mutate(visualize = pct_sngl_f)
        } else if (input$features == 'pct_single_mother') {
          filtered() %>%
            mutate(visualize = pct_sngl_m)
        } else if (input$features == 'fertility') {
          filtered() %>%
            mutate(visualize = fertlty)
        } else if (input$features == 'pct_born_us') {
          filtered() %>%
            mutate(visualize = pct_br_)
        } else if (input$features == 'pct_only_speaks_english') {
          filtered() %>%
            mutate(visualize = pct_n__)
        }
      })
    
    # Summary (mean) tables for tab

    summarized <-
      reactive({
        filtered() %>%
          st_drop_geometry() %>%
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
    
    # Data processed for Trend Lines
    
    # Melt features
    
    trend <-
      reactive({
        city_tibble() %>%
        filter(GEOID == input$geoid_select) %>%
        pivot_longer(
          cols = mdn_nc_:pct_n__,
          names_to = 'feature') %>%
        mutate(feature = factor(
          feature,
          levels = c(
            'mdn_nc_', 
            'mdn_hs_',
            'pct_bch',
            'pct_sngl_f',
            'pct_sngl_m',
            'fertlty', 
            'pct_br_', 
            'pct_n__')))
      })
    
    # Outputs -------------------------------------------------------------
    
    # Map:
    
    output$map <-
      renderTmap(
        map_filtered() %>% 
          
          # Census Tracts
          
          tm_shape() + 
          tm_polygons(
            col = 'visualize',
            title = '') +
          
          tm_view(bbox = st_bbox(city()))
      )
    
    # Summary table:
    
    output$summary_table <-
      renderDataTable(
        summarized())
    
    # Plot:
    
    output$plot_trend <-
      renderPlot(
        trend() %>%
        ggplot() + 
          geom_line(aes(
            x = year, 
            y = value),
            size = 2) +
          facet_wrap(
            ~ feature,
            labeller = facet_labels,
            nrow = 4,
            scales = 'free') + 
          labs(
            x = "",
            y = "") +
          scale_x_continuous(
            breaks = 2010:2017) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(
              angle = 90, 
              hjust = 1, 
              vjust = 0.5))
      )
    
    # I would have liked to color each line based on upward/downward trend,
    # but I am unsure of how I would implement this according to inputs.
    
    # Data Table
    
    output$data_table <-
      renderDataTable(
        city_tibble()
      )
  }

# knit and run app --------------------------------------------------------


shinyApp(ui, server)
