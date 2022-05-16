# -------------------------------------------------------------------
# Program   : Use of digital technologies in LAC
# Source    : IDB survey
# Country   : Multiple 
# Author    : Laura Goyeneche, Consultant
#             lauragoy@iadb.org / lgoyenechec5@gmail.com
# Dependency: SCL-SPH
# Repository: dashboard_digital_tools_covid19
# Objective : Shinny app
# -------------------------------------------------------------------

# Basics
rm(list = ls(all.names = T))

# Libraries
# -------------------------------------------------------------------

# Install library
# install.packages("rgdal", repos = "http://R-Forge.R-project.org")

# Define packages
packages = c("shiny","shinythemes","shinydashboard","shinyWidgets",
             "leaflet","leaflet.extras","leaflet.minicharts",
             "httr","jsonlite",
             "readxl",
             "rgdal",
             "lubridate","data.table",
             "stringi","stringr","dplyr",
             "DT",
             "plotly")

# Attach packages
invisible(suppressMessages(lapply(packages, library, character.only = T)))

# Data 
# -------------------------------------------------------------------
# Working directory
scldatalake = "s3://cf-p-scldata-prod-s3-p-scldata-app"
recurso     = "/Specialized Survey/Survey on the use of digital tools during COVID-19/"
app_        = "data/app/"
path_       = paste0(scldatalake, recurso, app_)


# Digital technologies
    # Import data
    # 10 countries in LAC
    df = read.csv(paste0(path_,"data-dashboard.csv"), encoding = 'UTF-8')
 
# Indicators
    ind = read.csv(paste0(path_,'module-indicators.csv'), sep = ",")
    names(ind)[1] = "modulo"
    ind = 
        ind %>%
        mutate(modulo    = as.character(modulo),
               indicador = as.character(indicador))

# Output data
    dfOutput = 
        df %>% 
        # Reshape dataset
        reshape2::melt(id.vars       = c("Ponderador","COUNTRY","P1","P2","P0"), 
                       variable.name = "var") %>%
        
        # Process `factor expansion`
        mutate(P0    = P0 * Ponderador, 
               value = as.numeric(value),
               value = value * Ponderador) %>%
        
        # Collapse by country, age, gender, and variable 
        dplyr::group_by(COUNTRY, P1, P2, var) %>%
        
        # summarize variables
        dplyr::summarize(total_ = sum(value, na.rm = T), 
                         count_ = sum(P0)) %>%
        
        # Create percentages
        mutate(n = round((total_ / count_) * 100,2)) %>%
        
        # Select variables of interest
        select(COUNTRY, P1, P2, n, var) %>%
    
        # Rename variables 
        rename(Pais = COUNTRY, 
               Edad = P1, 
               Sexo = P2,
               variable = var, 
               Porcentaje = n) %>%
        
        # Create summary table 
        tidyr::pivot_wider(names_from = Pais, values_from = c("Porcentaje")) %>%
        
        # Format data
        as.data.frame() %>%
        
        # Merge with `indicators` dictionary
        plyr::join(ind, by = "variable") %>%
        
        # Remove NAs
        filter(complete.cases(.)) %>%
        
        # Order variables
        dplyr::relocate(modulo, .after = variable) %>% 
        dplyr::relocate(indicador, .after = modulo) %>%
        
        # Rename variable
        rename(code = variable) %>%
        
        # Drop variables
        select(-notes)
    
# Import Shapefiles
    # Countries in America 
    sh_america = readOGR(dsn     = paste0(path_,"shp/"),
                         layer   = 'Americas',
                         verbose = F)

# Other
# -------------------------------------------------------------------

    # Changes in absolutePanel:
    # For both absolute panel the default color of the background is white
    # With this code, the background is between white and transparent
    absPanel1 = '#panelOpts  {background-color: rgba(255,255,255, 0.7); padding: 0 20px 20px 20px;}'
    absPanel2 = '#graphsOpts {background-color: rgba(255,255,255, 0.7); padding: 0 20px 20px 20px;}'
    
    # Color Palette
    # Generate personlize palette for the choropleth map
    cPal = c('#005B5B','#007979','#009797','#17AEAE','#2EB6B6','#45BEBE','#5CC6C6',
             '#73CECE','#8BD6D6','#A2DEDE','#B9E6E6','#D0EEEE','#E7F6F6')

# User interface 
# -------------------------------------------------------------------
ui = navbarPage(
    
    # Name of the app
    "Uso de Tecnologia en ALC", 
    id = "nav",
    
    # Theme
    theme = shinytheme("flatly"),
    
    # Create different tabs
    tabPanel(
        
        # Interactive Map & Graphs
        #------------------------------------------------------------
        "Mapa Interactivo",
        
        # Leaflet map 
        
        leafletOutput("map", width = "100%", height = 900),
        
        # Interactive graphs
        # Draggable Panel with the graphs
        absolutePanel(
            
            # Define options of absolute panel
            id        = "graphsOpts",
            class     = "panel panel-default",
            draggable = T,
            top       = 100, 
            bottom    = "auto",
            left      = "auto", 
            right     = 450,
            width     = 800, height = "95%",
            
            # Title for absolute panel
            h3(strong("Resultados")),
            h6(strong("Fuente:"), "Entrevista telefonica, BID"),
            
            br(),
            
            # Interactive plots 
            # Country & variable figure
            plotlyOutput("plot3", height = 400),
            
            br(), 
            
            # Estadisticas por genero
            absolutePanel(
                right = 450,
                width = 320, 
                h5(strong("Genero")),
                plotlyOutput("plot1", height = 240)
            ),
            
            # Estadisticas por grupos de edad
            absolutePanel(
                right = 70, 
                width = 320, 
                h5(strong("Grupos de edad")),
                plotlyOutput("plot2", height = 240)
            ),
            
            # With the following code we're applying the changes in absolutePanel 
            # The options are in line 103
            fluidPage(tags$head(tags$style(HTML(absPanel2))))
        ),
        
        # Input options
        # Draggable Panel with input options
        absolutePanel(
            
            # Define options of absolute panel
            id        = "panelOpts",
            class     = "panel panel-default",
            draggable = T,
            top       = 100, 
            bottom    = "auto",
            left      = "auto", 
            right     = 30,
            width     = 400, height = "95%",
            
            # Title for absolute panel
            h3(strong("Filtros")),
            
            # Input options
            # Module name
            uiOutput("input0"),
            
            # Modulo options
            uiOutput("input1"),

            br(),
            
            # With the following code we're applying the changes in absolutePanel 
            # The options are in line 102
            fluidPage(tags$head(tags$style(HTML(absPanel1))))
        )
    ),
    tabPanel(
        
        # Data explorer
        #------------------------------------------------------------
        "Data explorer",
        
        # Title
        h5(strong(paste("Datos"))),
        hr(),
        
        # Data table output
        DT::dataTableOutput("table1"),
        hr(),
        
        # Download Handler
        h5(strong("Download data:")),
        downloadButton('downloadData',"Download data"), 
        hr()
    )
)

# Server function 
# -------------------------------------------------------------------

server = function(input, output, session) {
    
    # Tab - Interactive Map
    # ---------------------------------------------------------------
    
    # Base map
    # ---------------------------------------------------------------
    output$map = renderLeaflet({
        leaflet() %>% 
            
            # Set view in area of interest
            setView(lng  = 23.7, #-38, 
                    lat  = -4.5, #-6.4,
                    zoom = 3) %>%
            
            # Add base maps 
            addProviderTiles("CartoDB.DarkMatterNoLabels"     , group = "World Dark") %>% 
            addProviderTiles(provider = "Esri.WorldGrayCanvas", group = "World Gray") %>%
            
            # Add polygons for districts in Chicago
            addPolygons(data   = sh_america, 
                        color  = "#878B8E",
                        fill   = F, 
                        weight = 1.5) %>%
            
            # Add layers control
            addLayersControl(
                baseGroups = c("World Dark","World Gray"),
                position   = "bottomleft")
    })
    
    # Update module indicators
    # ---------------------------------------------------------------
    # Modulo category
    output$input0 = renderUI({
        radioButtons("input0",
                     h5(strong("Modulo:")),
                     choices  = unique(ind$modulo),
                     selected = "Uso de tecnologia")
    })
    
    # Subset indicators dataframe
    indlistInput = eventReactive(input$input0,{
        if (length(input$input0) > 0) {
            ind_ = 
                ind %>%
                #filter(modulo == "Uso de tecnologia") %>%
                filter(modulo == as.character(input$input0)) %>%
                filter(variable != "")
            
            return(ind_)
        }
    })
    
    # Output indicators in selected module only
    output$input1 = renderUI({
        selectInput("input1",
                    h5(strong("Indicador:")),
                    choices = unique(indlistInput()$indicador), 
                    selected = "% que usa smartphones",
                    width = 450)
    })
    
    # Subset variable name 
    indnameInput = eventReactive(input$input1,{
        if (length(input$input1) > 0) {
            name = 
                indlistInput() %>%
                filter(indicador == as.character(input$input1))
            
            return(name$variable)
        } 
    })
    
    # subset category name
    catnameInput = eventReactive(input$input1, {
        if (length(input$input1) > 0) {
            name = 
                indlistInput() %>%
                filter(indicador == as.character(input$input1)) %>%
                select(category)
            name = as.character(name$category)
            return(name)
        }
    })

    # Leaflet maps 
    # ---------------------------------------------------------------
    
    # Data
    dfTemp = eventReactive (input$input1, {
        # Subset dataset
        temp = df %>%
            select(COUNTRY, Ponderador, P0, indnameInput())   %>%
            rename(val    = indnameInput())                   %>%
            mutate(P0     = ifelse(val >= 0, P0, NA),
                   tot    = val * Ponderador,
                   count_ = P0  * Ponderador)                 %>%                 
            dplyr::group_by(COUNTRY)                          %>%
            dplyr::summarize(total_ = sum(tot, na.rm = T),
                             count_ = sum(count_, na.rm = T)) %>%
            mutate(n = (total_ / count_) * 100)

        return(temp)
    })

    # Map
    observe({
        # Merge `temp` data with country shp
        map      = sh_america
        map@data = plyr::join(map@data, dfTemp(), by = "COUNTRY")

        # Create bins for choropleth map
        dom  = map$n
        bin  = quantile(dom, na.rm = T)
        bin  = round(bin,1)

        # For one of the selections, the number of bins is 1
        # bin length has to be at least 2
        if (length(bin) == 1) {
            bin = c(0, bin)
        }

        # Create and match color palette with bins
        pal  = colorBin(rev(cPal), domain = dom, bins = bin, na.color = 'transparent')

        # Choropleth map
        leafletProxy("map", data = map) %>%

            clearGroup(group = "marker")      %>%
            clearGroup(group = "chloropleth") %>%
            clearControls() %>%

            addPolygons(stroke      = T,
                        fillOpacity = 0.7,
                        fillColor   =~ pal(dom),
                        color       = "white",
                        weight      = 0.3,
                        popup       =~ paste(COUNTRY, " %:",round(n,1)),
                        group       = "chloropleth") %>%

            addLegend(title    = '',
                      pal      = pal,
                      values   =~ n,
                      na.label = '',
                      position = 'topleft')
    })

    # Tab - Statistics
    # ---------------------------------------------------------------

    # Plot 1: Bar plot by gender
    # ---------------------------------------------------------------
    output$plot1 = renderPlotly({
        df %>% 
            # Subset
            select(COUNTRY, Ponderador, indnameInput(), P0, P2) %>%
            rename(val = indnameInput())                        %>%
            mutate(P0     = ifelse(val >= 0, P0, NA),
                   tot    = val * Ponderador,
                   count_ = P0  * Ponderador)                   %>% 
            dplyr::group_by(COUNTRY, P2)                        %>%
            dplyr::summarize(total_ = sum(tot, na.rm = T),
                             count_ = sum(count_, na.rm = T))   %>%
            mutate(n = (total_ / count_) * 100)                 %>% 
            
            # Figure
            plot_ly(x     =~ COUNTRY,
                    y     =~ n,
                    color =~ P2,
                    type  = 'bar',
                    alpha = 0.7) %>%
            
            layout(xaxis         = list(title = '', tickangle = 90),
                   yaxis         = list(title = '', range = c(0,100)),
                   plot_bgcolor  = 'transparent',
                   paper_bgcolor = 'transparent',
                   legend        = list(orientation = 'h',
                                        xanchor     = 'center',
                                        x = 0.5,
                                        y = 1.2),
                   margin        = list(l = 0, r = 0, b = 0, t = 0, pad = 0))
    
    })
    
    # # Plot 2: Bar plot by age groups
    # # ---------------------------------------------------------------
    output$plot2 = renderPlotly({
        df %>% 
            # Subset
            select(COUNTRY, Ponderador, indnameInput(), P0, P1) %>%
            rename(val = indnameInput())                        %>%
            mutate(P0     = ifelse(val >= 0, P0, NA),
                   tot    = val * Ponderador,
                   count_ = P0  * Ponderador)                   %>% 
            dplyr::group_by(COUNTRY, P1)                        %>%
            dplyr::summarize(total_ = sum(tot, na.rm = T),
                             count_ = sum(count_, na.rm = T))   %>%
            mutate(n = (total_ / count_) * 100)                 %>% 
            
            # Figure
            plot_ly(x     =~ COUNTRY,
                    y     =~ n,
                    color =~ P1,
                    type  = 'bar',
                    alpha = 0.7) %>%
            
            layout(xaxis         = list(title = '', tickangle = 90),
                   yaxis         = list(title = '', range = c(0,100)),
                   plot_bgcolor  = 'transparent',
                   paper_bgcolor = 'transparent',
                   legend        = list(orientation = 'h',
                                        xanchor     = 'center',
                                        x = 0.5,
                                        y = 1.2),
                   margin        = list(l = 0, r = 0, b = 0, t = 0, pad = 0))
    })
        
    # # Plot 3: Bar plot by age groups
    # # ---------------------------------------------------------------
    output$plot3 = renderPlotly({
        
        if (indnameInput() %in% subset(ind, ind$cat_id == 1)$variable) {
            # Subset
            temp = 
                df %>% 
                select(COUNTRY, Ponderador, P0, starts_with(catnameInput())) %>%
                # select(COUNTRY, Ponderador, P0, starts_with("P8"))    %>%
                select(COUNTRY, Ponderador, P0, ends_with("_cat"))    %>%
                melt(id = c("COUNTRY","Ponderador","P0"))             %>%
                rename(category = value)                              %>%
                mutate(category = as.character(category))             %>% 
                mutate(variable = as.character(variable))             %>% 
                dplyr::group_by(COUNTRY, variable, category)          %>%
                dplyr::summarize(total_ = sum(Ponderador, na.rm = T)) %>%
                dplyr::group_by(COUNTRY,variable)                     %>%
                mutate(count_ = sum(total_, na.rm = T))               %>%
                mutate(n = (total_ / count_) * 100)                   %>% 
                mutate(category = ifelse(category == "","N/A",category))
            
            # Unique values variable
            nvariable = unique(temp$variable)
            
            # Title
            # title = ind[ind$category == "P8",]$title
            title = ind[ind$category == catnameInput(),]$title
            title = unique(title)
            title = as.character(title)
            title = paste(strwrap(title, width = 90), collapse = "\n")
            
            # Figure
            plots = lapply(nvariable, function(i) {
                # Subtitle name
                subtitle = gsub("_cat","",i)
                subtitle = ind[ind$variable == subtitle,]$category_var
                subtitle = as.character(subtitle)
                
                temp %>% 
                    mutate(COUNTRY  = as.character(COUNTRY),
                           variable = as.character(variable)) %>%
                    filter(variable == i)                     %>%
                    plot_ly(x     =~ COUNTRY,
                            y     =~ n,
                            color =~ category,
                            type  = 'bar',
                            alpha = 0.7,
                            showlegend = (i == nvariable[round(length(nvariable)/2)])) %>%
                    layout(xaxis         = list(title = subtitle, tickangle = 90),
                           yaxis         = list(title = '', range = c(0,100)),
                           plot_bgcolor  = 'transparent',
                           paper_bgcolor = 'transparent',
                           barmode       = 'stack',
                           legend        = list(orientation = 'h',
                                                xanchor     = 'center',
                                                x = 0.5,
                                                y = -0.5),
                           margin        = list(l = 0, r = 0, b = 20, t = 90, pad = 0))
            })
            subplot(plots, nrows = 1, shareY = T, titleX = T) %>% layout(title = list(text = title, y = 1.5, font = list(size = 13)))
        } else {
            df %>% 
                # Subset
                select(COUNTRY, Ponderador, indnameInput(), P0)     %>%
                rename(val = indnameInput())                        %>%
                mutate(P0     = ifelse(val >= 0, P0, NA),
                       tot    = val * Ponderador,
                       count_ = P0  * Ponderador)                   %>% 
                dplyr::group_by(COUNTRY)                            %>%
                dplyr::summarize(total_ = sum(tot, na.rm = T),
                                 count_ = sum(count_, na.rm = T))   %>%
                mutate(n = (total_ / count_) * 100)                 %>% 
                
                # Figure
                plot_ly(x     =~ COUNTRY,
                        y     =~ n,
                        type  = 'bar',
                        alpha = 0.7) %>%
                
                layout(xaxis         = list(title = '', tickangle = 90),
                       yaxis         = list(title = '', range = c(0,100)),
                       plot_bgcolor  = 'transparent',
                       paper_bgcolor = 'transparent',
                       legend        = list(orientation = 'h',
                                            xanchor     = 'center',
                                            x = 0.5,
                                            y = 1.2),
                       margin        = list(l = 0, r = 0, b = 0, t = 0, pad = 0))
        }
    })
    
    
    # Tab - Data Explorer 
    # ---------------------------------------------------------------
    
    # Data table
    # ---------------------------------------------------------------
    output$table1 = DT::renderDataTable({
        DT::datatable(dfOutput,
                      options = list(pageLength = 10), 
                      rownames = F)
    })
    
    # Download
    # ---------------------------------------------------------------
    output$downloadData = downloadHandler(
        filename = function() {paste("data-", Sys.Date(), ".csv", sep = "")},
        content  = function(file) {write.csv(df,file)}
    )
}

# Run app
# -------------------------------------------------------------------
shinyApp(ui = ui, server = server)
# -------------------------------------------------------------------