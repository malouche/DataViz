library(shiny)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(DT)
library(scales)
library(tidyr)
library(colourpicker)
library(tikzDevice)  # For LaTeX export

# Custom CSS
css <- '
.shiny-input-container {
  margin-bottom: 15px;
}

.main-header {
  background: linear-gradient(to right, #2c3e50, #3498db);
  color: white;
  padding: 20px;
  margin-bottom: 20px;
  border-radius: 5px;
  box-shadow: 0 2px 5px rgba(0,0,0,0.2);
}

.plot-container {
  background: white;
  border-radius: 8px;
  box-shadow: 0 4px 6px rgba(0,0,0,0.1);
  padding: 20px;
  margin-bottom: 20px;
}

.stat-panel {
  background: #f8f9fa;
  border-radius: 8px;
  padding: 15px;
  margin-bottom: 15px;
}

.tab-content {
  padding: 20px;
  background: white;
  border-radius: 0 0 8px 8px;
}

.custom-select {
  border-radius: 4px !important;
  border: 1px solid #ddd !important;
}

.download-button {
  background-color: #2ecc71 !important;
  color: white !important;
  border-radius: 4px !important;
  border: none !important;
  padding: 8px 15px !important;
  transition: background-color 0.3s;
}

.download-button:hover {
  background-color: #27ae60 !important;
}

#plotOutput {
  transition: all 0.3s ease;
}

.variable-select-container {
  display: flex;
  gap: 20px;
  margin-bottom: 20px;
}

.variable-panel {
  flex: 1;
  background: #f8f9fa;
  padding: 15px;
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0,0,0,0.1);
}

.nav-tabs {
  border-bottom: 2px solid #dee2e6;
}

.nav-tabs > li > a {
  margin-right: 2px;
  line-height: 1.42857143;
  border: 1px solid transparent;
  border-radius: 4px 4px 0 0;
  color: #2c3e50;
}

.nav-tabs > li > a:hover {
  border-color: #eee #eee #ddd;
  background-color: #eee;
}

.nav-tabs > li.active > a,
.nav-tabs > li.active > a:hover,
.nav-tabs > li.active > a:focus {
  color: #3498db;
  background-color: #fff;
  border: 1px solid #ddd;
  border-bottom-color: transparent;
  cursor: default;
}
'

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(css)
  ),
  useShinyjs(),
  
  div(class = "main-header",
      h1("DataViz Studio", align = "center"),
      p("Transform Your Data into Compelling Stories", align = "center")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # File input
      fileInput("file1", "Choose CSV File",
                accept = c("text/csv", 
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Data info
      uiOutput("dataInfo"),
      
      # Variable selection panels
      div(class = "variable-select-container",
          div(class = "variable-panel",
              h4("Continuous Variable"),
              uiOutput("contVarSelect"),
              # Continuous plot controls
              uiOutput("contPlotControls")
          ),
          div(class = "variable-panel",
              h4("Categorical Variable"),
              uiOutput("catVarSelect"),
              # Categorical plot controls
              uiOutput("catPlotControls")
          )
      ),
      
      # Plot customization
      uiOutput("plotCustomization"),
      
      # Download options
      conditionalPanel(
        condition = "input.file1",
        hr(),
        div(
          selectInput("downloadFormat", "Export Format:",
                      choices = c("PNG", "PDF", "SVG", "LaTeX")),
          downloadButton("downloadPlot", "Export Plot", class = "download-button")
        )
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "mainTabs",
        type = "tabs",
        tabPanel("Plot", div(class = "plot-container",
                             plotOutput("plotOutput", height = "600px"))),
        tabPanel("Data Preview", DTOutput("dataTable")),
        tabPanel("Statistics", div(class = "stat-panel",
                                   verbatimTextOutput("statistics")))
      )
    )
  )
)



# Server logic
server <- function(input, output, session) {
  # Reactive values
  rv <- reactiveValues(
    data = NULL,
    continuous_vars = NULL,
    categorical_vars = NULL
  )
  
  # Read file and categorize variables
  observeEvent(input$file1, {
    tryCatch({
      df <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
      rv$data <- df
      
      # Categorize variables
      rv$continuous_vars <- names(df)[sapply(df, is.numeric)]
      rv$categorical_vars <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
      
      showNotification("Data loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })
  
  # Display data info
  output$dataInfo <- renderUI({
    req(rv$data)
    div(
      p(strong("Dataset Summary:")),
      p(icon("table"), " Rows: ", nrow(rv$data)),
      p(icon("columns"), " Columns: ", ncol(rv$data)),
      p(icon("exclamation-triangle"), " Missing Values: ", sum(is.na(rv$data)))
    )
  })
  
  # Variable selection UIs
  output$contVarSelect <- renderUI({
    req(rv$continuous_vars)
    selectInput("contVar", "Select Variable:", 
                choices = rv$continuous_vars,
                selected = rv$continuous_vars[1])
  })
  
  output$catVarSelect <- renderUI({
    req(rv$categorical_vars)
    tagList(
      selectInput("catVar", "Select Variable:", 
                  choices = rv$categorical_vars,
                  selected = rv$categorical_vars[1]),
      checkboxInput("useGrouping", "Use as Grouping Variable", FALSE)
    )
  })
  
  # Continuous plot controls
  output$contPlotControls <- renderUI({
    req(input$contVar)
    tagList(
      selectInput("contPlotType", "Plot Type:",
                  choices = c("Histogram + Density" = "histdens",
                              "Box Plot" = "box",
                              "Violin Plot" = "violin")),
      conditionalPanel(
        condition = "input.contPlotType == 'histdens'",
        sliderInput("bins", "Number of bins:",
                    min = 5, max = 100, value = 30),
        checkboxInput("showDensity", "Show Density Curve", TRUE)
      )
    )
  })
  
  # Categorical plot controls
  output$catPlotControls <- renderUI({
    req(input$catVar)
    tagList(
      selectInput("catPlotType", "Plot Type:",
                  choices = c("Bar Plot" = "bar",
                              "Pie Chart" = "pie",
                              "Donut Chart" = "donut")),
      checkboxInput("sortBars", "Sort by Frequency", TRUE)
    )
  })
  
  # Plot customization UI
  output$plotCustomization <- renderUI({
    req(!is.null(input$contVar) || !is.null(input$catVar))
    
    
    baseControls <- list(
      colourInput("fillColor", "Fill Color", value = "#4682B4"),
      sliderInput("transparency", "Transparency:", 
                  min = 0, max = 1, value = 0.7, step = 0.1),
      textInput("plotTitle", "Plot Title:", 
                value = ""),
      selectInput("theme", "Theme:", 
                  choices = c("theme_minimal", "theme_light", 
                              "theme_dark", "theme_classic"))
    )
    
    # Add any type-specific controls
    if (!is.null(input$catPlotType) && input$catPlotType %in% c("pie", "donut")) {
      baseControls <- c(baseControls,
                        list(checkboxInput("showLabels", "Show Labels", TRUE),
                             checkboxInput("showPercentages", "Show Percentages", TRUE)))
    }
    
    do.call(tagList, baseControls)
  })
  
  # Reactive plot
  plot_output <- reactive({
    req(rv$data)
    
    if (!is.null(input$contVar) && !is.null(input$contPlotType)) {
      # Continuous variable plots
      p <- ggplot(rv$data)
      
      if (input$contPlotType == "histdens") {
        p <- p + 
          geom_histogram(aes_string(x = input$contVar, y = "..density.."),
                         bins = input$bins,
                         fill = input$fillColor,
                         alpha = input$transparency)
        
        if (input$showDensity) {
          p <- p + geom_density(aes_string(x = input$contVar),
                                color = "black", linewidth = 1)
        }
      } else if (input$contPlotType == "box") {
        p <- p + 
          geom_boxplot(aes_string(x = 1, y = input$contVar),
                       fill = input$fillColor,
                       alpha = input$transparency)
      } else if (input$contPlotType == "violin") {
        p <- p + 
          geom_violin(aes_string(x = 1, y = input$contVar),
                      fill = input$fillColor,
                      alpha = input$transparency)
      }
      
    } else if (!is.null(input$catVar) && !is.null(input$catPlotType)) {
      # Categorical variable plots
      df_summary <- rv$data %>%
        count(!!sym(input$catVar)) %>%
        arrange(if(input$sortBars) desc(n) else n)
      
      if (input$catPlotType == "bar") {
        # Create a frequency table for the categorical variable
        df_summary <- rv$data %>%
          count(!!sym(input$catVar)) %>%
          arrange(if (input$sortBars) desc(n) else n)
        
        p <- ggplot(df_summary, aes_string(x = input$catVar, y = "n")) +
          geom_bar(stat = "identity",
                   fill = input$fillColor,
                   alpha = input$transparency) +
          labs(x = input$catVar, y = "Frequency") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
      } else if (input$catPlotType %in% c("pie", "donut")) {
        # Create a frequency table with percentages
        df_summary <- rv$data %>%
          count(!!sym(input$catVar)) %>%
          mutate(percentage = n / sum(n) * 100)
        
        p <- ggplot(df_summary, aes_string(x = "1", y = "n", fill = input$catVar)) +
          geom_bar(stat = "identity", width = 1) +
          coord_polar(theta = "y") +
          labs(x = NULL, y = NULL) +
          theme_void()
        
        if (input$catPlotType == "donut") {
          p <- p + xlim(c(0.2, 1.5))  # Adjust inner radius for donut chart
        }
        
        if (input$showLabels) {
          label_text <- if (input$showPercentages) {
            paste0(df_summary[[input$catVar]], "\n(", round(df_summary$percentage, 1), "%)")
          } else {
            df_summary[[input$catVar]]
          }
          
          p <- p + geom_text(aes(label = label_text),
                             position = position_stack(vjust = 0.5),
                             size = 4)
        }
      }
      
    }
    
    # Add theme and labels
    title <- if(input$plotTitle == "") {
      ifelse(!is.null(input$contVar), 
             paste("Distribution of", input$contVar),
             paste("Distribution of", input$catVar))
    } else {
      input$plotTitle
    }
    
    p <- p + 
      get(input$theme)() +
      labs(title = title) +
      theme(plot.title = element_text(hjust = 0.5))
    
    return(p)
  })
  
  # Render plot
  output$plotOutput <- renderPlot({
    plot_output()
  })
  
  # Data preview
  output$dataTable <- renderDT({
    req(rv$data)
    datatable(rv$data, 
              options = list(pageLength = 10, 
                             scrollX = TRUE,
                             dom = 'Bfrtip'),
              rownames = FALSE)
  })
  
  # Statistics output
  output$statistics <- renderPrint({
    req(rv$data)
    
    if (!is.null(input$contVar)) {
      cat("Summary Statistics for", input$contVar, ":\n\n")
      print(summary(rv$data[[input$contVar]]))
      
      cat("\nAdditional Statistics:\n")
      cat("Standard Deviation:", sd(rv$data[[input$contVar]], na.rm = TRUE), "\n")
      cat("Skewness:", moments::skewness(rv$data[[input$contVar]], na.rm = TRUE), "\n")
      cat("Kurtosis:", moments::kurtosis(rv$data[[input$contVar]], na.rm = TRUE), "\n")
      
      if (input$useGrouping && !is.null(input$catVar)) {
        cat("\nGroup-wise Statistics:\n")
        group_stats <- rv$data %>%
          group_by(!!sym(input$catVar)) %>%
          summarise(
            Mean = mean(!!sym(input$contVar), na.rm = TRUE),
            SD = sd(!!sym(input$contVar), na.rm = TRUE),
            Median = median(!!sym(input$contVar), na.rm = TRUE),
            n = n()
          )
        print(group_stats)
      }
    } else if (!is.null(input$catVar)) {
      cat("Frequency Table for", input$catVar, ":\n\n")
      freq_table <- table(rv$data[[input$catVar]])
      print(freq_table)
      
      cat("\nPercentage Distribution:\n")
      prop_table <- prop.table(freq_table) * 100
      print(round(prop_table, 2))
    }
  })
  
  # Download handlers
  output$downloadPlot <- downloadHandler(
    filename = function() {
      ext <- tolower(input$downloadFormat)
      if (ext == "latex") ext <- "tex"
      paste0("plot.", ext)
    },
    content = function(file) {
      if (input$downloadFormat == "LaTeX") {
        tikz(file, width = 6, height = 4)
        print(plot_output())
        dev.off()
        
        # Add LaTeX wrapper
        header <- c(
          "\\documentclass{standalone}",
          "\\usepackage{tikz}",
          "\\usepackage{pgfplots}",
          "\\pgfplotsset{compat=newest}",
          "\\begin{document}",
          ""
        )
        
        tikz_code <- readLines(file)
        footer <- c("", "\\end{document}")
        writeLines(c(header, tikz_code, footer), file)
      } else {
        ggsave(file, plot_output(), 
               device = tolower(input$downloadFormat),
               width = 10, height = 8)
      }
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)