# Interactive elements: 
# - Region data selection 
# - slider with animation for years
# - check boxes (multiple choices possible) for color, representing fishery type
# - drop down menu for size (options to choose: MSY, catch, surplus production)
# - check boxes for stock ID to include (default is all)
# - hover text for each point with stock name


require(shiny) 
library(ggplot2) 
library(RColorBrewer) 
library(tidyr) 
library(dplyr) 
library(httr) 
library(shinyBS)

myFile <- "https://raw.githubusercontent.com/krmistry/dynamic_kobe/master/SummaryData.csv"
data <- read.csv(myFile)
data$Region <- data$Region %>% 
  gsub("Russia Japan", "Northwest Pacific", .) %>%
  gsub("Europe non EU", "Norway, Iceland, Faroe Islands", .) %>%
  gsub("European Union", "European Union (non Mediterranean)", .) # region names as they appear on website graphics

data1 <- data[which(data$Year >= 1950), ] 
size_NA_ind <- which(is.na(data1$MSY) & is.na(data1$C) & is.na(data1$P))
data1 <- data1[-size_NA_ind, ]
split <- split(data1, data1$Region)
split <- subset(split, 
                names(split) != "Antarctic" & names(split) != "Other") # these regions are currently excluded from website

region_plot_titles <- names(split)

for (i in 1:length(region_plot_titles)) {
  colnames(split[[i]]) <- c('ID','Year','BoverBMSY','UoverUMSY','Catch',
                            'Surprod','Biomass','u','BMSY','UMSY','MSY',
                            'stockid','Fisherytype','region') 
  for (j in 1:(length(split[[i]]$ID))){
    split[[i]]$BoverBMSY[j] = min(split[[i]]$BoverBMSY[j],3)
    split[[i]]$UoverUMSY[j] = min(split[[i]]$UoverUMSY[j],3)
  }
  split[[i]] <- split[[i]][!is.na(split[[i]]$BoverBMSY) & 
                             !is.na(split[[i]]$UoverUMSY), ]
  split[[i]]$Fisherytype <- factor(split[[i]]$Fisherytype, 
                                   levels = as.character(unique(split[[i]]$Fisherytype)))
}


# For custom colors for fishery type in ggplot 
# (keeps colors and legend consistent throughout animation)
named_FT_Colors <- c("Gadids" = "yellowgreen", "Flatfish" = "palegreen", "Rockfish" = "tomato", 
                     "Forage Fish" = "darkorange", "Tuna and Marlin" = "violet", 
                     "Sharks Rays and Skates" = "mediumpurple", "Other Marine" = "slategray1", 
                     "Pacific Salmon" = "firebrick3", "Invertebrate" = "gold")


size_scale_per_region <- vector("list", length = length(region_plot_titles))
names(size_scale_per_region) <- region_plot_titles
named_custom_FT_Colors <- vector("list", length = length(region_plot_titles))
names(named_custom_FT_Colors) <- region_plot_titles

for (i in 1:length(region_plot_titles)){
  size_scale_per_region[[i]] <- data.frame(matrix(NA, nrow = 3, ncol = 2))
  dimnames(size_scale_per_region[[i]]) <- list(c("MSY", "Catch", "Productivity"), c("min", "max"))
  
  if (all(is.na(split[[i]]$MSY)) == TRUE) {
    size_scale_per_region[[i]][1, ] == c(NA, NA)
  } else {
    size_scale_per_region[[i]][1, ] <- c(max(min(split[[i]]$MSY - 100, na.rm = T), 0), max(split[[i]]$MSY + 100, na.rm = T))
  } 
  
  if (all(is.na(split[[i]]$Catch)) == TRUE) {
    size_scale_per_region[[i]][2, ] == c(NA, NA)
  } else {
    size_scale_per_region[[i]][2, ] <- c(max(min(split[[i]]$Catch - 100, na.rm = T), 0), max(split[[i]]$Catch + 100, na.rm = T))
  }
  
  if (all(is.na(split[[i]]$Catch)) == TRUE) {
    size_scale_per_region[[i]][3, ] == c(NA, NA)
  } else {
    size_scale_per_region[[i]][3, ] <- c(min(split[[i]]$Surprod - 100, na.rm = T), max(split[[i]]$Surprod + 100, na.rm = T))
  }
 
  size_scale_per_region[[i]] <- round(size_scale_per_region[[i]], digits = -2)
  
  # Create list for fishery types found in each region
  named_custom_FT_Colors[[i]] <- data.frame(matrix(NA, nrow = length(levels(split[[i]]$Fisherytype))))
  named_custom_FT_Colors[[i]] <- named_FT_Colors[names(named_FT_Colors) %in% levels(split[[i]]$Fisherytype)]
}


require(shiny)

ui <- fluidPage(
  tags$head(tags$style('
    #my_tooltip {
    position: absolute;
    width: 300px;
    z-index: 100;
    }
  ')),
  tags$script('
    $(document).ready(function(){
        // id of the plot
        $("#plot").mousemove(function(e){ 
              
          // ID of uiOutput
          $("#my_tooltip").show();         
          $("#my_tooltip").css({             
            top: (e.pageY + 5) + "px",             
            left: (e.pageX + 5) + "px"         
          });     
        });     
      });
    '),
  pageWithSidebar(
    titlePanel("Test"), 
    mainPanel(
      plotOutput("plot", 
                 click = "plot_click", 
                 height = "600px"),
      uiOutput("my_tooltip")
    ),
    sidebarPanel(
      wellPanel(
        selectInput("region", "Region", region_plot_titles)
      ),
      wellPanel(
        sliderInput("Year", "Year", 
                    min = min(data1$Year), 
                    max = max(data1$Year), 
                    value = min(split[[1]]$Year),
                    step = 1,
                    sep = "",
                    animate = animationOptions(interval = 1000)), 
        selectInput("size", "Choose variable for point size:",
                    c("MSY" = "MSY",
                      "Catch" = "Catch",
                      "Productivity" = "Surprod")),
        selectInput("stock", "Select stock to display:",
                    c("All",
                      as.character(unique(split[[1]]$ID)))),
        checkboxGroupInput("fishery_type", "Fishery Type:",
                           c(as.character(unique(split[[1]]$Fisherytype))),
                           selected = c(as.character(unique(split[[1]]$Fisherytype))))
      )
    )
  )
)

require(shiny)
library(ggplot2)
library(RColorBrewer)
library(tidyr)

server <- function(input, output, session) {
  myRegion <- reactive({
    input$region
  })

  observe({
    # Isolate and format region data:
    myRegion <- input$region
    region_data <- split[[myRegion]]
    region_data <- data.frame(region_data)
    colnames(region_data) <- c('ID','Year','BoverBMSY','UoverUMSY','Catch',
                               'Surprod','Biomass','u','BMSY','UMSY','MSY',
                               'stockid','Fisherytype','region')
    
    # Update stock and fishery type inputs based on regional data:
    updateSelectInput(session, "stock", "Select stock to display:",
                      c("All",
                        as.character(unique(region_data[, 1]))))
    
    updateCheckboxGroupInput(session, "fishery_type", "Fishery Type:",
                             c(as.character(unique(region_data[, 13]))),
                             selected = c(as.character(unique(region_data[, 13]))))
    
    # Update size scale options based on if data is available in each region:
    if (all(is.na(region_data$MSY)) == TRUE) {
      updateSelectInput(session, "size", "Choose variable for point size:",
                        c("Catch" = "Catch",
                          "Productivity" = "Surprod"))
    # } else if (all(is.na(region_data$Catch)) == TRUE) {
    #   updateSelectInput(session, "size", "Choose variable for point size:",
    #                     c("MSY" = "MSY",
    #                       "Productivity" = "Surprod"))
    # } else if (all(is.na(region_data$Surprod)) == TRUE) {
    #   updateSelectInput(session, "size", "Choose variable for point size:",
    #                     c("MSY" = "MSY",
    #                       "Catch" = "Catch"))
    } else { #at the moment, didn't include options if more than 1 size scale is missing (may need to add in later)
      updateSelectInput(session, "size", "Choose variable for point size:",
                        c("MSY" = "MSY",
                          "Catch" = "Catch",
                          "Productivity" = "Surprod"))
    }
    
    # Update year slider with 1st year with data in selected region:
    updateSliderInput(session, "Year","Year", 
                      min = min(data1$Year), 
                      max = max(data1$Year), 
                      value = max(min(region_data[, 2]), min(data1$Year)))
  })
  
  # When fewer than all fishery types are selected, restrict stock options and adjust year slider:
  observe({
    myFisherytype <- input$fishery_type
    myRegion <- input$region
    region_data <- split[[myRegion]]
    region_data <- data.frame(region_data)
    colnames(region_data) <- c('ID','Year','BoverBMSY','UoverUMSY','Catch',
                               'Surprod','Biomass','u','BMSY','UMSY','MSY',
                               'stockid','Fisherytype','region')
    
    if (identical(myFisherytype, as.character(unique(region_data$Fisherytype))) == FALSE) {
      fishery_type_region_data <- filter(region_data,
                                         Fisherytype %in% myFisherytype)
    
    # Jump to 1st year with data available for that fishery type(s):
    updateSliderInput(session, "Year","Year",
                      min = min(data1$Year),
                      max = max(data1$Year),
                      value = max(min(fishery_type_region_data[, 2]), min(data1$Year)))
    
    # Update stock selection options to only those in this fishery type(s):
    updateSelectInput(session, "stock", "Select stock to display:",
                      c("All",
                        as.character(unique(fishery_type_region_data[, 1]))))
    } else {
      updateSliderInput(session, "Year","Year", 
                        min = min(data1$Year), 
                        max = max(data1$Year), 
                        value = max(min(region_data[, 2]), min(data1$Year)))
      updateSelectInput(session, "stock", "Select stock to display:",
                        c("All",
                          as.character(unique(region_data[, 1]))))
    }
    # myStock <- input$stock
    # FT_stock_region_data <- filter(fishery_type_region_data,
    #                                ID == myStock)
    # }
    # if (myStock != paste("All", myFisherytype, sep = " ")) {
      # myRegion <- input$region
      # region_data <- split[[myRegion]]
      # region_data <- data.frame(region_data)
      #
      # colnames(region_data) <- c('ID','Year','BoverBMSY','UoverUMSY','Catch',
      #                            'Surprod','Biomass','u','BMSY','UMSY','MSY',
      #                            'stockid','Fisherytype','region')
      
      # Jump to 1st year with data available for that stock:
  #     updateSliderInput(session, "Year","Year",
  #                       min = min(data1$Year),
  #                       max = max(data1$Year),
  #                       value = max(min(FT_stock_region_data[, 2]), min(data1$Year)))
  #   }
  #   
  #   # Update size scale options based on what data is available for each stock:
  #   if (all(is.na(FT_stock_region_data$MSY)) == TRUE) {
  #     updateSelectInput(session, "size", "Choose variable for point size:",
  #                       c("Catch" = "Catch",
  #                         "Productivity" = "Surprod"))
  #   } else if (all(is.na(FT_stock_region_data$Catch)) == TRUE) {
  #     updateSelectInput(session, "size", "Choose variable for point size:",
  #                       c("MSY" = "MSY",
  #                         "Productivity" = "Surprod"))
  #   } else if (all(is.na(FT_stock_region_data$Surprod)) == TRUE) {
  #     updateSelectInput(session, "size", "Choose variable for point size:",
  #                       c("MSY" = "MSY",
  #                         "Catch" = "Catch"))
  #   } else { #at the moment, didn't include options if more than 1 size scale is missing
  #     updateSelectInput(session, "size", "Choose variable for point size:",
  #                       c("MSY" = "MSY",
  #                         "Catch" = "Catch",
  #                         "Productivity" = "Surprod"))
  #   }
  })
  
  # When a single stock is selected, update size options and year slider:
  observe({
      myStock <- input$stock
      if (myStock != "All") {
      myRegion <- input$region
      region_data <- split[[myRegion]]
      region_data <- data.frame(region_data)

      colnames(region_data) <- c('ID','Year','BoverBMSY','UoverUMSY','Catch',
                                 'Surprod','Biomass','u','BMSY','UMSY','MSY',
                                 'stockid','Fisherytype','region')
      stock_region_data <- filter(region_data,
                                  ID == myStock)

      # Jump to 1st year with data available for that stock:
      updateSliderInput(session, "Year","Year",
                        min = min(data1$Year),
                        max = max(data1$Year),
                        value = min(stock_region_data[, 2]))
      }

  #    # Update size scale options based on what data is available for each stock:
  #    if (all(is.na(stock_region_data$MSY)) == TRUE) {
  #      updateSelectInput(session, "size", "Choose variable for point size:",
  #                        c("Catch" = "Catch",
  #                          "Productivity" = "Surprod"))
  #    } else if (all(is.na(stock_region_data$Catch)) == TRUE) {
  #      updateSelectInput(session, "size", "Choose variable for point size:",
  #                        c("MSY" = "MSY",
  #                          "Productivity" = "Surprod"))
  #    } else if (all(is.na(stock_region_data$Surprod)) == TRUE) {
  #      updateSelectInput(session, "size", "Choose variable for point size:",
  #                        c("MSY" = "MSY",
  #                          "Catch" = "Catch"))
  #    } else { #at the moment, didn't include options if more than 1 size scale is missing
  #      updateSelectInput(session, "size", "Choose variable for point size:",
  #                        c("MSY" = "MSY",
  #                          "Catch" = "Catch",
  #                          "Productivity" = "Surprod"))
  #    }
  })


  
  # Reactive variables:
  myYear <- reactive({
    input$Year
  })
  myFisherytype <- reactive({
    input$fishery_type
  })
  mySize <- reactive({
    input$size
  })
  myStock <- reactive({
    input$stock
  })
  
  # Isolate min, max and legend titles for size scale in ggplot for selected region:
  min_size <- reactive({
    size_scale <- size_scale_per_region[[myRegion()]]
    if (input$size == "MSY") {
      size_scale[1, 1]
    } else if (input$size == "Catch") {
      size_scale[2, 1]
    } else {
      size_scale[3, 1]
    }
  })
  max_size <- reactive({
    size_scale <- size_scale_per_region[[myRegion()]]
    if (input$size == "MSY") {
      size_scale[1, 2]
    } else if (input$size == "Catch") {
      size_scale[2, 2]
    }  else {
      size_scale[3, 2]
    }
  })
  size_legend_title <- reactive({
    size_scale <- size_scale_per_region[[myRegion()]]
    if (input$size == "MSY") {
      rownames(size_scale)[1]
    } else if (input$size == "Catch") {
      rownames(size_scale)[2]
    }  else {
      rownames(size_scale)[3]
    }
  })

  # Select list of fishery type colors for the selected region:
  custom_colors <- reactive({
    named_custom_FT_Colors[[myRegion()]]
  })

  # Create dataframe for selected region and stock, to pass to all outputs:
  myData <- reactive({
    region_subset <- split[[myRegion()]]
    if (myStock() == "All") {
      data <- filter(region_subset,
                     Year > myYear() - 1,
                     Year < myYear() + 1,
                     Fisherytype %in% myFisherytype()
      )
    } else {
      data <- filter(region_subset,
                     Year > myYear() - 1,
                     Year < myYear() + 1,
                     Fisherytype %in% myFisherytype(),
                     ID %in% myStock())
    }
    data <- as.data.frame(data)
    data
  })
  
  output$plot <- renderPlot({
    ggplot(myData(), aes(x = BoverBMSY,
                         y = UoverUMSY,
                         color = Fisherytype)) +
      geom_point(aes(size = !!as.symbol(mySize())),
                 alpha = 0.8) +
      # annotate("rect", xmin = -Inf, xmax = 1, ymin = 1, ymax = Inf,
      #          fill = "firebrick2", alpha = 0.1) +
      # annotate("rect", xmin = 1, xmax = Inf, ymin = -Inf, ymax = 1,
      #          fill = "yellowgreen", alpha = 0.1) +
      geom_hline(yintercept = 1) +
      geom_vline(xintercept = 1) +
      scale_size_continuous(name = size_legend_title(),
                            limits = c(min_size(), max_size()),
                            labels = scales::comma) +
      ylim(0, 3) +
      xlim(0, 3) +
      scale_color_manual(name = "Fishery Type", 
                         values = custom_colors(),
                         drop = F) +
      theme_light() +
      labs(x = "B/BMSY", y = "U/UMSY") +
      theme(legend.position = "bottom", legend.box = "vertical",
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 18)) +
      guides(color = guide_legend(override.aes = list(size = 5)))
  })
  
  output$my_tooltip <- renderUI({
    click <- input$plot_click 
    y <- nearPoints(myData(), input$plot_click)
    req(nrow(y) != 0)
    verbatimTextOutput("vals")
  })
  
  output$vals <- renderText({
    click <- input$plot_click 
    y <- nearPoints(myData(), input$plot_click)[, c("ID", "MSY", "Catch", "Surprod")]
    req(nrow(y) != 0)
    if (nrow(y) == 1) {
      paste("Stock Name: ", y$ID, "\nMSY: ", y$MSY, "\nCatch: ", 
            y$Catch, "\nProductivity: ", y$Surprod)
    } else {
      paste("\nStock Name: ", y$ID, "\nMSY: ", y$MSY, "\nCatch: ", 
            y$Catch, "\nProductivity: ", y$Surprod) 
    }
  })
}

shinyApp(ui = ui, server = server)

