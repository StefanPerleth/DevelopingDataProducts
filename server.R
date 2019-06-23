# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    helpInfo <- modalDialog(
        title = "App Info",
        "This app shows the values of ozone, wind and temperature in New York City
        from May to September 1973. You can select a time period as you want. The app estimates the 
        missing ozone values with two methods. If there is anough data available in the selected period, 
        a LM estimation can be applied. Otherwise a imputation is done using additional to the selected period 
        30 days before and after the period as donators.",
        easyClose = TRUE,
        fade = TRUE,
        footer = tagList(
            actionButton("close", "Close")
        )
    )
    
    showModal(helpInfo)
    
    # ... or when user wants to change query
    observeEvent(input$help1,
                 {
                     showModal(helpInfo)
                 })

    observeEvent(input$close,
                 {
                     removeModal()
                 })
    
    output$showPlot1 <- renderPlotly({
        period <- input$period
        airqualitys <- airquality %>% 
            dplyr::mutate(Year = "1973") %>% 
            dplyr::mutate(Date = as.Date(paste(Year, Month, Day, sep = "-"))) %>% 
            dplyr::filter(Date >= period[1] & Date <= period[2])
        
        plot1 <- ggplot(airqualitys, aes(Date, Ozone)) + 
            geom_line(colour = "aquamarine4", size = 0.5) + 
            ggtitle("Original Ozone in NYC") + 
            theme_bw(base_family = "Arial", base_size = 16) + 
            theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, face = 'bold'))
        ggplotly(plot1)
    })
    output$showPlot2 <- renderPlotly({
        period <- input$period
        airqualitys <- airquality %>% 
            dplyr::mutate(Year = "1973") %>% 
            dplyr::mutate(Date = as.Date(paste(Year, Month, Day, sep = "-"))) %>% 
            dplyr::filter(Date >= period[1] & Date <= period[2])
        plot2 <- ggplot(airqualitys, aes(Date, Wind)) + 
            geom_line(colour = "royalblue", size = 0.5) + 
            ggtitle("Wind in NYC in Knotes") + 
            theme_bw(base_family = "Arial", base_size = 16) + 
            theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, face = 'bold'))
        ggplotly(plot2)

    })
    output$showPlot3 <- renderPlotly({
        period <- input$period
        airqualitys <- airquality %>% 
            dplyr::mutate(Year = "1973") %>% 
            dplyr::mutate(Date = as.Date(paste(Year, Month, Day, sep = "-"))) %>% 
            dplyr::filter(Date >= period[1] & Date <= period[2])
        plot3 <- ggplot(airqualitys, aes(Date, Temp)) + 
            geom_line(colour = "indianred", size = 0.5) + 
            labs(y = "Temperature") + 
            ggtitle("Temperature in NYC in °F") + 
            theme_bw(base_family = "Arial", base_size = 16) + 
            theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, face = 'bold'))
        ggplotly(plot3)

    })
    output$showPlot4 <- renderPlotly({
        period <- input$period
        airqualityDate <- airquality %>% 
            dplyr::mutate(Year = "1973") %>% 
            dplyr::mutate(Date = as.Date(paste(Year, Month, Day, sep = "-"))) %>% 
            dplyr::filter(Date >= period[1] - 30 & Date <= period[2] + 30)
        set.seed(20160705)
        
        airqualitys <- airqualityDate %>% 
            dplyr::filter(Date >= period[1] & Date <= period[2])
        if ((sum(!is.na(airqualitys$Ozone)) >= 30) & (input$method == "lm")) {
            ozfit <- lm(Ozone ~ Wind + Temp, data = airqualitys, na.action = na.omit)
            airqualitys$ozest <- predict(ozfit, newdata = airqualitys)
            airqualitys$EstimatedOzone <- airqualitys$Ozone
            airqualitys$EstimatedOzone[is.na(airqualitys$Ozone)] <- airqualitys$ozest[is.na(airqualitys$Ozone)]
            plotTitle <- "LM Estimated Ozone in NYC"
            plotSubtitle <- ""
        } else {
            if (sum(!is.na(airqualitys$Ozone)) < 30 & input$method == "lm") {
                plotSubtitle <- "Too few data points. LM not possible!"
            } else plotSubtitle <- ""
            
            airqualityDate <- airqualityDate %>% 
                dplyr::select(Date, Ozone, Wind, Temp) %>% 
                dplyr::mutate(ozest = Ozone)
            airqualityDateImp <- mice(data = airqualityDate[,-2], method = "pmm", maxit = 100, m = 1)
            airqualityDateComp <- complete(airqualityDateImp)
            airqualitys <- airqualityDate %>% 
                dplyr::select(-ozest) %>% 
                dplyr::left_join(airqualityDateComp) %>% 
                dplyr::filter(Date >= period[1] & Date <= period[2]) %>% 
                dplyr::rename(EstimatedOzone = ozest)
            plotTitle <- "Imputated Ozone in NYC"
        }

        plot4 <- ggplot(airqualitys, aes(x = Date)) +
            geom_line(aes(y = EstimatedOzone), colour = "darkorchid", size = 0.5) +
            geom_line(aes(y = Ozone), colour = "aquamarine4", size = 0.5) +
            labs(y = "(Estimated) Ozone") +
            ggtitle(plotTitle) +
            theme_bw(base_family = "Arial", base_size = 16) +
            theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, face = 'bold')) +
            annotate("text", label = "Estimated Ozone", x = period[1] + floor((period[2] - period[1])/2),
                     y = max(rbind(airqualitys$EstimatedOzone, airqualitys$Ozone), na.rm = TRUE),
                     colour = "darkorchid") +
            annotate("text", label = "Ozone", x = period[1] + floor((period[2] - period[1])/2),
                 y = max(rbind(airqualitys$EstimatedOzone, airqualitys$Ozone), na.rm = TRUE) * 0.92,
                 colour = "aquamarine4") + 
            annotate("text", label = plotSubtitle, x = period[1] + floor((period[2] - period[1])/2),
                 y = max(rbind(airqualitys$EstimatedOzone, airqualitys$Ozone), na.rm = TRUE) * 0.5,
                 colour = "red")
        
            ggplotly(plot4)


    })
    
}
)