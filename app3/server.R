# server.R
source("helpers.R")

#Read and Preprocess NYC current Weather data

shinyServer(
    function(input, output) {
        output$text1 <- renderText({ 
            flag <- drop_get(paste0("data/model_",input$var,".RDS"),dtoken = token,overwrite = TRUE)
            if (flag)
            {
                model <- readRDS(paste0("model_",input$var,".RDS"))
                
                group_input <- which(nyc@data$ZIPCODE==input$var)[1] #not precise in case of a few polygons per group
                
                if (!is.na(group_input))
                {
                    zip_code_boundry <- NYboundry@polygons[[group_input]]
                
                    #select proper wather by palce
                    idx <- which.min(deg.dist(zip_code_boundry@labpt[1],zip_code_boundry@labpt[2],stations$LONG,stations$LAT))
                    NYPD_Motor_Vehicle_Collisions <- list(d1,d2,d3,d4)[[idx]]
                    
                    NYPD_Motor_Vehicle_Collisions$HOUR <- factor(NYPD_Motor_Vehicle_Collisions$HOUR,levels = model$forest$xlevels$HOUR)
                    NYPD_Motor_Vehicle_Collisions$Wind_Direction <- factor(NYPD_Motor_Vehicle_Collisions$Wind_Direction,levels = model$forest$xlevels$Wind_Direction)
                    NYPD_Motor_Vehicle_Collisions$Events <- factor(NYPD_Motor_Vehicle_Collisions$Events,levels = model$forest$xlevels$Events)
                    NYPD_Motor_Vehicle_Collisions$Conditions <- factor(NYPD_Motor_Vehicle_Collisions$Conditions,levels = model$forest$xlevels$Conditions)
                    NYPD_Motor_Vehicle_Collisions$Weekday <- factor(NYPD_Motor_Vehicle_Collisions$Weekday,levels = model$forest$xlevels$Weekday)
                    NYPD_Motor_Vehicle_Collisions$Weekend <- factor(NYPD_Motor_Vehicle_Collisions$Weekend,levels = model$forest$xlevels$Weekend)
                    NYPD_Motor_Vehicle_Collisions$Holiday <- factor(NYPD_Motor_Vehicle_Collisions$Holiday,levels = model$forest$xlevels$Holiday)
                    NYPD_Motor_Vehicle_Collisions$Bizday <- factor(NYPD_Motor_Vehicle_Collisions$Bizday,levels = model$forest$xlevels$Bizday)
                    NYPD_Motor_Vehicle_Collisions$Month <- factor(NYPD_Motor_Vehicle_Collisions$Month,levels = model$forest$xlevels$Month)
                    
                    NYPD_Motor_Vehicle_Collisions$Precipitationmm <- Prec_data(input$var)
                    NYPD_Motor_Vehicle_Collisions$TemperatureC <- Temp_data(input$var)
                    NYPD_Motor_Vehicle_Collisions$Wind_SpeedKm_h <- Wind_data(input$var)
                    NYPD_Motor_Vehicle_Collisions$VisibilityKm <- 10 #this should be imported somehow!
                    #NYPD_Motor_Vehicle_Collisions$Humidity 
                        
                    prob <- predict(model,NYPD_Motor_Vehicle_Collisions,type="prob")[,2]
                    
                    paste0("Probability to collison: ",prob*100,"%\n")
                } else {
                    paste0("No Model! \n") 
                }
            } else
            {
                paste0("No Model! \n") 
            }
        })
        output$map <- renderPlot({
            #Find group in polygons (the current zip code)
            group_input <- which(nyc@data$ZIPCODE==input$var)
            if (length(group_input)>1)
            {
                zip_code_boundry <- lapply(group_input,function(x) {NYboundry@polygons[[x]]})
                NYboundryTMP <- NYboundry
                NYboundryTMP@polygons <- zip_code_boundry
            } else {
                if (length(group_input)==1) {NYboundryTMP <- NYboundry@polygons[[group_input]]}
            }

            gg <- ggmap(Newyork) + 
                geom_path(data=NYboundry,
                          aes(x=long, y=lat, group=group),
                          color="black", size=0.25) +
                ggtitle("NYC ZIP CODES\n") + 
                theme(panel.border = element_rect(colour = "gray", fill=NA, size=1))
            if (length(group_input)>0) {
                gg <- gg + geom_polygon(data= NYboundryTMP,
                                        aes(x=long, y=lat, group=group),alpha=0.3,
                                        color="black", fill="red", size=0.25) }
            
            gg
            
        }, width = 1024, height = 800)
    }
)
