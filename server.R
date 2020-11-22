


function(input, output) {
  
  reactive({withProgress(message = 'Loading data',
                                                 detail = 'This should take around 15 seconds...', value = 0, {
                                                   incProgress(1/15)
  shape <- readOGR('shape', layer = 'shape1')
  incProgress(1/15)
  incProgress(1/15)
  incProgress(1/15)
  County_C <- read_csv(file = "County_C.csv")
  incProgress(1/15)
  shape_C <- merge(shape, County_C, by = "fipscode")
  incProgress(1/15)
  incProgress(1/15)
  shape_C@data$GEOID.x <- as.numeric(shape_C@data$GEOID.x)
  shape_C@data$GEOID.y <- as.numeric(shape_C@data$GEOID.y)
  shape_C@data$smoking_prop <- as.numeric(shape_C@data$smoking_prop)
  shape_C@data$obesity_prop <- as.numeric(shape_C@data$obesity_prop)
  shape_C@data$rural_prop <- as.numeric(shape_C@data$rural_prop)
  shape_C@data$median_income <- as.numeric(shape_C@data$median_income)
  shape_C@data$diabetes_prop <- as.numeric(shape_C@data$diabetes_prop)
  shape_C@data$polution_prop <- as.numeric(shape_C@data$polution_prop)
  shape_C@data$high_school_prop <- as.numeric(shape_C@data$high_school_prop)
  shape_C@data$alcoholic_prop <- as.numeric(shape_C@data$alcoholic_prop)
  shape_C@data$identity <- paste(paste(shape_C$NAMELSAD, ",", sep = ''), shape_C$StateAb)
  County_T <- fread("County_T.csv") 
  incProgress(1/15)
  incProgress(1/15)
  incProgress(1/15)
  incProgress(1/15)
  incProgress(1/15)
  incProgress(1/15)
  incProgress(1/15)
  US_Health <- read_csv(file = "US_Health.csv")
  
  county_T <- County_T %>%
    filter(days == 188)
  
  shape_T <- sp::merge(shape, county_T,by = "fipscode")
  incProgress(1/15)
  shape_T@data$GEOID.x <- as.numeric(shape_T@data$GEOID.x)
  shape_T@data$GEOID.y <- as.numeric(shape_T@data$GEOID.y)
  shape_T@data$smoking_prop <- as.numeric(shape_T@data$smoking_prop)
  shape_T@data$obesity_prop <- as.numeric(shape_T@data$obesity_prop)
  shape_T@data$rural_prop <- as.numeric(shape_T@data$rural_prop)
  shape_T@data$median_income <- as.numeric(shape_T@data$median_income)
  shape_T@data$diabetes_prop <- as.numeric(shape_T@data$diabetes_prop)
  shape_T@data$polution_prop <- as.numeric(shape_T@data$polution_prop)
  shape_T@data$high_school_prop <- as.numeric(shape_T@data$high_school_prop)
  shape_T@data$alcoholic_prop <- as.numeric(shape_T@data$alcoholic_prop)
                                                 })
  })
  
  
  data <- reactive({ withProgress(message = 'Loading data',
                                  detail = 'This should take around 5 seconds...', value = 0, {
    
    type <- switch(input$type,
                   mang = 'mgmt_prof_prop',
                   alc = 'alcoholic_prop',
                   diabetes= 'diabetes_prop',
                   csmoking = 'smoking_prop',
                   obesity = 'obesity_prop',
                   male = 'male_prop',
                   white = 'white_prop',
                   hispanic = 'hispanic_prop',
                   over_65 = 'age_over_65_prop',
                   hs = 'high_school_prop',
                   phone = 'phone_service_prop',
                   novehicle = 'vehicle_none_prop',
                   polution = 'polution_prop',
                   int = 'internet_prop',
                   young = 'age_under_5_prop',
                   poverty = 'poverty_prop',
                   crowd = 'crowded_prop',
                   minc = 'median_income'
    )
    type
  })
  })
  
  data1 <- reactive({ withProgress(message = 'Loading data',
                                   detail = 'This should take around 5 seconds...', value = 0, {
    day <- input$day
    day
  })
  })
  
  data2 <- reactive({ withProgress(message = 'Loading data',
                                   detail = 'This should take around 5 seconds...', value = 0, {
    type1 <- switch(input$type1,
                    mang = 'mgmt_prof_prop',
                    alc = 'alcoholic_prop',
                    diabetes= 'diabetes_prop',
                    csmoking = 'smoking_prop',
                    obesity = 'obesity_prop',
                    male = 'male_prop',
                    white = 'white_prop',
                    hispanic = 'hispanic_prop',
                    over_65 = 'age_over_65_prop',
                    hs = 'high_school_prop',
                    phone = 'phone_service_prop',
                    novehicle = 'vehicle_none_prop',
                    polution = 'polution_prop',
                    int = 'internet_prop',
                    young = 'age_under_5_prop',
                    poverty = 'poverty_prop',
                    crowd = 'crowded_prop',
                    minc = 'median_income'
    )
    type1
  })
  })
  
  
  
  data3 <- reactive({ withProgress(message = 'Loading data',
                                   detail = 'This should take around 5 seconds...', value = 0, {
    day1 <- input$day1
    day1
  })
  })
  
  scale <- reactive({ withProgress(message = 'Loading data',
                                   detail = 'This should take around 5 seconds...', value = 0, {
    scale <- input$scale
    scale
  })
  })
  
  searchc <- reactive({ withProgress(message = 'Loading data',
                                     detail = 'This should take around 5 seconds...', value = 0, {
    data4 <- input$searchc
    data4
  })
  })
  
  searchs <- reactive({withProgress(message = 'Loading data',
                                     detail = 'This should take around 5 seconds...', value = 0, {
    data5 <- input$searchs
    data5
  })
  })
  
  labels <- reactive({withProgress(message = 'Loading data',
                                   detail = 'This should take around 5 seconds...', value = 0, {
    labels1 <- sprintf("<strong> %s </strong> <br/>
  Prevalence: %s %%", shape_T$NAME, round(shape_C@data[,data()]*100, 2)
    ) %>%
      lapply(htmltools::HTML)
    
    labels1
  })
  })
  
  labels4 <- reactive({withProgress(message = 'Loading data',
                                    detail = 'This should take around 5 seconds...', value = 0, {
    labels5 <- sprintf("<strong> %s </strong> <br/>
  $%s", shape_T$NAME, shape_C@data[,data()]
    ) %>%
      lapply(htmltools::HTML)
    
    labels5
  })
  })
  
  labels6 <- reactive({withProgress(message = 'Loading data',
                                    detail = 'This should take around 5 seconds...', value = 0, {
    labels7 <- sprintf("<strong> %s </strong> <br/>
  PM2.5 Concentration: %s ug/m3", shape_T$NAME, shape_C@data[,data()]
    ) %>%
      lapply(htmltools::HTML)
    
    labels7
  })
  })
  
  labels2 <- reactive({withProgress(message = 'Loading data',
                                    detail = 'This should take around 5 seconds...', value = 0, {
    labels3 <- sprintf("<strong> %s </strong> <br/>
  Cases Per One-Hundred-Thousand: %s", shape_T$NAME, round(shape_T$case_per_oht, 0)
    ) %>%
      lapply(htmltools::HTML)
    
    labels3
  })
  })
  
  
  ## Make plot 
  
  output$plot <- renderLeaflet({
    withProgress(message = 'Loading data',
                 detail = 'This should take around 5 seconds...', value = 0, {
    type <- input$type
    pal <- colorNumeric(
      palette = "Blues",
      domain = shape_C@data[,data()])
    leaflet(shape_C) %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7,
                  color = ~pal(eval(parse(text = data()))), 
                  label = if (data() == 'median_income') {
                    labels4()} else if (data() == 'polution_prop') {labels6()} else {labels()},
                  labelOptions = labelOptions(
                    style = list(
                      "font-weight" = "normal",
                      padding = "3px 8px"),
                    textsize = "15px", direction = "auto")) %>%
      addProviderTiles(providers$Stamen.TonerLite) %>%
      setView(lng = -98.583, lat = 39.833, zoom = 5) %>%
      addLegend("bottomright", pal = pal, title = 'Attribute Prevalence',  values = ~eval(parse(text = data())),
                opacity = 1
      )
  })
  })
  
  
  output$plot1 <- renderLeaflet({withProgress(message = 'Loading data',
                                              detail = 'This should take around 5 seconds...', value = 0, {
    day <- input$day
    county_T <- County_T %>%
      filter(days == data1())
    shape_T <- sp::merge(shape, county_T,by = "fipscode")
    shape_T@data$GEOID.x <- as.numeric(shape_T@data$GEOID.x)
    shape_T@data$GEOID.y <- as.numeric(shape_T@data$GEOID.y)
    shape_T@data$smoking_prop <- as.numeric(shape_T@data$smoking_prop)
    shape_T@data$obesity_prop <- as.numeric(shape_T@data$obesity_prop)
    shape_T@data$rural_prop <- as.numeric(shape_T@data$rural_prop)
    shape_T@data$median_income <- as.numeric(shape_T@data$median_income)
    shape_T@data$diabetes_prop <- as.numeric(shape_T@data$diabetes_prop)
    shape_T@data$polution_prop <- as.numeric(shape_T@data$polution_prop)
    shape_T@data$high_school_prop <- as.numeric(shape_T@data$high_school_prop)
    shape_T@data$alcoholic_prop <- as.numeric(shape_T@data$alcoholic_prop)
    
    pal1 <- colorNumeric(
      palette = c("#a6cee3", "#1f78b4", '#7570b3'),
      domain = shape_T$case_per_oht)
    leaflet(shape_T) %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7,
                  color = ~pal1(case_per_oht), 
                  label = labels2(),
                  labelOptions = labelOptions(
                    style = list(
                      "font-weight" = "normal",
                      padding = "3px 8px"),
                    textsize = "15px", direction = "auto")) %>%
      addProviderTiles(providers$Stamen.TonerLite) %>%
      setView(lng = -98.583, lat = 39.833, zoom = 5) %>%
      addLegend("bottomright", pal = pal1, title = 'Cases Per One-Hundred-Thousand',  values = ~case_per_oht,
                opacity = 1)
  })
  })
  
  output$plot2 <- renderPlot({withProgress(message = 'Loading data',
                                           detail = 'This should take around 5 seconds...', value = 0, {
    day1 <- input$day1
    type1 <- input$type1
    county_T <- County_T %>%
      filter(days == data3())
    shape_T <- sp::merge(shape, county_T,by = "fipscode")
    shape_T@data$GEOID.x <- as.numeric(shape_T@data$GEOID.x)
    shape_T@data$GEOID.y <- as.numeric(shape_T@data$GEOID.y)
    shape_T@data$smoking_prop <- as.numeric(shape_T@data$smoking_prop)
    shape_T@data$obesity_prop <- as.numeric(shape_T@data$obesity_prop)
    shape_T@data$rural_prop <- as.numeric(shape_T@data$rural_prop)
    shape_T@data$median_income <- as.numeric(shape_T@data$median_income)
    shape_T@data$diabetes_prop <- as.numeric(shape_T@data$diabetes_prop)
    shape_T@data$polution_prop <- as.numeric(shape_T@data$polution_prop)
    shape_T@data$high_school_prop <- as.numeric(shape_T@data$high_school_prop)
    shape_T@data$alcoholic_prop <- as.numeric(shape_T@data$alcoholic_prop)
    shape_T@data %>%
      ggplot(aes(x= as.numeric(eval(parse(text = data2()))), y= case_per_oht, color = case_per_oht)) + 
      geom_point() +
      scale_color_gradient(low = "#99d8c9",
                           high = "#de2d26", 
                           na.value = "grey70",
                           name = 'Cases Per One-Hundred Thousand') +
      labs(x = "Attribute Percentage", y = "Cases Per One-Hundred Thousand") +
      scale_y_continuous(limits = c(0, 7500)) +
      geom_smooth(formula = y ~ x, method = "lm",se = FALSE, color="grey50") +
      ggtitle("Demogrphic Statistics and COVID Progression for US Counties") 
  })}, height = 650, width = 800)
  
  
  output$plot3 <- renderPlot({withProgress(message = 'Loading data',
                                           detail = 'This should take around 5 seconds...', value = 0, {
    barc <- shape_C@data[shape_C@data$identity == searchc(),] %>%
      dplyr::select(mgmt_prof_prop, alcoholic_prop, diabetes_prop, smoking_prop, obesity_prop, male_prop, white_prop,
             hispanic_prop, age_over_65_prop, high_school_prop, phone_service_prop, vehicle_none_prop, internet_prop, age_under_5_prop, poverty_prop, crowded_prop) %>%
      rename(Management = mgmt_prof_prop, Alcoholism = alcoholic_prop, Diabetes = diabetes_prop, Smoker = smoking_prop, Obese = obesity_prop, Male=male_prop, White=white_prop,
             Hispanic=hispanic_prop, Elderly=age_over_65_prop, 'High School Degree' = high_school_prop, Phone = phone_service_prop, 'No Vehicle' = vehicle_none_prop, Internet = internet_prop, Infants = age_under_5_prop, Poverty = poverty_prop, Crowded = crowded_prop) %>%
      gather('Attribute', 'Percentage')
    barc$type <- 'County Percentage'
    barn <- US_Health %>%
      dplyr::select(mgmt_prof_prop, alcoholic_prop, diabetes_prop, smoking_prop, obesity_prop, male_prop, white_prop,
             hispanic_prop, age_over_65_prop, high_school_prop, phone_service_prop, vehicle_none_prop, internet_prop, age_under_5_prop, poverty_prop, crowded_prop) %>%
      rename(Management = mgmt_prof_prop, Alcoholism = alcoholic_prop, Diabetes = diabetes_prop, Smoker = smoking_prop, Obese = obesity_prop, Male=male_prop, White=white_prop,
             Hispanic=hispanic_prop, Elderly=age_over_65_prop, 'High School Degree' = high_school_prop, Phone = phone_service_prop, 'No Vehicle' = vehicle_none_prop, Internet = internet_prop, Infants = age_under_5_prop, Poverty = poverty_prop, Crowded = crowded_prop) %>%
      gather('Attribute', 'Percentage')
    barn$type <- 'National Percentage'
    ggplot() + geom_bar(data = barc, stat = 'identity',aes(x= Attribute, y = Percentage*100, fill = type)) + scale_fill_manual(name = element_blank(), values=c("navy")) + scale_color_manual(name = element_blank(), values = ('red')) + scale_y_continuous(limits = c(0, 100)) + labs(x = 'Attribute', y = 'Percent of Population') + coord_flip() + geom_point(data = barn, aes(x= Attribute, y = Percentage*100, color = 'National Percentage'), shape = 23, fill = 'red', size= 3) +  ggtitle("US National and County Demographic Information") 
  })}, height = 600, width = 800)
  
  
  
  output$plot4 <- renderPlot({withProgress(message = 'Loading data',
                                           detail = 'This should take around 5 seconds...', value = 0, {
    covidbarc <- shape_C@data[shape_C@data$identity == searchc(),] %>%
      dplyr::select(case_per_oht) %>%
      rename('Cases Per One-Hundred Thousand' = case_per_oht) %>%
      gather('Attribute', 'Cases')
    covidbarn <- US_tests %>%
      dplyr::select(case_per_oht) %>%
      rename('Cases Per One-Hundred Thousand' = case_per_oht) %>%
      gather('Attribute', 'Cases')
    ggplot(covidbarc, aes(x= Attribute, y = Cases)) + geom_bar(stat = 'identity', fill='navy') + labs(x = 'Attribute', y = 'Cases Per One-Hundred Thousand') + coord_flip() + geom_point(data = covidbarn, aes(x= Attribute, y = Cases), shape = 23, color = 'red', fill = 'red', size= 3) +  ggtitle("US National and County COVID Cases Per One-Hundred Thousand") + theme(axis.title.y = element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
  })}, height = 75, width = 800)
  
  
}

