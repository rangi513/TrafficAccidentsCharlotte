
# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)


function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -80.8431, lat = 35.2271, zoom = 11)
  })

  output$xgbplot <- renderPlot({
    showWaterfall(xgb_1, explainer, df_full_Dmatrix, df_full_matrix,  as.numeric(input$gridID), type = "binary")
    
  })
  
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(zipdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(cleantable,
      latitude >= latRng[1] & latitude <= latRng[2] &
        longitude >= lngRng[1] & longitude <= lngRng[2])
  })



  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    sizeBy <- input$size

    pal <- colorBin(
      palette = c("green", "red"),
      domain = cleantable$Preds,
      6,
      pretty = FALSE
    )

    leafletProxy("map", data = cleantable) %>%
      clearShapes() %>%
      addCircles(~Long, ~Lat, radius = 400, color = ~pal(cleantable$Preds), stroke = FALSE, fillOpacity = .6) # %>%
  })


  # # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      print(paste('Lat = ', event$lat, 'Long = ', event$lng))
      index <- which(preds_DF$Latitude == event$lat & preds_DF$Longitude == event$lng)
      
      updateSelectInput(session, 'gridID', 'Select Location', choices = seq(1:nrow(preds_DF)), selected = index)
    })
  })


  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })


}
