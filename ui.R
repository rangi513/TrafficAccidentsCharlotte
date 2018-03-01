# 
# Choices for drop-downs
vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)


navbarPage("Charlotte Traffic Incidents", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 700, height = "auto",

        h2("XGBoost Explainer"),
        selectInput('gridID', 'Select Location', choices = seq(1:nrow(preds_DF)), selected = 1),

        plotOutput("xgbplot", height = 700),
        textOutput('Click_text')
      ),

      tags$div(id="cite",
        'Queen City Outliers', tags$em('- Traffic Incident Model'), ' by Ryan Angi, Josh Lee, Dylan Swanson, Kieran Derfus, and Mason Kirchner.'
      )
    )
  )

)
