#' Launch an interactive Shiny helper for map projections and plot extents
#'
#' Intended as a small utility for exploring how maps behave under different
#' coordinate reference systems, especially for regional, polar, and world maps.
#'
#' @return
#' A Shiny application object. Running `ne_projection_helper()` launches
#' the interactive application via `shiny::shinyApp()`.
#'
#' @importFrom ggplot2 coord_sf geom_sf ggplot labs theme_minimal
#' @importFrom rnaturalearth ne_countries
#' @import rnaturalearthdata
#' @import rnaturalearthhires
#' @importFrom sf st_bbox st_crs st_transform
#' @importFrom shiny tags column fluidPage fluidRow h4 observeEvent plotOutput reactive renderPlot renderTable renderUI selectInput shinyApp sliderInput tableOutput titlePanel uiOutput updateSelectInput updateSliderInput
#' @importFrom stats setNames
#' @export
ne_projection_helper <- function(){
  # ------------------------------------------------------------------------------
  # Projection choices
  # ------------------------------------------------------------------------------
  projections <- data.frame(
    name = c(
      "WGS84 geographic",
      "Web Mercator",
      "Arctic Polar Stereographic",
      "Antarctic Polar Stereographic",
      "ETRS89 / LAEA Europe",
      "World Mollweide",
      "World Robinson"
    ),
    crs = c(
      "EPSG:4326",
      "EPSG:3857",
      "+proj=ups",
      "+proj=ups +south",
      "EPSG:3035",
      "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs",
      "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs"
    )
  )

  # ------------------------------------------------------------------------------
  # Presets
  # ------------------------------------------------------------------------------
  presets <- list(
    "Norway" = list(
      west = 4, east = 32, south = 57, north = 72,
      projection = "EPSG:4326"
    ),
    "Europe" = list(
      west = -15, east = 40, south = 34, north = 72,
      projection = "EPSG:3035"
    ),
    "Arctic" = list(
      west = -180, east = 180, south = 60, north = 90,
      projection = "+proj=ups"
    ),
    "Antarctica" = list(
      west = -180, east = 180, south = -90, north = -60,
      projection = "+proj=ups +south"
    ),
    "World" = list(
      west = -180, east = 180, south = -90, north = 90,
      projection = "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs"
    )
  )

  # ------------------------------------------------------------------------------
  # Natural Earth detail mapping
  # ------------------------------------------------------------------------------
  detail_options <- data.frame(
    name = c("Low (110)", "Medium (50)", "High (10)"),
    value = c("110", "50", "10")
  )

  # ------------------------------------------------------------------------------
  # Convert decimal degrees to degrees + minutes + seconds
  # ------------------------------------------------------------------------------
  to_dms <- function(x, type = c("lon", "lat")) {
    type <- match.arg(type)

    hemisphere <- if (type == "lon") {
      if (x < 0) "W" else "E"
    } else {
      if (x < 0) "S" else "N"
    }

    ax <- abs(x)
    degrees <- floor(ax)
    minutes_full <- (ax - degrees) * 60
    minutes <- floor(minutes_full)
    seconds <- (minutes_full - minutes) * 60

    sprintf("%d\u00B0 %d' %.2f\" %s", degrees, minutes, seconds, hemisphere)
  }

  # ------------------------------------------------------------------------------
  # Generate paste-ready ggplot code
  # ------------------------------------------------------------------------------
  make_code <- function(w, s, e, n, projection, scale) {
    paste(
      "library(sf)",
      "library(rnaturalearth)",
      "library(ggplot2)",
      "",
      sprintf("world <- ne_countries(scale = %s, returnclass = 'sf')", scale),
      sprintf("projection <- \"%s\"", projection),
      "",
      "# Geographic bbox in lon/lat",
      sprintf(
        "bbox_ll <- st_bbox(c(xmin = %.1f, ymin = %.1f, xmax = %.1f, ymax = %.1f), crs = st_crs(4326))",
        w, s, e, n
      ),
      "",
      "# Transform bbox (densify necessary for edge cases in polar regions)",
      "bb <- st_transform(bbox_ll, crs = st_crs(projection), densify = 181)",
      "world_pr <- st_transform(world, projection)",
      "",
      "ggplot() +",
      "  geom_sf(data = world_pr) +",
      "  coord_sf(",
      "    xlim = c(bb['xmin'], bb['xmax']),",
      "    ylim = c(bb['ymin'], bb['ymax']),",
      "    expand = FALSE,",
      "    crs = st_crs(projection)",
      "  ) +",
      "  theme_minimal(base_size = 12)",
      sep = "\n"
    )
  }

  # ------------------------------------------------------------------------------
  # UI
  # ------------------------------------------------------------------------------
  ui <- fluidPage(
    titlePanel("rnaturalearth projection helper"),

    fluidRow(
      column(
        width = 4,

        selectInput(
          inputId = "preset",
          label = "Preset",
          choices = names(presets),
          selected = "Europe"
        ),

        selectInput(
          inputId = "projection",
          label = "Projection",
          choices = setNames(projections$crs, projections$name),
          selected = "EPSG:3035"
        ),

        selectInput(
          inputId = "detail",
          label = "Map detail",
          choices = setNames(detail_options$value, detail_options$name),
          selected = "50"
        ),

        tags$hr(),

        sliderInput(
          inputId = "west",
          label = "West",
          min = -180,
          max = 180,
          value = -15,
          step = 0.5
        ),

        sliderInput(
          inputId = "east",
          label = "East",
          min = -180,
          max = 180,
          value = 40,
          step = 0.5
        ),

        sliderInput(
          inputId = "south",
          label = "South",
          min = -90,
          max = 90,
          value = 34,
          step = 0.5
        ),

        sliderInput(
          inputId = "north",
          label = "North",
          min = -90,
          max = 90,
          value = 72,
          step = 0.5
        ),

        tags$hr(),

        h4("Extent coordinates"),
        tableOutput("coord_table"),
      ),

      column(
        width = 8,

        plotOutput("map_plot", height = 520),

        h4("ggplot code"),
        uiOutput("code_box")
      )
    )
  )

  # ------------------------------------------------------------------------------
  # Server
  # ------------------------------------------------------------------------------
  server <- function(input, output, session) {

    # Apply preset to sliders and projection
    observeEvent(input$preset, {
      p <- presets[[input$preset]]

      updateSliderInput(session, "west",  value = p$west)
      updateSliderInput(session, "east",  value = p$east)
      updateSliderInput(session, "south", value = p$south)
      updateSliderInput(session, "north", value = p$north)
      updateSelectInput(session, "projection", selected = p$projection)
    }, ignoreInit = FALSE)

    # Ordered extent used for plotting/code generation
    plot_bbox <- reactive({
      # Reorder bounds for plotting and code generation
      # Since you can enter bogus values via the sliders (e.g. a southern border more
      # northerly than the northern one) we reorder the values here.
      list(
        west  = min(input$west, input$east),
        east  = max(input$west, input$east),
        south = min(input$south, input$north),
        north = max(input$south, input$north)
      )
    })

    # Coordinate readout table
    output$coord_table <- renderTable({
      b <- plot_bbox()
      data.frame(
        boundary = c("West", "East", "South", "North"),
        degrees_minutes_seconds = c(
          to_dms(b$west,  "lon"),
          to_dms(b$east,  "lon"),
          to_dms(b$south, "lat"),
          to_dms(b$north, "lat")
        ),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    # Load Natural Earth data only when detail changes
    world_data <- reactive({
      ne_countries(
        scale = as.integer(input$detail),
        returnclass = "sf"
      )
    })

    # Static map preview
    output$map_plot <- renderPlot({
      b <- plot_bbox()

      bbox_ll <- st_bbox(
        c(
          xmin = b$west,
          ymin = b$south,
          xmax = b$east,
          ymax = b$north
        ),
        crs = st_crs(4326)
      )

      bb <- st_transform(
        bbox_ll,
        crs = st_crs(input$projection),
        densify = 181
      )

      world_pr <- st_transform(world_data(), input$projection)

      ggplot() +
        geom_sf(data = world_pr) +
        coord_sf(
          xlim = c(bb["xmin"], bb["xmax"]),
          ylim = c(bb["ymin"], bb["ymax"]),
          expand = FALSE,
          crs = st_crs(input$projection)
        ) +
        theme_minimal(base_size = 12) +
        labs(
          title = "Map preview"
        )
    })

    # Paste-ready ggplot code
    output$code_box <- renderUI({
      b <- plot_bbox()

      code <- make_code(
        w = b$west,
        s = b$south,
        e = b$east,
        n = b$north,
        projection = input$projection,
        scale = input$detail
      )

      tags$textarea(
        style = "width: 100%; height: 340px; font-family: monospace;",
        readonly = NA,
        code
      )
    })
  }

  # ------------------------------------------------------------------------------
  # Run app
  # ------------------------------------------------------------------------------
  shinyApp(ui, server)
}
