# ============================================================
# APP INTEGRADA (MAPA + HEATMAP)
# - Se mantiene tu app original del mapa
# - Se agrega una NUEVA pestaña "Heatmap" con el archivo appHeatmap.R integrado como módulo
# ============================================================

library(shiny)
library(sf)
library(dplyr)
library(stringr)
library(readxl)
library(leaflet)
library(scales)
library(DT)
library(openxlsx)
library(htmlwidgets)
library(webshot2)
library(htmltools)
library(tidyr)
library(plotly)
library(paletteer)

options(shiny.sanitize.errors = FALSE)

# ------------------- Helpers -------------------
`%||%` <- function(a, b) if (is.null(a)) b else a

empty_like <- function(cols){
  out <- as.list(rep(NA, length(cols))); names(out) <- cols
  dplyr::bind_rows(out)[0, ]
}

safe_rescale01 <- function(x){
  x <- as.numeric(x)
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) {
    return(ifelse(is.na(x), NA_real_, 0))
  }
  scales::rescale(x, to = c(0,1), from = rng)
}

# ==========================================================
# ------------------- LOAD GEO (MAPA) -----------------------
# ==========================================================
capa1 <- st_read("LÍMITES MUNICIPALES.shp", quiet = TRUE) |>
  st_make_valid() |>
  st_transform(4326)

Capa_dep <- readRDS("DeptoGeom.rds") |>
  st_make_valid() |>
  st_transform(4326)

geom_by_cod <- capa1 |>
  transmute(COD = as.character(NA3), geometry)

geom_by_cod_simpl <- geom_by_cod |>
  st_simplify(dTolerance = 30) |>
  st_make_valid()

frontera_es <- Capa_dep |>
  summarise(geometry = st_union(geometry)) |>
  st_boundary() |>
  st_as_sf()

bbox_es <- sf::st_bbox(Capa_dep)
padx_es <- 0.01
pady_es <- 0.01
xmin_es <- unname(bbox_es["xmin"]) - padx_es
ymin_es <- unname(bbox_es["ymin"]) - pady_es
xmax_es <- unname(bbox_es["xmax"]) + padx_es
ymax_es <- unname(bbox_es["ymax"]) + pady_es

# ==========================================================
# ------------------- DATA (MAPA) ---------------------------
# ==========================================================
Sectores_Empresas_raw <- read_excel(
  "SOLVENCIAS 2022 DEPTMUN.xlsx",
  sheet = "Directorio Registro Sol", skip = 4
) %>%
  select(3, 5:7, 10, 12) %>%
  rename(
    Departamento     = 2,
    Municipio        = 3,
    Distrito         = 4,
    COD              = 6,
    Sectores         = 5,
    NombreComercial  = 1
  ) %>%
  mutate(
    COD = as.character(COD),
    Departamento = as.character(Departamento),
    Municipio = as.character(Municipio),
    Distrito = as.character(Distrito),
    Sectores = as.character(Sectores),
    NombreComercial = as.character(NombreComercial)
  )

Sectores_Empresas <- Sectores_Empresas_raw %>%
  group_by(Departamento, Municipio, Distrito, COD, Sectores) %>%
  summarise(Valor = n_distinct(NombreComercial), .groups="drop") %>%
  mutate(Año = 2025L, Categoria = "Empresas operando") %>%
  select(Departamento, Municipio, Distrito, COD, Sectores, Valor, Año, Categoria)

Inversiones_raw <- read_excel("Matriz de Inversiones.xlsx", skip = 2) %>%
  mutate(Año = str_sub(Fecha, 1L, 4L)) %>%
  rename(
    Inversiones = "Monto mill",
    Empresa     = "Empresa",
    Sector      = "Sector"
  ) %>%
  mutate(
    Año = as.integer(Año),
    COD = as.character(COD),
    Departamento = as.character(Departamento),
    Municipio = as.character(Municipio),
    Distrito = as.character(Distrito),
    Sectores = as.character(Sector),
    Empresa  = as.character(Empresa)
  )

Matriz <- Inversiones_raw %>%
  group_by(Año, Sectores, Departamento, COD, Municipio, Distrito) %>%
  summarise(
    `Inversiones (millones de USD)` = sum(Inversiones, na.rm = TRUE),
    Empleos = sum(Empleo, na.rm = TRUE),
    Proyectos = n_distinct(Descripción, na.rm = TRUE),
    `Empresas invirtiendo` = n_distinct(Empresa, na.rm = TRUE),
    .groups="drop"
  ) %>%
  pivot_longer(
    cols = -c(Año, Sectores, Departamento, Municipio, COD, Distrito),
    names_to = "Categoria",
    values_to = "Valor"
  ) %>%
  select(Departamento, Municipio, Distrito, COD, Sectores, Valor, Año, Categoria) %>%
  bind_rows(Sectores_Empresas) %>%
  left_join(capa1, by = c("COD" = "NA3")) %>%
  st_as_sf() %>%
  st_make_valid() %>%
  st_transform(4326)

# ==========================================================
# ------------------- PALETAS (MAPA) ------------------------
#   + Se agrega ggthemes::Blue (paletteer)
# ==========================================================
custom_palettes <- list(
  "Standar"= c("#D4D4D4", "#49525E", "#111E60")
 # "Blue 1" = c("#9EC9D9", "#1F74B1", "#26456E"),
 # "Blue 2" = c("#A0C4F1", "#0067A7", "#00467D"),
 #"Black diverging" = c("#6F6F6F", "#434343", "#1E1E1E"),
 # "Reds"   = c("#DE9FD2", "#AB3F6A", "#7D0112")
)

make_custom_palette <- function(name, n, reverse = FALSE){
  cols <- NULL
  
  if (identical(name, "ggthemes::Blue")) {
    cols <- as.character(paletteer::paletteer_c("ggthemes::Blue", n))
  } else {
    anchors <- custom_palettes[[name]]
    cols <- grDevices::colorRampPalette(anchors)(n)
  }
  
  if (reverse) cols <- rev(cols)
  cols
}

# ==========================================================
# ------------------- DATA (HEATMAP) ------------------------
#   (Asegúrate de tener este Excel en el working directory)
# ==========================================================
turismo <- read_excel(
  "Necesidades_turismo_normalizado.xlsx",
  sheet = "Turismo_Normalizado"
) %>%
  mutate(
    Sectores     = as.character(Sectores),
    Departamento = as.character(Departamento),
    Categoria    = as.character(Categoria),
    Item         = as.character(Item),
    Orden        = suppressWarnings(as.integer(Orden))
  ) %>%
  filter(
    !is.na(Item), str_trim(Item) != "",
    !is.na(Categoria), str_trim(Categoria) != "",
    !is.na(Departamento), str_trim(Departamento) != ""
  )

# ==========================================================
# ------------------- HEATMAP MODULE ------------------------
# ==========================================================
heatmap_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(tags$style(HTML("
      .panel { background:#fff; border:1px solid #e6e6e6; border-radius:10px; padding:12px; }
      .filters { display:flex; gap:12px; flex-wrap:wrap; align-items:flex-end; }
      .filters .form-group { margin-bottom:0 !important; }
      .filters .shiny-input-container { width: 260px; }
      .btnrow { display:flex; gap:10px; flex-wrap:wrap; margin-top:10px; }
    "))),
    div(class = "panel",
        div(class = "filters",
            selectInput(ns("cat"), "Categoría", choices = NULL, multiple = TRUE),
            selectInput(ns("dep"), "Departamento", choices = NULL, multiple = TRUE) ,
            #sliderInput(ns("top_items"), "Máx. Items", min = 20, max = 300, value = 80, step = 10),
            checkboxInput(ns("orden_por_frecuencia"), "Ordenar Items por frecuencia", TRUE) #,
            #checkboxInput(ns("escala_log"), "Escala log (log10(1+n))", FALSE)
        )
    ),
    br(),
    div(class="panel",
        plotlyOutput(ns("heatmap"), height = "650px"),
        div(class="btnrow",
            downloadButton(ns("dl_excel"), "Descargar datos (Excel)"),
            downloadButton(ns("dl_heat_html"), "Descargar heatmap (HTML)")
        )
    ),
    br(),
    div(class="panel",
        DTOutput(ns("tabla"))
    )
  )
}

heatmap_server <- function(id, turismo_df){
  moduleServer(id, function(input, output, session){
    
    observe({
      deps <- sort(unique(turismo_df$Departamento))
      cats <- sort(unique(turismo_df$Categoria))
      
      updateSelectInput(session, "dep",
                        choices = deps,
                        selected = intersect(input$dep %||% character(0), deps))
      updateSelectInput(session, "cat",
                        choices = cats,
                        selected = intersect(input$cat %||% character(0), cats))
    })
    
    base_filtrada <- reactive({
      df <- turismo_df
      if (length(input$dep) > 0) df <- df %>% filter(Departamento %in% input$dep)
      if (length(input$cat) > 0) df <- df %>% filter(Categoria %in% input$cat)
      df
    })
    
    # ---- Preparación robusta: construir matriz z ----
    heat_data <- reactive({
      df <- base_filtrada()
      validate(need(nrow(df) > 0, "Sin registros para los filtros seleccionados."))
      
      ctab <- df %>% count(Categoria, Item, name = "n")
      
      top_items_tbl <- ctab %>%
        group_by(Item) %>%
        summarise(total = sum(n), .groups = "drop") %>%
        arrange(desc(total), Item)
      
      n_keep <- min(input$top_items, nrow(top_items_tbl))
      items_keep <- top_items_tbl %>% slice_head(n = n_keep) %>% pull(Item)
      
      ctab <- ctab %>% filter(Item %in% items_keep)
      
      cat_levels <- sort(unique(ctab$Categoria))
      item_levels <- if (isTRUE(input$orden_por_frecuencia)) {
        top_items_tbl %>%
          filter(Item %in% items_keep) %>%
          arrange((total), Item) %>%
          pull(Item)
      } else {
        sort(items_keep)
      }
      
      mat <- ctab %>%
        mutate(
          Categoria = factor(Categoria, levels = cat_levels),
          Item      = factor(Item, levels = item_levels)
        ) %>%
        tidyr::complete(Categoria, Item, fill = list(n = 0)) %>%
        arrange(Categoria, Item)
      
      z <- matrix(mat$n, nrow = length(cat_levels), byrow = FALSE)
      
      if (isTRUE(input$escala_log)) {
        z <- log10(1 + z)
        ztitle <- "log10(1+n)"
      } else {
        ztitle <- "n"
      }
      
      list(
        z = t(z),              # <-- TRANSPOSE
        x = cat_levels,        # <-- Categoria ahora en X
        y = item_levels,       # <-- Item ahora en Y
        ztitle = ztitle,
        zmax = max(z, na.rm = TRUE)
      )
      
    })
    
    rv <- reactiveValues(last_plot = NULL)
    
    output$heatmap <- renderPlotly({
      hd <- heat_data()  # ya valida si no hay datos
      
      pal_blue <- grDevices::colorRampPalette(
        c("#E3F2FD", "#90CAF9", "#1E88E5", "#0D47A1")
      )(30)
      cs_blue  <- Map(function(i, col) list(i, col),
                      seq(0, 1, length.out = length(pal_blue)),
                      pal_blue)
      
      p <- plot_ly(
        x = hd$x,
        y = hd$y,
        z = hd$z,
        type = "heatmap",
        colorscale = cs_blue,
        zmin = 0,
        zmax = hd$zmax,
        hovertemplate = "<b>Categoría:</b> %{x}<br><b>Habilidad:</b> %{y}<br><b>Departamentos:</b> %{z}<extra></extra>"
      ) %>%
        layout(
          title = list(
            text = paste0(
              "<b>Mapa de habilidades requeridas</b><br>",
              "<span style='font-size:13px;color:#666;'>",
              "(Intensidad del color indica mayor demanda en Departamentos)",
              "</span>"
            ),
            x = 0.5,
            xanchor = "center",
            yanchor = "top"
          ),
          xaxis = list(
            title = "",
            tickfont = list(size = 12),  # ⬅️ tamaño etiquetas eje X
            tickangle = -45
          ),
          
          # -------- EJE Y (Items) --------
          yaxis = list(
            title = "",
            tickfont = list(size = 13)   # ⬅️ tamaño etiquetas eje Y
          ),
          margin = list(l = 200, r = 40, t = 80, b = 40)
        )
      
      rv$last_plot <- p
      p
    })
    
    output$tabla <- renderDT({
      df <- base_filtrada() %>%
        arrange(Departamento, Categoria, Orden, Item, Sectores)
      
      datatable(
        df,
        rownames = FALSE,
        options = list(
          pageLength = 12,
          scrollX = FALSE,
          language = list(
            emptyTable = "Sin registros para los filtros seleccionados",
            zeroRecords = "Sin registros para los filtros seleccionados"
          )
        )
      )
    })
    
    output$dl_excel <- downloadHandler(
      filename = function() paste0("Turismo_heatmap_filtrado_", Sys.Date(), ".xlsx"),
      content  = function(file){
        df <- base_filtrada() %>% arrange(Departamento, Categoria, Orden, Item, Sectores)
        
        wb <- createWorkbook()
        addWorksheet(wb, "Filtrado")
        writeData(wb, "Filtrado", df)
        
        addWorksheet(wb, "Heatmap_counts")
        counts <- df %>% count(Categoria, Item, name = "n") %>% arrange(desc(n))
        writeData(wb, "Heatmap_counts", counts)
        
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    output$dl_heat_html <- downloadHandler(
      filename = function() paste0("heatmap_", Sys.Date(), ".html"),
      content = function(file){
        req(rv$last_plot)
        htmlwidgets::saveWidget(rv$last_plot, file, selfcontained = TRUE)
      }
    )
  })
}

# ==========================================================
# ------------------- UI (TABs) -----------------------------
# ==========================================================
ui <- fluidPage(
  tags$head(tags$style(HTML("
    .leaflet .legend { line-height: 18px; }
    .value-card{
      padding:12px;border-radius:12px;box-shadow:0 6px 16px rgba(0,0,0,0.08);
      margin-bottom:10px;border:1px solid #eee;background:#fff;width:100%;
      display:flex;flex-direction:column;justify-content:center;min-height:72px;
    }
    .value-title{font-size:14px;color:#666;margin-bottom:4px;}
    .value-number{font-size:22px;font-weight:700;}
    .kpi-row{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:12px;margin-bottom:8px;}
    @media (max-width:1100px){.kpi-row{grid-template-columns:repeat(2,1fr);} }
    @media (max-width:576px){ .kpi-row{grid-template-columns:1fr;} }
  "))),
  # Encabezado con logo (igual que tu otro ejemplo)
  tags$div(
    style = "display:flex;align-items:center;justify-content:space-between;margin-bottom:10px;width:100%;",
    tags$img(src = "LogoMINEC.png", height = "60px", style = "margin-left:10px;"),
    tags$div(style = "flex:1; text-align:center;",
             tags$h2("Mapa de Actividad Empresarial e Inversión para la Planificación Educativa", style="margin:0;font-weight:700;")
    ),
    tags$div(style="width:80px;")
  ),
  
  tabsetPanel(
    tabPanel("Mapa",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 h4("Filtros"),
                 hr(),
                 radioButtons("nivel", "Nivel geográfico",
                              choices = c("Distritos","Departamentos"),
                              selected = "Distritos", inline = TRUE),
                 
                # selectInput("anio", "Año", choices = NULL, multiple = TRUE),
                 selectInput("dep", "Departamento", choices = NULL, multiple = TRUE),
                # selectInput("mun", "Municipio", choices = NULL, multiple = TRUE),
                 selectInput("sector", "Sectores", choices = NULL, multiple = TRUE),
                 
                 hr(),
                 h4("Intensidad del color en el mapa:"),
                 sliderInput(
                   "mix_color",
                   "Empresas operando ↔ Inversión",
                   min = 0, max = 100, value = 0, step = 5
                 ),
                 helpText("0 = solo Empresas operando; 100 = solo Inversión; valores intermedios = combinación ponderada."),
                 
                 selectInput("paleta", "Paleta de colores",
                             choices = c(names(custom_palettes), "ggthemes::Blue"),
                             selected = "Standar"),
                 checkboxInput("paleta_rev", "Invertir paleta", FALSE),
                 checkboxInput("mostrar_etiquetas", "Mostrar etiquetas en el mapa", value = TRUE),
                 
                 hr(),
                 #h4("Empresas por sector (operando vs invirtiendo)"),
                 #plotlyOutput("bar_empresas_sector", height = "280px"),
                 helpText("Fuente: DIPE-MINEC con datos de ONEC y medios de prensa"),
                 
                 hr(),
                 downloadButton("descargar_excel", "Descargar datos (Excel)"),
                 downloadButton("descargar_mapa", "Descargar mapa (PNG)")
               ),
               mainPanel(
                 div(class = "kpi-row",
                     div(class="value-card", style="background-color:#EAEDF5; color:black;",
                         div(class="value-title","Empresas operando (2022)"),
                         div(class="value-number", textOutput("kpi_emp_operando"))),
                     div(class="value-card", style="background-color:#EAEDF5; color:black;",
                         div(class="value-title","Inversión anunciada (millones USD, 2023-2026)"),
                         div(class="value-number", textOutput("kpi_inversion"))),
                     div(class="value-card", style="background-color:#EAEDF5; color:black;",
                         div(class="value-title","Empresas con anuncio de inversión (2023-2026)"),
                         div(class="value-number", textOutput("kpi_emp_invirtiendo")))
                 ),
                 leafletOutput("mapa", height = "650px", width="100%"),
                 br(),
                 h4("Detalle (filtrado)"),
                 DTOutput("tabla_detalle", width="100%")
               )
             )
    ),
    
    tabPanel("Heatmap",
             tags$div(
               style = "padding:12px 6px 6px 6px;",
               tags$h3("Habilidades demandadas en sector Turismo",
                       style = "margin:0; font-weight:700; text-align:center;")
             ),
             heatmap_ui("hm")
    )
    
    
  )
)

# ==========================================================
# ------------------- SERVER -------------------------------
# ==========================================================
server <- function(input, output, session){
  
  # Activar módulo heatmap
  heatmap_server("hm", turismo_df = turismo)
  
  rv <- reactiveValues(export_map = NULL, bnds = NULL, ctr = NULL, zm = NULL)
  observeEvent(input$mapa_bounds, ignoreInit = TRUE, { rv$bnds <- input$mapa_bounds })
  observeEvent(input$mapa_center, ignoreInit = TRUE, { rv$ctr  <- input$mapa_center })
  observeEvent(input$mapa_zoom,   ignoreInit = TRUE, { rv$zm   <- input$mapa_zoom })
  
  # Inicializar choices
  observe({
    updateSelectInput(session, "anio",
                      choices = sort(unique(Matriz$Año)),
                      selected = sort(unique(Matriz$Año)))
    updateSelectInput(session, "dep",
                      choices = sort(unique(Matriz$Departamento)))
    updateSelectInput(session, "sector",
                      choices = sort(unique(Matriz$Sectores)))
    updateSelectInput(session, "mun",
                      choices = sort(unique(Matriz$Municipio)))
  })
  
  # Municipios dependientes de Departamentos
  observeEvent(input$dep, {
    df_tmp <- Matriz
    if (length(input$dep) > 0) df_tmp <- df_tmp %>% filter(Departamento %in% input$dep)
    mun_choices <- sort(unique(df_tmp$Municipio))
    updateSelectInput(session, "mun",
                      choices = mun_choices,
                      selected = intersect(input$mun %||% character(0), mun_choices))
  }, ignoreInit = TRUE)
  
  # ------------------- Base filtrada (sf consolidada) -------------------
  base_filtrada <- reactive({
    df <- Matriz
    if (length(input$anio) > 0)   df <- df %>% filter(Año %in% input$anio)
    if (length(input$dep) > 0)    df <- df %>% filter(Departamento %in% input$dep)
    if (length(input$mun) > 0)    df <- df %>% filter(Municipio %in% input$mun)
    if (length(input$sector) > 0) df <- df %>% filter(Sectores %in% input$sector)
    df
  })
  
  # ------------------- KPIs -------------------
  output$kpi_emp_operando <- renderText({
    df <- base_filtrada()
    number(sum(df$Valor[df$Categoria == "Empresas operando"], na.rm = TRUE),
           big.mark=",", accuracy=1)
  })
  
  output$kpi_inversion <- renderText({
    df <- base_filtrada()
    number(sum(df$Valor[df$Categoria == "Inversiones (millones de USD)"], na.rm = TRUE),
           big.mark=",", accuracy=1)
  })
  
  output$kpi_emp_invirtiendo <- renderText({
    df <- base_filtrada()
    number(sum(df$Valor[df$Categoria == "Empresas invirtiendo"], na.rm = TRUE),
           big.mark=",", accuracy=1)
  })
  
  # ------------------- Datos para gráfico (burbujas) -------------------
  datos_bar_sector <- reactive({
    emp_raw <- Sectores_Empresas_raw
    if (length(input$dep) > 0)    emp_raw <- emp_raw %>% filter(Departamento %in% input$dep)
    if (length(input$mun) > 0)    emp_raw <- emp_raw %>% filter(Municipio %in% input$mun)
    if (length(input$sector) > 0) emp_raw <- emp_raw %>% filter(Sectores %in% input$sector)
    
    emp_sector <- emp_raw %>%
      group_by(Sectores) %>%
      summarise(Empresas_operando = n_distinct(NombreComercial), .groups="drop")
    
    inv_raw <- Inversiones_raw
    if (length(input$anio) > 0)   inv_raw <- inv_raw %>% filter(Año %in% input$anio)
    if (length(input$dep) > 0)    inv_raw <- inv_raw %>% filter(Departamento %in% input$dep)
    if (length(input$mun) > 0)    inv_raw <- inv_raw %>% filter(Municipio %in% input$mun)
    if (length(input$sector) > 0) inv_raw <- inv_raw %>% filter(Sectores %in% input$sector)
    
    inv_sector <- inv_raw %>%
      group_by(Sectores) %>%
      summarise(Empresas_invirtiendo = n_distinct(Empresa), .groups="drop")
    
    out <- full_join(emp_sector, inv_sector, by = "Sectores") %>%
      replace_na(list(Empresas_operando = 0, Empresas_invirtiendo = 0)) %>%
      mutate(Total = Empresas_operando + Empresas_invirtiendo) %>%
      arrange(desc(Total))
    
    out %>% slice_head(n = 5)
  })
  


  
  # ------------------- Agregado para mapa + TOPs -------------------
  datos_mapa <- reactive({
    df <- base_filtrada()
    if (nrow(df) == 0) return(NULL)
    
    key <- if (input$nivel == "Distritos") "COD" else "Departamento"
    
    resumen <- df %>%
      st_drop_geometry() %>%
      group_by(across(all_of(key))) %>%
      summarise(
        Empresas_operando    = sum(Valor[Categoria == "Empresas operando"], na.rm = TRUE),
        Empresas_invirtiendo = sum(Valor[Categoria == "Empresas invirtiendo"], na.rm = TRUE),
        Empleos              = sum(Valor[Categoria == "Empleos"], na.rm = TRUE),
        Proyectos            = sum(Valor[Categoria == "Proyectos"], na.rm = TRUE),
        Inversiones          = sum(Valor[Categoria == "Inversiones (millones de USD)"], na.rm = TRUE),
        .groups = "drop"
      )
    
    top_sectores <- df %>%
      st_drop_geometry() %>%
      filter(Categoria == "Inversiones (millones de USD)") %>%
      group_by(across(all_of(key)), Sectores) %>%
      summarise(Monto = sum(Valor, na.rm = TRUE), .groups="drop") %>%
      group_by(across(all_of(key))) %>%
      arrange(desc(Monto), .by_group = TRUE) %>%
      slice_head(n = 3) %>%
      summarise(
        TopSectores = paste0(Sectores, " ($", comma(Monto, accuracy = 0.01), " M)") %>% paste(collapse = "<br/>"),
        .groups="drop"
      )
    
    top_sectores_empresas <- df %>%
      st_drop_geometry() %>%
      filter(Categoria == "Empresas operando") %>%
      group_by(across(all_of(key)), Sectores) %>%
      summarise(Emp = sum(Valor, na.rm = TRUE), .groups = "drop") %>%
      group_by(across(all_of(key))) %>%
      mutate(P = Emp / sum(Emp, na.rm = TRUE)) %>%
      arrange(desc(Emp), .by_group = TRUE) %>%
      slice_head(n = 3) %>%
      summarise(
        TopSectoresEmp = paste0("• ", Sectores, " (", percent(P, accuracy = 0.1), ")") %>% paste(collapse = "<br/>"),
        .groups = "drop"
      )
    
    out <- resumen %>%
      left_join(top_sectores_empresas, by = key) %>%
      left_join(top_sectores,          by = key)
    
    if (input$nivel == "Distritos") {
      out <- out %>%
        mutate(COD = as.character(COD)) %>%
        left_join(geom_by_cod_simpl, by = "COD") %>%
        st_as_sf() %>%
        left_join(
          Matriz %>% st_drop_geometry() %>% distinct(COD, Departamento, Municipio, Distrito),
          by = "COD"
        )
    } else {
      out <- Capa_dep %>%
        left_join(out, by = "Departamento") %>%
        st_as_sf()
    }
    
    out %>% filter(!st_is_empty(geometry))
  })
  
  # ------------------- Mapa base -------------------
  output$mapa <- renderLeaflet({
    leaflet(options = leafletOptions(
      preferCanvas = TRUE,
      zoomControl = FALSE,
      updateWhenZooming = FALSE,
      updateWhenIdle = TRUE,
      minZoom = 8,
      maxZoom = 14,
      worldCopyJump = FALSE,
      maxBoundsViscosity = 1.0
    )) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(lng = -88.9, lat = 13.75, zoom = 9) %>%
      setMaxBounds(xmin_es, ymin_es, xmax_es, ymax_es)
  })
  
  observeEvent(
    list(datos_mapa(), input$paleta, input$paleta_rev, input$mostrar_etiquetas, input$nivel, input$mix_color),
    ignoreInit = FALSE, {
      
      df <- datos_mapa()
      
      if (is.null(df) || nrow(df) == 0) {
        leafletProxy("mapa") %>%
          clearShapes() %>% clearControls() %>%
          addPolylines(data = frontera_es, color = "#49525E", weight = 1, opacity = 1) %>%
          addControl(
            html = "<div style='padding:6px 8px;background:#fff;border-radius:8px;box-shadow:0 2px 8px rgba(0,0,0,.15);font-weight:600'>
                    Sin registros para los filtros seleccionados</div>",
            position = "topright"
          )
        rv$export_map <- leaflet() %>%
          addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
          addPolylines(data = frontera_es, color = "#49525E", weight = 1, opacity = 1)
        return(invisible(NULL))
      }
      
      w_inv <- input$mix_color / 100
      w_emp <- 1 - w_inv
      
      emp01 <- safe_rescale01(df$Empresas_operando)
      inv01 <- safe_rescale01(df$Inversiones)
      color_value <- w_emp * emp01 + w_inv * inv01
      
      N_CLASSES <- 6
      pal_vec <- make_custom_palette(input$paleta, N_CLASSES, input$paleta_rev)
      pal <- colorBin(palette = pal_vec, domain = color_value, bins = N_CLASSES, na.color = "#FFFFFF", pretty = TRUE)
      col_vec <- pal(color_value)
      
      titulo_legend <- if (input$mix_color == 0) {
        "Empresas operando"
      } else if (input$mix_color == 100) {
        "Inversión (M USD)"
      } else {
        paste0("Mix (", 100 - input$mix_color, "% Emp + ", input$mix_color, "% Inv)")
      }
      
      # ==============================
      # ETIQUETA: + Dep/Mun/Dist arriba
      #          - Se elimina "Top 3 empresas"
      # ==============================
      if (input$nivel == "Distritos") {
        etiquetas <- sprintf(
          "<strong>UBICACIÓN</strong><br/>
           <strong>Departamento:</strong> %s<br/>
           <strong>Municipio:</strong> %s<br/>
           <strong>Distrito:</strong> %s<br/><br/>

           <strong>EMPRESAS</strong><br/>
           <strong>Empresas operando:</strong> %s<br/><br/>

           <strong>Top 3 sectores:</strong><br/>%s<br/><br/>

           <strong>INVERSIONES ANUNCIADAS</strong><br/>
           <strong>Monto anunciado:</strong> $%s M<br/>
           <strong>Empleos previstos:</strong> %s<br/>
           <strong>Proyectos:</strong> %s<br/><br/>

           <strong>Sectores con mayor inversión anunciada:</strong><br/>%s",
          df$Departamento %||% "-",
          df$Municipio %||% "-",
          df$Distrito %||% "-",
          comma(df$Empresas_operando %||% 0),
          df$TopSectoresEmp %||% "-",
          comma(df$Inversiones %||% 0, accuracy = 0.01),
          comma(df$Empleos %||% 0),
          comma(df$Proyectos %||% 0),
          df$TopSectores %||% "-"
        ) %>% lapply(htmltools::HTML)
      } else {
        etiquetas <- sprintf(
          "<strong>UBICACIÓN</strong><br/>
           <strong>Departamento:</strong> %s<br/><br/>

           <strong>EMPRESAS</strong><br/>
           <strong>Empresas operando:</strong> %s<br/><br/>

           <strong>Top 3 sectores:</strong><br/>%s<br/><br/>

           <strong>INVERSIONES ANUNCIADAS</strong><br/>
           <strong>Monto anunciado:</strong> $%s M<br/>
           <strong>Empleos previstos:</strong> %s<br/>
           <strong>Proyectos:</strong> %s<br/><br/>

           <strong>Sectores con mayor inversión anunciada:</strong><br/>%s",
          df$Departamento %||% "-",
          comma(df$Empresas_operando %||% 0),
          df$TopSectoresEmp %||% "-",
          comma(df$Inversiones %||% 0, accuracy = 0.01),
          comma(df$Empleos %||% 0),
          comma(df$Proyectos %||% 0),
          df$TopSectores %||% "-"
        ) %>% lapply(htmltools::HTML)
      }
      
      leafletProxy("mapa") %>%
        clearShapes() %>% clearControls() %>%
        addPolygons(
          data = df,
          stroke = TRUE, weight = 1, color = "#666",
          fillOpacity = 0.8, fillColor = col_vec,
          smoothFactor = 0.3,
          label = if (isTRUE(input$mostrar_etiquetas)) etiquetas else NULL,
          highlightOptions = highlightOptions(weight = 2, color = "#49525E", bringToFront = TRUE)
        ) %>%
        addPolylines(data = frontera_es, color = "#49525E", weight = 1, opacity = 1) %>%
        addLegend(position = "bottomleft", pal = pal, values = color_value,
                  title = titulo_legend, opacity = 0.9)
      
      rv$export_map <- leaflet(options = leafletOptions(preferCanvas = TRUE, zoomControl = TRUE)) %>%
        addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
        addPolygons(
          data = df,
          stroke = TRUE, weight = 1, color = "#666",
          fillOpacity = 0.8, fillColor = col_vec,
          smoothFactor = 0.3
        ) %>%
        addPolylines(data = frontera_es, color = "#49525E", weight = 1, opacity = 1) %>%
        addLegend(position = "bottomleft", pal = pal, values = color_value,
                  title = titulo_legend, opacity = 0.9)
    }
  )
  
  # ------------------- Tabla detalle -------------------
  output$tabla_detalle <- renderDT({
    df <- base_filtrada() %>% st_drop_geometry()
    if (nrow(df) == 0) df <- empty_like(c("Departamento","Municipio","Distrito","COD","Sectores","Año","Categoria","Valor"))
    
    datatable(
      df %>% select(Departamento, Municipio, Distrito, COD, Sectores, Año, Categoria, Valor),
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        language = list(
          emptyTable = "Sin registros para los filtros seleccionados",
          zeroRecords = "Sin registros para los filtros seleccionados"
        )
      )
    )
  })
  
  # ------------------- Descargar Excel (Resumen + Detalle) -------------------
  output$descargar_excel <- downloadHandler(
    filename = function() paste0("Mapa_Integrado_", Sys.Date(), ".xlsx"),
    content = function(file) {
      df_detalle <- base_filtrada() %>% st_drop_geometry()
      df_resumen <- datos_mapa()
      if (inherits(df_resumen, "sf")) df_resumen <- st_drop_geometry(df_resumen)
      
      wb <- createWorkbook()
      addWorksheet(wb, "Resumen (Mapa)")
      writeData(wb, "Resumen (Mapa)", df_resumen)
      addWorksheet(wb, "Detalle (Filtrado)")
      writeData(wb, "Detalle (Filtrado)", df_detalle)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # ------------------- Descargar mapa PNG -------------------
  output$descargar_mapa <- downloadHandler(
    filename = function() paste0("Mapa_Integrado_", Sys.Date(), ".png"),
    content = function(file) {
      req(rv$export_map)
      map_to_save <- rv$export_map
      
      if (!is.null(rv$bnds)) {
        b <- rv$bnds
        map_to_save <- map_to_save %>%
          leaflet::fitBounds(lng1 = b$west, lat1 = b$south, lng2 = b$east, lat2 = b$north)
      } else if (!is.null(rv$ctr) && !is.null(rv$zm)) {
        map_to_save <- map_to_save %>%
          leaflet::setView(lng = rv$ctr$lng, lat = rv$ctr$lat, zoom = rv$zm)
      } else {
        map_to_save <- map_to_save %>% leaflet::fitBounds(xmin_es, ymin_es, xmax_es, ymax_es)
      }
      
      tmp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(map_to_save, tmp_html, selfcontained = FALSE)
      
      webshot2::webshot(
        url     = tmp_html,
        file    = file,
        vwidth  = 1200,
        vheight = 800,
        zoom    = 5,
        cliprect = "viewport",
        delay   = 1
      )
    }
  )
}

shinyApp(ui, server)