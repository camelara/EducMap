# app.R
library(shiny)
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(plotly)
library(DT)
library(openxlsx)
library(paletteer)
library(htmlwidgets)

options(shiny.sanitize.errors = FALSE)
`%||%` <- function(a, b) if (is.null(a)) b else a

# ------------------- LOAD DATA -------------------
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

# ------------------- UI -------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: Arial, sans-serif; }
    .panel { background:#fff; border:1px solid #e6e6e6; border-radius:10px; padding:12px; }
    .filters { display:flex; gap:12px; flex-wrap:wrap; align-items:flex-end; }
    .filters .form-group { margin-bottom:0 !important; }
    .filters .shiny-input-container { width: 260px; }
    .btnrow { display:flex; gap:10px; flex-wrap:wrap; margin-top:10px; }
  "))),
  
  # ---- Filtros arriba del gráfico ----
  div(class = "panel",
      div(class = "filters",
          selectInput("cat", "Categoría",
                      choices = sort(unique(turismo$Categoria)),
                      multiple = TRUE),
          selectInput("dep", "Departamento",
                      choices = sort(unique(turismo$Departamento)),
                      multiple = TRUE),
          
          # (Opcional) mantener por rendimiento: top N items
          sliderInput("top_items", "Máx. Items",
                      min = 20, max = 300, value = 80, step = 10),
          
          checkboxInput("orden_por_frecuencia", "Ordenar Items por frecuencia", TRUE),
          checkboxInput("escala_log", "Escala log (log10(1+n))", FALSE)
      )
  ),
  
  br(),
  
  # ---- Heatmap ----
  div(class="panel",
      plotlyOutput("heatmap", height = "650px"),
      
      div(class="btnrow",
          downloadButton("dl_excel", "Descargar datos (Excel)"),
          downloadButton("dl_heat_html", "Descargar heatmap (HTML)")
      )
  ),
  
  br(),
  
  # ---- Tabla abajo ----
  div(class="panel",
      DTOutput("tabla")
  )
)

# ------------------- SERVER -------------------
server <- function(input, output, session){
  
  # Inicializar choices (y mantener selección válida)
  observe({
    deps <- sort(unique(turismo$Departamento))
    cats <- sort(unique(turismo$Categoria))
    
    updateSelectInput(session, "dep",
                      choices = deps,
                      selected = intersect(input$dep %||% character(0), deps))
    updateSelectInput(session, "cat",
                      choices = cats,
                      selected = intersect(input$cat %||% character(0), cats))
  })
  
  # Base filtrada (para tabla)
  base_filtrada <- reactive({
    df <- turismo
    if (length(input$dep) > 0) df <- df %>% filter(Departamento %in% input$dep)
    if (length(input$cat) > 0) df <- df %>% filter(Categoria %in% input$cat)
    df
  })
  
  # Heatmap data
  heat_data <- reactive({
    df <- base_filtrada()
    if (nrow(df) == 0) return(NULL)
    
    ctab <- df %>% count(Categoria, Item, name = "n")
    
    top_items <- ctab %>%
      group_by(Item) %>%
      summarise(total = sum(n), .groups = "drop")
    
    # Selección de items (top N)
    items_keep <- if (isTRUE(input$orden_por_frecuencia)) {
      top_items %>%
        arrange(desc(total), Item) %>%
        slice_head(n = input$top_items) %>%
        pull(Item)
    } else {
      top_items %>%
        arrange(Item) %>%
        slice_head(n = input$top_items) %>%
        pull(Item)
    }
    
    ctab <- ctab %>% filter(Item %in% items_keep)
    
    # niveles ejes
    item_levels <- if (isTRUE(input$orden_por_frecuencia)) {
      top_items %>%
        filter(Item %in% items_keep) %>%
        arrange(desc(total), Item) %>%
        pull(Item)
    } else {
      sort(items_keep)
    }
    cat_levels <- sort(unique(ctab$Categoria))
    
    mat <- ctab %>%
      mutate(
        Categoria = factor(Categoria, levels = cat_levels),
        Item      = factor(Item, levels = item_levels)
      ) %>%
      complete(Categoria, Item, fill = list(n = 0)) %>%
      arrange(Categoria, Item)
    
    if (isTRUE(input$escala_log)) {
      mat <- mat %>% mutate(z = log10(1 + n))
      list(mat = mat, z_col = "z", ztitle = "log10(1+n)", zmax = max(mat$z, na.rm = TRUE))
    } else {
      list(mat = mat, z_col = "n", ztitle = "n", zmax = max(mat$n, na.rm = TRUE))
    }
  })
  
  # Guardar último plot para descargar HTML
  rv <- reactiveValues(last_plot = NULL)
  
  output$heatmap <- renderPlotly({
    hd <- heat_data()
    if (is.null(hd)) {
      p <- plotly_empty(type = "heatmap") %>%
        layout(title = list(text = "Sin registros para los filtros seleccionados"))
      rv$last_plot <- p
      return(p)
    }
    
    mat  <- hd$mat
    zcol <- hd$z_col
    
    pal_blue <- as.character(paletteer::paletteer_c("ggthemes::Blue", 30))
    cs_blue  <- Map(function(i, col) list(i, col),
                    seq(0, 1, length.out = length(pal_blue)),
                    pal_blue)
    
    z_vals <- mat[[zcol]]
    
    p <- plot_ly(
      data = mat,
      x = ~Item,
      y = ~Categoria,
      z = z_vals,
      type = "heatmap",
      colorscale = cs_blue,
      zmin = 0,
      zmax = hd$zmax,
      hovertemplate = paste(
        "<b>Tipo:</b> %{y}<br>",
        "<b>Habilidad:</b> %{x}<br>",
        if (zcol == "n") "<b>Conteo:</b> %{z}<extra></extra>"
        else "<b>Valor:</b> %{z:.3f}<extra></extra>"
      )
    ) %>%
      layout(
        # Quitar títulos de ejes / etiquetas
        xaxis = list(title = "", tickangle = -90, automargin = TRUE),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 170, r = 30, t = 10, b = 230),
        coloraxis = list(colorbar = list(title = ""))  # sin título en la barra
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
        scrollX = FALSE,   # <-- sin scroll bar horizontal
        language = list(
          emptyTable = "Sin registros para los filtros seleccionados",
          zeroRecords = "Sin registros para los filtros seleccionados"
        )
      )
    )
  })
  
  # -------- Descargar Excel (Filtrado + Conteos Heatmap) --------
  make_excel <- function(file){
    df <- base_filtrada() %>% arrange(Departamento, Categoria, Orden, Item, Sectores)
    
    wb <- createWorkbook()
    addWorksheet(wb, "Filtrado")
    writeData(wb, "Filtrado", df)
    
    addWorksheet(wb, "Heatmap_counts")
    counts <- df %>% count(Categoria, Item, name = "n") %>% arrange(desc(n))
    writeData(wb, "Heatmap_counts", counts)
    
    saveWorkbook(wb, file, overwrite = TRUE)
  }
  
  output$dl_excel <- downloadHandler(
    filename = function() paste0("Turismo_heatmap_filtrado_", Sys.Date(), ".xlsx"),
    content  = make_excel
  )
  
  # -------- Descargar HTML del heatmap --------
  output$dl_heat_html <- downloadHandler(
    filename = function() paste0("heatmap_", Sys.Date(), ".html"),
    content = function(file){
      req(rv$last_plot)
      htmlwidgets::saveWidget(rv$last_plot, file, selfcontained = TRUE)
    }
  )
}

shinyApp(ui, server)
