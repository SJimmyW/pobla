
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readr)
library(writexl)
library(readxl)


ui <- fluidPage(
  titlePanel("Carga de Datos (imaginación titulística = 0)"),
  
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Carga manual",
                 selectInput("evento", "Evento a cargar:",
                             choices = c("Nacimiento", "Tratamiento", 
                                         "Muerte", "Desleche",
                                         "Alerta de mortalidad", 
                                         "Alerta de morbilidad"
                                         )
                             ),
                 
                 conditionalPanel(
                   condition = "input.evento == 'Nacimiento'",
                   textInput("caravana_nac", "Caravana:"),
                   dateInput("fecha_nac", "Fecha de nacimiento:"),
                   actionButton("agregar_nacimiento",
                                "Agregar Nacimiento")
                   ),
                 
                 conditionalPanel(
                   condition = "input.evento == 'Tratamiento'",
                   selectInput("caravana_trat", "Caravana:", 
                               choices = NULL),
                   dateInput("fecha_trat", "Fecha del tratamiento:", 
                             # propone fecha del sistema, elegible si es carga tardía
                             value = Sys.Date()
                             ),
                   textInput("motivo_trat", "Motivo del tratamiento:"),
                   actionButton("agregar_tratamiento", "Agregar")
                   ),
                 
                 conditionalPanel(
                   condition = "input.evento == 'Muerte'",
                   selectInput("caravana_muerte", "Caravana:", 
                               choices = NULL),
                   dateInput("fecha_muerte", "Fecha de muerte:",
                             value = Sys.Date()),
                   # propone fecha del sistema, elegible si es carga tardía
                   textInput("motivo_muerte",
                             "Causa de muerte:"),
                   actionButton("agregar_muerte", "Agregar")
                 ),
                 
                 conditionalPanel(
                   condition = "input.evento == 'Desleche'",
                   selectInput("caravana_desleche", "Caravana:",
                               choices = NULL),
                   dateInput("fecha_desleche", "Fecha de desleche:",
                             value = Sys.Date()),
                   actionButton("agregar_desleche", "Agregar")
                 ),
                 
                 conditionalPanel(
                   condition = "input.evento == 'Alerta de mortalidad'",
                   numericInput("alerta_mortalidad", 
                                "Valor (entre 0 a 100):",
                                value = 5, min = 0, max = 100, 
                                step = 0.5),
                   actionButton("registrar_alerta_mort", 
                                "Registrar alerta")
                 ),
                 
                 conditionalPanel(
                   condition = "input.evento == 'Alerta de morbilidad'",
                   numericInput("alerta_morbilidad", "Valor (0 a 100):",
                                value = 5, min = 0, max = 100, step = 0.5),
                   actionButton("registrar_alerta_morb", "Registrar alerta")
                 )
        ),
        
   
        
        tabPanel("Carga desde archivo",
                 fileInput("archivo_datos", "Seleccioná tu archivo base (xlsx)",
                           accept = ".xlsx"),
                 helpText("El archivo debe cumplir con las especificaciones
                          detalladas en el enlace a continuación 
                          (https://docs.google.com/spreadsheets/d/1ZW-wP_XXhVXDzzJWlfoos203AxnPJlY_/edit?usp=sharing&ouid=114656964206259110283&rtpof=true&sd=true"),
                 textInput("ruta_archivo", "Ruta al archivo base (.xlsx)",
                           value = "eventos.xlsx"),
                 actionButton("cargar_archivo", "Cargar Datos")
        )
      )
    ),
    
    mainPanel(
      h4("Resumen de Eventos cargados"),
      tableOutput("resumen"),
      
      downloadButton("descargar_datos", 
                     "Descargar todos los datos en Excel"),
      
      h4("Mortalidad mensual"),
      plotOutput("plot_mortalidad"),
      
      h4("Morbilidad mensual"),
      plotOutput("plot_morbilidad")
    )
  )
)

server <- function(input, output, session) {
  
  max_deleche <- 90 # máximo tiempo posible en guachera
  eventos <- reactiveValues(
    nacimientos = data.frame(caravana = character(), 
                             fecha = as.Date(character())),
    tratamientos = data.frame(caravana = character(), 
                              fecha = as.Date(character()),
                              motivo = character()),
    muertes = data.frame(caravana = character(), 
                         fecha = as.Date(character())),
    desleches = data.frame(caravana = character(), 
                           fecha = as.Date(character())),
    mortalidad = data.frame(fecha = as.Date(character()), 
                            valor = numeric()),
    morbilidad = data.frame(fecha = as.Date(character()), 
                            valor = numeric())
  )
  
  caravanas_activas <- reactive({
    nacidas <- unique(eventos$nacimientos$caravana)
    muertas <- unique(eventos$muertes$caravana)
    deslechadas <- unique(eventos$desleches$caravana)
    
    setdiff(nacidas, union(muertas, deslechadas))
  })
  
  # Carga manual
  observeEvent(input$agregar_nacimiento, {
    
    nacidos <- any(
      eventos$nacimientos$caravana == input$caravana_nac &
      eventos$nacimientos$fecha    == input$fecha_nac
    )
    if (nacidos) {
      showNotification( paste0(
        "⚠️ Ya se registró el nacimiento para la caravana ", input$caravana_nac,
        " hoy.", type = "warning", duration = 5
        )
        )
      
    } else {
      eventos$nacimientos <- rbind(
        eventos$nacimientos,
        data.frame(
          caravana = input$caravana_nac,
          fecha    = input$fecha_nac,
          stringsAsFactors = FALSE
        )
      )
    }
    
  })
  
  observeEvent(input$agregar_tratamiento, {
    
    tratados <- any(
      eventos$tratamientos$caravana == input$caravana_trat &
        eventos$tratamientos$fecha    == input$fecha_trat
    )
    if (tratados) {
      showNotification( paste0(
        "⚠️ Ya se registró un tratamiento para la caravana ", input$caravana_trat, 
        " hoy", type = "warning", duration = 5
        )
        )
    } else {
      eventos$tratamientos <- rbind(
        eventos$tratamientos,
        data.frame(
          caravana = input$caravana_trat,
          fecha    = input$fecha_trat,
          motivo   = input$motivo_trat,
          stringsAsFactors = FALSE
        )
      )
    }
  })
  
  observeEvent(input$agregar_muerte, {
    eventos$muertes <- rbind(eventos$muertes,
                             data.frame(caravana = input$caravana_muerte,
                                        fecha = input$fecha_muerte))
  })
  
  observeEvent(input$agregar_desleche, {
    eventos$desleches <- rbind(eventos$desleches,
                               data.frame(caravana = input$caravana_desleche,
                                          fecha = input$fecha_desleche))
  })
  
  
  ###  controla que ningún ternero permanezca más de max_desleche#####################
  control_calidad <- function() {
    nac <- eventos$nacimientos
    muertes <- eventos$muertes
    desleches <- eventos$desleches
    
    # Comprobamos si algún ternero ha permanecido más de 90 días en guachera
    for (i in 1:nrow(nac)) {
      caravana <- nac$caravana[i]
      fecha_nac <- nac$fecha[i]
      
      fecha_fin <- pmin(
        fecha_nac + days(max_deleche),
        coalesce(muertes$fecha[match(caravana, muertes$caravana)],
                 fecha_nac + days(max_deleche)),
        coalesce(desleches$fecha[match(caravana, desleches$caravana)],
                 fecha_nac + days(max_deleche))
      )
      
      if (fecha_fin < Sys.Date()) {
        # Asignar el evento de desleche con la fecha actual
        eventos$desleches <- rbind(eventos$desleches,
                                   data.frame(caravana = caravana,
                                              fecha = Sys.Date()))  # Fecha actual
        motivo <- "Desleche automático, más de 90 días en guachera"
        eventos$tratamientos <- rbind(eventos$tratamientos,
                                      data.frame(caravana = caravana,
                                                 fecha = Sys.Date(),
                                                 motivo = motivo))
      }
    }
    
    # Actualizar las caravanas activas  caravanas_activas()
  }
  
  # Carga desde archivo
  observeEvent(input$cargar_archivo, {
    req(input$archivo_datos)
    datos <- read_excel(input$archivo_datos$datapath)
    
    if ("fecha" %in% names(datos)) {
      datos$fecha <- as.Date(datos$fecha)
    }
    
    if ("evento" %in% names(datos)) {
      
      if ("Nacimiento" %in% datos$evento) {
        nac <- filter(datos, evento == "Nacimiento") %>%
          select(caravana, fecha)
        eventos$nacimientos <- bind_rows(eventos$nacimientos, nac)
      }
      if ("Tratamiento" %in% datos$evento) {
        trat <- filter(datos, evento == "Tratamiento") %>%
          select(caravana, fecha, motivo)
        eventos$tratamientos <- bind_rows(eventos$tratamientos, trat)
      }
      if ("Muerte" %in% datos$evento) {
        muerte <- filter(datos, evento == "Muerte") %>%
          select(caravana, fecha)
        eventos$muertes <- bind_rows(eventos$muertes, muerte)
      }
      if ("Desleche" %in% datos$evento) {
        des <- filter(datos, evento == "Desleche") %>%
          select(caravana, fecha)
        eventos$desleches <- bind_rows(eventos$desleches, des)
      }
      
    }
    
    control_calidad()
  })
  
 
  # vector reactivo, actualiza las disponibles
  observe({
    caravanas <- caravanas_activas()
   # caravanas <- unique(eventos$nacimientos$caravana)
    
    updateSelectInput(session, "caravana_trat", 
                      choices = caravanas_activas())
    updateSelectInput(session, "caravana_muerte", 
                      choices = caravanas_activas())
    updateSelectInput(session, "caravana_desleche", 
                      choices = caravanas_activas())
    
    })
  
  # Tabla resumen
  output$resumen <- renderTable({
   
    ternero_guachera_hoy <- nrow(eventos$nacimientos) - 
                      (nrow(eventos$muertes) + nrow(eventos$desleches))
    ternero_dia <- ternero_guachera_hoy / days_in_month(today())
    muertos <-  nrow(eventos$muertes)
    hoy <- Sys.Date()
    
    data.frame(
      Evento = c("Nacimientos", "Tratamientos", "Muertes", "Desleches", 
                 "Terneros en guachera hoy",
                 "Promedio de terneros dia", "% de mortalidad"),
      
      Cantidad = c(
        nrow(filter(eventos$nacimientos,  fecha == hoy)),    #  nrow(eventos$nacimientos),
        nrow(filter(eventos$tratamientos, fecha == hoy)),   # nrow(eventos$tratamientos),
        nrow(filter(eventos$muertes,      fecha == hoy)),        # muertos,
        nrow(filter(eventos$desleches,    fecha == hoy)),                      #nrow(eventos$desleches),
        ternero_guachera_hoy,
        ternero_dia ,
        muertos / ternero_dia
      )
    )
  })
  
  # Descarga  en Excel
  output$descargar_datos <- downloadHandler(
    
    filename = function() {
      if (!is.null(input$archivo_datos)) {
        basename(input$archivo_datos$name)
      } else {
        paste0("eventos-", Sys.Date(), ".xlsx")
      }
    },
    
    content = function(file) {
      # Crear un único data frame
      df_novedades <- bind_rows(
        eventos$nacimientos %>% mutate(evento = "Nacimiento", 
                                       motivo = NA_character_),
        eventos$tratamientos %>% mutate(evento = "Tratamiento"),
        eventos$muertes %>% mutate(evento = "Muerte", 
                                   motivo = NA_character_),
        eventos$desleches %>% mutate(evento = "Desleche",
                                     motivo = NA_character_)
      ) %>%
        select(caravana, fecha, evento, motivo)
      
      if (!is.null(input$archivo_datos)) {
        datos_existentes <- read_excel(input$archivo_datos$datapath)
        
        datos_actualizados <- bind_rows(datos_existentes, df_novedades)
        
        write_xlsx(datos_actualizados, path = file)
        
      } else {
        write_xlsx(df_novedades, path = file)
      }
    }
  )
  
  # Gráfico de mortalidad mensual corregido
  output$plot_mortalidad <- renderPlot({
    
    req(nrow(eventos$nacimientos) > 0)
    
    umbral <- input$alerta_mortalidad
    
    nac <- eventos$nacimientos
    muertes <- eventos$muertes
    desleches <- eventos$desleches
    
    guachera_dias <- nac %>%
      mutate(fecha_fin = pmin(
        fecha + days(max_deleche),
        coalesce(muertes$fecha[match(caravana, muertes$caravana)], 
                 fecha + days(max_deleche)),
        coalesce(desleches$fecha[match(caravana, desleches$caravana)], 
                 fecha + days(max_deleche))
      ),
      fecha_fin = if_else(fecha_fin < fecha, fecha, fecha_fin)
      ) %>%
      rowwise() %>%# mete un control de calidad. Que fecha desleche, muerte o tratamiento sea posterior al nacimiento 
      mutate(fecha_fin = if_else(fecha_fin < Sys.Date(), Sys.Date(),
                                 fecha_fin),
             dias = list(seq.Date(fecha, fecha_fin, by = "day"))
      ) %>%
      unnest(dias)
    
    if(any(guachera_dias$fecha_fin < Sys.Date())) {
  showNotification(
    "Se corrigieron fechas inconsistentes (ajuste a la fecha actual).",
    type = "warning",
    duration = 5
    ) 
      }
    
    terneros_por_dia <- guachera_dias %>%
      group_by(dia = dias) %>%
      summarise(terneros = n(), .groups = "drop") %>%
      mutate(mes = floor_date(dia, "month"))
    
    promedio_mes <- terneros_por_dia %>%
      group_by(mes) %>%
      summarise(promedio_diario = mean(terneros), .groups = "drop")
    
    muertos_mes <- eventos$muertes %>%
      mutate(mes = floor_date(fecha, "month")) %>%
      group_by(mes) %>%
      summarise(muertes = n(), .groups = "drop")
    
    datos_mortalidad <- left_join(promedio_mes, muertos_mes, by = "mes") %>%
      replace_na(list(muertes = 0)) %>%
      mutate(
        mortalidad = ifelse(promedio_diario > 0, 
                            (muertes / promedio_diario) * 100, NA),
        color = ifelse(mortalidad >= umbral, "supera", "normal")
      )
    
    ggplot(datos_mortalidad, aes(x = mes, y = mortalidad, fill = color)) +
      geom_col() +
      scale_fill_manual(values = c("normal" = "skyblue", 
                                   "supera" = "red")) +
      labs( x = "Mes", y = "Mortalidad (%)") +
      guides(fill = "none") +
      theme_minimal()
    
  
  })
  
  # Gráfico de morbilidad mensual corregido
  output$plot_morbilidad <- renderPlot({
    req(nrow(eventos$tratamientos) > 0, nrow(eventos$nacimientos) > 0)
    
    umbral <- input$alerta_morbilidad
    
    nac <- eventos$nacimientos
    muertes <- eventos$muertes
    desleches <- eventos$desleches
    
    guachera_dias <- nac %>%
      mutate(fecha_fin = pmin(
        fecha + days(max_deleche),
        coalesce(muertes$fecha[match(caravana, muertes$caravana)],
                 fecha + days(max_deleche)),
        coalesce(desleches$fecha[match(caravana, desleches$caravana)], 
                 fecha + days(max_deleche))
      ),
      fecha_fin = if_else(fecha_fin < fecha, fecha, fecha_fin)
      ) %>%
      rowwise() %>% # mete un control de calidad. Que fecha desleche, muerte o tratamiento sea posterior al nacimiento 
      mutate(fecha_fin = if_else(fecha_fin < Sys.Date(), Sys.Date(),
                                 fecha_fin),
             dias = list(seq.Date(fecha, fecha_fin, by = "day"))
      ) %>%
      unnest(dias)
    
    terneros_por_dia <- guachera_dias %>%
      group_by(dia = dias) %>%
      summarise(terneros = n(), .groups = "drop") %>%
      mutate(mes = floor_date(dia, "month"))
    
    promedio_mes <- terneros_por_dia %>%
      group_by(mes) %>%
      summarise(promedio_diario = mean(terneros), .groups = "drop")
    
    morb <- eventos$tratamientos %>%
      mutate(mes = floor_date(fecha, "month")) %>%
      group_by(mes) %>%
      summarise(caravanas_tratadas = n_distinct(caravana), .groups = "drop")
    
    datos_morbilidad <- left_join(promedio_mes, morb, by = "mes") %>%
      replace_na(list(caravanas_tratadas = 0)) %>%
      mutate(
        morbilidad = ifelse(promedio_diario > 0,
                            (caravanas_tratadas / promedio_diario) * 100,
                            NA),
        color = ifelse(morbilidad >= umbral, "supera", "normal")
      )
    
    ggplot(datos_morbilidad, aes(x = mes, y = morbilidad)) +
      geom_line(color = "black") +
      #geom_point(aes(color = color), size = 3) +
      geom_smooth(method = "loess", se = FALSE,
                  color = "darkgreen", linewidth = 1) +
      scale_color_manual(values = c("normal" = "skyblue",
                                    "supera" = "red")) +
      labs(title = "", x = "Mes", y = "Morbilidad (%)") +
      theme_minimal()
  })
  
  session$onSessionEnded(function() {

    df_novedades <- bind_rows(
      eventos$nacimientos %>% mutate(evento = "Nacimiento", 
                                     motivo = NA_character_),
      eventos$tratamientos %>% mutate(evento = "Tratamiento"),
      eventos$muertes %>% mutate(evento = "Muerte", 
                                 motivo = NA_character_),
      eventos$desleches %>% mutate(evento = "Desleche", 
                                   motivo = NA_character_)
    ) %>%
      select(caravana, fecha, evento, motivo)
    
    # Definir la ruta donde guardar
    ruta_guardado <- paste0("www/eventos-guardados-", Sys.Date(), ".xlsx")
    
    if (!dir.exists("www")) {
      dir.create("www")  # Crear carpeta www si no existe
    }
    
    if (!is.null(input$archivo_datos)) {
      # Si hubo archivo cargado
      datos_existentes <- read_excel(input$archivo_datos$datapath)
      datos_actualizados <- bind_rows(datos_existentes, df_novedades)
      write_xlsx(datos_actualizados, path = ruta_guardado)
    } else {
      write_xlsx(df_novedades, path = ruta_guardado)
    }
    
    cat("Archivo guardado automáticamente en", ruta_guardado, "\n")
  })
  
}


shinyApp(ui = ui, server = server)


#library(rsconnect)

#rsconnect::setAccountInfo(name='jimmymega', token='2BB4FBD759719DB0716D4CD468FD3386', secret='Z8rMNdmwYoc36hAViM6BybGztmij6LoX5ev/2S/h')

#setwd("C:/Users/GoodGame/Mi unidad/shiny2")
#rsconnect::deployApp()

