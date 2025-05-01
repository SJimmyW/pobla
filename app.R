
library(shiny)
library(ggplot2)
update.packages("rsconnect")
library(rsconnect)
library(MASS)

ui <- fluidPage(
  titlePanel("Fuerzas evolutivas y equilibrio Hardy-Weinberg"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("q", "Frecuencia del alelo recesivo (q):", 
                  min = 0.05, max = 1, value = 0.5, step = 0.01),
      sliderInput("p", "Frecuencia del alelo dominante (p):", 
                  min = 0, max = 1, value = 0.5, step = 0.001),
      
      selectInput("fuerza", "Fuerza evolutiva:",
                  choices = c(
                    "Ninguna (HWE)", 
                    "Selección contra alelo recesivo Dominancia completa",
                    "Selección contra alelo recesivo No dominancia completa",
                    "Mutación", 
                    "Migración", 
                    "Apareamiento clasificado positivo", 
                    "Apareamiento clasificado negativo", 
                    "Consanguinidad",
                    "Deriva genética",
                    "Apareamiento"
                  )),
      
      # Panel para selección (usa indexOf en lugar de includes)
      conditionalPanel(
        condition = "input.fuerza.indexOf('Selección') !== -1",
        sliderInput("s", "Fitness relativo del genotipo aa (w):", 
                    min = 0, max = 1, value = 0.5),
        sliderInput("ngen_sel", "Número de generaciones de selección:",
                    min = 1, max = 100, value = 5)
      ),
      
      conditionalPanel(
        condition = "input.fuerza == 'Mutación'",
        sliderInput("mu", "Tasa de mutación A → a:", 
                    min = 1e-8, max = 1e-4, value = 1e-6),
        sliderInput("nu", "Tasa de mutación a → A:", 
                    min = 1e-8, max = 1e-4, value = 1e-6)
      ),
      
      conditionalPanel(
        condition = "input.fuerza == 'Migración'",
        sliderInput("m", "Proporción de migrantes:", 
                    min = 0, max = 1, value = 0.5),
        sliderInput("q_mig", "Frecuencia de q en migrantes:", 
                    min = 0, max = 1, value = 0.5)
      ),
      
      conditionalPanel(
        condition = "input.fuerza == 'Consanguinidad'",
        sliderInput("F", "Coeficiente de consanguinidad (F):",
                    min = 0, max = 1, value = 0)
      ),
      
      conditionalPanel(
        condition = "input.fuerza == 'Deriva genética'",
        sliderInput("tam", "Tamaño poblacional efectivo:",
                    min = 10, max = 10000, value = 10),
        sliderInput("ngen", "Número de generaciones:",
                    min = 1, max = 100, value = 5),
        sliderInput("npob", "Número de poblaciones simuladas:",
                    min = 1, max = 50, value = 5)
      ),
      
      conditionalPanel(
        condition = "input.fuerza == 'Apareamiento'",
        fluidRow(
          column(6,
                 textInput("genotipo_padre", "Genotipo del padre:", 
                           value = "", placeholder = "Ej: AA")),
          column(6,
                 textInput("genotipo_madre", "Genotipo de la madre:", 
                           value = "", placeholder = "Ej: aa"))
        ),
        conditionalPanel(
          condition = "input.fuerza == 'Apareamiento'",
          numericInput("ndesc", "Número de descendientes:",
                       value = 1, min = 1, max = 50, step = 1),
        ),

      )
    ),
    
    mainPanel(
      #plotOutput("barplot"),
      uiOutput("plot_principal"),
      
      conditionalPanel(
        condition = "input.fuerza == 'Deriva genética' || input.fuerza.indexOf('Selección') !== -1",
        plotOutput("evolplot")
        ),
    #  plotOutput("barplot_geno"),
     # conditionalPanel(
      #  condition = "input.fuerza == 'Apareamiento'",
             #),
      
    conditionalPanel(
        condition = "input.fuerza == 'Apareamiento'",
        tableOutput("tabla_probs"),
        h4("Descendencia simulada"),
        verbatimTextOutput("desc_simulada")
        )
    )
  )
)


server <- function(input, output, session) {
  
  observeEvent(input$p, {
    
    req(input$fuerza != "Apareamiento")
    
    updateSliderInput(session, "q",
                      value = round(1 - input$p, 3))
    
    })
      
  observeEvent(input$q, {
    
    req(input$fuerza != "Apareamiento")
    
    updateSliderInput(session, "p", value = round(1 - input$q, 3))
  })
  
  output$plot_principal <- renderUI({
    if (input$fuerza == "Apareamiento") {
      plotOutput("barplot_geno")
    } else {
      plotOutput("barplot")
    }
  })
  
  output$barplot <- renderPlot({
    
    req(input$fuerza != "Apareamiento")
    
    q <- input$q
    p <- 1 - q
    
    hwe <- c(AA = p^2, Aa = 2*p*q, aa = q^2)
    
    after <- switch(input$fuerza,
                    
                    "Ninguna (HWE)" = hwe,
                    
                    "Selección contra alelo recesivo Dominancia completa" = {
                      w <- c(AA = 1, Aa = 1, aa = input$s)
                      geno <- c(p^2, 2*p*q, q^2)
                      geno <- geno * w
                      geno / sum(geno)
                    },
                    
                    "Selección contra alelo recesivo No dominancia completa" = {
                      w <- c(AA = 1, Aa = input$s * 0.5 + 0.5, 
                             aa = input$s)
                      geno <- c(p^2, 2*p*q, q^2)
                      geno <- geno * w
                      geno / sum(geno)
                    },
                    
                    "Mutación" = {
                      mu <- input$mu
                      nu <- input$nu
                      q_new <- (q * (1 - nu) + p * mu)
                      p_new <- 1 - q_new
                      c(AA = p_new^2, Aa = 2*p_new*q_new, aa = q_new^2)
                    },
                    
                    "Migración" = {
                      q_new <- (1 - input$m) * q + input$m * input$q_mig
                      p_new <- 1 - q_new
                      c(AA = p_new^2, Aa = 2*p_new*q_new, aa = q_new^2)
                    },
                    
                    "Apareamiento clasificado positivo" = {
                      p2 <- p^2
                      pq <- 2*p*q
                      q2 <- q^2
                      p4 <- p^2/(p^2 + 2*p*q)
                      gena <- data.frame( a1 = (p^2 *    p4 ) * c( 1  ,  0 ,   0 ),
                                          a2 = (p^2 * (1-p4)) * c(0.5 , 0.5,   0 ),
                                          a3 = (pq  *    p4 ) * c(0.5 , 0.5,   0 ),
                                          a4 = (p^2 * (1-p4)) * c(0.25, 0.5, 0.25),
                                          a5 =     q^2        * c( 0  ,  0 ,   1 )
                                         )
                                          
                
                      c(AA = rowSums(gena)[1],
                        Aa = rowSums(gena)[2],
                        aa = rowSums(gena)[3])
                    },
                    
                    "Apareamiento clasificado negativo" = {
                      p2 <- p^2
                      pq <- 2*p*q
                      q2 <- q^2
                      p4 <- p2/(p2 + pq)
                      gena <- data.frame( a1 = (p^2 *   q^2 ) * c( 0 ,  1 ,   0 ),
                                          a2 = (pq  *   q^2 ) * c( 0 , 0.5,  0.5),
                                          a3 = (q^2 *   p4  ) * c( 0 ,  1 ,   0 ),
                                          a4 = (q^2 * (1-p4)) * c( 0 , 0.5,  0.5)
                      )
                      
                      
                      c(AA = rowSums(gena)[1],
                        Aa = rowSums(gena)[2],
                        aa = rowSums(gena)[3])
                    },
                    
                    "Consanguinidad" = {
                      F <- input$F
                      c(AA = p^2 + p*q*F,
                        Aa = 2*p*q*(1 - F),
                        aa = q^2 + p*q*F)
                    },
                    
                    "Deriva genética" = {
                      N <- input$tam
                      q_drift <- rbinom(1, 2*N, q)/(2*N)
                      p_drift <- 1 - q_drift
                      c(AA = p_drift^2, Aa = 2*p_drift*q_drift,
                        aa = q_drift^2)
                    }
    )
    
    df <- data.frame(
      Genotipo = rep(c("AA", "Aa", "aa"), 2),
      Frecuencia = c(hwe, after),
      Estado = rep(c("Frec. Eq. H-W", "Post intervención"), 
                   each = 3)
    )
    
    ggplot(df, aes(x = Genotipo, y = Frecuencia, fill = Estado)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_color_brewer(palette = "Set1") + # scale_fill_manual(values = c("#999999", "#0072B2")) +
      labs(title = "Comparación de frecuencias genotípicas",
           y = "Frecuencia", x = "Genotipo") +
      ylim(0,1) +
      theme_minimal(base_size = 15)
  })
  
  output$evolplot <- renderPlot({
    fuerza <- input$fuerza
    req(input$fuerza != "Apareamiento")
    q <- input$q
    p <- 1 - q
    
    if (fuerza %in% c("Selección contra alelo recesivo Dominancia completa",
                      "Selección contra alelo recesivo No dominancia completa")) {
      ngen <- input$ngen_sel
      q_vec <- numeric(ngen)
      q_vec[1] <- q
      
      for (gen in 2:ngen) {
        q <- q_vec[gen - 1]
        p <- 1 - q
        geno <- c(p^2, 2*p*q, q^2)
        
        if (fuerza == "Selección contra alelo recesivo Dominancia completa") {
          w <- c(1, 1, input$s)
        } else {
          w <- c(1, input$s * 0.5 + 0.5, input$s)
        }
        
        geno_sel <- geno * w
        geno_sel <- geno_sel / sum(geno_sel)
        q_vec[gen] <- geno_sel[3] + 0.5 * geno_sel[2]
      }
      
      df <- data.frame(
        Generación = 1:ngen,
        q = q_vec
      )
      
      ggplot(df, aes(x = Generación, y = q)) +
        geom_line(size = 1.5, color = "#D55E00") +
        labs(title = paste("Evolución de q por selección -", fuerza),
             x = "Generación", y = "Frecuencia de q") +
        ylim(0, 1) +
        theme_minimal(base_size = 15)
      
    } else if (fuerza == "Deriva genética") {
      ngen <- input$ngen
      N <- input$tam
      q0 <- input$q
      npob <- input$npob
      
      df_drift <- data.frame()
      
      for (i in 1:npob) {
        q_vec <- numeric(ngen)
        q_vec[1] <- q0
        for (j in 2:ngen) {
          q_vec[j] <- rbinom(1, 2 * N, q_vec[j - 1]) / (2 * N)
        }
        df_drift <- rbind(df_drift,
                          data.frame(Generación = 1:ngen, 
                                     q = q_vec, 
                                     Población = paste("Pop", i)))
      }
      
      ggplot(df_drift, aes(x = Generación, y = q, color = Población)) +
        geom_line(size = 1) +
        labs(title = "Deriva genética del alelo q en múltiples poblaciones",
             x = "Generación",
             y = "Frecuencia del alelo q") +
        ylim(0, 1) +
        theme_minimal(base_size = 15) +
        theme(legend.position = "right")
    }
  })
  
  output$barplot_geno <- renderPlot({

    req(input$fuerza == "Apareamiento",
        nzchar(as.character(input$genotipo_padre)),
        nzchar(as.character(input$genotipo_madre)))
    
    padre_str <- as.character(input$genotipo_padre)
    madre_str <- as.character(input$genotipo_madre)
   
     validate(
      need(nchar(padre_str) == nchar(madre_str), 
           "Genotipos de diferente longitud")
    )
    
    # Separa alelos y generar gametos
    n_alleles <- nchar(padre_str)
    n_genes <- n_alleles / 2
    
    split_alleles <- function(geno) split(strsplit(geno, "")[[1]], rep(1:n_genes, each=2))
    p_alleles <- split_alleles(padre_str)
    m_alleles <- split_alleles(madre_str)
    gam_p <- apply(expand.grid(p_alleles, stringsAsFactors=FALSE), 1, paste0, collapse="")
    gam_m <- apply(expand.grid(m_alleles, stringsAsFactors=FALSE), 1, paste0, collapse="")
    combos <- expand.grid(gam_p, gam_m, stringsAsFactors=FALSE)
    
    # Formar genotipos hijos ordenados
    hijos <- apply(combos, 1, function(x) {
      gp <- strsplit(x[[1]], "")[[1]]
      gm <- strsplit(x[[2]], "")[[1]]
      gene_strs <- vapply(seq_len(n_genes), function(i) {
        pair <- c(gp[i], gm[i])
        ord <- order(tolower(pair), -(pair == toupper(pair)))
        paste0(pair[ord], collapse="")
      }, "")
      paste0(gene_strs, collapse="")
    })
    
    # Tabla de probabilidades
    tab <- as.data.frame(table(hijos) / length(hijos),
                         stringsAsFactors = FALSE)
    colnames(tab) <- c("Genotipo", "Probabilidad")
    # Añadir fracción
    tab$Fraccion <- as.character(fractions(tab$Probabilidad))
    tab <- tab[order(tab$Genotipo), ]
    
    # Graficar
    ggplot(tab, aes(x = Genotipo, y = Probabilidad)) +
      geom_col(fill = "steelblue") +
      geom_text(aes(label = Fraccion), vjust = -0.5, size = 5) +
      labs(title = "Probabilidades genotípicas - Apareamiento",
           x = "Genotipo", y = "Probabilidad") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      theme_minimal(base_size = 14)
  })

  output$tabla_probs <- renderTable({
    
    req(input$fuerza == "Apareamiento",
        nzchar(as.character(input$genotipo_padre)),
        nzchar(as.character(input$genotipo_madre)))
    
    validate(
      need(nchar(padre_str) == nchar(madre_str), 
           "Genotipos de diferente longitud") )
    
    padre_str <- as.character(input$genotipo_padre)
    madre_str <- as.character(input$genotipo_madre)
    
    nalelos <- nchar(padre_str)
    n_genes <- nalelos / 2
    
    # Función para separar alelos por gen
    sep_alelos <- function(geno_str) {
      alelos <- strsplit(geno_str, "")[[1]]
      split(alelos, rep(1:n_genes, each = 2))
    }
    
    padre_alelos <- sep_alelos(padre_str)
    madre_alelos <- sep_alelos(madre_str)
    
    # Generar lista de gametos posibles (uno por gen)
    gametos_padre <- expand.grid(padre_alelos, 
                                 stringsAsFactors = FALSE)
    gametos_padre <- apply(gametos_padre, 1, paste0, collapse = "")
    
    gametos_madre <- expand.grid(madre_alelos, 
                                 stringsAsFactors = FALSE)
    gametos_madre <- apply(gametos_madre, 1, paste0, collapse = "")
    
    # combinaciones padre - madre
    combos <- expand.grid(gametos_padre, 
                          gametos_madre, 
                          stringsAsFactors = FALSE)
    
    # cada combinación,dell genotipo hijo ordenando cada par
    hijos <- apply(combos, 1, function(x) {
      
      gp <- strsplit(x[[1]], "")[[1]]
      gm <- strsplit(x[[2]], "")[[1]]
      
      geno_progenie <- vapply(seq_len(n_genes), function(i) {
        pair <- c(gp[i], gm[i])
        # Orden mayúscula antes que minúscula y alfabético
        ord <- order(tolower(pair), -(pair == toupper(pair)))
        paste0(pair[ord], collapse = "")
      }, FUN.VALUE = "")
      
      paste0(geno_progenie, collapse = "")
    })
    
    # Calcular probabilidades
    probs <- as.data.frame(table(hijos) / length(hijos))
    colnames(probs) <- c("Genotipo", "Probabilidad")
    probs[order(probs$Genotipo), , drop = FALSE]
    
  }, rownames = TRUE)
  
  output$desc_simulada <- renderPrint({
    
    req(input$fuerza == "Apareamiento",
        nzchar(as.character(input$genotipo_padre)),
        nzchar(as.character(input$genotipo_madre)))
    
    padre_str <- as.character(input$genotipo_padre)
    madre_str <- as.character(input$genotipo_madre)
    
    validate(
      need(nchar(as.character(padre_str)) == nchar(as.character(madre_str)), 
           "Genotipos de diferente longitud") 
      )
    
    nalelos <- nchar(padre_str)
    n_genes <- nalelos / 2
    
    n <- input$ndesc
    
    sep_alelos <- function(geno_str) {
      alelos <- strsplit(geno_str, "")[[1]]
      split(alelos, rep(1:n_genes, each = 2))
    }
    
    padre_alelos <- sep_alelos(padre_str)
    madre_alelos <- sep_alelos(madre_str)
    
    gametos_padre <- apply(expand.grid(padre_alelos,
                                       stringsAsFactors = FALSE), 
                           1, paste0, collapse = "")
    gametos_madre <- apply(expand.grid(madre_alelos,
                                       stringsAsFactors = FALSE), 
                           1, paste0, collapse = "")
    
    gametos_p <- sample(gametos_padre, n, replace = TRUE)
    gametos_m <- sample(gametos_madre, n, replace = TRUE)
    
    # Función para ordenar alelos gen por gen
    hijos <- mapply(function(p, m) {
      gp <- strsplit(p, "")[[1]]
      gm <- strsplit(m, "")[[1]]
      
      geno_progenie <- vapply(seq_len(n_genes), function(i) {
        pair <- c(gp[i], gm[i])
        ord <- order(tolower(pair), -(pair == toupper(pair)))
        paste0(pair[ord], collapse = "")
      }, FUN.VALUE = "")
      
      paste0(geno_progenie, collapse = "")
    }, gametos_p, gametos_m)
    
    tabla <- sort(table(hijos), decreasing = TRUE)
    
    cat("Genotipos posibles y frecuencia:\n")
    print(tabla)
   
  })
  
  }

shinyApp(ui = ui, server = server)

shinyApp(ui = ui, server = server)
