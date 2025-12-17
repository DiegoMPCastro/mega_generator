# ============================================================================
# GERADOR DE SEQUÊNCIAS MEGA-SENA COM ANÁLISE BAYESIANA (SHINY APP)
# ============================================================================

library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(DT)

# ============================================================================
# 1. FUNÇÕES DO NÚCLEO (Lógica Original Mantida + Novas Análises)
# ============================================================================

extrair_numeros <- function(dados) {
    colunas_nomes <- names(dados)
    
    # Padrões comuns
    bola_cols <- grep("^(bola|Bola|BOLA|Dezena|dezena|DEZENA|B)", colunas_nomes, value = TRUE)
    
    # Tentativa automática por posição se falhar o nome
    if (length(bola_cols) < 6) {
        numeric_cols <- sapply(dados, function(x) is.numeric(x) || all(!is.na(suppressWarnings(as.numeric(x)))))
        numeric_col_names <- names(dados)[numeric_cols]
        
        if (length(numeric_col_names) >= 6) {
            primeira_col <- dados[[numeric_col_names[1]]]
            # Se a primeira coluna parece índice/concurso, pula ela
            if (is.numeric(primeira_col) && all(diff(primeira_col[1:min(10, length(primeira_col))]) > 0)) {
                bola_cols <- numeric_col_names[2:7]
            } else {
                bola_cols <- numeric_col_names[1:6]
            }
        }
    }
    
    if (length(bola_cols) < 6) {
        stop("Não foi possível identificar as 6 colunas de números sorteados automaticamente.")
    } else {
        bola_cols <- bola_cols[1:6]
    }
    
    numeros_matrix <- as.matrix(dados[, bola_cols])
    numeros_list <- lapply(1:nrow(numeros_matrix), function(i) {
        nums <- as.numeric(numeros_matrix[i, ])
        nums <- nums[!is.na(nums)]
        sort(nums)
    })
    
    return(numeros_list)
}

calcular_frequencias <- function(numeros_list) {
    freq <- table(unlist(numeros_list))
    freq_df <- data.frame(
        numero = as.numeric(names(freq)),
        frequencia = as.numeric(freq)
    )
    todos_numeros <- data.frame(numero = 1:60)
    freq_completo <- merge(todos_numeros, freq_df, by = "numero", all.x = TRUE)
    freq_completo$frequencia[is.na(freq_completo$frequencia)] <- 0
    return(freq_completo)
}

# NOVA FUNÇÃO: Calcular atrasos (concursos desde a última aparição)
calcular_atrasos <- function(numeros_list) {
    atrasos <- rep(0, 60)
    names(atrasos) <- 1:60
    n_jogos <- length(numeros_list)
    
    for (num in 1:60) {
        encontrou <- FALSE
        for(i in n_jogos:1) {
            if(num %in% numeros_list[[i]]) {
                atrasos[num] <- n_jogos - i
                encontrou <- TRUE
                break
            }
        }
        if(!encontrou) atrasos[num] <- n_jogos # Nunca saiu
    }
    return(atrasos)
}

calcular_coocorrencias <- function(numeros_list, numero_base) {
    coocorrencias <- rep(0, 60)
    names(coocorrencias) <- 1:60
    
    for (sorteio in numeros_list) {
        if (numero_base %in% sorteio) {
            for (num in sorteio) {
                if (num != numero_base) {
                    coocorrencias[num] <- coocorrencias[num] + 1
                }
            }
        }
    }
    return(coocorrencias)
}

calcular_probabilidades_bayesianas <- function(freq_completo, coocorrencias, numero_base, 
                                               peso_frequencia = 0.4, peso_coocorrencia = 0.6) {
    prior <- freq_completo$frequencia / sum(freq_completo$frequencia)
    likelihood <- coocorrencias / sum(coocorrencias)
    
    # Tratar divisões por zero se houver
    prior[is.nan(prior)] <- 0
    likelihood[is.nan(likelihood)] <- 0
    
    posterior <- peso_frequencia * prior + peso_coocorrencia * likelihood
    posterior[numero_base] <- 0
    
    if(sum(posterior) > 0) {
        posterior <- posterior / sum(posterior)
    }
    return(posterior)
}

analisar_padroes_recentes <- function(numeros_list, ultimos_n = 50) {
    n_sorteios <- length(numeros_list)
    inicio <- max(1, n_sorteios - ultimos_n + 1)
    numeros_recentes <- numeros_list[inicio:n_sorteios]
    freq_recente <- table(unlist(numeros_recentes))
    return(freq_recente)
}

calcular_score_pares <- function(numero_base, candidatos, numeros_list) {
    scores <- numeric(length(candidatos))
    for (i in seq_along(candidatos)) {
        num <- candidatos[i]
        count <- sum(sapply(numeros_list, function(sorteio) {
            numero_base %in% sorteio && num %in% sorteio
        }))
        scores[i] <- count
    }
    return(scores)
}

gerar_sequencia <- function(numero_inicial, quantidade, freq_completo, 
                            coocorrencias_base, numeros_list, freq_recente) {
    
    # 1. Probabilidades Base (Freq Global + Recente)
    peso_recente <- 0.3
    peso_historico <- 0.7
    
    # Prob Recente
    prob_recente <- rep(0, 60)
    nomes_recentes <- names(freq_recente)
    for (num in nomes_recentes) {
        idx <- as.numeric(num)
        if(!is.na(idx) && idx >= 1 && idx <= 60) {
            prob_recente[idx] <- as.numeric(freq_recente[num])
        }
    }
    if(sum(prob_recente) > 0) prob_recente <- prob_recente / sum(prob_recente)
    
    # Prob Historica (Prior)
    prior <- freq_completo$frequencia / sum(freq_completo$frequencia)
    prior[is.nan(prior)] <- 0
    
    # 2. Inicializar Seleção
    numeros_selecionados <- c()
    numeros_disponiveis <- 1:60
    
    # --- LÓGICA DE DECISÃO: COM OU SEM PIVÔ ---
    if (!is.null(numero_inicial)) {
        # MODO A: COM PIVÔ
        numeros_selecionados <- c(numero_inicial)
        numeros_disponiveis <- setdiff(numeros_disponiveis, numero_inicial)
        
        # Calcular Bayesiana (apenas se tiver pivô)
        likelihood <- coocorrencias_base / sum(coocorrencias_base)
        likelihood[is.nan(likelihood)] <- 0
        
        prob_bayesiana <- 0.4 * prior + 0.6 * likelihood
        prob_bayesiana[numero_inicial] <- 0
        if(sum(prob_bayesiana) > 0) prob_bayesiana <- prob_bayesiana / sum(prob_bayesiana)
        
        prob_base_step <- peso_historico * prob_bayesiana + peso_recente * prob_recente
        
    } else {
        # MODO B: SEM PIVÔ
        # Probabilidade inicial é puramente estatística
        prob_base_step <- peso_historico * prior + peso_recente * prob_recente
        
        # Escolher primeiro número baseado nas probabilidades
        prob_init <- prob_base_step
        if(sum(prob_init) > 0) prob_init <- prob_init / sum(prob_init)
        
        primeiro <- sample(numeros_disponiveis, 1, prob = prob_init)
        numeros_selecionados <- c(primeiro)
        numeros_disponiveis <- setdiff(numeros_disponiveis, primeiro)
    }
    
    # 3. Loop para os números restantes
    target_size <- quantidade
    current_size <- length(numeros_selecionados)
    
    for (i in 1:(target_size - current_size)) {
        
        # Atualizar Probabilidades (remover selecionados)
        prob_atual <- prob_base_step
        prob_atual[numeros_selecionados] <- 0
        if(sum(prob_atual) > 0) prob_atual <- prob_atual / sum(prob_atual)
        
        # Calcular Afinidade (Pares)
        if (!is.null(numero_inicial)) {
            # Com Pivô: Afinidade com o pivô (Comportamento Original)
            scores_pares <- calcular_score_pares(numero_inicial, numeros_disponiveis, numeros_list)
        } else {
            # Sem Pivô: Afinidade acumulada com TODOS os números já selecionados neste jogo
            scores_acumulados <- rep(0, length(numeros_disponiveis))
            
            # Iterar sobre os números que já estão no jogo atual para ver quem combina com eles
            # (Isso garante coesão no jogo gerado)
            for(sel_num in numeros_selecionados) {
                s <- calcular_score_pares(sel_num, numeros_disponiveis, numeros_list)
                scores_acumulados <- scores_acumulados + s
            }
            scores_pares <- scores_acumulados
        }
        
        scores_pares_norm <- if(sum(scores_pares) > 0) scores_pares / sum(scores_pares) else rep(0, length(scores_pares))
        
        # Combinar Probabilidade Estatística + Afinidade de Pares
        prob_combinada <- 0.7 * prob_atual[numeros_disponiveis] + 0.3 * scores_pares_norm
        
        if(sum(prob_combinada) == 0) {
            proximo_numero <- sample(numeros_disponiveis, 1)
        } else {
            proximo_numero <- sample(numeros_disponiveis, 1, prob = prob_combinada)
        }
        
        numeros_selecionados <- c(numeros_selecionados, proximo_numero)
        numeros_disponiveis <- setdiff(numeros_disponiveis, proximo_numero)
    }
    
    return(sort(numeros_selecionados))
}

# ============================================================================
# 2. UI (INTERFACE DO USUÁRIO)
# ============================================================================

ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "flatly"),
    
    titlePanel("Gerador Mega-Sena Bayesiano"),
    
    sidebarLayout(
        sidebarPanel(
            h4("Configurações"),
            fileInput("arquivo_excel", "Carregar Histórico (.xlsx)",
                      accept = c(".xlsx")),
            
            helpText("Faça upload do arquivo 'mega_sena_asloterias...xlsx' para começar."),
            
            hr(),
            
            # CONTROLE DE PIVÔ
            checkboxInput("usar_pivo", "Fixar Número Pivô?", value = TRUE),
            helpText("Se marcado, todos os jogos incluirão este número e buscarão seus pares mais comuns."),
            
            conditionalPanel(
                condition = "input.usar_pivo == true",
                sliderInput("num_inicial", "Número Base (Pivô):",
                            min = 1, max = 60, value = 10)
            ),
            
            hr(),
            
            sliderInput("qtd_dezenas", "Dezenas por Jogo:",
                        min = 6, max = 15, value = 6),
            
            numericInput("qtd_jogos", "Gerar quantos jogos?", 
                         value = 5, min = 1, max = 50),
            
            checkboxInput("usar_filtros", "Filtros Inteligentes (Soma/Pares)", value = TRUE),
            
            actionButton("btn_processar", "Gerar Jogos", 
                         class = "btn-primary btn-lg btn-block", icon = icon("dice")),
            
            hr(),
            div(style = "color: #7f8c8d; font-size: 0.8em;",
                "AVISO: Este app usa métodos estatísticos para diversão. Não aumenta chances reais de ganho.")
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Jogos Gerados", icon = icon("list-ol"),
                         br(),
                         h3("Sugestões de Jogos"),
                         DTOutput("tabela_jogos"),
                         br(),
                         uiOutput("stats_resumo")
                ),
                
                tabPanel("Análise Gráfica", icon = icon("chart-bar"),
                         br(),
                         # Layout condicional para os gráficos
                         fluidRow(
                             column(6, plotOutput("plot_freq_geral")),
                             column(6, plotOutput("plot_coocorrencia"))
                         ),
                         br(),
                         fluidRow(
                             column(12, plotOutput("plot_atrasos"))
                         ),
                         br(),
                         p("Abaixo: Dezenas mais Atrasadas (Frias).")
                ),
                
                tabPanel("Dados Brutos", icon = icon("table"),
                         br(),
                         textOutput("info_arquivo"),
                         verbatimTextOutput("log_processamento")
                )
            )
        )
    )
)

# ============================================================================
# 3. SERVER (LÓGICA DO SERVIDOR)
# ============================================================================

server <- function(input, output, session) {
    
    # Variáveis Reativas
    dados_processados <- reactiveVal(NULL)
    historico_numeros <- reactiveVal(NULL)
    resultados_gerados <- reactiveVal(NULL)
    log_msgs <- reactiveVal("Aguardando arquivo...")
    
    # Função para adicionar log
    add_log <- function(msg) {
        log_msgs(paste(log_msgs(), msg, sep = "\n"))
    }
    
    # 1. Carregar e Processar Arquivo
    observeEvent(input$arquivo_excel, {
        req(input$arquivo_excel)
        
        tryCatch({
            caminho <- input$arquivo_excel$datapath
            df <- read_excel(caminho)
            dados_processados(df)
            
            # Extrair lista de números
            lista_nums <- extrair_numeros(df)
            historico_numeros(lista_nums)
            
            add_log(paste("Arquivo carregado com sucesso.", nrow(df), "linhas encontradas."))
            add_log(paste("Total de concursos processados:", length(lista_nums)))
            
            showNotification("Dados carregados com sucesso!", type = "message")
            
        }, error = function(e) {
            add_log(paste("ERRO ao ler arquivo:", e$message))
            showNotification("Erro ao ler arquivo Excel.", type = "error")
        })
    })
    
    # 2. Gerar Jogos
    observeEvent(input$btn_processar, {
        req(historico_numeros())
        
        withProgress(message = 'Gerando combinações...', value = 0, {
            
            nums_list <- historico_numeros()
            
            # Verifica se usa pivô ou não
            num_base <- if(input$usar_pivo) input$num_inicial else NULL
            
            qtd_nums <- input$qtd_dezenas
            qtd_jogos <- input$qtd_jogos
            usar_filtros <- input$usar_filtros
            
            incProgress(0.2, detail = "Calculando frequências e atrasos...")
            freq_comp <- calcular_frequencias(nums_list)
            freq_rec <- analisar_padroes_recentes(nums_list)
            
            # Se houver base, calcula coocorrências dela. Se não, NULL.
            cooc <- NULL
            if (!is.null(num_base)) {
                incProgress(0.4, detail = "Analisando coocorrências do pivô...")
                cooc <- calcular_coocorrencias(nums_list, num_base)
            } else {
                incProgress(0.4, detail = "Analisando padrões globais...")
            }
            
            incProgress(0.6, detail = "Otimizando sequências...")
            
            lista_jogos <- list()
            
            for(k in 1:qtd_jogos) {
                set.seed(as.numeric(Sys.time()) + k * 1000)
                
                melhor_seq <- NULL
                tentativas <- 0
                max_tentativas <- if(usar_filtros) 100 else 1
                
                while(tentativas < max_tentativas) {
                    seq_cand <- gerar_sequencia(
                        num_base, qtd_nums, freq_comp, cooc, nums_list, freq_rec
                    )
                    
                    if (usar_filtros && qtd_nums == 6) {
                        soma <- sum(seq_cand)
                        pares <- sum(seq_cand %% 2 == 0)
                        
                        # Filtro 1: Soma típica (140-240)
                        # Filtro 2: Pares (2, 3 ou 4 pares)
                        if (soma >= 140 && soma <= 240 && pares >= 2 && pares <= 4) {
                            melhor_seq <- seq_cand
                            break # Achou um bom jogo
                        }
                    } else {
                        melhor_seq <- seq_cand
                        break # Sem filtros ou jogo > 6 numeros
                    }
                    tentativas <- tentativas + 1
                }
                
                if(is.null(melhor_seq)) melhor_seq <- seq_cand
                
                lista_jogos[[k]] <- data.frame(
                    Jogo = k,
                    Dezenas = paste(sprintf("%02d", melhor_seq), collapse = " - "),
                    Soma = sum(melhor_seq),
                    Pares = sum(melhor_seq %% 2 == 0),
                    Impares = sum(melhor_seq %% 2 != 0)
                )
            }
            
            df_final <- do.call(rbind, lista_jogos)
            resultados_gerados(df_final)
            
            incProgress(1, detail = "Concluído!")
        })
    })
    
    # 3. Outputs
    
    output$tabela_jogos <- renderDT({
        req(resultados_gerados())
        datatable(resultados_gerados(), 
                  options = list(pageLength = 10, dom = 't'),
                  rownames = FALSE) %>%
            formatStyle('Dezenas', fontWeight = 'bold', color = '#2c3e50')
    })
    
    output$stats_resumo <- renderUI({
        req(historico_numeros())
        if(input$usar_pivo) {
            tagList(
                h4("Estatísticas"),
                p(paste("Jogos gerados ancorados no pivô:", input$num_inicial))
            )
        } else {
            tagList(
                h4("Estatísticas"),
                p("Jogos gerados sem número fixo (modo aleatório estatístico).")
            )
        }
    })
    
    output$plot_freq_geral <- renderPlot({
        req(historico_numeros())
        freq <- calcular_frequencias(historico_numeros())
        top10 <- freq[order(-freq$frequencia), ][1:10, ]
        
        ggplot(top10, aes(x = reorder(factor(numero), -frequencia), y = frequencia)) +
            geom_bar(stat = "identity", fill = "#3498db") +
            labs(title = "Top 10 Mais Frequentes", 
                 x = "Dezena", y = "Frequência") +
            theme_minimal()
    })
    
    output$plot_coocorrencia <- renderPlot({
        req(historico_numeros())
        
        # Se NÃO usa pivô, mostra mensagem ou esconde
        if (!input$usar_pivo) {
            plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
            text(0, 0, "Gráfico de Pares\ndisponível apenas\ncom Pivô Ativo", cex = 1.5, col = "#7f8c8d")
            return()
        }
        
        cooc <- calcular_coocorrencias(historico_numeros(), input$num_inicial)
        df_cooc <- data.frame(numero = as.numeric(names(cooc)), freq = cooc)
        top10_cooc <- df_cooc[order(-df_cooc$freq), ][1:10, ]
        
        ggplot(top10_cooc, aes(x = reorder(factor(numero), -freq), y = freq)) +
            geom_bar(stat = "identity", fill = "#e74c3c") +
            labs(title = paste("Pares com o Nº", input$num_inicial), 
                 x = "Dezena Parceira", y = "Vezes Juntos") +
            theme_minimal()
    })
    
    output$plot_atrasos <- renderPlot({
        req(historico_numeros())
        atrasos <- calcular_atrasos(historico_numeros())
        df_atrasos <- data.frame(numero = as.numeric(names(atrasos)), atraso = atrasos)
        top10_atrasos <- df_atrasos[order(-df_atrasos$atraso), ][1:10, ]
        
        ggplot(top10_atrasos, aes(x = reorder(factor(numero), -atraso), y = atraso)) +
            geom_bar(stat = "identity", fill = "#f39c12") +
            labs(title = "Top 10 Dezenas Mais Atrasadas (Dias Frias)", 
                 x = "Dezena", y = "Concursos sem sair") +
            geom_text(aes(label=atraso), vjust=-0.3, size=3.5) +
            theme_minimal()
    })
    
    output$info_arquivo <- renderText({
        req(input$arquivo_excel)
        paste("Arquivo carregado:", input$arquivo_excel$name)
    })
    
    output$log_processamento <- renderText({
        log_msgs()
    })
}

# ============================================================================
# 4. RODAR APP
# ============================================================================
shinyApp(ui = ui, server = server)