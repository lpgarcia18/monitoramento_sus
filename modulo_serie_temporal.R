setwd(getwd())

#######################################################################
##Descrição
#######################################################################
#'Esse módulo permite ao usuário receber uma série temporal, realizar transformação box-cox e escolher um tempo para predição
#'mostrando os dados usados, a predição, o sumário do modelo escolhido pelo sistema (o sistema escolhe entre modelos arima
#'e ets, o com o menor Mean Squared Error - MSE), o diagnóstico e os dados.
#'
#######################################################################
##Pacotes
#######################################################################
require(shiny)
require(readr)
require(tidyverse)
require(zoo)
require(fpp2)
require(DT)


#######################################################################
##UI
#######################################################################
#A função UI deve entra como argumento de um tabPanel
serie_temporal_UI <- function(id, banco, tipo_atendimento_choice, tipo_unidade_choice, profissao_choice, sexo_choice, faixa_etaria_choice){
	ns <- NS(id)
	tagList(
		fluidPage(
			sidebarLayout(
				sidebarPanel(
					#Selecionando lambda
					textInput(inputId = ns("lambda_banco"), 
						  label = "Transformação de Box-Cox(lambda):",
						  value = "NULO"),
					helpText("Se nenhuma transformação for necessária,",
						 "manter Lambda = NULO, ou seja, sem transforação.",
						 "Lambda = 0 produz uma transformação logarítimica."),
					#Selecionando período de previsão
					numericInput(inputId = ns("periodo_prev_banco"), 
						     label = "Período desejado para previsão:",
						     value = 4, 
						     min = 0)
				),
				# Série temporal 
				mainPanel(
					fluidRow(
						column(12,
						       column(4,
						              #Selecionando tipo de unidade
						              selectInput(inputId = ns("tipo_unidade_select_proj"),
						              	    label = "Selecione o Tipo de Unidade",
						              	    choices = tipo_unidade_choice,
						              	    selected = "Total", 
						              	    multiple = TRUE)
						       ),
						       column(4,
						              #Selecionando Profissão
						              selectInput(inputId = ns("profissao_select_proj"),
						              	    label = "Selecione a Profissão",
						              	    choices = profissao_choice,
						              	    selected = "Total", 
						              	    multiple = TRUE)
						       ),
						       column(4,
						              #Selecionando tipo de atendimento
						              selectInput(inputId = ns("tipo_atendimento_select_proj"),
						              	    label = "Selecione o Tipo de Atendimento",
						              	    choices = tipo_atendimento_choice,
						              	    selected = "Total", 
						              	    multiple = TRUE)
						       ),
						       column(4),
						       column(4,
						              #Selecionando sexo
						              selectInput(inputId = ns("sexo_select_proj"),
						              	    label = "Selecione o Sexo dos Pacientes",
						              	    choices = sexo_choice,
						              	    selected = "Total", 
						              	    multiple = TRUE)
						       ),
						       column(4,
						              #Selecionando faixa etaria
						              selectInput(inputId = ns("faixa_etaria_select_proj"),
						              	    label = "Selecione a Faixa Etaria dos Pacientes",
						              	    choices = faixa_etaria_choice,
						              	    selected = "Total", 
						              	    multiple = TRUE)
						       )
						)
					),
					tabsetPanel(type = "tabs",
						    #Gráfico da série temporal
						    tabPanel("Previsão", plotOutput(outputId = ns("serie_banco"), width = "100%", height = 330),
						    	 verbatimTextOutput(ns("summary_previsao_banco"))),
						    tabPanel("Decomposição", plotOutput(outputId = ns("decomposicao_banco"), width = "100%", height = 660),
						    	 plotOutput(outputId = ns("sazonal_banco"), width = "100%", height = 330)),
						    tabPanel("Diagnóstico", plotOutput(outputId = ns("residuos_banco"), width = "100%", height = 660)),
						    tabPanel("Dados de Produção",DT::dataTableOutput(ns("dados_real"), width = "100%", height = 660)),
						    tabPanel("Previsão de Demanda",DT::dataTableOutput(ns("dados_prev"), width = "100%", height = 660))
					)
				)
			)
		)
	)
}

#################################################################################
#Server
#################################################################################

serie_temporal_SERV <- function(id, banco){
	moduleServer(
		id,
		function(input, output, session)
		{
			
			banco_preparado_proj <- reactive({
				
				producao_select  <- banco  %>%
					subset(banco$tipo_atendimento %in% input$tipo_atendimento_select_proj &
					       	banco$tipo_unidade %in% input$tipo_unidade_select_proj &
					       	banco$profissao %in% input$profissao_select_proj &
					       	banco$sexo %in% input$sexo_select_proj &
					       	banco$faixa_etaria %in% input$faixa_etaria_select_proj 
					)
				producao_select
			})
			
			
			inicio <- reactive({
				a <- banco_preparado_proj()$dt_atendimento
				a <- min(a)
				a
			})
			
			banco_pre <- reactive({         
				banco_prep <- banco_preparado_proj() %>%
					dplyr::group_by(dt_atendimento) %>%
					dplyr::summarise(quantidade = sum(quantidade, na.rm = T))
				banco_prep <- banco_prep$quantidade
				banco_prep <- ts(banco_prep[-1],start = inicio(), frequency = 12)
				banco_prep 
			})        
			
			
			
			lambda <-reactive({
				req(input$lambda_banco)
			})     
			
			
			periodo_prev_banco <-reactive({
				req(input$periodo_prev_banco)
			})
			
			
			
			banco_prev <-reactive({
				
				#Função para ETS
				ifelse(input$lambda_banco == "NULO",
				       funcets <- ets(banco_pre(), lambda = NULL),
				       funcets <- ets(banco_pre(), lambda = input$lambda_banco))
				fets <- function(x, h) {
					forecast(funcets, h = 1)
				}
				
				##Função para ARIMA
				ifelse(input$lambda_banco == "NULO",
				       funcarima<- auto.arima(banco_pre(), lambda = NULL),
				       funcarima<- auto.arima(banco_pre(), lambda = input$lambda_banco))
				farima <- function(x, h) {
					forecast(funcarima, h = 1)
				}
				
				## Compute CV errors for ETS as e1
				e1 <- tsCV(banco_pre(), fets, h=1)
				
				## Compute CV errors for ARIMA as e2
				e2 <- tsCV(banco_pre(), farima, h=1)
				
				## Find MSE of each model class
				g<-mean(e1^2, na.rm=TRUE)
				h<-mean(e2^2, na.rm=TRUE)
				
				ifelse(g > h, fit_banco <- funcets , fit_banco <- funcarima)
				"Intervalo de Confiança"
				forecast(fit_banco, periodo_prev_banco(), level = c(80,95))
			})
			
			#Gráfico com previsão
			output$serie_banco <- renderPlot({
				autoplot(banco_prev())+
					xlab("Ano") +
					ylab("")+
					ggtitle("Série Temporal - Trimestral")
			})
			
			#Gráfico dos resíduos
			output$residuos_banco <- renderPlot({
				checkresiduals(banco_prev())
			})
			
			
			#Sumário
			output$summary_previsao_banco <- renderPrint({
				summary(banco_prev())
				checkresiduals(banco_prev())
			})
			
			
			#Gráfico dos resíduos
			output$residuos_banco <- renderPlot({
				checkresiduals(banco_prev())
			})
			
			#Gráfico de decomposição 
			output$decomposicao_banco <- renderPlot({
				##Decomposição por STL
				stl(banco_pre(), s.window="periodic", robust=TRUE) %>% autoplot()
			})
			
			
			#Gráfico de sazolnalidade 
			output$sazonal_banco <- renderPlot({
				ggsubseriesplot(banco_pre())+
					ylab("Quantidade")
			})
			
			#Tabela de Dados
			
			output$dados_real <- DT::renderDataTable({
				      tabela_real <- banco_preparado_proj() %>%
				      	dplyr::select(
				      		tipo_unidade,
				      		profissao,
				      		tipo_atendimento,
				      		sexo,
				      		faixa_etaria,
				      		dt_atendimento,
				      		quantidade
				      	)
				      
				      names(tabela_real) <- c(
				      			"Tipo de Unidade",
				      			"Profissão",
				      			"Tipo de Atendimento",
				      			"Sexo",
				      			"Faixa Etária",
				      			"Mês de Atendimento",
				      			"Número de Atendimentos"
				      			)
				      tabela_real
			}, extensions = 'Buttons',
			options = list(
				"dom" = 'T<"clear">lBfrtip',
				buttons = list('copy', 'csv','excel', 'pdf', 'print'),
				pageLength = 100,
				searching = FALSE))
			
			
			dados_fim <- reactive({
				VALOR <- as.data.frame(banco_prev())
			})
			
			
			output$dados_prev <- DT::renderDataTable({
				dados_fim()
				}, extensions = 'Buttons',
				options = list(
					"dom" = 'T<"clear">lBfrtip',
					buttons = list('copy', 'csv','excel', 'pdf', 'print'),
					pageLength = 100,
					searching = FALSE))
			
			
		})
}
		