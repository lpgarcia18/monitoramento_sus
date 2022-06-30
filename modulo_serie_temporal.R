setwd(getwd())

#######################################################################
##Descrição
#######################################################################
#'Esse módulo permite ao usuário receber uma série temporal, realizar transformação box-cox e escolher um tempo para predição
#'mostrando os dados usados, a predição, o sumário do modelo escolhido pelo sistema (o sistema escolhe entre modelos arima
#'e ets, o com o menor Mean Squared Error - MSE), o diagnóstico e os dados.
#'
#'O módulo tem como entrada um módulo, que deverá ser usado como INPUT_DADOS na sidebar (esse mode deve fazer a entrada de dados, a sua seleção
#'e transformação em uma série temporal; e a série temporal gerada pelo módulo. Ex: módulo = modulo_cid_local; e série teporal = banco_preparado())
#'
#'Este módulo deve ser usado em um TABPANEL
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
serie_temporal_UI <- function(id){
	ns <- NS(id)
	tagList(
		fluidPage(
			sidebarLayout(
				sidebarPanel(
					#Selecionando lambda
					textInput(inputId = ns("lambda_banco"), 
						  label = "Transformação de Box-Cox(lambda):",
						  value = "NULO"),
					helpText("Se nenhuma transformação for necessária",
						 "não intruduzir nenhum valor.",
						 "Lambda = NULO = sem transforação.",
						 "Lambda = 0 = transformação Logarítimica."),
					#Selecionando período de previsão
					numericInput(inputId = ns("periodo_prev_banco"), 
						     label = "Período desejado para previsão:",
						     value = 4, 
						     min = 0)
				),
				# Série temporal 
				mainPanel(
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
			
			banco_tipo_unidade <- reactive({
				
				producao_select  <- banco  %>%
					subset(banco$tipo_unidade == "UPA")
				producao_select
			})
	
	
			banco_preparado <- reactive({
				banco_tipo_unidade() %>% 
					dplyr::select("quantidade", "dt_atendimento")
			})
			
			inicio <- reactive({
				a <- banco_preparado()$dt_atendimento
				a <- substr(a,0,4)
				a <- as.numeric(a)
				a <- min(a)
				a
			})
			
			banco_pre <- reactive({         
				banco_prep <- banco_preparado()$quantidade 
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
				forecast(fit_banco, periodo_prev_banco(), level = c(50,80,95))
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
			
			dados_fim <- reactive({
				VALOR <- as.data.frame(banco_prev())
			})
			
			
			
			output$dados_prev <- DT::renderDataTable({
				DT::datatable(dados_fim(),
					      rownames = FALSE,
					      editable = FALSE,
					      options = list(lengthMenu = c(10,20, 40, 60, 80, 100), pageLength = 20))
			})
			
			
			
		})
}
		