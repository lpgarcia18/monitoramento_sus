setwd(getwd())

#######################################################################
##DescriÃ§Ã£o
#######################################################################
#'Esse módulo permite ao usuário selecionar dados do tipo de unidade de produção 
#'e exportar a série temporal destes dados
#'O banco de dados precisa conter 
#'CODMUNRES (Código do IBGE do município de residência)
#'CODMUNNASC (Código do IBGE do município de ocorrência)
#'CID
#'Com dados desagregados a partir de 2006
#'
#'O módulo deve ser utilizado como INPUT para outros módulos.
#'
#######################################################################
##Pacotes
#######################################################################
require(shiny)
require(readr)
require(tidyverse)
require(zoo)
require(plotly)

#######################################################################
##UI
#######################################################################
#A função UI deve entra como argumento de um tabPanel

producao_UI <- function(id, banco, tipo_atendimento_choice, tipo_unidade_choice, profissao_choice, sexo_choice, faixa_etaria_choice, data_choice){
	ns <- NS(id)
	tagList(
		fluidPage(
			hr(),
			fluidRow(
				column(12,
				       column(4,
				              #Selecionando data de inicio
				              selectInput(inputId = ns("dt_inicio"),
				              	    label = "Selecione o Mês de Início",
				              	    choices = data_choice,
				              	    selected = min(as.Date(data_choice, format = "%Y-%m-%d"), na.rm = T))
				       ),
				       column(4,
				              #Selecionando data final
				              selectInput(inputId = ns("dt_final"),
				              	    label = "Selecione o Mês Final",
				              	    choices = data_choice,
				              	    selected = max(as.Date(data_choice, format = "%Y-%m-%d"), na.rm = T))
				       )
				)
			),
			br(),
			fluidRow(
				valueBoxOutput(outputId = ns("prod_tot"), width = 3),
				valueBoxOutput(outputId = ns("prod_aps"), width = 3),
				valueBoxOutput(outputId = ns("prod_upa"), width = 3),
				valueBoxOutput(outputId = ns("prod_policlinica"), width = 3),
				valueBoxOutput(outputId = ns("prod_caps"), width = 3),
				valueBoxOutput(outputId = ns("prod_alo"), width = 3),
				valueBoxOutput(outputId = ns("prod_cedra"), width = 3),
				valueBoxOutput(outputId = ns("prod_outros"), width = 3)
			),
			br(),
			fluidRow(
				tabPanel("Gráficos", plotlyOutput(outputId = ns("producao_aps_graf"), width = "100%", height = 660)),
			),
			br(),
			hr(),
			br(),
			fluidRow(
				column(12,
				       column(4,
				              #Selecionando tipo de unidade
				              selectInput(inputId = ns("tipo_unidade_select"),
				              	    label = "Selecione o Tipo de Unidade",
				              	    choices = tipo_unidade_choice,
				              	    selected = "Total", 
				              	    multiple = TRUE)
				       ),
				       column(4,
				              #Selecionando Profissão
				              selectInput(inputId = ns("profissao_select"),
				              	    label = "Selecione a Profissão",
				              	    choices = profissao_choice,
				              	    selected = "Total", 
				              	    multiple = TRUE)
				       ),
				       column(4,
				              #Selecionando tipo de atendimento
				              selectInput(inputId = ns("tipo_atendimento_select"),
				              	    label = "Selecione o Tipo de Atendimento",
				              	    choices = tipo_atendimento_choice,
				              	    selected = "Total", 
				              	    multiple = TRUE)
				       ),
				       column(4),
				       column(4,
				              #Selecionando sexo
				              selectInput(inputId = ns("sexo_select"),
				              	    label = "Selecione o Sexo dos Pacientes",
				              	    choices = sexo_choice,
				              	    selected = "Total", 
				              	    multiple = TRUE)
				       ),
				       column(4,
				              #Selecionando faixa etaria
				              selectInput(inputId = ns("faixa_etaria_select"),
				              	    label = "Selecione a Faixa Etaria dos Pacientes",
				              	    choices = faixa_etaria_choice,
				              	    selected = "Total", 
				              	    multiple = TRUE)
				       )
				)
			),
			br(),
			fluidRow(
				tabsetPanel(type = "tabs",
					    tabPanel("Gráficos", plotlyOutput(outputId = ns("producao_graf"), width = "100%", height = 660)),
					    tabPanel("Tabelas", DT::dataTableOutput(ns("producao_tab"), width = "100%", height = 660))
				)
			)
		)
	)
}

#################################################################################
#Server
#################################################################################
producao_SERV <- function(id, banco){
	moduleServer(
		id,
		function(input, output, session)
		{
			
			startdate_sh <- reactive({
				startdate <- as.Date(input$dt_inicio, format = "%Y-%m-%d")
				startdate
			})
			
			enddate_sh <- reactive({
				enddate <- as.Date(input$dt_final, format = "%Y-%m-%d")
				enddate
			})
			
			
			banco_tot <- reactive({
				producao_select  <- banco  %>%
					subset(banco$tipo_unidade == "Total" &
					       	banco$tipo_atendimento == "Total" &
					       	banco$profissao == "Total" &
					       	banco$faixa_etaria == "Total" &
					       	banco$sexo == "Total"  &
					       	banco$mes_atendimento >=  startdate_sh() &
					       	banco$mes_atendimento <=  enddate_sh()
					)
				
				
			})
			
			
			banco_aps <- reactive({
				producao_select  <- banco  %>%
					subset(banco$tipo_unidade == "Centro de Saúde" &
					       	banco$tipo_atendimento == "Total" &
					       	banco$profissao == "Total" &
					       	banco$faixa_etaria == "Total" &
					       	banco$sexo == "Total" &
					       	banco$mes_atendimento >=  startdate_sh() &
					       	banco$mes_atendimento <=  enddate_sh()
					)
				
				producao_select
			})
			
			banco_alo <- reactive({
				
				producao_select  <- banco  %>%
					subset(banco$tipo_unidade == "Alô Saúde" &
					       	banco$tipo_atendimento == "Total" &
					       	banco$profissao == "Total" &
					       	banco$faixa_etaria == "Total" &
					       	banco$sexo == "Total" &
					       	banco$mes_atendimento >=  startdate_sh() &
					       	banco$mes_atendimento <=  enddate_sh()
					)
				
				producao_select
			})
			
			banco_caps <- reactive({
				
				producao_select  <- banco  %>%
					subset(banco$tipo_unidade == "CAPS" &
					       	banco$tipo_atendimento == "Total" &
					       	banco$profissao == "Total" &
					       	banco$faixa_etaria == "Total" &
					       	banco$sexo == "Total" &
					       	banco$mes_atendimento >=  startdate_sh() &
					       	banco$mes_atendimento <=  enddate_sh()
					)
				
				producao_select
			})
			
			banco_cedra <- reactive({
				
				producao_select  <- banco  %>%
					subset(banco$tipo_unidade == "CEDRA" &
					       	banco$tipo_atendimento == "Total" &
					       	banco$profissao == "Total" &
					       	banco$faixa_etaria == "Total" &
					       	banco$sexo == "Total" &
					       	banco$mes_atendimento >=  startdate_sh() &
					       	banco$mes_atendimento <=  enddate_sh()
					)
				
				producao_select
			})
			
			banco_policlinica <- reactive({
				
				producao_select  <- banco  %>%
					subset(banco$tipo_unidade == "Policlínica" &
					       	banco$tipo_atendimento == "Total" &
					       	banco$profissao == "Total" &
					       	banco$faixa_etaria == "Total" &
					       	banco$sexo == "Total" &
					       	banco$mes_atendimento >=  startdate_sh() &
					       	banco$mes_atendimento <=  enddate_sh()
					)
				
				producao_select
			})
			
			banco_upa <- reactive({
				
				producao_select  <- banco  %>%
					subset(banco$tipo_unidade == "UPA" &
					       	banco$tipo_atendimento == "Total" &
					       	banco$profissao == "Total" &
					       	banco$faixa_etaria == "Total" &
					       	banco$sexo == "Total" &
					       	banco$mes_atendimento >=  startdate_sh() &
					       	banco$mes_atendimento <=  enddate_sh()
					)
				
				producao_select
			})
			
			
			banco_outros <- reactive({
				
				producao_select  <- banco  %>%
					subset(banco$tipo_unidade == "Outros" &
					       	banco$tipo_atendimento == "Total" &
					       	banco$profissao == "Total" &
					       	banco$faixa_etaria == "Total" &
					       	banco$sexo == "Total" &
					       	banco$mes_atendimento >=  startdate_sh() &
					       	banco$mes_atendimento <=  enddate_sh()
					)
				
				producao_select
			})
			
			
			banco_preparado <- reactive({
				
				producao_select  <- banco  %>%
					subset(banco$tipo_atendimento %in% input$tipo_atendimento_select &
					       	banco$tipo_unidade %in% input$tipo_unidade_select &
					       	banco$profissao %in% input$profissao_select &
					       	banco$sexo %in% input$sexo_select &
					       	banco$faixa_etaria %in% input$faixa_etaria_select  &
					       	banco$mes_atendimento >=  startdate_sh() &
					       	banco$mes_atendimento <=  enddate_sh()
					)
				producao_select
			})
			
			
			
			# Valuebox ----------------------------------------------------------------
			
			#Value Box Tipo de Unidade -----------------------------------------------
			output$prod_tot <- renderValueBox({
				tot <- banco_tot()
				tot <- sum(tot$quantidade, na.rm = T)
				valueBox(
					value = format(tot, nsmall =0, big.mark = "."),
					icon = icon('hospital', lib = 'font-awesome'), #icon("sign-in"),
					color = "blue",
					subtitle = "Total"
				)
			})
			
			output$prod_alo <- renderValueBox({
				tot <- banco_alo()
				tot <- sum(tot$quantidade, na.rm = T)
				valueBox(
					value = format(tot, nsmall =0, big.mark = "."),
					icon = icon('hospital', lib = 'font-awesome'), #icon("sign-in"),
					color = "blue",
					subtitle = "Alô Saúde"
				)
			})
			
			output$prod_aps <- renderValueBox({
				tot <- banco_aps()
				tot <- sum(tot$quantidade)
				valueBox(
					value = format(tot, nsmall =0, big.mark = "."),
					icon = icon('hospital', lib = 'font-awesome'), #icon("sign-in"),
					color = "blue",
					subtitle = "Atenção Primária"
				)
			})
			
			output$prod_caps <- renderValueBox({
				tot <- banco_caps()
				tot <- sum(tot$quantidade, na.rm = T)
				valueBox(
					value = format(tot, nsmall =0, big.mark = "."),
					icon = icon('hospital', lib = 'font-awesome'), #icon("sign-in"),
					color = "blue",
					subtitle = "CAPS"
				)
			})
			
			
			output$prod_cedra <- renderValueBox({
				tot <- banco_cedra()
				tot <- sum(tot$quantidade, na.rm = T)
				valueBox(
					value = format(tot, nsmall =0, big.mark = "."),
					icon = icon('hospital', lib = 'font-awesome'), #icon("sign-in"),
					color = "blue",
					subtitle = "CEDRA"
				)
			})
			
			output$prod_policlinica <- renderValueBox({
				tot <- banco_policlinica()
				tot <- sum(tot$quantidade, na.rm = T)
				valueBox(
					value = format(tot, nsmall =0, big.mark = "."),
					icon = icon('hospital', lib = 'font-awesome'), #icon("sign-in"),
					color = "blue",
					subtitle = "Policlínica"
				)
			})
			
			output$prod_upa <- renderValueBox({
				tot <- banco_upa()
				tot <- sum(tot$quantidade, na.rm = T)
				valueBox(
					value = format(tot, nsmall =0, big.mark = "."),
					icon = icon('hospital', lib = 'font-awesome'), #icon("sign-in"),
					color = "blue",
					subtitle = "UPA"
				)
			})
			
			output$prod_outros <- renderValueBox({
				tot <- banco_outros()
				tot <- subset(tot, tipo_unidade == "Outros")
				tot <- sum(tot$quantidade, na.rm = T)
				valueBox(
					value = format(tot, nsmall =0, big.mark = "."),
					icon = icon('hospital', lib = 'font-awesome'), #icon("sign-in"),
					color = "blue",
					subtitle = "Outros"
				)
			})
			
			# Gráfico geral  --------------------------------------------------------
			output$producao_aps_graf <- renderPlotly({
				
				graf_func <- function(banco, titulo){
					banco$mes_atendimento <- as.Date(banco$mes_atendimento, format = "%Y")
					graf <- plot_ly(banco, x = ~ mes_atendimento, y = ~quantidade)  %>%
						add_lines() %>%
						layout(showlegend = FALSE,
						       yaxis = list(title = titulo, titlefont = list(size = 12),
						       	     showgrid = F, zerolinewidth = 2, zerolinecolor = 'lightgrey', range = c(0,max(as.numeric(banco$quantidade),na.rm = T))),
						       xaxis = list(showgrid = F))
				}
				
				graf_tot <- graf_func(banco = banco_tot(), "Total/mês")
				graf_aps <- graf_func(banco = banco_aps(), "APS/mês")
				graf_upa <- graf_func(banco = banco_upa(), "UPAs/mês")
				graf_policlinica <- graf_func(banco = banco_policlinica(), "Policlínicas/mês")
				graf_caps <- graf_func(banco = banco_caps(), "CAPS/mês")
				graf_cedra <- graf_func(banco = banco_cedra(), "Cedra/mês")
				graf_alo <- graf_func(banco = banco_alo(), "Alô Saúde/mês")
				graf_outros <- graf_func(banco = banco_outros(), "Outros/mês")
				
				subplot1 <- subplot(graf_tot, graf_aps, nrows=1, shareY = F, shareX = T, titleY = T, titleX = F)
				subplot2 <- subplot(graf_upa, graf_policlinica, graf_caps, nrows=1, shareY = F, shareX = T, titleY = T, titleX = F)
				subplot3 <- subplot(graf_alo, graf_cedra, graf_outros, nrows=1, shareY = F, shareX = T, titleY = T, titleX = F)
				
				subplot(subplot1, subplot2, subplot3,
					nrows=3, heights = c(0.3, 0.3, 0.3), margin = 0.05, shareY = F, shareX = T, titleY = T, titleX = F)
				
			})
			
			# Gráfico e tabela análise --------------------------------------------------------
			output$producao_graf <- renderPlotly({
				banco_graf <- banco_preparado()
				banco_graf_sum <- banco_graf %>%
					dplyr::group_by(mes_atendimento) %>%
					dplyr::summarise(quantidade = sum(quantidade, na.rm = T))
				graf <- ggplot(banco_graf_sum, aes(as.Date(banco_graf_sum$mes_atendimento), as.numeric(banco_graf_sum$quantidade)))+
					geom_line()+
					xlab("Ano") +
					ylab("")+
					ggtitle("Série Temporal - Mensal")+
					theme_bw()
				graf_plotly <- ggplotly(graf,)
				graf_plotly_fim <- graf_plotly %>% style(showlegend = T)
				graf_plotly_fim
				
			})
			
			output$producao_tab <- DT::renderDataTable({
				tabela_real <- banco_preparado() %>% dplyr::select(tipo_unidade,
										   profissao,
										   tipo_atendimento,
										   sexo,
										   faixa_etaria,
										   dt_atendimento,
										   quantidade)
				tabela_real$dt_atendimento <- as.yearmon(tabela_real$dt_atendimento) %>% as.character()
				tabela_real <- tabela_real[order(tabela_real$dt_atendimento),]
				names(tabela_real) <- c("Tipo de Unidade",
							"Profissão",
							"Tipo de Atendimento",
							"Sexo",
							"Faixa Etária",
							"Data de Atendimento",
							"Número de Atendimentos")
				tabela_real
			}, extensions = 'Buttons',
			options = list(
				"dom" = 'T<"clear">lBfrtip',
				buttons = list('copy', 'csv','excel', 'pdf', 'print'),
				pageLength = 100,
				searching = FALSE))
		})
	
}