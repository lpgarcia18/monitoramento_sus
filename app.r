# Setando ambiente --------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)
setwd(getwd())

########################################################################################### 
#pacotes 
###########################################################################################
require(shiny)
require(shinydashboard)
require(tidyverse)
require(htmltools)
require(leaflet)
require(DT)
library(readr)

########################################################################################### 
#Dados
###########################################################################################
source("modulo_carregamento_dados.R", encoding = "UTF-8")
#Banco de dados para o mapa
 acesso_dat <- read_csv("dados/dados_cs.csv", 
 		       col_types = cols(DISTRITO = col_factor(levels = c("Centro","Continente", "Norte", "Sul"))))



########################################################################################### 
#Módulos 
###########################################################################################
source("modulo_producao_filtro.R", encoding = "UTF-8")
source("modulo_serie_temporal.R", encoding = "UTF-8")

########################################################################################### 
#UI
###########################################################################################
###########################################################################################
dbHeader <- dashboardHeader(title = "SMS - Florianópolis", 
			    tags$li(a(href = 'http://www.pmf.sc.gov.br/entidades/saude/index.php?cms=salas+de+situacao&menu=4&submenuid=152',
			    	  icon("power-off"),
			    	  title = "Sair"),
			    	class = "dropdown"),
			    tags$li(a(href = 'http://www.pmf.sc.gov.br/entidades/saude/index.php?cms=salas+de+situacao&menu=4&submenuid=152',
			    	  tags$img(src = 'logo_geinfo.png',
			    	  	 title = "Gerência de Inteligência e Informação", height = "30px"),
			    	  style = "padding-top:10px; padding-bottom:10px;padding-left:30px, padding-right:30px"),
			    	class = "dropdown"))


ui <- dashboardPage(
	########################################################################################### 
	dbHeader,
	########################################################################################### 
	dashboardSidebar(
		########################################################################################### 
		sidebarMenu(
			menuItem("Apresentação", tabName = "apresentacao", icon = icon("heartbeat")),
			# menuItem("Demografia", icon = icon("dashboard"), startExpanded = F,  
			# 	 menuItem("Demografia", tabName = "demografia"),
			# 	 menuItem("Poulação", tabName = "populacao"),
			# 	 menuItem("Nascidos Vivos", tabName = "nascidos_vivos")),
			# menuItem("Morbimortalidade", icon = icon("dashboard"), startExpanded = F,   
			# 	 menuItem("Agravos", tabName = "agravos"),
			# 	 menuItem("Internações", tabName = "internacoes"),
			# 	 menuItem("Óbitos", tabName = "obitos")),
			menuItem("Produção", icon = icon("dashboard"), startExpanded = F,   
				 menuItem("Descrição", tabName = "producao_descricao"),
				 menuItem("Projeção", tabName = "producao_projecao")),
			# menuItem("Prestadores", icon = icon("dashboard"), startExpanded = F,  
			# 	 menuItem("Tipo", tabName = "prestadores_tipo"),
			# 	 menuItem("Natureza", tabName = "prestadores_natureza"),
			# 	 menuItem("Faltas em Consultas", tabName = "faltas_cons_esf")),
			# menuItem("Profissionais", icon = icon("dashboard"), startExpanded = F),
			# menuItem("Pactuação Interfederativa", icon = icon("dashboard"), startExpanded = F),
			# menuItem("Orçamento e Finanças", icon = icon("dashboard"), startExpanded = F),
			menuItem("Instruções", icon = icon("question-circle"),
				 href = "https://github.com/lpgarcia18/monitoramento_sus#readme"),
			menuItem("Código-fonte", icon = icon("code"), 
				 href = "https://github.com/lpgarcia18/monitoramento_sus"),
			menuItem("Licença de Uso", icon = icon("cc"), 
				 href = "https://github.com/lpgarcia18/monitoramento_sus/blob/main/LICENSE.md")
		)
	),
	
	########################################################################################### 
	dashboardBody(
		tags$head(tags$style(HTML('
                          /* logo */
                          .skin-blue .main-header .logo {
                          background-color: rgb(255,255,255); color: rgb(14, 59, 79);
                          font-weight: bold;font-size: 20px;text-align: Right;
                          }

                          /* logo when hovered */

                          .skin-blue .main-header .logo:hover {
                          background-color: rgb(255,255,255);
                          }


                          /* navbar (rest of the header) */
                          .skin-blue .main-header .navbar {
                          background-color: rgb(255,255,255);
                          }

                          /* main sidebar */
                          .skin-blue .main-sidebar {
                          background-color: rgb(14, 59, 79);
                          }
                          

                          /* active selected tab in the sidebarmenu */
                          .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                          background-color: rgb(14, 59, 79);
                          color: rgb(255,255,255);font-weight: bold;font-size: 18px;
                          }

                          /* other links in the sidebarmenu */
                          .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                          background-color: rgb(14, 59, 79);
                          color: rgb(255,255,255);font-weight: bold;
                          }

                          /* other links in the sidebarmenu when hovered */
                          .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                          background-color: rgb(147,181,198);color: rgb(14, 59, 79);font-weight: bold;
                          }

                          /* toggle button color  */
                          .skin-blue .main-header .navbar .sidebar-toggle{
                          background-color: rgb(255,255,255);color:rgb(14, 59, 79);
                          }

                          /* toggle button when hovered  */
                          .skin-blue .main-header .navbar .sidebar-toggle:hover{
                          background-color: rgb(147,181,198);color:rgb(14, 59, 79);
                          }
                          
                          /* body */
                          .content-wrapper, .right-side {
                          background-color: rgb(147,181,198);
                         

#                           '))),
		tags$style(".small-box.bg-blue { background-color: rgb(181, 80, 12) !important; color: rgb(255,255,255) !important; };"
		),
		tags$style(".fa-check {color:#B5500C}"),
		tags$style(".fa-check-double {color:#B5500C}"),
		########################################################################################### 
		#Dash
		###########################################################################################
		tabItems(
			###########################################################################################
			#Apresentação
			##########################################################################################
			tabItem(tabName = "apresentacao", h2("Monitoramento e Avaliação Estratégicos - RAG e RDQAS"),
				hr(),
				hr(),
				fluidPage(
					fluidRow(
						column(12,
							p("O Relatório Anual de Gestão (RAG) é o instrumento de
								gestão que permite
								ao gestor do SUS apresentar os resultados
								alcançados com a execução da
								Programação Anual de Saúde (PAS) e
								orienta eventuais redirecionamentos que
								se fizerem necessários no Plano de Saúde."),
							p(),
							p(),
							p("O Relatório Detalhado do Quadrimestre Anterior, por sua vez, é um 
							        instrumento de monitoramento e acompanhamento da execução da PAS  
							        e deve ser apresentado pelo gestor do SUS até o final dos meses de maio, 
							        setembro e fevereiro, em audiência pública à Câmara de Vereadores de Florianópolis."),
							p(),
							p(),
							p("Desta forma, esses instrumentos permitem tanto o acompanhamento da execução
							        do que está planejado quando a melhoria continuada do planejamento em busca
								# de melhores resultados para a saúde e melhor utilização dos recursos públicos."),
							p(),
							p(),
							p("Essa plataforma, desenvolvida pela Gerência de Inteligência e Informação da 
								Secretaria de Saúde de Florianópolis, tem como objetivos: subsidiar o Conselho Municipal
								de Saúde, o Prefeito, os Secretários e os Diretores da Secretaria com informações para 
								a tomada de decisão; e servir à transparência pública.
								")
							 )
						),
					
					hr(),
					fluidRow(
						column(12,
							box(title = "Centros de Saúde de Florianópolis", status = "primary", solidHeader = T, collapsible = T, width=12, leafletOutput("localizacao_cs", height = 400))
						)
					)
				)
				
			),

			###########################################################################################
			#População
			###########################################################################################

			###########################################################################################
			#Nascidos vivos
			###########################################################################################

			###########################################################################################
			#Agravos
			###########################################################################################

			###########################################################################################
			#Internações
			###########################################################################################

			###########################################################################################
			#Óbitos
			###########################################################################################

			###########################################################################################
			#Produção Descrição
			###########################################################################################
			#Checkbox
			tabItem(tabName = "producao_descricao", h2("Descrição da Produção de Serviços de Saúde"),
				tabPanel("Produção",
						producao_UI(id = "producao_descr",
				 		 	     banco = producao_cubo,
				 		 	     tipo_atendimento_choice = tipo_atendimento_ch,
				 		 	     tipo_unidade_choice = tipo_unidade_ch,
				 		 	     profissao_choice = profissao_ch,
							     sexo_choice = sexo_ch,
							     faixa_etaria_choice = faixa_etaria_ch,
							     data_choice = data_ch)
				)

			),
			###########################################################################################
			#Produção Projeções
			###########################################################################################
			tabItem(tabName = "producao_projecao", h2("Projeção de Demanda por Procedimentos de Saúde"),
				tabPanel("Séries Temporais",
					 serie_temporal_UI(id = "producao_projecao_serie",
					 		  banco = producao_cubo,
					 		  tipo_atendimento_choice = tipo_atendimento_ch,
					 		  tipo_unidade_choice = tipo_unidade_ch,
					 		  profissao_choice = profissao_ch,
					 		  sexo_choice = sexo_ch,
					 		  faixa_etaria_choice = faixa_etaria_ch)
				)
			)
			###########################################################################################
			#Produção Vigilância em Saúde
			###########################################################################################

			###########################################################################################
			#Produção Outros
			###########################################################################################

			###########################################################################################
			#Prestadores Tipo
			###########################################################################################

			###########################################################################################
			#Prestadores Natureza
			###########################################################################################

			###########################################################################################
			#Profissionais
			###########################################################################################

			###########################################################################################
			#Pactuação Interfederativa
			###########################################################################################

			###########################################################################################
			#Orçamento e Finanças
			###########################################################################################




		)
	)
)


########################################################################################### 
server <- function(input, output, session) {
	###########################################################################################
	
	########################################################################################### 
	#Apresentação
	###########################################################################################
	output$localizacao_cs<- renderLeaflet({
		leaflet(data = acesso_dat) %>% addTiles() %>%
			addMarkers(lng = ~X, lat = ~Y, 
				   popup = ~htmlEscape(DESCRICAO))
	})
	########################################################################################### 
	#População
	###########################################################################################
	
	########################################################################################### 
	#Nascidos vivos
	###########################################################################################
	
	########################################################################################### 
	#Agravos
	###########################################################################################
	
	########################################################################################### 
	#Internações
	###########################################################################################
	
	########################################################################################### 
	#Óbitos
	###########################################################################################

	########################################################################################### 
	#Produção 
	###########################################################################################
	#Desrição
	producao_SERV(id = "producao_descr", banco = producao_cubo)
	#Projeção
	serie_temporal_SERV(id = "producao_projecao_serie", banco = producao_cubo)
		
	########################################################################################### 
	#Produção Vigilância em Saúde
	###########################################################################################
	
	########################################################################################### 
	#Produção Outros
	###########################################################################################
	
	########################################################################################### 
	#Prestadores Tipo
	###########################################################################################
	
	########################################################################################### 
	#Prestadores Natureza
	###########################################################################################
	
	########################################################################################### 
	#Profissionais
	###########################################################################################
	
	########################################################################################### 
	#Pactuação Interfederativa
	###########################################################################################
	
	########################################################################################### 
	#Orçamento e Finanças
	###########################################################################################
}

###########################################################################################
#Aplicação
###########################################################################################
shinyApp(ui, server)