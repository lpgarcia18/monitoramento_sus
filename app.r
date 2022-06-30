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



########################################################################################### 
#Módulos 
###########################################################################################
source("modulo_producao_filtro.R", encoding = "UTF-8")
#source("modulo_serie_temporal.R", encoding = "UTF-8")

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
				 menuItem("Análise e Projeção", tabName = "producao_analise"))),
			# menuItem("Prestadores", icon = icon("dashboard"), startExpanded = F,  
			# 	 menuItem("Tipo", tabName = "prestadores_tipo"),
			# 	 menuItem("Natureza", tabName = "prestadores_natureza"),
			# 	 menuItem("Faltas em Consultas", tabName = "faltas_cons_esf")),
			# menuItem("Profissionais", icon = icon("dashboard"), startExpanded = F),
			# menuItem("Pactuação Interfederativa", icon = icon("dashboard"), startExpanded = F),
			# menuItem("Orçamento e Finanças", icon = icon("dashboard"), startExpanded = F),
			menuItem("Instruções", icon = icon("question-circle"),
				 href = "https://github.com/analisededadosemsaudefloripa/saladesituacao/wiki/Instru%C3%A7%C3%B5es-para-Utiliza%C3%A7%C3%A3o-das-Salas-de-Situa%C3%A7%C3%A3o-em-Sa%C3%BAde"),
			menuItem("Código-fonte", icon = icon("code"), 
				 href = "https://github.com/analisededadosemsaudefloripa/saladesituacao/blob/master/aps"),
			menuItem("Licença de Uso", icon = icon("cc"), 
				 href = "https://github.com/analisededadosemsaudefloripa/saladesituacao/blob/atencao_primaria/LICENSE")
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
			tabItem(tabName = "apresentacao", h2("Monitoramento do RAG e dos RDQAs"),
				fluidRow(
					mainPanel(
						p("")
					)
				),
				fluidRow(
					#box(title = "Mapa", status = "primary", width=12, solidHeader = T, collapsible = T, leafletOutput("localizacao_cs", height = 400))
				)
			),
			###########################################################################################
			#Apresentação
			###########################################################################################

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

			)#,
		# 	
		# 	########################################################################################### 
		# 	#Produção Análise e Projeções
		# 	###########################################################################################
		# 	#Tipo de Unidade
			# tabItem(tabName = "producao_tipo_unidade", h2("Projeção de Demanda por Procedimentos de Saúde"),
			# 	tabPanel("Produção Total - Séries Temporais",
			# 		 serie_temporal_UI(id = "producao_tipo_unidade_serie")
			# 	)
			# )
		# 	#Profissão
		# 	
		# 	
		# 	#Procedimento
		# 	
		# 	
		# 	########################################################################################### 
		# 	#Produção Atenção Psicossocial
		# 	###########################################################################################

		# 	########################################################################################### 
		# 	#Produção Atenção Especializada
		# 	###########################################################################################
		# 	
		# 	########################################################################################### 
		# 	#Produção Atenção Farmacêutica
		# 	###########################################################################################
		# 	
		# 	########################################################################################### 
		# 	#Produção Vigilância em Saúde
		# 	###########################################################################################
		# 	
		# 	########################################################################################### 
		# 	#Produção Outros
		# 	###########################################################################################
		# 	
		# 	########################################################################################### 
		# 	#Prestadores Tipo
		# 	###########################################################################################
		# 	
		# 	########################################################################################### 
		# 	#Prestadores Natureza
		# 	###########################################################################################
		# 	
		# 	########################################################################################### 
		# 	#Profissionais
		# 	###########################################################################################
		# 	
		# 	########################################################################################### 
		# 	#Pactuação Interfederativa
		# 	###########################################################################################
		# 	
		# 	########################################################################################### 
		# 	#Orçamento e Finanças
		# 	###########################################################################################
		# 	
		# 	
		# 	
		# 	
		)
	)
)


########################################################################################### 
server <- function(input, output, session) {
	###########################################################################################
	
	########################################################################################### 
	#Apresentação
	###########################################################################################
# 	tabItem(tabName = "apresentacao", h2("Monitoramento e Avaliação"),
# 		fluidRow(
# 			mainPanel(
# 				p("A atenção primária é uma estratégia de organização da atenção à saúde voltada para responder de forma regionalizada, contínua e sistematizada à maior parte das necessidades de saúde de uma população."),
# 				p("No Brasil, seu principal efetor são as equipes de saúde da família."),
# 				p("Florianópolis destaca-se nacionalmente por ser a única capital com 100% de cobertura de atenção primária e por possuir os melhores resultados no PMAQ."),
# 				p("Ela é composta por Centros de Saúde, dividios em distritos sanitários. Cada
#                                       Centro de Saúde, por sua vez, é formado por uma ou mais Equipes de Saúde da Família.
#                                       Informações básicas sobre os centros de saúde podem ser vistas no mapa a seguir.")
# 			)
# 		)
# 		#,
# 		# fluidRow(
# 		# 	box(title = "Mapa", status = "primary", width=12, solidHeader = T, collapsible = T, leafletOutput("localizacao_cs", height = 400))
# 		# )
# 	)
	########################################################################################### 
	#Apresentação
	###########################################################################################
	
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
	#Avaliação
	# serie_temporal_SERV(id = "producao_tipo_unidade_serie", banco = producao_cubo)
		
	########################################################################################### 
	#Produção Avaliação
	###########################################################################################

	########################################################################################### 
	#Produção Atenção Básica
	###########################################################################################
	
	########################################################################################### 
	#Produção Atenção Psicossocial
	###########################################################################################
		
	########################################################################################### 
	#Produção Atenção Especializada
	###########################################################################################
	
	########################################################################################### 
	#Produção Atenção Farmacêutica
	###########################################################################################
	
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