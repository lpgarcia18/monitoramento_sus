setwd(getwd())

library(readr)
########################################################################################### 
#Dados 
###########################################################################################
producao_cubo <- read_csv("dados/producao_cubo.csv", 
			  locale = locale(encoding = "WINDOWS-1252"))


#Dados básicos
# Extraindo tipo de unidade, procedimento e profissao -------------------------------------
tipo_unidade_ch <- sort(unique(as.character(producao_cubo$tipo_unidade))) 
names(tipo_unidade_ch) <- tipo_unidade_ch 

tipo_atendimento_ch <- sort(unique(as.character(producao_cubo$tipo_atendimento))) #está se usando a descrição de procedimento aqui (nomenclatura da celk) 
names(tipo_atendimento_ch) <- tipo_atendimento_ch 

profissao_ch <- sort(unique(as.character(producao_cubo$profissao)))  
names(profissao_ch) <- profissao_ch 

faixa_etaria_ch <- sort(unique(as.character(producao_cubo$faixa_etaria)))  
names(faixa_etaria_ch) <- faixa_etaria_ch 

sexo_ch <- sort(unique(as.character(producao_cubo$sexo)))  
names(sexo_ch) <- sexo_ch 

data_ch <- unique(as.character(producao_cubo$dt_atendimento))
names(data_ch) <- data_ch 