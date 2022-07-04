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

faixa_etaria_ch <- c("Total", 
		     "MENOR 6 MESES", 
		     "MAIOR OU IGUAL A 01 ANO E MENOR QUE 05 ANOS",   
		     "MAIOR OU IGUAL A 05 ANOS E MENOR QUE 10 ANOS",
		     "MAIOR OU IGUAL A 06 MESES E MENOR QUE 01 ANO",
		     "MAIOR OU IGUAL A 10 ANOS E MENOR QUE 15 ANOS",
		     "MAIOR OU IGUAL A 15 ANOS E MENOR QUE 20 ANOS",
		     "MAIOR OU IGUAL A 20 ANOS E MENOR QUE 25 ANOS",
		     "MAIOR OU IGUAL A 25 ANOS E MENOR QUE 30 ANOS",
		     "MAIOR OU IGUAL A 30 ANOS E MENOR QUE 35 ANOS",
		     "MAIOR OU IGUAL A 35 ANOS E MENOR QUE 40 ANOS",
		     "MAIOR OU IGUAL A 40 ANOS E MENOR QUE 45 ANOS", 
		     "MAIOR OU IGUAL A 45 ANOS E MENOR QUE 50 ANOS",
		     "MAIOR OU IGUAL A 50 ANOS E MENOR QUE 55 ANOS",
		     "MAIOR OU IGUAL A 55 ANOS E MENOR QUE 60 ANOS", 
		     "MAIOR OU IGUAL A 60 ANOS E MENOR QUE 55 ANOS",
		     "MAIOR OU IGUAL A 65 ANOS E MENOR QUE 70 ANOS",
		     "MAIOR OU IGUAL A 70 ANOS E MENOR QUE 75 ANOS", 
		     "MAIOR OU IGUAL A 75 ANOS E MENOR QUE 80 ANOS",
		     "MAIOR OU IGUAL A 80 ANOS E MENOR QUE 85 ANOS",
		     "MAIOR OU IGUAL A 85 ANOS E MENOR QUE 90 ANOS", 
		     "MAIOR OU IGUAL A 90 ANOS E MENOR QUE 95 ANOS",
		     "MAIOR OU IGUAL A 95 ANOS E MENOR QUE 100 ANOS",
		     "MAIOR OU IGUAL A 100 ANOS")    
names(faixa_etaria_ch) <- faixa_etaria_ch 

sexo_ch <- sort(unique(as.character(producao_cubo$sexo)))  
names(sexo_ch) <- sexo_ch 

data_ch <- unique(as.character(producao_cubo$dt_atendimento))
names(data_ch) <- data_ch 