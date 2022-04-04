########## CONFIGURACOES INICIAIS ##########

#unlink("C:\\Users\\paulo\\Documents\\R\\win-library\\4.1/00LOCK", recursive = TRUE)

if(!require(pacman)){install.packages("pacman")}
pacman::p_load(gganimate, hrbrthemes, ggplot2, dplyr, tidyverse, lubridate, ggpmisc, readr)

setwd("G:\\Meu Drive\\Estudos\\Ciencias Economicas\\UFRN\\Observatorio da Conjuntura Economica do RN\\Sidra_IBGE\\Dados")

desalentados_percentual <- read.csv2("desalentados_percentual.csv", header = T)
ocupados_mil <- read.csv2("ocupados_mil.csv", header = T)
rendimento_medio_reais <- read.csv2("rendimento_medio_reais.csv", header = T)
forca_de_trabalho_mil <- read.csv2("forca_de_trabalho_mil.csv", header = T)
desocupados_mil <- read.csv2("desocupados_mil.csv", header = T)

########## Tratamento das tabelas ##########

names(desalentados_percentual) <- c("Localidade", "Trimestre", "Taxa_desalentados")
names(ocupados_mil) <- c("Localidade", "Trimestre", "Ocupados_mil")
names(rendimento_medio_reais) <- c("Localidade", "Trimestre", "Rendimento_medio")
names(desocupados_mil) <- c("Localidade", "Trimestre", "Desocupados_mil")
names(forca_de_trabalho_mil) <- c("Localidade", "Trimestre", "Forca_de_trabalho_mil")

########## Descobrindo a porcentagem de ocupacao e desocupacao e o total de desalentados  ##########

localidade <- data.frame(desalentados_percentual$Localidade)
trimestre <- data.frame(desalentados_percentual$Trimestre)

ocupados_percentual <- data.frame(localidade, trimestre, (ocupados_mil[, 3]/forca_de_trabalho_mil[, 3])*100)
desocupados_percentual <- data.frame(localidade, trimestre, (desocupados_mil[, 3]/forca_de_trabalho_mil[, 3])*100)
desalentados_mil <- data.frame(localidade, trimestre, (desalentados_percentual[, 3]/100)*forca_de_trabalho_mil[, 3])

names(ocupados_percentual) <- c("Localidade", "Trimestre", "Ocupacao_percentual")
names(desocupados_percentual) <- c("Localidade", "Trimestre", "Desocupacao_percentual")
names(desalentados_mil) <- c("Localidade", "Trimestre", "Desalentados_mil")

########## Separando as tabelas  ##########

ocupados_regiao_percentual <- data.frame(ocupados_percentual[17:96, 1:3])
desocupados_regiao_percentual <- data.frame(desocupados_percentual[17:96, 1:3])
desalentados_regiao_percentual <- data.frame(desalentados_percentual[17:96, 1:3])

ocupados_norte_percentual <- data.frame(ocupados_percentual[97:208, 1:3])
ocupados_nordeste_percentual <- data.frame(ocupados_percentual[209:352, 1:3])
ocupados_sudeste_percentual <- data.frame(ocupados_percentual[353:416, 1:3])
ocupados_sul_percentual <- data.frame(ocupados_percentual[417:464, 1:3])
ocupados_centrooeste_percentual <- data.frame(ocupados_percentual[465:528, 1:3])

desocupados_norte_percentual <- data.frame(desocupados_percentual[97:208, 1:3])
desocupados_nordeste_percentual <- data.frame(desocupados_percentual[209:352, 1:3])
desocupados_sudeste_percentual <- data.frame(desocupados_percentual[353:416, 1:3])
desocupados_sul_percentual <- data.frame(desocupados_percentual[417:464, 1:3])
desocupados_centrooeste_percentual <- data.frame(desocupados_percentual[465:528, 1:3])

desalentados_norte_percentual <- data.frame(desalentados_percentual[97:208, 1:3])
desalentados_nordeste_percentual <- data.frame(desalentados_percentual[209:352, 1:3])
desalentados_sudeste_percentual <- data.frame(desalentados_percentual[353:416, 1:3])
desalentados_sul_percentual <- data.frame(desalentados_percentual[417:464, 1:3])
desalentados_centrooeste_percentual <- data.frame(desalentados_percentual[465:528, 1:3])


########## Plotando os graficos  ##########

ggplot(data = ocupados_regiao_percentual, aes(x = Trimestre, y = as.numeric(Ocupacao_percentual), group = Localidade, color = Localidade))+
  geom_line() +
  geom_point() +
  ggtitle(label = "Percentual de Ocupados por Macrorregiao", ) +
  geom_vline(xintercept= "2020/1",
             linetype=2, colour="black", size=1.2) +
  ylim(80, 95) +
  ylab(label = "Ocupacao  percentual" ) +
  xlab(label = "Ano/Trimestre") +
  labs(caption = "A linha tracejada corresponde ao inicio das restricoes da pandemia do covid-19") +
  theme_update()

ggplot(data = desocupados_regiao_percentual, aes(x = Trimestre, y = Desocupacao_percentual, group = Localidade, color = Localidade))+
  geom_line() +
  geom_point() +
  ggtitle(label = "Percentual de Desocupados por Macrorregiao", ) +
  geom_vline(xintercept= "2020/1",
             linetype=2, colour="black", size=1.2) +
  ylim(0, 20) +
  ylab(label = "Desocupacao  percentual" ) +
  xlab(label = "Ano/Trimestre") +
  labs(caption = "A linha tracejada corresponde ao inicio das restricoes da pandemia do covid-19") +
  theme_update()

ggplot(data = desalentados_regiao_percentual, aes(x = Trimestre, y = Taxa_desalentados, group = Localidade, color = Localidade))+
  geom_line() +
  geom_point() +
  ggtitle(label = "Percentual de Desalentados por Macrorregiao" ) +
  geom_vline(xintercept= "2020/1",
             linetype=2, colour="black", size=1.2) +
  ylim(0, 15) +
  ylab(label = "Desalentados  percentual" ) +
  xlab(label = "Ano/Trimestre") +
  labs(caption = "A linha tracejada corresponde ao inicio das restricoes da pandemia do covid-19") +
  theme_update()

ggplot(data = ocupados_norte_percentual, aes(x = Trimestre, y = Ocupacao_percentual, group = Localidade, color = Localidade))+
  geom_line() +
  geom_point() +
  ggtitle(label = "Percentual de Ocupados na Macrorregiao Norte" ) +
  geom_vline(xintercept= "2020/1",
             linetype=2, colour="black", size=1.2) +
  ylim(75, 95) +
  ylab(label = "Ocupados  percentual" ) +
  xlab(label = "Ano/Trimestre") +
  labs(caption = "A linha tracejada corresponde ao inicio das restricoes da pandemia do covid-19") +
  theme_update()

ggplot(data = ocupados_nordeste_percentual, aes(x = Trimestre, y = Ocupacao_percentual, group = Localidade, color = Localidade))+
  geom_line() +
  geom_point() +
  ggtitle(label = "Percentual de Ocupados na Macrorregiao Nordeste" ) +
  geom_vline(xintercept= "2020/1",
             linetype=2, colour="black", size=1.2) +
  ylim(75, 95) +
  ylab(label = "Ocupados  percentual" ) +
  xlab(label = "Ano/Trimestre") +
  labs(caption = "A linha tracejada corresponde ao inicio das restricoes da pandemia do covid-19") +
  theme_update()

ggplot(data = ocupados_sul_percentual, aes(x = Trimestre, y = Ocupacao_percentual, group = Localidade, color = Localidade))+
  geom_line() +
  geom_point() +
  ggtitle(label = "Percentual de Ocupados na Macrorregiao Sul" ) +
  geom_vline(xintercept= "2020/1",
             linetype=2, colour="black", size=1.2) +
  ylim(75, 95) +
  ylab(label = "Ocupados  percentual" ) +
  xlab(label = "Ano/Trimestre") +
  labs(caption = "A linha tracejada corresponde ao inicio das restricoes da pandemia do covid-19") +
  theme_update()

ggplot(data = ocupados_sudeste_percentual, aes(x = Trimestre, y = Ocupacao_percentual, group = Localidade, color = Localidade))+
  geom_line() +
  geom_point() +
  ggtitle(label = "Percentual de Ocupados na Macrorregiao Sudeste" ) +
  geom_vline(xintercept= "2020/1",
             linetype=2, colour="black", size=1.2) +
  ylim(75, 95) +
  ylab(label = "Ocupados  percentual" ) +
  xlab(label = "Ano/Trimestre") +
  labs(caption = "A linha tracejada corresponde ao inicio das restricoes da pandemia do covid-19") +
  theme_update()

ggplot(data = ocupados_centrooeste_percentual, aes(x = Trimestre, y = Ocupacao_percentual, group = Localidade, color = Localidade))+
  geom_line() +
  geom_point() +
  ggtitle(label = "Percentual de Ocupados na Macrorregiao Centro-Oeste" ) +
  geom_vline(xintercept= "2020/1",
             linetype=2, colour="black", size=1.2) +
  ylim(75, 95) +
  ylab(label = "Ocupados  percentual" ) +
  xlab(label = "Ano/Trimestre") +
  labs(caption = "A linha tracejada corresponde ao inicio das restricoes da pandemia do covid-19") +
  theme_update()

ggplot(data = desocupados_norte_percentual, aes(x = Trimestre, y = Desocupacao_percentual, group = Localidade, color = Localidade))+
  geom_line() +
  geom_point() +
  ggtitle(label = "Percentual de Desocupados na Macrorregiao Norte" ) +
  geom_vline(xintercept= "2020/1",
             linetype=2, colour="black", size=1.2) +
  ylim(0, 25) +
  ylab(label = "Desocupados  percentual" ) +
  xlab(label = "Ano/Trimestre") +
  labs(caption = "A linha tracejada corresponde ao inicio das restricoes da pandemia do covid-19") +
  theme_update()

ggplot(data = desocupados_nordeste_percentual, aes(x = Trimestre, y = Desocupacao_percentual, group = Localidade, color = Localidade))+
  geom_line() +
  geom_point() +
  ggtitle(label = "Percentual de Desocupados na Macrorregiao Nordeste" ) +
  geom_vline(xintercept= "2020/1",
             linetype=2, colour="black", size=1.2) +
  ylim(0, 25) +
  ylab(label = "Desocupados  percentual" ) +
  xlab(label = "Ano/Trimestre") +
  labs(caption = "A linha tracejada corresponde ao inicio das restricoes da pandemia do covid-19") +
  theme_update()

ggplot(data = desocupados_sul_percentual, aes(x = Trimestre, y = Desocupacao_percentual, group = Localidade, color = Localidade))+
  geom_line() +
  geom_point() +
  ggtitle(label = "Percentual de Desocupados na Macrorregiao Nordeste" ) +
  geom_vline(xintercept= "2020/1",
             linetype=2, colour="black", size=1.2) +
  ylim(0, 25) +
  ylab(label = "Desocupados  percentual" ) +
  xlab(label = "Ano/Trimestre") +
  labs(caption = "A linha tracejada corresponde ao inicio das restricoes da pandemia do covid-19") +
  theme_update()
