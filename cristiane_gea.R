# Chamada Pública 095/2020 - processo de seleção - prova
# Candidata: Cristiane Gea
# Objetivo: Desenvolver um script em R que realize as tarefas descritas nos itens 1 a 3.

# Item 1: Obtenção de dados

#   Download dos arquivos "sinopse_estatistica_da_educacao_basica_XXXX.zip"
#   Download do arquivo "divulgacao_anos_iniciais_municipios_2019.zip"

# Item 2: Estruturação dos dados

#   Objetivo: criação de um dataframe municipal em formato de painel.

#   Variáveis contidas no dataframe:
#     (i)   código do município
#     (ii)  nome do município
#     (iii) ano
#     (iv)  razão estudante-professor nos anos inicias do ensino fundamental nas escolas municipais
#     (v)   IDEB das escolas municipais para os anos iniciais do ensino fundamental

#   Diante da quantidade de abas que cada arquivo "sinopse_estatistica_da_educacao_basica_XXXX.xlsx",
#   separei as abas correspondentes aos dados dos alunos e dos professores do ensino fundamental (abas
#   "Ensino Fundamental 1.14" e "Ensino Fundamental 2.13", respectivamente) em arquivo a parte.
#   Para o total de matrículas dos alunos e dos professores dos anos iniciais do ensino fundamental,
#   foram consideradas a coluna "Municipal" dos "Anos Iniciais" (para cada caso).

#   Glossário das variáveis:
#     (i)   ano => ano no qual os dados estão inseridos
#     (ii)  codigo_municipio => código dos municípios
#     (iii) nome_municipio => nome dos municípios
#     (iv)  total_matricula_aluno => total de matrícula dos alunos da rede municipal
#     (v)   total_matricula_professor => total de matrícula dos professor da rede municipal
#     (vi)  ideb => nota ideb

#   Obs.: os valores faltantes estão como NA.

#  Importação dos dados

  library(readxl)
  
  dados2007 <- read_excel(
    "C:/Users/GEA/Google Drive/Ipea/consolidado_sinopse_estatistica_da_educacao_basica.xlsx",
    sheet = "2007")
  
  dados2008 <- read_excel(
    "C:/Users/GEA/Google Drive/Ipea/consolidado_sinopse_estatistica_da_educacao_basica.xlsx",
    sheet = "2008")
  
  dados2009 <- read_excel(
    "C:/Users/GEA/Google Drive/Ipea/consolidado_sinopse_estatistica_da_educacao_basica.xlsx",
    sheet = "2009")
  
  dados2010 <- read_excel(
    "C:/Users/GEA/Google Drive/Ipea/consolidado_sinopse_estatistica_da_educacao_basica.xlsx",
    sheet = "2010")
  
  dados2011 <- read_excel(
    "C:/Users/GEA/Google Drive/Ipea/consolidado_sinopse_estatistica_da_educacao_basica.xlsx",
    sheet = "2011")
  
  dados2012 <- read_excel(
    "C:/Users/GEA/Google Drive/Ipea/consolidado_sinopse_estatistica_da_educacao_basica.xlsx",
    sheet = "2012")
  
  dados2013 <- read_excel(
    "C:/Users/GEA/Google Drive/Ipea/consolidado_sinopse_estatistica_da_educacao_basica.xlsx",
    sheet = "2013")
  
  dados2014 <- read_excel(
    "C:/Users/GEA/Google Drive/Ipea/consolidado_sinopse_estatistica_da_educacao_basica.xlsx",
    sheet = "2014")
  
  dados2015 <- read_excel(
    "C:/Users/GEA/Google Drive/Ipea/consolidado_sinopse_estatistica_da_educacao_basica.xlsx",
    sheet = "2015")
  
  dados2016 <- read_excel(
    "C:/Users/GEA/Google Drive/Ipea/consolidado_sinopse_estatistica_da_educacao_basica.xlsx",
    sheet = "2016")
 
  dados2017 <- read_excel(
    "C:/Users/GEA/Google Drive/Ipea/consolidado_sinopse_estatistica_da_educacao_basica.xlsx",
    sheet = "2017")
  
  dados2018 <- read_excel(
    "C:/Users/GEA/Google Drive/Ipea/consolidado_sinopse_estatistica_da_educacao_basica.xlsx",
    sheet = "2018")
  
  dados2019 <- read_excel(
    "C:/Users/GEA/Google Drive/Ipea/consolidado_sinopse_estatistica_da_educacao_basica.xlsx",
    sheet = "2019")

# COncatenação dos dados
  dados <- rbind(dados2007, dados2008, dados2009, dados2010, dados2011, dados2012, dados2013,
                 dados2014, dados2015, dados2016, dados2017, dados2018, dados2019)
  
  View(dados)
  
# Remoção dos dados ausentes
  dados2 <- na.omit(dados)
  
  View(dados2)
  
  # Criação da variável "estudante-professor"
  dados2$estudante_professor <- (dados2$total_matricula_aluno/dados2$total_matricula_professor)
  
  View(dados2)
  
# Criação do dataframe "ensino_fundamental"
  ensino_fundamental <- data.frame(dados2)
  
  # Salvando data.frame com o nome "ensino_fundamental.Rdata"
  
    ensino_fundamental <- 'C:/Users/GEA/Google Drive/Ipea/ensino_fundamental.RData'
  
    saveRDS(object = ensino_fundamental, file = ensino_fundamental)
    
# Item 3: Análise dos dados
  
  # (a) Criação de uma função chamada "grafico_ideb"
        library(ggplot2)
        library(gridExtra)
        library(dplyr)
        library(addinslist) 
          
        grafico_ideb = function(dados2, ano) {
          for(i in 2007:2019) {
            ano <- filter(dados2, ano == i)
            plot <- ggplot(ano, mapping = aes(x = estudante_professor, y = ideb)) +
              geom_point(colour = "navyblue", size = 3.5,
                         shape = 22, fill = alpha("blue", 0.4)) 
            (gg <- ggplotly(plot))
          }
          return(plot)
        }
    
  # (b) Elaboração de gráficos da relação razão estudante-professor x nota ideb para os anos 2007 e 2019
        library(ggplot2)
        library(dplyr)
        library(addinslist)
        
        # Ano 2007
          ano <- filter(dados2, ano == 2007)
          plot <- ggplot(ano, mapping = aes(x = ideb, y = estudante_professor)) +
            geom_point(colour = "navyblue", size = 3.5,
                       shape = 22, fill = alpha("blue", 0.4)) 
          (gg <- ggplotly(plot))
          
        # Ano 2019
          ano <- filter(dados2, ano == 2019)
          plot <- ggplot(ano, mapping = aes(x = ideb, y = estudante_professor)) +
            geom_point(colour = "navyblue", size = 3.5,
                       shape = 22, fill = alpha("blue", 0.4)) 
          (gg <- ggplotly(plot))

  # (c) Criação um dataframe com estatísticas descritivas por ano
        dt2007 <- dados2[1:4985,]
        dt2009 <- dados2[4986:9944,]
        dt2011 <- dados2[9945:15080,]
        dt2013 <- dados2[15081:20047,]
        dt2015 <- dados2[20048:25034,]
        dt2017 <- dados2[25035:30235,]
        dt2019 <- dados2[30236:35373,]
    
        # ano 2007
          media_ideb_07 <- mean(dt2007$ideb)
          sd_ideb_07 <- sd(dt2007$ideb)
          mediana_ideb_07 <- median(dt2007$ideb)        
          min_ideb_07 <- min(dt2007$ideb)      
          max_ideb_07 <- max(dt2007$ideb)
          
          media_ratio_07 <- mean(dt2007$estudante_professor)
          sd_ratio_07 <- sd(dt2007$estudante_professor)
          mediana_ratio_07 <- median(dt2007$estudante_professor)        
          min_ratio_07 <- min(dt2007$estudante_professor)      
          max_ratio_07 <- max(dt2007$estudante_professor)
          
        # ano 2009
          media_ideb_09 <- mean(dt2009$ideb)
          sd_ideb_09 <- sd(dt2009$ideb)
          mediana_ideb_09 <- median(dt2009$ideb)        
          min_ideb_09 <- min(dt2009$ideb)      
          max_ideb_09 <- max(dt2009$ideb)
          
          media_ratio_09 <- mean(dt2009$estudante_professor)
          sd_ratio_09 <- sd(dt2009$estudante_professor)
          mediana_ratio_09 <- median(dt2009$estudante_professor)        
          min_ratio_09 <- min(dt2009$estudante_professor)      
          max_ratio_09 <- max(dt2009$estudante_professor)
          
        # ano 2011
          media_ideb_11 <- mean(dt2011$ideb)
          sd_ideb_11 <- sd(dt2011$ideb)
          mediana_ideb_11 <- median(dt2011$ideb)        
          min_ideb_11 <- min(dt2011$ideb)      
          max_ideb_11 <- max(dt2011$ideb)
          
          media_ratio_11 <- mean(dt2011$estudante_professor)
          sd_ratio_11 <- sd(dt2011$estudante_professor)
          mediana_ratio_11 <- median(dt2011$estudante_professor)        
          min_ratio_11 <- min(dt2011$estudante_professor)      
          max_ratio_11 <- max(dt2011$estudante_professor)
          
        # ano 2013
          media_ideb_13 <- mean(dt2013$ideb)
          sd_ideb_13 <- sd(dt2013$ideb)
          mediana_ideb_13 <- median(dt2013$ideb)        
          min_ideb_13 <- min(dt2013$ideb)      
          max_ideb_13 <- max(dt2013$ideb)
          
          media_ratio_13 <- mean(dt2013$estudante_professor)
          sd_ratio_13 <- sd(dt2013$estudante_professor)
          mediana_ratio_13 <- median(dt2013$estudante_professor)        
          min_ratio_13 <- min(dt2013$estudante_professor)      
          max_ratio_13 <- max(dt2013$estudante_professor)
          
        # ano 2015
          media_ideb_15 <- mean(dt2015$ideb)
          sd_ideb_15 <- sd(dt2015$ideb)
          mediana_ideb_15 <- median(dt2015$ideb)        
          min_ideb_15 <- min(dt2015$ideb)      
          max_ideb_15 <- max(dt2015$ideb)
          
          media_ratio_15 <- mean(dt2015$estudante_professor)
          sd_ratio_15 <- sd(dt2015$estudante_professor)
          mediana_ratio_15 <- median(dt2015$estudante_professor)        
          min_ratio_15 <- min(dt2015$estudante_professor)      
          max_ratio_15 <- max(dt2015$estudante_professor)
          
        # ano 2017
          media_ideb_17 <- mean(dt2017$ideb)
          sd_ideb_17 <- sd(dt2017$ideb)
          mediana_ideb_17 <- median(dt2017$ideb)        
          min_ideb_17 <- min(dt2017$ideb)      
          max_ideb_17 <- max(dt2017$ideb)
          
          media_ratio_17 <- mean(dt2017$estudante_professor)
          sd_ratio_17 <- sd(dt2017$estudante_professor)
          mediana_ratio_17 <- median(dt2017$estudante_professor)        
          min_ratio_17 <- min(dt2017$estudante_professor)      
          max_ratio_17 <- max(dt2017$estudante_professor)
          
        # ano 2019
          media_ideb_19 <- mean(dt2019$ideb)
          sd_ideb_19 <- sd(dt2019$ideb)
          mediana_ideb_19 <- median(dt2019$ideb)        
          min_ideb_19 <- min(dt2019$ideb)      
          max_ideb_19 <- max(dt2019$ideb)
          
          media_ratio_19 <- mean(dt2019$estudante_professor)
          sd_ratio_19 <- sd(dt2019$estudante_professor)
          mediana_ratio_19 <- median(dt2019$estudante_professor)        
          min_ratio_19 <- min(dt2019$estudante_professor)      
          max_ratio_19 <- max(dt2019$estudante_professor)
      
        # Criação do dataframe
          estatistica_descritiva <- data.frame(
            ano = c(2007, 2009, 2011, 2013, 2015, 2017, 2019),
            media_ideb = c(media_ideb_07, media_ideb_09, media_ideb_11, media_ideb_13, media_ideb_15,
                           media_ideb_17, media_ideb_19),
            media_ratio = c(media_ratio_07, media_ratio_09, media_ratio_11, media_ratio_13,
                            media_ratio_15, media_ratio_17, media_ratio_19),
            sd_ideb = c(sd_ideb_07, sd_ideb_09, sd_ideb_11, sd_ideb_13, sd_ideb_15, sd_ideb_17,
                        sd_ideb_19),
            sd_ratio = c(sd_ratio_07, sd_ratio_09, sd_ratio_11, sd_ratio_13, sd_ratio_15,
                         sd_ratio_17, sd_ratio_19),
            mediana_ideb = c(mediana_ideb_07, mediana_ideb_09, mediana_ideb_11, mediana_ideb_13,
                             mediana_ideb_15, mediana_ideb_17, mediana_ideb_19),
            mediana_ratio = c(mediana_ratio_07, mediana_ratio_09, mediana_ratio_11, mediana_ratio_13,
                              mediana_ratio_15, mediana_ratio_17, mediana_ratio_19),
            min_ideb = c(min_ideb_07, min_ideb_09, min_ideb_11, min_ideb_13, min_ideb_15, min_ideb_17,
                         min_ideb_19),
            min_ratio = c(min_ratio_07, min_ratio_09, min_ratio_11, min_ratio_13, min_ratio_15,
                          min_ratio_17, min_ratio_19),
            max_ideb = c(max_ideb_07, max_ideb_09, max_ideb_11, max_ideb_13, max_ideb_15, max_ideb_17,
                         max_ideb_19),
            max_ratio = c(max_ratio_07, max_ratio_09, max_ratio_11, max_ratio_13, max_ratio_15,
                          max_ratio_17, max_ratio_19),
            stringsAsFactors = FALSE
            )
          View(estatistica_descritiva)
    