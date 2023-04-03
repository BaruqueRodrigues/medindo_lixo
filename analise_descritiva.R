#Análise Descritiva

library(tidyverse)

#modelo DiD
# y= score islu,
# tratamento = ano de aprovação do plano

dados <- read_rds("data/islu.rds")

dados %>% glimpse()

# Visualização do Modelo
dados %>%
   lm(formula = islu~ano:tem_plano,
     data = .) %>%
  summary()

#Modelos 2
#modelo DiD
#y = dimensão_r
#tratamento = ano de aprovação do plano
#pop_urbana, pop_atendida_coleta_seletiva, 
#despesa_servicos_manejo, n_trabalhadores_coleta_seletiva 

theme_set(hrbrthemes::theme_ipsum())
dados %>% 
  lm(
    formula = dr~ano:tem_plano+pop_urbana+pop_atendida_coleta_seletiva+
      despesa_servicos_manejo+n_trabalhadores_coleta_seletiva
  ) 

  # Análise Descritiva islu ---------------------------------------------------


## Análise da distribuição islu-------------------------------------------------
library(patchwork)
# Histograma islu
dist_islu <-(dados %>% 
  ggplot(aes(
    x = islu 
  ))+ 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="lightgreen")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = .5))+
  labs(x = "Score Islu",
       y = NULL,
       title = "Distribuição Score Islu")+


# Boxplot islu
dados %>% 
  ggplot(aes(
    x = islu 
  ))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = .5))+
  labs(x = "Islu",
       title = "Boxplot Islu"))
#Histograma islu por tempo
dados %>% 
  ggplot(aes(
    x = islu 
  ))+ 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="lightgreen")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = .5))+
  labs(x = "Islu",
       title = "Distribuição Islu")+
  facet_wrap(~ano, nrow = 3)
# Boxplot por ano islu
boxplot_ano_islu <-dados %>% 
  ggplot(aes(
    x = factor(ano),
    y = islu
    
  ))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = .5))+
  labs(y = "Islu", 
       x = "Ano",
       title = "Boxplot Islu por Ano")
# Distribuição por Ano islu
dist_ano_islu <- dados %>% 
  ggplot(aes(
    y = factor(ano),
    x = islu
    
  ))+
  ggridges::geom_density_ridges()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = .5))+
  labs(y = "Ano", 
       x = "Islu",
       title = "Distribuição por Ano do Score Islu")

# Análise barra de erro islu por ano
dados %>% 
  group_by(ano) %>% 
  summarise(media_islu = mean(islu, na.rm = TRUE),
            sd_islu = sd(islu, na.rm = TRUE)) %>% 
  ggplot(aes(
    x= as.factor(ano),
    y= media_islu
  ))+
  geom_point()+
  geom_linerange(aes(ymin = media_islu-sd_islu,
                     ymax = media_islu+sd_islu))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = .5))+
  labs(y = "Islu", 
       x = "Ano",
       title = "Análise Barra de Erro Islu por Ano")



# Analise dimensão R ------------------------------------------------------
## Análise da distribuição Dimensão R-------------------------------------------------

# Histograma Dimensão R
dis_dr <- (dados %>% 
  ggplot(aes(
    x = dr 
  ))+ 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="lightgreen")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = .5))+
  labs(x = "Score Dimensão R",
       y = NULL,
       title = "Distribuição Score Dimensão R")+

# Boxplot dr
dados %>% 
  ggplot(aes(
    x = dr 
  ))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = .5))+
  labs(x = "dr",
       title = "Boxplot Dimensão R"))

#Histograma dr por tempo
dados %>% 
  ggplot(aes(
    x = dr 
  ))+ 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="lightgreen")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = .5))+
  labs(x = "Dimensão R",
       title = "Distribuição Dimensão R")+
  facet_wrap(~ano, nrow = 3)
# Boxplot por ano dr
boxplot_ano_dr <- dados %>% 
  ggplot(aes(
    x = factor(ano),
    y = dr
    
  ))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = .5))+
  labs(y = "dr", 
       x = "Ano",
       title = "Boxplot Dimensão R por Ano")
# Distribuição por Ano dr
dist_ano_dr <-dados %>% 
  ggplot(aes(
    y = factor(ano),
    x = dr
    
  ))+
  ggridges::geom_density_ridges()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = .5))+
  labs(y = "Ano", 
       x = "dr",
       title = "Distribuição por Ano do Dimensão R")

# Análise barra de erro dr por ano
dados %>% 
  group_by(ano) %>% 
  summarise(media_dr = mean(dr, na.rm = TRUE),
            sd_dr = sd(dr, na.rm = TRUE)) %>% 
  ggplot(aes(
    x= as.factor(ano),
    y= media_dr
  ))+
  geom_point()+
  geom_linerange(aes(ymin = media_dr-sd_dr,
                     ymax = media_dr+sd_dr))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = .5))+
  labs(y = "dr", 
       x = "Ano",
       title = "Análise Barra de Erro dr por Ano")

## Correção da Dimensão R por log-------------------------------
dados<-dados %>% 
  mutate(log_dr = log(dr+1)) 

# Histograma Dimensão R
dados %>% 
  ggplot(aes(
    x = log_dr 
  ))+ 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="lightgreen")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = .5))+
  labs(x = "Score Dimensão R",
       y = NULL,
       title = "Distribuição Score Dimensão R")


#Histograma log_dr por tempo
dados %>% 
  ggplot(aes(
    x = log_dr 
  ))+ 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="lightgreen")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = .5))+
  labs(x = "Dimensão R",
       title = "Distribuição Dimensão R")+
  facet_wrap(~ano, nrow = 3)

# Boxplot log_dr
dados %>% 
  ggplot(aes(
    x = log_dr 
  ))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = .5))+
  labs(x = "log_dr",
       title = "Boxplot log_dr")
# Boxplot por ano log_dr
dados %>% 
  ggplot(aes(
    x = factor(ano),
    y = log_dr
    
  ))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = .5))+
  labs(y = "log_dr", 
       x = "Ano",
       title = "Boxplot log_dr por Ano")
# Distribuição por Ano log_dr
dados %>% 
  ggplot(aes(
    y = factor(ano),
    x = log_dr
    
  ))+
  ggridges::geom_density_ridges()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = .5))+
  labs(y = "Ano", 
       x = "log_dr",
       title = "Distribuição por Ano do Score log_dr")

# Análise barra de erro log_dr por ano
dados %>% 
  group_by(ano) %>% 
  summarise(media_log_dr = mean(log_dr, na.rm = TRUE),
            sd_log_dr = sd(log_dr, na.rm = TRUE)) %>% 
  ggplot(aes(
    x= as.factor(ano),
    y= media_log_dr
  ))+
  geom_point()+
  geom_linerange(aes(ymin = media_log_dr-sd_log_dr,
                     ymax = media_log_dr+sd_log_dr))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = .5))+
  labs(y = "log_dr", 
       x = "Ano",
       title = "Análise Barra de Erro log_dr por Ano")





# Tabelas -----------------------------------------------------------------

map_dfr(c("islu", "dr"),
    ~dados %>% 
      group_by(ano) %>% 
      summarise(across(c(.x),
                       list(
                         "Mínimo" = min,
                         'Média' = mean,
                         "Mediana" = median,
                         "Desvio Padrão" = sd,
                         "Máximo" = max
                         ),
                       na.rm = TRUE
      )
      ) %>% 

      pivot_longer(
        c(2:6),
        names_to = "nome",
        values_to = "valor"
      ) %>% 
      mutate(
        nome = str_remove(nome, paste0(.x, "_")),
        fonte = .x
      ) %>% 
      pivot_wider(
        names_from = nome,
        values_from = valor
      ) 
  
) %>%
  left_join(bind_rows(
    dados %>% 
      group_by(ano) %>% 
      summarise(distribuicao = list(islu)) %>%
      mutate(fonte = "islu"),
    dados %>% 
      group_by(ano) %>% 
      summarise(distribuicao = list(dr)) %>%
      mutate(fonte = "dr")
  )
  ) %>% 
  mutate(boxplot = distribuicao) %>% 
  gt::gt() %>% 
  gt::fmt_number(columns = 3:7, decimals = 3) %>%
  gtExtras::gt_plt_dist(distribuicao) %>% 
  gtExtras::gt_plt_dist(boxplot, type = "boxplot") %>%
  gtExtras::gt_theme_538() %>% 
  gt::gtsave("tab_desc_anos.docx")
  
  
map_dfr(c("islu", "dr"),
    ~dados %>% 
      summarise(across(.x,  list(
        "Mínimo" = min,
        'Média' = mean,
        "Mediana" = median,
        "Desvio Padrão" = sd,
        "Máximo" = max
      ), na.rm = TRUE,.names = "{.fn}" )) %>% 
      mutate(fonte = .x) 
      )  %>% 
  left_join(
    bind_rows(
    dados %>% 
      summarise(distribuicao = list(islu)) %>%
      mutate(fonte = "islu"),
    dados %>% 
      summarise(distribuicao = list(dr)) %>%
      mutate(fonte = "dr")
  )
  ) %>%
  mutate(boxplot = distribuicao) %>%
  relocate(fonte, .before = `Mínimo`) %>% 
  gt::gt() %>% 
  gt::fmt_number(columns = 2:6, decimals = 3) %>% 
    gtExtras::gt_plt_dist(distribuicao) %>% 
    gtExtras::gt_plt_dist(boxplot, type = "boxplot") %>% 
  gtExtras::gt_theme_538() 
  gt::gtsave("tab_desc.docx")
  

dados %>% 
  group_by(ano) %>% 
  summarise(distribuicao = list(dr))
  

dados %>% 
  group_by(ano) %>% 
  count(ano) %>% 
  ungroup() %>% 
  ggplot(
    aes( x= ano,
         y = n)
  )+
  geom_line(size = 1.5, color = "lightgreen")+
  geom_point(size = 3, color = "green")+
  geom_text(aes(label = n), color = "black", nudge_y  = 100)+
  labs(x = NULL, 
       y= "Número de Municípios")
  
  


# Visualização script -----------------------------------------------------
(dist_islu+
dis_dr
)
((dist_ano_islu+boxplot_ano_islu)/(dist_ano_dr+boxplot_ano_dr))



