#Análise DID
library(tidyverse)
#install.packages(fixest)

#modelo DiD
# y= score islu,
# tratamento = ano de aprovação do plano

dados <- read_rds("data/islu.rds")

dados %>% glimpse()

# Faremos 2 modelos.
# Modelo 1 DID simples
# MOdelo 2 DiD Tendências Paralelas

# Visualização do Modelo 1
modelo1 <-  dados %>%
  lm(formula = islu~ano:tem_plano,
     data = .)

# Modelo de Regressão
constroi_modelo <-function(modelo){
  modelo %>% 
    broom::tidy() %>% 
    mutate(across(2:5, ~round(., 3))) %>% 
    mutate_all(as.character) %>% 
    add_row(term = paste0("R² ",round(broom::glance(modelo)$r.squared, 3)),
            estimate = paste0("R² Ajustado ", round(broom::glance(modelo)$adj.r.squared, 3))
    ) %>% 
    add_row(term = paste0("N de Observações ", modelo$scores %>% nrow())) %>% 
    mutate(across(everything(),~replace_na(., ""))) %>% 
    rename(termo = term, "beta estimado" = estimate,
           "Erro Padrão" = std.error, "Estatística" = statistic,
           "P Valor" = p.value ) 
}
modelo1 %>%
 constroi_modelo() %>%
  gt::gt() %>% 
  gtExtras::gt_theme_538()
  gt::tab_header(title = "Modelo ISLU")

# Visualização do modelo

modelo1 %>% 
  sjPlot::plot_model()+
  theme_minimal()+
  labs( x= "Betas Estimados",
        title = "DiD Islu")+
  theme(plot.title = element_text(hjust = .5))


# MOdelo 2

modelo2<- fixest::feols(islu~ i(ano, tem_plano),
              data = dados) 
modelo2 %>%
  constroi_modelo() %>% 
  mutate(termo = str_replace(termo, "tem_plano::sim", "Tratamento"),
         termo = str_replace(termo, "tem_plano::não", "Controle"),
         termo = str_remove(termo, "ano::"),
         termo = str_replace(termo, ":", " "),
         termo = str_replace(termo, "\\(Intercept\\)", "Intercepto"),
         
         ) %>% 
  gt::gt() %>% 
  gtExtras::gt_theme_538() %>% 
  gt::tab_header(title = "Modelo ISLU - Tendências Paralelas")
  


#Tabela do Modelo de Regressão
modelo2 %>% 
  sjPlot::tab_model()

# Visualização do modelo
modelo2 %>% 
  sjPlot::plot_model()+
  theme_minimal()+
  labs( x= "Betas Estimados",
        title = "DiD Islu - Tendencias Paralelas")+
  theme(plot.title = element_text(hjust = .5))




# Analise da Dimensão R ---------------------------------------------------

dados %>% 
  lm(
    formula = dr~ano:tem_plano+pop_urbana+pop_atendida_coleta_seletiva+
      despesa_servicos_manejo+n_trabalhadores_coleta_seletiva
  ) %>% 
  sjPlot::plot_model()

dados %>% 
  lm(
    formula = dr~ano:tem_plano+pop_urbana+pop_atendida_coleta_seletiva+
      despesa_servicos_manejo+n_trabalhadores_coleta_seletiva
  ) %>% 
  constroi_modelo() %>% 
  tab_header(title = "Modelo Dimensão R")

fixest::feols(dr~ i(ano, tem_plano)+pop_urbana+pop_atendida_coleta_seletiva+
                despesa_servicos_manejo+n_trabalhadores_coleta_seletiva,
                        data = dados) %>%
  constroi_modelo() %>% 
  mutate(termo = str_replace(termo, "tem_plano::sim", "Tratamento"),
         termo = str_replace(termo, "tem_plano::não", "Controle"),
         termo = str_remove(termo, "ano::"),
         termo = str_replace(termo, ":", " "),
         termo = str_replace(termo, "\\(Intercept\\)", "Intercepto"),
         termo = str_replace(termo, "pop_urbana", "População Urbana"),
         termo = str_replace(termo, "pop_atendida_coleta_seletiva", "População Atendida por Coleta Seletiva"),
         termo = str_replace(termo, "despesa_servicos_manejo", "Despesas com Serviço de Manejo"),
         termo = str_replace(termo, "n_trabalhadores_coleta_seletiva", "N de Trabalhadores Coleta Seletiva"),
         
  ) %>% 
  gt::gt() %>% 
  gtExtras::gt_theme_538() %>% 
  gt::tab_header(title = "Modelo Dimensão R - Tendências Paralelas")
