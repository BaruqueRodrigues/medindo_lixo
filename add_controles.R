## Control Data

library(tidyverse)
#carrega os dados
dados <- read_rds("data/islu.rds")

#pega o diretorio dos dados
diretorio <- paste0("data/indicadores para covariadas/",
       list.files("data/indicadores para covariadas/", pattern = "Agrupamento"),
       sep = "") 
#importa os dados dos controles
controles <- diretorio[1:6] %>% 
  map_dfr(~read_delim(., 
                       delim = ";", escape_double = FALSE, 
                       locale = locale(encoding = "UTF-16LE",
                                       decimal_mark = ",", 
                                       grouping_mark = "."),
                       trim_ws = TRUE) %>% 
            janitor::clean_names() %>% 
            dplyr::slice(-c(1:27)) %>% 
            select(-33))

# pega os controles de 2020
controles_2020 <- read_delim(diretorio[7], 
                             delim = ";", escape_double = FALSE, 
                             locale = locale(encoding = "WINDOWS-1252",
                                             decimal_mark = ",", 
                                             grouping_mark = "."),
                             trim_ws = TRUE) %>% 
  janitor::clean_names() %>% 
  dplyr::slice(-c(1:27)) %>% 
  select(-33)

#junta os dados de controles
controles <-bind_rows(controles,controles_2020)

#remove o dataset controles_2020 da staging area
rm("controles_2020")

dados <- dados %>% 
  left_join(
  controles %>% 
  select(municipio, estado, ano_de_referencia,
         prestador, natureza_juridica,
         pop_urbana = pop_urb_populacao_urbana_do_municipio_fonte_ibge,
         pop_atendida_coleta_seletiva = cs050_populacao_urbana_do_municipio_atendida_com_a_coleta_seletiva_do_tipo_porta_a_porta_executada_pela_prefeitura_ou_slu,
         despesa_servicos_manejo = fn220_despesa_total_com_servicos_de_manejo_de_rsu_antigo_campo_ge007,
         #data_aprovacao_plano = po049_quando_foi_aprovado,
         n_trabalhadores_coleta_seletiva = tb014_quantidade_de_trabalhadores_de_agentes_privados_envolvidos_nos_servicos_de_manejo_de_rsu_antigo_campo_ge016
         ) %>% 
  mutate(ano_de_referencia = as.numeric(ano_de_referencia),
         pop_atendida_coleta_seletiva = replace_na(pop_atendida_coleta_seletiva, 0),
         #data_aprovacao_plano = replace_na(data_aprovacao_plano, "Ausente")
         ),
  by = c("municipio" = "municipio",
         "uf" = "estado",
         "ano" = "ano_de_referencia")
  )


dados %>% 
  unique() %>% 
  mutate(islu = islu*4) %>% 
  write_rds("data/islu.rds")


