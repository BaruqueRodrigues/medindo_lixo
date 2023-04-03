### Pegar os dados
library(tidyverse)


# Importando dados vd -----------------------------------------------------

## Pegando dados por ano
islu_consolidado_2016 <- readxl::read_excel("data/ISLU_2016_consolidado_v2.xlsx") %>% 
  select(-DS_2016) %>% 
  janitor::clean_names()

islu_correto <- read_delim("data/islu_co/DS_ISLU_2016.csv", locale = locale(encoding = "latin1")) %>% 
  janitor::clean_names() %>% 
  select(municipio, estado, ds_2016)

islu_consolidado_2016<- left_join(islu_consolidado_2016,
                                  islu_correto,
                                  
                                  by = c("municipio" = "municipio",
                                         "uf" = "estado"))

islu_2016 <- islu_consolidado_2016 %>% 
 
  mutate_all(as.character) %>% 
  pivot_longer(
    c(-municipio,-uf),
    names_to = "variavel",
    values_to = "valor"
  ) %>% 
  separate(
    variavel, 
  into = c("variavel", "ano"),
  sep = "_"
  ) %>% 
  pivot_wider(
    names_from = variavel,
    values_from = valor
  ) 
#2017

islu_consolidado_2017 <- readxl::read_excel("data/ISLU_2017_consolidado_v2.xlsx") %>% 
  select(-4) %>% 
  janitor::clean_names()

islu_correto <- read_delim("data/islu_co/DS_ISLU_2017.csv", locale = locale(encoding = "latin1")) %>% 
  janitor::clean_names() %>% 
  select(1,2, 7)

islu_consolidado_2017<- left_join(islu_consolidado_2017,
                                  islu_correto,
                                  
                                  by = c("municipio" = "municipio",
                                         "uf" = "estado"))
islu_2017 <- islu_consolidado_2017 %>% 
  mutate_all(as.character) %>%  
  pivot_longer(
    c(-municipio,-uf),
    names_to = "variavel",
    values_to = "valor"
  ) %>% 
  separate(
    variavel, 
    into = c("variavel", "ano"),
    sep = "_"
  ) %>% 
  pivot_wider(
    names_from = variavel,
    values_from = valor
  ) 

## 2018
islu_consolidado_2018 <- readxl::read_excel("data/ISLU_2018_consolidado_v2.xlsx") %>% 
  select(-4) %>% 
  janitor::clean_names()

islu_correto <- read_delim("data/islu_co/DS_ISLU_2018.csv", locale = locale(encoding = "latin1")) %>% 
  janitor::clean_names() %>% 
  select(1,2, 7)

islu_consolidado_2018<- left_join(islu_consolidado_2018,
                                  islu_correto,
                                  
                                  by = c("municipio" = "municipio",
                                         "uf" = "estado"))
islu_2018 <- islu_consolidado_2018%>%
  mutate_all(as.character) %>% 
  pivot_longer(
    c(-municipio,-uf),
    names_to = "variavel",
    values_to = "valor"
  ) %>% 
  separate(
    variavel, 
    into = c("variavel", "ano"),
    sep = "_"
  ) %>% 
  pivot_wider(
    names_from = variavel,
    values_from = valor
  )
## 2019
islu_consolidado_2019 <- readxl::read_excel("data/ISLU_2019_consolidado_v2.xlsx") %>% 
  select(-4) %>% 
  janitor::clean_names()

islu_correto <- read_delim("data/islu_co/DS_ISLU_2019.csv", locale = locale(encoding = "latin1")) %>% 
  janitor::clean_names() %>% 
  select(1,2, 7)

islu_consolidado_2019<- left_join(islu_consolidado_2019,
                                  islu_correto,
                                  
                                  by = c("municipio" = "municipio",
                                         "uf" = "estado"))



islu_2019 <- islu_consolidado_2019 %>% 
  mutate_all(as.character) %>% 
  pivot_longer(
    c(-municipio,-uf),
    names_to = "variavel",
    values_to = "valor"
  ) %>% 
  separate(
    variavel, 
    into = c("variavel", "ano"),
    sep = "_"
  ) %>% 
  pivot_wider(
    names_from = variavel,
    values_from = valor
  ) 

islu_2020 <- readxl::read_excel("data/ISLU_2020_consolidado_v2.xlsx") %>% 
  janitor::clean_names() %>% 
  mutate_all(as.character) %>% 
  pivot_longer(
    c(-municipio,-uf),
    names_to = "variavel",
    values_to = "valor"
  ) %>% 
  separate(
    variavel, 
    into = c("variavel", "ano"),
    sep = "_"
  ) %>% 
  pivot_wider(
    names_from = variavel,
    values_from = valor
  ) %>% 
  unnest() 

islu_2021 <- readxl::read_excel("data/ISLU_2021_consolidado_v2.xlsx") %>% 
  janitor::clean_names() %>% 
  mutate_all(as.character) %>% 
  pivot_longer(
    c(-municipio,-uf),
    names_to = "variavel",
    values_to = "valor"
  ) %>% 
  separate(
    variavel, 
    into = c("variavel", "ano"),
    sep = "_"
  ) %>% 
  pivot_wider(
    names_from = variavel,
    values_from = valor
  )

islu_2022 <- readxl::read_excel("data/ISLU_2022_consolidado_v2.xlsx") %>% 
  janitor::clean_names() %>% 
  mutate_all(as.character) %>% 
  pivot_longer(
    c(-municipio,-uf),
    names_to = "variavel",
    values_to = "valor"
  ) %>% 
  separate(
    variavel, 
    into = c("variavel", "ano"),
    sep = "_"
  ) %>% 
  pivot_wider(
    names_from = variavel,
    values_from = valor
  ) 

islu <- bind_rows(islu_2016,islu_2017,islu_2018,
                  islu_2019,islu_2020,islu_2021,
                  islu_2022)


  
  
dados_controles <- readxl::read_excel("data/dataset.xlsx",
                                      sheet = 2) %>% 
  janitor::clean_names() %>% 
  select(-c(1:2)) %>% 
    rename(plano_municipal=dummy_po048_o_municipio_possui_plano_municipal_de_gestao_integrada_de_residuos_solidos_pmgirs_conforme_a_lei_no_12_305_2010_que_trata_da_politica_nacional_de_residuos_solidos,
           data_aprovacao = po049_quando_foi_aprovado) %>% 
  #removendo colunas sujas
  select(-ano_de_referencia, -plano_municipal) %>% 
  #selecionando valores unicos
  unique() %>%  
  #agrupando a analis e a nível de municipio
  group_by(municipio, estado) %>% 
  #encontrando o valor da data de aprovacao
  mutate(data_aprovacao = min(data_aprovacao, na.rm = TRUE)) %>% 
  unique() %>% 
  #calculando o ano de aprovação
  mutate(ano_aprovacao = ifelse(is.infinite(data_aprovacao), 0, lubridate::year(data_aprovacao))) %>% 
  select(-data_aprovacao) %>% 
  ungroup()



islu %>% 
  left_join(dados_controles) %>% 
  mutate(
         tem_plano = ifelse(ano_aprovacao > 0, "sim", "não"),
         #islu mede o indicador para 2 anos atrás
         ano = as.numeric(ano),
         ano = ano-2,
         tempo_tratamento = ifelse(tem_plano == "sim", ano-ano_aprovacao, 0),
         islu = str_replace_all(islu, fixed(","), fixed(".")) %>% as.numeric()
  ) %>% 
  arrange(municipio) %>% 
  mutate(across(de:ds, ~str_replace_all(., fixed(","), fixed(".")) %>% 
                  as.numeric()
                ),
         islu = ifelse(ano >= 2019, (.31*de+0.24*ds+0.222*dr+0.229*di)/4, islu)) %>% 
  write_rds("data/islu.rds")
  

## Control Data

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
  mutate(islu = ifelse(ano %in% c(2019, 2020), islu*4, islu)) %>% 
  write_rds("data/islu.rds")

