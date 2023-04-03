library(tidyverse)
#importa as tabelas

arquivos <- list.files("data/pdf/", pattern = ".pdf")

# Executando o processo para multiplas tabelas
varias_tabelas<-map(paste0("data/pdf/", arquivos[14:15]),
                    ~tabulizer::extract_tables(file = .x,
  output = "data.frame"
)
)

#Criando multiplas tabelas
v_tabela_excel <-varias_tabelas %>% 
  tibble() %>% 
  unnest(.) %>% 
  filter(. != "") %>% 
  mutate(nome_tabela = paste0("tbl", 1:length(.))) %>% 
  group_split(nome_tabela)

#Nomeando a lista de tabelas
v_tabela_excel %>% 
  
  map(~pull(., nome_tabela)) %>% 
  map(~unique(.)) -> names(v_tabela_excel)

#Exportando a tabela pra um excel
v_tabela_excel %>% 
  map(~unnest(.)) %>% 
  writexl::write_xlsx("data/todas_tabelas_2022.xlsx")

# Executando para apenas 1 tabela

#lendo a tabela
  tabelas <-tabulizer::extract_tables(
  #Declara a localização do arquivo
  file = "data/pdf/ISLU 2022-43-165.pdf",
                                    output = "data.frame")
  
  # Criando lita com varias tabelas tabelas
tabelas <- tabelas %>% 
  tibble() %>% 
  filter(. != "") %>% 
  mutate(nome_tabela = paste0("tbl", 1:length(.))) %>% 
  group_split(nome_tabela)

#Nomeando as tabelas
tabelas%>% 
  map(~pull(., nome_tabela)) %>% 
  map(~unique(.)) -> names(tabelas)


# Exportando as tabelas para um xlsx
tabelas %>% 
    map(~unnest(.)) %>% 
 writexl::write_xlsx("data/teste_tabelas.xlsx")




v_tabela_excel[1:220] %>% 
  map(~unnest(.)) %>% 
  writexl::write_xlsx("data/todas_tabelas_1.xlsx")


v_tabela_excel %>% 
  map(~unnest(.)) %>% 
  writexl::write_xlsx("data/todas_tabelas_2.xlsx")

