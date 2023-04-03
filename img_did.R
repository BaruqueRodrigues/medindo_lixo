tibble(controle = rep(25, 10),
       tratamento = c(rep(30,5), 40, 50, 60, 70, 80),
       periodo = 2010L:2019L) %>% 
  pivot_longer(
    c(1:2)) %>% 
  ggplot(aes(x = periodo,
             y = value, color = name))+
    geom_line(size = 1.5)+
  
  scale_x_continuous(breaks = seq(2010, 2019, 1))+
  scale_y_continuous(breaks = seq(0, 80, 10))+
  geom_vline(aes(xintercept = 2014), color = "black",
             linetype = "dashed")+
  geom_text(aes(x = 2014, y =27,
                label = "Inicio do Tratamento"),
            color = "grey60")+
  geom_text(aes(x= 2012, y = 31.5,
                label = "Grupo de Tratamento"),
            color = "grey60")+
  geom_text(aes(x= 2011.9, y = 21.5,
                label = "Grupo de Controle"),
            color = "grey60")+
  scale_colour_viridis_d()+
  labs(x = NULL, y = NULL, title = "Visualização do DiD")+
  theme(legend.position = "none")
