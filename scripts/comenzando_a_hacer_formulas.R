ensanut_limpia %>% group_by(edad_categorica) %>%
  summarise(total = count(edad_categorica))


ensanut_limpia %>%
  filter(edad_categorica == "adultos",
         frutas == 1) %>%
  group_by(as.character(frutas)) %>%
  count(frutas)


ensanut_limpia %>%
  select(frutas, edad_categorica) %>%
  group_by(edad_categorica) %>%
  count() %>%
  filter(frutas == 1) %>%
  mutate(porcentaje = case_when(edad_categorica == "adolescentes" ~ freq/5182,
                                edad_categorica == "adultos" ~ freq/15791,
                                edad_categorica == "escolares" ~ freq/5989,
                                edad_categorica == "preescolares" ~ freq/3068),
         edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                              "escolares", "adolescentes",
                                                              "adultos"))) %>%
  ggplot(aes(x = edad_categorica, y = porcentaje, fill=edad_categorica))+
  geom_col()+
  theme(legend.position="none",
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1))+
  ylim(0,1)



# ensanut agua

ensanut_limpia %>%
  select(agua, edad_categorica) %>%
  group_by(edad_categorica) %>%
  count() %>%
  filter(agua == 1) %>%
  mutate(porcentaje = case_when(edad_categorica == "adolescentes" ~ freq/5182,
                                edad_categorica == "adultos" ~ freq/15791,
                                edad_categorica == "escolares" ~ freq/5989,
                                edad_categorica == "preescolares" ~ freq/3068),
         edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                              "escolares", "adolescentes",
                                                              "adultos"))) %>%
  ggplot(aes(x = edad_categorica, y = porcentaje, fill=edad_categorica))+
  geom_col()+
  theme_classic()+
  theme(legend.position="none",
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1))+
  ylim(0,1)

# lacteos

ensanut_limpia %>%
  select(lacteos, edad_categorica) %>%
  group_by(edad_categorica) %>%
  count() %>%
  filter(lacteos == 1) %>%
  mutate(porcentaje = case_when(edad_categorica == "adolescentes" ~ freq/5182,
                                edad_categorica == "adultos" ~ freq/15791,
                                edad_categorica == "escolares" ~ freq/5989,
                                edad_categorica == "preescolares" ~ freq/3068),
         edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                              "escolares", "adolescentes",
                                                              "adultos"))) %>%
  ggplot(aes(x = edad_categorica, y = porcentaje, fill=edad_categorica))+
  geom_col()+
  theme_classic()+
  theme(legend.position="none",
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1))+
  ylim(0,1)

# frutas

ensanut_limpia %>%
  select(frutas, edad_categorica) %>%
  group_by(edad_categorica) %>%
  count() %>%
  filter(frutas == 1) %>%
  mutate(porcentaje = case_when(edad_categorica == "adolescentes" ~ freq/5182,
                                edad_categorica == "adultos" ~ freq/15791,
                                edad_categorica == "escolares" ~ freq/5989,
                                edad_categorica == "preescolares" ~ freq/3068),
         edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                              "escolares", "adolescentes",
                                                              "adultos"))) %>%
  ggplot(aes(x = edad_categorica, y = porcentaje, fill=edad_categorica))+
  geom_col()+
  theme_classic()+
  theme(legend.position="none",
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1))+
  ylim(0,1)

# leguminosas

ensanut_limpia %>%
  select(leguminosas, edad_categorica) %>%
  group_by(edad_categorica) %>%
  count() %>%
  filter(leguminosas == 1) %>%
  mutate(porcentaje = case_when(edad_categorica == "adolescentes" ~ freq/5182,
                                edad_categorica == "adultos" ~ freq/15791,
                                edad_categorica == "escolares" ~ freq/5989,
                                edad_categorica == "preescolares" ~ freq/3068),
         edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                              "escolares", "adolescentes",
                                                              "adultos"))) %>%
  ggplot(aes(x = edad_categorica, y = porcentaje, fill=edad_categorica))+
  geom_col()+
  theme_classic()+
  theme(legend.position="none",
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1))+
  ylim(0,1)

# carnes

ensanut_limpia %>%
  select(carnes, edad_categorica) %>%
  group_by(edad_categorica) %>%
  count() %>%
  filter(carnes == 1) %>%
  mutate(porcentaje = case_when(edad_categorica == "adolescentes" ~ freq/5182,
                                edad_categorica == "adultos" ~ freq/15791,
                                edad_categorica == "escolares" ~ freq/5989,
                                edad_categorica == "preescolares" ~ freq/3068),
         edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                              "escolares", "adolescentes",
                                                              "adultos"))) %>%
  ggplot(aes(x = edad_categorica, y = porcentaje, fill=edad_categorica))+
  geom_col()+
  theme_classic() +
  theme(legend.position="none",
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1))+
  ylim(0,1)

# huevo

ensanut_limpia %>%
  select(huevo, edad_categorica) %>%
  group_by(edad_categorica) %>%
  count() %>%
  filter(huevo == 1) %>%
  mutate(porcentaje = case_when(edad_categorica == "adolescentes" ~ freq/5182,
                                edad_categorica == "adultos" ~ freq/15791,
                                edad_categorica == "escolares" ~ freq/5989,
                                edad_categorica == "preescolares" ~ freq/3068),
         edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                              "escolares", "adolescentes",
                                                              "adultos"))) %>%
  ggplot(aes(x = edad_categorica, y = porcentaje, fill=edad_categorica))+
  geom_col()+
  theme_classic() +
  theme(legend.position="none",
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1))+
  ylim(0,1)

# verduras

ensanut_limpia %>%
  select(verduras, edad_categorica) %>%
  group_by(edad_categorica) %>%
  count() %>%
  filter(verduras == 1) %>%
  mutate(porcentaje = case_when(edad_categorica == "adolescentes" ~ freq/5182,
                                edad_categorica == "adultos" ~ freq/15791,
                                edad_categorica == "escolares" ~ freq/5989,
                                edad_categorica == "preescolares" ~ freq/3068),
         edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                              "escolares", "adolescentes",
                                                              "adultos"))) %>%
  ggplot(aes(x = edad_categorica, y = porcentaje, fill=edad_categorica))+
  geom_col()+
  theme_classic() +
  theme(legend.position="none",
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1))+
  ylim(0,1)


#### bedidas no recomendables

# bebidas no lacteas endulzadas

ensanut_limpia %>%
  select(bebidas_no_lacteas, edad_categorica) %>%
  group_by(edad_categorica) %>%
  count() %>%
  filter(bebidas_no_lacteas == 1) %>%
  mutate(porcentaje = case_when(edad_categorica == "adolescentes" ~ freq/5182,
                                edad_categorica == "adultos" ~ freq/15791,
                                edad_categorica == "escolares" ~ freq/5989,
                                edad_categorica == "preescolares" ~ freq/3068),
         edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                              "escolares", "adolescentes",
                                                              "adultos"))) %>%
  ggplot(aes(x = edad_categorica, y = porcentaje, fill=edad_categorica))+
  geom_col()+
  theme_classic() +
  theme(legend.position="none",
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1))+
  ylim(0,1)

# dulces y botanas

ensanut_limpia %>%
  select(dulces, edad_categorica) %>%
  group_by(edad_categorica) %>%
  count() %>%
  filter(dulces == 1) %>%
  mutate(porcentaje = case_when(edad_categorica == "adolescentes" ~ freq/5182,
                                edad_categorica == "adultos" ~ freq/15791,
                                edad_categorica == "escolares" ~ freq/5989,
                                edad_categorica == "preescolares" ~ freq/3068),
         edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                              "escolares", "adolescentes",
                                                              "adultos"))) %>%
  ggplot(aes(x = edad_categorica, y = porcentaje, fill=edad_categorica))+
  geom_col()+
  theme_classic() +
  theme(legend.position="none",
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1))+
  ylim(0,1)

# cereales *

ensanut_limpia %>%
  select(cereales, edad_categorica) %>%
  group_by(edad_categorica) %>%
  count() %>%
  filter(cereales == 1) %>%
  mutate(porcentaje = case_when(edad_categorica == "adolescentes" ~ freq/5182,
                                edad_categorica == "adultos" ~ freq/15791,
                                edad_categorica == "escolares" ~ freq/5989,
                                edad_categorica == "preescolares" ~ freq/3068),
         edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                              "escolares", "adolescentes",
                                                              "adultos"))) %>%
  ggplot(aes(x = edad_categorica, y = porcentaje, fill=edad_categorica))+
  geom_col()+
  theme_classic() +
  theme(legend.position="none",
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1))+
  ylim(0,1)

# bebidas lacteas endulzadas

ensanut_limpia %>%
  select(bebidas_lacteas, edad_categorica) %>%
  group_by(edad_categorica) %>%
  count() %>%
  filter(bebidas_lacteas == 1) %>%
  mutate(porcentaje = case_when(edad_categorica == "adolescentes" ~ freq/5182,
                                edad_categorica == "adultos" ~ freq/15791,
                                edad_categorica == "escolares" ~ freq/5989,
                                edad_categorica == "preescolares" ~ freq/3068),
         edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                              "escolares", "adolescentes",
                                                              "adultos"))) %>%
  ggplot(aes(x = edad_categorica, y = porcentaje, fill=edad_categorica))+
  geom_col()+
  theme_classic() +
  theme(legend.position="none",
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1))+
  ylim(0,1)

# comida rapida y antojitos

ensanut_limpia %>%
  select(antojitos, edad_categorica) %>%
  group_by(edad_categorica) %>%
  count() %>%
  filter(antojitos == 1) %>%
  mutate(porcentaje = case_when(edad_categorica == "adolescentes" ~ freq/5182,
                                edad_categorica == "adultos" ~ freq/15791,
                                edad_categorica == "escolares" ~ freq/5989,
                                edad_categorica == "preescolares" ~ freq/3068),
         edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                              "escolares", "adolescentes",
                                                              "adultos"))) %>%
  ggplot(aes(x = edad_categorica, y = porcentaje, fill=edad_categorica))+
  geom_col()+
  theme_classic() +
  theme(legend.position="none",
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1))+
  ylim(0,1)

# carnes procesadas

ensanut_limpia %>%
  select(carnes_procesadas, edad_categorica) %>%
  group_by(edad_categorica) %>%
  count() %>%
  filter(carnes_procesadas == 1) %>%
  mutate(porcentaje = case_when(edad_categorica == "adolescentes" ~ freq/5182,
                                edad_categorica == "adultos" ~ freq/15791,
                                edad_categorica == "escolares" ~ freq/5989,
                                edad_categorica == "preescolares" ~ freq/3068),
         edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                              "escolares", "adolescentes",
                                                              "adultos"))) %>%
  ggplot(aes(x = edad_categorica, y = porcentaje, fill=edad_categorica))+
  geom_col()+
  theme_classic() +
  theme(legend.position="none",
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1))+
  ylim(0,1)
