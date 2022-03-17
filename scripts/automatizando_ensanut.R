### automatizando ensanut

ensanut_limpia <- readRDS("clean_data/ensanut_limpia.rds")

# funcion para conocer porcentaje de consumo de alimentos

nutri_plot <- function(x = ensanut_limpia, food, x_name = "edad", y_name=""){
  x %>%
    select(food, edad_categorica) %>%
    filter(x[,(colnames(x)%in%c(food))] == 1) %>%
    group_by(edad_categorica) %>%
    count() %>%
  mutate(porcentaje = case_when(edad_categorica == "adolescentes" ~ freq/5182,
                                edad_categorica == "adultos" ~ freq/15791,
                                edad_categorica == "escolares" ~ freq/5989,
                                edad_categorica == "preescolares" ~ freq/3068),
         edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                              "escolares", "adolescentes",
                                                              "adultos"))) %>%
    ggplot(aes(x = edad_categorica, y = porcentaje, fill=edad_categorica))+
    xlab(x_name)+
    ylab(y_name)+
    geom_col()+
    theme_classic()+
    theme(legend.position="none",
          axis.text.x = element_text(size = 10, angle = 90, hjust = 1))+
    ylim(0,1)
}


nutri_plot(food = "agua")


# funcion para conocer porcentaje de consumo de alimentos dividido por estados
# para esta funcion se debe conocer el total de población que conforma cada estado
# posteriormente, se divide la frecuencia por el total de personas por estado

nutri_plot_all_states <- function(x = ensanut_limpia, food, x_name = "edad", y_name=""){
  ensanut_limpia %>%
    select(agua, edad_categorica, entidad) %>%
    filter(agua == 1) %>%
    group_by(edad_categorica, entidad) %>%
    count() %>%
    mutate(porcentaje = case_when(edad_categorica == "adolescentes" ~ freq/5182,
                                  edad_categorica == "adultos" ~ freq/15791,
                                  edad_categorica == "escolares" ~ freq/5989,
                                  edad_categorica == "preescolares" ~ freq/3068),
           edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                                "escolares", "adolescentes",
                                                                "adultos"))) %>%
    ggplot(aes(x = edad_categorica, y = porcentaje, fill=edad_categorica))+
    xlab(x_name)+
    ylab(y_name)+
    geom_col()+
    facet_wrap(.~entidad)+
    theme_classic()+
    theme(legend.position="none",
          axis.text.x = element_text(size = 10, angle = 90, hjust = 1))+
    ylim(0,1)
}

nutri_plot_all_states(food = "agua")

# para saber cuantas personas se encuestaron por estado
ensanut_limpia %>%
  select(agua,edad_categorica, entidad) %>%
  filter(entidad == "Aguascalientes")

# funcion para obtner porcentaje de consumo de alimento y estado


state_nutri_plot <- function(x = ensanut_limpia, food,state){
  pre <- x %>%
    select(food, edad_categorica, entidad) %>%
    filter(entidad == state) %>%
    group_by(edad_categorica, entidad) %>%
    count() %>%
    filter(edad_categorica == "preescolares") %>%
    summarise(total = sum(freq))

  pre_total <- pre$total


  esc <- x %>%
    select(food, edad_categorica, entidad) %>%
    filter(entidad == state) %>%
    group_by(edad_categorica, entidad) %>%
    count() %>%
    filter(edad_categorica == "escolares") %>%
    summarise(total = sum(freq))

  esc_total <- esc$total

  ado <- x %>%
    select(food, edad_categorica, entidad) %>%
    filter(entidad == state) %>%
    group_by(edad_categorica, entidad) %>%
    count() %>%
    filter(edad_categorica == "adolescentes") %>%
    summarise(total = sum(freq))

  ado_total <- ado$total

  adul <- x %>%
    select(food, edad_categorica, entidad) %>%
    filter(entidad == state) %>%
    group_by(edad_categorica, entidad) %>%
    count() %>%
    filter(edad_categorica == "adultos") %>%
    summarise(total = sum(freq))

  adul_total <- adul$total

  x %>%
    select(food, edad_categorica, entidad) %>%
    filter(x[,(colnames(x)%in%c(food))] == 1,
           entidad == state) %>%
    group_by(edad_categorica, entidad) %>%
    count() %>%
    mutate(porcentaje = case_when(edad_categorica == "adolescentes" ~ freq/ado_total,
                                  edad_categorica == "adultos" ~ freq/adul_total,
                                  edad_categorica == "escolares" ~ freq/esc_total,
                                  edad_categorica == "preescolares" ~ freq/pre_total),
           edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                                "escolares", "adolescentes",
                                                                "adultos"))) %>%
    ggplot(aes(x = edad_categorica, y = porcentaje, fill=edad_categorica))+
    xlab("")+
    ylab("")+
    geom_col()+
    theme_classic()+
    theme(legend.position="none",
          axis.text.x = element_text(size = 10, angle = 90, hjust = 1))+
    ylim(0,1)

}

state_nutri_plot(food = "frutas", state = "Chihuahua")


# graficar por alimento, entidad, sexo y area


filter_nutri_plot <- function(x = ensanut_limpia, food,state,
                             sex, domain){
  pre <- x %>%
    select(food, edad_categorica, entidad, sexo, area) %>%
    filter(entidad == state,
           sexo == sex,
           area == domain) %>%
    group_by(edad_categorica, entidad) %>%
    count() %>%
    filter(edad_categorica == "preescolares") %>%
    summarise(total = sum(freq))

  pre_total <- pre$total


  esc <-x %>%
    select(food, edad_categorica, entidad, sexo, area) %>%
    filter(entidad == state,
           sexo == sex,
           area == domain) %>%
    group_by(edad_categorica, entidad) %>%
    count() %>%
    filter(edad_categorica == "escolares") %>%
    summarise(total = sum(freq))

  esc_total <- esc$total

  ado <- x %>%
    select(food, edad_categorica, entidad, sexo, area) %>%
    filter(entidad == state,
           sexo == sex,
           area == domain) %>%
    group_by(edad_categorica, entidad) %>%
    count() %>%
    filter(edad_categorica == "adolescentes") %>%
    summarise(total = sum(freq))

  ado_total <- ado$total

  adul <- x %>%
    select(food, edad_categorica, entidad, sexo, area) %>%
    filter(entidad == state,
           sexo == sex,
           area == domain) %>%
    group_by(edad_categorica, entidad) %>%
    count() %>%
    filter(edad_categorica == "adultos") %>%
    summarise(total = sum(freq))

  adul_total <- adul$total

  x %>%
    select(food, edad_categorica, entidad, sexo, area) %>%
    filter(x[,(colnames(x)%in%c(food))] == 1,
           entidad == state,
           sexo == sex,
           area == domain) %>%
    group_by(edad_categorica, entidad, sexo, area) %>%
    count() %>%
    mutate(porcentaje = case_when(edad_categorica == "adolescentes" ~ freq/ado_total,
                                  edad_categorica == "adultos" ~ freq/adul_total,
                                  edad_categorica == "escolares" ~ freq/esc_total,
                                  edad_categorica == "preescolares" ~ freq/pre_total),
           edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                                "escolares", "adolescentes",
                                                                "adultos"))) %>%
    ggplot(aes(x = edad_categorica, y = porcentaje, fill=edad_categorica))+
    xlab("")+
    ylab("")+
    geom_col()+
    theme_classic()+
    theme(legend.position="none",
          axis.text.x = element_text(size = 10, angle = 90, hjust = 1))+
    ylim(0,1)


}

filter_nutri_plot(food = "frutas", state = "Chihuahua", sex = "hombre", domain = "urbano")

###

# filtrar por estado, area, grupo de alimento y una barra por cada grupo de edad


state_area_nutri_plot <- function(x = ensanut_limpia, food, state, domain){
  pre <- x %>%
    select(food, edad_categorica, entidad, area) %>%
    filter(entidad == state,
           area == domain) %>%
    group_by(edad_categorica, entidad) %>%
    count() %>%
    filter(edad_categorica == "preescolares") %>%
    summarise(total = sum(freq))

  pre_total <- pre$total


  esc <-x %>%
    select(food, edad_categorica, entidad, sexo, area) %>%
    filter(entidad == state,
           area == domain) %>%
    group_by(edad_categorica, entidad) %>%
    count() %>%
    filter(edad_categorica == "escolares") %>%
    summarise(total = sum(freq))

  esc_total <- esc$total

  ado <- x %>%
    select(food, edad_categorica, entidad, sexo, area) %>%
    filter(entidad == state,
           area == domain) %>%
    group_by(edad_categorica, entidad) %>%
    count() %>%
    filter(edad_categorica == "adolescentes") %>%
    summarise(total = sum(freq))

  ado_total <- ado$total

  adul <- x %>%
    select(food, edad_categorica, entidad, sexo, area) %>%
    filter(entidad == state,
           area == domain) %>%
    group_by(edad_categorica, entidad) %>%
    count() %>%
    filter(edad_categorica == "adultos") %>%
    summarise(total = sum(freq))

  adul_total <- adul$total

  x %>%
    select(food, edad_categorica, entidad, area) %>%
    filter(x[,(colnames(x)%in%c(food))] == 1,
           entidad == state,
           area == domain) %>%
    group_by(edad_categorica, entidad, area) %>%
    count() %>%
    mutate(porcentaje = case_when(edad_categorica == "adolescentes" ~ freq/ado_total,
                                  edad_categorica == "adultos" ~ freq/adul_total,
                                  edad_categorica == "escolares" ~ freq/esc_total,
                                  edad_categorica == "preescolares" ~ freq/pre_total),
           edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                                "escolares", "adolescentes",
                                                                "adultos"))) %>%
    ggplot(aes(x = edad_categorica, y = porcentaje, fill=edad_categorica))+
    xlab("")+
    ylab("")+
    geom_col()+
    theme_classic()+
    theme(legend.position="none",
          axis.text.x = element_text(size = 10, angle = 90, hjust = 1))+
    ylim(0,1)


}

state_area_nutri_plot(food = "frutas", state = "Morelos", domain = "rural")

### filtrar por estado, area, grupo de alimento y plot por sexo

gender_nutri_plot <- function(x = ensanut_limpia, food,state, domain){
  pre_hombre <- x %>%
    select(food, edad_categorica, entidad, sexo, area) %>%
    filter(entidad == state,
           area == domain) %>%
    group_by(edad_categorica, sexo) %>%
    count() %>%
    filter(edad_categorica == "preescolares",
           sexo == "hombre") %>%
    summarise(total = sum(freq))

  pre_hombre_total <- pre_hombre$total

  pre_mujer <- x %>%
    select(food, edad_categorica, entidad, sexo, area) %>%
    filter(entidad == state,
           area == domain) %>%
    group_by(edad_categorica, sexo) %>%
    count() %>%
    filter(edad_categorica == "preescolares",
           sexo == "mujer") %>%
    summarise(total = sum(freq))

  pre_mujer_total <- pre_mujer$total


  esc_hombre <-x %>%
    select(food, edad_categorica, entidad, sexo, area) %>%
    filter(entidad == state,
           area == domain) %>%
    group_by(edad_categorica, sexo) %>%
    count() %>%
    filter(edad_categorica == "escolares",
           sexo == "hombre") %>%
    summarise(total = sum(freq))

  esc_hombre_total <- esc_hombre$total

  esc_mujer <-x %>%
    select(food, edad_categorica, entidad, sexo, area) %>%
    filter(entidad == state,
           area == domain) %>%
    group_by(edad_categorica, sexo) %>%
    count() %>%
    filter(edad_categorica == "escolares",
           sexo == "mujer") %>%
    summarise(total = sum(freq))

  esc_mujer_total <- esc_mujer$total

  ado_hombre <- x %>%
    select(food, edad_categorica, entidad, sexo, area) %>%
    filter(entidad == state,
           area == domain) %>%
    group_by(edad_categorica, sexo) %>%
    count() %>%
    filter(edad_categorica == "adolescentes",
           sexo == "hombre") %>%
    summarise(total = sum(freq))

  ado_hombre_total <- ado_hombre$total

  ado_mujer <- x %>%
    select(food, edad_categorica, entidad, sexo, area) %>%
    filter(entidad == state,
           area == domain) %>%
    group_by(edad_categorica, sexo) %>%
    count() %>%
    filter(edad_categorica == "adolescentes",
           sexo == "mujer") %>%
    summarise(total = sum(freq))

  ado_mujer_total <- ado_mujer$total

  adul_hombre <- x %>%
    select(food, edad_categorica, entidad, sexo, area) %>%
    filter(entidad == state,
           area == domain) %>%
    group_by(edad_categorica, sexo) %>%
    count() %>%
    filter(edad_categorica == "adultos",
           sexo == "hombre") %>%
    summarise(total = sum(freq))

  adul_hombre_total <- adul_hombre$total

  adul_mujer <- x %>%
    select(food, edad_categorica, entidad, sexo, area) %>%
    filter(entidad == state,
           area == domain) %>%
    group_by(edad_categorica, sexo) %>%
    count() %>%
    filter(edad_categorica == "adultos",
           sexo == "mujer") %>%
    summarise(total = sum(freq))

  adul_mujer_total <- adul_mujer$total

  x %>%
    select(food, edad_categorica, entidad, sexo, area) %>%
    filter(x[,(colnames(x)%in%c(food))] == 1,
           entidad == state,
           area == domain) %>%
    group_by(edad_categorica, entidad, sexo, area) %>%
    count() %>%
    mutate(porcentaje = case_when(edad_categorica == "adolescentes" & sexo == "hombre" ~ freq/ado_hombre_total,
                                  edad_categorica == "adolescentes" & sexo == "mujer" ~ freq/ado_mujer_total,
                                  edad_categorica == "adultos" & sexo == "hombre" ~ freq/adul_hombre_total,
                                  edad_categorica == "adultos" & sexo == "mujer" ~ freq/adul_mujer_total,
                                  edad_categorica == "escolares" & sexo == "hombre" ~ freq/esc_hombre_total,
                                  edad_categorica == "escolares" & sexo == "mujer" ~ freq/esc_mujer_total,
                                  edad_categorica == "preescolares" & sexo == "hombre" ~ freq/pre_hombre_total,
                                  edad_categorica == "preescolares" & sexo == "mujer" ~ freq/pre_mujer_total),
           edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                                "escolares", "adolescentes",
                                                                "adultos"))) %>%
    ggplot(aes(x = edad_categorica, y = porcentaje, fill=edad_categorica))+
    xlab("")+
    ylab("")+
    geom_col()+
    theme_classic()+
    theme(legend.position="none",
          axis.text.x = element_text(size = 10, angle = 90, hjust = 1))+
    ylim(0,1)+
    facet_wrap(.~sexo)


}

gender_nutri_plot(food = "frutas", state = "Morelos", domain = "rural")

###

# filtrar por grupo de alimento y region

zone_nutri_plot <- function(x = ensanut_limpia, food, zone){
  pre <- x %>%
    select(food, edad_categorica, region) %>%
    filter(region == zone) %>%
    group_by(edad_categorica, region) %>%
    count() %>%
    filter(edad_categorica == "preescolares") %>%
    summarise(total = sum(freq))

  pre_total <- pre$total


  esc <-x %>%
    select(food, edad_categorica, region) %>%
    filter(region == zone) %>%
    group_by(edad_categorica, region) %>%
    count() %>%
    filter(edad_categorica == "escolares") %>%
    summarise(total = sum(freq))

  esc_total <- esc$total

  ado <- x %>%
    select(food, edad_categorica, region) %>%
    filter(region == zone) %>%
    group_by(edad_categorica, region) %>%
    count() %>%
    filter(edad_categorica == "adolescentes") %>%
    summarise(total = sum(freq))

  ado_total <- ado$total

  adul <- x %>%
    select(food, edad_categorica, region) %>%
    filter(region == zone) %>%
    group_by(edad_categorica, region) %>%
    count() %>%
    filter(edad_categorica == "adultos") %>%
    summarise(total = sum(freq))

  adul_total <- adul$total

  x %>%
    select(food, edad_categorica, region) %>%
    filter(x[,(colnames(x)%in%c(food))] == 1,
           region == zone) %>%
    group_by(edad_categorica, region) %>%
    count() %>%
    mutate(porcentaje = case_when(edad_categorica == "adolescentes" ~ freq/ado_total,
                                  edad_categorica == "adultos" ~ freq/adul_total,
                                  edad_categorica == "escolares" ~ freq/esc_total,
                                  edad_categorica == "preescolares" ~ freq/pre_total),
           edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                                "escolares", "adolescentes",
                                                                "adultos"))) %>%
    ggplot(aes(x = edad_categorica, y = porcentaje, fill=edad_categorica))+
    xlab("")+
    ylab("")+
    geom_col()+
    theme_classic()+
    theme(legend.position="none",
          axis.text.x = element_text(size = 10, angle = 90, hjust = 1))+
    ylim(0,1)


}

zone_nutri_plot(food = "frutas", zone = "sur")


###

# filtrar por grupo de alimento, region y un plot por sexo

zone_gender_nutri_plot <- function(x = ensanut_limpia, food, zone){
  pre_hombre <- x %>%
    select(food, edad_categorica, region, sexo) %>%
    filter(region == zone) %>%
    group_by(edad_categorica, region) %>%
    count() %>%
    filter(edad_categorica == "preescolares",
           sexo == "hombre") %>%
    summarise(total = sum(freq))

  pre_hombre_total <- pre_hombre$total

  pre_mujer <- x %>%
    select(food, edad_categorica, region, sexo) %>%
    filter(region == zone) %>%
    group_by(edad_categorica, region) %>%
    count() %>%
    filter(edad_categorica == "preescolares",
           sexo == "mujer") %>%
    summarise(total = sum(freq))

  pre_mujer_total <- pre_mujer$total


  esc_hombre <-x %>%
    select(food, edad_categorica, region, sexo) %>%
    filter(region == zone) %>%
    group_by(edad_categorica, region) %>%
    count() %>%
    filter(edad_categorica == "escolares",
           sexo == "hombre") %>%
    summarise(total = sum(freq))

  esc_hombre_total <- esc_hombre$total

  esc_mujer <-x %>%
    select(food, edad_categorica, region, sexo) %>%
    filter(region == zone) %>%
    group_by(edad_categorica, region) %>%
    count() %>%
    filter(edad_categorica == "escolares",
           sexo == "mujer") %>%
    summarise(total = sum(freq))

  esc_mujer_total <- esc_mujer$total

  ado_hombre <- x %>%
    select(food, edad_categorica, region, sexo) %>%
    filter(region == zone) %>%
    group_by(edad_categorica, region) %>%
    count() %>%
    filter(edad_categorica == "adolescentes",
           sexo == "hombre") %>%
    summarise(total = sum(freq))

  ado_hombre_total <- ado_hombre$total

  ado_mujer <- x %>%
    select(food, edad_categorica, region, sexo) %>%
    filter(region == zone) %>%
    group_by(edad_categorica, region) %>%
    count() %>%
    filter(edad_categorica == "adolescentes",
           sexo == "mujer") %>%
    summarise(total = sum(freq))

  ado_mujer_total <- ado_mujer$total

  adul_hombre <- x %>%
    select(food, edad_categorica, region, sexo) %>%
    filter(region == zone) %>%
    group_by(edad_categorica, region) %>%
    count() %>%
    filter(edad_categorica == "adultos",
           sexo == "hombre") %>%
    summarise(total = sum(freq))

  adul_hombre_total <- adul_hombre$total

  adul_mujer <- x %>%
    select(food, edad_categorica, region, sexo) %>%
    filter(region == zone) %>%
    group_by(edad_categorica, region) %>%
    count() %>%
    filter(edad_categorica == "adultos",
           sexo == "mujer") %>%
    summarise(total = sum(freq))

  adul_mujer_total <- adul_mujer$total

  x %>%
    select(food, edad_categorica, region, sexo) %>%
    filter(x[,(colnames(x)%in%c(food))] == 1,
           region == zone) %>%
    group_by(edad_categorica, region) %>%
    count() %>%
    mutate(porcentaje = case_when(edad_categorica == "adolescentes" & sexo == "hombre" ~ freq/ado_hombre_total,
                                  edad_categorica == "adolescentes" & sexo == "mujer" ~ freq/ado_mujer_total,
                                  edad_categorica == "adultos" & sexo == "hombre" ~ freq/adul_hombre_total,
                                  edad_categorica == "adultos" & sexo == "mujer" ~ freq/adul_mujer_total,
                                  edad_categorica == "escolares" & sexo == "hombre" ~ freq/esc_hombre_total,
                                  edad_categorica == "escolares" & sexo == "mujer" ~ freq/esc_mujer_total,
                                  edad_categorica == "preescolares" & sexo == "hombre" ~ freq/pre_hombre_total,
                                  edad_categorica == "preescolares" & sexo == "mujer" ~ freq/pre_mujer_total),
           edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                                "escolares", "adolescentes",
                                                                "adultos"))) %>%
    ggplot(aes(x = edad_categorica, y = porcentaje, fill=edad_categorica))+
    xlab("")+
    ylab("")+
    geom_col()+
    theme_classic()+
    theme(legend.position="none",
          axis.text.x = element_text(size = 10, angle = 90, hjust = 1))+
    ylim(0,1)+
    facet_wrap(.~sexo)

}

zone_gender_nutri_plot(food = "frutas", zone = "norte")

### filtrar por alimentos recomendables


recommendable_food_plot <- function(){
  a <- nutri_plot(food = "agua", x_name = "")
  b <- nutri_plot(food = "lacteos", x_name = "")
  c <- nutri_plot(food = "frutas", x_name = "")
  d <- nutri_plot(food = "leguminosas", x_name = "")
  e <- nutri_plot(food = "carnes", x_name = "")
  f <- nutri_plot(food = "huevo", x_name = "")
  g <- nutri_plot(food = "verduras", x_name = "")

  figure <- ggarrange(a, b, c, d, e, f, g,
                      labels = c("Agua", "lácteos", "Frutas", "Leguminosas",
                                 "Carnes", "Huevo", "Verduras"),
                      ncol = 7, nrow = 1,
                      font.label = list(size = 10), hjust = -0.5, vjust = 1)
  figure
}

recommendable_food_plot()


### grafico de alimentos no recomendables

non_recommendable_food_plot <- function(){
  a <- nutri_plot(food = "bebidas_no_lacteas", x_name = "")
  b <- nutri_plot(food = "dulces", x_name = "")
  c <- nutri_plot(food = "cereales", x_name = "")
  d <- nutri_plot(food = "bebidas_lacteas", x_name = "")
  e <- nutri_plot(food = "antojitos", x_name = "")
  f <- nutri_plot(food = "carnes_procesadas", x_name = "")

  figure <- ggarrange(a, b, c, d, e, f,
                      labels = c("Bebidas no lácteas endulzadas",
                                 "Dulces y botanas", "Cereales dulces",
                                 "Bebidas lácteas endulzadas",
                                 "Antojitos mexicanos", "Carnes procesadas"),
                      ncol = 6, nrow = 1,
                      font.label = list(size = 7), hjust = -0.5, vjust = 1)
  figure
}

non_recommendable_food_plot()

### grafico de alimentos recomendables por estado

state_recommendable_food_plot <- function(estado){
  a <- state_nutri_plot(food = "agua", state = estado)
  b <- state_nutri_plot(food = "lacteos", state = estado)
  c <- state_nutri_plot(food = "frutas", state = estado)
  d <- state_nutri_plot(food = "leguminosas", state = estado)
  e <- state_nutri_plot(food = "carnes", state = estado)
  f <- state_nutri_plot(food = "huevo", state = estado)
  g <- state_nutri_plot(food = "verduras", state = estado)

  figure <- ggarrange(a, b, c, d, e, f, g,
                      labels = c("Agua", "lácteos", "Frutas", "Leguminosas",
                                 "Carnes", "Huevo", "Verduras"),
                      ncol = 7, nrow = 1,
                      font.label = list(size = 10), hjust = -0.5, vjust = 1)
  figure
}

state_recommendable_food_plot(estado = "Morelos")


# grafico de alimentos no recomendables por estado

state_non_recommendable_food_plot <- function(estado){
  a <- state_nutri_plot(food = "bebidas_no_lacteas", state = estado)
  b <- state_nutri_plot(food = "dulces", state = estado)
  c <- state_nutri_plot(food = "cereales", state = estado)
  d <- state_nutri_plot(food = "bebidas_lacteas", state = estado)
  e <- state_nutri_plot(food = "antojitos", state = estado)
  f <- state_nutri_plot(food = "carnes_procesadas", state = estado)

  figure <- ggarrange(a, b, c, d, e, f,
                      labels = c("Bebidas no lácteas endulzadas",
                                 "Dulces y botanas", "Cereales dulces",
                                 "Bebidas lácteas endulzadas",
                                 "Antojitos mexicanos", "Carnes procesadas"),
                      ncol = 6, nrow = 1,
                      font.label = list(size = 7), hjust = -0.5, vjust = 1)
  figure
}

state_non_recommendable_food_plot("Morelos")
