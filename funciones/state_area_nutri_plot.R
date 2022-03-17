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
