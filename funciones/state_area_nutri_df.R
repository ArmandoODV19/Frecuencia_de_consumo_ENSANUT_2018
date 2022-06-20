state_area_nutri_df <- function(x = ensanut_limpia, food, state, domain){
  pre <- x %>%
    select(food, edad_categorica, entidad, area) %>%
    filter(entidad == state,
           area == domain) %>%
    group_by(edad_categorica, entidad) %>%
    count() %>%
    filter(edad_categorica == "preescolares") %>%
    summarise(total = sum(n))

  pre_total <- pre$total


  esc <-x %>%
    select(food, edad_categorica, entidad, sexo, area) %>%
    filter(entidad == state,
           area == domain) %>%
    group_by(edad_categorica, entidad) %>%
    count() %>%
    filter(edad_categorica == "escolares") %>%
    summarise(total = sum(n))

  esc_total <- esc$total

  ado <- x %>%
    select(food, edad_categorica, entidad, sexo, area) %>%
    filter(entidad == state,
           area == domain) %>%
    group_by(edad_categorica, entidad) %>%
    count() %>%
    filter(edad_categorica == "adolescentes") %>%
    summarise(total = sum(n))

  ado_total <- ado$total

  adul <- x %>%
    select(food, edad_categorica, entidad, sexo, area) %>%
    filter(entidad == state,
           area == domain) %>%
    group_by(edad_categorica, entidad) %>%
    count() %>%
    filter(edad_categorica == "adultos") %>%
    summarise(total = sum(n))

  adul_total <- adul$total

  x %>%
    select(food, edad_categorica, entidad, area) %>%
    filter(x[,(colnames(x)%in%c(food))] == 1,
           entidad == state,
           area == domain) %>%
    group_by(edad_categorica, entidad, area) %>%
    count() %>%
    mutate(porcentaje = case_when(edad_categorica == "adolescentes" ~ n/ado_total,
                                  edad_categorica == "adultos" ~ n/adul_total,
                                  edad_categorica == "escolares" ~ n/esc_total,
                                  edad_categorica == "preescolares" ~ n/pre_total),
           edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                                "escolares", "adolescentes",
                                                                "adultos")))

}


