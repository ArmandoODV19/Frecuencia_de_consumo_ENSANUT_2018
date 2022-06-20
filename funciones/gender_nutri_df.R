gender_nutri_df <- function(x = ensanut_limpia, food,state, domain){
  pre_hombre <- x %>%
    select(food, edad_categorica, entidad, sexo, area) %>%
    filter(entidad == state,
           area == domain) %>%
    group_by(edad_categorica, sexo) %>%
    count() %>%
    filter(edad_categorica == "preescolares",
           sexo == "hombre") %>%
    summarise(total = sum(n))

  pre_hombre_total <- pre_hombre$total

  pre_mujer <- x %>%
    select(food, edad_categorica, entidad, sexo, area) %>%
    filter(entidad == state,
           area == domain) %>%
    group_by(edad_categorica, sexo) %>%
    count() %>%
    filter(edad_categorica == "preescolares",
           sexo == "mujer") %>%
    summarise(total = sum(n))

  pre_mujer_total <- pre_mujer$total


  esc_hombre <-x %>%
    select(food, edad_categorica, entidad, sexo, area) %>%
    filter(entidad == state,
           area == domain) %>%
    group_by(edad_categorica, sexo) %>%
    count() %>%
    filter(edad_categorica == "escolares",
           sexo == "hombre") %>%
    summarise(total = sum(n))

  esc_hombre_total <- esc_hombre$total

  esc_mujer <-x %>%
    select(food, edad_categorica, entidad, sexo, area) %>%
    filter(entidad == state,
           area == domain) %>%
    group_by(edad_categorica, sexo) %>%
    count() %>%
    filter(edad_categorica == "escolares",
           sexo == "mujer") %>%
    summarise(total = sum(n))

  esc_mujer_total <- esc_mujer$total

  ado_hombre <- x %>%
    select(food, edad_categorica, entidad, sexo, area) %>%
    filter(entidad == state,
           area == domain) %>%
    group_by(edad_categorica, sexo) %>%
    count() %>%
    filter(edad_categorica == "adolescentes",
           sexo == "hombre") %>%
    summarise(total = sum(n))

  ado_hombre_total <- ado_hombre$total

  ado_mujer <- x %>%
    select(food, edad_categorica, entidad, sexo, area) %>%
    filter(entidad == state,
           area == domain) %>%
    group_by(edad_categorica, sexo) %>%
    count() %>%
    filter(edad_categorica == "adolescentes",
           sexo == "mujer") %>%
    summarise(total = sum(n))

  ado_mujer_total <- ado_mujer$total

  adul_hombre <- x %>%
    select(food, edad_categorica, entidad, sexo, area) %>%
    filter(entidad == state,
           area == domain) %>%
    group_by(edad_categorica, sexo) %>%
    count() %>%
    filter(edad_categorica == "adultos",
           sexo == "hombre") %>%
    summarise(total = sum(n))

  adul_hombre_total <- adul_hombre$total

  adul_mujer <- x %>%
    select(food, edad_categorica, entidad, sexo, area) %>%
    filter(entidad == state,
           area == domain) %>%
    group_by(edad_categorica, sexo) %>%
    count() %>%
    filter(edad_categorica == "adultos",
           sexo == "mujer") %>%
    summarise(total = sum(n))

  adul_mujer_total <- adul_mujer$total

  x %>%
    select(food, edad_categorica, entidad, sexo, area) %>%
    filter(x[,(colnames(x)%in%c(food))] == 1,
           entidad == state,
           area == domain) %>%
    group_by(edad_categorica, entidad, sexo, area) %>%
    count() %>%
    mutate(porcentaje = case_when(edad_categorica == "adolescentes" & sexo == "hombre" ~ n/ado_hombre_total,
                                  edad_categorica == "adolescentes" & sexo == "mujer" ~ n/ado_mujer_total,
                                  edad_categorica == "adultos" & sexo == "hombre" ~ n/adul_hombre_total,
                                  edad_categorica == "adultos" & sexo == "mujer" ~ n/adul_mujer_total,
                                  edad_categorica == "escolares" & sexo == "hombre" ~ n/esc_hombre_total,
                                  edad_categorica == "escolares" & sexo == "mujer" ~ n/esc_mujer_total,
                                  edad_categorica == "preescolares" & sexo == "hombre" ~ n/pre_hombre_total,
                                  edad_categorica == "preescolares" & sexo == "mujer" ~ n/pre_mujer_total),
           edad_categorica = factor(edad_categorica, levels = c("preescolares",
                                                                "escolares", "adolescentes",
                                                                "adultos")))

}
