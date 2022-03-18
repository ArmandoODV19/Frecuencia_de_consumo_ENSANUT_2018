zone_gender_nutri_df <- function(x = ensanut_limpia, food, zone){
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
                                                                "adultos")))
}

