nutri_df <- function(x = ensanut_limpia, food){
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
                                                                "adultos")))
}

