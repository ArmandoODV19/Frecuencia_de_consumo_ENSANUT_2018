zone_nutri_plot <- function(x = ensanut_limpia, food, zone){
  pre <- x %>%
    select(food, edad_categorica, region) %>%
    filter(region == zone) %>%
    group_by(edad_categorica, region) %>%
    count() %>%
    filter(edad_categorica == "preescolares") %>%
    summarise(total = sum(n))

  pre_total <- pre$total


  esc <-x %>%
    select(food, edad_categorica, region) %>%
    filter(region == zone) %>%
    group_by(edad_categorica, region) %>%
    count() %>%
    filter(edad_categorica == "escolares") %>%
    summarise(total = sum(n))

  esc_total <- esc$total

  ado <- x %>%
    select(food, edad_categorica, region) %>%
    filter(region == zone) %>%
    group_by(edad_categorica, region) %>%
    count() %>%
    filter(edad_categorica == "adolescentes") %>%
    summarise(total = sum(n))

  ado_total <- ado$total

  adul <- x %>%
    select(food, edad_categorica, region) %>%
    filter(region == zone) %>%
    group_by(edad_categorica, region) %>%
    count() %>%
    filter(edad_categorica == "adultos") %>%
    summarise(total = sum(n))

  adul_total <- adul$total

  x %>%
    select(food, edad_categorica, region) %>%
    filter(x[,(colnames(x)%in%c(food))] == 1,
           region == zone) %>%
    group_by(edad_categorica, region) %>%
    count() %>%
    mutate(porcentaje = case_when(edad_categorica == "adolescentes" ~ n/ado_total,
                                  edad_categorica == "adultos" ~ n/adul_total,
                                  edad_categorica == "escolares" ~ n/esc_total,
                                  edad_categorica == "preescolares" ~ n/pre_total),
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
