nutri_plot_all_states <- function(x = ensanut_limpia, food, x_name = "edad", y_name=""){
  ensanut_limpia %>%
    select(agua, edad_categorica, entidad) %>%
    filter(agua == 1) %>%
    group_by(edad_categorica, entidad) %>%
    count() %>%
    mutate(porcentaje = case_when(edad_categorica == "adolescentes" ~ n/5182,
                                  edad_categorica == "adultos" ~ n/15791,
                                  edad_categorica == "escolares" ~ n/5989,
                                  edad_categorica == "preescolares" ~ n/3068),
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
