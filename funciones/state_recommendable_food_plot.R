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
