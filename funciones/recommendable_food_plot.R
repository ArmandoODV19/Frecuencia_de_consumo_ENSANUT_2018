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
                      ncol = 4, nrow = 2,
                      font.label = list(size = 10), hjust = -0.5, vjust = 1)
  figure
}
