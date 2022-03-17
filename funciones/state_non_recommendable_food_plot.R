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
