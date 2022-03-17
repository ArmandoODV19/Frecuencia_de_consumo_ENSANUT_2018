non_recommendable_food_plot <- function(){
  a <- nutri_plot(food = "bebidas_no_lacteas", x_name = "")
  b <- nutri_plot(food = "dulces", x_name = "")
  c <- nutri_plot(food = "cereales", x_name = "")
  d <- nutri_plot(food = "bebidas_lacteas", x_name = "")
  e <- nutri_plot(food = "antojitos", x_name = "")
  f <- nutri_plot(food = "carnes_procesadas", x_name = "")

  figure <- ggarrange(a, b, c, d, e, f,
                      labels = c("Bebidas no lácteas endulzadas",
                                 "Dulces y botanas", "Cereales dulces",
                                 "Bebidas lácteas endulzadas",
                                 "Antojitos mexicanos", "Carnes procesadas"),
                      ncol = 6, nrow = 1,
                      font.label = list(size = 7), hjust = -0.5, vjust = 1)
  figure
}
