nutri_plot(food = "agua")

nutri_df(food = "agua")


state_nutri_plot(food = "frutas", state = "Aguascalientes")
state_nutri_df(food = "frutas", state = "Aguascalientes")


filter_nutri_plot(food = "frutas", state = "Chihuahua", sex = "hombre", domain = "urbano")
filter_nutri_df(food = "frutas", state = "Chihuahua", sex = "hombre", domain = "urbano")

state_area_nutri_plot(food = "frutas", state = "Morelos", domain = "rural")
state_area_nutri_df(food = "frutas", state = "Morelos", domain = "rural")

gender_nutri_plot(food = "frutas", state = "Morelos", domain = "rural")
gender_nutri_df(food = "frutas", state = "Morelos", domain = "rural")


zone_nutri_plot(food = "frutas", zone = "cdmx")
zone_nutri_df(food = "frutas", zone = "cdmx")

zone_gender_nutri_plot(food = "frutas", zone = "norte")
zone_gender_nutri_df(food = "frutas", zone = "norte")


recommendable_food_plot()


non_recommendable_food_plot()


state_recommendable_food_plot(estado = "Chihuahua")


state_non_recommendable_food_plot("Morelos")
