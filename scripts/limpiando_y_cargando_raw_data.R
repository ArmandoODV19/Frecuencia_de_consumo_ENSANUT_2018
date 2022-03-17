# cargando raw data
preescolares <- read.csv(unzip("raw_data/CN_FCA_PREES.csv.csv.zip"))
escolares <- read.csv(unzip("raw_data/CN_FCA_ESC.csv.csv.zip"))
adolescentes <- read.csv(unzip("raw_data/CN_FCA_ADOLESCENTES.csv.csv.zip"))
adultos <- read.csv(unzip("raw_data/CN_FCA_ADU.csv.csv.zip"))


### trabajando con datos preescolares ###


# cambiando el nombre de las columnas

colnames(preescolares) <- c("upm", "vivienda_seleccionada", "numero_hogar",
                            "numero_renglon", "sexo", "entidad", "area",
                            "region", "edad", "dias_frutas", "frutas",
                            "dias_leguminosas", "leguminosas", "dias_carnes",
                            "carnes", "dias_car_procesadas", "carnes_procesadas",
                            "dias_antojitos", "antojitos", "dias_dulces", "dulces",
                            "dias_cereales", "cereales", "dias_bebidas_no_lacteas",
                            "bebidas_no_lacteas", "dias_agua", "agua", "dias_bebidas_lacteas",
                            "bebidas_lacteas", "dias_lacteos", "lacteos", "dias_huevo",
                            "huevo", "dias_verduras", "verduras", "estrato_diseno",
                            "upm_diseno", "factor_expansion")

# modificando el sexo
preescolares$sexo <- as.factor(preescolares$sexo)
preescolares$sexo <- as.character(preescolares$sexo)

preescolares$sexo <- revalue(preescolares$sexo,
                             c(`1` = "hombre", `2` = "mujer"))

# modificando la entidad

preescolares$entidad <- as.factor(preescolares$entidad)
preescolares$entidad <- as.character(preescolares$entidad)

preescolares$entidad <- revalue(preescolares$entidad,
                                c("1"= "Aguascalientes", "2"="Baja California Norte",
                                  "3"="Baja California Sur", "4"="Campeche",
                                  "5"="Coahuila", "6"="Colima", "7"="Chiapas",
                                  "8"= "Chihuahua", "9"="CdMx", "10"="Durango",
                                  "11"="Guanajuato", "12"="Guerrero", "13"="Hidalgo",
                                  "14"="Jalisco", "15"="Edo. México","16"= "Michoacán",
                                  "17"="Morelos", "18"="Nayarit", "19"= "Nuevo León",
                                  "20"="Oaxaca", "21"= "Puebla", "22"="Querétaro",
                                  "23"="Quintana Roo", "24"="San Luis Potosí",
                                  "25"= "Sinaloa", "26"="Sonora", "27"="Tabasco",
                                  "28"="Tamaulipas","29"="Tlaxcala", "30"="Veracruz",
                                  "31"="Yucatán", "32"="Zacatecas"))

# modificando area

preescolares$area <- as.factor(preescolares$area)
preescolares$area <- as.character(preescolares$area)

preescolares$area <- revalue(preescolares$area,
                                c("1" = "urbano", "2" = "rural"))

# modificando la region

preescolares$region <- as.factor(preescolares$region)
preescolares$region <- as.character(preescolares$region)

preescolares$region <- revalue(preescolares$region,
                               c("1" = "norte", "2" = "centro",
                                 "3" = "cdmx", "4" = "sur"))


### limpiando datos de escolares ###

# cambiando el nombre de las columnas

colnames(escolares) <- c("upm", "vivienda_seleccionada", "numero_hogar",
                            "numero_renglon", "sexo", "entidad", "area",
                            "region", "edad", "dias_frutas", "frutas",
                            "dias_leguminosas", "leguminosas", "dias_carnes",
                            "carnes", "dias_car_procesadas", "carnes_procesadas",
                            "dias_antojitos", "antojitos", "dias_dulces", "dulces",
                            "dias_cereales", "cereales", "dias_bebidas_no_lacteas",
                            "bebidas_no_lacteas", "dias_agua", "agua", "dias_bebidas_lacteas",
                            "bebidas_lacteas", "dias_lacteos", "lacteos", "dias_huevo",
                            "huevo", "dias_verduras", "verduras", "estrato_diseno",
                            "upm_diseno", "factor_expansion")


# modificando el sexo
escolares$sexo <- as.factor(escolares$sexo)
escolares$sexo <- as.character(escolares$sexo)

escolares$sexo <- revalue(escolares$sexo,
                             c(`1` = "hombre", `2` = "mujer"))

# modificando la entidad

escolares$entidad <- as.factor(escolares$entidad)
escolares$entidad <- as.character(escolares$entidad)

escolares$entidad <- revalue(escolares$entidad,
                                c("1"= "Aguascalientes", "2"="Baja California Norte",
                                  "3"="Baja California Sur", "4"="Campeche",
                                  "5"="Coahuila", "6"="Colima", "7"="Chiapas",
                                  "8"= "Chihuahua", "9"="CdMx", "10"="Durango",
                                  "11"="Guanajuato", "12"="Guerrero", "13"="Hidalgo",
                                  "14"="Jalisco", "15"="Edo. México","16"= "Michoacán",
                                  "17"="Morelos", "18"="Nayarit", "19"= "Nuevo León",
                                  "20"="Oaxaca", "21"= "Puebla", "22"="Querétaro",
                                  "23"="Quintana Roo", "24"="San Luis Potosí",
                                  "25"= "Sinaloa", "26"="Sonora", "27"="Tabasco",
                                  "28"="Tamaulipas","29"="Tlaxcala", "30"="Veracruz",
                                  "31"="Yucatán", "32"="Zacatecas"))

# modificando area

escolares$area <- as.factor(escolares$area)
escolares$area <- as.character(escolares$area)

escolares$area <- revalue(escolares$area,
                             c("1" = "urbano", "2" = "rural"))

# modificando la region

escolares$region <- as.factor(escolares$region)
escolares$region <- as.character(escolares$region)

escolares$region <- revalue(escolares$region,
                               c("1" = "norte", "2" = "centro",
                                 "3" = "cdmx", "4" = "sur"))

### limpiando datos de adolescentes ###

# cambiando el nombre de las columnas

colnames(adolescentes) <- c("upm", "vivienda_seleccionada", "numero_hogar",
                            "numero_renglon", "sexo", "entidad", "area",
                            "region", "edad", "dias_frutas", "frutas",
                            "dias_leguminosas", "leguminosas", "dias_carnes",
                            "carnes", "dias_car_procesadas", "carnes_procesadas",
                            "dias_antojitos", "antojitos", "dias_dulces", "dulces",
                            "dias_cereales", "cereales", "dias_bebidas_no_lacteas",
                            "bebidas_no_lacteas", "dias_agua", "agua", "dias_bebidas_lacteas",
                            "bebidas_lacteas", "dias_lacteos", "lacteos", "dias_huevo",
                            "huevo", "dias_verduras", "verduras", "estrato_diseno",
                            "upm_diseno", "factor_expansion")

# modificando el sexo
adolescentes$sexo <- as.factor(adolescentes$sexo)
adolescentes$sexo <- as.character(adolescentes$sexo)

adolescentes$sexo <- revalue(adolescentes$sexo,
                             c(`1` = "hombre", `2` = "mujer"))

# modificando la entidad

adolescentes$entidad <- as.factor(adolescentes$entidad)
adolescentes$entidad <- as.character(adolescentes$entidad)

adolescentes$entidad <- revalue(adolescentes$entidad,
                                c("1"= "Aguascalientes", "2"="Baja California Norte",
                                  "3"="Baja California Sur", "4"="Campeche",
                                  "5"="Coahuila", "6"="Colima", "7"="Chiapas",
                                  "8"= "Chihuahua", "9"="CdMx", "10"="Durango",
                                  "11"="Guanajuato", "12"="Guerrero", "13"="Hidalgo",
                                  "14"="Jalisco", "15"="Edo. México","16"= "Michoacán",
                                  "17"="Morelos", "18"="Nayarit", "19"= "Nuevo León",
                                  "20"="Oaxaca", "21"= "Puebla", "22"="Querétaro",
                                  "23"="Quintana Roo", "24"="San Luis Potosí",
                                  "25"= "Sinaloa", "26"="Sonora", "27"="Tabasco",
                                  "28"="Tamaulipas","29"="Tlaxcala", "30"="Veracruz",
                                  "31"="Yucatán", "32"="Zacatecas"))

# modificando area

adolescentes$area <- as.factor(adolescentes$area)
adolescentes$area <- as.character(adolescentes$area)

adolescentes$area <- revalue(adolescentes$area,
                             c("1" = "urbano", "2" = "rural"))

# modificando la region

adolescentes$region <- as.factor(adolescentes$region)
adolescentes$region <- as.character(adolescentes$region)

adolescentes$region <- revalue(adolescentes$region,
                               c("1" = "norte", "2" = "centro",
                                 "3" = "cdmx", "4" = "sur"))



### limpiando datos de adultos ###

colnames(adultos) <- c("upm", "vivienda_seleccionada", "numero_hogar",
                            "numero_renglon", "sexo", "entidad", "area",
                            "region", "edad", "dias_frutas", "frutas",
                            "dias_leguminosas", "leguminosas", "dias_carnes",
                            "carnes", "dias_car_procesadas", "carnes_procesadas",
                            "dias_antojitos", "antojitos", "dias_dulces", "dulces",
                            "dias_cereales", "cereales", "dias_bebidas_no_lacteas",
                            "bebidas_no_lacteas", "dias_agua", "agua", "dias_bebidas_lacteas",
                            "bebidas_lacteas", "dias_lacteos", "lacteos", "dias_huevo",
                            "huevo", "dias_verduras", "verduras", "estrato_diseno",
                            "upm_diseno", "factor_expansion")

# modificando el sexo
adultos$sexo <- as.factor(adultos$sexo)
adultos$sexo <- as.character(adultos$sexo)

adultos$sexo <- revalue(adultos$sexo,
                             c(`1` = "hombre", `2` = "mujer"))

# modificando la entidad

adultos$entidad <- as.factor(adultos$entidad)
adultos$entidad <- as.character(adultos$entidad)

adultos$entidad <- revalue(adultos$entidad,
                                c("1"= "Aguascalientes", "2"="Baja California Norte",
                                  "3"="Baja California Sur", "4"="Campeche",
                                  "5"="Coahuila", "6"="Colima", "7"="Chiapas",
                                  "8"= "Chihuahua", "9"="CdMx", "10"="Durango",
                                  "11"="Guanajuato", "12"="Guerrero", "13"="Hidalgo",
                                  "14"="Jalisco", "15"="Edo. México","16"= "Michoacán",
                                  "17"="Morelos", "18"="Nayarit", "19"= "Nuevo León",
                                  "20"="Oaxaca", "21"= "Puebla", "22"="Querétaro",
                                  "23"="Quintana Roo", "24"="San Luis Potosí",
                                  "25"= "Sinaloa", "26"="Sonora", "27"="Tabasco",
                                  "28"="Tamaulipas","29"="Tlaxcala", "30"="Veracruz",
                                  "31"="Yucatán", "32"="Zacatecas"))

# modificando area

adultos$area <- as.factor(adultos$area)
adultos$area <- as.character(adultos$area)

adultos$area <- revalue(adultos$area,
                             c("1" = "urbano", "2" = "rural"))

# modificando la region

adultos$region <- as.factor(adultos$region)
adultos$region <- as.character(adultos$region)

adultos$region <- revalue(adultos$region,
                               c("1" = "norte", "2" = "centro",
                                 "3" = "cdmx", "4" = "sur"))


