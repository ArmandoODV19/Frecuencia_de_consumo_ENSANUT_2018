# juntando la ensanut


preescolares$edad_categorica <- "preescolares"
escolares$edad_categorica <- "escolares"
adolescentes$edad_categorica <- "adolescentes"
adultos$edad_categorica <- "adultos"

todo <- rbind(preescolares, escolares, adolescentes, adultos)

ensanut_limpia <- todo





# guardando el dataset

write.csv(ensanut_limpia, "clean_data/ensanut_limpia.csv")
saveRDS(ensanut_limpia, "clean_data/ensanut_limpia.rds")
