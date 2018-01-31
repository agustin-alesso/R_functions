setwd("./aurelia/RP cilindros/")

# Tasa de avance en mm/seg
tasa <- 10.02/60

# generar lista de cilindros
cil <- list.files(pattern = "[^marconi.R]")

# data.frame resultado
resumen <- data.frame(cil =cil, med = 0, mna = 0)

# 
for (i in cil) {
	tmp <- read.table(file = i, header = T, sep = "", col.names = c("prof", "RP"))
	tmp <- within(tmp, {
		RP <- RP / 0.1256*0.098
		prof <- prof * tasa
	})

	# Seleccionar valores entre 10 y 40 mm del cilindro
	tmp <- tmp[tmp$prof > 10 & tmp$prof <40,]
	
	# calcular mediana RP
	mna <- with(tmp, median(RP))
	med <- with(tmp, mean(RP))
	
	resumen[resumen$cil == i, 2:3] <- c(med,mna)
}
	
resumen

# write.table()

