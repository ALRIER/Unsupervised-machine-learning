install.packages("semPlot")
install.packages("readr") 
install.packages("lavaan")
library("semPlot")
library("readr")
library("lavaan")

datos2 <- (Variables_Cho_amp_Yang)

modelo_confir2 <- 'V.util =~ utilitario1	+ utilitario2	+ utilitario3	+ ries_perc1	+ ries_perc2	+ ries_perc3 
V.espx =~ Hedónico1	+ Hedónico2	+ Hedónico3	+ busq_sens1	+ busq_sens2	+ busq_sens3	+ social1	+ social2	+ social3	+ val_social1	+ val_social2	+ val_social3	+ uso_r_sociales1	+ uso_r_sociales2	+ uso_r_sociales3	+ uso_r_sociales4	+ uso_r_sociales5	+ Int_busq_info1	+ Int_busq_info2	+ Int_busq_info3	+ int_visita1	+ int_visita2	+ int_visita3	+ int_visita4
V.util~~V.espx'
modelo_confir2

modelo2 <- cfa(modelo_confir2, data = datos2)
summary(modelo2, fit.measures=TRUE, rsq=TRUE)
parameterestimates(modelo2, standardized = TRUE)
.
semPaths(modelo2, what = "paths", layout = "circle", title = TRUE, style = "LISREL")
semPaths(modelo2, what = "est", layout = "circle", title = TRUE, style = "LISREL")
