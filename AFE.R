'''Analisis Factorial exploratorio'''
data <- Respuestas_encuesta_final

'''verificar que es viable relizar el analisis factorial'''

'''primero correr una tabla de correlación'''
mcorre <-cor(data)
View(mcorre)
''' luego correr una matriz de varianza covarianza'''
cov(data)
install.packages("corrplot")
library("corrplot")
corrplot(cor(data), order="hclust", tl.col='black', tl.cex=0.5)
'''aplico pruebas de bartlett, covarianza, kmo, etc'''
'''importante tener en cuenta que para kmo las variables deben estar por encima de
0.70 para ser excelente nivel de analisis factorial'''
bartlett.test(data)
install.packages("psych")
library("psych")
KMO(data)
'''una vez obtenidos todos los indicadores se procede a identificar el
el número de factres recomendados, en especial por sistemas matemáticos'''
install.packages("corrr")
library("corrr")
eigen(mcorre)
'''también puedo usar el analisis de componentes principales'''
acp <- prcomp(Respuestas_encuesta_final, center = T, scale. = T)
print(acp)
plot(acp, type="l")
'''procedo a determinar el número de factores'''
AnalisisF <- factanal(data, factors = 4, rotation = "none", scores= "Bartlett")
AnalisisF


'''Análisis factorial confirmatorio'''

install.packages("semPlot")
install.packages("latticeExtra")
install.packages("readr") 
install.packages("lavaan")
library("semPlot")
library("readr")
library("lavaan")

datos <- (Respuestas_encuesta_final_1_)

modelo_confir0 <- 'vp =~ utilitario1	+ utilitario2	+ utilitario3	+ ries_perc1	+ ries_perc2	+ ries_perc3	+ Hedónico1	+ Hedónico2	+ Hedónico3	+ busq_sens1	+ busq_sens2	+ busq_sens3	+ social1	+ social2	+ social3	+ val_social1	+ val_social2	+ val_social3	+ uso_r_sociales1	+ uso_r_sociales2	+ uso_r_sociales3	+ uso_r_sociales4	+ uso_r_sociales5	+ Int_busq_info1	+ Int_busq_info2	+ Int_busq_info3	+ int_visita1	+ int_visita2	+ int_visita3	+ int_visita4 + cond_usual1 + cond_usual2 + cond_usual3 + cond_usual4 + cond_usual5 + cond_usual6 + cond_usual7 + cond_usual8 + cond_usual9 + cond_usual10 + cond_usual11 + cond_usual12 + determinantes1 + determinantes2 + determinantes3 + determinantes4 + determinantes5 + determinantes6 + determinantes7 + i&s1 + i&s2 + i&s3 + i&s4 + i&s5 + i&s6 + i&s7 + i&s8 + i&s9 + i&s10 + i&s11 + i&s12 + i&s13 + i&s14 + i&s15 + i&s16 + i&s17 + i&s18 + i&s19 + i&s20'
modelo_confir

modelo0 <- cfa(modelo_confir0, data = datos)
summary(modelo0, fit.measures=TRUE, rsq=TRUE)
parameterestimates(modelo0, standardized = TRUE)

semPaths(modelo0, what = "paths", layout = "circle", title = TRUE, style = "LISREL")
semPaths(modelo0, what = "est", layout = "circle", title = TRUE, style = "LISREL")
