pkg <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse", "raster" ,"sf" ,"ggspatial","cluster", "factoextra",
              "NbClust","tidyr", "semPlot", "semTools", "corrplot", "corrr",
              "haven", "psych", "dplyr", "lavaan", "readr", "cvms","tm","NLP",
              "SnowballC","RColorBrewer","wordcloud", "RefManageR",
              "bibliometrix","quanteda","ggplot2", "ggpubr","Factoshiny")

pkg(packages)


??Lavaan

'''Analisis Factorial exploratorio'''
rf <- read_excel("Documentos/AAA DOCTORADO/PROYECTO DOCTORAL/Respuestas_encuesta_final.xlsx")
data1 <- rf%>%select(uso_r_sociales1, uso_r_sociales2, uso_r_sociales3, uso_r_sociales4, uso_r_sociales5, Int_busq_info1, Int_busq_info2, Int_busq_info3, int_visita1, int_visita2, int_visita3, int_visita4, Sexo, Edad)

'''primero correr una tabla de correlación'''
mcorre <-cor(data1)
View(mcorre)
''' luego correr una matriz de varianza covarianza'''
cov(data1)

corrplot(cor(data1), order="hclust", tl.col='black', tl.cex=0.5)
'''aplico pruebas de bartlett, covarianza, kmo, etc'''
'''importante tener en cuenta que para kmo las variables deben estar por encima de
0.70 para ser excelente nivel de analisis factorial'''
bartlett.test(data1)

KMO(data1)
'''una vez obtenidos todos los indicadores se procede a identificar el
el número de factres recomendados, en especial por sistemas matemáticos'''

eigen(mcorre)
'''también puedo usar el analisis de componentes principales'''
acp <- prcomp(Respuestas_encuesta_final, center = T, scale. = T)
print(acp)
plot(acp, type="l")
'''AFE -> procedo a determinar el número de factores'''
AnalisisF <- factanal(data1, factors = 3, rotation = "varimax", scores= "Bartlett")
AnalisisF
 
'''Análisis factorial confirmatorio'''

modelo0 <- 'Uso_rd =~ uso_r_sociales5 + uso_r_sociales4 + uso_r_sociales3
Int_B =~ Int_busq_info1 + Int_busq_info2 + Int_busq_info3  
Int_V =~ int_visita1 + int_visita2 + int_visita3 + int_visita4
uso_r_sociales4 ~~ uso_r_sociales3
'

modelo0 <- 'a =~ uso_r_sociales2 + uso_r_sociales1 + uso_r_sociales5 + uso_r_sociales4 + uso_r_sociales3 + Int_busq_info1 + Int_busq_info2 + Int_busq_info3 + int_visita1 + int_visita2 + int_visita3 + int_visita4

'

CFA1<- cfa(modelo0, orthogonal= FALSE, data = data1,
           estimator= "MLM")

#modelo0 <- sem(modelo0, data = data1)
summary(CFA1, fit.measures=TRUE, rsq=TRUE)
#parameterestimates(modelo0, standardized = TRUE)
#fitMeasures(CFA1)

#semPaths(CFA1, what = "paths", layout = "tree", title = TRUE, style = "LISREL")
#semPaths(CFA1, what = "est", layout = "tree", title = TRUE, style = "LISREL")
semPaths(CFA1, intercepts = FALSE,edge.label.cex=1.5, optimizeLatRes = TRUE, groups = "lat",pastel = TRUE, exoVar = FALSE, sizeInt=5,edge.color ="black",esize = 6, label.prop=2,sizeLat = 6,"std", layout="circle2")

  fitMeasures(CFA1,
            c("chisq", "df", "rmsea", "tli", "cfi", "aic", "nfi", "ifi", "agfi", "gfi") )
  fitMeasures(CFA1)
modindices(CFA1)
#Invariance 1 Sexo
#conf1<-cfa(modelo0, orthogonal=FALSE, data= data1, estimator="WLSMV",group="Sexo", ordered =names(data1))
#summary(conf1, fit.measures=TRUE)
#fitMeasures(conf1,
#            c( 'rmr','rmsea','gfi','agfi', 'nfi','cfi', 'ifi' ) )

