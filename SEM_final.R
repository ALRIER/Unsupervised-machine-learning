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
setwd('home/alrier/carpeta personal')
rf <- read_excel("Documentos/AAA DOCTORADO/PROYECTO DOCTORAL/Respuestas_encuesta_final_1.xlsx")
data1 <- rf%>%select(utilitario1, utilitario3, ries_perc1,
                                            ries_perc2, Hedónico1, Hedónico2, busq_sens1,
                                            busq_sens2, busq_sens3, social2, social1, 
                                            social3, uso_r_sociales5, uso_r_sociales3,
                                            Int_busq_info1, Int_busq_info3, int_visita1, 
                                            int_visita2, int_visita3, int_visita4, Sexo, Edad)
'''primero correr una tabla de correlación'''
mcorre <-cor(data1)
View(mcorre)
write.csv(x = mcorre, file = "correlaciones.csv", row.names = T)
''' luego correr una matriz de varianza covarianza'''
cov(data1)
corrplot(cor(data1), order="hclust", tl.col='black', tl.cex=0.5)

c1<- rf%>%select(social2, social1, social3)
c1<-as.data.frame(c1)
social<-alpha(c1)
social
c2<- rf%>%select(utilitario1, utilitario3)
c2<-as.data.frame(c2)
utilitario<-alpha(c2)
utilitario
c3<- rf%>%select(ries_perc1, ries_perc2)
c3<-as.data.frame(c3)
riesgo<-alpha(c3)
riesgo
c4<- rf%>%select(Hedónico1, Hedónico2)
c4<-as.data.frame(c4)
hedonico<-alpha(c4)
hedonico
c5<- rf%>%select(busq_sens1, busq_sens2, busq_sens3)
c5<-as.data.frame(c5)
busq<-alpha(c5)
busq
c6<- rf%>%select(Int_busq_info1, Int_busq_info3)
c6<-as.data.frame(c6)
busqinfo<-alpha(c6)
busqinfo
c7<- rf%>%select(int_visita1, int_visita2, int_visita3, int_visita4)
c7<-as.data.frame(c7)
int_visita<-alpha(c7)
int_visita

'''aplico pruebas de bartlett, covarianza, kmo, etc'''
'''importante tener en cuenta que para kmo las variables deben estar por encima de
0.70 para ser excelente nivel de analisis factorial'''
bartlett.test(data1)
?bartlett.test
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

'''SEM'''
modelo0 <- ' 
V_U =~ utilitario1 + utilitario3
R_P =~ ries_perc1	+ ries_perc2 
V_H =~ Hedónico1	+ Hedónico2	 
B_S =~ busq_sens1	+ busq_sens2	+ busq_sens3
V_S =~ social1 + social3 + social2
I_B =~ Int_busq_info1 + Int_busq_info3  
I_V =~ int_visita1 + int_visita2 + int_visita3 + int_visita4
Hedónico2 ~~ busq_sens1'

SEM <- sem(modelo0, orthogonal= FALSE, data = rf,
           estimator= "MLM")

summary(SEM, fit.measures=TRUE, rsq=TRUE)
#parameterestimates(modelo0, standardized = TRUE)
#fitMeasures(CFA1)
 
p<-semPaths(SEM,intercepts = F, edge.label.cex=2, 
         optimizeLatRes = T, groups = "lat", 
         pastel = T, sizeInt=2, edge.color ="black", esize = 3, 
         label.prop=2, sizeLat = 3,"std", layout="tree2",
         style = "lisrel", sizeMan = 2,
         edge.label.bg = F, edge.width = 1)
plot(p)
p

#setwd()
#png(file="fig1.png")
#p
#dev.off()

#bmp()
#jpeg()
#tiff()
#pdf()
#ps()
#svg()


#png(file="fig1.png",height=600,width=600)
#p
#dev.off()

#ggsave("figura2.pdf")


#ggsave(
 # "figura2.pdf",
  #plot = last_plot(),
  #device = NULL, #"eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf" (windows only).
  #path = NULL,
  #scale = 1,
  #width = NA,
  #height = NA,
  #units = c("in", "cm", "mm"),
  #dpi = 300,
  #limitsize = TRUE,
#)



#crear figura en power point  modificable
#primero pasamos nuestro objeto p a formato dml
#esto dentro del paquete rvg
p_dml <- rvg::dml(ggobj = p)

officer::read_pptx() %>%
  # añadir diapositiva ----
officer::add_slide() %>%
  # especificar objeto y lugar ----
officer::ph_with(p_dml, ph_location()) %>%
  # exportar diapositiva -----
base::print("demostracion.pptx")













#############################################

str<-standardizedsolution(SEM)
str
write.csv(x = str, file = "str1.csv", row.names = T)
semPaths(SEM)
fitMeasures(SEM,
            c("chisq", "df", "rmsea", "tli", "cfi", "aic", "nfi", "ifi", "agfi", "gfi") )
fitMeasures(SEM)
modindices(SEM, sort. = TRUE)


######################modelo0.configural
modelo0.conf <- measEq.syntax(configural.model = modelo0, estimator="MLM", ID.fac = "std.lv", parameterization = "theta",  group = "Sexo", orthogonal=FALSE, data=data1,
                              ID.cat = "Wu.Estabrook.2016",return.fit=TRUE,group.equal = c("thresholds"))

summary(modelo0.conf, fit.measures=TRUE)

fitMeasures(modelo0.conf,
            c("chisq", "df", "rmsea", "tli", "cfi", "aic") )

modindices(modelo0.conf)
######################modelo0.metric
modelo0.metric<- measEq.syntax(configural.model = modelo0 ,estimator="MLM", ID.fac = "std.lv", parameterization = "theta",  group = "Sexo", orthogonal=FALSE, data=data1, parameterization = "theta",
                               ID.cat = "Wu.Estabrook.2016",return.fit=TRUE, group.equal = c("thresholds","loadings"),long.equal  = c("thresholds","loadings"))
summary(modelo0.metric, fit.measures=TRUE)
fitMeasures(modelo0.metric,
            c("chisq", "df", "rmsea", "tli", "cfi", "aic") )
lavTestScore(modelo0.metric)
######################modelo0.escalar
modelo0.scalar<- measEq.syntax(configural.model = modelo0 ,estimator="MLM", ID.fac = "std.lv", parameterization = "theta",  group = "Sexo", orthogonal=FALSE, data=data1, parameterization = "theta",
                               ID.cat = "Wu.Estabrook.2016",return.fit=TRUE, group.equal = c("thresholds","loadings","intercepts"),long.equal  = c("thresholds","loadings","intercepts"))
summary(modelo0.scalar, fit.measures=TRUE)
fitMeasures(modelo0.scalar,
            c("chisq", "df", "rmsea", "tli", "cfi", "aic") )
lavTestScore(modelo0.scalar)
######################modelo0.estricto
modelo0.stric<- measEq.syntax(configural.model = modelo0 ,estimator="MLM", ID.fac = "std.lv", parameterization = "theta",  group = "Sexo", orthogonal=FALSE, data=data1, parameterization = "theta",
                              ID.cat = "Wu.Estabrook.2016",return.fit=TRUE, group.equal = c("thresholds","loadings","intercepts","residuals"),long.equal  = c("thresholds","loadings","intercepts","residuals"))
summary(modelo0.stric, fit.measures=TRUE)
fitMeasures(modelo0.stric,
            c("chisq", "df", "rmsea", "tli", "cfi", "aic") )
lavTestScore(modelo0.stric)

#datos mejor organizados
lavaan::anova(modelo0.stric,modelo0.scalar,modelo0.metric,modelo0.conf)

fit.stats <- rbind(fitmeasures(modelo0.conf, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic")),
                   fitmeasures(modelo0.metric, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic")),
                   fitmeasures(modelo0.scalar, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic")),
                   fitmeasures(modelo0.stric, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic")))
rownames(fit.stats) <- c("configural", "metric","scalar", "strict")
fit.stats


#Invariance 2 Edad
######################modelo0.configural
modelo0.conf.ed <- measEq.syntax(configural.model = modelo0, estimator="MLM", ID.fac = "std.lv", parameterization = "theta",  group = "Edad", orthogonal=FALSE, data=data1,
                                 ID.cat = "Wu.Estabrook.2016",return.fit=TRUE,group.equal = c("thresholds"))

summary(modelo0.conf.ed, fit.measures=TRUE)
fitMeasures(modelo0.conf.ed,
            c("chisq", "df", "rmsea", "tli", "cfi", "aic") )
modindices(modelo0.conf.ed, sort. = TRUE)

######################modelo0.parcial I
modelo0.conf.parcial <- measEq.syntax(configural.model = modelo0, estimator="MLM", ID.fac = "std.lv", parameterization = "theta",  group = "Sexo", orthogonal=FALSE, data=data1,
                                      ID.cat = "Wu.Estabrook.2016",return.fit=TRUE,group.equal = c("thresholds"), group.partial=c("Int_busq_info3 ~~    int_visita4"))

summary(modelo0.conf.parcial, fit.measures=TRUE)
fitMeasures(modelo0.conf.parcial,
            c("chisq", "df", "rmsea", "tli", "cfi", "aic") )


######################modelo0.metric
modelo0.metric.ed<- measEq.syntax(configural.model = modelo0 ,estimator="MLM", ID.fac = "std.lv", parameterization = "theta",  group = "Edad", orthogonal=FALSE, data=data1, parameterization = "theta",
                                  ID.cat = "Wu.Estabrook.2016",return.fit=TRUE, group.equal = c("thresholds","loadings"),long.equal  = c("thresholds","loadings"), group.partial=c("Int_B =~ uso_r_sociales3"))
summary(modelo0.metric.ed, fit.measures=TRUE)
fitMeasures(modelo0.metric.ed,
            c("chisq", "df", "rmsea", "tli", "cfi", "aic") )
lavTestScore(modelo0.metric.ed)
######################modelo0.escalar
modelo0.scalar.ed<- measEq.syntax(configural.model = modelo0 ,estimator="MLM", ID.fac = "std.lv", parameterization = "theta",  group = "Edad", orthogonal=FALSE, data=data1, parameterization = "theta",
                                  ID.cat = "Wu.Estabrook.2016",return.fit=TRUE, group.equal = c("thresholds","loadings","intercepts"),long.equal  = c("thresholds","loadings","intercepts"), group.partial=c("Int_B =~ uso_r_sociales3", "uso_r_sociales5 ~~ int_visita1"))
summary(modelo0.scalar.ed, fit.measures=TRUE)
fitMeasures(modelo0.scalar.ed,
            c("chisq", "df", "rmsea", "tli", "cfi", "aic") )
modindices(modelo0.scalar.ed)
######################modelo0.estricto
modelo0.stric.ed<- measEq.syntax(configural.model = modelo0 ,estimator="MLM", ID.fac = "std.lv", parameterization = "theta",  group = "Edad", orthogonal=FALSE, data=data1, parameterization = "theta",
                                 ID.cat = "Wu.Estabrook.2016",return.fit=TRUE, group.equal = c("thresholds","loadings","intercepts","residuals"),long.equal  = c("thresholds","loadings","intercepts","residuals"))
summary(modelo0.stric.ed, fit.measures=TRUE)
fitMeasures(modelo0.stric.ed,
            c("chisq", "df", "rmsea", "tli", "cfi", "aic") )
lavTestScore(modelo0.stric.ed)

#datos mejor organizados
lavaan::anova(modelo0.stric.ed,modelo0.conf.parcial,modelo0.scalar.ed,modelo0.metric.ed,modelo0.conf.ed)

fit.stats <- rbind(fitmeasures(modelo0.conf.ed, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic")),
                   fitmeasures(modelo0.conf.parcial, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic")),
                   fitmeasures(modelo0.metric.ed, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic")),
                   fitmeasures(modelo0.scalar.ed, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic")),
                   fitmeasures(modelo0.stric.ed, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic")))
rownames(fit.stats) <- c("configural","Parcial" ,"metric","scalar", "strict")
fit.stats

d<-parTable(model3)
modindices(model3)
model3partial<-cfa(fivefactors, orthogonal=FALSE, data=DFinvar, estimator="MLM",group="Sex", group.equal=c("loadings","intercepts"), group.partial=c("UCLA3~1"))
summary(model3partial,fit.measures=TRUE)
