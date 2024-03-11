###Trabajo final de espacial 
### librerias
library(sf)
library(tidyverse)
library(mvtnorm)
library(scatterplot3d)
library(car)
library(akima)
library(gstat)
library(geoR)
library(lattice)
library(maptools)
library(sp)
library(spatial)
library(graphics)
library(BCA)
library(aplpack)
library(scatterplot3d)
library(geoR)
library(fields)
library(rgdal)
require(maptools)
library(raster)
library(randomcoloR)
library(sf)
library(spdep)
require(RColorBrewer)
library(mapview)
library(readxl)
library(spatstat)
library(ggplot2)
library(ggmap)
require(MASS)
library(maptools)
library(RANN)
library(sp)
library(maptools)
library(spdep)
library(rgdal)
require(sf)
library(spData)
install.packages("mapview")
#######
radios <- st_read("https://bitsandbricks.github.io/data/CABA_rc.geojson")
deli<-st_read("~/Desktop/deli.geojson")
del <- read_excel("Desktop/del.xlsx")
reg<-st_read("~/Desktop/reg.geojson")
regespa <- read_excel("Documents/Datos de usuario de Microsoft/Office 2011 AutoRecovery/regespa.xlsx")
ate_ <- read_excel("Documents/ate_.xlsx")

View(regespa)
summary(radios)
summary(deli)
### plot

library(mapview)
devtools::install_github("r-spatial/mapview@develop")
mapView(radios)

ggplot() + geom_sf(data = radios)

ggplot() + geom_sf(data = radios)+
  geom_sf(data = deli,color="yellow",size=0.3) +
  labs(title = "5 dias de Delitos",
       subtitle = "Ciudad de Buenos Aires-2019")



cx<-del$lat
cy<-del$long
plot(patron, main="")
patron<-ppp(cy,cx,c(-58.554,-58.32),c(-34.71,-34.519))
plot(patron, main="Aleatorio",size=0.4)
summary(patron)
plot(density(patron),main="Robos ")
contour(density(patron),add=TRUE)

##
dpat<-as_data_frame(density(patron))
write.csv(dpat,file="dpat.csv")
densi<-st_read("~/Desktop/del1.geojson")
ggplot()+ geom_sf(data = densi, aes(color =value))
ggplot()+ geom_sf(data = densi, aes(color =value))+ geom_sf(data = barrios_geo, alpha = .01) +geom_smooth(se = FALSE) 

###
plot(deli$franja_horaria)
length(deli$franja_horaria[deli$franja_horaria== 12])
ggplot()+ geom_sf(data = barrios_geo)  + geom_sf(data = deli, aes(color =franja_horaria== 12),size=0.7)
ggplot()+ geom_sf(data = base_,aes(color=fran))
names(base_)
####



ggplot() + geom_sf(data = radios)+ geom_sf(data = deli,size = 0.3, aes(color=tipo_delito))

plot(deli)
ggplot() + geom_sf(data = radios, aes(fill = VIVIENDAS))
plot(radios)


ggplot() + geom_sf(data = radios, aes(fill = BARRIO), color = NA)
barrios_geo <- radios %>% 
  group_by(BARRIO) %>% 
  summarise(POBLACION = sum(POBLACION),
            VIVIENDAS = sum(VIVIENDAS),
            HOGARES = sum(HOGARES),
            HOGARES_NBI = sum(HOGARES_NBI),
            AREA_KM2 = sum(AREA_KM2))

ggplot() + geom_sf(data = barrios_geo)

View(barrios_geo)
ggplot(data = barrios_geo) +
  geom_sf()+
  geom_text_repel(mapping = aes(coords_x, coords_y, label = BARRIO), size = 4, min.segment.length = 0)

ggplot() +
  geom_sf(data = barrios_geo) +
  geom_sf(data = deli, color = "yellow",size=0.3) +
  labs(title = "5 dias de Delitos por Barrio",
       subtitle = "Ciudad de Buenos Aires")

ates<- del %>% 
  group_by(BARRIO) %>% 
  summarise(robos = sum(cantidad_registrada),lesiones=sum(lesiones))

write.csv(ates, file="ates.csv")

View(ates)
bar<- barrios_geo %>% left_join(ates)
barr<-bar%>% left_join(ates)

ats<- del %>% 
  group_by(BARRIO) %>% 
  summarise(fran =geometric(franja_horaria))
geometric<-function(x) exp(sum(log(x))/length(x))
base_<-barr%>% left_join(ats)
barrios_geo_ <-base_%>% left_join(ats)

barrios_geo<-barrios_geo_%>% left_join(ate_)
View(barrios_geo)

ggplot() +
  geom_sf(data = barr,aes(fill =robos )) +
  geom_sf(data = deli,size=0.3) +
  labs(title = "5 dias de Delitos por Barrio",
       subtitle = "Ciudad de Buenos Aires")



## RESUL
ggplot() +
  geom_sf(data =barrios_geo,aes(fill =robos )) +
  labs(title = "5 dias de Delitos por Barrio",
       subtitle = "Ciudad de Buenos Aires")


## modelo 1 robos

glmbaseLR <-glm(robos~ HOGARES_NBI ,family = negative.binomial(70.17), data=barrios_geo)
anova(glmbaseLR)
summary(glmbaseLR)
barrios_geo$residLR=residuals(glmbaseLR)
barrios_geo$residLRnn=residuals(glmbaseLRnn)
round(exp(coef(glmbaseLR)), 3)
## modelo 2  Hogares 

glmbaseLMR <-glm(HOGARES_NBI~  VIVIENDAS+HOGARES,family = poisson, data=barrios_geo)
#glmbaseLMR <-lm(HOGARES_NBI~  VIVIENDAS+HOGARES, data=barrios_geo)
anova(glmbaseLMR)
summary(glmbaseLMR)
barrios_geo$residLMR=residuals(glmbaseLMR)

mu_LR <- predict(glmbaseLR, type = "response")
mu_LR <- predict(glmbaseLR, type = "response")  # predict expected mean count
expLR <- sum(dpois(x = 0, lambda = mu_LR))     # sum the probabilities of a zero count for each mean
round(expLR)                                # predicted number of zeros

mu_LMR <- predict(glmbaseLR, type = "response")  # predict expected mean count
expLMR <- sum(dpois(x = 0, lambda = mu_LMR))     # sum the probabilities of a zero count for each mean
round(expLMR) 

rook_nb_b=nb2listw(poly2nb(barrios_geo,queen=FALSE), style="B",zero.policy = TRUE)# 1 matriz W
rook_nb_w=nb2listw(poly2nb(barrios_geo,queen=FALSE), style="W",zero.policy = TRUE)

queen_nb_b=nb2listw(poly2nb(barrios_geo,queen=TRUE), style="B",zero.policy = TRUE)
queen_nb_w=nb2listw(poly2nb(barrios_geo,queen=TRUE), style="W",zero.policy = TRUE)


regespa
xy0=data.frame(x=regespa$lat,y=regespa$long)
coordinates(xy0) <- c('x','y')
proj4string(xy0) <- CRS("+init=epsg:4326")
xy0=spTransform(xy0,CRS("+init=epsg:21897"))
#Graphs neighbours
trinb=tri2nb(xy0)

options(warn = -1)

tri_nb_b=nb2listw(tri2nb(xy0), style="B",zero.policy = TRUE)#1
tri_nb_w=nb2listw(tri2nb(xy0), style="W",zero.policy = TRUE)
soi_nb_b=nb2listw(graph2nb(soi.graph(trinb,xy0)), style="B",zero.policy = TRUE)
soi_nb_w=nb2listw(graph2nb(soi.graph(trinb,xy0)), style="W",zero.policy = TRUE)

relative_nb_b=nb2listw(graph2nb(relativeneigh(xy0), sym=TRUE), style="B",zero.policy = TRUE)
relative_nb_w=nb2listw(graph2nb(relativeneigh(xy0), sym=TRUE), style="W",zero.policy = TRUE)

gabriel_nb_b=nb2listw(graph2nb(gabrielneigh(xy0), sym=TRUE), style="B",zero.policy = TRUE)
gabriel_nb_w=nb2listw(graph2nb(gabrielneigh(xy0), sym=TRUE), style="W",zero.policy = TRUE)
#Distance neighbours

knn1_nb_b=nb2listw(knn2nb(knearneigh(xy0, k = 1)), style="B",zero.policy = TRUE) # como escoger la
knn1_nb_w=nb2listw(knn2nb(knearneigh(xy0, k = 1)), style="W",zero.policy = TRUE)
knn2_nb_b=nb2listw(knn2nb(knearneigh(xy0, k = 2)), style="B",zero.policy = TRUE)
knn2_nb_w=nb2listw(knn2nb(knearneigh(xy0, k = 2)), style="W",zero.policy = TRUE)
knn3_nb_b=nb2listw(knn2nb(knearneigh(xy0, k = 3)), style="B",zero.policy = TRUE)
knn3_nb_w=nb2listw(knn2nb(knearneigh(xy0, k = 3)), style="W",zero.policy = TRUE)
knn4_nb_b=nb2listw(knn2nb(knearneigh(xy0, k = 4)), style="B",zero.policy = TRUE)
knn4_nb_w=nb2listw(knn2nb(knearneigh(xy0, k = 4)), style="W",zero.policy = TRUE)

### graficos matrices W

par(mfrow=c(2, 2), mai=c(0,0,0,0))

#Physical neighbours
plot(barrios_geo$geometry,main="1.rook_nb_b")
plot(rook_nb_b,st_coordinates(reg), col="red", lwd=2, add=TRUE) #1


plot(barrios_geo$geometry,main="2.rook_nb_w")
plot(rook_nb_w,st_coordinates(reg), col="red", lwd=2, add=TRUE)#2

plot(barrios_geo$geometry,main="3.queen_nb_b")
plot(queen_nb_b,st_coordinates(reg), col="blue", lwd=2, add=TRUE)#3

plot(barrios_geo$geometry,main="4.queen_nb_w")
plot(queen_nb_w,st_coordinates(reg), col="blue", lwd=2, add=TRUE)#4

#Graphs neighbours

plot(barrios_geo$geometry,main="5.tri_nb_b")
plot(tri_nb_b,st_coordinates(reg), col="yellow", lwd=2, add=TRUE)#5<-aux

plot(barrios_geo$geometry,main="6.tri_nb_w")
plot(tri_nb_w,st_coordinates(reg), col="yellow", lwd=2, add=TRUE)#6

plot(barrios_geo$geometry,main="7.soi_nb_b")
plot(soi_nb_b,st_coordinates(reg), col="pink", lwd=2, add=TRUE)#7

plot(barrios_geo$geometry,main="8.soi_nb_w")
plot(soi_nb_w,st_coordinates(reg), col="pink", lwd=2, add=TRUE)#8

plot(barrios_geo$geometry,main="9.gabriel_nb_b")
plot(gabriel_nb_b,st_coordinates(reg), col="purple", lwd=2, add=TRUE)#9

plot(barrios_geo$geometry,main="10.gabriel_nb_w")
plot(gabriel_nb_w,st_coordinates(reg), col="purple", lwd=2, add=TRUE)#10

plot(barrios_geo$geometry,main="11.relative_nb_b")
plot(relative_nb_b,st_coordinates(reg), col="gray", lwd=2, add=TRUE)#11

plot(barrios_geo$geometry,main="12.relative_nb_w")
plot(relative_nb_w,st_coordinates(reg), col="gray", lwd=2, add=TRUE)#12

#Distance neighbours

plot(barrios_geo$geometry,main="13.knn1_nb_b")
plot(knn1_nb_b,st_coordinates(reg), col="darkgreen", lwd=2, add=TRUE)#13

plot(barrios_geo$geometry,main="14.knn1_nb_w")
plot(knn1_nb_w,st_coordinates(reg), col="darkgreen", lwd=2, add=TRUE)#14

plot(barrios_geo$geometry,main="15.knn2_nb_b")
plot(knn2_nb_b,st_coordinates(reg), col="darkgreen", lwd=2, add=TRUE)#15


plot(barrios_geo$geometry,main="16.knn2_nb_w")
plot(knn2_nb_w,st_coordinates(reg), col="darkgreen", lwd=2, add=TRUE)#16


plot(barrios_geo$geometry,main="17.knn3_nb_b")
plot(knn3_nb_b,st_coordinates(reg), col="darkgreen", lwd=2, add=TRUE)#17


plot(barrios_geo$geometry,main="18.knn3_nb_w")
plot(knn3_nb_w,st_coordinates(reg), col="darkgreen", lwd=2, add=TRUE)#18

plot(barrios_geo$geometry,main="19.knn4_nb_b")
plot(knn4_nb_b,st_coordinates(reg), col="darkgreen", lwd=2, add=TRUE)#19

plot(barrios_geo$geometry,main="20.knn4_nb_w")
plot(knn4_nb_w,st_coordinates(reg), col="darkgreen", lwd=2, add=TRUE)#20


#####################################################
moran.mc(barrios_geo$residLR,tri_nb_w, 999)
EBImoran.mc(barrios_geo$robos,barrios_geo$HOGARES_NBI,tri_nb_w, 999)



plotit <- function(nb, lab='') {
  plot(SpP, col='gray', border='white')
  plot(nb, xy, add=TRUE, pch=20)
  text(2.5, 2.8, paste0('(', lab, ')'), cex=1.25)
}
par(mfrow=c(1, 1), mai=c(0,0,0,0))
plotit(knn1_nb_b, 'adjacency')
plotit(wr2, 'lag-2 adj.')
plotit(wd10, '10 km')
plotit(wd25, '25 km')
plotit(k3, 'k=3')
plotit(k6, 'k=6')
##
#select

mat=list(rook_nb_b,rook_nb_w,
         queen_nb_b,queen_nb_w,
         tri_nb_b,tri_nb_w,
         soi_nb_b,soi_nb_w,
         gabriel_nb_b,gabriel_nb_w,
         relative_nb_b,relative_nb_w,
         knn1_nb_b,knn1_nb_w,
         knn2_nb_b,knn2_nb_w,
         knn3_nb_b,knn3_nb_w,
         knn4_nb_b,knn4_nb_w)
aux=numeric(0)


for(i in 1:length(mat))
  options(warn = -1)
{
  aux[i]=EBImoran.mc(barrios_geo$robos,barrios_geo$HOGARES_NBI,mat[[i]],nsim=999,zero.policy=F)$"statistic"
  which.max(aux)
  mat[[which.max(aux)]]
  }
which.max(aux)


#Defining W for mortality leukemia rate

maux=numeric(0)
for(i in 1:length(mat))
  options(warn = -1)
{
  maux[i]=EBImoran.mc(barrios_geo$robos,barrios_geo$HOGARES_NBI,mat[[i]],nsim=999,zero.policy=F)$"statistic"
  which.max(maux)
  mat[[which.max(maux)]]
  }

which.max(maux)

moran.test(barrios_geo$residLR, mat[[which.max(aux)]], alternative="two.sided")#Validating independence assumption
moran.test(barrios_geo$residLMR, mat[[which.max(maux)]], alternative="two.sided") #Validating independence assumption CASAS
moran.test(barrios_geo$robos, mat[[which.max(aux)]], alternative="two.sided")
moran.test(barrios_geo$HOGARES_NBI,mat[[which.max(aux)]], alternative="two.sided")
moran.test(barrios_geo$POBLACION,mat[[which.max(aux)]], alternative="two.sided")
moran.test(barrios_geo$VIVIENDAS,mat[[which.max(aux)]], alternative="two.sided")
moran.test(barrios_geo$HOGARES,mat[[which.max(aux)]], alternative="two.sided")
moran.test(barrios_geo$AREA_KM2,mat[[which.max(aux)]], alternative="two.sided")
moran.test(barrios_geo$fran,mat[[which.max(aux)]], alternative="two.sided")

moran.plot(barrios_geo$robos, listw =  knn4_nb_w)

#Testing spatial autocorrelation in covariates choosing a spatial weights matrix by maximising Moran test

for(i in 1:length(mat)){
  aux[i]=moran.test(barrios_geo$robos, mat[[i]], alternative="two.sided")$statistic
}
which.max(aux)

mat[[which.max(aux)]]



for(i in 1:length(mat)){
  aux[i]=moran.test(barrios_geo$HOGARES_NBI, mat[[i]], alternative="two.sided")$statistic
}
which.max(aux)

mat[[which.max(aux)]]


for(i in 1:length(mat)){
  aux[i]=moran.test(barrios_geo$VIVIENDAS, mat[[i]], alternative="two.sided")$statistic
}
which.max(aux)
mat[[which.max(aux)]]



for(i in 1:length(mat))
  options(warn = -1)
{
  aux[i]=EBImoran.mc(barrios_geo$robos,barrios_geo$HOGARES_NBI,mat[[i]],nsim=999,zero.policy=F)$"statistic"
  which.max(aux)
  mat[[which.max(aux)]]
  }

moran.test(barrios_geo$robos, mat[[which.max(aux)]], alternative="two.sided")

for(i in 1:length(mat)){
  aux[i]=moran.test(barrios_geo$robos, mat[[i]], alternative="two.sided")$statistic
}
which.max(aux)
mat[[which.max(aux)]]
for(i in 1:length(mat)){
  aux[i]=moran.test(barrios_geo$residLR, mat[[i]], alternative="two.sided")$statistic
}
which.max(aux)
mat[[which.max(aux)]]
data("barrios_geo")
x <- barrios_geo$HOGARES_NBI
y <- barrios_geo$robos
#======================================================
# Programming some functions

# Bivariate Moran's I
moran_I <- function(x, y = NULL, W){
  if(is.null(y)) y = x
  
  xp <- (x - mean(x, na.rm=T))/sd(x, na.rm=T)
  yp <- (y - mean(y, na.rm=T))/sd(y, na.rm=T)
  W[which(is.na(W))] <- 0
  n <- nrow(W)
  
  global <- (xp%*%W%*%yp)/(n - 1)
  local  <- (xp*W%*%yp)
  
  list(global = global, local  = as.numeric(local))
}

#Permutations for the Bivariate Moran's I
simula_moran <- function(x, y = NULL, W, nsims = 1000){
  
  if(is.null(y)) y = x
  
  n   = nrow(W)
  IDs = 1:n
  
  xp <- (x - mean(x, na.rm=T))/sd(x, na.rm=T)
  W[which(is.na(W))] <- 0
  
  global_sims = NULL
  local_sims  = matrix(NA, nrow = n, ncol=nsims)
  
  ID_sample = sample(IDs, size = n*nsims, replace = T)
  
  y_s = y[ID_sample]
  y_s = matrix(y_s, nrow = n, ncol = nsims)
  y_s <- (y_s - apply(y_s, 1, mean))/apply(y_s, 1, sd)
  
  global_sims  <- as.numeric( (xp%*%W%*%y_s)/(n - 1) )
  local_sims  <- (xp*W%*%y_s)
  
  list(global_sims = global_sims,
       local_sims  = local_sims)
}

mat.w.mat <- nb2mat(mat[[which.max(aux)]]$neighbours,mat[[which.max(aux)]]$weights,zero.policy=TRUE)
WTW=t(mat.w.mat)%*%mat.w.mat
W  <- as(WTW, "symmetricMatrix")
W  <- as.matrix(W)
W  <- as.matrix(W/rowSums(W))
W[which(is.na(W))] <- 0

m   <- moran_I(x, y, W)
m[[1]] # global value

m_i <- m[[2]]  # local values

local_sims <- simula_moran(x, y, W)$local_sims

alpha <- .05  # for a 95% confidence interval
probs <- c(alpha/2, 1-alpha/2)
intervals <- t( apply(local_sims, 1, function(x) quantile(x, probs=probs)))
sig        <- ( m_i < intervals[,1] )  | ( m_i > intervals[,2] )

#======================================================
# Preparing for plotting

barrios_geo    <- st_as_sf(barrios_geo)
barrios_geo$sig <- sig
# Identifying the LISA patterns
xp <- (x-mean(x))/sd(x)
yp <- (y-mean(y))/sd(y)

rob_hnbi <- as.character( interaction(xp > 0, W%*%yp > 0) ) 
rob_hnbi<- rob_hnbi%>% 
  str_replace_all("TRUE","High") %>% 
  str_replace_all("FALSE","Low")
rob_hnbi[barrios_geo$sig==0] <- "Not significant"
barrios_geo$rob_hnbi<- rob_hnbi
# Plotting############################# GRAFICA
ggplot() + geom_sf(data=barrios_geo, aes(fill=rob_hnbi), color="NA") +
  scale_fill_manual(values = c("red", "pink", "light blue", "dark blue", "grey95")) + 
  theme_minimal()


glmbaseLMR <-glm(HOGARES_NBI~  VIVIENDAS+HOGARES +robos+fran ,family = poisson, data=barrios_geo)
#
#Lagged covariates
#W=as.matrix(knn1_nb_w)
Wpeli=W%*%barrios_geo$robos_
Wrobos=W%*%barrios_geo$robos
WNBI=W%*%barrios_geo$HOGARES_NBI
Wviviendas=W%*%barrios_geo$VIVIENDAS
Whogares=W%*%barrios_geo$HOGARES
Wfran=W%*%barrios_geo$fran


MEpoisLR <- spatialreg::ME(robos~ HOGARES_NBI, data=barrios_geo ,family="poisson",listw=knn4_nb_w, alpha=0.2, verbose=TRUE)
MoranEigenVLR=data.frame(fitted(MEpoisLR))
fits<-data.frame(fitted(me.fit))
summary(MoranEigenVLR)
attach(MoranEigenVLR)
glmbaseLRm <-glm(robos~ HOGARES_NBI+fitted(MEpoisLR) ,family = poisson, data=barrios_geo)
summary(glmbaseLRm)

m1s = lagsarlm(glmbaseLR, data=barrios_geo, knn4_nb_w, tol.solve=1.0e-30)
summary(m1s)
m1d = lagsarlm(glmbaseLR, data=barrios_geo, knn4_nb_w, Durbin=TRUE, tol.solve=1.0e-30)
summary(m1d)
m1e = errorsarlm(glmbaseLR, data=barrios_geo, knn4_nb_w,  na.action = na.omit,tol.solve=1.0e-30)
summary(m1e)



MEpoisLMR <- spatialreg::ME(HOGARES_NBI~ HOGARES+VIVIENDAS, data=barrios_geo ,family="poisson",listw=knn4_nb_w, alpha=0.2, verbose=TRUE)
MoranEigenVLMR=data.frame(fitted(MEpoisLMR))
summary(MoranEigenVLMR)
attach(MoranEigenVLMR)
glmbaseLRmW <-glm(HOGARES_NBI~ HOGARES+VIVIENDAS+fitted(MEpoisLMR) ,family = poisson, data=barrios_geo)
summary(glmbaseLRmW)


m2s = lagsarlm(glmbaseLMR, data=barrios_geo, knn4_nb_w, tol.solve=1.0e-30)
summary(m2s)
m2d = lagsarlm(glmbaseLMR, data=barrios_geo, Durbin=TRUE ,knn4_nb_w, tol.solve=1.0e-30)
summary(m2d)
m2e = errorsarlm(glmbaseLMR, data=barrios_geo, knn4_nb_w,  na.action = na.omit,tol.solve=1.0e-30)
summary(m2e)

glmbaseLMR
AIC( glmbaseLR,glmbaseLRm,m1s,m1d,m1e) 
AIC(glmbaseLMR,glmbaseLRmW,m2s,m2d,m2e)
