# Biogeograohy with R-----
rm(list = ls())
## Load the packages
library(terra)
library(rgeos)
## Import the data point 
Thunb<-read.csv("/Users/jm200/Library/CloudStorage/Dropbox/Biogeography in R/Project/Coordinates.csv")
Thunbcoord<-Thunb[,1:2]
coord<-vect(Thunbcoord,geom=c("Longitude","Latitude"), crs="epsg:4326")
# rascoord<-sp::rasterize(coord)
# coorraster<-rast(coor, nrow=12, ncol=12)
coordinates(Thunbcoord) <- ~ Longitude + Latitude
CRS1 <- CRS("+init=epsg:4326") # WGS 84
crs(Thunbcoord) <- CRS1

plot(coord)

## Load the elevation raster from https://www.worldclim.org/data/worldclim21.html
elev <- rast("/Users/jm200/Library/CloudStorage/Dropbox/Biogeography in R/Project/wc2.1_30s_elev.tif")  
plot(elev)

## Elevation data for each site 
Thunbelev<-raster::extract(elev,coord)

## Population density 

pop_ben <- list.files("/Users/jm200/Library/CloudStorage/Dropbox/Biogeography in R/Project/BEN_msk_pop", pattern="\\.grd$", full.names=TRUE,recursive=TRUE)
raster_pop_ben<-raster(pop_ben)
plot(raster_pop_ben)
Tunbpop<-raster::extract(raster_pop_ben,Thunbcoord)
Tunbpop<-data.frame(Population=Tunbpop)
## Habitat heterogeneity 
habitat_list <- list.files("/Users/jm200/Library/CloudStorage/Dropbox/Biogeography in R/Project/Habitat heterogeneity",pattern=".tif$",full.names = T)# load bioclimatic layers
habitat <- raster::stack(habitat_list) # put all the raster together
plot(habitat)
habitat_value <- raster::extract(habitat,Thunbcoord)
colnames(habitat_value)<-c("CV","evenness","homogeneity")
# Human footprint
# he human pressure is measured using eight variables including built-up environments, population density, electric power infrastructure, crop lands, pasture lands, roads, railways, and navigable waterways. It is expressed on a scale of 0 (low) to 50 (high footprint). 
# geodata::footprint(year=2009, path="/Users/jm200/Library/CloudStorage/Dropbox/Biogeography in R/Project/Human footprint") 

human_list <- list.files("/Users/jm200/Library/CloudStorage/Dropbox/Biogeography in R/Project/Human footprint",pattern=".tif$",full.names = T)# load bioclimatic layers
human <- raster::stack(human_list) # put all the raster together
plot(human)
human_value <- raster::extract(human,Thunbcoord)
colnames(human_value)<-c("footprint")
## Distance from each point to the road 
# roads <- vect("/Users/jm200/Library/CloudStorage/Dropbox/Biogeography in R/Project/BEN_rds/BEN_roads.shp")
# class(roads)
# plot(roads)
# plot(coord,add=TRUE,col="blue")
# distance(roads,coord)

## Lambda 
load("/Users/jm200/Library/CloudStorage/Dropbox/Biogeography in R/Project/lambda_damping.rda")
View(lambda_damping)

lambda<-lambda_damping[-3,1]
lambda<-as.data.frame(lambda)  

## Final data
Thunbgeomhd<-read.csv("/Users/jm200/Library/CloudStorage/Dropbox/Biogeography in R/Project/distance.csv")

Treecover<-read.csv("/Users/jm200/Library/CloudStorage/Dropbox/Biogeography in R/Project/Coordinates_tree.csv")

data<-data.frame(lambda,human_value,habitat_value,Tunbpop,elevation=Thunbelev[,2],Thunbgeomhd,Thunb,Treecover=Treecover[,16])

## Testing for spatial auto-correlation using the Moran I 

lambdatable<-data[,c(1,11,12)]
lambdatable_dists <- as.matrix(dist(cbind(lambdatable$Longitude, lambdatable$Latitude)))
lambdatable_dists_inv <- 1/lambdatable_dists
diag(lambdatable_dists_inv) <- 0
lambdatable_dists_inv[1:5, 1:5]
Moran.I(lambdatable$lambda, lambdatable_dists_inv)

# Based on these results, we can validate the null hypothesis that there is zero spatial autocorrelation present in the variable lambda at alpha = .05.


## Regression
### One predictor

modmhd<-lm(lambda~mhd,data =data )
summary(modmhd)

# plot(data$CV,data$evenness)

modfootprint<-lm(lambda~footprint,data =data )
summary(modfootprint)
modCV<-lm(lambda~CV,data =data )
summary(modCV)
modfootevenness<-lm(lambda~evenness,data =data )
summary(modfootevenness)
modfoothomogeneity<-lm(lambda~homogeneity,data =data )
summary(modfoothomogeneity)
modfootPopulation<-lm(lambda~Population,data =data )
summary(modfootPopulation)
modfootelevation<-lm(lambda~elevation,data =data )
summary(modfootelevation)
modtreecover<-lm(lambda~Treecover,data =data )
summary(modtreecover)

## Plot the regression line 

## Plot the most significant relationship
fig1<-ggplot(data, aes(x=CV, y=lambda)) +
  geom_point(bg = "#E69F00", pch = 21, col = "#E69F00", cex = 2) +
  # geom_hline(yintercept = 1, lty=2,
             # color = "#999999")+
  # annotate("text", x = c(4.4, 4.4), y = c(00.02,-0.05),size=2.5,
  #          label = c(expression(R^2 == 0.21 ),expression( beta== 0.32)))+
  xlab(" Habitat heterogeneity (CV) ") +
  ylab(expression(paste("Population growth rate, ", lambda))) +
  ggplot2::stat_smooth()+
  ggtitle("")+
  theme_bw() +
  theme(legend.position="none", panel.border = element_rect(colour = "black", fill=NA, size=0.5),panel.grid.major = element_blank(),panel.grid.minor = element_blank()
  )

ggplot(data, aes(CV, lambda) ) +
  geom_point() +
  stat_smooth()





attach(data)
plot(data$CV, data$lambda, pch = 16, cex = 1.3, col = "#E69F00", xlab = "Habitat heterogeneity (CV)", ylab = expression(paste("Population growth rate, ", lambda)))
abline(lm(data$lambda ~ data$CV))


fig2<-ggplot(data, aes(x=evenness, y=lambda)) +
  geom_point(bg = "#E69F00", pch = 21, col = "#E69F00", cex = 2) +
  geom_hline(yintercept = 1, lty=2,
             color = "#999999")+
  # annotate("text", x = c(4.4, 4.4), y = c(00.02,-0.05),size=2.5,
  #          label = c(expression(R^2 == 0.21 ),expression( beta== 0.32)))+
  xlab(" Habitat heterogeneity (CV) ") +
  ylab(expression(paste("Population growth rate, ", lambda))) +
  # geom_smooth(method = lm, se = TRUE, fill = "#E69F00",color="#E69F00") +
  ggtitle("")+
  theme_bw() +
  theme(legend.position="none", panel.border = element_rect(colour = "black", fill=NA, size=0.5),panel.grid.major = element_blank(),panel.grid.minor = element_blank()
  )







fig2<-ggplot(dataplotw, aes(x=log(mhd), y=log(Lambda)))+
  geom_point(bg = "#E69F00", pch = 21, col = "#E69F00", cex = 2) +
  annotate("text", x = c(2.5, 2.5), y = c(00.02,-0.05),size=2.5,
           label = c(expression(R^2 == 0.005 ),expression( beta== -0.03)))+
  xlab("log (Climatic distance)") +
  ylab("") +
  ggtitle("")+
  theme_bw() +
  theme(legend.position="none", panel.border = element_rect(colour = "black", fill=NA, size=0.5),panel.grid.major = element_blank(),panel.grid.minor = element_blank()
  )

fig3<-ggplot(dat_regfit, aes(x=log(N), y=log(Lambda))) +
  geom_point(bg = "#E69F00", pch = 21, col = "#E69F00", cex = 2) +
  annotate("text", x = c(2.8, 2.8), y = c(0.30,0.20),size=2.5,
           label = c(expression(R^2 == 0.575 ),expression( beta== -0.421)))+
  xlab("log (Soil nitrogen/g)") +
  ylab(expression(paste("Population growth rate, ", log (lambda)))) +
  geom_smooth(method = lm, se = TRUE, fill = "#E69F00",color="#E69F00", lwd = 1) +
  geom_line(aes(x = log(dat_regfit$N), y = dat_regfit$lwr), 
            linetype = 2,color = "#E69F00") +
  geom_line(aes(x = log(dat_regfit$N), y = dat_regfit$upr), 
            linetype = 2,color = "#E69F00")+
  ggtitle("")+
  theme_bw() +
  theme(legend.position="none", panel.border = element_rect(colour = "black", fill=NA, size=0.5),panel.grid.major = element_blank(),panel.grid.minor = element_blank()
  )

fig4<-ggplot(dataplotw, aes(x=log(P), y=log(Lambda))) +
  geom_point(bg = "#E69F00", pch = 21, col = "#E69F00", cex = 2) +
  annotate("text", x = c(2.6, 2.6), y = c(0.02,-0.05),size=2.5,
           label = c(expression(R^2 ==  0.085 ),expression( beta==-0.511)))+
  xlab("log (Soil phosphorus/g)") +
  ylab("") +
  # geom_smooth(method = lm, se = TRUE, color = "black", lwd = 0.3) +
  ggtitle("")+
  theme_bw() +
  theme(legend.position="none", panel.border = element_rect(colour = "black", fill=NA, size=0.5),panel.grid.major = element_blank(),panel.grid.minor = element_blank()
  )

pdf("/Users/jmoutouama/Dropbox/PhD Project/Chapter3/Demography Thunbergia/Figure/2Revision/lambda_distance.pdf",width=5,height=5)
(figure1 <- ggarrange(fig1, fig2,fig3,fig4,
                      hjust = -2, labels = c("(a)", "(b)","(c)","(d)"), common.legend = TRUE, legend = "right", font.label = list(size = 14, color = "black", face = "plain", family = NULL), vjust = 2, widths = c(2, 2), heights = 0.5, align = "hv",
                      ncol = 2, nrow = 2
))

dev.off()






### Interaction
modfootprintmhd<-lm(lambda~footprint*mhd,data =data )
summary(modfootprintmhd)
modCVmhd<-lm(lambda~CV*mhd,data =data )
summary(modCVmhd)
modfootevennessmhd<-lm(lambda~evenness*mhd,data =data )
summary(modfootevennessmhd)
modfoothomogeneitymhd<-lm(lambda~homogeneity*mhd,data =data )
summary(modfoothomogeneitymhd)
modfootPopulationmhd<-lm(lambda~Population*mhd,data =data )
summary(modfootPopulationmhd)
modfootelevationmhd<-lm(lambda~elevation*mhd,data =data )
summary(modfootelevationmhd)

# Plot 


