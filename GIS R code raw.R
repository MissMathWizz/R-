install.packages ('raster')
install.packages ('sf')
install.packages ('tidyverse')
install.packages ('ggplot2')
install.packages ('srvyr')
install.packages ('Hmisc')
install.packages ('rgdal')
install.packages ("exactextractr")
require(rgdal)
require(Hmisc)
require(raster)
require(sf)
require(tidyverse)
require(tidygeocoder)
require(ggplot2)
require(srvyr)
library(dplyr)
library(tidyr)
require(exactextractr)
Ethiopia_2011 <- read.csv("~/R/Data/Ethiopia_2011.csv")
View(Ethiopia_2011)

#a.weighted descriptive data by Household Head Sex (femhead)

Mean_ETH<- Ethiopia_2011 %>%group_by(femhead)%>%
  summarise_at(vars(urban, agehead:mean_rain_lt), list(~wtd.mean(.,weight)))
write.csv(Mean_ETH, "Mean_ETH1.csv")

var_ETH<- Ethiopia_2011 %>%group_by(femhead)%>%
  summarise_at(vars(urban, agehead:mean_rain_lt), list(~wtd.var(.,weight)))
write.csv(var_ETH, "var_ETH1.csv")

quantile_ETH<- Ethiopia_2011 %>%group_by(femhead)%>%
  summarise_at(vars(urban, agehead:mean_rain_lt), list(~wtd.quantile(.,weight)))
write.csv(quantile_ETH, "quantile_ETH1.csv")

#t test

ee<-Ethiopia_2011 %>% 
  select(weight,femhead, urban, agehead:mean_rain_lt) %>% 
  gather(key = variable, value = value, -femhead) %>% 
  group_by(femhead, variable) %>% 
  summarise(value = list(value)) %>% 
  spread(femhead, value) %>% 
  group_by(variable)

colnames(ee)[2] <- 'female'

colnames(ee)[3] <- 'male'

ETH_ttest <- ee%>%
  mutate(p_value = t.test(unlist(female), unlist(male))$p.value)

ETH_t<-ETH_ttest%>%select(-female,-male)%>%mutate(sig= ifelse( p_value<0.05,1,0))
write.csv(ETH_t, "ETH_sig1.csv")

#b

#unzip gz filse 
install.packages ('R.utils')
library(R.utils)

c4<-'chirps/chirps-v2.0.2011.04.tif.gz'
R.utils::gunzip(c4, remove = FALSE)

c5<-'chirps/chirps-v2.0.2011.05.tif.gz'
R.utils::gunzip(c5, remove = FALSE)

c6<-'chirps/chirps-v2.0.2011.06.tif.gz'
R.utils::gunzip(c6, remove = FALSE)

c7<-'chirps/chirps-v2.0.2011.07.tif.gz'
R.utils::gunzip(c7, remove = FALSE)

c8<-'chirps/chirps-v2.0.2011.08.tif.gz'
R.utils::gunzip(c8, remove = FALSE)

c9<-'chirps/chirps-v2.0.2011.09.tif.gz'
R.utils::gunzip(c9, remove = FALSE)

c10<-'chirps/chirps-v2.0.2011.10.tif.gz'
R.utils::gunzip(c10, remove = FALSE)

c11<-'chirps/chirps-v2.0.2011.11.tif.gz'
R.utils::gunzip(c11, remove = FALSE)

c12<-'chirps/chirps-v2.0.2011.12.tif.gz'
R.utils::gunzip(c12, remove = FALSE)


#stack rasters tif files
setwd("C:/Users/stant/Documents/R/Data/")
getwd()

f49<- paste0('chirps/chi/chirps-v2.0.2011.0', 4:9, ".tif")
f49
f12 <- paste0('chirps/chi/chirps-v2.0.2011.', 10:12, ".tif")
f12

l49<-stack(f49)
l12<-stack(f12)
lll<-stack(l49,l12)
names(lll) <- paste0("band_", 1:9)
crs(lll)

spm<-sum(lll)/3

ETH.sf<-st_read("./Ethiopia_shape_ADM3/ETH_adm3.shp",
                quiet = T)
corte.precipitacion <- raster::crop(spm, extent(ETH.sf)) 
precip.mask <- mask(x = corte.precipitacion, mask = ETH.sf)
writeRaster(precip.mask,'smp.tif')


plot(precip.mask, main= "Seasonal percipitation mean CHIRPS Apr-Dec 2011 [mm]")


#Open ERSS and make it spatially aware

Ethiopia_2011 <- read.csv("~/R/Data/Ethiopia_2011.csv")

Ethiopia_2011<-Ethiopia_2011 %>% filter(!(is.na(lat_mod_11) & is.na(lon_mod_11)))

ETH_sf<-st_as_sf(Ethiopia_2011, 
                    coords = c("lat_mod_11", "lon_mod_11"),
                    crs = 4326)%>%st_transform(st_crs(ETH.sf))


# Making rasters for different agricultural season for product Meher

#S2 sowing --> ERRS
s2<-sum(lll[[1]], lll[[2]],lll[[3]])
corte.precipitacion2 <- raster::crop(s2, extent(ETH)) 
precip.mask2 <- mask(x = corte.precipitacion2, mask = ETH)
writeRaster(precip.mask2,'s2.tif')

ETH.ex2 <- raster::extract(precip.mask2, ETH.sf,
                           fun=mean, na.rm=T, df=T)
ETH.sf <- ETH.sf %>%left_join(ETH.ex2, by = "ID")
ETH.sf
ETH.sf<-ETH.sf%>%rename(s2=layer)

#S3 growing --> ERRS
s3<-sum(lll[[4]], lll[[5]],lll[[6]])
corte.precipitacion3 <- raster::crop(s3, extent(ETH)) 
precip.mask3 <- mask(x = corte.precipitacion3, mask = ETH)
writeRaster(precip.mask3,'s3.tif')


#S4 harvesting --> ERRS
s4<-sum(lll[[7]], lll[[8]],lll[[9]])
corte.precipitacion4 <- raster::crop(s4, extent(ETH)) 
precip.mask4 <- mask(x = corte.precipitacion4, mask = ETH)
writeRaster(precip.mask4,'s4.tif')

#zonal statistic smp into ERRS survey (failed, I used QGIS instead)

crs(precip.mask)<-crs(ETH)
projection(ETH)
st_crs(ETH_sf)<- "+proj=longlat +datum=WGS84 +no_defs"

ETH.ex <- raster::extract(precip.mask, ETH.sf,
                          fun=mean, na.rm=T, df=T)

ETH.sf <- ETH.sf %>% mutate(ID = 1:nrow(.)) %>%left_join(ETH.ex, by = "ID")%>%rename(smp=layer)

#Used "point sampling tool" (zonal statistics) in QGIS to extract rasters percip.mask and percip.mask234 to ERRS points coordinate
#generated an excel file Rain.xlsx

#c
#Merge and clean datasets (Rain.xlsx and ERRS dataset)
library(readxl)
Rain <- read_excel("Rain.xlsx")
View(Rain)

Ethiopia_2011 <- read.csv("~/R/Data/Ethiopia_2011.csv")


ETH11<-merge(Rain, Ethiopia_2011, key="ea_id")

ETH11<-ETH11%>%select(-household_)

ETH11<-ETH11%>%mutate( sd= mean_rain_lt*covrain_lt)


#Making SPI <- drought occurance index
ETH11<- ETH11%>%mutate(SPI=(smp-mean_rain_lt)/sd, SPI2=(s2-mean_rain_lt)/sd, SPI3=(s3-mean_rain_lt)/sd, SPI4=(s4-mean_rain_lt)/sd)

head(ETH11)

write.csv(ETH11, "ETH11.csv")

ETH111<-ETH11%>%select(-sd,-ea_id, -fid,-household_id, -lat_mod_11, -lon_mod_11,-kebele, -town, -subcity)
write.csv(ETH111, "ETH111.csv")

#check correlations 
nums <- unlist(lapply(ETH111, is.numeric))  

corm<-ETH111[ , nums]
res <- cor(corm)
round(res, 2)
install.packages("corrplot")
library(corrplot)

res[is.na(res)] <- 0
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#d
#Run regressions
FImonth<-lm(nmonth_food_insecure~ free_food*as.factor(region)+femhead+urban*femhead+SPI+
         SPI2+SPI3+SPI4+agehead*femhead+hhlabor*femhead
       +hhszae*femhead+educhead*femhead+educave15_60*femhead+safewater+landown*femhead
       +TLU_total*femhead+dist_market+improved_maize+fert_inorg_use
       +pest_use+wealth_ag*femhead+credit*femhead+extension_advice+area_gps*femhead,
       data= ETH111,weights=weight)

summary(FImonth)

FIprob<-lm(food_not_enough12m~ free_food*as.factor(region)+femhead+urban*femhead+
              SPI2+SPI3+SPI4+agehead*femhead+hhlabor*femhead
            +hhszae*femhead+educhead*femhead+educave15_60*femhead+safewater+landown*femhead
            +TLU_total*femhead+dist_market+improved_maize+fert_inorg_use
            +pest_use+wealth_ag*femhead+credit*femhead+extension_advice+area_gps*femhead,
            data= ETH111,weights=weight)

summary(FIprob)

#export regression results
install.packages('stargazer')
library(stargazer)
stargazer(FImonth, FIprob, type = "html", out="together.htm")

#e
#check multicollinearity
random<-lm(educave15_60~educhead*femhead, data=ETH111)
summary(random)

fi_rain <- lm(free_food~SPI2+SPI3+SPI4, data=ETH111)
summary(fi_rain)

