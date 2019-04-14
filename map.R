
library(rgdal)
library(rgeos)
library(ggplot2)
library(maptools)
library(ggmap)
library(GGally)

###############

ggplot() +
  geom_point(data = mrt, aes(x=緯度, y=經度)) +
  geom_point(data = r, aes(x=緯度, y=經度), size=0.2, color="red")

# 村里界圖
tw <-readOGR(dsn="村里界圖",layer="Village_NLSC_1050715", stringsAsFactors = F,encoding = "big5")
head(tw@data)
tw@polygons[[1]]
tw@data %>% filter(V_Name != Substitute) %>% head

# 把里跟區貼起來
#tw@data <- tw@data %>% mutate(行政區域 = paste0(T_Name,Substitute)) 
tp.v <- tw[tw$C_Name %in% c("臺北市", "新北市"),] %>% fortify(region = "行政區域") 

v <- ggplot(tp.v, aes(long,lat)) +
  geom_polygon(data = tp.v, aes(long,lat), fill=NA, color = "white") 

v +
  geom_point(data=n.mor.in.mean ,aes(x=經度, y=緯度 ,size = num),
             colour = "blue",alpha=0.5)

ggplot() +
  geom_point(data = Traingdata, aes(x=平均人口, y=流量, size=房價中位數, color=as.factor(住宅區) ), alpha=0.3) +
  annotate("text", x =Traingdata[Traingdata$平均人口>75000,7], y =Traingdata[Traingdata$平均人口>75000,3], 
           label =as.character(Traingdata[Traingdata$平均人口>75000,1]),family="STHeiti", colour = "black", size=2) +
  theme_grey(base_family="STHeiti")

ggplot() +
  geom_point(data = Traingdata[-68,], aes(x=平均人口*房價中位數, y=流量, color=as.factor(住宅區) ), alpha=0.7) +
  annotate("text", x=Traingdata[-68,][Traingdata$流量>2000 ,4]*Traingdata[-68,][Traingdata$流量>2000,7], y=Traingdata[Traingdata$流量>2000,3],
           label =as.character(Traingdata[Traingdata$流量>2000,1]),family="STHeiti", colour = "black", size=2) +
  theme_grey(base_family="STHeiti")

plot(Traingdata$`15-64`, Traingdata$流量)
plot( Traingdata$房價中位數, Traingdata$流量)
