
library(dplyr)
library(highcharter)
#### 
inn <- read.csv("平日_in.csv",sep = ",", fileEncoding = "big5")
out <- read.csv("平日_out.csv",sep = ",", fileEncoding = "big5")
vinn <- read.csv("假日_in.csv",sep = ",", fileEncoding = "big5")
vout <- read.csv("假日_out.csv",sep = ",", fileEncoding = "big5")

# 平日進站平均mean.n.inn
mean.n.inn <- NULL 
for (i in 0:23) {
  a <- filter(inn, 時段==i) 
  k <- apply(a[-c(1,2)], 2, mean)
  mean.n.inn <- rbind(mean.n.inn, k)
}
mean.n.inn <- mean.n.inn[-c(3:5),-1] %>% t() %>% as.data.frame()
colnames(mean.n.inn) = c(0,1,5:23)

#write.csv(mean.n.inn, "mean.in.csv", fileEncoding = "big5")

# 平日出站平均mean.n.out
mean.n.out <- NULL 
for (i in 0:23) {
  a <- filter(out, 時段==i) 
  k <- apply(a[-c(1,2)], 2, mean)
  mean.n.out <- rbind(mean.n.out, k)
}
mean.n.out <- mean.n.out[-c(3:5),-1] %>% t() %>% as.data.frame()
colnames(mean.n.out) = c(0,1,5:23)

# 假日進站平均mean.v.inn
mean.v.inn <- NULL 
for (i in 0:23) {
  a <- filter(vinn, 時段==i) 
  k <- apply(a[-c(1,2)], 2, mean)
  mean.v.inn <- rbind(mean.v.inn, k)
}
mean.v.inn <- mean.v.inn[,-1] %>% t() %>% as.data.frame()
colnames(mean.v.inn) = c(0:23)

# 假日出站平均mean.v.out
mean.v.out <- NULL 
for (i in 0:23) {
  a <- filter(vout, 時段==i) 
  k <- apply(a[-c(1,2)], 2, mean)
  mean.v.out <- rbind(mean.v.out, k)
}
mean.v.out <- mean.v.out[,-1] %>% t() %>% as.data.frame()
colnames(mean.v.out) = c(0:23)

# 平日進站中位數med.n.inn 
med.n.inn <- NULL 
for (i in 0:23) {
  a <- filter(inn, 時段==i) 
  k <- apply(a[-c(1,2)], 2, median)
  med.n.inn <- rbind(med.n.inn, k)
}
med.n.inn <- med.n.inn[-c(3:5),-1] %>% t() %>% as.data.frame()
colnames(med.n.inn) = c(0,1,5:23)

# 假日進站中位數med.v.inn
med.v.inn <- NULL 
for (i in 0:23) {
  a <- filter(vinn, 時段==i) 
  k <- apply(a[-c(1,2)], 2, median)
  med.v.inn <- rbind(med.v.inn, k)
}
med.v.inn <- med.v.inn[-c(3:5),-1] %>% t() %>% as.data.frame()
colnames(med.v.inn) = c(0,1,5:23)

# for share
t(mean.n.inn) %>% as.data.frame() %>% mutate(time=5:25) -> new.mean.in
write.csv(new.mean.in, "mean.in.csv", row.names = FALSE, fileEncoding = "big5")
write.csv(mean.n.inn, "mean.n.csv", fileEncoding = "big5")
write.csv(mean.n.out, "mean.o.csv", fileEncoding = "big5")

###############
highchart() %>%
  hc_title(text ="平日") %>%
  hc_xAxis(categories=row.names(mean.n.out) ,title = list(text = "時間點") ) %>%
  hc_yAxis(title = list(text = "捷運站")) %>%
  hc_add_series(data= mean.n.inn$`6` , name="in6") %>%
  hc_add_series(data= mean.n.inn$`7` , name="in7") %>%
  hc_add_series(data= mean.n.inn$`8` , name="in8") %>%
  hc_add_series(data= med.n.inn$`6` , name="min6") %>%
  hc_add_series(data= med.n.inn$`7` , name="min7") %>%
  hc_add_series(data= med.n.inn$`8` , name="min8") 

  hc_add_series(data= mean.n.out$`6`, name="out6") %>%
  hc_add_series(data= mean.n.out$`7`, name="out7") %>%
  hc_add_series(data= mean.n.out$`8`, name="out8") 

highchart() %>%
  hc_title(text ="平日") %>%
  hc_xAxis(categories=row.names(mean.n.out) ,title = list(text = "時間點") ) %>%
  hc_yAxis(title = list(text = "捷運站")) %>%
  hc_add_series(data= mean.n.inn$`17` , name="in17") %>%
  hc_add_series(data= mean.n.inn$`18` , name="in18") %>%
  hc_add_series(data= mean.n.inn$`19` , name="in19") %>%
  hc_add_series(data= mean.n.out$`17`, name="out17") %>%
  hc_add_series(data= mean.n.out$`18`, name="out18") %>%
  hc_add_series(data= mean.n.out$`19`, name="out19") 

  
highchart() %>%
  hc_title(text ="假日") %>%
  hc_xAxis(categories=row.names(mean.v.out) ,title = list(text = "時間點") ) %>%
  hc_yAxis(title = list(text = "捷運站")) %>%
  hc_add_series(data= mean.v.inn$`10` , name="in10") %>%
  hc_add_series(data= mean.v.inn$`11` , name="in11") %>%
  hc_add_series(data= mean.v.inn$`12` , name="in12") %>%
  hc_add_series(data= mean.v.out$`10`, name="out10") %>%
  hc_add_series(data= mean.v.out$`11`, name="out11") %>%
  hc_add_series(data= mean.v.out$`12`, name="out12") 

highchart() %>%
  hc_title(text ="平日中位數") %>%
  hc_xAxis(categories=row.names(med.n.inn) ,title = list(text = "時間點") ) %>%
  hc_yAxis(title = list(text = "捷運站")) %>%
  hc_add_series(data= med.n.inn$`6` , name="in6") %>%
  hc_add_series(data= med.n.inn$`7` , name="in7") %>%
  hc_add_series(data= med.n.inn$`8` , name="in8") 
 
highchart() %>%
  hc_xAxis(categories= colnames(mean.n.inn) ) %>%
  hc_add_series(data= as.numeric(mean.n.inn[42,]), name="北車進站") %>%
  hc_add_series(data= as.numeric(mean.n.out[42,]), name="北車出站") %>%
  hc_add_series(data= as.numeric(mean.n.inn[41,]), name="台大醫院平日進站") %>%
  hc_add_series(data= as.numeric(mean.n.out[41,]), name="台大醫院平日出站")

highchart() %>%
  hc_xAxis(categories= new.mean.in$time) %>%
  hc_add_series(data= new.mean.in$台北車站 , name="北車") %>% 
  hc_add_series(data= new.mean.in$景安 , name="景安") %>%  
  hc_add_series(data= new.mean.in$頂溪 , name="頂溪") %>% 
  hc_add_series(data= new.mean.in$市政府 , name="市政府") %>% 
  hc_add_series(data= new.mean.in$松江南京 , name="松江南京") %>%  
  hc_add_series(data= new.mean.in$公館 , name="公館") %>% 
  hc_add_series(data= new.mean.in$萬芳社區 , name="萬芳社區") %>% 
  hc_add_series(data= new.mean.in$板橋 , name="板橋") %>% 
  hc_add_series(data= new.mean.in$大直 , name="大直") %>% 
  hc_add_series(data= new.mean.in$淡水 , name="淡水") %>% 
  hc_add_series(data= new.mean.in$西門 , name="西門") 
  
  

# AM 5-9 進站總人數
filter(inn , 時段 %in% c(5:9)) %>%
  select(-c(2,3)) %>%
  group_by(日期) %>% 
  summarise_each(funs(sum(.))) -> n.mor.in 

summary(n.mor.in)

colnames(n.mor.in)[order(apply(n.mor.in[-1], 2, mean) ,decreasing = T)+1][11:20] 

n.mor.in.mean <- filter(inn , 時段 %in% c(5:9)) %>%
  select(-c(1:3)) %>%
  summarise_each(funs(mean(.))) %>% t() %>% as.data.frame() %>%
  mutate( mrt= colnames(inn)[-c(1:3)] ) 

n.mor.in.mean <- merge(n.mor.in.mean, mrt, by.x="mrt", by.y="捷運名稱", all=T) 

colnames(n.mor.in.mean) <- c("mrt_name", "num", colnames(n.mor.in.mean)[3:5])


# 想看每個時段站的人次排序
# 平日出站排序 mean.n.out.s
matrix(apply(mean.n.out, 2, order, decreasing=T) , ncol = 21)  %>% as.data.frame() %>%
sapply( function(x) rownames(mean.n.out)[x]) %>% as.data.frame() -> mean.n.out.s
colnames(mean.n.out.s) = c(0,1,5:23)
table(as.character(unlist(mean.n.out.s[1:10,]))) %>% as.data.frame()-> a
  filter(Freq < 5) %>%
  View
length(a)

# 平日進站排序 mean.n.inn.s
matrix(apply(mean.n.inn, 2, order, decreasing=T) , ncol = 21)  %>% as.data.frame() %>%
  sapply( function(x) rownames(mean.n.inn)[x]) %>% as.data.frame() -> mean.n.inn.s
colnames(mean.n.inn.s) = c(0,1,5:23)
table(as.character(unlist(mean.n.inn.s[1:10,])))
table(as.character(unlist(mean.n.inn.s[99:108,])))

a <- as.character(unlist(mean.n.out.s[1:10,]))
b <- as.character(unlist(mean.n.inn.s[1:10,]))
table(a,b) %>% as.data.frame() %>% View


# 假日出站排序 mean.v.out.s
matrix(apply(mean.v.out, 2, order, decreasing=T) , ncol = 24)  %>% as.data.frame() %>%
  sapply( function(x) rownames(mean.n.out)[x]) %>% as.data.frame() -> mean.v.out.s
colnames(mean.v.out.s) = c(0:23)
table(as.character(unlist(mean.v.out.s[1:10,])))  %>% as.data.frame() -> b
  

merge(a,b, by=c("Var1", "Var1"), all = T) #淡水 劍潭


############ 租屋資料 ###################################
rent <- read.csv("rent_taipei.csv", fileEncoding = "big5")
table(rent$主要用途)
table(rent$建物型態)

filter(rent, 建物型態=="工廠") %>% View #工廠的單價每坪方公尺皆為空白
filter(rent, 租賃標的 == "土地") %>% View

rent[-is.na(rent$總額元), ] %>%
  filter(租賃總面積平方公尺.1 != 0) %>% 
  filter(租賃標的 != "土地") %>%
  filter(租賃標的 != "車位") %>%
  mutate(price = 總額元/(0.3025*租賃總面積平方公尺.1)) -> rentp
rentp %>%
  group_by(鄉鎮市區) %>%
  summarise(mean=mean(price, na.rm=T), median=median(price, na.rm=T)) %>%
  arrange(mean) %>% View


# 捷運站經緯度
mrt <- read.csv("捷運站經緯度.csv")
#write.csv(mrt,"捷運站經緯度.csv", row.names = F)
table(mrt$區)

library(geosphere)

t <- table(rentp$鄉鎮市區) %>% rownames()
mrt.rent.mean <- NULL
r <- filter(rentp, 鄉鎮市區 == a[1]) 
m <- filter(mrt, 區 == a[1]) 
k <- NULL

for (j in 1:nrow(m)) {
  for (i in 1:nrow(r)) {
    r[i,19]=as.numeric( distm( c(r$經度[i],r$緯度[i]), c(m$經度[j], m$緯度[j]),
                               fun = distHaversine) )
  }
  a <- filter(r, V19 < 500 ) %>% summarise(mean = mean(price)) 
  k <- rbind(k,a)
}

row.names(k) <- m$捷運站名稱
mrt.rent.mean <- rbind(mrt.rent.mean, k)



#### 算捷運站跟每筆租屋資料距離 ##############

library(parallel)
rentp %>% select(10) -> r
cl <-makeCluster(getOption("cl.cores",3) )
for(j in 1:108){
  for(i in 1:nrow(rentp)){
    r[i,j+1]<-as.numeric( distm ( c(rentp$經度[i],rentp$緯度[i]), c(mrt$經度[j], mrt$緯度[j]), 
                                fun = distHaversine) )
  }
}
stopCluster(cl)

######### 算住商工個數 ################
colnames(r) = c("主要用途", as.character(mrt$捷運名稱 ) )
r$type[r$主要用途 %in% c("住家用", "國民住宅")] <- "住" 
r$type[r$主要用途 %in% c("工商用", "商業用", "住商用")] <- "商" 
r$type[r$主要用途 %in% c("住工用", "工業用")] <- "工" 

a = select(b, t)
for (i in 1:108) {
  r %>%
    select(i+1,type) -> c 
    c[c[,1] < 600 , ] %>% 
    group_by(type) %>%
    summarise_each(funs(n())) -> b

a <- merge(a,b ,by = c("type", "type"), all=T)
}

######## 佩雯的 ###################

bb <- read.csv("ratio.csv", fileEncoding = "big5")
colnames(b) <-  c("type", as.character(mrt$捷運名稱 ) )
colnames(a) <-  c("type", as.character(mrt$捷運名稱 ) )
a[is.na(a) ]= 0
bb[is.na(bb) ]= 0
bb <- rbind(bb[,-1], apply(bb[1:4,-1], 2, sum)) 
bb[8,] <- bb[1,]/bb[7,]
bb[9,] <- bb[2,]/bb[7,]
bb[10,] <- bb[3,]/bb[7,]

# 租屋的
aaa <- matrix(c(rep(0,5), 10,rep(0,6),10,10,0,rep(10,2),rep(0,7)), ncol = 6, byrow = T)
a[,88:93] <- a[,88:93]+aaa
aa <- rbind(a[,-1], apply(a[,-1], 2, sum))  
aa[6,] <- aa[1,]/aa[5,]
aa[7,] <- aa[2,]/aa[5,]
aa[8,] <- aa[3,]/aa[5,]
aa[9,] <- aa[2,]/aa[3,]

apply(aa[6:8, ], 2, max) %>% as.data.frame() %>%View()

a1 <-  apply(aa[6:8,], 2, order, decreasing=T) %>% as.data.frame() %>%
  sapply(function(x) b$type[1:3][x])  
a1[ a1 != "商"] =0
a1[ a1 == "商"] =1

b2 <-  apply(bb[8:10,-1], 2, order, decreasing=T) %>% as.data.frame() %>%
  sapply(function(x) b$type[1:3][x]) 
b2[ b2 != "工"] =0
b2[ b2 == "工"] =1

# 先擺著
rbind(a1[1,], a2[1,]) %>% rbind(b1[1,]) %>% rbind(b2[1,]) %>%
  View()

################# 畫圖的 ###############
library(rgdal)
library(rgeos)
library(ggplot2)
library(maptools)
library(ggmap)
library(GGally)

ggpairs(Traingdata[, c(3,4,7,13:15)]) + 
  theme_grey(base_family="STHeiti")

ggplot(Traingdata, aes(房價中位數.區中位數)) +
  geom_histogram(bins = 30)+ 
  geom_vline(xintercept =c(1.25, 0.9), color="red")+
  labs( x = "房價中位數/行政區房價中位數")+
  annotate("text", x =c(1.27, 0.92), y =10, label =c("1.25", "0.9"),family="STHeiti", colour = "blue", size=5) +
  theme_grey(base_family="STHeiti")

ggplot(Traingdata, aes(流量.人口)) +
  geom_histogram(bins = 120)+ 
  geom_vline(xintercept =0.045, color="red")+
  labs( x = "流量/權重人口")+
  annotate("text", x =0.08, y =30, label ="0.045",family="STHeiti", colour = "blue", size=5) +
  theme_grey(base_family="STHeiti")

ggplot(Traingdata[-68,], aes(x=流量.人口, y=房價中位數.區中位數)) +
 # geom_point()  +
  labs( x = "流量/權重人口", y="房價中位數/行政區房價中位數")+
 annotate("text",x=Traingdata$流量.人口[-68], y=Traingdata$房價中位數.區中位數[-68], label =Traingdata$mrt[-68] ,family="STHeiti", size=1.5, color="#495397") +
 annotate("text",x=Testingdata$流人 , y=Testingdata$房價中位數.區中位數, label =Testingdata$mrt, colour = "blue", family="STHeiti", size=3) +
  geom_vline(xintercept =0.045, color="red")+
  geom_hline(yintercept =c(1.25, 0.9), color="red")+
  theme_grey(base_family="STHeiti")

ggplot(Testingdata, aes(x=流人, y=房價中位數.區中位數))+
  geom_point() +
  labs( x = "流量/權重人口", y="房價中位數/行政區房價中位數")+
annotate("text",x=Testingdata$流人 , y=Testingdata$房價中位數.區中位數+0.01, label =Testingdata$mrt, colour = "blue", family="STHeiti", size=3) +
  geom_vline(xintercept =0.045, color="red")+
  geom_hline(yintercept =c(1.25, 0.9), color="red")+
  theme_grey(base_family="STHeiti")




########### 整併資料 #######################################

# 年齡與鄉里資料
mrt.li <- read.csv("mrt.ratio.csv") %>% select(-1)
age <- read.csv("里年齡.csv", fileEncoding = "big5") %>% select(1:3,54)
traing <- read.csv("TrainingDataRule1.csv", fileEncoding = "big5") %>% select(-c(1,5,6,10))
traing2 <- read.csv("TrainingDataRule2.csv", fileEncoding = "big5" ) %>% select(-c(1,6,7,11))
p <- read.csv("population.csv") %>% select(-c(1,2)) %>% 
  mutate(name=c(as.character(mrt$捷運名稱), as.character(testing$捷運站)) ) 
mini <- read.csv("mini.csv", fileEncoding = "big5") 
row.names(mini) = mini$X
testing <- read.csv("TestingData.csv", fileEncoding = "big5")

# 15-64 人數
mrt.age <- merge(mrt.li, age, by=c("縣市","區", "里"), all.x=T) %>%
  mutate(ratio=sum/人數)

rage <-  mrt.age$sum %*% as.matrix(mrt.age[,6:113]) %>% round() %>% t() %>%
  as.data.frame() %>%
  mutate(name=c(mrt$捷運名稱))
colnames(rage)[1] = "15-64"

# 最小統計區
m <- mini[,-1]  %>% t() %>%
  as.data.frame() %>% mutate(name=c(as.character(mrt$捷運名稱), as.character(testing$捷運站))) 

# Traing data 產出
Traingdata <- merge(traing, p[1:108,], by.x="mrt", by.y="name", all=T) %>% 
  merge( rage, by.x="mrt", by.y="name", all=T) %>% 
  mutate(age.ratio=`15-64`/平均人口) %>%
  merge(m[1:108, ] , by.x="mrt", by.y="name", all=T) 

Traingdata2 <- merge(traing2, p[1:108, ], by.x="mrt", by.y="name", all=T) %>% 
  merge( rage, by.x="mrt", by.y="name", all=T) %>% 
  mutate(age.ratio=`15-64`/平均人口) %>%
  merge(m[1:108, ] , by.x="mrt", by.y="name", all=T) 

cor(Traingdata[,-c(1,2)]) %>% View

# traing data output
write.csv(Traingdata , "Trainingdata1.csv", fileEncoding = "big-5", row.names = F)
write.csv(Traingdata2 , "Trainingdata2.csv", fileEncoding = "big-5", row.names = F)

######## try model ###############
lm1 <- lm(流量~房價中位數+平均人口+age.ratio+工業區+商業區+住宅區, data = Traingdata)
lm2 <- lm(流量~ I(房價中位數*平均人口)+房價中位數+平均人口+age.ratio+工業區+商業區+住宅區 , data = Traingdata)
lm3 <- lm(I(log(流量))~ I(房價中位數*平均人口)+房價中位數+平均人口+age.ratio+工業區+商業區+住宅區 , data = Traingdata)
lm4 <- lm( I(log(流量)) ~ 房價中位數 + `15-64` + 工業區 + 商業區 + 住宅區+ I( 房價中位數^2 * 住宅區) + I(房價中位數 * (商業區 + 工業區)) , data=Traingdata[-c(68,83),] )

add1(im1, test = "F")

summary(lm4)
par(mfrow=c(1,2))
plot(lm4)
lm4 <- step(lm4, test="F") # stepwise
step(lm1, direction = "backward") #用完整的模型給他backward

# 比較兩個模型 後面的要包含前面的
anova(lm2, lm3)

## box cox transfrom 
library(MASS)
##### buliding model & boxcox #####
Traingdata <- read.csv("Trainingdata1.csv", fileEncoding = "big5")
y = lm(流量~ 房價中位數 + 工業區 + 商業區 + 住宅區 + I(房價中位數*X15.64*(5*住宅區-工業區-商業區))  , data=Traingdata[-68,] )
boxcox(y, lambda = seq(-0.25, 0.25, length = 10)) #看係數
tt = {Traingdata$流量-mean(Traingdata$流量)}/sd(Traingdata$流量)+2
tt = {(tt^0.1-1)/0.1}*{{{cumprod(tt)[108]}^{1/108}}^(1-0.1)}
# 想辦法轉回去～～
k <- {{{cumprod(tt)[108]}^{1/108}}^(1-0.1)}
(tt/(10*k)+1)^10
hist(tt)
ks.test({tt-mean(tt)}/sd(tt), "pnorm")

Traingdata[,3] =tt

###### 最終模型
lm <- lm( 流量 ~ 房價中位數 + 工業區 + 商業區 + 住宅區+  I(房價中位數 * X15.64 * (5*住宅區 -商業區 - 工業區)) , data=Traingdata[-68,] )
summary(lm)

# 丟Testing 進去
Testingdata <- mutate(Testingdata,v=房價中位數 * X15.64 * (5 * 住宅區 - 商業區 - 工業區))

n=as.numeric(lm$coefficients)
Testingdata <- mutate(Testingdata, result=n[1]+Testingdata[,5]*n[2]+Testingdata[,9]*n[3]+Testingdata[,10]*n[4]+Testingdata[,11]*n[5]+ Testingdata[,12]*n[6]) 
predict(lm)

  # Traingdata 試試看
cbind(as.character(Traingdata$mrt), n[1]+Traingdata[,5]*n[2]+Traingdata[,9]*n[3]+Traingdata[,10]*n[4]+Traingdata[,11]*n[5]+ Traingdata[,12]*n[6]) %>% 
  cbind(Traingdata$流量) %>% View()

##### Testing data ######################
testing <- read.csv("TestingData.csv", fileEncoding = "big5")
test.age <-  mrt.age$sum %*% as.matrix(mrt.age[,114:137]) %>% round() %>% t() %>%
  as.data.frame() %>%
  mutate(name=testing$捷運站)  
colnames(test.age)[1] = "15-64" 

Testingdata <- merge(testing, p[109:132,], by.x="捷運站", by.y="name", all=T) %>% 
  merge( test.age, by.x="捷運站", by.y="name", all=T) %>% 
  mutate(age.ratio=`15-64`/平均人口) %>%
  merge(m[109:132, ] , by.x="捷運站", by.y="name", all=T) 

##### read XML #####
library(xml2)
library(XML)
library("methods")

doc1 <- read_xml("O-A0001-001.xml")
class(doc1)
point <- doc1 %>% xml_find_all("//lat")


dd <- xmlParse("O-A0001-001.xml", encoding = "utf8")
class(dd)
xmltop = xmlRoot(dd)
xmltop <- xmlSApply(xmltop,function(x) xmlSApply(x, xmlValue))
xml_df <- data.frame(t(xmltop))
xml_df <- xml_df[-c(1:8)] %>% t()
xml_df[2] %>% unlist() %>% strsplit(",") %>% View()

print(xmltop[1])
print(xmltop[[7]][[1]])
xml <- xmlToDataFrame("O-A0001-001.xml")
xmlName(xmltop)
xmlSize(xmltop)
xmlName(xmltop[[1]]) 
xmltop[[1]]
xmlSize(xmltop[[1]])#number of nodes in each child
xmlSApply(xmltop[[1]], xmlName)

xmltop[[2]][[1]]
xmltop[[1]][[1]][[5]][[2]]

xml_data <- xmlToList(dd)
xml_data <- xml_data[-c(1:8)]
xmldataframe <- xmlToDataFrame(xml_data)

location <- as.list(xml_data[["location"]])
location$weatherElement

location <- as.list(xml_data[["data"]][["location"]][["point"]])
unlist(location)
loca <- unlist(xml_data[["location"]][names(xml_data[["location"]]) == "lat" ])

#### 讀檔 #####
Traingdata <- read.csv("TrainingData.csv", fileEncoding = "big5")
Testingdata <- read.csv("TestingData.csv", fileEncoding = "big5")
Testingdata <- Testingdata[1:24,]
Testingdata <- mutate(Testingdata, 流人= 預估流量/平均人口)

#### k-means #####
a = kmeans(Traingdata$流量, 3, 20)
a1 = {Data[a$cluster == 1,]}[,1:2]
a2 = {Data[a$cluster == 2,]}[,1:2]
a3 = {Data[a$cluster == 3,]}[,1:2]
a4 = {Data[a$cluster == 4,]}[,1:2]
a$cluster <- as.factor(a$cluster)
ggplot(Traingdata, aes(流量, 房價中位數, color = a$cluster)) + 
  geom_point() +
  theme_grey(base_family="STHeiti")  

x <- Traingdata$流量
x <- (x-mean(x))/sd(x)
t <- Traingdata$公車種類數
t <- (t-mean(t))/sd(t)
b <- kmeans(cbind(x,t), 4,20)

c <- kmeans(cbind(x,Traingdata[,4]*Traingdata[,7] ), 4, 20)
b$cluster <- as.factor(b$cluster)
ggplot(Traingdata, aes(流量, 公車種類數, color =b$cluster  )) + 
  geom_point() +
  theme_grey(base_family="STHeiti")

  annotate("text", x=Traingdata$流量, y=Traingdata$平均人口+2, label = Traingdata$mrt ,family="STHeiti", colour = "black", size=2)+

