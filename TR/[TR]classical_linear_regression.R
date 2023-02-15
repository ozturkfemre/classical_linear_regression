###################################
# Fatih Emre Öztürk               #                     
# Denetimli İstatistiksel Öğrenme #
###################################

####################
### Kütüphaneler ###
####################

library(corrplot)
library(nortest)
library(ISLR)
library(Hmisc)
library(caret)
library(dplyr)
library(ModelMetrics)
library(lmtest)
library(car)
library(olsrr)
library(tidyverse)
library(moments)
library(bestNormalize)
library(magrittr)


###########################
# Veri Setinin Çağrılması #
###########################

df <- Wage

str(df)

# Veri setindeki değişkenleri veri tipleri incelendiğinde 2 nümerik, 6 factor ve 2 integer değer dikkat çekmekte.


dim(df)

# Veri setinde 3000 gözlem 11 değişken bulunmakta.

# Değişkenlerin ne anlama geldiği açıklanmak istenilmiştir.

# Year	Maaş bilgisinin edinildiği yıl
# Age	İşçinin yaşı
# Maritl	medeni durum
# Race	işçinin ırkı
# Education	işçinin eğitim seviyesi
# Region	ülkenin bölgesi
# Jobclass	işin tipi ile ilgili endüstriyel bilgi
# Health	işçinin sağlık durumu
# Health_ins	işçinin sağlık sigortası
# Logwage	işçilerin maaşının logaritması
# Wage	işçilerin maaşı

###########################
### Test - Train Ayrımı ###
###########################

smp_size <- floor(0.75 * nrow(df)) 
set.seed(2021900444) 
train_ind <- sample(nrow(df), size = smp_size, replace = FALSE)
train <- df[train_ind, ]
test <- df[-train_ind, ]
test <- test[-6]
train <- train[-6]
#############################
# Tanımlayıcı İstatistikler #
#############################

summary(df)

# Sayısal değişkenlerin tanımlayıcı istatistikleri incelendiğinde:

# Year değişkeninin range'inin oldukça düşük(6) olduğu dikkat çekmektedir. 
# Age değişkeninin mean ile medyanı birbirine oldukça yakın çıkmıştır. 
# logwage değişkeninin tanımlayıcı istatistiği incelendiğinde mean ile median değerleri birbirine oldukça yakın olduğı, çarpıklığın yok denilebilecek kadar az olduğu fark edilmiştir.
# Wage değişeninin mean'i mediandan yüksek çıkmıştır. Bu sebeple sağa çarpık olabileceği düşünülmektedir. Ayrıca 1. kartil(85) ile min değer(20) değer arasındaki fark düşünüldüğünde aykırı değer olabileceği varsayılmaktadır. Boxplot analizi ile daha sağlıklı bir yorum yapmak mümkün olabilir.


# Kategorik değişkenlerin tanımlayıcı istatistikleri incelendiğinde:
# Medeni durumu gösteren faktör değişkeninde çok fazla level olduğu fark edilmiştir. Grafik incelemesi sonrası bu seviyelerin düşürülüp düşürülmeyeceğine karar verilecektir.
# Irk bilgisini içeren değişkende Beyaz ırkının çokluğu dikkat çekmekle birlikte bölge bilgisini içeren değişkene bakıldığında anlaşılmıştır.
# Eğitim bilgisini içeren grafik incelendiğinde "Some College" level'ı anlaşılmamış, yarıda bırakmış olabileceği düşünülmektedir. Grafik incelemesi sonrası bu değişkenin levellerinde de değişikliğe gidilebileceği düşünülmektedir.
# Region değişkeninde Middle Atlantic dışında bir bilgi olmadığı fark edilmiştir. Başka seviye olmaması bu değişkenin hiçbir farklılık yaratmayacağını düşündürtmüştür. Bu sebeple bu değişkeni modele katmanın bir anlamı yoktur. 
# Analiz sonucunda yapılacak olan tahmin yalnızca Middle Atlantic bölgesinde yaşayan işçiler için olacaktır.Bu sebeple bu değişken veri setinden çıkarılmıştır.
# Jobclas değişkeninin iki seviyesi olduğu, bu seviyelere sahip gözlem değerlerinin de birbirlerine oldukça yakın olduğu fark edilmiştir.
# Health değişkeninin seviyelerinde Very Good değişkeninin daha fazla gözlemi olduğu fark edilmiştir.
# Health Insurance değişkeni incelendiğinde işçilerin büyük bir çoğunluğunun sigortası olduğu fark edilmiştir.



### Grafik Analizi

par(mfrow = c(1,4))

boxplot(df$year)
boxplot(df$age)
boxplot(df$logwage)
boxplot(df$wage)


# Sayısal değerlerin kutu grafikleri incelendiğinde Age değişkeninde bir adet, logwage ve wage değişkenlerinde oldukça fazla aykırı gözlem olduğu fark edilmiştir. 
# Bağımlı değişken seçimi olarak elimizde olan iki seçenek logwage ve wage değişkenlerinin dağılımları ile ilgili yorum yapabilmek için, bu iki değişkenin histogram grafiği ile normallik testleri yapılacaktır.


## Logwage

dev.off()
fun <- dnorm(df$logwage, mean = mean(test$logwage), sd = sd(df$logwage))

hist(df$logwage, main = "Log(Wage) histogramı", ylab = "Frekans", xlab = "Log(Wage)",col="burlywood2",
     border="burlywood4", probability = T, ylim = c(0, max(fun) + 1))

lines(density(df$logwage), col = 9, lwd = 2)

skewness(df$logwage)

# Logwage değişkeninin iki tepeli olduğu histogram grafiğinden anlaşılmıştır. Sola çarpıklık da görülmektedir.

# Normallik sınamaları ile daha sağlıklı bir yorum yapılmak istenilmiştir. 

### Normallik Testleri ###

# Bütün testlerin hipotezleri aynı olduğu için hipotezler yalnızca bir kere gösterilmek istenilmiştir.

# Ho : Logwage değişkeni normal dağılmıştır.
# Ha : Logwage değişkeni normal dağılmamaktadır.


### Anderson - Darling normallik testi

ad.test(df$logwage)
# P-value 0.05'ten  küçük olduğu için değişkenin normal dağıldığı hipotezi reddedilebilir.

### Shapiro - Wilk normallik testi

shapiro.test(df$logwage)
# P-value 0.05'ten küçük olduğu için değişkenin normal dağıldığı hipotezi reddedilebilir.


# Hipotez testleri de histogramın çıktısını doğrulamıştır. Logwage değişkeni normal dağılmamaktadır. 


## Wage

dev.off()

fun <- dnorm(df$wage, mean = mean(df$wage), sd = sd(df$wage))

hist(df$wage, main = "Wage histogramı", ylab = "Frekans", xlab = "Wage",col="burlywood2",
     border="burlywood4", probability = T, ylim = c(0, max(fun))+0.001)

lines(density(df$wage), col = 9, lwd = 2)

# wage değişkeninin iki tepeli olduğu histogram grafiğinden anlaşılmıştır. Sağa çarpıklık da görülmektedir. Uç değer olarak da düşünebileceğimiz 250 ve fazlası değerler çıkarılırsa normallik sağlanabilir. 

# Ancak yine de normallik sınamaları ile daha sağlıklı bir yorum yapılmak istenilmiştir. 


### Normallik Testleri ###

# Bütün testlerin hipotezleri aynı olduğu için hipotezler yalnızca bir kere gösterilmek istenilmiştir.

# Ho : wage değişkeni normal dağılmıştır.
# Ha : wage değişkeni normal dağılmamaktadır.


### Anderson - Darling normallik testi

ad.test(df$wage)
# P-value 0.05'ten  küçük olduğu için değişkenin normal dağıldığı hipotezi reddedilebilir.

### Shapiro - Wilk normallik testi

shapiro.test(df$wage)
# P-value 0.05'ten küçük olduğu için değişkenin normal dağıldığı hipotezi reddedilebilir.


# Hipotez testleri de histogramın çıktısını doğrulamıştır. wage değişkeni normal dağılmamaktadır.
# Ancak logwage değişkenine kıyasla normalleştirmesi daha kolay olacağı düşünülmektedir. Bu sebeple wage değişkeni ile devam edilme kararı alınmıştır.
# Değişkenin normal dağılmaması, analizin ilerleyen bölümlerinde hataların normal dağılmasını da engelleyebilecek bir ayrıntıdır. 
# Değişkenin normal dağılmamasının sebebinin aykırı gözlemler olduğu histogram grafiğinde görülmektedir. 
# Ancak bu aykırı gözlemlerin çıkarılıp çıkarılmayacağına ilerleyen kısımlarda, standartlaştırılmış artıkların uç değer analizinde karar verilecektir.


# Kategorik Değişkenlerin Grafik

indexes = sapply(df, is.factor)
indexes["wage"] = TRUE
indexes["region"] = FALSE
df[,indexes]%>%
  gather(-wage, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = wage, color = value)) +
  geom_boxplot() +
  facet_wrap(~ var, scales = "free")+
  theme(axis.text.x = element_text(angle = 30, hjust = 0.85),legend.position="none")


# Kategorik değişkenlerin wage değişkeni ile ilişkisini içeren grafik incelendiğinde eğitim seviyesi arttıkça maaşta bir artık gözlemlenmektedir. Bu sebeple model için iyi bir değişken olabileceği düşünülmektedir.
# Health, Health Insurance, Jobclas ve Race değişkenlerinin çok büyük bir etkisi olmadığı gözlemlenmiştir. Bu değişkenlerle devam edilip edilemeyeceğine değişken seçimi kısmında karar verilecektir.
# Medeni durumu gösteren grafik incelendiğinde ise Never Married, Divorced ve Separated değişkenlerinin çok da farklı olmadığı fark edilmiştir. Aslında bekar anlamına gelen bu 3 seviyeyi tek çatı altında birleştirerek
# değişkenin seviyelerini "Single, Divorced ve Married olmak üzere üçe düşürmek mantıklı olabilir. İlerleyen bölümlerde bu olasılık üzerinde düşünülecektir.

pairs(df[c(1,2,9,10,11)])

# Değişkenleri birbirleri ile arasındaki ilişkiyi ortaya çıkaran serpme grafikleri incelendiğinde herhangi bir doğrusal ilişkiye rastlanılmamıştır.
# Bazı değişkenlerin sayısal olmasına rağmen kategorik hareket ettiği fark edilmiştir. 

# Korelasyon Analizi 
cor(df[c(1,2,9,10)])

# Sayısal değişkenlerin korelasyon değerleri incelendiğinde bağımlı değişken wage ile year değişkeni arasında (0.05) korelasyon bulunmamaktadır.
# Year değişkeni sayısal bir değişken olmasına rağmen kategorik davranan bir değişken olarak dikkat çekmektedir. Belki bu değişkeni kategorik olarak kullanmak bir değişikliğe sebep olabilir. İlerleyen bölümlerde bu ihtimal dikkate alınacaktır.
# Bağımlı değişken wage ile bağımsız değişken age arasında oldukça az(0.25) pozitif bir korelasyon bulunduğu saptanmıştır.

#################
### İlk Model ###
#################

model1 <- lm(wage ~ year + age + maritl + race + education + jobclass + health + health_ins, data = train)

summary(model1)

# Model Geçerliliği F Hipotez Testi

# Ho : Bağımlı değişken ve bağımsız değişkenler arasında doğrusal bir ilişki bulunmamaktadır. 
# Ha : Bağımlı değişken ile en az bir bağımsız değişken arasında doğrusal bir ilişki bulunmaktadır. (Bi != 0)

# Varyans analizine göre oluşturulan modelin F istatistiği 70.22 olarak saptanmıştır. 
# F istatistiğinin p değeri 0.05 önem düzeyinden küçük olduğu için %95 güven düzeyinde Ho reddedilmiştir. 
# En az bir kat sayı model için anlamlıdır.

## Katsayı Analizi

# Regresyon Katsayılarının Anlamlılığına İlişkin Hipotez Testi

# Ho : Eğim katsayısı 0'dan önemli ölçüde farklı değildir.
# Ha : Eğim katsayısı 0'dan önemli ölçüde farklıdır.

# Aşağıda belirtilen değişkenlerin p değeri 0.05'ten küçük olduğu için eğim katsayısının sıfırdan önemli ölçüde farklı olmadığı hipotezi reddedilemez; dolayısıyla değişken(ler) anlamlıdır:

# year, age, maritl2. Married, education faktörünün tüm levelları, jobclass, health ve health insurance

# Aşağıda belirtilen değişkenlerin p değeri 0.05'ten büyük olduğu için eğim katsayısının sıfırdan önemli ölçüde farklı olmadığı hipotezini reddetmek için geçerli kanıta sahibiz; dolayısıyla değişken(ler) anlamlı değildir:

# maritl3, maritl4, maritl5, race2, race3, race4 

# Kategorik değişkenlerin grafik incelenmesi sırasında medeni durum ile ilgili böyle bir durumla karşılaşabileceğimiz öngörülmüştü. Bu iyileştirmeler model geliştirme kısmında yapılacaktır.


###########
### VIF ###
###########

vif(model1)

# Modelin VIF değerleri incelendiğinde hiçbir bağımsız değişkenin VIF değeri 5'ten büyük çıkmadığı için modelde herhangi bir bağlantı problemi bulunmamaktadır.


####################################
### Olası En iyi Alt Küme Seçimi ###
####################################

all.steps <- ols_step_all_possible(model1)
plot(all.steps)

# Parametre sayısı birden çok olduğu ve model karşılaştırması yapılacağı için Rsquare yerine, Adjusted R square değerleri incelenmiştir.
# Tüm metriklere(Rsquare, Adj.R Square, Cp, AIC, SBIC ve SBC) göre en iyi sonuç veren modeller  255(full model), 248, 247, 219, 163, 93, 37, 1 olarak saptanmıştır.
# Bu modeller kendi aralarında kıyaslandığında Adjusted Rsquare değerleri arasında dikkat çeken bir farklılık bulunmamaktadır.
# Rsquare, AIC, SBIC ve SBC kriterleri incelendiğinde de ayrım yapmanın değerler birbirine yakın olduğu için mümkün olmadığı fark edilmiştir.

# Ayrım yapabilmek için en iyi seçeneğin Cp değerlerine bakmak olduğu fark edilmiştir.
# Cp değerlerine bakıldığında ise Cp değeri parametre sayısından büyük olmayan gözlemler(yanlılığı engellemek için) seçilmiştir. Model seçenekleri 255(full model), 248,247 ve 219 olarak azaltılmıştır.
# Bu model seçeneklerinin Cp değerleri de ayrıntılı olarak incelendiği zaman Cp değeri parametre sayısına en yakın olarak dikkat çeken 219. model, en iyi model olarak seçilmiştir.
# Bu model seçeneğinin seçilmesindeki bir başka neden ise parametre sayısıdır. Önerilen diğer model seçeneklerindeki parametre sayısı 219. modelden fazla olmasına rağmen; Adj. R square ve R square değerlerinde kayda değer bir artış olmamıştır. Bu da o bağımsız değişkenlerin eklenmesinin modelin açıklayıcılığı üzerinde olumlu bir etkisi olmadığı fark edilmiştir.
# Olası en İyi Alt Küme Seçimi sonrasında karar verilen modelin bağımsız değişkenleri aşağıdaki gibidir: 

all.steps$predictors[219]

# Yine de test seti üzerinde karşılaştırma yapabilmek için 247. model seçilmiştir .
# Bu iki modelin bağımsız değişkenleri 247  üzere aşağıdaki gibidir:

all.steps$predictors[247]
all.steps$predictors[248]


#######################################################
### Performans Kriterlerine Göre Uygun Model Seçimi ###
#######################################################

model219 <- lm(wage ~year+ age + maritl + education + health + health_ins, data=train)
summary(model219)


model247 <- lm(wage ~ year+age+ maritl+ education +jobclass+ health + health_ins, data=train)
summary(model247)

model248 <- lm(wage ~ year+ age + maritl + education + jobclass + health + health_ins, data=train)
summary(model248)


predictions1 <- predict(model219,test)
predictions2 <- predict(model247,test)
predictions3 <- predict(model248,test)

RMSE1 <- RMSE(predictions1, test$wage)
RMSE2 <- RMSE(predictions2, test$wage)
RMSE3 <- RMSE(predictions3, test$wage)

cbind(RMSE1,RMSE2, RMSE3)

# Model 219, model 247 ve model 248'in test veri setleri ile yapılmış olan tahminlerinin Root Mean Square Error değerleri incelendiğinde arada anlamlı bir fark olmadığı fark edilmiştir. En düşük hataya sahip olan model, 248. model olarak saptanmıştır.

mae1 <- mae(predictions1, test$wage)
mae2 <- mae(predictions2, test$wage)
mae3 <- mae(predictions3, test$wage)

cbind(mae1,mae2, mae3)

# Model 219, model 247 ve model 248'in test veri setleri ile yapılmış olan tahminlerinin Mean Absolute Error değerleri incelendiğinde arada anlamlı bir fark olmadığı saptanmıştır. En düşük hataya sahip olan model, 219. model olarak saptanmıştır.

# Hatalar arasındaki farkın çok da anlamlı olmaması, daha çok değişken sayısı ile hataların ve Rsquare ve Adj.Rsquare değerlerinde anlamlı bir değişim sebebiyle model 219 en iyi model olarak seçilmiştir.

# Son kararda oluşturulan model aşağıdaki gibidir:

modelbest <-  lm(wage ~year+ age + maritl + education + health + health_ins, data=train)


############################
### Hataların Normalliği ###
############################

fun <- dnorm(modelbest$residuals, mean = mean(modelbest$residuals), sd = sd(modelbest$residuals))

hist(modelbest$residuals, ylab = "Frekans", xlab = "Hatalar", main = "Hataların Histogram Grafiği", col="burlywood2",
     border="burlywood4", probability = T, ylim = c(0, max(fun) + 0.01))


lines(density(modelbest$residuals), col = 9, lwd = 2)

# Hataların histogram grafiği incelendiğinde bariz bir şekilde sağa çarpıklık fark edilmektedir. Daha önce de bu çarpıklığın olacağı öngörülmüştü. Bunun sebebi bağımlı değişkenin normal dağılmaması olabilir. 
# Eğer uç değer, kaldıraç ve etkin gözlemlerin veri setinden çıkarılması sonucunda yine hataların normal dağılması sağlanmazsa, bağımlı değişken üzerinde değişiklikler yapılacaktır. 

## Hipotez Tesi 

# Shapiro-Wilk Normallik Testi Hipotezleri:
  
# Ho : Hatalar normal dağılmaktadır.

# Ha : Hatalar normal dağılmamaktadır.

shapiro.test(modelbest$residuals)

# P değeri 0.05’ten küçük olduğu için %95 güvenle değişkenin normal dağıldığı hipotezini reddetmek için yeterli kanıt vardır.

#####################################
### Hataların Sabit Varyanslılığı ###
#####################################

# BP test hipotezleri:
  
# Ho: Hatalar sabit varyanslıdır.

# Ha: Hatalar sabit varyanslı değildir.

bptest(wage ~year+ age + maritl + education + health + health_ins, data=train)

# P değeri 0.05’ten küçük olduğu için %95 güvenle hatalar sabit varyanslıdır hipotezi reddedilebilir.

par(mfrow = c(2,2))
plot(modelbest)


# Residuallar ile Fitted Valuelar arasındaki grafik incelendiğinde bir daralma gözlemlenmemiştir. Hataların sabit varyanslı olmadığı görülmektedir.
# Ayrıca uç değerlerin modeli baskıladığı da dikkat çekmektedir. Uç değerlerin veri setinden çıkarılması mantıklı bir hareket olabilir. 

########################
### Uç Değer Analizi ###
########################

standardized.residuals<-
  modelbest$residuals/sqrt(var(modelbest$residuals))

dev.off()
plot(modelbest$fitted.values,standardized.residuals, xlab = "Tahmin Değerleri", ylab = "Standarlaştırılmış Artıklar")
abline(h=0)
abline(h=-3)
abline(h=3)

# Tahmin değerleri ile standartlaştırılmış artıklar grafiği incelendiğinde;
length(which(abs(standardized.residuals)>3))
# Standartlaştırılmış artık değeri -3'den küçük, 3'den büyük olan 62 adet artık saptanmıştır.
# Bu 62 değer, uç değer olarak yorumlanmıştır. 

# Aykırı değer olarak sağtanan gözlemlerin indeks numaraları aşağıdaki gibidir.
which(abs(standardized.residuals)>3)

########################
### Kaldıraç Analizi ###
########################

st.res <- modelbest$residuals/sd(modelbest$residuals) #modelin hatasını bulmak gerekir, degrees of freedom hatası yüzünden
plot(hatvalues(modelbest),st.res)
abline(h=c(-2,2),v=2*mean(hatvalues(modelbest)))

# Hat değerleri ile standardized residuals arasındaki grafik incelendiğinde hiç kötü kaldıraç olmadığı gözlemlenmiştir. İyi kaldıraç sayısı ise 86 olarak saptanmıştır. 
which(hatvalues(modelbest)>2*mean(hatvalues(modelbest)))
length(which(hatvalues(modelbest)>2*mean(hatvalues(modelbest))))

######################
### Cooks Distance ###
######################

plot(train$wage,cooks.distance(modelbest))
abline(h=4/(length(train$wage)-6))

# Cooks Discance grafiği incelendiğinde 101 adet etkin gözlem olduğu saptanmıştır. Grafikte outlier olarak saptanan gözlemlerin etkin gözlem olduğu fark edilmiştir. Demek ki aykırı gözlemler modelin eğimine çok etki yapıyor. Model geliştirmede bu konuda değişiklikler yapmak mantıklı olabilir.
length(which(cooks.distance(modelbest) > 4/(length(train$wage)-6)))
# Cooks Distance'a göre etkin gözlemlerin indeks numaraları aşağıdaki gibidir. 
which(cooks.distance(modelbest) > 4/(length(train$wage)-6))


########################
### Model Geliştirme ###
########################

# Kurulan modellerin hepsinde medeni durumu belirten factorun sadece married level'ının anlamlı olduğu saptanmıştır. Bu sebeple model geliştirme yöntemine bu factorun seviyesini ikiye düşürerek devam edilecektir.
# Married 1 - 0

train %<>%  mutate(maritl = ifelse(train$maritl == "2. Married", 1, 0)) 
str(train)
train$maritl <- as.factor(train$maritl)

# Year değişkeni sayısal değer olmasına rağmen kategorik davranış sergilediği için bu değişken kategoriğe çevrilmek istenilmiştir. Rsquare ve Adj. R square değerlerindeki 0.004'lük artık olması bu dönüşümün yapılmasına sebep olmuştur. Aslında çok büyük bir etki olmamasına rağmen; yine de bu artışın sağlanması amaçlanmıştır.


train$year <- as.factor(train$year)
str(train)

# Bu kısma kadar yapılan değişikliklerin hiçbiri modelin varsayımları sağlamasına yardımcı olmamıştır. 
# Bağımlı değişkenin normal dağılmıyor oluşu buna sebep olabilir.
# Bu sebeple bağımlı değişkenin normal dağılmasını sağlamak modeli iyileştirmek adına yapılabilecek ilk adım olabilir.
# bestNormalize paketi kullanılarak Y'nin normal dağılması sağlanmaya çalışılacaktır.

bestNormalize(test$wage)

# bestNormalize paketi bağımlı değişkenin normalleşmesi için aşağıdaki dönüşümleri önermiştir:
# boxcox, yeo-johnson, arcsinh
# Buna ek olarak tanımlayıcı istatistik kısmında logaritması alınan wage değişkenin sola çarpık olduğu gözlemlenmişti. Bu sebeple bestNormalize'ın önermelerine ek olarak bir de logaritması alınmış wage'in karekökü alınacaktır.

## boxcox dönüşümü
boxwage <- boxcox(train$wage)

ad.test(boxwage$x.t)
shapiro.test(boxwage$x.t)

# Box Cox dönüşümü sonrası Y'nin normal dağılması sağlanamamıştır.

## yeo- johnson dönüşümü
yeowage <- yeojohnson(train$wage)
ad.test(yeowage$x.t)
shapiro.test(yeowage$x.t)

# Yeo-Johnson dönüşümü sonrası Y'nin normal dağılması sağlanamamıştır.

## arcsinh dönüşümü
arcwage <- arcsinh_x(train$wage)
ad.test(arcwage$x.t)
shapiro.test(arcwage$x.t)

# arcsinh dönüşümü sonrası Y'nin normal dağılması sağlanamamıştır.


# sqrt(log(x)) dönüşümü
donusuwage <- sqrt(log(train$wage))
ad.test(donusuwage)
shapiro.test(donusuwage)

# sqrt(log(x)) dönüşümü sonrası Y'nin normal dağılması sağlanamamıştır.

# Önerilen hiçbir dönüşüm Y'nin normalleşmesini sağlamamıştır. Tanımlayıcı istatistikler incelendiği kısımda wage değişkeninin çift tepeli olduğu ve aykırı gözlemlerin çok olduğu fark edilmişti.
# Bu sebeple ikinci tepeye sebep olan aykırı gözlemlerin 
# Uç değerlerin bağımlı değişkenden çıkarılmasının normalleştirmeyi sağlayabileceği düşünülmektedir.

### Aykırı Gözlemlerin IQR yöntemi ile çıkarılması

quartiles <- quantile(train$wage, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(train$wage)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

train_no_outlier <- subset(train, train$wage > Lower & train$wage < Upper)

ad.test(train_no_outlier$wage)
shapiro.test(train_no_outlier$wage)

# Uç değerler veri setinden çıkarıldıktan sonra da normalleştirme sağlanamamıştır; ancak biraz olsun yaklaşılmıştır. Histogram grafiği de bu bilgiyi desteklemiştir.

fun <- dnorm(train_no_outlier$wage, mean = mean(train_no_outlier$wage), sd = sd(train_no_outlier$wage))

hist(train_no_outlier$wage, col="burlywood2",
     border="burlywood4", probability = T, ylim = c(0, max(fun)))

lines(density(train_no_outlier$wage), col = 9, lwd = 2)


skewness(train_no_outlier$wage)
#skewness değeri 0'dan büyük olduğu için sağa çarpık olduğu saptanmıştır.

# Uç değerlerin çıkarıldığı bağımlı değişkenin normalliğinin sağlanması için bestNormalize paketi yapılacaktır.

bestNormalize(train_no_outlier$wage)
# bestNormalize paketi aykırı gözlemler çıkarılmış bağımlı değişkenleri normalleştirmek için en iyi yöntemin orderNorm olduğu saptamıştır.
# Ordered Quantile (ORQ) normalleştirme dönüşümü, bir vektörün değerlerinin yüzdelik dilimlerine eşlendiği ve daha sonra normal dağılımın aynı yüzdelik dilimine eşlendiği sıralama tabanlı bir prosedürdür. Bağların varlığı olmadan, bu esasen dönüşümün düzgün bir dağılıma yol açmasını garanti eder.

orderwage <- orderNorm(train_no_outlier$wage)

ad.test(orderwage$x.t)
shapiro.test(orderwage$x.t)

# Anderson-Darling ve Shapiro-Wilk normallik testlerinin sonucuna göre bağımlı değişkenin normalliği sağlanmıştır.
# Bu sebeple bu dönüşüm yapılmış bağımlı değişken ile tekrar model kurulmak istenilmiştir.

modelout <- lm(wage ~ year + age + maritl + education + health + health_ins, data= train_no_outlier)
summary(modelout)

# Aykırı gözlemlerin çıkarılması ve  medeni durum faktörünün seviyesinin ikiye düşürülmesi sonrasında oluşturulan yeni nodelin çıktısı incelendiğinde;
# yeni model geçerli olmuş, bütün bağımsız değişkenler anlamlı olarak görülmüştür. 
# Adj. Rsquare ve Rsquare değerlerinde 0.044'lük bir artık gözlemlenmiştir. 


##########################################################################################

############################################
### Geliştirilen Model Varsayım Kontrolü ###
############################################

############################
### Hataların Normalliği ###
############################

fun <- dnorm(modelout$residuals, mean = mean(modelout$residuals), sd = sd(modelout$residuals))

hist(modelout$residuals,col="brown1",
     border="brown4", ylab = "Frekans", xlab = "Hatalar", main = "Hataların Histogram Grafiği", probability = T, ylim = c(0, max(fun)))


lines(density(modelout$residuals), col = 1, lwd = 2)


# Hataların histogram grafiği incelendiğinde normale oldukça yakın bir grafik görülmektedir. İki taraftaki kuyruklar da uzun olduğu için skewness değerine bakmak daha sağlıklı bir yorum yapılmasını sağlayacaktır.

skewness(modelout$residuals)
# Hataların skewness değeri 0.1 çıkmıştır. Normale oldukça yaklaştığı gözlemlenmiştir. Yine de hipotez testi ile kesin bir yargıya varılmak istenilmiştir.

shapiro.test(modelout$residuals)
# P-value 0.05'ten  küçük olduğu için %95 güvenle değişkenin normal dağıldığı hipotezi reddedilebilir.


#####################################
### HATALARIN SABİT VARYANSLILIĞI ###
#####################################

bptest(wage ~ year + age + maritl + education + health + health_ins, data= train_no_outlier)

# P-value 0.05'ten  küçük olduğu için %95 güvenle hatalar serbest varyanslıdır hipotezi reddedilebilir.

par(mfrow = c(2,2))
plot(modelout)

# Dörtlü model grafiği incelendiğinde;
# Hatalar ile Tahmin değerleri arasındaki grafik incelendiğinde hataların sabit varyanslı olmadığı dikkat çekmektedir. Ayrıca hataların uç değerlerinin grafiğin sol kısmında bir baskılama yaptığı görülmektedir.
# Artıklardaki bu uç değerlerin veri setinden çıkarılmasının modeli daha da iyileştirebileceği ve (belki) varsayımların sağlanabileceği düşünülmektedir.
# Bu yüzden bu işlemler yapılacaktır.

## Artıklara Göre Uç Değerler ##

standardized.residuals<-
  modelout$residuals/sqrt(var(modelout$residuals))
dev.off()
plot(modelout$fitted.values,standardized.residuals, xlab = "Tahmin Değerleri", ylab = "Standarlaştırılmış Artıklar")
abline(h=0)
abline(h=-3)
abline(h=3)

length(which(abs(standardized.residuals)>3))

# Artıklara göre 12 değer uç değer olarak saptanmıştır. Bu değerler veri setinden çıkarılarak yeni model kurulacaktır.

which(abs(standardized.residuals)>3)

train.out.out <- train_no_outlier[-c(which(abs(standardized.residuals)>3)),]

modeloutout <- lm(wage ~year+ age + maritl + education + health + health_ins, data= train.out.out)

summary(modeloutout)

# Artıklara göre uç değer olarak saptanan gözlemlerin veri setinden çıkarılmasıyla oluşan modelin çıktısı incelendiğinde;
# Modelin ve modelde yer alan bütün bağımsız değişkenlerin anlamlı olduğu saptanmıştır.
# Modelin Rsquare ve Adj.Rsquare değerlerinde 0.03'lük bir artık gözlemlenmiştir.
# Varsayımların sağlanıp sağlanmadığı test edilmek istenilmiştir.


shapiro.test(modeloutout$residuals)

# Oldukça yaklaşılmasına rağmen p değeri yine 0.05'ten küçük çıkmıştır. Bu sebeple 95 güvenle hatalar serbest varyanslıdır hipotezi reddedilebilir.

bptest(wage ~year+ age + maritl + education + health + health_ins, data= train.out.out)

# # P-value 0.05'ten  küçük olduğu için %95 güvenle hatalar serbest varyanslıdır hipotezi reddedilebilir.

# Tüm bu analizler sonucunda:
# Modelde yer alan bağımlı değişkenin normalliğinin sağlanması modelin Rsquare ve Adj.Rsquare değerini arttırmış; ancak yine de varsayımları sağlamak mümkün olamamıştır.
# Bu varsayımların sağlanması için standartlaşmış hatalara göre uç değer olarak saptanan gözlemler veri setinden çıkarılmış ve yeni model kurulmuştur.
# Bu modelde hipotez testleri uyguladığında varsayımların yaklaşılmasına rağmen yine sağlanmadığı fark edilmiştir. 
# Tanımlayıcı istatistikler kısmında da değişkenler arasında doğrusal ilişki olmadığı fark edilmişti.
# Bağımlı değişken ile doğrusal olarak söyleyebileceğimiz tek ilişki education ileydi.
# Bu sebeple bu veri setini doğrusal olmayan regresyon modelleriyle denemek daha mantıklı olabilir.
# Wage değişkeni ile doğrusal ilişkisi olabilecek yeni bağımsız değişkenlerin eklenmesi de mantıklı olabilir.


train_deneme <- train_no_outlier
train_deneme[11] <- (train_no_outlier$age)^2
train_deneme[12] <- (train_no_outlier$age)^3

modeldeneme <- lm(wage ~ year + age + V11 + V12 + maritl + education + health + health_ins, data= train_deneme)
summary(modeldeneme)

traindeneme2 <- train_deneme
traindeneme2 %<>%  mutate(year = ifelse(traindeneme2$year < 2004, 0, 1)) 
traindeneme2$year <- as.factor(traindeneme2$year)
str(traindeneme2)
traindeneme2$year <- as.numeric(traindeneme2$year)

modeldeneme2 <- lm(wage ~ year + age + V11 + V12 + maritl + education + health + health_ins, data= traindeneme2)
summary(modeldeneme2)
