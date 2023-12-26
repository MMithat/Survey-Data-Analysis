library(readxl)
library(dplyr)
library(base)
library(stats)
library(tidyverse)
library(maps)
library(sp)
library(mapproj)
library(ggmap)
library(ggplot2)
library(leaflet)

data <- read.csv("/Users/mithatevci/Desktop/Total_data.csv")
uni <- read.csv("/Users/mithatevci/Desktop/üniler.csv")
iller_kordinat <- read_excel("/Users/mithatevci/Desktop/iller_kordinatları.xlsx")
sehir_isim <- read.csv("/Users/mithatevci/Desktop/şehirler.csv")
iller_kordinat$Enlem

bölge <- c()

for (i in data$D.9..Hangi.şehirde.yaşıyorsunuz.) {
    region_out <- case_when(
      i %in% c("ANK","ESK","KAY","KON","NEV","AKS","KRK","NEV","NIG","SIV","YOZ") ~ "İç Anadolu",
      i %in% c("BAL","BIL","BRS","CKL","EDI","IST","KOC","SAK") ~ "Marmara",
      i %in% c("AMA","BOL","ORD","RIZ","SAM","TOK","TRA","ZON") ~ "Karadeniz",
      i %in% c("AFY","AYI","DEN","MAN","MUG","USK","IZM") ~ "Ege",
      i %in% c("ELA","MAL","TEL","VAN") ~ "Doğu Anadolu",
      i %in% c("ADI","BAT","DIY","GAZ","MAR") ~ "Güneydoğu Anadolu",
      i %in% c("ADA","ANT","BRD","HTY","MER","OSM") ~ "Akdeniz",
    )
    print(region_out)
    bölge <- append(bölge, region_out)
  
  
}

survey <- cbind(data,bölge)
survey <- cbind(survey,uni)

table(survey$bölge)

survey$A.1..Sigara.kullanıyor.musunuz.
survey$A.2..Alkol.kullanıyor.musunuz.
vecF3<- survey$F.3..Gelir.durumunuzun.aşağıdaki.göstergeler.üzerinde.etkisini.derecelendiriniz.................1..Hiçbir.zaman.5..Her.zaman.....Sigara.ve.Alkol.tüketimi

vec <- c(survey$F.3..Gelir.durumunuzun.aşağıdaki.göstergeler.üzerinde.etkisini.derecelendiriniz.................1..Hiçbir.zaman.5..Her.zaman.....Sigara.ve.Alkol.tüketimi)
vec1 <- c()

for (k in survey$A.1..Sigara.kullanıyor.musunuz.){
  if(k=="Evet"){
    vec1 <- append(vec1,1)
  }else if (k=="Hayır"){
    vec1 <- append(vec1,0)
  }
}

vec2 <- c()

for (k in survey$A.2..Alkol.kullanıyor.musunuz.){
  if(k=="Evet"){
    vec2 <- append(vec2,1)
  }else if (k=="Hayır"){
    vec2 <- append(vec2,0)
  }
}

vec3<- vec1+vec2
vec3
kritik <- c()
for (l in 1:length(vec3)) {
  if(vec3[l] == 0){
    kritik <- append(kritik,l)
  }
}
table(vec3)
length(vec3)
kritik
for (i in kritik) {
  vecF3[i] <- NA
}
vecF3#sigara ve alkol içmeyenler NA olarak çevirilmiştir

table(vecF3)


library(base)
library(stats)
library(tidyverse)
library(maps)
library(sp)
library(mapproj)
library(ggmap)
library(ggplot2)
library(leaflet)

#Mapping

#Mapping_kordinatlar
#########X kordinat 

as.data.frame(table(survey$HangiÜni))

kordinat_x <- c()

for (i in survey$HangiÜni) {
  kordinat_x_out <- case_when(
    i %in% c("Orta Doğu Teknik Üniversitesi") ~ 39.894244503741184,
    i %in% c("Hacettepe Üniversitesi") ~ 39.87127704620178,
    i %in% c("Gazi Üniversitesi") ~ 39.941582782754644,
    i %in% c("Sakarya Üniversitesi") ~ 40.74220178453988,
    i %in% c("Ankara Üniversitesi") ~ 39.93493724538813,
    i %in% c("Bilkent Üniversitesi") ~ 39.93493724538813,
    i %in% c("Kocaeli Üniversitesi") ~ 40.82208516804715,
    i %in% c("Ondokuz Mayıs Üniversitesi") ~ 41.372328679056785,
    i %in% c("Adnan Menderes Üniversitesi") ~ 37.85355959287673,
  )
  print(kordinat_x_out)
  kordinat_x <- append(kordinat_x, kordinat_x_out)
  
  
}

survey <- cbind(survey,kordinat_x)
survey$kordinat_x

#########Y kordinat

kordinat_y <- c()

for (i in survey$HangiÜni) {
  kordinat_y_out <- case_when(
    i %in% c("Orta Doğu Teknik Üniversitesi") ~ 32.78192445525217,
    i %in% c("Hacettepe Üniversitesi") ~ 32.73351999703783,
    i %in% c("Gazi Üniversitesi") ~ 32.82331339175239,
    i %in% c("Sakarya Üniversitesi") ~ 30.33215003993404,
    i %in% c("Ankara Üniversitesi") ~ 32.833915141761935,
    i %in% c("Bilkent Üniversitesi") ~ 32.747445993873185,
    i %in% c("Kocaeli Üniversitesi") ~ 32.73351999703783,
    i %in% c("Ondokuz Mayıs Üniversitesi") ~ 36.21091308465523,
    i %in% c("Adnan Menderes Üniversitesi") ~ 27.854146523276135,
  )
  print(kordinat_y_out)
  kordinat_y <- append(kordinat_y, kordinat_y_out)
  
  
}

survey <- cbind(survey,kordinat_y)
survey$kordinat_y

#------MAPPİNG------

table(survey$HangiÜni)

leaflet(survey) %>% 
  addProviderTiles("OpenStreetMap") %>% 
  addMarkers(lng = kordinat_y , lat = kordinat_x)

as.data.frame(table(survey$HangiÜni))

ggplot(as.data.frame(table(survey$HangiÜni)),
       aes(x= Freq,
           y= Var1))+
  geom_bar(stat = "identity")


kirmizi <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = "red"
)

mor <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = "purple"
)

orange <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = "orange"
)

green <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = "green"
)

leaflet(iller_kordinat) %>% 
  addProviderTiles("OpenStreetMap") %>% 
  addMarkers(lng = iller_kordinat$Boylam , lat = iller_kordinat$Enlem) %>% 
  addAwesomeMarkers(lng = 32.85411 , lat = 39.92077 , icon = kirmizi ) %>% #ankara
  addAwesomeMarkers(lng = 28.97696 , lat = 41.00527 , icon = mor ) %>% #istabul
  addAwesomeMarkers(lng = 27.12872 , lat = 38.41885 , icon = orange ) %>% #kocaeli
  addAwesomeMarkers(lng = 29.88152 , lat = 40.85327 , icon = orange ) %>% #izmir
  addAwesomeMarkers(lng = 30.43576 , lat = 40.69400 , icon = green ) #sakarya

sort(table(survey$D.9..Hangi.şehirde.yaşıyorsunuz.))


#Aylık Barınma Genel
aylıkbarınma_numeric <- as.factor(survey$B.3..Aylık.barınma.masrafınız.kaç...dir.)
table(aylıkbarınma_numeric)

aylıkbarınma <- as.data.frame(table(aylıkbarınma_numeric))

ggplot(aylıkbarınma,
       aes(x= aylıkbarınma_numeric,
           y= Freq))+
  geom_bar(stat = "identity")

#Ankara
ank_aylık_barınma<- survey[survey$D.9..Hangi.şehirde.yaşıyorsunuz.=="ANK",
                            "B.3..Aylık.barınma.masrafınız.kaç...dir."]
ank_ab_numeric <- as.factor(ank_aylık_barınma)

ank_ab_numeric_dataframe <- as.data.frame(table(ank_ab_numeric))

table(ank_aylık_barınma)
table(ank_ab_numeric)

ggplot(ank_ab_numeric_dataframe,
       aes(x= ank_ab_numeric,
           y= Freq))+
  geom_bar(stat = "identity")

#Istanbul

ist_aylık_barınma<- survey[survey$D.9..Hangi.şehirde.yaşıyorsunuz.=="IST",
                            "B.3..Aylık.barınma.masrafınız.kaç...dir."]
table(ist_aylık_barınma)

ist_ab_numeric <- as.factor(ist_aylık_barınma)
ist_ab_numeric_dataframe <- as.data.frame(table(ist_ab_numeric))

ggplot(ist_ab_numeric_dataframe,
       aes(x= ist_ab_numeric,
           y= Freq))+
  geom_bar(stat = "identity")


#Gpa Düzenleme
gpa <- as.data.frame(survey$D.4..Kümülatif.ortalamanız.nedir.)

gpa_vec <- survey$D.4..Kümülatif.ortalamanız.nedir.

gpa_vec[gpa_vec < 1 | gpa_vec > 4] <- NA

gpa_vec <- gpa_vec[-which(gpa_vec < 1 & gpa_vec > 4)]



#Aylık Sigara Düzenleme
aylık_sig<- as.factor(survey$A.1.1.Aylık.sigara.masrafınız.kaç...dir..)
table(aylık_sig)

aylık_sig[aylık_sig == "" ] <- "0"


96750/165
#586.3636


survey$Y.4..Aylık.ortalama.yemek.masrafınız.kaç...dir.
table(survey$Y.4..Aylık.ortalama.yemek.masrafınız.kaç...dir.)

yemek_dataframe <- as.data.frame(table(survey$Y.4..Aylık.ortalama.yemek.masrafınız.kaç...dir.))



####################

install.packages(c("base", "stats"))
library(base)
library(stats)

freq_table <- data.frame(
  category = c("0", "1 - 500", "1001 - 1500", "1501 - 2000", "2001 - 2500", "2501 - 3000", "3001 - 3500", "3501 - 4000", "4001 - 4500", "4501 - 5000", "5001 - 5500", "501 - 1000", "5501 - 6000", "6000+"),
  count = c(3, 72, 104, 84, 42, 27, 19, 5, 7, 1, 2, 118, 1, 6)
)

freq_table$lower <- as.numeric(sub(" .*", "", freq_table$category))
freq_table$upper <- as.numeric(sub(".*- ", "", freq_table$category))

mean_value <- weighted.mean((freq_table$lower + freq_table$upper) / 2, freq_table$count)
print(mean_value)
summary(survey$D.4..Kümülatif.ortalamanız.nedir.)

sayı_data <- as.data.frame(sayı)


deneme1 <- lm(survey$`Y-3. Yeme düzeninizle ilgili soruları derecelendiriniz                                                    (1: Hiçbir zaman, 5: Her zaman) >> Yeterli miktarda besleniyorum`~survey$`Y-1. Aylık beslenme ihtiyacınızı nasıl karşıladığınızı derecelendiriniz.                        (1: Hiçbir zaman, 5: Her zaman) >> Okul yemekhanesini kullanıyorum`+
                survey$`Y-1. Aylık beslenme ihtiyacınızı nasıl karşıladığınızı derecelendiriniz.                        (1: Hiçbir zaman, 5: Her zaman) >> Ücretsiz yemek olan yerlerde yiyorum`+
                survey$`Y-1. Aylık beslenme ihtiyacınızı nasıl karşıladığınızı derecelendiriniz.                        (1: Hiçbir zaman, 5: Her zaman) >> Dışardan yiyorum`+
                survey$`Y-3. Yeme düzeninizle ilgili soruları derecelendiriniz                                                    (1: Hiçbir zaman, 5: Her zaman) >> Yediğim yemekleri kaliteli buluyorum`+
                survey$`Y-3. Yeme düzeninizle ilgili soruları derecelendiriniz                                                    (1: Hiçbir zaman, 5: Her zaman) >> Yeterli miktarda protein alıyorum`+
                survey$`Y-3. Yeme düzeninizle ilgili soruları derecelendiriniz                                                    (1: Hiçbir zaman, 5: Her zaman) >> Yeterli miktarda vitamin alıyorum`+
                survey$`Y-3. Yeme düzeninizle ilgili soruları derecelendiriniz                                                    (1: Hiçbir zaman, 5: Her zaman) >> Sağlıklı olan yerine ucuz olanı tercih ediyorum`+
                survey$`D-1. Cinsiyetiniz Nedir?`)
summary(deneme1)


okul_yemekhanesi<- as.numeric(survey$`Y-1. Aylık beslenme ihtiyacınızı nasıl karşıladığınızı derecelendiriniz.                        (1: Hiçbir zaman, 5: Her zaman) >> Okul yemekhanesini kullanıyorum`)
ucretsiz_yemek <- as.numeric(survey$`Y-1. Aylık beslenme ihtiyacınızı nasıl karşıladığınızı derecelendiriniz.                        (1: Hiçbir zaman, 5: Her zaman) >> Ücretsiz yemek olan yerlerde yiyorum`)
disardan_yemem <- as.numeric(survey$`Y-1. Aylık beslenme ihtiyacınızı nasıl karşıladığınızı derecelendiriniz.                        (1: Hiçbir zaman, 5: Her zaman) >> Dışardan yiyorum`)
yemek_kalitesi<- as.numeric(survey$`Y-3. Yeme düzeninizle ilgili soruları derecelendiriniz                                                    (1: Hiçbir zaman, 5: Her zaman) >> Yediğim yemekleri kaliteli buluyorum`)
yeterli_protein <- as.numeric(survey$`Y-3. Yeme düzeninizle ilgili soruları derecelendiriniz                                                    (1: Hiçbir zaman, 5: Her zaman) >> Yeterli miktarda protein alıyorum`)
yeterli_vitamin <- as.numeric(survey$`Y-3. Yeme düzeninizle ilgili soruları derecelendiriniz                                                    (1: Hiçbir zaman, 5: Her zaman) >> Yeterli miktarda vitamin alıyorum`)
saglik_ucuz <- as.numeric(survey$`Y-3. Yeme düzeninizle ilgili soruları derecelendiriniz                                                    (1: Hiçbir zaman, 5: Her zaman) >> Sağlıklı olan yerine ucuz olanı tercih ediyorum`)
et_balık <- as.numeric(survey$`Y-2. Hangi besin ağırlıklı beslendiğinizi derecelendiriniz.                                            (1: Hiçbir zaman, 5: Her zaman) >> Et/Balık`)
makarna <- as.numeric(survey$`Y-2. Hangi besin ağırlıklı beslendiğinizi derecelendiriniz.                                            (1: Hiçbir zaman, 5: Her zaman) >> Makarna/Hamur işi`)
fast_food <- as.numeric(survey$`Y-2. Hangi besin ağırlıklı beslendiğinizi derecelendiriniz.                                            (1: Hiçbir zaman, 5: Her zaman) >> Fast Food`)
tahil <- as.numeric(survey$`Y-2. Hangi besin ağırlıklı beslendiğinizi derecelendiriniz.                                            (1: Hiçbir zaman, 5: Her zaman) >> Tahıl`)
süt_ürünleri <- as.numeric(survey$`Y-2. Hangi besin ağırlıklı beslendiğinizi derecelendiriniz.                                            (1: Hiçbir zaman, 5: Her zaman) >> Süt ve süt ürünleri`)
sebze <- as.numeric(survey$`Y-2. Hangi besin ağırlıklı beslendiğinizi derecelendiriniz.                                            (1: Hiçbir zaman, 5: Her zaman) >> Sebze`)
meyve <- as.numeric(survey$`Y-2. Hangi besin ağırlıklı beslendiğinizi derecelendiriniz.                                            (1: Hiçbir zaman, 5: Her zaman) >> Meyve`)




deneme2 <- lm(survey$`Y-3. Yeme düzeninizle ilgili soruları derecelendiriniz                                                    (1: Hiçbir zaman, 5: Her zaman) >> Yeterli miktarda besleniyorum`~survey$`Y-1. Aylık beslenme ihtiyacınızı nasıl karşıladığınızı derecelendiriniz.                        (1: Hiçbir zaman, 5: Her zaman) >> Okul yemekhanesini kullanıyorum`+
                survey$`Y-1. Aylık beslenme ihtiyacınızı nasıl karşıladığınızı derecelendiriniz.                        (1: Hiçbir zaman, 5: Her zaman) >> Ücretsiz yemek olan yerlerde yiyorum`+
                survey$`Y-1. Aylık beslenme ihtiyacınızı nasıl karşıladığınızı derecelendiriniz.                        (1: Hiçbir zaman, 5: Her zaman) >> Dışardan yiyorum`+
                survey$`Y-3. Yeme düzeninizle ilgili soruları derecelendiriniz                                                    (1: Hiçbir zaman, 5: Her zaman) >> Yediğim yemekleri kaliteli buluyorum`+
                survey$`Y-3. Yeme düzeninizle ilgili soruları derecelendiriniz                                                    (1: Hiçbir zaman, 5: Her zaman) >> Yeterli miktarda protein alıyorum`+
                survey$`Y-3. Yeme düzeninizle ilgili soruları derecelendiriniz                                                    (1: Hiçbir zaman, 5: Her zaman) >> Yeterli miktarda vitamin alıyorum`+
                survey$`Y-3. Yeme düzeninizle ilgili soruları derecelendiriniz                                                    (1: Hiçbir zaman, 5: Her zaman) >> Sağlıklı olan yerine ucuz olanı tercih ediyorum`+
                survey$`Y-2. Hangi besin ağırlıklı beslendiğinizi derecelendiriniz.                                            (1: Hiçbir zaman, 5: Her zaman) >> Et/Balık`+
                survey$`Y-2. Hangi besin ağırlıklı beslendiğinizi derecelendiriniz.                                            (1: Hiçbir zaman, 5: Her zaman) >> Makarna/Hamur işi`+
                survey$`Y-2. Hangi besin ağırlıklı beslendiğinizi derecelendiriniz.                                            (1: Hiçbir zaman, 5: Her zaman) >> Fast Food`+
                survey$`Y-2. Hangi besin ağırlıklı beslendiğinizi derecelendiriniz.                                            (1: Hiçbir zaman, 5: Her zaman) >> Tahıl`+
                survey$`Y-2. Hangi besin ağırlıklı beslendiğinizi derecelendiriniz.                                            (1: Hiçbir zaman, 5: Her zaman) >> Süt ve süt ürünleri`+
                survey$`Y-2. Hangi besin ağırlıklı beslendiğinizi derecelendiriniz.                                            (1: Hiçbir zaman, 5: Her zaman) >> Sebze`+
                survey$`Y-2. Hangi besin ağırlıklı beslendiğinizi derecelendiriniz.                                            (1: Hiçbir zaman, 5: Her zaman) >> Meyve`)
summary(deneme2)

as.numeric(survey$`Y-3. Yeme düzeninizle ilgili soruları derecelendiriniz                                                    (1: Hiçbir zaman, 5: Her zaman) >> Yeterli miktarda besleniyorum`)

den_1_res <- residuals.lm(deneme1)
plot(den_1_res)
qqPlot(den_1_res)
#install.packages("olsrr")
library(olsrr)
ols_plot_resid_qq(deneme1)
ols_test_normality(deneme1)
ols_plot_resid_fit(deneme1)
ols_plot_resid_hist(deneme1)

shapiro.test(den_1_res)
yeterli_miktar_beslenme <- as.numeric(survey$`Y-3. Yeme düzeninizle ilgili soruları derecelendiriniz                                                    (1: Hiçbir zaman, 5: Her zaman) >> Yeterli miktarda besleniyorum`)
okul_yemekhanesi<- as.numeric(survey$`Y-1. Aylık beslenme ihtiyacınızı nasıl karşıladığınızı derecelendiriniz.                        (1: Hiçbir zaman, 5: Her zaman) >> Okul yemekhanesini kullanıyorum`)
ucretsiz_yemek <- as.numeric(survey$`Y-1. Aylık beslenme ihtiyacınızı nasıl karşıladığınızı derecelendiriniz.                        (1: Hiçbir zaman, 5: Her zaman) >> Ücretsiz yemek olan yerlerde yiyorum`)
disardan_yemem <- as.numeric(survey$`Y-1. Aylık beslenme ihtiyacınızı nasıl karşıladığınızı derecelendiriniz.                        (1: Hiçbir zaman, 5: Her zaman) >> Dışardan yiyorum`)
yemek_kalitesi<- as.numeric(survey$`Y-3. Yeme düzeninizle ilgili soruları derecelendiriniz                                                    (1: Hiçbir zaman, 5: Her zaman) >> Yediğim yemekleri kaliteli buluyorum`)
yeterli_protein <- as.numeric(survey$`Y-3. Yeme düzeninizle ilgili soruları derecelendiriniz                                                    (1: Hiçbir zaman, 5: Her zaman) >> Yeterli miktarda protein alıyorum`)
yeterli_vitamin <- as.numeric(survey$`Y-3. Yeme düzeninizle ilgili soruları derecelendiriniz                                                    (1: Hiçbir zaman, 5: Her zaman) >> Yeterli miktarda vitamin alıyorum`)
saglik_ucuz <- as.numeric(survey$`Y-3. Yeme düzeninizle ilgili soruları derecelendiriniz                                                    (1: Hiçbir zaman, 5: Her zaman) >> Sağlıklı olan yerine ucuz olanı tercih ediyorum`)
et_balık <- as.numeric(survey$`Y-2. Hangi besin ağırlıklı beslendiğinizi derecelendiriniz.                                            (1: Hiçbir zaman, 5: Her zaman) >> Et/Balık`)
makarna <- as.numeric(survey$`Y-2. Hangi besin ağırlıklı beslendiğinizi derecelendiriniz.                                            (1: Hiçbir zaman, 5: Her zaman) >> Makarna/Hamur işi`)
fast_food <- as.numeric(survey$`Y-2. Hangi besin ağırlıklı beslendiğinizi derecelendiriniz.                                            (1: Hiçbir zaman, 5: Her zaman) >> Fast Food`)
tahil <- as.numeric(survey$`Y-2. Hangi besin ağırlıklı beslendiğinizi derecelendiriniz.                                            (1: Hiçbir zaman, 5: Her zaman) >> Tahıl`)
süt_ürünleri <- as.numeric(survey$`Y-2. Hangi besin ağırlıklı beslendiğinizi derecelendiriniz.                                            (1: Hiçbir zaman, 5: Her zaman) >> Süt ve süt ürünleri`)
sebze <- as.numeric(survey$`Y-2. Hangi besin ağırlıklı beslendiğinizi derecelendiriniz.                                            (1: Hiçbir zaman, 5: Her zaman) >> Sebze`)
meyve <- as.numeric(survey$`Y-2. Hangi besin ağırlıklı beslendiğinizi derecelendiriniz.                                            (1: Hiçbir zaman, 5: Her zaman) >> Meyve`)


mlr_1 <- lm(yeterli_miktar_beslenme ~ okul_yemekhanesi+
              ucretsiz_yemek+
              disardan_yemem+
              yemek_kalitesi+
              yeterli_protein+
              yeterli_vitamin+
              saglik_ucuz)

summary(mlr_1)
residuals_mlr_1 <- residuals.lm(mlr_1)
plot(resid(mlr_1))
library(olsrr)
plot(mlr_1)
plot(residuals_mlr_1)
qqPlot(residuals_mlr_1)

ols_plot_resid_qq(mlr_1 )
ols_test_normality(mlr_1)
ols_plot_resid_fit(mlr_1)
ols_plot_resid_hist(mlr_1)

?ols_plot_resid_qq

library(car)
vif(mlr_1)