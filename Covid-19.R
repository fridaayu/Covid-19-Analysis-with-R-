library(httr)
cov_ind <- GET("https://data.covid19.go.id/public/api/update.json") #akses API
status_code(cov_ind) #cek status,jika outputnya 200 berarti data sukses diakses
head(cov_ind) #untuk tau isinya apa aja

cov_ekstrak_raw <- content(cov_ind,as="parsed",simplifyVector=TRUE)
str(cov_ekstrak_raw)
cov_in_raw <- cov_ekstrak_raw$update
lapply(cov_in_raw,names)

#input data
cov_jatim <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_TIMUR.json")
status_code(cov_jatim)
cov_jateng <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_TENGAH.json")
status_code(cov_jateng)
cov_jabar <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_BARAT.json")
status_code(cov_jabar)

#ekstrak 
cov_jatim_raw <- content(cov_jatim,as="parsed",simplifyVector=TRUE)
cov_jatim_update <- cov_ekstrak_raw$update
cov_jatim_raw$last_date
cov_jatim_raw$kasus_total
cov_jatim_raw$meninggal_persen
cov_jatim_raw$sembuh_persen

#perkembangan
cov_jatim_raw$list_perkembangan$KASUS
cov_jatim_raw$list_perkembangan$MENINGGAL
cov_jatim_raw$list_perkembangan$SEMBUH
str(cov_jatim_raw)
cov_jatim_kembang <- cov_jatim_raw$list_perkembangan

#cleaning data
library(dplyr)
#mengubah format tanggal
new_cov_jatim_kembang <- cov_jatim_kembang%>%
  mutate(
    tanggal=as.POSIXct(tanggal/1000,origin="1970-01-01"),
    tanggal=as.Date(tanggal)
  )
str(new_cov_jatim_kembang)

#Eksplorasi
library(ggplot2)
install.packages("hrbrthemes")
library(hrbrthemes)
#eksplorasi jumlah kasus perhari
ggplot(new_cov_jatim_kembang,aes(x=tanggal,y=KASUS))+geom_col()+ 
  geom_line(colour="red")+geom_point(colour="blue")+
  labs(x="Bulan",y="Jumlah Kasus",title = "Jumlah Kasus COVID-19 di Jawa Timur",caption = "Sumber data: covid.19.go.id")
#eksplorasi jumlah meninggal perhari
ggplot(new_cov_jatim_kembang, aes(tanggal,MENINGGAL))+
  geom_col(fill="blue")+labs(x="Bulan",y="Jumlah meninggal",
                             title = "Jumlah meninggal COVID-19 di Jawa Timur",
                             caption = "Sumber data: covid.19.go.id")
#eksplorasi jumlah pasien sembuh 
ggplot(new_cov_jatim_kembang,aes(tanggal,SEMBUH))+geom_col(fill="green")+
  labs(x="Bulan",y="Jumlah pasien sembuh",title = "Jumlah Pasien Sembuh COVID-19 di Jawa Timur",
       caption = "Sumber data: covid.19.go.id")
#eksplorasi per minggu
library(dplyr)
library(lubridate)
#untuk tau apalah kasus minggu lalu lebih baik/buruk dari sekarang
new_cov_jatim_minggu <- new_cov_jatim_kembang%>%
  count(
    tahun=year(tanggal),
    minggu_ke=week(tanggal),
    wt=KASUS,
    name = "jumlah"
  )
glimpse(new_cov_jatim_minggu) #u/ inspeksi data

new_cov_jatim_minggu <- new_cov_jatim_minggu%>%
  mutate(jumlah_minggulalu = dplyr::lag(jumlah,1),
         jumlah_minggulalu=ifelse(is.na(jumlah_minggulalu),0,jumlah_minggulalu),
         lebih_baik=jumlah < jumlah_minggulalu)
glimpse(new_cov_jatim_minggu)
new_cov_jatim_minggu$minggu_ke

ggplot(new_cov_jatim_minggu,aes(minggu_ke,jumlah,fill=lebih_baik))+
  geom_col(show.legend = FALSE)+scale_fill_continuous(breaks=12:31,expand=c(0,0))+
  scale_fill_manual(values = c("TRUE"="salmon","FALSE"="black"))+
  labs(x="Minggu ke",y="Jumlah kasus",title = "Kasus Mingguan COVID-19 di Jawa Timur",
       subtitle = "Warna pink menunjukkan bahwa minggu ini jauhlebih baik dari sebelumnya",
       caption = "Sumber data: covid.19.go.id")

#pola dinamika
new_cov_jatim_akumulasi <-
  new_cov_jatim_kembang %>%
  transmute(tanggal,kasus_aktif=cumsum(KASUS)-cumsum(SEMBUH)-cumsum(MENINGGAL),
            kasus_sembuh = cumsum(SEMBUH),kasus_meninggal=cumsum(MENINGGAL))

ggplot(new_cov_jatim_akumulasi,aes(tanggal,kasus_aktif))+geom_line(colour="red")+
  labs(x="Bulan",y="Jumlah aktif",
       title = "Jumlah kasus aktif COVID-19 di Jawa Timur",
       caption = "Sumber data: covid.19.go.id")

ggplot(new_cov_jatim_akumulasi,aes(tanggal,kasus_sembuh))+geom_line(colour="green")+
  labs(x="Bulan",y="Jumlah sembuh",
       title = "Jumlah kasus sembuh COVID-19 di Jawa Timur",
       caption = "Sumber data: covid.19.go.id")

ggplot(new_cov_jatim_akumulasi,aes(tanggal,kasus_meninggal))+geom_line(colour="black")+
  labs(x="Bulan",y="Jumlah meninggal",
       title = "Jumlah kasus meinggal COVID-19 di Jawa Timur",
       caption = "Sumber data: covid.19.go.id")


#membuat pivot
library(tidyr)
dim(new_cov_jatim_akumulasi)

new_covjatim_akumulasi_pivot <-
  new_cov_jatim_akumulasi%>%
  gather(key="kategori",
         value="jumlah",-tanggal)%>%
  mutate(kategori=sub(pattern = "kasus_",replacement = "",kategori))
dim(new_covjatim_akumulasi_pivot)
glimpse(new_covjatim_akumulasi_pivot)

ggplot(new_covjatim_akumulasi_pivot,aes(tanggal,jumlah,colour=(kategori)))+geom_line()+
  scale_y_continuous(sec.axis = dup_axis(name = NULL))+
  scale_colour_manual(values = c("aktif"="red","sembuh"="green","meninggal"="black"),
                      labels=c("aktif","meninggal","sembuh"))+labs(x=NULL,y="Jumlah kasus akumulasi",
                                                                  title = "Profil Kasus CIVID-19 di Jawa Timur",
                                                                  caption = "Sumber data: covid.19.go.id")

#eksplorasi berdasarkan jenis kelamin
jk_perawatan <- cov_jatim_raw$data$perawatan$jenis_kelamin$list_data
cov_jatim_raw$data$kasus$jenis_kelamin$list_data
