if(!require("lubridate")) install.packages("lubridate"); library("lubridate") # for date conversion
if(!require("tidyverse")) install.packages("tidyverse"); library("tidyverse") # tidyr,dplyr,ggplot
if(!require("zoo")) install.packages("zoo"); library("zoo") # for date conversion
if(!require("reshape2")) install.packages("reshape2"); library("tidyverse") ## for data conversion
if(!require("gridExtra")) install.packages("gridExtra"); library("gridExtra") ## for grid.arrange
if(!require("data.table")) install.packages("data.table"); library("data.table") ## for data conversion
if(!require("eurostat")) install.packages("eurostat"); library("eurostat") ## for eurostat data import
if(!require("ggthemes")) install.packages("ggthemes"); library("ggthemes") ## ggplot themes

## Data import
# GDP

gdp <- get_eurostat("namq_10_gdp", filters=list(s_adj="SCA",
                                                geo=c("EA19","DE", "SE", "UK"),
                                                na_item="B1GQ",
                                                unit="CLV15_MEUR"))
gdp <- gdp%>% select(time,values, geo)%>%
        group_by(geo)%>% 
        spread(geo,values)%>%
        filter(time>="2002-01-01")%>%
        mutate(DE=(DE-lag(DE,4))/lag(DE,4)*100,
               EA19=(EA19-lag(EA19,4))/lag(EA19,4)*100,
               SE=(SE-lag(SE,4))/lag(SE,4)*100,
               UK=(UK-lag(UK,4))/lag(UK,4)*100)
gdp$time <- as.yearqtr(gdp$time)
gdp<- filter(gdp, time>="2008-01-01")


# LT Eksportas pagal valstybes
Sys.setlocale("LC_ALL","Lithuanian")
df <- read.delim ("https://raw.githubusercontent.com/tomasdzedulionis/Data/master/lteksport.csv", header=TRUE, stringsAsFactors = FALSE, sep=";", encoding="UTF-8")
df <- df%>% rename(grupe=X.U.FEFF.aa)%>%
        mutate(JAV11=(parse_number(df$JAV.1)/100),
               JK11=(parse_number(df$JK.1)/100),
               SE11=(parse_number(df$SE)/100),
               DE11=parse_number(df$DE)/100)%>%
        select(grupe, JAV11, JK11,SE11,DE11)


# Pagrindinis Lietuvos eksportas

dff <- read.csv("https://raw.githubusercontent.com/tomasdzedulionis/Data/master/Eksportas.csv", header=TRUE, encoding="UTF-8", stringsAsFactors = FALSE)
Sys.setlocale("LC_ALL","Lithuanian")
dff <- dff%>% 
        filter(Šalys.ir.teritorijos.Kodas!="QS")%>%
        top_n(30)%>%
        select(Šalys.ir.teritorijos.Pavadinimas, X)%>%
        mutate(Proc=as.numeric(sub("%","",X)))%>%
        mutate_if(is.numeric, ~round(., 1))

# Ekonomikos atvirumas

exp <- get_eurostat("tet00003", filters=list(time="2019"))
exp <- exp%>% filter(geo%in%c("AT", "BE", "CY", "EE", "FI", "FR", "DE", "GR", "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PT", "SK", "SI", "ES","BG", "HR", "CZ", "DK", "HU", "RO", "SE","EL","EU28"))%>%
        select(geo,values)%>%
        rename(export=values)
imp <- get_eurostat("tet00004", filters=list(time="2019"))
imp <- imp%>% filter(geo%in%c("AT", "BE", "CY", "EE", "FI", "FR", "DE", "GR", "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PT", "SK", "SI", "ES","BG", "HR", "CZ", "DK", "HU","RO", "SE","EL","EU28"))%>%
        select(geo,values)%>%
        rename(import=values)
dfff <- merge(exp,imp)
dfff <- dfff%>%mutate(trade=import+export)%>%
        mutate_if(is.numeric, ~round(., 0))

# Vokietijos PMI ir BVP

Sys.setlocale("LC_ALL", "english")
german <- read.delim("https://raw.githubusercontent.com/tomasdzedulionis/Data/master/GermanyManufacturing.txt", header=FALSE,stringsAsFactors = FALSE)
german<- german%>%select(V1,V2)%>%
        rename(PMI=V2, Date=V1)%>%
        slice(-1)%>%
        filter(!grepl("[1-3][0-9]([,])", Date))
german$Date <- format(as.Date(german$Date, format="%b %d,%Y"),"%Y.%m.%d")
german$Date <- as.Date(german$Date, format="%Y.%m.%d")
german$Date <- german$Date %m+% months(-1)
german1 <- german
setDT(german)
german <- german[, mean(PMI), keyby = .(year(Date), quarter(Date))]
german$Date<-as.yearqtr((paste0(german$year,"Q",german$quarter)))



germser<- read.delim("https://raw.githubusercontent.com/tomasdzedulionis/Data/master/GermanyServices.txt", header=FALSE, stringsAsFactors = FALSE)
germser<- germser%>%select(V1,V2)%>%
        rename(Date=V1, PMI=V2)%>%
        slice(-1)%>%
        filter(!grepl("[1-3][0-9]([,])", Date))
germser$Date <- format(as.Date(germser$Date, format="%b %d,%Y"),"%Y-%m-%d")
germser$Date <- as.Date(germser$Date, format="%Y-%m-%d")
germser$Date <- germser$Date %m+% months(-1)
germser1 <- germser
setDT(germser)
germser <- germser[, mean(PMI), keyby = .(year(Date), quarter(Date))]
germser$Date<-as.yearqtr((paste0(germser$year,"Q",germser$quarter)))


germcom<-read.delim("https://raw.githubusercontent.com/tomasdzedulionis/Data/master/germanycomposite.txt", header=TRUE, stringsAsFactors = FALSE)
germcom<- germcom %>%select(Date,X)%>%
        rename(PMI=X)%>%
        filter(!grepl("[1-3][0-9]([,])", Date))
germcom$Date <- format(as.Date(germcom$Date, format="%b %d,%Y"),"%Y.%m.%d")
germcom$Date <- as.Date(germcom$Date, format="%Y.%m.%d")
germcom$Date <- germcom$Date %m+% months(-1)
germcom1<-germcom
setDT(germcom)
germcom <- germcom[, mean(PMI), keyby = .(year(Date), quarter(Date))]
germcom$Date<-as.yearqtr((paste0(germcom$year,"Q",germcom$quarter)))

## Svedija PMI ir BVP

Sys.setlocale("LC_ALL", "english")
seman<- read.delim("https://raw.githubusercontent.com/tomasdzedulionis/Data/master/SwedenManufacturing.txt", header=FALSE,stringsAsFactors = FALSE)
seman<- seman%>%select(V1,V2)%>%
        rename(Date=V1, PMI=V2)%>%
        slice(-1)%>%
        filter(!grepl("[1-3][0-9]([,])", Date))
seman$Date <- format(as.Date(seman$Date, format="%b %d,%Y"),"%Y.%m.%d")
seman$Date <- as.Date(seman$Date, format="%Y.%m.%d")
seman$Date <- seman$Date %m+% months(-1)
seman1 <- seman
setDT(seman)
seman <- seman[, mean(PMI), keyby = .(year(Date), quarter(Date))]
seman$Date<-as.yearqtr((paste0(seman$year,"Q",seman$quarter)))

seser<- read.delim("https://raw.githubusercontent.com/tomasdzedulionis/Data/master/SwedenServices.txt", header=FALSE, stringsAsFactors = FALSE)
seser<- seser%>%select(V1,V2)%>%
        rename(Date=V1, PMI=V2)%>%
        slice(-1)%>%
        filter(!grepl("[1-3][0-9]([,])", Date))
seser<-na.omit(seser)
seser$Date <- format(as.Date(seser$Date, format="%b %d,%Y"),"%Y-%m-%d")
seser$Date <- as.Date(seser$Date, format="%Y-%m-%d")
seser$Date <- seser$Date %m+% months(-1)
seser1 <- seser
setDT(seser)
seser <- seser[, mean(PMI), keyby = .(year(Date), quarter(Date))]
seser$Date<-as.yearqtr((paste0(seser$year,"Q",seser$quarter)))

secom <- read.delim("https://raw.githubusercontent.com/tomasdzedulionis/Data/master/sweden.silf-swedbank-composite-pmi.csv", header=TRUE, stringsAsFactors = FALSE)
secom <- secom %>% select(Date,ActualValue)%>%
        rename(PMI=ActualValue)
secom$Date <- format(as.Date(secom$Date, format="%Y.%m.%d"),"%Y-%m-%d")
secom$Date <- as.Date(secom$Date, format="%Y-%m-%d")
secom$Date <- secom$Date %m+% months(-1)
secom1 <- secom
setDT(secom)
secom <- secom[, mean(PMI), keyby = .(year(Date), quarter(Date))]
secom$Date<-as.yearqtr((paste0(secom$year,"Q",secom$quarter)))

## JAV PMI ir BVP


Sys.setlocale("LC_ALL", "english")
usman <- read.delim("https://raw.githubusercontent.com/tomasdzedulionis/Data/master/us_pmi_man.csv", header=TRUE, stringsAsFactors = FALSE, sep=",")
usman <- usman %>%filter(Date>="2008-01-01")%>% select(Date, PMI)
usman$Date <- as.Date(usman$Date, format="%Y-%m-%d")
usman1 <- usman
setDT(usman)
usman <- usman[, mean(PMI), keyby = .(year(Date), quarter(Date))]
usman$Date<-as.yearqtr((paste0(usman$year,"Q",usman$quarter)))

usser<- read.delim("https://raw.githubusercontent.com/tomasdzedulionis/Data/master/usservices.txt", header=FALSE, stringsAsFactors = FALSE)
usser<- usser%>%select(V1,V2)%>%
        rename(Date=V1, PMI=V2)%>%
        slice(-1)%>%
        filter(!grepl("[1-3][0-9]([,])", Date))
usser$Date <- format(as.Date(usser$Date, format="%b %d,%Y"),"%Y-%m-%d")
usser$Date <- as.Date(usser$Date, format="%Y-%m-%d")
usser$Date <- usser$Date %m+% months(-1)
usser1 <- usser
setDT(usser)
usser <- usser[, mean(PMI), keyby = .(year(Date), quarter(Date))]
usser$Date<-as.yearqtr((paste0(usser$year,"Q",usser$quarter)))


uscom <- read.delim("https://raw.githubusercontent.com/tomasdzedulionis/Data/master/JAVcomposite.txt", header=TRUE, stringsAsFactors = FALSE)
uscom<- uscom %>%select(Date,X)%>%
        rename(PMI=X)%>%
        filter(!grepl("[1-3][0-9]([,])", Date))
uscom$Date <- format(as.Date(uscom$Date, format="%b %d,%Y"),"%Y.%m.%d")
uscom$Date <- as.Date(uscom$Date, format="%Y.%m.%d")
uscom$Date <- uscom$Date %m+% months(-1)
uscom1 <- uscom
setDT(uscom)
uscom <- uscom[, mean(PMI), keyby = .(year(Date), quarter(Date))]
uscom$Date<-as.yearqtr((paste0(uscom$year,"Q",uscom$quarter)))

usgdp <- read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1075&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=GDPC1&scale=left&cosd=2007-04-30&coed=2019-10-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2009-06-01&line_index=1&transformation=pc1&vintage_date=2020-04-14&revision_date=2020-04-14&nd=2015-01-01", header=TRUE, stringsAsFactors = FALSE)
usgdp$DATE <- as.Date(usgdp$DATE, format="%Y-%m-%d")
usgdp$DATE <- as.yearqtr(usgdp$DATE)
usgdp<- filter(usgdp, DATE>="2008-01-01")

##JK eksportas ir PMI
Sys.setlocale("LC_ALL", "English")
ukman<- read.delim("https://raw.githubusercontent.com/tomasdzedulionis/Data/master/UKManufacturing.csv", header=TRUE,stringsAsFactors = FALSE)
ukman[,1]<- rownames(ukman)
rownames(ukman) <- 1:nrow(ukman)
ukman<- ukman%>%select(Date,X)%>%
        rename(PMI=X)%>%
        filter(!grepl("[1-3][0-9]([,])", Date))
ukman$Date <- format(as.Date(ukman$Date, format="%b %d,%Y"),"%Y.%m.%d")
ukman$Date <- as.Date(ukman$Date, format="%Y.%m.%d")
ukman$Date <- ukman$Date %m+% months(-1)
ukman1 <- ukman
setDT(ukman)
ukman <- ukman[, mean(PMI), keyby = .(year(Date), quarter(Date))]
ukman$Date<-as.yearqtr((paste0(ukman$year,"Q",ukman$quarter)))

ukser<- read.delim("https://raw.githubusercontent.com/tomasdzedulionis/Data/master/UKServices.csv", header=FALSE, stringsAsFactors = FALSE)
ukser<- ukser%>%select(V1,V2)%>%
        rename(Date=V1, PMI=V2)%>%
        slice(-1)%>%
        filter(!grepl("[1-3][0-9]([,])", Date))
ukser$Date <- format(as.Date(ukser$Date, format="%b %d,%Y"),"%Y-%m-%d")
ukser$Date <- as.Date(ukser$Date, format="%Y-%m-%d")
ukser$Date <- ukser$Date %m+% months(-1)
ukser1 <- ukser
setDT(ukser)
ukser <- ukser[, mean(PMI), keyby = .(year(Date), quarter(Date))]
ukser$Date<-as.yearqtr((paste0(ukser$year,"Q",ukser$quarter)))

ukcom<-read.delim("https://raw.githubusercontent.com/tomasdzedulionis/Data/master/UKComposite.csv", header=TRUE, stringsAsFactors = FALSE)
ukcom<- ukcom %>%select(Date,X)%>%
        rename(PMI=X)%>%
        filter(!grepl("[1-3][0-9]([,])", Date))
ukcom$Date <- format(as.Date(ukcom$Date, format="%b %d,%Y"),"%Y-%m-%d")
ukcom$Date <- as.Date(ukcom$Date, format="%Y-%m-%d")
ukcom$Date <- ukcom$Date %m+% months(-1)
ukcom1 <- ukcom
setDT(ukcom)
ukcom <- ukcom[, mean(PMI), keyby = .(year(Date), quarter(Date))]
ukcom$Date<-as.yearqtr((paste0(ukcom$year,"Q",ukcom$quarter)))


# Uzsikretimai

data_world <- read.csv("./content/data/data_world.csv", header=TRUE, stringsAsFactors = FALSE, encoding="UTF-8")
max.date<-max(data_world$date)
pop <- get_eurostat("tps00001") %>% 
        filter(time=="2019-01-01")%>%
        rename(CNTR_CODE=geo,
               pop=values)%>%
        select(CNTR_CODE, pop)
data_world <- data_world %>%
        filter(var=="confirmed",
               CNTR_CODE %in% c("BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE","UK","IS","LI","NO","CH"),
               date==max(date))%>%
        select (valstybe, value, CNTR_CODE)%>%
        left_join(., pop, by="CNTR_CODE")%>%
        mutate(value=round((value/pop*100000),1))%>%
        select(valstybe, value)%>%
        mutate_if(is.numeric, ~round(., 0))

## Grafikai

## Uzsikretimau EU
png("./static/post/2020-04-18-verslo-lukesciai-uzsienio-rinkose/uzsikretimai.png", width = 7, height = 5, units = 'in', res = 100)                       
Sys.setlocale("LC_ALL","Lithuanian")
ggplot(data_world,aes(x=reorder(valstybe,-value), y=value)) +
        geom_bar(stat='identity',
                 fill="steelblue")+
        theme_stata()+
        geom_text(aes(label=value, y=value), position=position_dodge(width=0.9), vjust=-0.25, size=3)+
        labs(x="Valstybė", 
             y="Užsikrėtimų skaičius", 
             title=paste0("Užsikrėtimų skaičius 100.000 gyventojų ( ", max.date, " duomenimis)"),
             subtitle="Šaltinis: JHCSSE \nSkaičiavimai: Corona-stat.lt" )+
        theme(legend.title=element_blank(),
              legend.position='none',
              axis.text.x=element_text(angle=45, hjust=1))+
        theme(plot.title = element_text(hjust = 0),
              plot.subtitle = element_text(hjust = 0),axis.text=element_text(size=9),
              axis.title=element_text(size=12,face="bold"),
              plot.background = element_rect(fill = "white"))+
        theme(plot.title = element_text(hjust = 0),
              plot.subtitle = element_text(hjust = 0),axis.text=element_text(size=9))
dev.off()



## nelygybe

ineq <- get_eurostat("ilc_di11b", filters=list(sex="T",
                                               age="Y_LT65"))
ineq <- ineq%>% filter(time=="2018-01-01", 
                       geo%in%c("AT", "BE", "CY", "EE", "FI", "FR", "DE", "GR", "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PT", "SK", "SI", "ES","BG", "HR", "CZ", "DK", "HU", "RO", "SE","EL","EU28"))%>%
        mutate_if(is.numeric, ~round(., 1))
png("./static/post/2020-04-18-verslo-lukesciai-uzsienio-rinkose/income.png", width = 7, height = 5, units = 'in', res = 100)                       
Sys.setlocale("LC_ALL","Lithuanian")
ggplot(ineq, aes(x=reorder(geo, -values), y=values))+
        geom_bar(stat="identity", fill="steelblue")+
        theme_stata()+
        theme(axis.text.x=element_text(angle=45,hjust=1),
              axis.text=element_text(size=9),
              axis.title=element_text(size=12,face="bold"),
              plot.background = element_rect(fill = "white"))+
        labs(x="Valstybė", y="Santykis, kartais", title="Santykis tarp 20% didžiausias ir mažiausias darbo pajamas gaunančių asmenų", 
             subtitle="Duomenų šaltinis: Eurostat \nSkaičiavimai: Corona-stat.lt")+
        geom_text(aes(label=paste0(values,"")), position=position_dodge(width=0.9), vjust=-0.25, size=3)+
        theme(plot.title = element_text(hjust = 0),
              plot.subtitle = element_text(hjust = 0),axis.text=element_text(size=9))
dev.off()

## EKSPORTAS

png("./static/post/2020-04-18-verslo-lukesciai-uzsienio-rinkose/Eksportas.png", width = 7, height = 5, units = 'in', res = 100)
Sys.setlocale("LC_ALL","Lithuanian")
ggplot(dff, aes(x=reorder(Šalys.ir.teritorijos.Pavadinimas, -Proc), y=Proc))+
        geom_bar(stat="identity", fill="steelblue")+
        theme_stata()+
        theme(axis.text.x=element_text(angle=45,hjust=1),
              axis.text=element_text(size=9),
              axis.title=element_text(size=12,face="bold"),
              plot.background = element_rect(fill = "white"))+
        labs(x="Valstybė", y="Eksporto dalis %", title="Lietuviškos kilmės prekių (be naftos prod.) rinkos, % viso eksporto", 
             subtitle="Duomenų šaltinis: Lietuvos Statistikos Departamentas \nSkaičiavimai: Corona-stat.lt")+
        ylim(0,10)+
        geom_text(aes(label=paste0(Proc,"%")), position=position_dodge(width=0.9), vjust=-0.25, size=2)+
        theme(plot.title = element_text(hjust = 0),
              plot.subtitle = element_text(hjust = 0),axis.text=element_text(size=9))
dev.off()


## Ekonomikos atvirumas

png("./static/post/2020-04-18-verslo-lukesciai-uzsienio-rinkose/atvirumas.png", width = 7, height = 5, units = 'in', res = 100)

Sys.setlocale("LC_ALL","Lithuanian")
ggplot(dfff, aes(x=reorder(geo, -trade),y=trade))+
        geom_bar(stat="identity", fill="steelblue")+
        theme_stata()+
        theme(axis.text.x=element_text(angle=45,hjust=1),
              axis.text=element_text(size=9),
              axis.title=element_text(size=12,face="bold"),
              plot.background = element_rect(fill = "white"))+
        labs(x="Valstybė", y="Ekonomikos atvirumas, %BVP", title="ES valstybių ekonomikų atvirumas 2019m.", 
             subtitle="Duomenų šaltinis: Eurostat \nSkaičiavimai: Corona-stat.lt")+
        geom_text(aes(label=paste0(trade,"")), vjust=-0.25, size=3)+
        theme(plot.title = element_text(hjust = 0),
              plot.subtitle = element_text(hjust = 0),axis.text=element_text(size=9))

dev.off()

## Eksportas į Vokietiją.


df1<- df%>%select(grupe,DE11)%>%top_n(20)   
png("./static/post/2020-04-18-verslo-lukesciai-uzsienio-rinkose/DEeksportas.png", width = 7.1, height = 5, units = 'in', res = 100)
ggplot(df1, aes(x=reorder(grupe, DE11), y=DE11))+
        coord_flip()+
        geom_bar(stat="identity", fill="steelblue")+
        theme_stata()+
        theme(axis.text.x=element_text(angle=0,hjust=1),
              axis.text.y=element_text(angle=360,hjust=1),
              axis.text=element_text(size=9),
              axis.title=element_text(size=12,face="bold"),
              plot.background = element_rect(fill = "white"))+
        labs(x="Grupė", y="Eksporto dalis %", title="Lietuviškos kilmės eksportas į Vokietiją 2019m.", 
             subtitle="Duomenų šaltinis: Lietuvos Statistikos Departamentas \nSkaičiavimai: Corona-stat.lt")+
        geom_text(aes(label=paste0(DE11,"%")), position=position_dodge(width=0.9), hjust=-0.05)+
        scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5))+
        theme(plot.title = element_text(hjust = 0),
              plot.subtitle = element_text(hjust = 0),axis.text=element_text(size=9))
dev.off()


## Vokietija PMI, BVP


png("./static/post/2020-04-18-verslo-lukesciai-uzsienio-rinkose/Vokietija.png", width = 8, height =6 , units = 'in', res = 100)

Sys.setlocale("LC_ALL", "Lithuanian")
par(mfrow=c(2,1),oma = c(0, 0, 0, 2),xpd="NA")
plot(german1$Date, german1$PMI, type="l", lwd=3, col="black", ylab="PMI", xlab="Metai",  
     main="Mėnesinis PMI Vokietijoje 2008-2020m.", ylim=c(32.5,65))
abline(h=50, untf = FALSE, lty=3, xpd=FALSE)
lines(germser1$Date, germser1$PMI, type="l", lwd=3, col="grey")
lines(germcom1$Date, germcom1$PMI,type="l", lwd=3, col="steelblue")
plot(german$Date, german$V1, type="l", lwd=3, col="black", ylab="PMI", xlab="Metai",  
     sub="Duomenų šaltiniai: IHS Markit, Eurostat", oma=c(2, 3, 5, 2), ylim=c(32.5,65))
abline(h=50, untf = FALSE, lty=3, xpd=FALSE)
lines(germser$Date, germser$V1, type="l", lwd=3, col="grey")
lines(germcom$Date, germcom$V1,type="l", lwd=3, col="steelblue")
par(new=TRUE)
plot( gdp$time, gdp$DE, type="l", lwd=3, col="red",axes=F,yaxt="n", xaxt="n", xlab=NA, ylab=NA,
      main="BVP ir PMI (ketvirtinių duomenų) kitimas Vokietijoje 2008-2020m.", ylim=c(-7,6),xlim=c(2008,2020))
axis(side = 4)
mtext(side = 4, line = 3, 'BVP pokytis, %')
legend(2007.5,18 ,c("Gamybinis PMI","Paslaugų PMI", "Bendras PMI", "BVP pokytis"),lty=c(1,1),bty = "n",lwd=3,col=c("black","grey","steelblue","red"), cex = 1, horiz = TRUE)

dev.off()

## Švedija eksportas



Sys.setlocale("LC_ALL","Lithuanian")
df1<- df%>%select(grupe,SE11)%>%top_n(20)
png("./static/post/2020-04-18-verslo-lukesciai-uzsienio-rinkose/SEeksportas.png", width = 7, height = 5, units = 'in', res = 100)
ggplot(df1, aes(x=reorder(grupe, SE11), y=SE11))+
        coord_flip()+
        geom_bar(stat="identity", fill="steelblue")+
        theme_stata()+
        theme(axis.text.x=element_text(angle=0,hjust=1),
              axis.text.y=element_text(angle=360,hjust=1),
              axis.text=element_text(size=9),
              axis.title=element_text(size=12,face="bold"),
              plot.background = element_rect(fill = "white"))+
        labs(x="Grupė", y="Eksporto dalis %", title="Lietuviškos kilmės eksportas į Švediją 2019m.", 
             subtitle="Duomenų šaltinis: Lietuvos Statistikos Departamentas \nSkaičiavimai: Corona-stat.lt")+
        geom_text(aes(label=paste0(SE11,"%")), position=position_dodge(width=0.9), hjust=-0.05)+
        scale_y_continuous(limits = c(0, 35), breaks = seq(0, 35, by = 5))+
        theme(plot.title = element_text(hjust = 0),
              plot.subtitle = element_text(hjust = 0),axis.text=element_text(size=9))
dev.off()

## SVEDIJA


png("./static/post/2020-04-18-verslo-lukesciai-uzsienio-rinkose/Svedija.png", width = 8, height = 6, units = 'in', res = 100)
par(mfrow=c(2,1),oma = c(0, 0, 0, 2),xpd="NA")
plot(seman1$Date, seman1$PMI, type="l", lwd=3, col="black", ylab="PMI", xlab="Metai",  
     main="Mėnesinis PMI Švedijoje 2008-2020m.")
abline(h=50, untf = FALSE, lty=3, xpd=FALSE)
lines(seser1$Date, seser1$PMI, type="l", lwd=3, col="steelblue")
lines(secom1$Date, secom1$PMI, type="l", lwd=3, col="grey")
plot(seman$Date, seman$V1, type="l", lwd=3, col="black", ylab="PMI", xlab="Metai",  
     sub="Duomenų šaltiniai: IHS Markit, Eurostat", oma=c(2, 3, 5, 2), ylim=c(30,76.65), xlim=c(2008,2020))
lines(seser$Date, seser$V1, type="l", lwd=3, col="steelblue")
lines(secom$Date, secom$V1, type="l", lwd=3, col="grey")
abline(h=50, untf = FALSE, lty=3, xpd=FALSE)
par(new=TRUE)
plot( gdp$time, gdp$SE, type="l", lwd=3, col="red",axes=F,yaxt="n", xaxt="n", xlab=NA, ylab=NA,
      main="BVP ir PMI (ketvirtinių duomenų) kitimas Švedijoje 2008-2020m.", ylim=c(-6,8), xlim=c(2008,2020))
axis(side = 4)
mtext(side = 4, line = 3, 'BVP pokytis, %')
legend(2008,20.5 ,c("Gamybinis PMI","Paslaugų PMI","Bendras PMI", "BVP pokytis"),lty=c(1,1),bty = "n",lwd=3,col=c("black","steelblue","grey","red"), cex = 1, horiz = TRUE)
dev.off()


## JAV EKsportas


Sys.setlocale("LC_ALL","Lithuanian")
df1<- df%>%select(grupe,JAV11)%>%top_n(20)
png("./static/post/2020-04-18-verslo-lukesciai-uzsienio-rinkose/JAVeksportas.png", width = 7.1, height = 5, units = 'in', res = 100)
ggplot(df1, aes(x=reorder(grupe, JAV11), y=JAV11))+
        coord_flip()+
        geom_bar(stat="identity", fill="steelblue")+
        theme_stata()+
        theme(axis.text.x=element_text(angle=0,hjust=1),
              axis.text.y=element_text(angle=360,hjust=1),
              axis.text=element_text(size=9),
              axis.title=element_text(size=12,face="bold"),
              plot.background = element_rect(fill = "white"))+
        theme(plot.title = element_text(hjust = 0.5))+
        labs(x="Grupė", y="Eksporto dalis %", title="Lietuviškos kilmės eksportas į JAV 2019m.", 
             subtitle="Duomenų šaltinis: Lietuvos Statistikos Departamentas \nSkaičiavimai: Corona-stat.lt")+
        geom_text(aes(label=paste0(JAV11,"%")), position=position_dodge(width=0.9), hjust=-0.05)+
        scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5))+
        theme(plot.title = element_text(hjust = 0),
              plot.subtitle = element_text(hjust = 0),axis.text=element_text(size=9))
dev.off()




## JAV

png("./static/post/2020-04-18-verslo-lukesciai-uzsienio-rinkose/JAV.png", width = 8, height =6 , units = 'in', res = 100)
Sys.setlocale("LC_ALL", "Lithuanian")
par(mfrow=c(2,1),oma = c(0, 0, 0, 2),xpd="NA")
plot(usman1$Date, usman1$PMI, type="l", lwd=3, col="black", ylab="PMI", xlab="Metai",  
     main="Mėnesinis PMI JAV 2008-2020m.")
abline(h=50, untf = FALSE, lty=3, xpd=FALSE)
lines(usser1$Date, usser1$PMI, type="l", lwd=3, col="steelblue")
lines(uscom1$Date, uscom1$PMI, type="l", lwd=3, col="grey")
plot(usman$Date, usman$V1, type="l", lwd=3, col="black", ylab="PMI", xlab="Metai",  
     sub="Duomenų šaltiniai: IHS Markit, Eurostat", oma=c(2, 3, 5, 2), ylim=c(30,66.6))
abline(h=50, untf = FALSE, lty=3,xpd=FALSE)
lines(usser$Date, usser$V1, type="l", lwd=3, col="grey")
lines(uscom$Date, uscom$V1,type="l", lwd=3, col="steelblue")
par(new=TRUE)
plot( usgdp$DATE, usgdp$GDPC1_PC1, type="l", lwd=3, col="red",axes=F,yaxt="n", xaxt="n", xlab=NA, ylab=NA,
      main="BVP ir PMI (ketvirtinių duomenų) kitimas JAV 2008-2020m.", ylim=c(-6,5),xlim=c(2008,2020))
axis(side = 4)
mtext(side = 4, line = 3, 'BVP pokytis, %')
legend(2007.25,15 ,c("Gamybinis PMI","Paslaugų PMI", "Bendras PMI", "BVP pokytis"),lty=c(1,1),bty = "n",lwd=3,col=c("black","steelblue", "grey", "red"), cex = 1, horiz = TRUE)
dev.off()



## JK EKSPORTAS


Sys.setlocale("LC_ALL","Lithuanian")
df1<- df%>%select(grupe,JK11)%>%top_n(20)
png("./static/post/2020-04-18-verslo-lukesciai-uzsienio-rinkose/UKeksportas.png", width = 7, height = 5, units = 'in', res = 100)
ggplot(df1, aes(x=reorder(grupe, JK11), y=JK11))+
        coord_flip()+
        geom_bar(stat="identity", fill="steelblue")+
        theme_stata()+
        theme(axis.text.x=element_text(angle=0,hjust=1),
              axis.text.y=element_text(angle=360,hjust=1),
              axis.text=element_text(size=9),
              axis.title=element_text(size=12,face="bold"),
              plot.background = element_rect(fill = "white"))+
        labs(x="Grupė", y="Eksporto dalis %", title="Lietuviškos kilmės eksportas į JK 2019m.", 
             subtitle="Duomenų šaltinis: Lietuvos Statistikos Departamentas \nSkaičiavimai: Corona-stat.lt")+
        geom_text(aes(label=paste0(JK11,"%")), position=position_dodge(width=0.9), hjust=-0.05)+
        scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5))+
        theme(plot.title = element_text(hjust = 0),
              plot.subtitle = element_text(hjust = 0),axis.text=element_text(size=9))
dev.off()

## JK



png("./static/post/2020-04-18-verslo-lukesciai-uzsienio-rinkose/UK.png", width = 8, height =6 , units = 'in', res = 100)
Sys.setlocale("LC_ALL", "Lithuanian")
par(mfrow=c(2,1),oma = c(0, 0, 0, 2),xpd="NA")
plot(ukman1$Date, ukman1$PMI, type="l", lwd=3, col="black", ylab="PMI", xlab="Metai",  
     main="Mėnesinis PMI Jungtinėje Karalystėje 2008-2020m.")
abline(h=50, untf = FALSE, lty=3, xpd=FALSE)
lines(ukser1$Date, ukser1$PMI, type="l", lwd=3, col="steelblue")
lines(ukcom1$Date, ukcom1$PMI, type="l", lwd=3, col="grey")
plot(ukman$Date, ukman$V1, type="l", lwd=3, col="black", ylab="PMI", xlab="Metai",  
     sub="Duomenų šaltiniai: IHS Markit, Eurostat", oma=c(2, 3, 5, 2))
abline(h=50, untf = FALSE, lty=3, xpd=FALSE)
lines(ukser$Date, ukser$V1, type="l", lwd=3, col="grey")
lines(ukcom$Date, ukcom$V1,type="l", lwd=3, col="steelblue")
par(new=TRUE)
plot( gdp$time, gdp$UK, type="l", lwd=3, col="red",axes=F,yaxt="n", xaxt="n", xlab=NA, ylab=NA,
      main="BVP ir PMI (ketvirtinių duomenų) kitimas Jungtinėje Karalystėje 2008-2020m.", ylim=c(-6,4.25), xlim=c(2008,2020))
axis(side = 4)
mtext(side = 4, line = 3, 'BVP pokytis, %')
legend(2007.25,13.5 ,c("Gamybinis PMI","Paslaugų PMI", "Bendras PMI", "BVP pokytis"),lty=c(1,1),bty = "n",lwd=3,col=c("black","steelblue", "grey", "red"), cex = 1, horiz = TRUE)
dev.off()
