table(nuovo2$Cittadinanza, nuovo2$`Voto diploma`)
boxplot(split(nuovo2$`Voto diploma`, nuovo2$Cittadinanza))

barplot( nuovo2$`Voto diploma`, names.arg = nuovo2$Cittadinanza,
        col=c(1:2),  cex.axis=0.8, cex.names=0.8)
barplot(table( nuovo2$Cittadinanza, nuovo2$`Voto diploma`))


counts <- table(nuovo2$Cittadinanza, nuovo2$`Voto diploma`)
barplot(counts, main="Voto di maturità vs Nazionalità",
        xlab="Voto di maturità", col=c("lightblue","darkblue"),
        legend = rownames(counts), beside=TRUE)

table(nuovo2$Sesso, nuovo2$`Isc. Università`)
pie(table(nuovo2$Sesso, nuovo2$`Isc. Università`))

counts <- table(nuovo2$`Regione di residenza`)
barplot(counts, width= 1,  main="Voto di maturità vs Nazionalità", horiz = TRUE,
        xlab="Voto di maturità", col=c("lightblue"),
        legend = rownames(nuovo2$`Regione di residenza`), beside=TRUE,  cex.names = 0.45, las = 2)

library(cartography)

suppressPackageStartupMessages(library(dplyr))



d1 <- nuovo2 %>% group_by(Votodip) %>% 
        summarize(tot = sum(Cittadinanza== "Italiana"))
d1


d2 <- nuovo2 %>% group_by(Votodip) %>% 
        summarize(tot1 = sum(Cittadinanza== "Straniera o apolide"))
d2


library(ggpl)

d1d2

barplot( d1d2voti, names.arg =d1d2$votodip,
         col=c(1:2),  cex.axis=0.8, cex.names=0.8)
barplot(table(d1d2voti,  d1d2$studentiita,))

library(dplyr)

ggplot(nuovo2, aes(votodip)) + 
        geom_col(data= nuovo2, aes(y = cittadinanza), color = "lightblue", position="dodge")

d1d2 %>% 
        ggplot(aes(votodip)) +
        geom_bar(aes = studentiita)
        + geom_bar(aes(y=studentistr),
                 position="dodge2"))

ggplot(d1d2, aes(factor(voto), fill = factor(nazionalità))) +
        geom_bar(position = "dodge2")

mediaita = w.mean(d1bis$voto, d1bis$perce)
varita = w.var(d1bis$voto, d1bis$perce)
sdita = w.sd(d1bis$voto, d1bis$perce)
quantiliita = wtd.quantile(d1$votodip, weights = d1$studentiita , probs = c(0,0.25,0.5,0.75,1))
median(nuovo2$votodip)

library(dplyr)

#calcolare la moda
calculate_mode <- function(votodip) {
  uniqx <- unique(nuovo2$votodip)
  uniqx[which.max(tabulate(match(votodip, uniqx)))]
}

nuovo2 %>% 
  group_by(cittadinanza) %>% 
  summarise(votodip = calculate_mode(votodip))

mediastr = w.mean(d2bis$voto, d2bis$perce)
varstr = w.var(d2bis$voto, d2bis$perce)
sdstr = w.sd(d2bis$voto, d2bis$perce)
quantilistr = wtd.quantile(d2$votodip, weights = d2$studentistr , probs = c(0,0.25,0.5,0.75,1))

#calcolare la moda
calculate_mode <- function(votodip) {
  uniqx <- unique(nuovo2$votodip)
  uniqx[which.max(tabulate(match(votodip, uniqx)))]
}



ggplot(nuovo2, aes(x= "regionedi" , fill= cittadinanza))+
  geom_bar(width = 1)+
  coord_polar("y")


library(purrr)
nuovo2 %>% split(nuovo2$cittadinanza) %>% map(summary)

ggplot(dtot, aes(voto, perce)) +   
  geom_bar(aes(fill = nazionalità), position = "dodge", stat="identity")+ 
  scale_fill_manual("legend", values = c("straniera o apolide" = "lightskyblue", "italiana" = "azure4")) + ggtitle("Distribuzione frequenze relative voti studenti italiani e stranieri") +
  xlab("Voto di maturità") + ylab("Frequenze relative") +
  theme(
    plot.title = element_text(color="black", size=14, family="Inter"),
    axis.title.x = element_text(color="black", size=14, family="Inter Medium"),
    axis.title.y = element_text(color="black", size=14, family="Inter Medium")
  )

ggplot(d1bis, aes(voto, perce)) +   
  geom_bar( position = "dodge", stat="identity", fill = "azure4") + ggtitle("Distribuzione frequenze relative voti studenti italiani") +
  xlab("Voto di maturità") + ylab("Frequenze relative") +
theme(
  plot.title = element_text(color="black", size=14, family="Inter"),
  axis.title.x = element_text(color="black", size=14, family="Inter Medium"),
  axis.title.y = element_text(color="black", size=14, family="Inter Medium")
) + geom_vline(xintercept=mediapondeita, color="red")

ggplot(d2bis, aes(voto, perce)) +   
  geom_bar( position = "dodge", stat="identity", fill = "lightskyblue") + ggtitle("Distribuzione frequenze relative voti studenti stranieri") +
  xlab("Voto di maturità") + ylab("Frequenze relative") +
  theme(
    plot.title = element_text(color="black", size=14, family="Inter"),
    axis.title.x = element_text(color="black", size=14, family="Inter Medium"),
    axis.title.y = element_text(color="black", size=14, family="Inter Medium")
  ) + geom_vline(xintercept=mediapondestr, color="red")

boxplot(nuovo2$votodip ~ nuovo2$cittadinanza, 
        main="Voti di maturità studenti italiani vs Voti di maturità studenti stranieri", 
        xlab= "Voto di maturità",
        ylab = "Cittadinanza", 
        col= rainbow(3),
        horizontal = TRUE )

tavola = table(nuovo2$votodip, nuovo2$cittadinanza)
tavola

tavola 

chisq.test(tavola, correct = FALSE)    # trova chi square
chi=chisq.test(tavola) # create an object
chi$statistic        # invocalo dall'oggetto
chi$residuals # contingenze  nij-nij di indipendenza
chi_norm<-chi$statistic/(10880*min(42-1,2-1))
percentualeita= totita/10880

asimmetriaita = skewness(d1$studentiita)
asimmetriastr = skewness(d2$studentistr)

simple.fit = lm(nuovo2$redditomensilenetto ~ nuovo2$votodip, data=nuovo2)
summary(simple.fit) #regressione lineare

simple.fit = lm(nuovo2$redditomensilenetto ~  nuovo2$votodip + nuovo2$dimestinternet, data=nuovo2)
summary(simple.fit) #regressione multipla

anova = aov(nuovo2$votodip~nuovo2$cittadinanza, data=nuovo2) #anova 
summary(anova)

#frequenze assolute italiani per regione
itaregione <- Italiani %>% group_by(regioni) %>% 
  summarize(tot = sum(cittadinanza== "Italiana"))

itaregione

#totali italiani
totitaregione = sum(itaregionenome$tot)


#trasformazione in num la colonna residenza
italia <- as.numeric(itaregione$regioneresidenza)

#creazione nuovo dataframe con regioni numeriche e tot
italia1= data.frame(regioni= c(italia), tot = c(itaregione$tot))
italia1

#riordine crescente colonna regioni
italia2 <- italia1[order(italia1$regioni),] 

#assegnazione ad un altro dataframe
itaregionenome <-italia2

#assegnazione regioni 

#grafico
ggplot(tabellaregioniita, aes(regioni, tot)) +   
  geom_bar( position = "dodge", stat="identity", fill = "cadetblue3") +  ggtitle("Distribuzione studenti italiani nelle regioni o all'estero") +
  xlab("Regioni italiane o Estero") + ylab("Frequenze relative") +
  theme(
    plot.title = element_text(color="black", size=14, family="Inter"),
    axis.title.x = element_text(color="black", size=14, family="Inter Medium"),
    axis.title.y = element_text(color="black", size=14, family="Inter Medium"),
    axis.text.y =element_text(size=rel(1.3)),
    axis.text.x=element_text(size=rel(1.3), angle=45))

#frequenze relatrive italiani nelle regioni 
tabellaregioniita <- data.frame(regioni= c(itaregionenome$regioni), tot = c(itaregionenome$tot/totitaregione))

ggplot(unioneregioni, aes(regioni, tot)) +   
  geom_bar(aes(fill = nazionalità), position = "dodge", stat="identity")+ 
  scale_fill_manual("Legend", values = c("Straniera o apolide" = "blue", "Italiana" = "azure4")) + ggtitle("Distribuzione frequenze relative degli studenti nelle regioni italiane o all'estero") +
  xlab("Regioni") + ylab("Frequenze relative") +
  theme(
    plot.title = element_text(color="black", size=14, family="Inter"),
    axis.title.x = element_text(color="black", size=14, family="Inter Medium"),
    axis.title.y = element_text(color="black", size=14, family="Inter Medium"),
    legend.title = element_text(color = "black", size = 16),
    legend.text = element_text(color = "black", size = 16),
    axis.text.y =element_text(size=rel(1.3)),
    axis.text.x=element_text(size=rel(1.3), angle=45)
  )

  a <- subset (nuovo2, Cittadinanza == "Italiana")
b <- subset (nuovo2, Cittadinanza == "Straniera o apolide")

hist(a$`Reddito mensile netto`, main="Distribuzione frequenze assolute \n italiani",
     col= "Azure4",
     xlab = "Reddito netto",
     ylab = "frequenze assolute")

table(b$`Reddito mensile netto`)
hist(b$`Reddito mensile netto`, main="Distribuzione frequenze assolute \n stranieri o apolidi",
     col= "lightskyblue",
     xlab = "Reddito netto",
     ylab = "frequenze assolute")


means <- aggregate (nuovo2$`Reddito mensile netto`, by=list(nuovo2$Cittadinanza), mean)
variances <- aggregate (nuovo2$`Reddito mensile netto`, by=list(nuovo2$Cittadinanza), var)
sdeviations <- aggregate (nuovo2$`Reddito mensile netto`, by=list(nuovo2$Cittadinanza), sd)
summaries <- aggregate (nuovo2$`Reddito mensile netto`, by=list(nuovo2$Cittadinanza), summary)
IQRs <- aggregate (nuovo2$`Reddito mensile netto`, by=list(nuovo2$Cittadinanza), IQR)
means
variances
sdeviations
summaries
IQRs


Aggregate (nuovo2$`Reddito mensile netto`, by=list(nuovo2$Cittadinanza), skew)

sigma2_2 <- function(x)
{
  mean((x-mean(x)) ^2)
}

cv <- function(x)
{
  sqrt(sigma2_2(x))/abs(mean(x))
}
Cv (a$`Reddito mensile netto`)
Cv (b$`Reddito mensile netto`)


boxplot(df$`Reddito mensile netto` ~ df$Cittadinanza, 
        main="Boxplot reddito netto e cittadinanza",
        col= rainbow(2),
        xlab = "Cittadinanza",
        ylab = "Reddito netto")

df$nuova_col[which(df$`Reddito mensile netto`>= 0  & df$`Reddito mensile netto`< 550)] <-"fascia1: 0-549"
df$nuova_col[which(df$`Reddito mensile netto`>= 550  & df$`Reddito mensile netto`< 1100)] <-"fascia2: 550-1099"
df$nuova_col[which(df$`Reddito mensile netto`>= 1100  & df$`Reddito mensile netto`< 1650)] <-"fascia3: 1100-1649"
df$nuova_col[which(df$`Reddito mensile netto`>= 1650  & df$`Reddito mensile netto`<= 2200)] <-"fascia4: 1650-2200"

d1 <- df %>% group_by(df$nuova_col) %>% 
  summarize(tot = sum(Cittadinanza== "Italiana"))
d2 <- df %>% group_by(df$nuova_col) %>% 
  summarize(tot1 = sum(Cittadinanza== "Straniera o apolide"))
totita = sum(d1$tot)
totstr = sum(d2$tot1)
d1bis= data.frame(voto= c(d1$`df$nuova_col`), perce = c(d1$tot/totita), nazionalità = c("italiana"))
d2bis= data.frame(voto= c(d2$`df$nuova_col`), perce = c(d2$tot1/totstr), nazionalità = c("Straniera o apolide"))
dtot = rbind(d1bis, d2bis)

ggplot(dtot, aes(voto, perce)) +   
  geom_bar(aes(fill = nazionalità), position = "dodge", stat="identity")+ 
  scale_fill_manual("legend", values = c("Straniera o apolide" = "lightskyblue", "italiana" = "azure4")) + ggtitle("Distribuz. Frequenze Relative redditi") +
  xlab("Fasce di reddito") + ylab("Frequenze relative")
+
  Theme (
    plot.title = element_text(color="black", size=14, family="Inter"),
    axis.title.x = element_text(color="black", size=14, family="Inter Medium"),
    axis.title.y = element_text(color="black", size=14, family="Inter Medium")
  )


table <- table(df$nuova_col, df$Cittadinanza)
table

chisq.test(table)   
chi=chisq.test(table)
chi$statistic     
chi_norm<-chi$statistic/(nrow(df)*min(nrow(table)-1, ncol(table)-1))
chi_norm  
chi$residuals 
chi$stdres   

margin.table(table, 1) 
margin.table(table, 2) 
prop.table(table)      
prop.table(table, 1) 
prop.table(table, 2)  


#pie ITA
mytable <- table(a$`Isc. Università`)/length(a$`Isc. Università`)
lbls <- paste(names(mytable), " ", round(mytable*100), sep="")
lbls2 <- paste(lbls,"%",sep="") # ad % to labels
pie(mytable, labels = lbls2,
    main="Tasso iscrizione universitaria \n studenti italiani") 

#pie STR
mytable_b <- table(b$`Isc. Università`)/length(b$`Isc. Università`)
lbls_b <- paste(names(mytable_b), " ", round(mytable_b*100), sep="")
lbls2_b <- paste(lbls_b,"%",sep="") # ad % to labels
pie(mytable_b, labels = lbls2_b,
    main="Tasso iscrizione universitaria \n studenti stranieri o apolidi")


#tolgo prima gli iscritti all'uni perchè non avendo reddito per scelta, falsano il risultato
no <- subset(nuovo2, `Isc. Università` == 2 )
cor(df$`Reddito mensile netto`, df$`Voto diploma`) #correlazione tutti
cor(no$`Reddito mensile netto`, no$`Voto diploma`) #correlaz solo con non iscritti



#tabella scuola pubblica/privata iscritti e no a uni
    attach(nuovo2)
    tab=table(scuola_pubblica,`Isc. Università`)
# matrice risultati
    pt=prop.table(as.matrix(tab),1)

#grafico 
    barplot(tab[,1])
#grafico
    barplot(tab[,2])

# Frequenza % studenti scuola pubblica iscritti in uni:
# Frequenza % studenti scuola privata iscritti in uni:
    pt=prop.table(as.matrix(tab),1)

#GRAFICO
    barplot(pt[,1],ylim=c(0,1),main = "iscritti in università  ")
#GRAFICO 
    barplot(pt[,2],ylim=c(0,1),main = " non iscritti in università ")

#GRAFICO 

    barplot(pt, col=c(4:7),beside=T,main="UNIONE ",   legend.text=c("iscritto in università da privata","iscritto in università da pubblica ","non iscritto in università da privata ","non iscritto in università da pubblica"),
        names.arg=c("Privata", "Pubblica", "Privata", "Pubblica"),
        ylim=c(0,1)
#Tabella contingenze delle 2 variabili qualitative:
   CrossTable(nuovo2$scuola_pubblica , nuovo2$`Isc. Università`, prop.chisq=FALSE )

#Indice Chi-Quadro:
  CrossTable(nuovo2$scuola_pubblica,nuovo2$`Isc. Università`, prop.chisq=FALSE, chisq=TRUE)

