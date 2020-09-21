#### Script zur Hausarbeit von Nikos Mertens:
####      Der zweidimensionale Politikraum in Bayern und Thüringen 
####                         -
####               Ein Vergleich zweier Landtagswahlen
#### im Seminar "Politikräume, Parteienwettbewerb und Wahlverhalten"
#### Modul 2: „Politische Institutionen und Prozesse“
#### Besucht im WS 19/20



# Librarys installieren
#install.packages("tidyverse")
#install.packages("haven")
#install.packages("psych")
#install.packages("GPArotation")
#install.packages("nFactors")
#install.packages("reshape2")
#install.packages("ggthemes")
#install.packages("corrplot")
#install.packages("RColorBrewer")
#install.packages("Cairo")
#install.packages("cairoDevice")
#install.packages("knitr")
#install.packages("scales")
#install.packages("stargazer")
#install.packages("nnet")
#install.packages("ggeffects")
#install.packages("effects")
#install.packages("see", dependencies = TRUE)
#install.packages("DescTools")
#install.packages("MASS")

# Environment leeren
remove(list = ls())

# Librarys laden
library(haven)
library(psych)
library(GPArotation)
library(nFactors)
library(reshape2)
library(ggthemes)
library(corrplot)
library(RColorBrewer)
library(Cairo)
library(cairoDevice)
library(knitr)
library(scales)
library(stargazer)
library(nnet)
library(ggeffects)
library(effects)
library(see)
library(tidyverse)
library(margins)
library(DescTools)
library(MASS)

# Funktionen erstellen
mutate_na <- function(x){
  x %>%
    mutate_if(is.numeric, na_if, -71) %>%
    mutate_if(is.numeric, na_if, -72) %>%
    mutate_if(is.numeric, na_if, -81) %>%
    mutate_if(is.numeric, na_if, -82) %>%
    mutate_if(is.numeric, na_if, -83) %>%
    mutate_if(is.numeric, na_if, -84) %>%
    mutate_if(is.numeric, na_if, -85) %>%
    mutate_if(is.numeric, na_if, -86) %>%
    mutate_if(is.numeric, na_if, -92) %>%
    mutate_if(is.numeric, na_if, -93) %>%
    mutate_if(is.numeric, na_if, -94) %>%
    mutate_if(is.numeric, na_if, -95) %>%
    mutate_if(is.numeric, na_if, -96) %>%
    mutate_if(is.numeric, na_if, -97) %>%
    mutate_if(is.numeric, na_if, -98) %>%
    mutate_if(is.numeric, na_if, -99)
}
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
makeFootnote <- function(footnoteText,size= .7, color= grey(.5)){

  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(1,"npc") - unit(2, "mm"),
            y= unit(2, "mm"),
            just=c("right", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}

# Working Directory setzen
setwd("C:/Users/niksm/Documents/Uni/Mainz/Master/SS 2020/Hausarbeit Politikräume/R Analyse")

# Daten einladen
df_th_raw <- read_dta("GLES Thüringen 2014/ZA5740_v2-0-0.dta")
df_by_raw <- read_dta("GLES Bayern 2013/ZA5736_v2-0-0.dta")

# Merge
df <- full_join(df_th_raw,df_by_raw) 
df <- df %>%
  mutate_na() %>%
  drop_na(l331a,l331c,l331d,l331e,l171,l156,l331h)


# Rekode für PCA
df <- df %>%
  mutate(l331a=case_when(
    l331a==1 ~ 0,
    l331a==2 ~ 2.5,
    l331a==3 ~ 5,
    l331a==4 ~ 7.5,
    l331a==5 ~ 10,
    TRUE ~ NA_real_ )) %>%
  mutate(l331c=case_when(
    l331c==1 ~ 0,
    l331c==2 ~ 2.5,
    l331c==3 ~ 5,
    l331c==4 ~ 7.5,
    l331c==5 ~ 10,
    TRUE ~ NA_real_ )) %>%
  mutate(l331d=case_when(
    l331d==1 ~ 10,
    l331d==2 ~ 7.5,
    l331d==3 ~ 5,
    l331d==4 ~ 2.5,
    l331d==5 ~ 0,
    TRUE ~ NA_real_ )) %>%
  mutate(l331e=case_when(
    l331e==1 ~ 10,
    l331e==2 ~ 7.5,
    l331e==3 ~ 5,
    l331e==4 ~ 2.5,
    l331e==5 ~ 0,
    TRUE ~ NA_real_ )) %>%
  mutate(l171=as.numeric(l171)-1) %>%
  mutate(l156=case_when(
    l156==1 ~ 10,
    l156==2 ~ 9,
    l156==3 ~ 8,
    l156==4 ~ 7,
    l156==5 ~ 6,
    l156==6 ~ 5,
    l156==7 ~ 4,
    l156==8 ~ 3,
    l156==9 ~ 2,
    l156==10 ~ 1,
    l156==11 ~ 0,
    TRUE ~ NA_real_ )) %>%
  mutate(l331h=case_when(
    l331h==1 ~ 10,
    l331h==2 ~ 7.5,
    l331h==3 ~ 5,
    l331h==4 ~ 2.5,
    l331h==5 ~ 0,
    TRUE ~ NA_real_ ))  %>%
  mutate(Bundesland=case_when(
    study==5736 ~ 0,
    study==5740 ~ 1,
    TRUE ~ NA_real_)) %>%
  mutate(ID=row_number())

df$Bundesland <- factor(
  df$Bundesland,
  levels = c(0,1),
  labels = c("Bayern","Thüringen"),
  ordered = FALSE)

df_pca <- df %>%
dplyr:: select(l331a,l331c,l331d,l331e,l171,l156,l331h,Bundesland,ID)

# Abbildung 1 Korrelationsmatrix
df_temp <- df_pca %>%
  dplyr::select(-Bundesland,-ID)
test <- cor(df_temp,use="pairwise.complete.obs", method = "pearson")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
test_mat <- cor.mtest(df_temp)
svg("Korrelationsmatrix.svg")
corrplot(round(test, digits = 1), method="color", col=col(200), 
         type="lower", order="original",cl.length=7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=30, #Text label color and rotation
         title="Korrelationsmatrix für beide Bundesländer",
         # Combine with significance
         p.mat = test_mat, sig.level = 0.05, insig = "blank",
         mar=c(0,0,1,0),cl.pos = "b",cl.cex = 1,tl.cex = 0.9,cl.ratio = 0.15,rect.lwd = 5,  cl.offset = 0.7,
         # hide correlation coefficient on the principal diagonal
         diag=FALSE )
makeFootnote("Farblich hinterlegt sind Koeffizienten, die ein Signifikanzniveau von 0.05 erfüllen.")
dev.off()

# Tabelle 2 KMO-Tests ohne Ehe für Alle
df_temp <- df_pca %>%
  dplyr::select(-Bundesland,-l331h,-ID)
kmo <- psych::KMO(df_temp)
MSA <- as.data.frame(kmo$MSAi)
MSA <- rename(MSA,MSA="kmo$MSAi")
MSA <- arrange(MSA,MSA)
oMSA <- round(kmo$MSA,digits = 2)
names <- row.names(MSA)
MSA <- rbind(MSA,oMSA)
MSA <- MSA %>%
  mutate(Beurteilung=case_when(
    MSA < 0.5 ~ "Nicht akzeptabel",
    MSA >=0.5 & MSA < 0.6 ~ "Schlecht",
    MSA >=0.6 & MSA < 0.7 ~ "Mäßig",
    MSA >=0.7 & MSA < 0.8 ~ "Gut",
    MSA >=0.8 & MSA < 0.9 ~ "Sehr Gut",
    MSA >=0.9  ~ "Perfekt",
    TRUE ~ NA_character_))
names <- append(names,"MSA insgesamt")
row.names(MSA) <- names
print(MSA,digits = 2)

# Abbildung 2 Scree-Plot ohne die gleichgeschlechtliche Ehe
df_temp <- df_pca %>%
  dplyr::select(-Bundesland,-l331h,-ID)
test <-fa.parallel(df_temp,fa="pc",main="Für beide Bundesländer ohne Ehe für Alle",sim=FALSE,
                   ylabel="Eigenvalue der Komponenten",show.legend = TRUE)
test2 <- as.data.frame(test$pc.values)
test2$number <- c(1,2,3,4,5,6)
png("Scree-Plot ohne gleichgeschlechtlicher Ehe.png", units = "cm", height = 8.64,width = 14, res=200)
ggplot(test2,aes(y=`test$pc.values`,x=number,label=round(`test$pc.values`,digits = 2)))+
  geom_line()+
  ylab("Eigenvalue")+
  xlab("Anzahl der Komponenten")+
  scale_x_continuous(n.breaks = 6,limits = c(1,6))+
  ylim(0,2.5)+
  geom_point()+
  geom_text(aes(label=round(`test$pc.values`,digits = 2)),hjust=0, vjust=-1)+
  theme_economist()
dev.off()

# Promax-rotierte PCA ohne gleichgeschlechtliche Ehe
df_temp <- df_pca %>%
  dplyr::select(-Bundesland,-l331h,-ID)
all <- principal(df_temp,nfactors = 2, rotate = "Promax")
summary(all)
# Tabelle 3
print(loadings(all),digits = 2)

# Tabelle 4 Mittelwertsvergleich PCA ohne gleichgeschlechtliche Ehe
df_pca %>%
  dplyr:: select(-l331h,-ID) %>%
  cbind(all$scores) %>%
  dplyr:: select(Bundesland,RC1,RC2) %>%
  group_by(Bundesland) %>%
  summarize(mean_1=round(mean(RC1,na.rm=TRUE),digits = 2),mean_2=round(mean(RC2,na.rm=TRUE),digits=2)) %>%
  print()

# Abbildung 3 Scatter-Plot ohne gleichgeschlechtliche Ehe
df_temp <- cbind(df_pca,all$scores)
png("Scatterplot ohne gleichgeschlechtlicher Ehe.png", units = "cm", height = 8.64,width = 14, res=200)
ggplot(df_temp,aes(x=RC2,y=RC1,color=Bundesland))+
  geom_point()+
  xlim(-4,4)+
  ylim(-4,4)+
  geom_hline(yintercept=0, linetype="dashed", color = "blue")+
  geom_vline(xintercept=0, linetype="dashed", color = "blue")+
  labs(title = "Scatterplot, aufgeschlüsselt über das Bundesland",subtitle="Promax Rotation",
       caption="Ohne Einstellung zur gleichgeschlechtlichen Ehe")+
  xlab("Wirtschaftliche Dimension")+
  ylab("Kulturelle Dimension")+
  annotate(geom="text", x=-3, y=3.5, label="Linksautoritär",
           color="black")+
  annotate(geom="text", x=-3, y=-3.5, label="Linksliberal",
           color="black")+
  annotate(geom="text", x=3, y=3.5, label="Rechtsautoritär",
           color="black")+
  annotate(geom="text", x=3, y=-3.5, label="Rechtsliberal",
           color="black")+
  theme_classic()
dev.off()

# Abbildung 4 Verteilung der Einstellungen je Bundesland - Promax-PCA ohne gleichgeschlechtliche Ehe
df_pca_analytics <- df_pca %>%
  dplyr:: select(-l331h) %>%
  cbind(all$scores) %>%
mutate(Linksautoritär=case_when(
  percent_rank(RC1)>0.55 &  percent_rank(RC2)<0.45 ~ 1,
  TRUE ~ 0)) %>%
  mutate(Rechtsautoritär=case_when(
    percent_rank(RC1)>0.55 &  percent_rank(RC2)>0.55 ~ 1,
    TRUE ~ 0  )) %>%
  mutate(Linksliberal=case_when(
    percent_rank(RC1)<0.45 &  percent_rank(RC2)<0.45 ~ 1,
    TRUE ~ 0  )) %>%
  mutate(Rechtsliberal=case_when(
    percent_rank(RC1)<0.45 &  percent_rank(RC2)>0.55 ~ 1,
    TRUE ~ 0 )) %>%
  mutate(Uneindeutig=case_when(
    Linksautoritär==0 & Rechtsautoritär==0 & Linksliberal==0 & Rechtsliberal==0 ~ 1,
    TRUE ~ 0 ))
png("Verteilung der Einstellungen je Bundesland - Promax-PCA ohne gleichgeschlechtliche Ehe.png", 
    units = "cm", height = 8.64,width = 14, res=200) 
df_pca_analytics %>%
  group_by(Bundesland) %>%
  summarize(Linksautoritär= round(mean(Linksautoritär,na.rm=TRUE),digits=3),
            Rechtsautoritär= round(mean(Rechtsautoritär,na.rm=TRUE),digits=3),
            Linksliberal= round(mean(Linksliberal,na.rm=TRUE),digits=3),
            Rechtsliberal= round(mean(Rechtsliberal,na.rm=TRUE),digits=3),
            Uneindeutig= 1 - Linksautoritär - Rechtsautoritär - Linksliberal - Rechtsliberal) %>%
  gather("Kategorie", "Anteil", -Bundesland) %>%
  ggplot( aes(x=Kategorie,y=Anteil,fill=Kategorie))+
  geom_col(position = "dodge")+
  facet_grid(~Bundesland,switch = "x")+
  xlab("")+
  ylab("Anteil")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1,decimal.mark = ","),limits = 
                       c(0,0.26),breaks = c(0,0.05,0.1,0.15,0.2,0.25))+
  ggtitle("Verteilung der Bürger im zweidimensionalen Politikraum")+
  geom_text(aes(label=paste0(format(Anteil*100,decimal.mark=","),"%")),vjust=-0.25)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),plot.background=element_blank(),panel.grid=element_blank(),
        panel.background=element_blank(),
        legend.position="bottom",legend.text = element_text(size=5))
dev.off()

# Welsh-t-Tests
t.test(df_pca_analytics$Linksautoritär~df_pca_analytics$Bundesland)
t.test(df_pca_analytics$Rechtsautoritär~df_pca_analytics$Bundesland)
t.test(df_pca_analytics$Linksliberal~df_pca_analytics$Bundesland)
t.test(df_pca_analytics$Rechtsliberal~df_pca_analytics$Bundesland)
t.test(df_pca_analytics$Uneindeutig~df_pca_analytics$Bundesland)
 

##### Analyse Teil ######

# PCA an Datensatz wieder heran mergen
df_analytics <- full_join(df,df_pca_analytics,by=c("ID","Bundesland","l331a","l331c","l331d","l331e","l171","l156"))

# Rekodierungen

## Hypothese 1
df_analytics$Linksautoritär <- factor(
  df_analytics$Linksautoritär,
  levels = c(0,1),
  labels = c("Nicht LA","LA"),
  ordered = FALSE)

df_analytics$Rechtsautoritär <- factor(
  df_analytics$Rechtsautoritär,
  levels = c(0,1),
  labels = c("Nicht rechtsautoritär","Rechtsautoritär"),
  ordered = FALSE)

df_analytics$Linksliberal <- factor(
  df_analytics$Linksliberal,
  levels = c(0,1),
  labels = c("Nicht linksliberal","Linksliberal"),
  ordered = FALSE)

df_analytics$Rechtsliberal <- factor(
  df_analytics$Rechtsliberal,
  levels = c(0,1),
  labels = c("Nicht rechtsliberal","Rechtsliberal"),
  ordered = FALSE)

### DDR-Sozialisation
df_analytics <- df_analytics %>%
  mutate('DDR_Sozialisation'=case_when(
    Bundesland=="Thüringen" & l2<=1989 & l197==1 ~ 1,
    TRUE ~ 0
  ))

df_analytics$`DDR_Sozialisation` <- factor(
  df_analytics$`DDR_Sozialisation`,
  levels = c(0,1),
  labels =  c("Keine DDR Sozialisation","DDR-Sozialisation"),
  ordered = FALSE)

### Kontrollvariablen H1

#### Alter
df_analytics <- df_analytics %>%
  mutate(Alter_Skala=case_when(
    Bundesland=="Thüringen" ~ 2014-l2,
    Bundesland=="Bayern" ~ 2013-l2,
    TRUE ~ NA_real_)) %>%
  mutate(Alter=case_when(
    l2 >= 1918 & l2 <= 1939 ~ 4,
    l2 >= 1940 & l2 <= 1954 ~ 3,
    l2 >= 1955 & l2 <= 1969 ~ 2,
    l2 >= 1970 & l2 <= 1984 ~ 1,
    l2 >= 1985              ~ 0,
    TRUE ~ NA_real_))

df_analytics$Alter <- factor(
  df_analytics$Alter,
  levels = c(0,1,2,3,4),
  labels = c("ab 1985","1970-1984","1955-1969","1940-1954","1918-1939"),
  ordered = FALSE)


#### Geschlecht
df_analytics <- df_analytics %>%
  mutate(Geschlecht=case_when(
    l1==1 ~ 0,
    l1==2 ~ 1,
    TRUE ~ NA_real_))

df_analytics$Geschlecht <- factor(
  df_analytics$Geschlecht,
  levels = c(0,1),
  labels = c("Mann","Frau"),
  ordered = FALSE)

#### Bildung
df_analytics <- df_analytics %>%
  mutate(Bildung=case_when(
    l3==1 ~ 0,
    l3==2 ~ 0,
    l3==3 ~ 0,
    l3==4 ~ 0,
    l3==5 ~ 1,
    l3==6 ~ 1,
    l3==7 ~ 2,
    l3==8 ~ 2,
    l3==9 ~ NA_real_,
    TRUE ~ NA_real_))

df_analytics$Bildung <- factor(
  df_analytics$Bildung,
  levels = c(0,1,2),
  labels = c("niedrig", "mittel", "hoch"),
  ordered = FALSE)

## Hypothese 2

### Wahrscheinlichkeit Teilnahme an der Wahl
df_analytics <- df_analytics %>%
  mutate(Wahlbeteiligungsabsicht=case_when(
    l70==5 ~ 0,
    l70==4 ~ 0,
    l70==3 ~ 0,
    l70==2 ~ 0,
    l70==1 ~ 1,
    l70==6 ~ 1,
    TRUE ~ NA_real_
  ))

df_analytics$Wahlbeteiligungsabsicht <- factor(
  df_analytics$Wahlbeteiligungsabsicht,
  levels = c(0,1),
  labels = c("eher nicht zur Wahl gehen",
             "bestimmt zur Wahl gehen / Briefwahl"),
  ordered = FALSE)

### Kontrollvariablen H2

#### Politisches Interesse
df_analytics <- df_analytics %>%
  mutate('Politisches Interesse'=case_when(
    l5==5 ~ 0,
    l5==4 ~ 1,
    l5==3 ~ 2,
    l5==2 ~ 3,
    l5==1 ~ 4,
    TRUE ~ NA_real_))

df_analytics$`Politisches Interesse` <- factor(
  df_analytics$`Politisches Interesse`,
  levels = c(0,1,2,3,4),
  labels = c("überhaupt nicht","weniger stark","mittelmäßig","stark","sehr stark"),
  ordered = FALSE)

#### Wahlbeteiligung der Nachbarschaft
df_analytics <- df_analytics %>%
  mutate("Wahlbeteiligung der Nachbarschaft"=case_when(
    l309==5 | l337==5 ~ 0,
    l309==4 | l337==4 ~ 0,
    l309==3 | l337==3 ~ 0,
    l309==2 | l337==2 ~ 1,
    l309==1 | l337==1 ~ 1,
    TRUE ~ NA_real_))

df_analytics$`Wahlbeteiligung der Nachbarschaft` <- factor(
  df_analytics$`Wahlbeteiligung der Nachbarschaft`,
  levels = c(0,1),
  labels = c("eher niemand","eher alle"),
  ordered = FALSE)

## Hypothese H3a & H3b

### Beabsichtigte Zweitstimmabgabe
df_analytics <- df_analytics %>%
  mutate('Wahlentscheidung'=as.numeric(case_when(
    l70<=3 ~ l71bb,
    l70==6 ~ l72bb,
    TRUE ~ NA_real_)))

table(df_analytics$Wahlentscheidung)

### Sonstige Parteien herausfiltern
df_analytics <- df_analytics %>%
  mutate('Wahlentscheidung'=case_when(
    Wahlentscheidung==126 ~ 8,
    Wahlentscheidung==146 ~ 8,
    Wahlentscheidung==151 ~ 8,
    Wahlentscheidung==206 ~ 8,
    Wahlentscheidung==209 ~ 8,
    Wahlentscheidung==225 ~ 8,
    Wahlentscheidung==237 ~ 8,
    Wahlentscheidung==330 ~ 8,
    is.na(l70) ~ NA_real_,
    TRUE ~ Wahlentscheidung))

table(df_analytics$Wahlentscheidung)

df_analytics$Wahlentscheidung <- factor(
  df_analytics$Wahlentscheidung,
  levels = c(2,3,4,5,6,7,8,180,215,322),
  labels = c("CDU","CSU","SPD","FDP","Grüne","Die Linke","Sonstige","Freie Wähler",
             "Piraten","AfD"),
  ordered = FALSE)

#### Kontrollvariablen H3a & H3b

#### Beruf
df_analytics <- df_analytics %>%
  mutate(Beruf=case_when(
    l186==1 | l187==1 ~ 1,
    l186==2 | l187==2 ~ 2,
    l186==3 | l187==3 ~ 3,
    l186==4 | l187==4 ~ 4,
    l186==5 | l187==5 ~ 5,
    l186==6 | l187==6 ~ 6,
    l186==7 | l187==7 ~ 7,
    l186==8 | l187==8 ~ 0,
    TRUE ~ NA_real_))

df_analytics$Beruf <- factor(
  df_analytics$Beruf,
  levels = c(0,1,2,3,4,5,6,7),
  labels = c("In Ausbildung","Arbeiter","Angestellter","Beamter/Richter/Berufssoldat",
             "kademisch freier Beruf (z.B. Arzt mit eigener Praxis, Rechtsanwalt)",
             "Selbstständiger in Handel, Gewerbe, Industrie, Dienstleistung u.a.",
             "selbstständiger Landwirt",
             "mithelfender Familienangehöriger"),
  ordered = FALSE)

#### Dimensionspräferenz
df_analytics <- df_analytics %>%
  mutate('Wichtigkeit wirtschaftliche Dimension'=case_when(
    l157==5 ~ 0,
    l157==4 ~ 1,
    l157==3 ~ 2,
    l157==2 ~ 3,
    l157==1 ~ 4,
    TRUE ~ NA_real_)) %>%
  mutate('Wichtigkeit kulturelle Dimension'=case_when(
    l172==5 ~ 0,
    l172==4 ~ 1,
    l172==3 ~ 2,
    l172==2 ~ 3,
    l172==1 ~ 4,
    TRUE ~ NA_real_))

df_analytics <- df_analytics %>%
  mutate('Differenz Wichtigkeiten'=`Wichtigkeit kulturelle Dimension` - 
           `Wichtigkeit wirtschaftliche Dimension`)

df_analytics$`Wichtigkeit kulturelle Dimension` <- factor(
  df_analytics$`Wichtigkeit kulturelle Dimension`,
  levels = c(0,1,2,3,4),
  labels = c("überhaupt nicht wichtig","nicht so wichtig","mittelmäßig",
             "wichtig","sehr wichtig"),
  ordered = FALSE)

df_analytics$`Wichtigkeit kulturelle Dimension` <- factor(
  df_analytics$`Wichtigkeit kulturelle Dimension`,
  levels = c(0,1,2,3,4),
  labels = c("überhaupt nicht wichtig","nicht so wichtig","mittelmäßig",
             "wichtig","sehr wichtig"),
  ordered = FALSE)

df_analytics <- df_analytics %>%
  mutate('Präferenz wirtschaftliche Dimension'=case_when(
    `Differenz Wichtigkeiten` <0 ~ 1,
    `Differenz Wichtigkeiten`>=0 ~ 0,
    TRUE ~ NA_real_)) %>%
  mutate('Präferenz kulturelle Dimension'=case_when(
    `Differenz Wichtigkeiten` >0 ~ 1,
    `Differenz Wichtigkeiten`<=0 ~ 0,
    TRUE ~ NA_real_))

df_analytics$`Präferenz kulturelle Dimension` <- factor(
  df_analytics$`Präferenz kulturelle Dimension`,
  levels = c(0,1),
  labels = c("Keine Präferenz der kulturellen Dimension",
             "Präferenz der kulturellen Dimension"),
  ordered = FALSE)

df_analytics$`Präferenz wirtschaftliche Dimension` <- factor(
  df_analytics$`Präferenz wirtschaftliche Dimension`,
  levels = c(0,1),
  labels = c("Keine Präferenz der wirtschaftlichen Dimension",
             "Präferenz der wirtschaftlichen Dimension"),
  ordered = FALSE)


### Sonstige Rekodierungen
#### Subjektive Klassenzugehörigkeit
df_analytics <- df_analytics %>%
  mutate('Subjektive_Klassenzugehörigkeit'=case_when(
    l196==1 ~ 0,
    l196==2 ~ 1,
    l196==3 ~ 2,
    l196==4 ~ 3,
    l196==5 ~ 4,
    l196==6 ~ 5,
    TRUE ~ NA_real_))

df_analytics$`Subjektive_Klassenzugehörigkeit` <- factor(
  df_analytics$`Subjektive_Klassenzugehörigkeit`,
  levels = c(0,1,2,3,4,5),
  labels = c("Unterschicht","Arbeiterschicht","untere Mittelschicht",
             "mittlere Mittelschicht","obere Mittelschicht","Oberschicht"),
  ordered = FALSE)

# Analyse

## Hypothese H1

### Altersskala
H1 <- glm(Linksautoritär ~ Geschlecht+Bildung+Alter_Skala*`DDR_Sozialisation`,data=df_analytics,family=binomial(link="probit"))
PseudoR2(H1,which="all")
print(summary(H1), digits=2)
#### Average Marginal Effects
ame_H1 <-margins(H1, ci.level=0.83)
print(summary(ame_H1),digits = 3)
#### Marginale Effekte
margins_H1 <- ggpredict(H1,terms = c("Alter_Skala","DDR_Sozialisation","Bildung","Geschlecht"),ci.lvl = 0.83)
svg("Marginale Effekte H1 Alterssakala.svg")
plot(margins_H1,connect.lines = TRUE,show.title=FALSE)
dev.off()

### Kategoriale Altersvariable
H1_1 <- glm(
  Linksautoritär ~ Geschlecht+Bildung+Alter*`DDR_Sozialisation`
  ,data=df_analytics,family=binomial(link="probit"))
summary(H1_1)
PseudoR2(H1_1,which="all")
#### Average Marginal Effects
ame_H1_1 <-margins(H1_1, ci.level=0.83)
print(summary(ame_H1_1),digits = 2)
#### Marginale Effekte
margins_H1_1 <- ggpredict(H1_1,terms = c("Alter","DDR_Sozialisation","Bildung","Geschlecht"),ci.lvl = 0.83)
svg("Marginale Effekte H1 Alterskategorie.svg", width = 20, height = 10)
plot(margins_H1_1,connect.lines = TRUE,show.title=FALSE)
dev.off()

## Hypothese H2
H2 <- glm(Wahlbeteiligungsabsicht ~ Linksautoritär*Bundesland+`Politisches Interesse`
            +`Wahlbeteiligung der Nachbarschaft`
            ,data=df_analytics,family=binomial(link="probit"))
PseudoR2(H2,which="all")
print(summary(H2),digits = 2)
### Marginale Effekte
margins_H2<- ggpredict(H2,terms=c("Linksautoritär","Bundesland","Wahlbeteiligung der Nachbarschaft",
                                  "Politisches Interesse"),ci.lvl = 0.83)
svg("Marginale Effekte H2.svg")
plot(margins_H2,show.title=FALSE)
dev.off()
### Average Marginal Effects
ame_H2<-margins(H2, ci.level=0.83)
print(summary(ame_H2),digits = 3)

df_sig_bayern <- df_analytics %>%
  filter(Bundesland=="Bayern") %>%
  mutate(Wahlbeteiligungsabsicht=as.numeric(Wahlbeteiligungsabsicht)-1)

t.test(df_sig_bayern$Wahlbeteiligungsabsicht~df_sig_bayern$Linksautoritär)

df_sig_th <- df_analytics %>%
  filter(Bundesland=="Thüringen") %>%
  mutate(Wahlbeteiligungsabsicht=as.numeric(Wahlbeteiligungsabsicht)-1)

t.test(df_sig_th$Wahlbeteiligungsabsicht~df_sig_th$Linksautoritär)

## Hypothese H3a
df_H3a <- df_analytics %>%
  filter(Bundesland=="Thüringen") %>%
  mutate(AfD=case_when(
    Wahlentscheidung=="AfD" ~ 1,
    Wahlentscheidung!="AfD" ~ 0,
    TRUE ~ NA_real_))

H3a <- multinom(Wahlentscheidung~Linksautoritär
                +Beruf+Bildung+Alter*`DDR_Sozialisation`
                +`Präferenz kulturelle Dimension`
                +`Präferenz wirtschaftliche Dimension`
                , data=df_H3a,model=TRUE)

print(summary(H3a),digits = 2)
stargazer(H3a, type="text")
PseudoR2(H3a,which="all")

H3a_dummy <- glm(AfD~Linksautoritär
          +Beruf+Bildung+Alter*`DDR_Sozialisation`
          +`Präferenz kulturelle Dimension`
          +`Präferenz wirtschaftliche Dimension`
          ,data=df_H3a,family=binomial(link="probit"))

### Average Marginal Effects
ame_H3a_dummy<-margins(H3a_dummy, ci.level=0.83)
print(summary(ame_H3a_dummy),digits = 2)

print(summary(H3a_dummy),digits = 2)
stargazer(H3a_dummy, type="text")
PseudoR2(H3a_dummy,which="all")
## Hypothese H3b
df_H3b <- df_analytics %>%
  filter(Bundesland=="Bayern", Wahlentscheidung!="AfD")%>%
  mutate(FW=case_when(
    Wahlentscheidung=="Freie Wähler" ~ 1,
    Wahlentscheidung!="Freie Wähler" ~ 0,
    TRUE ~ NA_real_
  ))

H3b <- multinom(Wahlentscheidung~Linksautoritär
                      +Beruf+Bildung+Alter
                      +`Präferenz kulturelle Dimension`
                      +`Präferenz wirtschaftliche Dimension`
                      , data=df_H3b,modell=TRUE)

print(summary(H3b),digits = 2)
stargazer(H3b, type="text")
PseudoR2(H3b,which="all")

H3b_Inter <- multinom(Wahlentscheidung~Linksautoritär*`Präferenz kulturelle Dimension`
                +Beruf+Bildung+Alter
               +`Präferenz wirtschaftliche Dimension`
                , data=df_H3b,modell=TRUE)

print(summary(H3b_Inter),digits = 2)
stargazer(H3b_Inter, type="text")
PseudoR2(H3b_Inter,which="all")

H3b_Dummy <- glm(FW~Linksautoritär*`Präferenz kulturelle Dimension`
                 +Beruf+Bildung+Alter
                 +`Präferenz wirtschaftliche Dimension`
                 ,data=df_H3b,family=binomial(link="probit"))
### Average Marginal Effects
ame_H3b_dummy<-margins(H3b_Dummy, ci.level=0.83)
print(summary(ame_H3b_dummy),digits = 2)

print(summary(H3b_Dummy),digits = 2)
stargazer(H3b_Dummy, type="text")
PseudoR2(H3b_Dummy,which="all")

###### Anhang

# Anhang b KMO-Tests mit Ehe für Alle
df_temp <- df_pca %>%
  dplyr::select(-Bundesland,-ID)
kmo <- psych::KMO(df_temp)
MSA <- as.data.frame(kmo$MSAi)
MSA <- rename(MSA,MSA="kmo$MSAi")
MSA <- arrange(MSA,MSA)
oMSA <- round(kmo$MSA,digits = 2)
names <- row.names(MSA)
MSA <- rbind(MSA,oMSA)
MSA <- MSA %>%
  mutate(Beurteilung=case_when(
    MSA < 0.5 ~ "Nicht akzeptabel",
    MSA >=0.5 & MSA < 0.6 ~ "Schlecht",
    MSA >=0.6 & MSA < 0.7 ~ "Mäßig",
    MSA >=0.7 & MSA < 0.8 ~ "Gut",
    MSA >=0.8 & MSA < 0.9 ~ "Sehr Gut",
    MSA >=0.9  ~ "Perfekt",
    TRUE ~ NA_character_))
names <- append(names,"MSA insgesamt")
row.names(MSA) <- names
print(MSA,digits = 2)

# Anhang c
# Varimax-rotierte PCA ohne gleichgeschlechtliche Ehe
df_temp <- df_pca %>%
  dplyr:: select(-Bundesland,-l331h,-ID)
vari <- principal(df_temp,nfactors = 2, rotate = "Varimax")
summary(vari)
print(loadings(vari),digits = 2)

# Anhang d
# Promax-rotierte PCA mit gleichgeschlechtliche Ehe
df_temp <- df_pca %>%
  dplyr:: select(-Bundesland,-ID)
alternative <- principal(df_temp,nfactors = 2, rotate = "Promax")
summary(alternative)
print(loadings(alternative),digits = 2)

# Anhang e
# Varimax-rotierte PCA mit gleichgeschlechtliche Ehe
df_temp <- df_pca %>%
  dplyr:: select(-Bundesland,-ID)
alternative2 <- principal(df_temp,nfactors = 2, rotate = "Varimax")
summary(alternative2)
print(loadings(alternative2),digits = 2)

# Anhang f Scree-Plot mit gleichgeschlechtlicher Ehe
df_temp <- df_pca %>%
  dplyr:: select(-Bundesland,-ID)
test <-fa.parallel(df_temp,fa="pc",main="Für beide Bundesländer mit Ehe für Alle",sim=FALSE,
                   ylabel="Eigenvalue der Komponenten",show.legend = TRUE)
test2 <- as.data.frame(test$pc.values)
test2$number <- c(1,2,3,4,5,6,7)
png("Scree-Plot mit gleichgeschlechtlicher Ehe.png", units = "cm", height = 8.64,width = 14, res=600)
ggplot(test2,aes(y=`test$pc.values`,x=number,label=round(`test$pc.values`,digits = 2)))+
  geom_line()+
  ylab("Eigenvalue")+
  xlab("Anzahl der Komponenten")+
  scale_x_continuous(n.breaks = 7,limits = c(1,7))+
  ylim(0,2.5)+
  geom_point()+
  geom_text(aes(label=round(`test$pc.values`,digits = 2)),hjust=0, vjust=-1)+
  theme_economist()
dev.off()
