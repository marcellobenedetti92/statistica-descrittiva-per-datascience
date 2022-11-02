# 1) importo il dataset dalla working directory
dati <- read.csv("realestate_texas.csv",sep = ",")

# verifico il separatore cliccando sull'oggetto appena creato nel Global Environment



# 3) INDICI DI POSIZIONE

# estraggo delle colonne dal data frame per evitare ogni volta di utilizzare il simbolo dollaro per riferirsi alle colonne
attach(dati)

# MEDIA ARITMETICA
# utilizzo la funzione round per rispettare le tipologie di variabili
round(mean(sales),0)
round(mean(volume),3)
round(mean(median_price),0)
round(mean(listings),0)
round(mean(months_inventory),1)
# in questo caso, trattandosi di variabile continua, ho preferito arrotondare rispettando il numero di decimali a dataset

# MEDIA PONDERATA
# non ha senso calcolarla se le variabili non presentano un peso
# suddividendo in classi perderemmo informazione, quindi non ha senso fare neanche questo
# fonte: Giuseppe Dejan Lucido

# MEDIA ARMONICA
# non ha senso calcolarla perché l'utilizzo è appropriato quando i dati rappresentano dei rapporti o delle velocità

# MEDIA GEOMETRICA
# per calcolarla (su una variabile come sales ad esempio) avrei bisogno di raggruppare e ordinare almeno per anno
# le % di variazione YoY potrebbero essere però negative, per cui la funzione geometric_mean restituirebbe NaN

# MEDIANA
nrow(dati)
# il numero di righe è pari, per cui la mediana verrebbe calcolata con
# la virgola, anche per variabili discrete
round(median(sales),0)
round(median(volume),3)
round(median(median_price),0)
round(median(listings),0)
round(median(months_inventory),1)

# MODA
table(city)
# si tratta di una variabile quadrimodale

# MIN E MAX
min(sales)
max(sales)

min(volume)
max(volume)

min(median_price)
max(median_price)

min(listings)
max(listings)

min(months_inventory)
max(months_inventory)


# PERCENTILE
round(quantile(sales,(seq(0,1,0.01))))
round(quantile(volume,(seq(0,1,0.01))),3)
round(quantile(median_price,(seq(0,1,0.01))))
round(quantile(listings,(seq(0,1,0.01))))
round(quantile(months_inventory,(seq(0,1,0.01))),1)



# INDICI DI VARIABILITÀ

# RANGE O INTERVALLO DI VARIAZIONE
max(sales)-min(sales)
max(volume)-min(volume)
max(median_price)-min(median_price)
max(listings)-min(listings)
max(months_inventory)-min(months_inventory)

# RANGE INTERQUANTILE
round(IQR(sales))
round(IQR(volume),3)
round(IQR(median_price))
round(IQR(listings))
round(IQR(months_inventory),1)

# VARIANZA
n=length(sales)
mu_sales=round(mean(sales))
sigma2_sales = sum((sales-mu_sales)^2)/n

mu_volume=round(mean(volume),3)
sigma2_volume = sum((volume-mu_volume)^2)/n

mu_median_price=round(mean(median_price))
sigma2_median_price = sum((median_price-mu_median_price)^2)/n

mu_listings=round(mean(listings))
sigma2_listings = sum((listings-mu_listings)^2)/n

mu_months_inventory=round(mean(months_inventory),1)
sigma2_months_inventory = sum((months_inventory-mu_months_inventory)^2)/n

# DEVIAZIONE STANDARD
sigma_sales = round(sqrt(sigma2_sales))
sigma_volume = round(sqrt(sigma2_volume),3)
sigma_median_price = round(sqrt(sigma2_median_price))
sigma_listings = round(sqrt(sigma2_listings))
sigma_months_inventory = round(sqrt(sigma2_months_inventory),1)

# COEFFICIENTE DI VARIAZIONE
CV <- function(x){
  return(sd(x)/mean(x)*100)
}

round(CV(sales))
round(CV(volume),3)
round(CV(median_price))
round(CV(listings))
round(CV(months_inventory),1)
# la variabile volume è relativamente più varibile delle altre
# la variabile median_price è quella relarivamente meno variabile (abbastanza scontato)

# INDICE DI GINI: non ha senso calcolarlo, dato che la variabile city abbiamo riscontrato essere quadrimodale, per cui indice = 1 (equidistribuzione)


# INDICI DI FORMA

# ASIMMETRIA

# MOMENTO TERZO DELLA DISTRIBUZIONE
m3_sales <- sum( (sales-mu_sales)^3 )/n
m3_volume <- sum( (volume-mu_volume)^3 )/n
m3_median_price <- sum( (median_price-mu_median_price)^3 )/n
m3_listings <- sum( (listings-mu_listings)^3 )/n
m3_months_inventory <- sum( (months_inventory-mu_months_inventory)^3 )/n

# INDICE DI FISHER
Asim.Index_sales <- m3_sales / sigma_sales^3
# distribuzione asimmetrica positiva
# verifico se vale media > mediana > moda
mean(sales)
median(sales)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(sales)
# fonte della funzione: https://www.tutorialspoint.com/r/r_mean_median_mode.htm

table(sales)
# verifico ulteriormente con quanto visto nel corso
# c'è un unico valore di moda, per cui il rapporto viene rispettato

# verifico anche graficamente
plot(density(sales))


Asim.Index_volume <- m3_volume / sigma_volume^3
# distribuzione asimmetrica positiva
# verifico se vale media > mediana > moda
mean(volume)
median(volume)
getmode(volume)
# ci sono diversi valori di moda, per cui il rapporto non viene rispettato se non si prende il valore più alto di moda
# HA SENSO QUINDI CALCOLARE LA MODA PER VARIABILI DI QUESTO TIPO?
# DOVE STA IL DISCRIMINE TRA SENSATEZZA DEL CALCOLO E NON SENSATEZZA?
# RISPOSTA DI GIUSEPPE DEJAN LUCIDO: la moda si calcola per le variabili qualitative e suddivise in classi, per quelle con tanti valori non ha molto senso, a meno che non ti riferisci a un intervallo di valori “modale”

Asim.Index_median_price <- m3_median_price / sigma_median_price^3
# distribuzione asimmetrica negativa
# verifico se vale media < mediana < moda
mean(median_price)
median(median_price)

Asim.Index_listings <- m3_listings / sigma_listings^3
# distribuzione asimmetrica positiva
# verifico se vale media > mediana > moda
mean(listings)
median(listings)

Asim.Index_months_inventory <- m3_months_inventory / sigma_months_inventory^3
# distribuzione asimmetrica positiva
# verifico se vale media > mediana > moda
mean(months_inventory)
median(months_inventory)


# COEFFICIENTE DI CURTOSI
install.packages("moments")
library(moments)

kurtosis(sales)-3
# distribuzione platicurtica
# verifico graficamente
plot(density(sales))

kurtosis(volume)-3
# distribuzione leptocurtica
plot(density(volume))
# NON SEMBRA UNA LEPTOCURTICA

kurtosis(median_price)-3
plot(density(median_price))
# distribuzione platicurtica

kurtosis(listings)-3
plot(density(listings))
# distribuzione platicurtica

kurtosis(months_inventory)-3
plot(density(months_inventory))
# distribuzione platicurtica



# 3) DISTRIBUZIONE IN CLASSI DELLA VARIABILE QUANTITATIVA months_inventory
range(months_inventory)
(max(months_inventory)-min(months_inventory))/4

months_inventory_cl <- cut(months_inventory,
                    breaks = c(3.4, 3.4+2.875, 3.4+(2.875*2), 3.4+(2.875*3), 3.4+(2.875*4)))

# verifico che abbia creato le giuste classi
levels(months_inventory_cl)

N <- length(months_inventory)
N

# calcolo le frequenze
ni <- table(months_inventory_cl)
fi <- ni/N
Ni <- cumsum(ni)
Fi <- Ni/N

# concateno le colonne in un data frame
distr_freq <- as.data.frame(cbind(ni,fi,Ni,Fi))
distr_freq

# costruisco il grafico a barre
barplot(distr_freq$ni,
        main = "Distribuzione delle classi di months_inventory",
        sub = "Quantità di tempo necessaria per vendere tutte le inserzioni",
        xlab = "Quanti mesi",
        ylab = "Frequenze assolute",
        ylim = c(0,150),
        col = "blue",
        names.arg = rownames(distr_freq))

# calcolo l'indice di Gini
gini.index <- function(x){
  ni=table(x)
  fi=ni/length(x)
  fi2=fi^2
  J = length(table(x))
  
  gini = 1-sum(fi2)
  gini.normalizzato = gini/((J-1)/J)
  
  return(gini.normalizzato)
}

attach(dati)
table(months_inventory)
gini.index(months_inventory)


# CREO UNA NUOVA COLONNA CON PREZZO MEDIO
install.packages("dplyr")
library(dplyr)

# CREO UNA COLONNA PER VALUTARE L'EFFICACIA DEGLI ANNUNCI
dati_con_percent_vendut <- mutate(dati, percentuale_venduta = sales/listings*100)

dati_con_percent_vendut %>% 
  select(city,percentuale_venduta) %>% 
  group_by(city) %>% 
  summarise(mean_percentuale_venduta = mean(percentuale_venduta)) %>% 
  arrange(desc(mean_percentuale_venduta))

# considerazione: la città di Bryan-College Station ha la più alta percentuale di venduto rispetto agli annunci attivi,
# questo senza considerare anno e mese, il che mi porta all'idea che sia una zona piuttosto ricercata dagli acquirenti



# 10) SUMMARY CONDIZIONATI
# sulla variabile sales
mean_sales_and_sd_per_city <- dati %>%
  select(city,sales) %>% 
  group_by(city) %>% 
  summarise(sales_per_city = round(mean(sales)),
            sd_per_city = round(sd(sales))) %>% 
  arrange(desc(sales_per_city))

# nonostante le percentuale_venduta media sia maggiore per la città di Bryan-College Station, da questo ultimo summary si può dedurre come il dato sia condizionato dall'alta deviazione standard (la città di Tyler è infatti prima per venduto)

mean_sales_and_sd_per_year <- dati %>%
  select(year,sales) %>% 
  group_by(year) %>% 
  summarise(sales_per_year = round(mean(sales)),
            sd_per_year = round(sd(sales))) %>% 
  arrange(desc(sales_per_year))

mean_sales_and_sd_per_month <- dati %>%
  select(month,sales) %>% 
  group_by(month) %>% 
  summarise(sales_per_month = round(mean(sales)),
            sd_per_month = round(sd(sales))) %>% 
  arrange(desc(sales_per_month))

# sulla variabile volume
mean_volume_and_sd_per_city <- dati %>%
  select(city,volume) %>% 
  group_by(city) %>% 
  summarise(volume_per_city = round(mean(volume),3),
            sd_per_city = round(sd(volume),3)) %>% 
  arrange(desc(volume_per_city))

mean_volume_and_sd_per_year <- dati %>%
  select(year,volume) %>% 
  group_by(year) %>% 
  summarise(volume_per_year = round(mean(volume),3),
            sd_per_year = round(sd(volume),3)) %>% 
  arrange(desc(volume_per_year))

mean_volume_and_sd_per_month <- dati %>%
  select(month,volume) %>% 
  group_by(month) %>% 
  summarise(volume_per_month = round(mean(volume),3),
            sd_per_month = round(sd(volume),3)) %>% 
  arrange(desc(volume_per_month))



# GRAFICI CON GGPLOT2
install.packages("ggplot2")
library(ggplot2)
attach(dati)

# media vendite per città
ggplot(data=dati,
       aes(x = city,
           y = sales,
           fill = city)) +
  geom_bar(stat = "summary", fun = mean) +
  labs(x = "Città",
       y = "Sales")

# media vendite per anno
ggplot(data=dati,
       aes(x = year,
           y = sales,
           fill = year)) +
  geom_bar(stat = "summary", fun = mean) +
  labs(x = "Anno",
       y = "Sales")

# media vendite per mese
ggplot(data=dati,
       aes(x = month,
           y = sales,
           fill = month)) +
  geom_bar(stat = "summary", fun = mean) +
  labs(x = "Mese",
       y = "Sales") +
  scale_x_continuous(breaks = seq(0,12,1))

# efficacia degli annunci per città
ggplot(data=dati_con_percent_vendut,
       aes(x = city,
           y = percentuale_venduta,
           fill = city)) +
  geom_bar(stat = "summary", fun = mean) +
  labs(x = "Città",
       y = "% Venduta")

install.packages("ggeasy")
install.packages("stringr")
install.packages("patchwork")
library(ggeasy)
library(stringr)
library(patchwork)

ggplot(data=dati_con_percent_vendut,
       aes(x = reorder(city,-percentuale_venduta),
           # ordinate in ordine decrescente le modalità sull'asse x
           y = percentuale_venduta,
           fill = city)) +
  geom_bar(stat = "summary", fun = mean) +
  labs(x = "Città",
       y = "% Venduta") +
  theme_classic() +
  scale_fill_discrete(breaks=c(
    "Bryan-College Station", "Wichita Falls", "Beaumont", "Tyler"),
    # ordinata la legenda manualmente
    labels=c("Br","Wi","Be","Ty")
    ) +
# minimizzati i caratteri della legenda per una più facile lettura
scale_x_discrete(labels= c("Bryan-College Station"="Br",
                           "Wichita Falls"="Wi",
                           "Beaumont"="Be",
                           "Tyler"="Ty"
                           )) +
# minimizzati i caratteri sull'asse x per una più facile lettura
annotate("text",x=3,y=14, label=
           "% sales/listings\nmigliore per Br", color=2)
# aggiunto commento con a capo

# COME COLORARE IL TESTO DELLO STESSO COLORE DELLA MODALITÀ NEL GRAFICO?

# grafico a barre affiancate (città) delle vendite negli anni
dati %>%
  group_by(year, city) %>%
  summarise(tot_sales = sum(sales)) %>%
  ggplot(aes(x = year, y = tot_sales, fill = city)) + 
  geom_bar(stat = 'identity', position = position_dodge())



# 1) BOXPLOT confronto la distribuzione del prezzo mediano delle case tra le varie città
ggplot(data=dati)+
  geom_boxplot(aes(x=city,
                   y=median_price),
               fill="lightgreen")
# commento il risultato:
# le case di Bryan-College Station non solo presentano la mediana più alta in termini di prezzo, ma anche il prezzo minimo supera il terzo quartile della seconda posizionata. Prababilmente è una zona "in"

# 2) Utilizzo il boxplot per confrontare la distribuzione del valore totale delle vendite tra le varie città ma anche tra i vari anni
ggplot(data=dati)+
  geom_boxplot(aes(x=year,
                   y=volume,
                   fill=city))

# non funziona, devo trasformare l'anno in fattore
anno <- as.factor(year)
città <- as.factor(city) 

ggplot(data=dati)+
  geom_boxplot(aes(x=anno,
                   y=volume,
                   fill=città))

# verifico con dplyr Tyler nel 2013: dovrebbe essere intorno ai 60 di volume
volume_tyler_2013 <- dati %>%
  select(volume) %>% 
  filter(year=="2013",city=="Tyler")
  
range(volume_tyler_2013)
# OK

# considerazioni:
# Wichita Falls e Beaumont muovono un volume minore rispetto alle altre due città
# Bryan-College Station ha un range interquartile meno concentrato rispetto a Tyler.



# 3) Uso un grafico a barre sovrapposte per ogni anno,  per confrontare il totale delle vendite nei vari mesi, sempre considerando le città
dati %>%
  group_by(year, city) %>%
  summarise(tot_sales = sum(sales)) %>%
  ggplot(aes(x = year, y = tot_sales, fill = city)) + 
  geom_bar(stat = 'identity')

# commenti:
# dal 2011 c'è una tendenza in positivo per le vendite totali
# già visivamente si vede come il contributo maggiore sia di Tyler

# grafico a barre sovrapposte normalizzato
# DATO CHE OGNI ANNO HA UN NUMERO DI SALES DIVERSO DA TUTTI GLI ALTRI, NON RIESCO A TROVARE UN MODO PER GRAFICARE LA CONTRIBUZIONE DI CIASCUNA CITTÀ (L'1% DI UN ANNO è INFATTI DIVERSO DALL'1% DI TUTTI GLI ALTRI ANNI)



# 4) Creo un line chart di una variabile a mia scelta per fare confronti commentati fra città e periodi storici

# preparo il dataframe
mean_percent_venduta_per_anno_city <- dati_con_percent_vendut %>% 
  select(city,year,percentuale_venduta) %>% 
  group_by(city,year) %>% 
  summarise(mean_percentuale_venduta = mean(percentuale_venduta))

mean_percent_venduta_per_anno_city %>% 
  ggplot(aes(x=year,
             y=mean_percentuale_venduta,
             colour=city))+
  geom_point(size=5, alpha=0.3)+
  geom_line(size=1)+
  theme_minimal()+
  labs(title = "sales/listings trends per city")

# commento a termine di Giuseppe Dejan Lucido: prova anche gli strati facet_grid() e facet_wrap() per fare più pannelli nella stessa finestra