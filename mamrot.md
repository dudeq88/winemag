---
title: "Analiza recenzji wina"
output: 
  html_document:
    keep_md: true
---

# wstęp

Nauczymy się pakietu tidyverse i przy okazji przeanalizujemy dane dotyczące recenzji win.

## Ładujemy biblioteki


```r
library(tidyverse)
library(mlr)
```

## wczytujemy dane

Dane w formacie .csv wczytujemy biblioteką readr (częśc tidyverse)


```r
wine <- read_csv(file="winemag-data-130k-v2.csv")
```

```
## Warning: Missing column names filled in: 'X1' [1]
```

## Zamiana na kategorie


```r
wine <- wine%>%mutate_at(vars(country, province, region_1, region_2,
                      variety, winery, designation, taster_name),
                 as.factor)
```


## Oglądamy wczytane dane


```r
glimpse(wine)
```

```
## Observations: 129,971
## Variables: 14
## $ X1                    <int> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12...
## $ country               <fct> Italy, Portugal, US, US, US, Spain, Ital...
## $ description           <chr> "Aromas include tropical fruit, broom, b...
## $ designation           <fct> Vulka Bianco, Avidagos, NA, Reserve Late...
## $ points                <int> 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, ...
## $ price                 <dbl> NA, 15, 14, 13, 65, 15, 16, 24, 12, 27, ...
## $ province              <fct> Sicily & Sardinia, Douro, Oregon, Michig...
## $ region_1              <fct> Etna, NA, Willamette Valley, Lake Michig...
## $ region_2              <fct> NA, NA, Willamette Valley, NA, Willamett...
## $ taster_name           <fct> Kerin O’Keefe, Roger Voss, Paul Gregutt,...
## $ taster_twitter_handle <chr> "@kerinokeefe", "@vossroger", "@paulgwin...
## $ title                 <chr> "Nicosia 2013 Vulka Bianco  (Etna)", "Qu...
## $ variety               <fct> White Blend, Portuguese Red, Pinot Gris,...
## $ winery                <fct> Nicosia, Quinta dos Avidagos, Rainstorm,...
```

```r
summary(wine)
```

```
##        X1             country      description              designation   
##  Min.   :     0   US      :54504   Length:129971      Reserve     : 2009  
##  1st Qu.: 32493   France  :22093   Class :character   Estate      : 1322  
##  Median : 64985   Italy   :19540   Mode  :character   Reserva     : 1259  
##  Mean   : 64985   Spain   : 6645                      Riserva     :  698  
##  3rd Qu.: 97478   Portugal: 5691                      Estate Grown:  621  
##  Max.   :129970   (Other) :21435                      (Other)     :86597  
##                   NA's    :   63                      NA's        :37465  
##      points           price               province    
##  Min.   : 80.00   Min.   :   4.00   California:36247  
##  1st Qu.: 86.00   1st Qu.:  17.00   Washington: 8639  
##  Median : 88.00   Median :  25.00   Bordeaux  : 5941  
##  Mean   : 88.45   Mean   :  35.36   Tuscany   : 5897  
##  3rd Qu.: 91.00   3rd Qu.:  42.00   Oregon    : 5373  
##  Max.   :100.00   Max.   :3300.00   (Other)   :67811  
##                   NA's   :8996      NA's      :   63  
##                  region_1                  region_2    
##  Napa Valley         : 4480   Central Coast    :11065  
##  Columbia Valley (WA): 4124   Sonoma           : 9028  
##  Russian River Valley: 3091   Columbia Valley  : 8103  
##  California          : 2629   Napa             : 6814  
##  Paso Robles         : 2350   Willamette Valley: 3423  
##  (Other)             :92050   (Other)          :12078  
##  NA's                :21247   NA's             :79460  
##             taster_name    taster_twitter_handle    title          
##  Roger Voss       :25514   Length:129971         Length:129971     
##  Michael Schachner:15134   Class :character      Class :character  
##  Kerin O’Keefe    :10776   Mode  :character      Mode  :character  
##  Virginie Boone   : 9537                                           
##  Paul Gregutt     : 9532                                           
##  (Other)          :33234                                           
##  NA's             :26244                                           
##                      variety                     winery      
##  Pinot Noir              :13272   Wines & Winemakers:   222  
##  Chardonnay              :11753   Testarossa        :   218  
##  Cabernet Sauvignon      : 9472   DFJ Vinhos        :   215  
##  Red Blend               : 8946   Williams Selyem   :   211  
##  Bordeaux-style Red Blend: 6915   Louis Latour      :   199  
##  (Other)                 :79612   Georges Duboeuf   :   196  
##  NA's                    :    1   (Other)           :128710
```

## braki w danych

SPrawdzamy czy brakuje danych


```r
colSums(is.na(wine))
```

```
##                    X1               country           description 
##                     0                    63                     0 
##           designation                points                 price 
##                 37465                     0                  8996 
##              province              region_1              region_2 
##                    63                 21247                 79460 
##           taster_name taster_twitter_handle                 title 
##                 26244                 31213                     0 
##               variety                winery 
##                     1                     0
```

# Analiza - EDA

Co nas interesuje w analizie:

- z jakiego kraju jest najdrozsze wino?
- z jakiego kraju jest najlepsze wino?
- ktor recenzent psize najdluzsze opinie?
- czy jest korelacja miedzy recenzentem i oceną?
- czy recenzent jest powiazany z panstwem?
- TOP 5 win ponizej 30zl
- cena vs punkty
- 5 najgorszych drogich win
- czy lepsze wino dostaje dluzsza recenzje?
- najczesciej wystepujace slowa w recenzjach?

## Duplikaty?

Usuwamy te wina ktore pojawiaja sie w ziorze po raz n-ty.


```r
wine <- distinct(wine, title, .keep_all = T)
```

## filtrowanie danych

Funkcja `filter` pozwala nam wybrac oserwacje spelniajace kryteria.


```r
wine %>% filter (points == 100)
```

```
## # A tibble: 18 x 14
##        X1 country description designation points price province region_1
##     <int> <fct>   <chr>       <fct>        <int> <dbl> <fct>    <fct>   
##  1    345 Austra~ This wine ~ Rare           100   350 Victoria Rutherg~
##  2   7335 Italy   Thick as m~ Occhio di ~    100   210 Tuscany  Vin San~
##  3  36528 France  This is a ~ Brut           100   259 Champag~ Champag~
##  4  39286 Italy   A perfect ~ Masseto        100   460 Tuscany  Toscana 
##  5  42197 Portug~ This is th~ Barca-Velha    100   450 Douro    <NA>    
##  6  45781 Italy   This gorge~ Riserva        100   550 Tuscany  Brunell~
##  7  45798 US      Tasted in ~ <NA>           100   200 Califor~ Napa Va~
##  8  58352 France  This is a ~ <NA>           100   150 Bordeaux Saint-J~
##  9  89728 France  This lates~ Cristal Vi~    100   250 Champag~ Champag~
## 10  89729 France  This new r~ Le Mesnil ~    100   617 Champag~ Champag~
## 11 111753 France  Almost bla~ <NA>           100  1500 Bordeaux Pauillac
## 12 111754 Italy   It takes o~ Cerretalto     100   270 Tuscany  Brunell~
## 13 111755 France  This is th~ <NA>           100  1500 Bordeaux Saint-É~
## 14 111756 France  A hugely p~ <NA>           100   359 Bordeaux Saint-J~
## 15 113929 US      In 2005 Ch~ Royal City     100    80 Washing~ Columbi~
## 16 114972 Portug~ A powerful~ Nacional V~    100   650 Port     <NA>    
## 17 118058 US      This wine ~ La Muse        100   450 Califor~ Sonoma ~
## 18 123545 US      Initially ~ Bionic Frog    100    80 Washing~ Walla W~
## # ... with 6 more variables: region_2 <fct>, taster_name <fct>,
## #   taster_twitter_handle <chr>, title <chr>, variety <fct>, winery <fct>
```

Filtrowanie wg kilku kryteriow


```r
wine%>%filter(points>90, price < 10)
```

```
## # A tibble: 5 x 14
##      X1 country description designation points price province region_1
##   <int> <fct>   <chr>       <fct>        <int> <dbl> <fct>    <fct>   
## 1 10386 US      A marvelou~ <NA>            91     9 Washing~ Columbi~
## 2 26101 US      With full ~ Winemaker'~     91     8 Washing~ Columbi~
## 3 34629 Portug~ Richly tan~ Toutalga        91     7 Alentej~ <NA>    
## 4 43977 US      Kudos to S~ Dry             91     9 Washing~ Columbi~
## 5 56988 US      This tangy~ Fumé Blanc      91     9 Washing~ Columbi~
## # ... with 6 more variables: region_2 <fct>, taster_name <fct>,
## #   taster_twitter_handle <chr>, title <chr>, variety <fct>, winery <fct>
```

## selekcja kolumn


```r
wine<-wine%>%select(-X1)
```

## Rzeczy naj - funkcja top_n


```r
wine%>%top_n(n=20,wt=-price)%>%
  arrange(-points)%>%
  select(title,country,points,price)
```

```
## # A tibble: 54 x 4
##    title                                              country points price
##    <chr>                                              <fct>    <int> <dbl>
##  1 In Situ 2008 Reserva Sauvignon Blanc (Aconcagua V~ Chile       87     5
##  2 Mancan NV Fizz Sparkling (California)              US          87     5
##  3 Anna Spinato NV Mini  (Prosecco)                   Italy       86     5
##  4 Bandit NV Merlot (California)                      US          86     4
##  5 Earth's Harvest 2013 Merlot (California)           US          86     5
##  6 Cramele Recas 2011 UnWineD Pinot Grigio (Viile Ti~ Romania     86     4
##  7 Felix Solis 2013 Flirty Bird Syrah (Vino de la Ti~ Spain       85     4
##  8 Earth's Harvest 2014 Organic Grapes Chardonnay (C~ US          85     5
##  9 Dancing Coyote 2015 White (Clarksburg)             US          85     4
## 10 Gallo Family Vineyards NV Twin Valley Pinot Grigi~ US          85     5
## # ... with 44 more rows
```

# top 5 win ponizej 30 zl


```r
wine%>%mutate(price_pln=price * 3.8)%>%
  filter(price_pln<30)%>%
  top_n(n=5,wt=points)%>%
  arrange(-points)%>%
  select(title,country,points,price)
```

```
## # A tibble: 19 x 4
##    title                                              country points price
##    <chr>                                              <fct>    <int> <dbl>
##  1 Herdade dos Machados 2012 Toutalga Red (Alentejan~ Portug~     91     7
##  2 Borsao 2008 Monte Oton Garnacha (Campo de Borja)   Spain       89     7
##  3 Hogue 2008 Red Table Wine Red (Columbia Valley (W~ US          89     7
##  4 Caves Velhas 2013 D. Fuas Reserva Red (Terras do ~ Portug~     89     7
##  5 Washington Hills 2006 Chardonnay (Washington)      US          88     7
##  6 Kirkland Signature 2009 Reserva  (Rioja)           Spain       88     7
##  7 Ste. Chapelle 2001 Johannisberg Riesling (Idaho)   US          88     6
##  8 Tagaris 2001 Johannisberg Riesling (Columbia Vall~ US          88     7
##  9 Ste. Chapelle 2001 Dry Riesling (Idaho)            US          88     6
## 10 Caves Primavera 2009 Primavera Branco White (Bair~ Portug~     88     7
## 11 Bridgeview 2000 Chardonnay (Oregon)                US          88     7
## 12 Kiona 1998 Late Harvest White Riesling (Yakima Va~ US          88     7
## 13 Kiona 1999 Late Harvest Muscat (Yakima Valley)     US          88     7
## 14 Ste. Chapelle 2001 Dry Gewürztraminer (Idaho)      US          88     6
## 15 Caves da Montanha 2011 A. Henriques Tinto Red (Ba~ Portug~     88     7
## 16 Caves da Montanha 2014 Trinca Espinhas Alvarinho-~ Portug~     88     7
## 17 Santa Ines 2000 Legado de Armida Reserva Chardonn~ Chile       88     6
## 18 Landshut 2013 Late Harvest Spätlese Riesling (Rhe~ Germany     88     6
## 19 The Ghost in the Machine 2011 Riesling (Columbia ~ US          88     7
```

## 5 najgorszych drogich win


```r
wine%>%
  top_n(n=100, wt=price)%>%
  arrange(points)
```

```
## # A tibble: 100 x 13
##    country description designation points price province region_1 region_2
##    <fct>   <chr>       <fct>        <int> <dbl> <fct>    <fct>    <fct>   
##  1 Portug~ Sweet, sur~ Colheita        87   790 Port     <NA>     <NA>    
##  2 France  Soft Pinot~ Morogues        87   800 Loire V~ Menetou~ <NA>    
##  3 France  This ripe ~ <NA>            88  3300 Bordeaux Médoc    <NA>    
##  4 Spain   Name and r~ Unico           89   500 Norther~ Ribera ~ <NA>    
##  5 Germany Zesty on t~ Hattenheim~     90   510 Rheingau <NA>     <NA>    
##  6 Austra~ This wine ~ Hill of Gr~     91   780 South A~ Eden Va~ <NA>    
##  7 US      The nose o~ Roger Rose~     91  2013 Califor~ Arroyo ~ Central~
##  8 Italy   Aromas of ~ Sori Tildin     92   500 Piedmont Langhe   <NA>    
##  9 US      This is a ~ Intrepid        92   750 Califor~ Paso Ro~ Central~
## 10 France  In its sil~ Blanc de B~     92   675 Champag~ Champag~ <NA>    
## # ... with 90 more rows, and 5 more variables: taster_name <fct>,
## #   taster_twitter_handle <chr>, title <chr>, variety <fct>, winery <fct>
```

```r
wine%>%mutate(cena_jakosc=price/points)%>%
  arrange(-cena_jakosc)
```

```
## # A tibble: 118,840 x 14
##    country description designation points price province region_1 region_2
##    <fct>   <chr>       <fct>        <int> <dbl> <fct>    <fct>    <fct>   
##  1 France  This ripe ~ <NA>            88  3300 Bordeaux Médoc    <NA>    
##  2 France  The wine i~ <NA>            96  2500 Bordeaux Pomerol  <NA>    
##  3 France  A superb w~ <NA>            96  2500 Burgundy La Roma~ <NA>    
##  4 US      The nose o~ Roger Rose~     91  2013 Califor~ Arroyo ~ Central~
##  5 France  A wonderfu~ <NA>            96  2000 Burgundy La Roma~ <NA>    
##  6 France  This extra~ <NA>            97  2000 Bordeaux Pomerol  <NA>    
##  7 France  A massive ~ <NA>            98  1900 Bordeaux Margaux  <NA>    
##  8 France  Almost bla~ <NA>           100  1500 Bordeaux Pauillac <NA>    
##  9 France  This is th~ <NA>           100  1500 Bordeaux Saint-É~ <NA>    
## 10 France  The purest~ <NA>            96  1300 Bordeaux Pauillac <NA>    
## # ... with 118,830 more rows, and 6 more variables: taster_name <fct>,
## #   taster_twitter_handle <chr>, title <chr>, variety <fct>, winery <fct>,
## #   cena_jakosc <dbl>
```

## agregacja i grupowanie

ktore panstwo ma srednio najlepsze wina?


```r
wine%>%group_by(country)%>%
  summarise(srednia=mean(points),
                 cena=mean(price, na.rm=T),
                 N=n())%>%
  arrange(-srednia)%>%
  filter(N>100)
```

```
## # A tibble: 18 x 4
##    country      srednia  cena     N
##    <fct>          <dbl> <dbl> <int>
##  1 Austria         90.1  31.4  3022
##  2 Germany         89.9  43.5  1990
##  3 Canada          89.3  35.8   226
##  4 Hungary         89.3  42.2   129
##  5 France          88.9  41.6 19739
##  6 Italy           88.6  40.0 17805
##  7 US              88.6  36.8 50229
##  8 Australia       88.5  35.4  2183
##  9 Israel          88.5  31.8   466
## 10 New Zealand     88.3  26.9  1276
## 11 Portugal        88.3  26.6  5222
## 12 South Africa    88.0  24.3  1301
## 13 Bulgaria        87.9  14.8   132
## 14 Spain           87.2  28.4  6021
## 15 Greece          87.2  22.3   429
## 16 Argentina       86.7  24.6  3543
## 17 Chile           86.5  20.9  4178
## 18 Romania         86.3  15.4   101
```

# ktory recenzent zrecenzowal najwiecej win? jka dal srednia ocene


```r
wine%>%
  group_by(taster_name)%>%
  summarise(N=n(),
         ocena=mean(points),
         cena_m=median(price,na.rm = T))%>%
  arrange(-N)
```

```
## # A tibble: 20 x 4
##    taster_name            N ocena cena_m
##    <fct>              <int> <dbl>  <dbl>
##  1 <NA>               24733  87.8   29  
##  2 Roger Voss         22962  88.7   22  
##  3 Michael Schachner  13944  86.9   17  
##  4 Kerin O’Keefe       9649  88.9   30  
##  5 Paul Gregutt        8849  89.1   29  
##  6 Virginie Boone      8674  89.2   39  
##  7 Matt Kettmann       5693  90.1   36  
##  8 Joe Czerwinski      4748  88.5   22  
##  9 Sean P. Sullivan    4442  88.8   30  
## 10 Anna Lee C. Iijima  4010  88.4   22  
## 11 Jim Gordon          3736  88.6   23  
## 12 Anne Krebiehl MW    3245  90.6   25  
## 13 Lauren Buzzeo       1700  87.7   18  
## 14 Susan Kostrzewa     1020  86.6   18  
## 15 Mike DeSimone        460  89.1   25  
## 16 Jeff Jenssen         436  88.3   17  
## 17 Alexander Peartree   383  85.8   25  
## 18 Carrie Dykes         127  86.4   28  
## 19 Fiona Adams           24  86.8   27.5
## 20 Christina Pickard      5  87.6   28
```

## rocznik wina


```r
wine%>%
  mutate(year=str_extract(title, pattern = "\\d{4}"))
```

```
## # A tibble: 118,840 x 14
##    country description designation points price province region_1 region_2
##    <fct>   <chr>       <fct>        <int> <dbl> <fct>    <fct>    <fct>   
##  1 Italy   Aromas inc~ Vulka Bian~     87    NA Sicily ~ Etna     <NA>    
##  2 Portug~ This is ri~ Avidagos        87    15 Douro    <NA>     <NA>    
##  3 US      Tart and s~ <NA>            87    14 Oregon   Willame~ Willame~
##  4 US      Pineapple ~ Reserve La~     87    13 Michigan Lake Mi~ <NA>    
##  5 US      Much like ~ Vintner's ~     87    65 Oregon   Willame~ Willame~
##  6 Spain   Blackberry~ Ars In Vit~     87    15 Norther~ Navarra  <NA>    
##  7 Italy   Here's a b~ Belsito         87    16 Sicily ~ Vittoria <NA>    
##  8 France  This dry a~ <NA>            87    24 Alsace   Alsace   <NA>    
##  9 Germany Savory dri~ Shine           87    12 Rheinhe~ <NA>     <NA>    
## 10 France  This has g~ Les Natures     87    27 Alsace   Alsace   <NA>    
## # ... with 118,830 more rows, and 6 more variables: taster_name <fct>,
## #   taster_twitter_handle <chr>, title <chr>, variety <fct>, winery <fct>,
## #   year <chr>
```

## wybor 15 krajow


```r
wine.top15<-wine%>%
  count(country)%>%
  top_n(n=15,n)%>%
  select(-n)%>%
  inner_join(wine)
```

```
## Joining, by = "country"
```






















