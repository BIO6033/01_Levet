---
title: "Project1_Manip_Data"
author: "Marie Levet"
date: "26/09/2019"
output: 
  html_document:
   keep_md: true
editor_options: 
  chunk_output_type: console
---



Import dataset into R studio 

Downloaded the csv file on Montreal city website 
Saved and placed within the "01_Levet" folder on the desktop


```r
library (tidyr) # restructuring the data
library (dplyr) # doing calculation with the data 
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library (ggplot2) # plot/visualise the data
library (magrittr) # to use the pipping function 
```

```
## 
## Attaching package: 'magrittr'
```

```
## The following object is masked from 'package:tidyr':
## 
##     extract
```

Exploring and restructuring the data 


```r
#Exploring the data
punaise<-readr::read_csv("Punaise_data.csv")
```

```
## Parsed with column specification:
## cols(
##   NO_DECLARATION = col_double(),
##   DATE_DECLARATION = col_datetime(format = ""),
##   DATE_INSP_VISPRE = col_character(),
##   NBR_EXTERMIN = col_double(),
##   DATE_DEBUTTRAIT = col_character(),
##   DATE_FINTRAIT = col_character(),
##   No_QR = col_character(),
##   NOM_QR = col_character(),
##   NOM_ARROND = col_character(),
##   COORD_X = col_double(),
##   COORD_Y = col_double(),
##   LONGITUDE = col_double(),
##   LATITUDE = col_double()
## )
```

```r
#view(punaise) to visualise the dataset
glimpse(punaise) # to see the first few rows of the data
```

```
## Observations: 35,363
## Variables: 13
## $ NO_DECLARATION   <dbl> 4254, 830, 1380, 455, 1243, 4331, 131, 2392, ...
## $ DATE_DECLARATION <dttm> 2012-10-28 16:36:04, 2011-09-16 09:45:58, 20...
## $ DATE_INSP_VISPRE <chr> "9/21/2012", "7/13/2011", "11/2/2011", "8/9/2...
## $ NBR_EXTERMIN     <dbl> 1, 1, 1, 1, 1, NA, 2, 1, 1, 2, 2, 1, 1, 1, 1,...
## $ DATE_DEBUTTRAIT  <chr> "9/21/2012", "7/27/2011", "11/7/2011", "8/9/2...
## $ DATE_FINTRAIT    <chr> "9/21/2012", "8/17/2011", "11/21/2011", "8/9/...
## $ No_QR            <chr> "24", "50", "30", "44", "19", "20", "33", "10...
## $ NOM_QR           <chr> "Beaurivage", "Saint-Henri", "Sainte-Marie", ...
## $ NOM_ARROND       <chr> "Mercier–Hochelaga-Maisonneuve", "Le Sud-Oues...
## $ COORD_X          <dbl> 303753.6, 298119.8, 300294.9, 296046.9, 29952...
## $ COORD_Y          <dbl> 5049836, 5036964, 5042372, 5036495, 5045640, ...
## $ LONGITUDE        <dbl> -73.51341, -73.58544, -73.55767, -73.61194, -...
## $ LATITUDE         <dbl> 45.58843, 45.47257, 45.52125, 45.46833, 45.55...
```

```r
summary(punaise) # to get an idea of the stats on the dataset 
```

```
##  NO_DECLARATION  DATE_DECLARATION              DATE_INSP_VISPRE  
##  Min.   :  104   Min.   :2011-07-05 20:24:56   Length:35363      
##  1st Qu.: 9106   1st Qu.:2013-10-11 00:32:23   Class :character  
##  Median :18120   Median :2015-07-28 19:58:27   Mode  :character  
##  Mean   :18138   Mean   :2015-07-27 11:50:52                     
##  3rd Qu.:27164   3rd Qu.:2017-05-30 13:49:19                     
##  Max.   :36154   Max.   :2019-06-29 17:59:29                     
##                                                                  
##   NBR_EXTERMIN   DATE_DEBUTTRAIT    DATE_FINTRAIT         No_QR          
##  Min.   :1.000   Length:35363       Length:35363       Length:35363      
##  1st Qu.:1.000   Class :character   Class :character   Class :character  
##  Median :1.000   Mode  :character   Mode  :character   Mode  :character  
##  Mean   :1.511                                                           
##  3rd Qu.:2.000                                                           
##  Max.   :4.000                                                           
##  NA's   :2297                                                            
##     NOM_QR           NOM_ARROND           COORD_X          COORD_Y       
##  Length:35363       Length:35363       Min.   :274266   Min.   :5030733  
##  Class :character   Class :character   1st Qu.:294884   1st Qu.:5041978  
##  Mode  :character   Mode  :character   Median :297707   Median :5044165  
##                                        Mean   :297113   Mean   :5044153  
##                                        3rd Qu.:299709   3rd Qu.:5046513  
##                                        Max.   :306022   Max.   :5062064  
##                                                                          
##    LONGITUDE         LATITUDE    
##  Min.   :-73.89   Min.   :45.42  
##  1st Qu.:-73.63   1st Qu.:45.52  
##  Median :-73.59   Median :45.54  
##  Mean   :-73.60   Mean   :45.54  
##  3rd Qu.:-73.57   3rd Qu.:45.56  
##  Max.   :-73.48   Max.   :45.70  
## 
```


```r
punaise_clean <- punaise[complete.cases(punaise),] # removing NA's from the dataset
punaise_clean <-select(punaise_clean, -COORD_X, -COORD_Y, -LONGITUDE, -LATITUDE) # create a subtract of dataset with columns of interest 
# view(punaise_clean) double check that the NA's are gone and the selection is made
```

Plotting the data 


```r
# plotting the number of declarations per arrondissement to see the differences between area of the city 
punaise_clean %>%
  mutate(compte_declaration = NO_DECLARATION/NO_DECLARATION)%>% #Valeur de 1 pour chaque déclaration
  ggplot(aes(y = compte_declaration, x=NOM_ARROND)) + 
  geom_col(position = position_stack(reverse = TRUE)) + #Inversion du tableau pour meilleure lecture
  coord_flip()+
  theme_classic()+ #Pour mieux voir
  ylab ("Nombre de déclarations") + xlab ("Arrondissement")+
  theme(axis.title.x = element_text(colour = "purple", size = rel(1.5)))+
  theme(axis.title.y = element_text(colour="purple", size=rel(1.5)))
```

![](Project1__files/figure-html/plotting_number_of_declarations-1.png)<!-- -->


```r
# plotting the number of exterminations per arrondissements
punaise_clean %>%
  ggplot(aes (y = NBR_EXTERMIN, x = NOM_ARROND)) +
  coord_flip()+
  geom_col() + 
  theme_classic() +
  ylab ("Nombre d'extermination") + xlab ("Arrondissement")+
  theme(axis.title.x = element_text(colour = "purple", size = rel(1.5)))+
  theme(axis.title.y = element_text(colour="purple", size=rel(1.5)))
```

![](Project1__files/figure-html/plotting_number_of_extermination-1.png)<!-- -->


```r
# plotting median of the number of times the exterminator had to come back
ggplot(punaise_clean, aes (x = NBR_EXTERMIN, y = NOM_ARROND)) +
  geom_boxplot() + 
  theme_classic() +
  xlab ("Nombre d'extermination") + ylab ("Arrondissement")+
  theme(axis.title.x = element_text(colour = "purple", size = rel(1.5)))+
  theme(axis.title.y = element_text(colour="purple", size=rel(1.5)))
```

![](Project1__files/figure-html/unnamed-chunk-1-1.png)<!-- -->
