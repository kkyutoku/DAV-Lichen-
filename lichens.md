---
title: "Lichens"
author: "Karen Kyutoku"
date: "2022/3/9"
output: 
  html_document: 
    keep_md: yes
---




```r
#install.packages("tidyverse")
#install.packages("here")
#install.packages("janitor")
#install.packages("ggthemes")
#install.packages("RColorBrewer")
#install.packages("paletteer")
#install.packages("skimr")
#install.packages("nanitor")
```

```r
library(tidyverse)
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
```

```
## v ggplot2 3.3.5     v purrr   0.3.4
## v tibble  3.1.6     v dplyr   1.0.8
## v tidyr   1.2.0     v stringr 1.4.0
## v readr   2.1.2     v forcats 0.5.1
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(here)
```

```
## here() starts at C:/Users/KAREN/Documents/DAV-Lichens/data
```

```r
library(janitor)
```

```
## 
##  次のパッケージを付け加えます: 'janitor'
```

```
##  以下のオブジェクトは 'package:stats' からマスクされています: 
## 
##      chisq.test, fisher.test
```

```r
library(ggthemes)
library(RColorBrewer)
library(paletteer)
library(skimr)
library(naniar)
```

```
## 
##  次のパッケージを付け加えます: 'naniar'
```

```
##  以下のオブジェクトは 'package:skimr' からマスクされています: 
## 
##      n_complete
```


```r
getwd()
```

```
## [1] "C:/Users/KAREN/Documents/DAV-Lichens/data"
```


```r
lichen_data <- read_csv(here("occurrences.csv")) %>% clean_names()
```

```
## Warning: One or more parsing issues, see `problems()` for details
```

```
## Rows: 1628 Columns: 91
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr  (49): institutionCode, collectionCode, basisOfRecord, occurrenceID, cat...
## dbl  (13): id, taxonID, year, month, day, startDayOfYear, localitySecurity, ...
## lgl  (28): ownerInstitutionCode, otherCatalogNumbers, taxonRemarks, typeStat...
## date  (1): eventDate
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
lichen_data
```

```
## # A tibble: 1,628 x 91
##         id institution_code collection_code owner_institution_c~ basis_of_record
##      <dbl> <chr>            <chr>           <lgl>                <chr>          
##  1 3314688 DAV              Lichens         NA                   PreservedSpeci~
##  2 3314689 DAV              Lichens         NA                   PreservedSpeci~
##  3 3314690 DAV              Lichens         NA                   PreservedSpeci~
##  4 3314691 DAV              Lichens         NA                   PreservedSpeci~
##  5 3314692 DAV              Lichens         NA                   PreservedSpeci~
##  6 3314693 DAV              Lichens         NA                   PreservedSpeci~
##  7 3314694 DAV              Lichens         NA                   PreservedSpeci~
##  8 3314695 DAV              Lichens         NA                   PreservedSpeci~
##  9 3314696 DAV              Lichens         NA                   PreservedSpeci~
## 10 3314697 DAV              Lichens         NA                   PreservedSpeci~
## # ... with 1,618 more rows, and 86 more variables: occurrence_id <chr>,
## #   catalog_number <chr>, other_catalog_numbers <lgl>,
## #   higher_classification <chr>, kingdom <chr>, phylum <chr>, class <chr>,
## #   order <chr>, family <chr>, scientific_name <chr>, taxon_id <dbl>,
## #   scientific_name_authorship <chr>, genus <chr>, subgenus <chr>,
## #   specific_epithet <chr>, verbatim_taxon_rank <chr>,
## #   infraspecific_epithet <chr>, taxon_rank <chr>, identified_by <chr>, ...
```

```r
lichens <- lichen_data %>% 
 select("class", "family", "scientific_name", "genus", "year", "recorded_by", "country", "state_province", "county")
lichens
```

```
## # A tibble: 1,628 x 9
##    class   family scientific_name genus  year recorded_by country state_province
##    <chr>   <chr>  <chr>           <chr> <dbl> <chr>       <chr>   <chr>         
##  1 Lecano~ Colle~ Collema nigres~ Coll~  2017 Beth Beyer  United~ California    
##  2 Lecano~ Clado~ Cladonia squam~ Clad~  2017 Benjamin M~ USA     California    
##  3 Lecano~ Clado~ Cladonia squam~ Clad~  2017 Lana Sayka~ USA     California    
##  4 Lecano~ Parme~ Evernia prunas~ Ever~  2017 Emma Hanse~ USA     California    
##  5 Lecano~ Parme~ Evernia prunas~ Ever~  2017 Lana Sayka~ USA     California    
##  6 Lecano~ Parme~ Flavoparmelia ~ Flav~  2017 Allie Weill USA     California    
##  7 Lecano~ Colle~ Collema nigres~ Coll~  2017 Beth Beyer  USA     California    
##  8 Lecano~ Parme~ Flavopunctelia~ Flav~  2017 Beth Beyer  USA     California    
##  9 Lecano~ Parme~ Flavopunctelia~ Flav~  2017 Emma Hanse~ USA     California    
## 10 Lecano~ Parme~ Flavopunctelia~ Flav~  2017 Benajmin M~ USA     California    
## # ... with 1,618 more rows, and 1 more variable: county <chr>
```

```r
lichens %>% 
  count(scientific_name) %>% 
  arrange(desc(n))
```

```
## # A tibble: 762 x 2
##    scientific_name               n
##    <chr>                     <int>
##  1 <NA>                         55
##  2 Flavoparmelia caperata       19
##  3 Evernia prunastri            16
##  4 Ramalina menziesii           16
##  5 Ramalina farinacea           15
##  6 Flavopunctelia flaventior    14
##  7 Hypogymnia physodes          14
##  8 Xanthoria polycarpa          14
##  9 Letharia vulpina             13
## 10 Parmelia sulcata             13
## # ... with 752 more rows
```

```r
lichens %>% 
  group_by(year) %>% 
  arrange()
```

```
## # A tibble: 1,628 x 9
## # Groups:   year [73]
##    class   family scientific_name genus  year recorded_by country state_province
##    <chr>   <chr>  <chr>           <chr> <dbl> <chr>       <chr>   <chr>         
##  1 Lecano~ Colle~ Collema nigres~ Coll~  2017 Beth Beyer  United~ California    
##  2 Lecano~ Clado~ Cladonia squam~ Clad~  2017 Benjamin M~ USA     California    
##  3 Lecano~ Clado~ Cladonia squam~ Clad~  2017 Lana Sayka~ USA     California    
##  4 Lecano~ Parme~ Evernia prunas~ Ever~  2017 Emma Hanse~ USA     California    
##  5 Lecano~ Parme~ Evernia prunas~ Ever~  2017 Lana Sayka~ USA     California    
##  6 Lecano~ Parme~ Flavoparmelia ~ Flav~  2017 Allie Weill USA     California    
##  7 Lecano~ Colle~ Collema nigres~ Coll~  2017 Beth Beyer  USA     California    
##  8 Lecano~ Parme~ Flavopunctelia~ Flav~  2017 Beth Beyer  USA     California    
##  9 Lecano~ Parme~ Flavopunctelia~ Flav~  2017 Emma Hanse~ USA     California    
## 10 Lecano~ Parme~ Flavopunctelia~ Flav~  2017 Benajmin M~ USA     California    
## # ... with 1,618 more rows, and 1 more variable: county <chr>
```


```r
lichens %>% 
  count(year) %>% 
  arrange(desc(n))
```

```
## # A tibble: 73 x 2
##     year     n
##    <dbl> <int>
##  1  1975   175
##  2  1971   158
##  3  1979   108
##  4  2017   107
##  5  1970    95
##  6  1966    82
##  7  1978    72
##  8  2001    59
##  9  1962    50
## 10  1969    50
## # ... with 63 more rows
```
1975, 1971, 1979, 2017 = over 100 lichens are collected


```r
lichens %>% 
  filter(year=="1975") %>% 
  count(recorded_by) %>% 
  arrange(desc(n))
```

```
## # A tibble: 15 x 2
##    recorded_by            n
##    <chr>              <int>
##  1 D. Showers            79
##  2 W. A. Weber           78
##  3 Shirley Tucker         3
##  4 David Showers          2
##  5 Halling                2
##  6 Shirley C. Tucker      2
##  7 Beecher Crampton       1
##  8 D. Toren               1
##  9 G. L. Webster          1
## 10 J. Campbell            1
## 11 Joseph M. DiTomaso     1
## 12 June McCaskill         1
## 13 M. A. Weber            1
## 14 Unknown                1
## 15 <NA>                   1
```
Major contributers in 1975 = D. Showers & W. A. Weber


```r
lichens %>% 
  filter(year=="1971") %>% 
  count(recorded_by) %>% 
  arrange(desc(n))
```

```
## # A tibble: 14 x 2
##    recorded_by             n
##    <chr>               <int>
##  1 Richard A. Bernhard   100
##  2 D. S. Horning          16
##  3 Fiona M. Pushman       12
##  4 Dr. He Kon Oh           5
##  5 P. M. Johns             5
##  6 A. W. Wilson            3
##  7 H. A. Best              3
##  8 Hugh A. Best            3
##  9 C. D. Meurk             2
## 10 C. J. Horning           2
## 11 Francis Rose            2
## 12 J. L. Burnip            2
## 13 Margaret E. Best        2
## 14 Margaret E. Skeel       1
```
Major contributer in 1971 = Richard A. Bernhard (mostly collected in the United Kingdom)


```r
lichens %>% 
  filter(year=="1979") %>% 
  count(recorded_by) %>% 
  arrange(desc(n))
```

```
## # A tibble: 4 x 2
##   recorded_by           n
##   <chr>             <int>
## 1 G. H. Snodgrass     104
## 2 D. T. Kowalski        2
## 3 Shirley C. Tucker     1
## 4 Shirley Tucker        1
```
Major contributer in 1979 = G. H. Snodgrass


```r
lichens %>% 
  filter(year=="2017") %>% 
  count(recorded_by) %>% 
  arrange(desc(n))
```

```
## # A tibble: 12 x 2
##    recorded_by           n
##    <chr>             <int>
##  1 Christopher Adlam    27
##  2 Allie Weill          16
##  3 Benjamin Maples       9
##  4 Beth Beyer            9
##  5 Emma Hansen-Smith     9
##  6 Tobias Mueller        9
##  7 Caroline Kneff        7
##  8 Lana Saykali          7
##  9 Brennen Dyer          5
## 10 Ellen Dean            4
## 11 Caroline Knef         3
## 12 Benajmin Maples       2
```
Major contributers in 2017 = Christopher Adlam & Alliee Weill


```r
lichens %>% 
  filter(year=="1966") %>% 
  count(recorded_by) %>% 
  arrange(desc(n))
```

```
## # A tibble: 11 x 2
##    recorded_by           n
##    <chr>             <int>
##  1 Shirley Tucker       44
##  2 Shirley C. Tucker    20
##  3 Roman Gankin          7
##  4 Hugh Dale             3
##  5 June McCaskill        2
##  6 F. Parker             1
##  7 John Tucker           1
##  8 Nancy Dengler         1
##  9 Ron Dengler           1
## 10 Ronald Dengler        1
## 11 Walter Russell        1
```



```r
lichens %>% 
  filter(state_province=="California") %>% 
  count(year) %>% 
  arrange(desc(n))
```

```
## # A tibble: 45 x 2
##     year     n
##    <dbl> <int>
##  1  1975   169
##  2  2017   107
##  3  1979   106
##  4  1966    81
##  5  1978    70
##  6  2000    46
##  7  1996    24
##  8  1969    21
##  9  1965    19
## 10  1997    19
## # ... with 35 more rows
```



```r
lichens %>% 
  filter(state_province=="California") %>% 
  group_by(year) %>% 
  summarize(n=n()) %>% 
  ggplot(aes(x=year, y=n))+
  geom_line()+
  theme(axis.text.x = element_text(angle= 60, hjust=1))+
  labs(title = "Number of DAV lichens collected in California",
       x="Year",
       y="Total number of lichens")
```

```
## Warning: Removed 1 row(s) containing missing values (geom_path).
```

![](lichens_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

```r
lichens %>% 
  group_by(year) %>% 
  summarize(n=n()) %>% 
  ggplot(aes(x=year, y=n))+
  geom_line()+
  theme(axis.text.x = element_text(angle= 60, hjust=1))+
  labs(title = "DAV Lichens (1908-2002)",
       x="Year",
       y="Total number of lichens")
```

```
## Warning: Removed 1 row(s) containing missing values (geom_path).
```

![](lichens_files/figure-html/unnamed-chunk-17-1.png)<!-- -->


```r
lichens %>% 
  count(genus) %>% 
  arrange(desc(n))
```

```
## # A tibble: 194 x 2
##    genus           n
##    <chr>       <int>
##  1 Cladonia      104
##  2 <NA>          100
##  3 Ramalina       73
##  4 Usnea          64
##  5 Hypogymnia     50
##  6 Lecanora       49
##  7 Xanthoria      41
##  8 Parmelia       39
##  9 Umbilicaria    36
## 10 Lecidea        33
## # ... with 184 more rows
```

```r
lichens <- lichens %>% 
  na_if("Lichen") %>% 
  mutate(family=na_if(family, ("Lichen")))
lichens
```

```
## # A tibble: 1,628 x 9
##    class   family scientific_name genus  year recorded_by country state_province
##    <chr>   <chr>  <chr>           <chr> <dbl> <chr>       <chr>   <chr>         
##  1 Lecano~ Colle~ Collema nigres~ Coll~  2017 Beth Beyer  United~ California    
##  2 Lecano~ Clado~ Cladonia squam~ Clad~  2017 Benjamin M~ USA     California    
##  3 Lecano~ Clado~ Cladonia squam~ Clad~  2017 Lana Sayka~ USA     California    
##  4 Lecano~ Parme~ Evernia prunas~ Ever~  2017 Emma Hanse~ USA     California    
##  5 Lecano~ Parme~ Evernia prunas~ Ever~  2017 Lana Sayka~ USA     California    
##  6 Lecano~ Parme~ Flavoparmelia ~ Flav~  2017 Allie Weill USA     California    
##  7 Lecano~ Colle~ Collema nigres~ Coll~  2017 Beth Beyer  USA     California    
##  8 Lecano~ Parme~ Flavopunctelia~ Flav~  2017 Beth Beyer  USA     California    
##  9 Lecano~ Parme~ Flavopunctelia~ Flav~  2017 Emma Hanse~ USA     California    
## 10 Lecano~ Parme~ Flavopunctelia~ Flav~  2017 Benajmin M~ USA     California    
## # ... with 1,618 more rows, and 1 more variable: county <chr>
```

```r
naniar::miss_var_summary(lichens)
```

```
## # A tibble: 9 x 3
##   variable        n_miss pct_miss
##   <chr>            <int>    <dbl>
## 1 county             288   17.7  
## 2 state_province     275   16.9  
## 3 family             176   10.8  
## 4 genus              101    6.20 
## 5 class               83    5.10 
## 6 scientific_name     56    3.44 
## 7 year                34    2.09 
## 8 country             12    0.737
## 9 recorded_by          6    0.369
```


```r
lichens %>% 
  filter(state_province == "California") %>% 
  count(family) %>% 
  arrange(desc(n))
```

```
## # A tibble: 51 x 2
##    family              n
##    <chr>           <int>
##  1 Parmeliaceae      220
##  2 <NA>               76
##  3 Ramalinaceae       60
##  4 Teloschistaceae    59
##  5 Physciaceae        52
##  6 Lecanoraceae       45
##  7 Cladoniaceae       37
##  8 Collemataceae      29
##  9 Caliciaceae        26
## 10 Verrucariaceae     21
## # ... with 41 more rows
```


```r
lichens %>% 
  filter(state_province=="California") %>% 
  ggplot(aes(x= year, fill= family))+
  geom_bar(position="dodge")+
  coord_flip()+
  labs(title = "Observations by Family in DAV Lichen Data",
       x = "Year",
       y = "Total",
       fill = "Family")
```

```
## Warning: Removed 17 rows containing non-finite values (stat_count).
```

![](lichens_files/figure-html/unnamed-chunk-22-1.png)<!-- -->


```r
lichens %>% 
  filter(state_province=="California") %>% 
  ggplot(aes(x=year, y=family, fill= family))+
  geom_boxplot()
```

```
## Warning: Removed 17 rows containing non-finite values (stat_boxplot).
```

![](lichens_files/figure-html/unnamed-chunk-23-1.png)<!-- -->


```r
lichens %>% 
  filter(state_province=="California") %>% 
  ggplot(aes(x= year, fill= class))+
  geom_bar(position="dodge")+
  labs(title = "Observations by Class in DAV Lichen Data",
       x = "Year",
       y = "Total",
       fill = "class")
```

```
## Warning: Removed 17 rows containing non-finite values (stat_count).
```

![](lichens_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

```r
lichens %>% 
  count(country) %>% 
  arrange(desc(n))
```

```
## # A tibble: 21 x 2
##    country            n
##    <chr>          <int>
##  1 United States   1185
##  2 New Zealand      115
##  3 United Kingdom   114
##  4 USA              100
##  5 Mexico            45
##  6 Canada            19
##  7 <NA>              12
##  8 Ecuador           11
##  9 Cayman Islands     6
## 10 Iceland            5
## # ... with 11 more rows
```

```r
lichens %>% 
  filter(state_province=="California") %>% 
  count(county) %>% 
  arrange(desc(n))
```

```
## # A tibble: 49 x 2
##    county              n
##    <chr>           <int>
##  1 Napa              113
##  2 Solano             83
##  3 Mendocino          82
##  4 Marin              64
##  5 Santa Barbara      48
##  6 San Mateo          46
##  7 Tehama             34
##  8 Butte              29
##  9 Alpine             26
## 10 San Luis Obispo    24
## # ... with 39 more rows
```


```r
lichens %>% 
  filter(state_province=="California") %>% 
  ggplot(aes(x= county, fill= county))+
  geom_bar(position="dodge")+
  coord_flip()+
  labs(title = "Number of Lichens Collected from Different Counties in California",
       x = "County",
       y = "Number of Lichens")
```

![](lichens_files/figure-html/unnamed-chunk-27-1.png)<!-- -->


```r
berkeley_lichen <- read_csv(here("occurrences_berkeley.csv")) %>% clean_names()
```

```
## Warning: One or more parsing issues, see `problems()` for details
```

```
## Rows: 55364 Columns: 91
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr  (38): institutionCode, basisOfRecord, occurrenceID, catalogNumber, high...
## dbl  (12): id, taxonID, year, month, day, startDayOfYear, endDayOfYear, cult...
## lgl  (39): collectionCode, ownerInstitutionCode, otherCatalogNumbers, identi...
## dttm  (1): modified
## date  (1): eventDate
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```


```r
berkeley_lichen %>% 
  filter(state_province=="California") %>% 
  group_by(year) %>% 
  summarize(n=n()) %>% 
  ggplot(aes(x=year, y=n))+
  geom_line()+
  theme(axis.text.x = element_text(angle= 60, hjust=1))+
  labs(title = "Number of UC lichens collected in California",
       x="Year",
       y="Total number of lichens")
```

```
## Warning: Removed 1 row(s) containing missing values (geom_path).
```

![](lichens_files/figure-html/unnamed-chunk-29-1.png)<!-- -->


