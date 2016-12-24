Allstate data overview
================
LipingLi
2016.12.23

### Important data

``` r
library(data.table)
train = fread("C:/data/train-allstate.csv", sep = ",", stringsAsFactors = T)
```

    ## 
    Read 42.5% of 188318 rows
    Read 74.3% of 188318 rows
    Read 188318 rows and 132 (of 132) columns from 0.065 GB file in 00:00:05

``` r
summary(train)
```

    ##        id         cat1       cat2       cat3       cat4       cat5      
    ##  Min.   :     1   A:141550   A:106721   A:177993   A:128395   A:123737  
    ##  1st Qu.:147748   B: 46768   B: 81597   B: 10325   B: 59923   B: 64581  
    ##  Median :294540                                                         
    ##  Mean   :294136                                                         
    ##  3rd Qu.:440681                                                         
    ##  Max.   :587633                                                         
    ##  
    ##  hidden for save space
    ##  
    ##  cat113          cat114           cat115          cat116  
    ##  BM     :26191   A      :131693   K      :43866   HK     : 21061  
    ##  AE     :22030   C      : 16793   O      :26813   DJ     : 20244  
    ##  L      :13058   E      : 16475   J      :23895   CK     : 10162  
    ##  AX     :12661   J      :  8199   N      :22438   DP     :  9202  
    ##  Y      :11374   F      :  7905   P      :21538   GS     :  8736  
    ##  K      : 7738   N      :  2455   L      :16125   CR     :  6862  
    ##  (Other):95266   (Other):  4798   (Other):33643   (Other):112051  
    ##      cont1              cont2              cont3              cont4       
    ##  Min.   :0.000016   Min.   :0.001149   Min.   :0.002634   Min.   :0.1769  
    ##  1st Qu.:0.346090   1st Qu.:0.358319   1st Qu.:0.336963   1st Qu.:0.3274  
    ##  Median :0.475784   Median :0.555782   Median :0.527991   Median :0.4529  
    ##  Mean   :0.493861   Mean   :0.507188   Mean   :0.498918   Mean   :0.4918  
    ##  3rd Qu.:0.623912   3rd Qu.:0.681761   3rd Qu.:0.634224   3rd Qu.:0.6521  
    ##  Max.   :0.984975   Max.   :0.862654   Max.   :0.944251   Max.   :0.9543  
    ##                                                                           
    ##      cont5            cont6             cont7            cont8       
    ##  Min.   :0.2811   Min.   :0.01268   Min.   :0.0695   Min.   :0.2369  
    ##  1st Qu.:0.2811   1st Qu.:0.33610   1st Qu.:0.3502   1st Qu.:0.3128  
    ##  Median :0.4223   Median :0.44094   Median :0.4383   Median :0.4411  
    ##  Mean   :0.4874   Mean   :0.49094   Mean   :0.4850   Mean   :0.4864  
    ##  3rd Qu.:0.6433   3rd Qu.:0.65502   3rd Qu.:0.5910   3rd Qu.:0.6236  
    ##  Max.   :0.9837   Max.   :0.99716   Max.   :1.0000   Max.   :0.9802  
    ##                                                                      
    ##      cont9             cont10           cont11            cont12       
    ##  Min.   :0.00008   Min.   :0.0000   Min.   :0.03532   Min.   :0.03623  
    ##  1st Qu.:0.35897   1st Qu.:0.3646   1st Qu.:0.31096   1st Qu.:0.31166  
    ##  Median :0.44145   Median :0.4612   Median :0.45720   Median :0.46229  
    ##  Mean   :0.48551   Mean   :0.4981   Mean   :0.49351   Mean   :0.49315  
    ##  3rd Qu.:0.56682   3rd Qu.:0.6146   3rd Qu.:0.67892   3rd Qu.:0.67576  
    ##  Max.   :0.99540   Max.   :0.9950   Max.   :0.99874   Max.   :0.99848  
    ##                                                                        
    ##      cont13             cont14            loss          
    ##  Min.   :0.000228   Min.   :0.1797   Min.   :     0.67  
    ##  1st Qu.:0.315758   1st Qu.:0.2946   1st Qu.:  1204.46  
    ##  Median :0.363547   Median :0.4074   Median :  2115.57  
    ##  Mean   :0.493138   Mean   :0.4957   Mean   :  3037.34  
    ##  3rd Qu.:0.689974   3rd Qu.:0.7246   3rd Qu.:  3864.05  
    ##  Max.   :0.988494   Max.   :0.8448   Max.   :121012.25  
    ## 

188318 rows and 132 columns. 116 categorical variables and 14 continous variables. The ture name of independent variables are unknown, thus we cannot engineer features by means of domain knowledge.

### Distribution of continuous variable

``` r
conts = train[, c(118:131), with = F]
library(psych)
multi.hist(conts)
```

![](allstate-data-overview_files/figure-markdown_github/unnamed-chunk-2-1.png)

These variables are standardized to 0-1 range, but not normally distributed.

``` r
library(ggplot2)
library(GGally)
ggcorr(conts, label = T)
```

![](allstate-data-overview_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
rm(conts)  #to save space
```

Some variables are highly correlated to others. What about categorical variables?

### Correlations between categorical variables

``` r
# # This part runs for dozens of minutes, so I annotated it
#
# c=rep(1,(116*116))
# rec=matrix(c,nrow=116) #create a chi-square p-value matrix
# cat=train[,c(2:117),with=F] 
# cat=data.frame(cat) 
#
# # using for loop to get pairwise chi-square test
# for (i in 1:115)
# {
#     for (j in (i+1):116) 
#     {
#         rec[i,j]=chisq.test(cat[,i],cat[,j])$p.value 
#     } 
# }
# 
# for (i in 1:115) 
# {
#     for (j in (i+1):116)
#     {
#         if(rec[i,j]<=0.01)
#         cat('Chi-square test [cat',i,', cat',j,'] <0.01\n') 
#         # This if control flow can be inserted into chi-square test part, but I'm thinking to build functions supporting different p-level.
#     }
# } 
# rm(cat,rec,c,i,j) #remove non-necessaries to save space
```

A great number of categorical varibles are not independent from each other.

### Distribution of dependent variable

``` r
loss = train[, 132, with = F]
loss = data.frame(loss)
b = qplot(loss, data = loss, geom = "histogram", binwidth = 10)
b
```

![](allstate-data-overview_files/figure-markdown_github/unnamed-chunk-5-1.png)

Loss is not normally distributed. Since money issues often exponentially distributed, we can take log.

``` r
c = qplot(log(loss), data = loss, geom = "histogram", binwidth = 0.01)
c
```

![](allstate-data-overview_files/figure-markdown_github/unnamed-chunk-6-1.png)

Much better, but still have long-tail outliers. We can take a shift and log it.

``` r
d = qplot(log(loss + 200), data = loss, geom = "histogram", binwidth = 0.01)
d
```

![](allstate-data-overview_files/figure-markdown_github/unnamed-chunk-7-1.png)

Change the number for shift may result in even better normality. Here we adopt 200.
