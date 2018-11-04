---
title: "White and Red Wine EDA by Mithil Viradia"
output:
  html_document: 
    keep_md: yes
    theme: spacelab
  pdf_document: default
  word_document: default
---

```r
library(ggplot2)
library(GGally)


wineWhite = read.csv('wineQuality-White.csv',sep = ';')
wineRed = read.csv('wineQuality-red.csv', sep = ';')

#made a new categorical variable, color
wineWhite['color'] = 'white'
wineRed['color'] = 'red'

#combine both the White wind and Red wine datasets
df = rbind(wineRed,wineWhite)

#set the ggplot grid style
my_style = theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank())
```

http://winefolly.com/review/sugar-in-wine-chart/

http://www.winespectator.com/drvinny/show/id/5035


# About the Data Set
Both of the data sets have the same variables

- Fixed Acidity(g/dm^3) - measure of tartic acid, having low acidity will result in a flat taste whereas too much will result in tartness. So, the plot for quailty vs Fixed Acidity should be  a bell curve.

- Volatile Acidity(g/dm^3) - meaure of acetic acid, produced through microbial action so too much will lead to spoilage. Wine makers try to make supress volatile acidity as much as they can.

- Citric Acid(g/dm^3) - small amounts lead to nice tangy, and fresh taste. 

- Residual Sugar(g/dm^3) - amount of sugar left, <6 g/dm^3 will lead to a dry taste and >21 g/dm^3 will lead to a sweet taste

- Chlorides(g/dm^3) - salt in wine

- Free Sulfur Dioxide(mg/dm^3) - prevents microbial growth/oxidation, I think that Votatile Acidity and Free Sulfur Dioxide will have an inverse realtionship beacuse since volatile acidity is produced through microbes and SO2 prevents microbial activity

- Total Sulfur Dioxide(mg/dm^3) - amount of free and bound SO2

- Density(g/cm^3) - %sugar to %alcohol, high sugar leads to higher density, high alcohol leads to lower density

- pH(1-14) - used to measure ripeness to acidity, most wines will have a pH from 3 to 4. So, the best whites will have pH(3-3.4) whereas best reds will have pH(3.3-3.6). Low pH will lead to a tart taste while high pH will lead to a bacterial growth.

- Sulphates(g/dm^3) - acts as a microbial deterrent

- Alcohol(% by volume) - % alcohol, measure of how much the fermentation has completed

- Quality(0-10) - the output, wine tasters gave a score of 0 - 10


```r
dim(df)
```

```
## [1] 6497   13
```


```r
summary(df)
```

```
##  fixed.acidity    volatile.acidity  citric.acid     residual.sugar  
##  Min.   : 3.800   Min.   :0.0800   Min.   :0.0000   Min.   : 0.600  
##  1st Qu.: 6.400   1st Qu.:0.2300   1st Qu.:0.2500   1st Qu.: 1.800  
##  Median : 7.000   Median :0.2900   Median :0.3100   Median : 3.000  
##  Mean   : 7.215   Mean   :0.3397   Mean   :0.3186   Mean   : 5.443  
##  3rd Qu.: 7.700   3rd Qu.:0.4000   3rd Qu.:0.3900   3rd Qu.: 8.100  
##  Max.   :15.900   Max.   :1.5800   Max.   :1.6600   Max.   :65.800  
##    chlorides       free.sulfur.dioxide total.sulfur.dioxide
##  Min.   :0.00900   Min.   :  1.00      Min.   :  6.0       
##  1st Qu.:0.03800   1st Qu.: 17.00      1st Qu.: 77.0       
##  Median :0.04700   Median : 29.00      Median :118.0       
##  Mean   :0.05603   Mean   : 30.53      Mean   :115.7       
##  3rd Qu.:0.06500   3rd Qu.: 41.00      3rd Qu.:156.0       
##  Max.   :0.61100   Max.   :289.00      Max.   :440.0       
##     density             pH          sulphates         alcohol     
##  Min.   :0.9871   Min.   :2.720   Min.   :0.2200   Min.   : 8.00  
##  1st Qu.:0.9923   1st Qu.:3.110   1st Qu.:0.4300   1st Qu.: 9.50  
##  Median :0.9949   Median :3.210   Median :0.5100   Median :10.30  
##  Mean   :0.9947   Mean   :3.219   Mean   :0.5313   Mean   :10.49  
##  3rd Qu.:0.9970   3rd Qu.:3.320   3rd Qu.:0.6000   3rd Qu.:11.30  
##  Max.   :1.0390   Max.   :4.010   Max.   :2.0000   Max.   :14.90  
##     quality         color          
##  Min.   :3.000   Length:6497       
##  1st Qu.:5.000   Class :character  
##  Median :6.000   Mode  :character  
##  Mean   :5.818                     
##  3rd Qu.:6.000                     
##  Max.   :9.000
```
# Observations from the Summary
- Residual Sugar has a huge outlier. The 3rd quantile is at 8.1 while the max is at 65.8. I think it can be split into groups of sweetness. 
- pH has a small range. All of it is contained withn 2.720 and 4.010. 
- Quality has 50% of its data withn 5 and 6 so it should look a nice bell curve. 
- Sulphates, Total SO2, Free SO2, Fixed acidity, Volatile acidity, and Citric acidity have maxes which are atleast double the 3rd quantile. 


# Single Variable Analysis 

Lets look at the frequenct of single variables


```r
ggplot(aes(x= quality, fill = color), data = df) + geom_histogram(colour= 'black', position = position_dodge()) + scale_x_continuous(breaks = seq(3,9,1)) + my_style 
```

![](wineAnalysis_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
- Most of the data comes from White wines. Also, the values of quality are mainly 5,6, or 7. The plot looks like a normal curve.


```r
by(df$pH, df$color, summary)
```

```
## df$color: red
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   2.740   3.210   3.310   3.311   3.400   4.010 
## -------------------------------------------------------- 
## df$color: white
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   2.720   3.090   3.180   3.188   3.280   3.820
```



```r
ggplot(aes(x = pH), data = df) + geom_histogram(colour = 'black', fill = I('#8F738F')) + facet_wrap(~color) + scale_x_continuous(breaks = seq(min(df$pH), max(df$pH),.3)) + my_style
```

![](wineAnalysis_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

- pH has a normal curve look to it. For the reds, most of the data is between 3.2 and 3.4. For the whites, most of the data is between 3.09 and 3.28.


```r
by(df$density, df$color, summary)
```

```
## df$color: red
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.9901  0.9956  0.9968  0.9967  0.9978  1.0037 
## -------------------------------------------------------- 
## df$color: white
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.9871  0.9917  0.9937  0.9940  0.9961  1.0390
```



```r
ggplot(aes(x = density, fill = color), data = df) + geom_histogram(colour = 'black') + scale_x_continuous(lim = c(.985,1.004)) + facet_wrap(~color) + my_style
```

![](wineAnalysis_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

- From this plot, it can seen that most of the density values are lower than 1. This means that most of the wine was fermented because higher % alcohol leads to lower density. I chose not to include the outliers because they made the distribution of data not as clear. I can be seen that the density for the red wine looks to have a normal curve distribution.


```r
ggplot(aes(x = chlorides, fill = color), data = df) + geom_histogram(colour = 'black') + scale_x_log10(lim = c(min(df$chlorides),.2)) + facet_wrap(~color) + my_style
```

![](wineAnalysis_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

- Here it can be seen that both of the plots have a nice unimodal distribution. This is because I transformed the x-axis to log10 scale otherwise the two plots had an unclear distribution pattern. 



```r
ggplot(aes(x = free.sulfur.dioxide, fill = color), data = df) + geom_histogram(colour = 'black') + facet_wrap(~color) + scale_x_sqrt(lim = c(min(df$free.sulfur.dioxide),100)) + my_style
```

![](wineAnalysis_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

- The red wines have a right skewed plot whereas the white wines have a left skewed plot. I transformed the sacle to sqrt beacuse the distribution is most clear on this scale. 



### What are the main features of interest in your dataset?

PH, density, and acid levels will the most effect on quailty. I also suspect that sulphates and volatile acidity are most likely realated.

### What other features do you think will help in your investigation?

Any measurement of sulfur and acid levels will be really important in building a predictive model for quailty.

### Did you make any new variables?

I used the categorical variable 'color' to analyze other variable differences between red and white wine.  

### Was there anything strange about any of the features you investigated? 

Sqrt and log10 transformations for chlorides and free SO2 was a bit strange. Maybe, those transformations will play a role in the predective model.

# Bivariate and Multivariate Anaylsis


```r
library(GGally)
ggcorr(df, label = TRUE, label_round = 2, label_size = 3, layout.exp = 3, hjust = .84) + my_style ##remove the X or id variable
```

![](wineAnalysis_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

### Observations about the Correlation Matrix

- Total sulfur dioxide and free sulfur dioxide has the highest correlation magnitude with .72
- As I stated earlier, votatile acidity and free/total sulfur dioxide have an inverse realtionship with magnitude -.35 and -.41, respectively
- Alcohol, density, and volatile acidity have the most impact on quality 
- Density is highly tied to alcohol and residual sugars which supports the data despriction
- It is odd that free/total sulfur dioxide has a negative realtionship with sulphates
- The matrix suggests pH doesn't have a high impact on quality but the data is mostly centered around 3 and 4 so its difficult to tell which pH level will have an impact
* I belive that this matrix plot is sufficent enough to explain what is going on at the two dimensional level




```r
ggplot(aes(x = density, y = alcohol, color = quality), data = df) + geom_point() + scale_x_continuous(limits = c(.98,1.02)) + my_style
```

![](wineAnalysis_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

- This plot compares desnity with alcohol based on quality. It is hard to distiguish a pattern from this plot. This means that the qulity factor needs to be narrowed down so we can if there is a pattern. 



```r
df$rating[5< df$quality & df$quality < 7] = 'Ok'
df$rating[5 >= df$quality] = 'Bad'
df$rating[df$quality >= 7] = 'Fantastic'

df$ratingsep[5>= df$quality] = 'Bad'
df$ratingsep[df$quality >= 7] = 'Fantastic'

ggplot(aes(x = rating, fill = color), data = df) + geom_bar(colour = 'black') + my_style
```

![](wineAnalysis_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


- I made a new variable that split the wine quality into 3 catagories(Bad, Good, and Fantastic). This will help narrow the quality variable, make it easier to anaylze data with several variables, and better predict quality. 


```r
ggplot(aes(y = alcohol, x  = density, color = rating), data = subset(df, rating %in% c('Bad' , 'Fantastic'))) + geom_point() + scale_x_continuous(limits = c(.98,1.005))  + scale_y_continuous(breaks = seq(8,14,1)) + my_style ##made limit .98 to 1.005 beacuse that is where most of the data is centered
```

![](wineAnalysis_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

- Again, this plot compares dessity with alcohol based on rating. The pattern here is very clear. If alcohol below 11, then for the most part it will be 'bad' wine. If it is above 11, then chances are that it will be 'fantastic'. There doesn't seem to be a pattern to the 'ok' wine, it's just sort of all over the place.


```r
ggplot(aes(y = volatile.acidity, x = chlorides, color = rating), data = subset(df, rating %in% c('Bad' , 'Fantastic')))+ geom_point() + scale_x_log10() + facet_wrap(~color) + my_style
```

![](wineAnalysis_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

- This is a plot of Volatile acidity with chlorides based on ratings and seprated by wine color. I made it log10 scale beacuse it seemed to seprate better with that scale as compared to sqrt or countinous. For red wines, there isn't much of a pattern. However, white wines seprate about evenly at chloride levels of about 1.04. Its easier to see this split between 'Bad' and 'Fantastic' wines beacuse the 'ok' wines were not included. 


```r
ggplot(aes(x = rating, y = citric.acid, fill = color), data = df) + geom_boxplot() + facet_wrap(~color) + my_style
```

![](wineAnalysis_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

- This plot shows citric acid levels for diffrent ratings seprated by wine color. White wines have about the same median and iqr for citric acid. Whereas, with the red wines, it is obvious to see that citric acid levels seprate 'Bad' and 'Fantastic' wine very clearly.


```r
ggplot(aes(x = free.sulfur.dioxide, y = total.sulfur.dioxide, color = rating), data = df) + geom_smooth(se = FALSE) + scale_x_continuous(breaks = seq(0,300, 50)) + scale_y_continuous(breaks = seq(0,400,50)) + my_style
```

![](wineAnalysis_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

- Based on this line plot, there is clear area where wine is bad. 'Fantastic' wine is at the lower left of the plot while bad wine countines well past the maximums of 'Good' and 'Fantastic' wine. If free SO2 level is above 115 and fixed SO2 level is above 190, then it is a bad wine. 


### Modeling and Predicting


```r
model1 = lm(df$quality ~ df$alcohol + df$density + df$fixed.acidity + df$chlorides + df$volatile.acidity + df$citric.acid + df$residual.sugar + df$pH + df$sulphates)

summary(model1)
```

```
## 
## Call:
## lm(formula = df$quality ~ df$alcohol + df$density + df$fixed.acidity + 
##     df$chlorides + df$volatile.acidity + df$citric.acid + df$residual.sugar + 
##     df$pH + df$sulphates)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.3633 -0.4624 -0.0361  0.4597  2.9813 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          41.575875  11.653184   3.568 0.000363 ***
## df$alcohol            0.298133   0.015928  18.717  < 2e-16 ***
## df$density          -41.068187  11.915053  -3.447 0.000571 ***
## df$fixed.acidity      0.067136   0.015676   4.283 1.87e-05 ***
## df$chlorides         -0.152571   0.332665  -0.459 0.646514    
## df$volatile.acidity  -1.359827   0.077453 -17.557  < 2e-16 ***
## df$citric.acid       -0.207294   0.078728  -2.633 0.008482 ** 
## df$residual.sugar     0.036364   0.004889   7.438 1.15e-13 ***
## df$pH                 0.439220   0.090813   4.837 1.35e-06 ***
## df$sulphates          0.763180   0.076539   9.971  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.7404 on 6487 degrees of freedom
## Multiple R-squared:  0.2821,	Adjusted R-squared:  0.2811 
## F-statistic: 283.2 on 9 and 6487 DF,  p-value: < 2.2e-16
```
- This is a really bad model with only 28% accuracy. This is mostly due to the fact that most of the 'quality' data was centered around 5,6, and 7. That is why the model should instead try to predict 'rating' rather than 'quality'.    


```r
library(rpart)
set.seed(124)
samp1 = sample(nrow(df), .6 * nrow(df))
train = df[samp1,]
test = df[-samp1,]

m2 = rpart(rating ~.-quality, data = train)

summary(m2)
```

```
## Call:
## rpart(formula = rating ~ . - quality, data = train)
##   n= 3898 
## 
##           CP nsplit rel error    xerror       xstd
## 1 0.23460145      0 1.0000000 1.0000000 0.01401273
## 2 0.09465580      1 0.7653986 0.7653986 0.01401273
## 3 0.03759058      2 0.6707428 0.6788949 0.01375612
## 4 0.03125000      3 0.6331522 0.6562500 0.01366497
## 5 0.02309783      4 0.6019022 0.6168478 0.01348165
## 6 0.01000000      5 0.5788043 0.5855978 0.01331322
## 
## Variable importance
##        ratingsep          alcohol          density        chlorides 
##               41               25               14                9 
## volatile.acidity        sulphates   residual.sugar 
##                8                1                1 
## 
## Node number 1: 3898 observations,    complexity param=0.2346014
##   predicted class=Ok         expected loss=0.5664443  P(node) =1
##     class counts:  1450   758  1690
##    probabilities: 0.372 0.194 0.434 
##   left son=2 (2539 obs) right son=3 (1359 obs)
##   Primary splits:
##       ratingsep        splits as  LR,           improve=995.56160, (1690 missing)
##       alcohol          < 10.65    to the left,  improve=195.76040, (0 missing)
##       density          < 0.992105 to the right, improve=108.16740, (0 missing)
##       chlorides        < 0.0395   to the right, improve= 85.28103, (0 missing)
##       volatile.acidity < 0.265    to the right, improve= 74.37511, (0 missing)
##   Surrogate splits:
##       alcohol          < 10.91667 to the left,  agree=0.811, adj=0.450, (1690 split)
##       density          < 0.99201  to the right, agree=0.760, adj=0.299, (0 split)
##       chlorides        < 0.0375   to the right, agree=0.735, adj=0.227, (0 split)
##       volatile.acidity < 0.195    to the right, agree=0.691, adj=0.100, (0 split)
##       sulphates        < 0.735    to the left,  agree=0.667, adj=0.029, (0 split)
## 
## Node number 2: 2539 observations,    complexity param=0.0946558
##   predicted class=Bad        expected loss=0.428909  P(node) =0.6513597
##     class counts:  1450     0  1089
##    probabilities: 0.571 0.000 0.429 
##   left son=4 (1628 obs) right son=5 (911 obs)
##   Primary splits:
##       volatile.acidity    < 0.265    to the right, improve=98.09537, (0 missing)
##       alcohol             < 10.94    to the right, improve=70.00296, (0 missing)
##       citric.acid         < 0.235    to the left,  improve=26.61454, (0 missing)
##       free.sulfur.dioxide < 16.5     to the left,  improve=24.79513, (0 missing)
##       density             < 0.99139  to the left,  improve=15.21110, (0 missing)
##   Surrogate splits:
##       residual.sugar      < 13.675   to the left,  agree=0.653, adj=0.032, (0 split)
##       chlorides           < 0.0475   to the right, agree=0.652, adj=0.031, (0 split)
##       density             < 0.99285  to the right, agree=0.651, adj=0.026, (0 split)
##       free.sulfur.dioxide < 36.5     to the left,  agree=0.649, adj=0.022, (0 split)
##       sulphates           < 0.395    to the right, agree=0.649, adj=0.021, (0 split)
## 
## Node number 3: 1359 observations,    complexity param=0.03759058
##   predicted class=Fantastic  expected loss=0.4422369  P(node) =0.3486403
##     class counts:     0   758   601
##    probabilities: 0.000 0.558 0.442 
##   left son=6 (240 obs) right son=7 (1119 obs)
##   Primary splits:
##       alcohol              < 10.91667 to the left,  improve=114.009400, (0 missing)
##       density              < 0.99662  to the right, improve= 23.543910, (0 missing)
##       residual.sugar       < 8.425    to the right, improve= 13.559820, (0 missing)
##       free.sulfur.dioxide  < 28.5     to the right, improve= 10.398520, (0 missing)
##       total.sulfur.dioxide < 92.5     to the right, improve=  6.653837, (0 missing)
##   Surrogate splits:
##       density              < 0.997835 to the right, agree=0.864, adj=0.229, (0 split)
##       residual.sugar       < 12.5     to the right, agree=0.854, adj=0.171, (0 split)
##       total.sulfur.dioxide < 199.5    to the right, agree=0.829, adj=0.033, (0 split)
##       pH                   < 2.905    to the left,  agree=0.826, adj=0.017, (0 split)
##       volatile.acidity     < 0.1125   to the left,  agree=0.826, adj=0.013, (0 split)
## 
## Node number 4: 1628 observations
##   predicted class=Bad        expected loss=0.3249386  P(node) =0.4176501
##     class counts:  1099     0   529
##    probabilities: 0.675 0.000 0.325 
## 
## Node number 5: 911 observations,    complexity param=0.02309783
##   predicted class=Ok         expected loss=0.3852909  P(node) =0.2337096
##     class counts:   351     0   560
##    probabilities: 0.385 0.000 0.615 
##   left son=10 (51 obs) right son=11 (860 obs)
##   Primary splits:
##       alcohol             < 10.95    to the right, improve=40.828120, (0 missing)
##       density             < 0.99199  to the left,  improve=16.081340, (0 missing)
##       free.sulfur.dioxide < 14.5     to the left,  improve= 9.591799, (0 missing)
##       volatile.acidity    < 0.2275   to the right, improve= 9.019994, (0 missing)
##       residual.sugar      < 1.25     to the left,  improve= 8.006228, (0 missing)
##   Surrogate splits:
##       density < 0.991365 to the left,  agree=0.962, adj=0.314, (0 split)
## 
## Node number 6: 240 observations
##   predicted class=Fantastic  expected loss=0  P(node) =0.06157004
##     class counts:     0   240     0
##    probabilities: 0.000 1.000 0.000 
## 
## Node number 7: 1119 observations,    complexity param=0.03125
##   predicted class=Ok         expected loss=0.4629133  P(node) =0.2870703
##     class counts:     0   518   601
##    probabilities: 0.000 0.463 0.537 
##   left son=14 (165 obs) right son=15 (954 obs)
##   Primary splits:
##       alcohol   < 12.775   to the right, improve=23.458090, (0 missing)
##       pH        < 3.075    to the right, improve=10.949200, (0 missing)
##       chlorides < 0.0395   to the left,  improve=10.145130, (0 missing)
##       density   < 0.990385 to the left,  improve= 7.666059, (0 missing)
##       sulphates < 0.705    to the right, improve= 6.901431, (0 missing)
##   Surrogate splits:
##       density       < 0.98911  to the left,  agree=0.878, adj=0.170, (0 split)
##       fixed.acidity < 4.75     to the left,  agree=0.855, adj=0.018, (0 split)
##       sulphates     < 0.255    to the left,  agree=0.853, adj=0.006, (0 split)
## 
## Node number 10: 51 observations
##   predicted class=Bad        expected loss=0  P(node) =0.01308363
##     class counts:    51     0     0
##    probabilities: 1.000 0.000 0.000 
## 
## Node number 11: 860 observations
##   predicted class=Ok         expected loss=0.3488372  P(node) =0.220626
##     class counts:   300     0   560
##    probabilities: 0.349 0.000 0.651 
## 
## Node number 14: 165 observations
##   predicted class=Fantastic  expected loss=0.2909091  P(node) =0.0423294
##     class counts:     0   117    48
##    probabilities: 0.000 0.709 0.291 
## 
## Node number 15: 954 observations
##   predicted class=Ok         expected loss=0.4203354  P(node) =0.2447409
##     class counts:     0   401   553
##    probabilities: 0.000 0.420 0.580
```

```r
library(rpart.plot)
rpart.plot(m2)
```

![](wineAnalysis_files/figure-html/unnamed-chunk-20-1.png)<!-- -->


```r
m2predict = predict(m2,test, type = 'class')
table(predicted = m2predict, actual = test$rating)
```

```
##            actual
## predicted   Bad Fantastic  Ok
##   Bad       700         0 358
##   Fantastic   0       227  42
##   Ok        234       292 746
```

```r
(560 + 80 + 768)/nrow(test)
```

```
## [1] 0.5417468
```

### Relationships Observed
- According to the correlation matrix and the decision tree model, alcohol has the highest impact on rating. Density, volatile.acidity, and chlorides follow after. There were some intresting relationships between factors besides the quality and rating factors. Such as the relationships of, fixed SO2 and total SO2, density and alcohol, total SO2 and volatile.acidity.  Most of these were observed beacause that is just how wine chemistry works.

### Suprising/Interesting Relationships
- The line plot where SO2 levels are compared for each 'rating' of wine was intresting. It showed that it was really easy to classify bad wine once SO2 levels exceed those levels. Also, the scaterplot for alcohol vs density based on 'rating' showed it was really easy to decide between 'Fantastic' wine and 'Bad' wine.

### Models
- The first model made was a multiple regression model which had an accuracy of ~28%. The second model which used decision trees to classify 'rating' had an accuracy of ~54%. The two models are uncomparable because they predicted two diffrent labels. The bad accuracy can be explained due to the fact that the majority of the 'quality' values were either 5,6 or 7. It could also be that I haven't caibrated the model well enough or that an entirely diffrent model should be used. 

## Final Plots and Summary
#### Plot 1

```r
ggplot(aes(x= quality, fill = color), data = df) + geom_histogram(colour= 'black', position = position_dodge()) + scale_x_continuous(breaks = seq(3,9,1)) + ggtitle('Quality count separated by wine color') + my_style
```

![](wineAnalysis_files/figure-html/unnamed-chunk-23-1.png)<!-- -->

- This histogram represents why it was so hard to predict 'quality' and 'rating'. Most of the data falls between 5,6 and 7. If  there were more values for lower end and upper end of the scale, I belive it would be easier to predict 'rating. Also, there are a lot more white wines than red. Which leads to the second point, for a lot of the factors, there wasn't enough variabilty or range to really distinguish what made sense. 


#### Plot 2

```r
ggcorr(df[,2:14], label = TRUE, label_round = 2, label_size = 4, hjust = .83, layout.exp = 3) + ggtitle('Correlation Matrix') + my_style ##remove the X or id variable
```

![](wineAnalysis_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

- This matrix shows the correlation coefficent for all the variables. It does all of the bivariate analysis in one plot. It can clearly be seen that quality is most impacted by alcohol. Also, alot of other neat realtionships can be seen just from this plot. It makes it really easy to get a nice picture of all the relationships.


#### Plot 3

```r
library(gridExtra)
p1 = ggplot(aes(y = alcohol, x  = density, color = rating), data = subset(df, rating %in% c('Bad' , 'Fantastic'))) + geom_point() + scale_x_continuous(limits = c(.98,1.005))  + scale_y_continuous(breaks = seq(8,14,1)) + my_style ##made limit .98 to 1.005 beacuse that is where most of the data is centered

p2 = ggplot(aes(y = alcohol, x  = density, color = rating), data = df) + geom_point() + scale_x_continuous(limits = c(.98,1.005))  + scale_y_continuous(breaks = seq(8,14,1)) + my_style ##made limit .98 to 1.005 beacuse that is where most of the data is centered

grid.arrange(p1,p2, ncol = 2)
```

![](wineAnalysis_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

- This plot has two great things about. One, it clearly shows what alcohol level separates 'Bad' win from the 'Fantastic' wine. Two, it shows just how messy the 'Ok' rating makes the plot. The 'Ok' rating is just a range of quality between 5 and 7. Again, that range for quality messes up anaylsis for the dataset.   

## Reflection
- This project was really fun. I learned alot about R and wine. There were many relationships between the variables and some were stronger than others. From the matrix and the model, the strongest relationships could be obtained. The hard part of this project was that, it was really difficult to pinpoint a concrete way to predict wine 'rating'. Two things limit this, one I have limited knowlege of organic chemistry and two there weren't enough diffrent variables in the dataset. In the future, the dataset could include the wine company, soil quality, or climate of the area. 
