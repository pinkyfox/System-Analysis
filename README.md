# System-Analysis

### INDEX 
1. [Regression | LW#1](#1)
2. [Mathematical expectation, dispersion, standard deviation | LW#2](#2) <br>
3. [Descriptive statistics for images | LW#3](#3) <br>
4. [Cluster Analysis | LW#4](#4) <br>
5. [Kohonen SOM | LW#5](#5)
____________________________________________________________________________________________________________________________________________________________________________________
# 1. Regression <a name = "1"></a>
## Upload Data


```R
data <- read.csv('echocardiogram_1.csv', sep = ',', dec = '.')
```

## Prepearing Data Frame


```R
data <- subset(data, select = c(5, 8))
data <- na.omit(data)
head(data)
```


<table class="dataframe">
<caption>A data.frame: 6 × 2</caption>
<thead>
	<tr><th></th><th scope=col>fractionalshortening</th><th scope=col>wallmotionscore</th></tr>
	<tr><th></th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><th scope=row>1</th><td>0.260</td><td>14</td></tr>
	<tr><th scope=row>2</th><td>0.380</td><td>14</td></tr>
	<tr><th scope=row>3</th><td>0.260</td><td>14</td></tr>
	<tr><th scope=row>4</th><td>0.253</td><td>16</td></tr>
	<tr><th scope=row>5</th><td>0.160</td><td>18</td></tr>
	<tr><th scope=row>6</th><td>0.260</td><td>12</td></tr>
</tbody>
</table>



## Data Plot


```R
plot(data$wallmotionscore, data$fractionalshortening, pch = 16, col = 'blue')
```


    
![png](https://github.com/pinkyfox/System-Analysis/blob/main/stuff/lw%231/output_5_0.png)
    


## Calculate Linear Regression


```R
regression <- lm(fractionalshortening ~ wallmotionscore, data = data)
summary(regression)
sub <- paste('fractionalshortening = ', coef(regression)[2], ' * wallmotionscore + ', coef(regression)[1])
plot(data$fractionalshortening ~ data$wallmotionscore, pch = 16, col = 'blue', sub = sub)
abline(coef(regression)[1:2])

```


    
    Call:
    lm(formula = fractionalshortening ~ wallmotionscore, data = data)
    
    Residuals:
         Min       1Q   Median       3Q      Max 
    -0.18453 -0.07267 -0.00884  0.05202  0.38753 
    
    Coefficients:
                     Estimate Std. Error t value Pr(>|t|)    
    (Intercept)      0.273106   0.031100   8.782 1.34e-14 ***
    wallmotionscore -0.003895   0.002042  -1.908   0.0588 .  
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    
    Residual standard error: 0.1046 on 120 degrees of freedom
    Multiple R-squared:  0.02943,	Adjusted R-squared:  0.02134 
    F-statistic: 3.639 on 1 and 120 DF,  p-value: 0.05883
    



    
![png](https://github.com/pinkyfox/System-Analysis/blob/main/stuff/lw%231/output_7_1.png)
    


## Calaculate Correlation


```R
cor(data$fractionalshortening, data$wallmotionscore)
```


-0.171557734476155


## Calculate Polynomial Regression


```R
polynomial_regression <- lm(fractionalshortening ~ poly(wallmotionscore, 2, raw = TRUE), data = data)
coef(summary(polynomial_regression))
sub <- paste("fractionalshortening = ", coef(polynomial_regression)[3], " * wallmotionscore^2 + ", coef(polynomial_regression)[2],  " * wallmotionscore + ",coef(polynomial_regression)[1])
plot(data$fractionalshortening ~ data$wallmotionscore, pch = 16, col = 'blue', sub = sub)
abline(coef(regression)[1:2])
lines(sort(data$wallmotionscore), fitted(polynomial_regression)[order(data$wallmotionscore)], col='red', type='l')
```


<table class="dataframe">
<caption>A matrix: 3 × 4 of type dbl</caption>
<thead>
	<tr><th></th><th scope=col>Estimate</th><th scope=col>Std. Error</th><th scope=col>t value</th><th scope=col>Pr(&gt;|t|)</th></tr>
</thead>
<tbody>
	<tr><th scope=row>(Intercept)</th><td> 0.1917937293</td><td>0.0684112224</td><td> 2.8035419</td><td>0.005903814</td></tr>
	<tr><th scope=row>poly(wallmotionscore, 2, raw = TRUE)1</th><td> 0.0060421591</td><td>0.0077255844</td><td> 0.7820974</td><td>0.435710788</td></tr>
	<tr><th scope=row>poly(wallmotionscore, 2, raw = TRUE)2</th><td>-0.0002709161</td><td>0.0002031897</td><td>-1.3333162</td><td>0.184974802</td></tr>
</tbody>
</table>




    
![png](https://github.com/pinkyfox/System-Analysis/blob/main/stuff/lw%231/output_11_1.png)  
____________________________________________________________________________________________________________________________________________________________________________________
# 2. Mathematical expectation, dispersion, standard deviation <a name = "2"></a>
## Load Data


```R
library(repr)

options(repr.plot.width = 16, repr.plot.height = 12)

data_frame <- read.csv('echocardiogram.csv', sep = ',', dec = '.')
data_frame <- subset(data_frame, select = c(5, 8))
data_frame_without_na <- na.omit(data_frame)
fractionalshortening <- na.omit(data_frame$fractionalshortening)
wallmotionscore <- na.omit(data_frame$wallmotionscore)
```

## Create Gistograms


```R
par(mfrow = c(1, 2))#The whole point of the line is to place to gistograms side by side
hist(fractionalshortening, freq = TRUE, right = F, col = 'seagreen', main = 'Fractional shortening')
hist(wallmotionscore, freq = TRUE, right = F, col = 'slateblue1', main = 'Wallmotion score')
```


    
![png](https://github.com/pinkyfox/System-Analysis/blob/main/stuff/lw%232/output_3_0.png)
    


## Calculate Mean Values, Despersions and Standart Deviations


```R
statistics_params <- matrix(c(mean(fractionalshortening), mean(wallmotionscore), #Mean Values
                        var(fractionalshortening), var(wallmotionscore),         #Variation or Dispersion 
                        sd(fractionalshortening), sd(wallmotionscore)), ncol = 3, nrow = 2) #sd -- Standart Deviation
colnames(statistics_params) <- c('Mean value', 'Despersion', 'Standart deviation')
rownames(statistics_params) <- c('Fractional shortening', 'Wallmotion score')
statistics_params
```


<table class="dataframe">
<caption>A matrix: 2 × 3 of type dbl</caption>
<thead>
	<tr><th></th><th scope=col>Mean value</th><th scope=col>Despersion</th><th scope=col>Standart deviation</th></tr>
</thead>
<tbody>
	<tr><th scope=row>Fractional shortening</th><td> 0.2167339</td><td> 0.01155901</td><td>0.1075128</td></tr>
	<tr><th scope=row>Wallmotion score</th><td>14.4381250</td><td>25.18600906</td><td>5.0185664</td></tr>
</tbody>
</table>



## Confidence Intervals for Mean Values and Despersions


```R
library(DescTools) #Load lib that contains functions for calculating confidence intervals

ci_for_means <- matrix(c(MeanCI(fractionalshortening),
                         MeanCI(wallmotionscore)), ncol = 3, byrow = TRUE)
colnames(ci_for_means) <- c("Mean value", "Lower CI", "Upper CI")
rownames(ci_for_means) <- c("Fractional shortening", "Wallmotion score")

ci_for_vars <- matrix(c(VarCI(fractionalshortening),
                        VarCI(wallmotionscore)), ncol = 3, byrow = TRUE)
colnames(ci_for_vars) <- c("Despersion", "Lower CI", "Upper CI")
rownames(ci_for_vars) <- c("Fractional shortening", "Wallmotion score")

ci_for_means
ci_for_vars
```


<table class="dataframe">
<caption>A matrix: 2 × 3 of type dbl</caption>
<thead>
	<tr><th></th><th scope=col>Mean value</th><th scope=col>Lower CI</th><th scope=col>Upper CI</th></tr>
</thead>
<tbody>
	<tr><th scope=row>Fractional shortening</th><td> 0.2167339</td><td> 0.1976225</td><td> 0.2358452</td></tr>
	<tr><th scope=row>Wallmotion score</th><td>14.4381250</td><td>13.5603547</td><td>15.3158953</td></tr>
</tbody>
</table>




<table class="dataframe">
<caption>A matrix: 2 × 3 of type dbl</caption>
<thead>
	<tr><th></th><th scope=col>Despersion</th><th scope=col>Lower CI</th><th scope=col>Upper CI</th></tr>
</thead>
<tbody>
	<tr><th scope=row>Fractional shortening</th><td> 0.01155901</td><td> 0.009137898</td><td> 0.01509378</td></tr>
	<tr><th scope=row>Wallmotion score</th><td>25.18600906</td><td>19.980678503</td><td>32.73974143</td></tr>
</tbody>
</table>



## Student 

* ***`Dispersions are known`*** 


```R
t.testk <- function(x, y, v_x, v_y, m0 = 0, alpha = 0.05, alternative = 'two.sided') {
    mean_x <- mean(x)
    mean_y <- mean(y)
    
    length_x <- length(x)
    length_y <- length(y)
    
    sd_x <- sqrt(v_x)
    sd_y <- sqrt(v_y)
    
    s <- sqrt((v_x / length_x) + (v_y / length_y))
    
    statistic <- (mean_x - mean_y - m0) / s
    
    p <- if (alternative == 'two.sided') {
        2 * pnorm(abs(statistic), lower.tail = FALSE)
    } else if (alternative == 'less') {
        pnorm(statistic, lower.tail = TRUE)
    } else {
        pnorm(statistic, lower.tail = FALSE)
    }
    
    LCL <- (mean_x - mean_y - s * qnorm(1 - alpha / 2))
    UCL <- (mean_x - mean_y + s * qnorm(1 - alpha / 2))
  
    result <- matrix(c(mean_x, mean_y, m0, sd_x, sd_y, s, statistic, p, LCL, UCL, alternative),
                     ncol = 11, byrow = TRUE)
    colnames(result) <- c('mean_x', 'mean_y', 'm0', 'sd_x', 'sd_y', 
                          's', 'statistic', 'p.value', 'LCL', 'UCL', 'alternative')
    return(result)
}

t.testk(fractionalshortening, wallmotionscore, 1, 0.78)
```


<table class="dataframe">
<caption>A matrix: 1 × 11 of type chr</caption>
<thead>
	<tr><th scope=col>mean_x</th><th scope=col>mean_y</th><th scope=col>m0</th><th scope=col>sd_x</th><th scope=col>sd_y</th><th scope=col>s</th><th scope=col>statistic</th><th scope=col>p.value</th><th scope=col>LCL</th><th scope=col>UCL</th><th scope=col>alternative</th></tr>
</thead>
<tbody>
	<tr><td>0.216733870967742</td><td>14.438125</td><td>0</td><td>1</td><td>0.883176086632785</td><td>0.118988512592738</td><td>-119.519025989574</td><td>0</td><td>-14.454604328288</td><td>-13.9881779297765</td><td>two.sided</td></tr>
</tbody>
</table>



 * ***`Dispersions are unknown`***


```R
t.test(fractionalshortening, wallmotionscore)
```


    
    	Welch Two Sample t-test
    
    data:  fractionalshortening and wallmotionscore
    t = -32.053, df = 127.12, p-value < 2.2e-16
    alternative hypothesis: true difference in means is not equal to 0
    95 percent confidence interval:
     -15.09936 -13.34342
    sample estimates:
     mean of x  mean of y 
     0.2167339 14.4381250 
    
____________________________________________________________________________________________________________________________________________________________________________________
# 3. Descriptive statistics for images <a name = "3"></a>
## Load Images


```R
library(imager)
library(repr)

options(repr.plot.width = 16, repr.plot.height = 12)

picture_1_path <- file.path('C:', 'Users', 'pinky', 'Desktop', '6sem', 'ml', 'lab3', 'picture-1.jpg') 
picture_2_path <- file.path('C:', 'Users', 'pinky', 'Desktop', '6sem', 'ml', 'lab3', 'picture-2.jpg')
picture_1 <- load.image(picture_1_path)
picture_2 <- load.image(picture_2_path)
```

## Convert Images into Grayscale Pictures


```R
par(mfrow = c(1,2))

plot(picture_1)
picture_1 <- grayscale(picture_1)
plot(picture_1)

plot(picture_2)
picture_2 <- grayscale(picture_2)
plot(picture_2)
```


    
![png](https://github.com/pinkyfox/System-Analysis/blob/main/stuff/lw%233/output_3_0.png)
    



    
![png](https://github.com/pinkyfox/System-Analysis/blob/main/stuff/lw%233/output_3_1.png)
    


## Pictures Histograms


```R
par(mfrow = c(1,2))
picture_1 %>% hist(main = 'Luminance values in picture_1')
picture_2 %>% hist(main = 'Luminance values in picture_2')
```


    
![png](https://github.com/pinkyfox/System-Analysis/blob/main/stuff/lw%233/output_5_0.png)
    


## Mean Values, Standart deviations, Modes and Medians for Histograms   


```R
mode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}
```


```R
statistics_params_for_pictures <- matrix(c(mean(picture_1), mean(picture_2), 
                                           sd(picture_1), sd(picture_2), 
                                           mode(picture_1), mode(picture_2), 
                                           median(picture_1), median(picture_2)), ncol = 4, nrow = 2)
colnames(statistics_params_for_pictures) <- c('Mean value', 'Standart deviation', 'Mode', 'Median')
rownames(statistics_params_for_pictures) <- c('Picture-1', 'Picture-2')
statistics_params_for_pictures <- as.table(statistics_params_for_pictures)
statistics_params_for_pictures
```


              Mean value Standart deviation      Mode    Median
    Picture-1  0.5100096          0.2356856 0.9007059 0.4631373
    Picture-2  0.5879227          0.2512607 0.9976863 0.5405882


## Pearson Test

`Так как статистика Пирсона измеряет разницу между эмпирическим и теоретическим распределениями, то чем больше ее наблюдаемое значение Kнабл, тем сильнее довод против основной гипотезы.`

***`Hypothesises`***:
 >* `(H0): The data are normally distributed`
 >* `(HA): The data are not normally distributed`


```R
library(nortest)
pearson.test(picture_1, adjust = TRUE)
pearson.test(picture_2, adjust = TRUE)
```


    
    	Pearson chi-square normality test
    
    data:  picture_1
    P = 88688, p-value < 2.2e-16
    



    
    	Pearson chi-square normality test
    
    data:  picture_2
    P = 125823, p-value < 2.2e-16
    


### *The low `p-value(<0.05)` indicates that* ***we fail to reject the null hypothesis and that the distribution is not normal.***
`According to the fd calculation formula` 
```R
x <- x[complete.cases(x)]
n <- length(x)
if (adjust) {
    dfd <- 2
} else {
    dfd <- 0
}
df <- n.classes - dfd - 1 
```
`we can get the fd values for two data frames. Due to the same size of pictures we get `***`fd = 260.`***
`Using Excel function `*`ХИ2ОБР`*` we can calculate Pc. It equals to `**`205.02.`**
### ***`Result:`***
*`P1 > Pc`*` and `*`P2 > Pc`*`, hence picture_1 and picture_2 `***`ARE'N NORMALLY DISTRIBUTED.`*** 

____________________________________________________________________________________________________________________________________________________________________________________
# 4. Cluster Analysis <a name = "4"></a>
## Load Libraries


```R
library(scatterplot3d)
library(heplots)
library(cluster)
library(MVN)
library(plotly)
library(klaR)
library(Morpho)
library(caret)
library(mclust)
library(ggplot2)
library(GGally)
library(plyr)
library(psych)
library(factoextra)
library(repr)

options(repr.plot.width = 16, repr.plot.height = 12)
```

## Load Data


```R
dataframe <- read.table('data.csv', header = TRUE,  sep = ',')
dataframe <- na.omit(dataframe)
head(dataframe)
```


<table class="dataframe">
<caption>A data.frame: 6 × 12</caption>
<thead>
	<tr><th></th><th scope=col>A</th><th scope=col>B</th><th scope=col>C</th><th scope=col>D</th><th scope=col>E</th><th scope=col>F</th><th scope=col>G</th><th scope=col>H</th><th scope=col>I</th><th scope=col>J</th><th scope=col>K</th><th scope=col>Name</th></tr>
	<tr><th></th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;chr&gt;</th></tr>
</thead>
<tbody>
	<tr><th scope=row>1</th><td>57.60238</td><td>21.71322</td><td>51.65182</td><td>63.15491</td><td>47.50259</td><td>59.35368</td><td> 8.753383</td><td>20.603756</td><td> 6.491436</td><td>65.02040</td><td> 8.078522</td><td>Dum      </td></tr>
	<tr><th scope=row>2</th><td>68.83134</td><td>24.31474</td><td>57.22687</td><td>62.42691</td><td>43.18839</td><td>51.63182</td><td>14.513575</td><td>73.935348</td><td>20.224595</td><td>74.94872</td><td> 8.554909</td><td>Dum      </td></tr>
	<tr><th scope=row>3</th><td>26.39129</td><td>33.54387</td><td>97.81550</td><td>62.18773</td><td>12.46912</td><td>61.80452</td><td>33.145005</td><td> 9.061499</td><td>26.646903</td><td>46.18950</td><td>11.804890</td><td>Albukerke</td></tr>
	<tr><th scope=row>4</th><td>67.07043</td><td>78.42686</td><td>49.09763</td><td>48.87779</td><td>22.98881</td><td>47.41171</td><td>85.111717</td><td>41.587100</td><td>18.941216</td><td>64.50461</td><td>13.156304</td><td>Eugene   </td></tr>
	<tr><th scope=row>5</th><td>72.76105</td><td>48.36484</td><td>68.36730</td><td>44.16909</td><td>19.68516</td><td>68.10718</td><td>83.206047</td><td>52.037685</td><td> 5.547961</td><td>74.68590</td><td>12.098432</td><td>Eugene   </td></tr>
	<tr><th scope=row>6</th><td>59.54595</td><td>78.88656</td><td>65.15747</td><td>21.71169</td><td>22.03904</td><td>64.93015</td><td>54.315721</td><td>71.587874</td><td> 7.979146</td><td>73.80838</td><td>12.670125</td><td>Eugene   </td></tr>
</tbody>
</table>



## Prepare Data for Further Analysis
<table>
<thead align = "center">
<tr>
<td><i><b>1st class</b></i></td>
<td><i><b>2st class</b></i></td>
<td><i><b>3st class</b></i></td>
<td><i><b>1st symptom</b></i></td>
<td><i><b>2st symptom</b></i></td>
<td><i><b>3st symptom</b></i></td>
</tr>
</thead>
<tbody align = "center">
<tr>
<td><i>Dum</i></td>
<td><i>Eugene</i></td>
<td><i>Baxan</i></td>
<td><i>E</i></td>
<td><i>J</i></td>
<td><i>D</i></td>
</tr>
</tbody>
</table>


```R
dataframe <- subset(dataframe, Name %in% c('Dum', 'Eugene', 'Baxan'), select = c(D, E, J, Name))
dataframe$Name <- as.factor(dataframe$Name)
head(dataframe)
```


<table class="dataframe">
<caption>A data.frame: 6 × 4</caption>
<thead>
	<tr><th></th><th scope=col>D</th><th scope=col>E</th><th scope=col>J</th><th scope=col>Name</th></tr>
	<tr><th></th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;fct&gt;</th></tr>
</thead>
<tbody>
	<tr><th scope=row>1</th><td>63.15491</td><td>47.502589</td><td>65.02040</td><td>Dum   </td></tr>
	<tr><th scope=row>2</th><td>62.42691</td><td>43.188385</td><td>74.94872</td><td>Dum   </td></tr>
	<tr><th scope=row>4</th><td>48.87779</td><td>22.988814</td><td>64.50461</td><td>Eugene</td></tr>
	<tr><th scope=row>5</th><td>44.16909</td><td>19.685162</td><td>74.68590</td><td>Eugene</td></tr>
	<tr><th scope=row>6</th><td>21.71169</td><td>22.039042</td><td>73.80838</td><td>Eugene</td></tr>
	<tr><th scope=row>7</th><td>78.19632</td><td> 8.671908</td><td>49.24287</td><td>Baxan </td></tr>
</tbody>
</table>



## Calculate Standart Statistics Metrics


```R
summary(dataframe)
```


           D                  E                  J             Name    
     Min.   : 0.03765   Min.   : 0.01439   Min.   :35.01   Baxan :749  
     1st Qu.:44.71768   1st Qu.:10.82736   1st Qu.:56.32   Dum   :749  
     Median :62.92686   Median :18.06381   Median :64.80   Eugene:749  
     Mean   :56.61332   Mean   :19.20471   Mean   :63.92               
     3rd Qu.:71.65353   3rd Qu.:24.29174   3rd Qu.:72.14               
     Max.   :99.98825   Max.   :49.99397   Max.   :86.99               


## Cluster Analysis
### ***`Get Optimal Numbers of Clusters`***


```R
fviz_nbclust(dataframe[,1:3], kmeans, method = 'wss') + geom_vline(xintercept = 2, linetype = 1)
```


    
![png](https://github.com/pinkyfox/System-Analysis/blob/main/stuff/lw%234/output_9_0.png)
    


#### ` Оптимальное число кластеров соответствует точке перегиба графика. В данном случае лучше разделить записи на 2 кластера.  Теперь выполним непосредственно кластерный анализ методом k-средних для 2 кластеров с помощью функции kmeans:`


```R
km <- kmeans(dataframe[,1:3], 2, nstart = 1000)
table(km$cluster, dataframe$Name)
```


       
        Baxan Dum Eugene
      1     6   0    599
      2   743 749    150


#### ***`Rough Accurancy Estimating`***
`Total number of correctly classified instances are: 743 + 749 + 599 = 2091.
Total number of incorrectly classified instances are: 6 + 150 = 156.
Accuracy = 2091/(2091 + 156) = 0.93 i.e our model has achieved 93% accuracy.`

#### ` Чтобы убедиться в результатах анализа определим средние значениях всех анализируемых параметров в каждом из кластеров:`


```R
centroids <- aggregate(dataframe[,1:3], by = list(km$cluster),FUN = mean)
centroids
```


<table class="dataframe">
<caption>A data.frame: 2 × 4</caption>
<thead>
	<tr><th scope=col>Group.1</th><th scope=col>D</th><th scope=col>E</th><th scope=col>J</th></tr>
	<tr><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>1</td><td>23.91189</td><td>19.80429</td><td>73.94881</td></tr>
	<tr><td>2</td><td>68.66226</td><td>18.98380</td><td>60.22417</td></tr>
</tbody>
</table>



#### `Из полученной таблицы различие между записями в разных кластерах уже видно. Теперь присвоим номера кластеров каждой из записей исходного набора данных:`


```R
dataframe <- data.frame(dataframe, km$cluster)
head(dataframe)
```


<table class="dataframe">
<caption>A data.frame: 6 × 5</caption>
<thead>
	<tr><th></th><th scope=col>D</th><th scope=col>E</th><th scope=col>J</th><th scope=col>Name</th><th scope=col>km.cluster</th></tr>
	<tr><th></th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><th scope=row>1</th><td>63.15491</td><td>47.502589</td><td>65.02040</td><td>Dum   </td><td>2</td></tr>
	<tr><th scope=row>2</th><td>62.42691</td><td>43.188385</td><td>74.94872</td><td>Dum   </td><td>2</td></tr>
	<tr><th scope=row>4</th><td>48.87779</td><td>22.988814</td><td>64.50461</td><td>Eugene</td><td>2</td></tr>
	<tr><th scope=row>5</th><td>44.16909</td><td>19.685162</td><td>74.68590</td><td>Eugene</td><td>1</td></tr>
	<tr><th scope=row>6</th><td>21.71169</td><td>22.039042</td><td>73.80838</td><td>Eugene</td><td>1</td></tr>
	<tr><th scope=row>7</th><td>78.19632</td><td> 8.671908</td><td>49.24287</td><td>Baxan </td><td>2</td></tr>
</tbody>
</table>



## 3D Plot Based on Cluster Group


```R
colors <- c('rosybrown3', 'paleturquoise4')
point_shapes <- c(15, 16)
s3d <- scatterplot3d(dataframe[,1:3], pch = point_shapes[dataframe$km.cluster], color = colors[dataframe$km.cluster])
s3d$points3d(centroids$D[1], centroids$E[1], centroids$J[1], pch = point_shapes[1], cex = 3)
s3d$points3d(centroids$D[2], centroids$E[2], centroids$J[2], pch = point_shapes[2], cex = 3)
legend('right', legend = unique(dataframe$km.cluster), 
       col = colors[unique(dataframe$km.cluster)], pch = point_shapes[unique(dataframe$km.cluster)])
```


    
![png](https://github.com/pinkyfox/System-Analysis/blob/main/stuff/lw%234/output_17_0.png)
    


## Validation of Clustering Results 

#### `We'll use the silhouette coefficient (silhouette width) to evaluate the goodness of our clustering.`

`The silhouette coefficient is calculated as follows:`
>    `For each observation `***`i,`***` it calculates the average dissimilarity between `***`i`***` and all the other points within the same cluster which `***`i`***` belongs. Let’s call this average dissimilarity `**`Di.`**
    <br>`Now we do the same dissimilarity calculation between `***`i`***` and all the other clusters and get the lowest value among them. That is, we find the dissimilarity between `***`i`***` and the cluster that is closest to `***`i`***` right after its own cluster. Let’s call that value `**`Ci.`**
    <br>`The silhouette (`**`Si`**`) width is the difference between `**`Ci`**` and `**`Di`**` divided by the greatest of those two values (`**`max(Di, Ci)`**`).`
    <br>**`Si = (Ci — Di) / max(Di, Ci)`**

`So, the interpretation of the silhouette width is the following:`

>* **`Si > 0`**` means that the observation is well clustered. The closest it is to 1, the best it is clustered.`
>* **`Si < 0`**` means that the observation was placed in the wrong cluster.`
>* **`Si = 0`**` means that the observation is between two clusters.`


```R
silhouette_width <- silhouette(dataframe$km.cluster, dist(dataframe[,1:3]))
fviz_silhouette(silhouette_width)
```

      cluster size ave.sil.width
    1       1  605          0.57
    2       2 1642          0.44
    


    
![png](https://github.com/pinkyfox/System-Analysis/blob/main/stuff/lw%234/output_19_1.png)
    


### *`The silhouette plot above gives us evidence that our clustering using four groups is good because there’s no negative silhouette width and most of the values are bigger than 0.5.`*
____________________________________________________________________________________________________________________________________________________________________________________
# 5. Kohonen SOM <a name = "5"></a>
## Load Libraries
<a href = "https://www.rpubs.com/loveb/som" style = "font: Consolas">Kohonen SOM Tutorial</a>


```R
library(scatterplot3d)
library(repr)
library(kohonen)
library(heplots)
library(cluster)
library(MVN)
library(plotly)
library(klaR)
library(Morpho)
library(caret)
library(mclust)
library(ggplot2)
library(GGally)
library(plyr)
library(psych)
library(factoextra)
library(GPArotation)
library(RColorBrewer)
library(ggpubr)

options(repr.plot.width = 16, repr.plot.height = 12)
```

## Load Data


```R
dataframe <- read.table('data.csv', header = TRUE,  sep = ',')
dataframe <- na.omit(dataframe)
head(dataframe)
```


<table class="dataframe">
<caption>A data.frame: 6 × 12</caption>
<thead>
	<tr><th></th><th scope=col>A</th><th scope=col>B</th><th scope=col>C</th><th scope=col>D</th><th scope=col>E</th><th scope=col>F</th><th scope=col>G</th><th scope=col>H</th><th scope=col>I</th><th scope=col>J</th><th scope=col>K</th><th scope=col>Name</th></tr>
	<tr><th></th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;chr&gt;</th></tr>
</thead>
<tbody>
	<tr><th scope=row>1</th><td>57.60238</td><td>21.71322</td><td>51.65182</td><td>63.15491</td><td>47.50259</td><td>59.35368</td><td> 8.753383</td><td>20.603756</td><td> 6.491436</td><td>65.02040</td><td> 8.078522</td><td>Dum      </td></tr>
	<tr><th scope=row>2</th><td>68.83134</td><td>24.31474</td><td>57.22687</td><td>62.42691</td><td>43.18839</td><td>51.63182</td><td>14.513575</td><td>73.935348</td><td>20.224595</td><td>74.94872</td><td> 8.554909</td><td>Dum      </td></tr>
	<tr><th scope=row>3</th><td>26.39129</td><td>33.54387</td><td>97.81550</td><td>62.18773</td><td>12.46912</td><td>61.80452</td><td>33.145005</td><td> 9.061499</td><td>26.646903</td><td>46.18950</td><td>11.804890</td><td>Albukerke</td></tr>
	<tr><th scope=row>4</th><td>67.07043</td><td>78.42686</td><td>49.09763</td><td>48.87779</td><td>22.98881</td><td>47.41171</td><td>85.111717</td><td>41.587100</td><td>18.941216</td><td>64.50461</td><td>13.156304</td><td>Eugene   </td></tr>
	<tr><th scope=row>5</th><td>72.76105</td><td>48.36484</td><td>68.36730</td><td>44.16909</td><td>19.68516</td><td>68.10718</td><td>83.206047</td><td>52.037685</td><td> 5.547961</td><td>74.68590</td><td>12.098432</td><td>Eugene   </td></tr>
	<tr><th scope=row>6</th><td>59.54595</td><td>78.88656</td><td>65.15747</td><td>21.71169</td><td>22.03904</td><td>64.93015</td><td>54.315721</td><td>71.587874</td><td> 7.979146</td><td>73.80838</td><td>12.670125</td><td>Eugene   </td></tr>
</tbody>
</table>



## Prepare Data for Further Analysis
<table>
<thead align = "center">
<tr>
<td><i><b>1st class</b></i></td>
<td><i><b>2st class</b></i></td>
<td><i><b>3st class</b></i></td>
<td><i><b>1st symptom</b></i></td>
<td><i><b>2st symptom</b></i></td>
<td><i><b>3st symptom</b></i></td>
<td><i><b>4st symptom</b></i></td>
<td><i><b>5st symptom</b></i></td>
</tr>
</thead>
<tbody align = "center">
<tr>
<td><i>Baxan</i></td>
<td><i>Choluteco</i></td>
<td><i>Eugene</i></td>
<td><i>K</i></td>
<td><i>J</i></td>
<td><i>G</i></td>
<td><i>B</i></td>
<td><i>A</i></td>
</tr>
</tbody>
</table>


```R
dataframe <- subset(dataframe, Name %in% c('Choluteco', 'Eugene', 'Baxan'), select = c(K, J, G, B, A, Name))
dataframe$Name <- as.factor(dataframe$Name)
head(dataframe)
```


<table class="dataframe">
<caption>A data.frame: 6 × 6</caption>
<thead>
	<tr><th></th><th scope=col>K</th><th scope=col>J</th><th scope=col>G</th><th scope=col>B</th><th scope=col>A</th><th scope=col>Name</th></tr>
	<tr><th></th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;fct&gt;</th></tr>
</thead>
<tbody>
	<tr><th scope=row>4</th><td>13.156304</td><td>64.50461</td><td>85.11172</td><td>78.42686</td><td>67.07043</td><td>Eugene</td></tr>
	<tr><th scope=row>5</th><td>12.098432</td><td>74.68590</td><td>83.20605</td><td>48.36484</td><td>72.76105</td><td>Eugene</td></tr>
	<tr><th scope=row>6</th><td>12.670125</td><td>73.80838</td><td>54.31572</td><td>78.88656</td><td>59.54595</td><td>Eugene</td></tr>
	<tr><th scope=row>7</th><td> 9.064479</td><td>49.24287</td><td>55.86420</td><td>32.18144</td><td>43.06241</td><td>Baxan </td></tr>
	<tr><th scope=row>8</th><td>11.276959</td><td>85.93160</td><td>71.59996</td><td>63.68944</td><td>36.47152</td><td>Eugene</td></tr>
	<tr><th scope=row>9</th><td> 9.249312</td><td>41.79568</td><td>48.30359</td><td>39.14198</td><td>13.87603</td><td>Baxan </td></tr>
</tbody>
</table>



## Calculate Standart Statistics Metrics


```R
summary(dataframe)
```


           K                J               G               B        
     Min.   : 9.002   Min.   :35.01   Min.   :45.01   Min.   :30.03  
     1st Qu.:10.487   1st Qu.:44.90   1st Qu.:55.73   1st Qu.:40.23  
     Median :11.610   Median :55.78   Median :58.37   Median :43.18  
     Mean   :11.490   Mean   :57.96   Mean   :62.34   Mean   :47.45  
     3rd Qu.:12.504   3rd Qu.:70.02   3rd Qu.:64.07   3rd Qu.:53.21  
     Max.   :14.000   Max.   :86.99   Max.   :99.93   Max.   :79.98  
           A                Name    
     Min.   :10.06   Baxan    :749  
     1st Qu.:40.69   Choluteco:749  
     Median :57.08   Eugene   :749  
     Mean   :56.91                  
     3rd Qu.:77.44                  
     Max.   :84.99                  


## Prepare Training Data 


```R
dataframe_train_matrix <- as.matrix(scale(dataframe[,1:5]))
```

## Train SOM


```R
set.seed(100)
som_grid = somgrid(xdim = 6, ydim = 6, topo = 'hexagonal')
som_model <- som(dataframe_train_matrix,
                 grid = som_grid,
                 rlen = 5000,
                 alpha = c(0.05, 0.01),
                 keep.data = TRUE)
plot(som_model, type = 'changes')
```


    
![png](https://github.com/pinkyfox/System-Analysis/blob/main/stuff/lw%235/output_11_0.png)
    



```R
set.seed(100)
clusterdata <- getCodes(som_model)
wss <- (nrow(clusterdata) - 1) * sum(apply(clusterdata, 2, var))

for (i in 2:35) { #i must be less than 6*6 the grid size defined at the begining
  wss[i] <- sum(kmeans(clusterdata, centers = i)$withinss)
}

par(mar = c(5.1, 4.1, 4.1, 2.1))
plot(wss, type = 'l',
     xlab = 'Number of Clusters',
     ylab = 'Within groups sum of squares',
     main = 'Within cluster sum of squares (WCSS)')
abline(v = 3, col = 'red')
```


    
![png](https://github.com/pinkyfox/System-Analysis/blob/main/stuff/lw%235/output_12_0.png)
    



```R
set.seed(100)
fit_kmeans <- kmeans(clusterdata[,1:5], 3)
cl_assignment_k <- fit_kmeans$cluster[som_model$unit.classif] 
#The above is to assign units to clusters based on their class-id (code-id) in the SOM model.
dataframe$clustersKm <- cl_assignment_k #back to original data.
```


```R
dataframe$trueN <- ifelse(dataframe$Name == 'Baxan',
                          1,
                          ifelse(dataframe$Name == 'Choluteco',
                                 2,
                                 ifelse(dataframe$Name == 'Eugene',
                                        3,
                                        NA
                                       )
                                )
                         )
```


```R
head(dataframe)
dataframe$testkm <- 1 #make everything "wrong" (i.e. 1).
#Cluster assignments can only be right in one whay, but wrong in k (no of clusters-1) ways.
#Better look for the "rights", rather than the "wrongs".
dataframe$testkm[dataframe$clustersKm == 2 & dataframe$trueN == 1] <- 0 #Make 2 right, i.e. 0.
dataframe$testkm[dataframe$clustersKm == 3 & dataframe$trueN == 2] <- 0 #3 was right for true 2.
dataframe$testkm[dataframe$clustersKm == 1 & dataframe$trueN == 3] <- 0 #1 was righht for true 3.
testKM <- sum(dataframe$testkm)
as.matrix(list(accurancy = 1 - (testKM / 36)))
```


<table class="dataframe">
<caption>A data.frame: 6 × 8</caption>
<thead>
	<tr><th></th><th scope=col>K</th><th scope=col>J</th><th scope=col>G</th><th scope=col>B</th><th scope=col>A</th><th scope=col>Name</th><th scope=col>clustersKm</th><th scope=col>trueN</th></tr>
	<tr><th></th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;fct&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><th scope=row>4</th><td>13.156304</td><td>64.50461</td><td>85.11172</td><td>78.42686</td><td>67.07043</td><td>Eugene</td><td>1</td><td>3</td></tr>
	<tr><th scope=row>5</th><td>12.098432</td><td>74.68590</td><td>83.20605</td><td>48.36484</td><td>72.76105</td><td>Eugene</td><td>1</td><td>3</td></tr>
	<tr><th scope=row>6</th><td>12.670125</td><td>73.80838</td><td>54.31572</td><td>78.88656</td><td>59.54595</td><td>Eugene</td><td>1</td><td>3</td></tr>
	<tr><th scope=row>7</th><td> 9.064479</td><td>49.24287</td><td>55.86420</td><td>32.18144</td><td>43.06241</td><td>Baxan </td><td>2</td><td>1</td></tr>
	<tr><th scope=row>8</th><td>11.276959</td><td>85.93160</td><td>71.59996</td><td>63.68944</td><td>36.47152</td><td>Eugene</td><td>1</td><td>3</td></tr>
	<tr><th scope=row>9</th><td> 9.249312</td><td>41.79568</td><td>48.30359</td><td>39.14198</td><td>13.87603</td><td>Baxan </td><td>2</td><td>1</td></tr>
</tbody>
</table>




<table class="dataframe">
<caption>A matrix: 1 × 1</caption>
<tbody>
	<tr><th scope=row>accurancy</th><td>0.9444444</td></tr>
</tbody>
</table>



## Clustering
#### `ACCCORING TO THE GRPHICS PRINTED ABOVE THE OPTIMAL VALUE OF `*`k`*` EQUALS TO 4`


```R
som_cluster <- cutree(hclust(dist(clusterdata)), k = 3)

palette <- c('#2ca02c', '#d62728', '#9467bd')

par(mfrow = c(1,2))
plot(som_model, type = 'codes') 
plot(som_model, type = 'codes', bgcol = palette[som_cluster], main = 'Clusters') 
add.cluster.boundaries(som_model, som_cluster, col = 'darkblue')
```
    
![png](https://github.com/pinkyfox/System-Analysis/blob/main/stuff/lw%235/output_17_0.png)
    
