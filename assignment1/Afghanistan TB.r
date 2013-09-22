
R version 3.0.1 (2013-05-16) -- "Good Sport"
Copyright (C) 2013 The R Foundation for Statistical Computing
Platform: x86_64-apple-darwin10.8.0 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[R.app GUI 1.61 (6492) x86_64-apple-darwin10.8.0]

[Workspace restored from /Users/shrutidezign/.RData]
[History restored from /Users/shrutidezign/.Rapp.history]

> setwd("/Users/shrutidezign/Documents/DataScience/Class2/Homework")
> x <- read.csv('TB114.csv', h=T)
> head(x)
      country iso2 iso3 iso_numeric g_whoregion year pulm_tb_rep_meth
1 Afghanistan   AF  AFG           4         EMR 1997            Smear
2 Afghanistan   AF  AFG           4         EMR 1998            Smear
3 Afghanistan   AF  AFG           4         EMR 1999            Smear
4 Afghanistan   AF  AFG           4         EMR 2000            Smear
5 Afghanistan   AF  AFG           4         EMR 2001            Smear
6 Afghanistan   AF  AFG           4         EMR 2002            Smear
  new_sp_coh new_sp_cur new_sp_cmplt new_sp_died new_sp_fail new_sp_def
1       2001        786          108          33          22        164
2       2913        772          199          48          23        168
3       2039       1571          189          83          45        113
4       3136       2396          283         107          91        198
5       6292       3305         2004         238         106        439
6       7780       4668         2090         308         141        368
  c_new_sp_tsr
1           45
2           33
3           86
4           85
5           84
6           87
> x$country <- NULL
> x$iso2 <- NULL
> x$iso3 <- NULL
> x$iso_numeric <- NULL
> x$g_whoregion <- NULL
> head(x)
  year pulm_tb_rep_meth new_sp_coh new_sp_cur new_sp_cmplt new_sp_died
1 1997            Smear       2001        786          108          33
2 1998            Smear       2913        772          199          48
3 1999            Smear       2039       1571          189          83
4 2000            Smear       3136       2396          283         107
5 2001            Smear       6292       3305         2004         238
6 2002            Smear       7780       4668         2090         308
  new_sp_fail new_sp_def c_new_sp_tsr
1          22        164           45
2          23        168           33
3          45        113           86
4          91        198           85
5         106        439           84
6         141        368           87
> fit <- lm(year ~., data =x)
Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
  contrasts can be applied only to factors with 2 or more levels
> x$pulm_tb_rep_meth <- NULL
> fit <- lm(year ~., data =x)
> summary(fit)

Call:
lm(formula = year ~ ., data = x)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.87714 -0.36383 -0.04936  0.46300  0.82686 

Coefficients:
               Estimate Std. Error t value Pr(>|t|)    
(Intercept)   1.996e+03  3.791e+00 526.577 3.17e-15 ***
new_sp_coh   -3.050e-04  1.397e-03  -0.218   0.8344    
new_sp_cur    1.447e-03  1.599e-03   0.905   0.4005    
new_sp_cmplt  1.961e-04  1.723e-03   0.114   0.9131    
new_sp_died   7.890e-03  1.239e-02   0.637   0.5477    
new_sp_fail  -3.384e-02  1.261e-02  -2.684   0.0363 *  
new_sp_def    3.011e-03  5.563e-03   0.541   0.6078    
c_new_sp_tsr  2.483e-02  4.853e-02   0.512   0.6272    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.7945 on 6 degrees of freedom
Multiple R-squared:  0.9834,	Adjusted R-squared:  0.9639 
F-statistic: 50.63 on 7 and 6 DF,  p-value: 6.456e-05

> fit2 <- update(fit, .~. -new_sp_cmplt )
\
> summary(fit2)

Call:
lm(formula = year ~ new_sp_coh + new_sp_cur + new_sp_died + new_sp_fail + 
    new_sp_def + c_new_sp_tsr, data = x)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.92420 -0.37464 -0.03787  0.45106  0.81924 

Coefficients:
               Estimate Std. Error  t value Pr(>|t|)    
(Intercept)   1.996e+03  1.930e+00 1034.321   <2e-16 ***
new_sp_coh   -1.649e-04  6.113e-04   -0.270   0.7952    
new_sp_cur    1.281e-03  6.174e-04    2.075   0.0766 .  
new_sp_died   8.251e-03  1.110e-02    0.744   0.4814    
new_sp_fail  -3.392e-02  1.167e-02   -2.908   0.0227 *  
new_sp_def    2.932e-03  5.116e-03    0.573   0.5845    
c_new_sp_tsr  2.913e-02  2.828e-02    1.030   0.3373    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.7364 on 7 degrees of freedom
Multiple R-squared:  0.9833,	Adjusted R-squared:  0.969 
F-statistic: 68.76 on 6 and 7 DF,  p-value: 7.233e-06

> plot(resid(fit2))
> ggnorm(resid(fit2))
Error: could not find function "ggnorm"
> qqnorm(resid(fit2))
> qqnorm(resid(fit2))
> fit3 <- update(fit2, .~. -new_sp_coh)
> summary(fit3)

Call:
lm(formula = year ~ new_sp_cur + new_sp_died + new_sp_fail + 
    new_sp_def + c_new_sp_tsr, data = x)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.97289 -0.39393  0.02408  0.43249  0.81974 

Coefficients:
               Estimate Std. Error  t value Pr(>|t|)    
(Intercept)   1.995e+03  1.218e+00 1638.789  < 2e-16 ***
new_sp_cur    1.117e-03  1.013e-04   11.034 4.05e-06 ***
new_sp_died   6.726e-03  8.978e-03    0.749   0.4752    
new_sp_fail  -3.381e-02  1.096e-02   -3.085   0.0150 *  
new_sp_def    2.533e-03  4.605e-03    0.550   0.5972    
c_new_sp_tsr  3.493e-02  1.726e-02    2.024   0.0776 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.6924 on 8 degrees of freedom
Multiple R-squared:  0.9831,	Adjusted R-squared:  0.9726 
F-statistic: 93.31 on 5 and 8 DF,  p-value: 7.141e-07

> plot(resid(fit3))
> qqnorm(resid(fit3))
> fit4 <- update(fit3, .~. -new_sp_def)
> summary(fit4)

Call:
lm(formula = year ~ new_sp_cur + new_sp_died + new_sp_fail + 
    c_new_sp_tsr, data = x)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.97114 -0.38191  0.03544  0.44886  0.88912 

Coefficients:
               Estimate Std. Error  t value Pr(>|t|)    
(Intercept)   1.996e+03  9.301e-01 2145.815  < 2e-16 ***
new_sp_cur    1.082e-03  7.511e-05   14.403  1.6e-07 ***
new_sp_died   1.067e-02  5.184e-03    2.059  0.06963 .  
new_sp_fail  -3.555e-02  1.008e-02   -3.525  0.00646 ** 
c_new_sp_tsr  3.312e-02  1.627e-02    2.036  0.07229 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.665 on 9 degrees of freedom
Multiple R-squared:  0.9825,	Adjusted R-squared:  0.9747 
F-statistic: 126.4 on 4 and 9 DF,  p-value: 6.719e-08

> plot(resid(fit4))
> qqnorm(resid(fit4))
> fit5 <- update(fit4, .~. - c_new_sp_tsr )
> summary(fit5)

Call:
lm(formula = year ~ new_sp_cur + new_sp_died + new_sp_fail, data = x)

Residuals:
     Min       1Q   Median       3Q      Max 
-1.11781 -0.32189 -0.03166  0.21741  1.35280 

Coefficients:
              Estimate Std. Error  t value Pr(>|t|)    
(Intercept)  1.997e+03  5.182e-01 3854.588  < 2e-16 ***
new_sp_cur   1.083e-03  8.611e-05   12.578 1.87e-07 ***
new_sp_died  1.260e-02  5.844e-03    2.156   0.0565 .  
new_sp_fail -3.018e-02  1.116e-02   -2.705   0.0221 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.7624 on 10 degrees of freedom
Multiple R-squared:  0.9745,	Adjusted R-squared:  0.9668 
F-statistic: 127.1 on 3 and 10 DF,  p-value: 2.916e-08

> plot(resid(fit5))
> qqnorm(resid(fit5))
> 