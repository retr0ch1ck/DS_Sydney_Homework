
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

[History restored from /Users/shrutidezign/.Rapp.history]

> 
> # Shruti Shetty
> # Assignment 2 - knn nfold cross validation
> 
> library(ggplot2)
> library(class)
> library(gridExtra)
Loading required package: grid
> data <- iris
> labels <- data$Species
> data$Species <- NULL
> N <- nrow(data)
> err.rates <- data.frame()  
> max.n <- 10
> max.k <- 100
> knn.fit <- factor(levels=c("setosa", "versicolor", "virginica"))
> 
> knn.nfold <- function(n, set, cl, k, err.rates) {
+ 
+ set.seed(1)
+   N <- nrow(set)
+   rand <- sample(1:N)
+   set.rand <- set[rand, ]
+   label <- cl[rand]
+   train.divisions <- cut(1:N, n, labels = FALSE) 
+  for (NTest in 1:n) {
+     test.index <- which(train.divisions == NTest)
+     test.data <- set.rand[test.index, ]
+     train.data <- set.rand[-test.index, ]
+     test.labels <- as.factor(as.matrix(label)[test.index, ])
+     train.labels <- as.factor(as.matrix(label)[-test.index, ])
+     
+     knn.fit[test.index] <- knn(train = train.data, 
+                                test  = test.data,
+                                cl    = train.labels,
+                                k     = k)
+   }
+ 
+  cat('\n', 'n = ', n, ', k = ', k, '\n', sep='')
+   print(table(label, knn.fit))
+   this.err <- sum(label != knn.fit) / length(label)
+   return(this.err)
+ 
+ }
> for (k in 1:max.k) {
+   
+   # Loop from n = 2 to max.n and perform n-fold cross validation with knn.nfold function
+   # Add error and k value to err.rates data frame
+   for (n in 2:max.n) {
+     err.rates[((k-1)*(max.n-1))+n-1, 1] <- knn.nfold(n, data, labels, k, err.rates)
+     err.rates[((k-1)*(max.n-1))+n-1, 2] <- k
+     
+   }
+ }

n = 2, k = 1
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          8        42

n = 3, k = 1
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 4, k = 1
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 5, k = 1
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 6, k = 1
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 7, k = 1
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 8, k = 1
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 9, k = 1
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 10, k = 1
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 2, k = 2
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 3, k = 2
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          3        47

n = 4, k = 2
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          3        47

n = 5, k = 2
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          5        45

n = 6, k = 2
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          3        47

n = 7, k = 2
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          4        46

n = 8, k = 2
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 9, k = 2
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 10, k = 2
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          3        47

n = 2, k = 3
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          4        46

n = 3, k = 3
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 4, k = 3
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          3        47

n = 5, k = 3
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 6, k = 3
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          3        47

n = 7, k = 3
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          4        46

n = 8, k = 3
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          3        47

n = 9, k = 3
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          3        47

n = 10, k = 3
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          3        47

n = 2, k = 4
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 3, k = 4
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          2        48

n = 4, k = 4
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          3        47

n = 5, k = 4
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          3        47

n = 6, k = 4
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          1        49

n = 7, k = 4
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          2        48

n = 8, k = 4
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          2        48

n = 9, k = 4
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 10, k = 4
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 2, k = 5
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          3        47

n = 3, k = 5
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 4, k = 5
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          2        48

n = 5, k = 5
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          3        47

n = 6, k = 5
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          1        49

n = 7, k = 5
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          1        49

n = 8, k = 5
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          2        48

n = 9, k = 5
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          1        49

n = 10, k = 5
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          1        49

n = 2, k = 6
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 3, k = 6
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          3        47

n = 4, k = 6
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          2        48

n = 5, k = 6
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          3        47

n = 6, k = 6
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          1        49

n = 7, k = 6
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          1        49

n = 8, k = 6
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          1        49

n = 9, k = 6
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          2        48

n = 10, k = 6
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          1        49

n = 2, k = 7
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          4        46

n = 3, k = 7
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          2        48

n = 4, k = 7
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          2        48

n = 5, k = 7
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          3        47

n = 6, k = 7
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          2        48

n = 7, k = 7
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          2        48

n = 8, k = 7
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          2        48

n = 9, k = 7
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          3        47

n = 10, k = 7
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          1        49

n = 2, k = 8
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          3        47

n = 3, k = 8
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          1        49

n = 4, k = 8
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          3        47

n = 5, k = 8
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 6, k = 8
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          1        49

n = 7, k = 8
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          2        48

n = 8, k = 8
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          2        48

n = 9, k = 8
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          2        48

n = 10, k = 8
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          2        48

n = 2, k = 9
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          3        47

n = 3, k = 9
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         49         1
  virginica       0          1        49

n = 4, k = 9
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          3        47

n = 5, k = 9
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 6, k = 9
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 7, k = 9
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          2        48

n = 8, k = 9
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 9, k = 9
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          2        48

n = 10, k = 9
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          2        48

n = 2, k = 10
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          4        46

n = 3, k = 10
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          2        48

n = 4, k = 10
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          1        49

n = 5, k = 10
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 6, k = 10
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          3        47

n = 7, k = 10
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 8, k = 10
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 9, k = 10
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          2        48

n = 10, k = 10
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          1        49

n = 2, k = 11
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          4        46

n = 3, k = 11
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         49         1
  virginica       0          2        48

n = 4, k = 11
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          3        47

n = 5, k = 11
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          4        46

n = 6, k = 11
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          2        48

n = 7, k = 11
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          3        47

n = 8, k = 11
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          2        48

n = 9, k = 11
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          3        47

n = 10, k = 11
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          2        48

n = 2, k = 12
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          6        44

n = 3, k = 12
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          1        49

n = 4, k = 12
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          3        47

n = 5, k = 12
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          4        46

n = 6, k = 12
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          3        47

n = 7, k = 12
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          3        47

n = 8, k = 12
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 9, k = 12
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          3        47

n = 10, k = 12
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 2, k = 13
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          4        46

n = 3, k = 13
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          1        49

n = 4, k = 13
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          3        47

n = 5, k = 13
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 6, k = 13
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          1        49

n = 7, k = 13
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 8, k = 13
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 9, k = 13
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          3        47

n = 10, k = 13
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          1        49

n = 2, k = 14
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          4        46

n = 3, k = 14
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         49         1
  virginica       0          3        47

n = 4, k = 14
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 5, k = 14
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 6, k = 14
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          1        49

n = 7, k = 14
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 8, k = 14
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          1        49

n = 9, k = 14
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          3        47

n = 10, k = 14
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          1        49

n = 2, k = 15
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 3, k = 15
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          2        48

n = 4, k = 15
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          3        47

n = 5, k = 15
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 6, k = 15
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 7, k = 15
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          3        47

n = 8, k = 15
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 9, k = 15
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          3        47

n = 10, k = 15
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          1        49

n = 2, k = 16
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          5        45

n = 3, k = 16
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         49         1
  virginica       0          2        48

n = 4, k = 16
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          3        47

n = 5, k = 16
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 6, k = 16
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          1        49

n = 7, k = 16
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          3        47

n = 8, k = 16
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 9, k = 16
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          3        47

n = 10, k = 16
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 2, k = 17
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         49         1
  virginica       0          5        45

n = 3, k = 17
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          3        47

n = 4, k = 17
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          4        46

n = 5, k = 17
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          2        48

n = 6, k = 17
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          3        47

n = 7, k = 17
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 8, k = 17
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 9, k = 17
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 10, k = 17
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 2, k = 18
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          5        45

n = 3, k = 18
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          4        46

n = 4, k = 18
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 5, k = 18
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          4        46

n = 6, k = 18
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          3        47

n = 7, k = 18
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          3        47

n = 8, k = 18
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 9, k = 18
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 10, k = 18
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          1        49

n = 2, k = 19
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          4        46

n = 3, k = 19
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          3        47

n = 4, k = 19
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 5, k = 19
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          3        47

n = 6, k = 19
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 7, k = 19
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          3        47

n = 8, k = 19
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 9, k = 19
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 10, k = 19
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          1        49

n = 2, k = 20
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 3, k = 20
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          3        47

n = 4, k = 20
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          4        46

n = 5, k = 20
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          4        46

n = 6, k = 20
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 7, k = 20
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 8, k = 20
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 9, k = 20
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          6        44

n = 10, k = 20
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          1        49

n = 2, k = 21
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 3, k = 21
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          3        47

n = 4, k = 21
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          5        45

n = 5, k = 21
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 6, k = 21
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          2        48

n = 7, k = 21
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          3        47

n = 8, k = 21
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 9, k = 21
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 10, k = 21
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          1        49

n = 2, k = 22
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          6        44

n = 3, k = 22
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          4        46

n = 4, k = 22
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 5, k = 22
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          5        45

n = 6, k = 22
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          3        47

n = 7, k = 22
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          4        46

n = 8, k = 22
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          1        49

n = 9, k = 22
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 10, k = 22
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          2        48

n = 2, k = 23
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          6        44

n = 3, k = 23
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          3        47

n = 4, k = 23
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          4        46

n = 5, k = 23
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         49         1
  virginica       0          5        45

n = 6, k = 23
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          4        46

n = 7, k = 23
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          5        45

n = 8, k = 23
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          2        48

n = 9, k = 23
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          5        45

n = 10, k = 23
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          4        46

n = 2, k = 24
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          6        44

n = 3, k = 24
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          3        47

n = 4, k = 24
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          4        46

n = 5, k = 24
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 6, k = 24
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          3        47

n = 7, k = 24
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          5        45

n = 8, k = 24
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          3        47

n = 9, k = 24
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          7        43

n = 10, k = 24
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          5        45

n = 2, k = 25
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          6        44

n = 3, k = 25
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          4        46

n = 4, k = 25
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 5, k = 25
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         49         1
  virginica       0          4        46

n = 6, k = 25
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          5        45

n = 7, k = 25
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          5        45

n = 8, k = 25
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          4        46

n = 9, k = 25
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          6        44

n = 10, k = 25
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          5        45

n = 2, k = 26
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          6        44

n = 3, k = 26
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 4, k = 26
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 5, k = 26
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          4        46

n = 6, k = 26
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          4        46

n = 7, k = 26
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          4        46

n = 8, k = 26
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          6        44

n = 9, k = 26
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          7        43

n = 10, k = 26
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          6        44

n = 2, k = 27
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          6        44

n = 3, k = 27
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 4, k = 27
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 5, k = 27
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          4        46

n = 6, k = 27
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          4        46

n = 7, k = 27
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          4        46

n = 8, k = 27
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          5        45

n = 9, k = 27
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          6        44

n = 10, k = 27
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          5        45

n = 2, k = 28
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          6        44

n = 3, k = 28
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          5        45

n = 4, k = 28
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          5        45

n = 5, k = 28
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          4        46

n = 6, k = 28
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 7, k = 28
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 8, k = 28
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 9, k = 28
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          5        45

n = 10, k = 28
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          4        46

n = 2, k = 29
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          8        42

n = 3, k = 29
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          5        45

n = 4, k = 29
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 5, k = 29
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          5        45

n = 6, k = 29
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 7, k = 29
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 8, k = 29
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 9, k = 29
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 10, k = 29
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 2, k = 30
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          8        42

n = 3, k = 30
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          5        45

n = 4, k = 30
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 5, k = 30
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          6        44

n = 6, k = 30
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 7, k = 30
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 8, k = 30
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 9, k = 30
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 10, k = 30
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 2, k = 31
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          9        41

n = 3, k = 31
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          5        45

n = 4, k = 31
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 5, k = 31
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          6        44

n = 6, k = 31
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          6        44

n = 7, k = 31
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          6        44

n = 8, k = 31
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 9, k = 31
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          4        46

n = 10, k = 31
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          4        46

n = 2, k = 32
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0         10        40

n = 3, k = 32
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          5        45

n = 4, k = 32
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          6        44

n = 5, k = 32
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          4        46

n = 6, k = 32
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 7, k = 32
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 8, k = 32
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 9, k = 32
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 10, k = 32
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 2, k = 33
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         43         7
  virginica       0         10        40

n = 3, k = 33
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          6        44

n = 4, k = 33
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          6        44

n = 5, k = 33
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          6        44

n = 6, k = 33
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          6        44

n = 7, k = 33
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          6        44

n = 8, k = 33
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          6        44

n = 9, k = 33
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          7        43

n = 10, k = 33
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          5        45

n = 2, k = 34
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0         11        39

n = 3, k = 34
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          7        43

n = 4, k = 34
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          6        44

n = 5, k = 34
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          6        44

n = 6, k = 34
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          5        45

n = 7, k = 34
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 8, k = 34
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 9, k = 34
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          6        44

n = 10, k = 34
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          5        45

n = 2, k = 35
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0         14        36

n = 3, k = 35
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         44         6
  virginica       0          7        43

n = 4, k = 35
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          6        44

n = 5, k = 35
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          6        44

n = 6, k = 35
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 7, k = 35
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          5        45

n = 8, k = 35
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          5        45

n = 9, k = 35
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          7        43

n = 10, k = 35
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          6        44

n = 2, k = 36
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0         15        35

n = 3, k = 36
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         44         6
  virginica       0          7        43

n = 4, k = 36
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          6        44

n = 5, k = 36
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          7        43

n = 6, k = 36
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 7, k = 36
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          6        44

n = 8, k = 36
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          5        45

n = 9, k = 36
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          7        43

n = 10, k = 36
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          6        44

n = 2, k = 37
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         44         6
  virginica       0         16        34

n = 3, k = 37
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         44         6
  virginica       0          7        43

n = 4, k = 37
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          6        44

n = 5, k = 37
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          6        44

n = 6, k = 37
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          7        43

n = 7, k = 37
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          6        44

n = 8, k = 37
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          5        45

n = 9, k = 37
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          7        43

n = 10, k = 37
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          6        44

n = 2, k = 38
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         44         6
  virginica       0         15        35

n = 3, k = 38
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         43         7
  virginica       0          8        42

n = 4, k = 38
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          7        43

n = 5, k = 38
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 6, k = 38
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          6        44

n = 7, k = 38
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          6        44

n = 8, k = 38
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          6        44

n = 9, k = 38
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          7        43

n = 10, k = 38
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          7        43

n = 2, k = 39
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         44         6
  virginica       0         15        35

n = 3, k = 39
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         43         7
  virginica       0          8        42

n = 4, k = 39
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          7        43

n = 5, k = 39
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          6        44

n = 6, k = 39
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          6        44

n = 7, k = 39
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          6        44

n = 8, k = 39
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          7        43

n = 9, k = 39
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          7        43

n = 10, k = 39
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          6        44

n = 2, k = 40
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         43         7
  virginica       0         20        30

n = 3, k = 40
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         44         6
  virginica       0          7        43

n = 4, k = 40
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          7        43

n = 5, k = 40
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 6, k = 40
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 7, k = 40
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          6        44

n = 8, k = 40
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          4        46

n = 9, k = 40
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          6        44

n = 10, k = 40
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 2, k = 41
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         41         9
  virginica       0         25        25

n = 3, k = 41
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         43         7
  virginica       0          7        43

n = 4, k = 41
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          6        44

n = 5, k = 41
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 6, k = 41
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 7, k = 41
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          5        45

n = 8, k = 41
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 9, k = 41
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         48         2
  virginica       0          7        43

n = 10, k = 41
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          6        44

n = 2, k = 42
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         41         9
  virginica       0         24        26

n = 3, k = 42
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         43         7
  virginica       0          7        43

n = 4, k = 42
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          7        43

n = 5, k = 42
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 6, k = 42
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          6        44

n = 7, k = 42
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 8, k = 42
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 9, k = 42
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          7        43

n = 10, k = 42
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          6        44

n = 2, k = 43
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         40         9
  virginica       0         30        20

n = 3, k = 43
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         42         8
  virginica       0          8        42

n = 4, k = 43
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          8        42

n = 5, k = 43
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          6        44

n = 6, k = 43
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          6        44

n = 7, k = 43
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          7        43

n = 8, k = 43
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 9, k = 43
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          8        42

n = 10, k = 43
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 2, k = 44
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         33        16
  virginica       0         29        21

n = 3, k = 44
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         43         7
  virginica       0          8        42

n = 4, k = 44
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          8        42

n = 5, k = 44
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          5        45

n = 6, k = 44
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          6        44

n = 7, k = 44
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          5        45

n = 8, k = 44
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          5        45

n = 9, k = 44
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          6        44

n = 10, k = 44
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 2, k = 45
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         28        21
  virginica       0         29        21

n = 3, k = 45
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         43         7
  virginica       0          8        42

n = 4, k = 45
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          8        42

n = 5, k = 45
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         44         6
  virginica       0          6        44

n = 6, k = 45
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          7        43

n = 7, k = 45
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          7        43

n = 8, k = 45
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          6        44

n = 9, k = 45
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          7        43

n = 10, k = 45
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          7        43

n = 2, k = 46
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         28        21
  virginica       0         29        21

n = 3, k = 46
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         43         7
  virginica       0          8        42

n = 4, k = 46
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          8        42

n = 5, k = 46
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          6        44

n = 6, k = 46
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         44         6
  virginica       0          6        44

n = 7, k = 46
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          5        45

n = 8, k = 46
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          4        46

n = 9, k = 46
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          7        43

n = 10, k = 46
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          6        44

n = 2, k = 47
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         27        22
  virginica       0         29        21

n = 3, k = 47
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         42         8
  virginica       0          8        42

n = 4, k = 47
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          8        42

n = 5, k = 47
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          6        44

n = 6, k = 47
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          6        44

n = 7, k = 47
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          5        45

n = 8, k = 47
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          4        46

n = 9, k = 47
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          7        43

n = 10, k = 47
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          6        44

n = 2, k = 48
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         27        22
  virginica       0         29        21

n = 3, k = 48
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         42         8
  virginica       0          8        42

n = 4, k = 48
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          9        41

n = 5, k = 48
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          6        44

n = 6, k = 48
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          7        43

n = 7, k = 48
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          7        43

n = 8, k = 48
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          4        46

n = 9, k = 48
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          7        43

n = 10, k = 48
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          6        44

n = 2, k = 49
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         27        22
  virginica       0         29        21

n = 3, k = 49
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         42         8
  virginica       0          8        42

n = 4, k = 49
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          9        41

n = 5, k = 49
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          9        41

n = 6, k = 49
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          7        43

n = 7, k = 49
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          7        43

n = 8, k = 49
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          6        44

n = 9, k = 49
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          7        43

n = 10, k = 49
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          6        44

n = 2, k = 50
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      2         26        22
  virginica       0         29        21

n = 3, k = 50
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         42         8
  virginica       0          8        42

n = 4, k = 50
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          9        41

n = 5, k = 50
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          7        43

n = 6, k = 50
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          7        43

n = 7, k = 50
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          8        42

n = 8, k = 50
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          8        42

n = 9, k = 50
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          8        42

n = 10, k = 50
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          6        44

n = 2, k = 51
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         27        22
  virginica       0         29        21

n = 3, k = 51
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         41         8
  virginica       0         10        40

n = 4, k = 51
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          9        41

n = 5, k = 51
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          8        42

n = 6, k = 51
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          7        43

n = 7, k = 51
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          9        41

n = 8, k = 51
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          8        42

n = 9, k = 51
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          7        43

n = 10, k = 51
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          6        44

n = 2, k = 52
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         27        23
  virginica       0         29        21

n = 3, k = 52
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         41         8
  virginica       0         11        39

n = 4, k = 52
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          9        41

n = 5, k = 52
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          8        42

n = 6, k = 52
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          7        43

n = 7, k = 52
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          8        42

n = 8, k = 52
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          7        43

n = 9, k = 52
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          7        43

n = 10, k = 52
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          6        44

n = 2, k = 53
            knn.fit
label        setosa versicolor virginica
  setosa         45          5         0
  versicolor      1         26        23
  virginica       0         29        21

n = 3, k = 53
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         41         8
  virginica       0         11        39

n = 4, k = 53
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0         10        40

n = 5, k = 53
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          8        42

n = 6, k = 53
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          8        42

n = 7, k = 53
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          7        43

n = 8, k = 53
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          7        43

n = 9, k = 53
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          7        43

n = 10, k = 53
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          7        43

n = 2, k = 54
            knn.fit
label        setosa versicolor virginica
  setosa         36         14         0
  versicolor      1         26        23
  virginica       0         29        21

n = 3, k = 54
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         42         7
  virginica       0         14        36

n = 4, k = 54
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          8        42

n = 5, k = 54
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0         10        40

n = 6, k = 54
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          8        42

n = 7, k = 54
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          9        41

n = 8, k = 54
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          8        42

n = 9, k = 54
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         47         3
  virginica       0          7        43

n = 10, k = 54
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          7        43

n = 2, k = 55
            knn.fit
label        setosa versicolor virginica
  setosa         28         22         0
  versicolor      1         26        23
  virginica       0         29        21

n = 3, k = 55
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         42         7
  virginica       0         11        39

n = 4, k = 55
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0         11        39

n = 5, k = 55
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0         10        40

n = 6, k = 55
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          9        41

n = 7, k = 55
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          9        41

n = 8, k = 55
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          8        42

n = 9, k = 55
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          7        43

n = 10, k = 55
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          7        43

n = 2, k = 56
            knn.fit
label        setosa versicolor virginica
  setosa         28         22         0
  versicolor      1         26        23
  virginica       0         29        21

n = 3, k = 56
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         37        12
  virginica       0         14        36

n = 4, k = 56
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0         11        39

n = 5, k = 56
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          7        43

n = 6, k = 56
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         44         6
  virginica       0          9        41

n = 7, k = 56
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0         11        39

n = 8, k = 56
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          8        42

n = 9, k = 56
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          8        42

n = 10, k = 56
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          8        42

n = 2, k = 57
            knn.fit
label        setosa versicolor virginica
  setosa         27         23         0
  versicolor      1         26        23
  virginica       0         29        21

n = 3, k = 57
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         32        17
  virginica       0         19        31

n = 4, k = 57
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0         11        39

n = 5, k = 57
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         44         6
  virginica       0         11        39

n = 6, k = 57
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          8        42

n = 7, k = 57
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0         11        39

n = 8, k = 57
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          9        41

n = 9, k = 57
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          8        42

n = 10, k = 57
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          7        43

n = 2, k = 58
            knn.fit
label        setosa versicolor virginica
  setosa         27         23         0
  versicolor      2         24        24
  virginica       0         29        21

n = 3, k = 58
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         32        17
  virginica       0         22        28

n = 4, k = 58
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0         11        39

n = 5, k = 58
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0          9        41

n = 6, k = 58
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          8        42

n = 7, k = 58
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         45         4
  virginica       0          8        42

n = 8, k = 58
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          8        42

n = 9, k = 58
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          8        42

n = 10, k = 58
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          7        43

n = 2, k = 59
            knn.fit
label        setosa versicolor virginica
  setosa         26         24         0
  versicolor      2         24        24
  virginica       0         29        21

n = 3, k = 59
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         31        18
  virginica       0         29        21

n = 4, k = 59
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0         11        39

n = 5, k = 59
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         11        39

n = 6, k = 59
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         44         6
  virginica       0          8        42

n = 7, k = 59
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         11        39

n = 8, k = 59
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          9        41

n = 9, k = 59
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          8        42

n = 10, k = 59
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          8        42

n = 2, k = 60
            knn.fit
label        setosa versicolor virginica
  setosa         26         24         0
  versicolor      2         24        24
  virginica       0         29        21

n = 3, k = 60
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      2         30        18
  virginica       0         28        22

n = 4, k = 60
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0         11        39

n = 5, k = 60
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0          9        41

n = 6, k = 60
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          7        43

n = 7, k = 60
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0          9        41

n = 8, k = 60
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          9        41

n = 9, k = 60
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          8        42

n = 10, k = 60
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          7        43

n = 2, k = 61
            knn.fit
label        setosa versicolor virginica
  setosa         26         24         0
  versicolor      3         23        24
  virginica       0         29        21

n = 3, k = 61
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         31        18
  virginica       0         29        21

n = 4, k = 61
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         12        38

n = 5, k = 61
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0         10        40

n = 6, k = 61
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          9        41

n = 7, k = 61
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0         11        39

n = 8, k = 61
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          9        41

n = 9, k = 61
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          7        43

n = 10, k = 61
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          7        43

n = 2, k = 62
            knn.fit
label        setosa versicolor virginica
  setosa         26         24         0
  versicolor      3         23        24
  virginica       0         29        21

n = 3, k = 62
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         30        19
  virginica       0         29        21

n = 4, k = 62
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         10        40

n = 5, k = 62
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0         10        40

n = 6, k = 62
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         44         6
  virginica       0          8        42

n = 7, k = 62
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0         10        40

n = 8, k = 62
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0          9        41

n = 9, k = 62
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0          8        42

n = 10, k = 62
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          9        41

n = 2, k = 63
            knn.fit
label        setosa versicolor virginica
  setosa         26         24         0
  versicolor      3         23        24
  virginica       0         29        21

n = 3, k = 63
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      2         30        18
  virginica       0         29        21

n = 4, k = 63
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0         13        37

n = 5, k = 63
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         13        37

n = 6, k = 63
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         42         7
  virginica       0         10        40

n = 7, k = 63
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         10        40

n = 8, k = 63
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0          9        41

n = 9, k = 63
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0          8        42

n = 10, k = 63
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0         11        39

n = 2, k = 64
            knn.fit
label        setosa versicolor virginica
  setosa         26         24         0
  versicolor      3         23        24
  virginica       0         29        21

n = 3, k = 64
            knn.fit
label        setosa versicolor virginica
  setosa         49          1         0
  versicolor      2         29        19
  virginica       0         35        15

n = 4, k = 64
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         12        38

n = 5, k = 64
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0         12        38

n = 6, k = 64
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         41         8
  virginica       0          9        41

n = 7, k = 64
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         10        40

n = 8, k = 64
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0         10        40

n = 9, k = 64
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0          8        42

n = 10, k = 64
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0         10        40

n = 2, k = 65
            knn.fit
label        setosa versicolor virginica
  setosa         26         24         0
  versicolor      3         23        24
  virginica       0         29        21

n = 3, k = 65
            knn.fit
label        setosa versicolor virginica
  setosa         49          1         0
  versicolor      1         29        20
  virginica       0         39        11

n = 4, k = 65
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         11        39

n = 5, k = 65
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         14        36

n = 6, k = 65
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         42         7
  virginica       0         12        38

n = 7, k = 65
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         45         4
  virginica       0         11        39

n = 8, k = 65
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         10        40

n = 9, k = 65
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0         10        40

n = 10, k = 65
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         46         4
  virginica       0         10        40

n = 2, k = 66
            knn.fit
label        setosa versicolor virginica
  setosa         26         24         0
  versicolor      3         22        25
  virginica       0         29        21

n = 3, k = 66
            knn.fit
label        setosa versicolor virginica
  setosa         46          4         0
  versicolor      1         30        19
  virginica       0         39        11

n = 4, k = 66
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         45         5
  virginica       0         12        38

n = 5, k = 66
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0         12        38

n = 6, k = 66
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         42         7
  virginica       0         10        40

n = 7, k = 66
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         45         4
  virginica       0         11        39

n = 8, k = 66
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0          9        41

n = 9, k = 66
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0          9        41

n = 10, k = 66
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0          9        41

n = 2, k = 67
            knn.fit
label        setosa versicolor virginica
  setosa         26         24         0
  versicolor      3         22        25
  virginica       0         29        21

n = 3, k = 67
            knn.fit
label        setosa versicolor virginica
  setosa         43          7         0
  versicolor      1         29        20
  virginica       0         39        11

n = 4, k = 67
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         13        37

n = 5, k = 67
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         42         7
  virginica       0         14        36

n = 6, k = 67
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         42         7
  virginica       0         12        38

n = 7, k = 67
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         11        39

n = 8, k = 67
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         10        40

n = 9, k = 67
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         42         7
  virginica       0         10        40

n = 10, k = 67
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         10        40

n = 2, k = 68
            knn.fit
label        setosa versicolor virginica
  setosa         26         24         0
  versicolor      3         22        25
  virginica       0         29        21

n = 3, k = 68
            knn.fit
label        setosa versicolor virginica
  setosa         37         13         0
  versicolor      1         30        19
  virginica       0         39        11

n = 4, k = 68
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         12        38

n = 5, k = 68
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         42         7
  virginica       0         14        36

n = 6, k = 68
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         41         8
  virginica       0         11        39

n = 7, k = 68
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         11        39

n = 8, k = 68
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0         11        39

n = 9, k = 68
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0         10        40

n = 10, k = 68
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         11        39

n = 2, k = 69
            knn.fit
label        setosa versicolor virginica
  setosa         26         24         0
  versicolor      3         22        25
  virginica       0         29        21

n = 3, k = 69
            knn.fit
label        setosa versicolor virginica
  setosa         33         17         0
  versicolor      1         29        20
  virginica       0         39        11

n = 4, k = 69
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         14        36

n = 5, k = 69
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         42         7
  virginica       0         14        36

n = 6, k = 69
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         42         7
  virginica       0         12        38

n = 7, k = 69
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         12        38

n = 8, k = 69
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         11        39

n = 9, k = 69
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0         11        39

n = 10, k = 69
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0         11        39

n = 2, k = 70
            knn.fit
label        setosa versicolor virginica
  setosa         14         24        12
  versicolor      2         22        26
  virginica       0         29        21

n = 3, k = 70
            knn.fit
label        setosa versicolor virginica
  setosa         33         17         0
  versicolor      1         29        20
  virginica       0         39        11

n = 4, k = 70
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         44         6
  virginica       0         11        39

n = 5, k = 70
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         41         8
  virginica       0         15        35

n = 6, k = 70
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         42         7
  virginica       0         13        37

n = 7, k = 70
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         13        37

n = 8, k = 70
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         10        40

n = 9, k = 70
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0         11        39

n = 10, k = 70
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         11        39

n = 2, k = 71
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 71
            knn.fit
label        setosa versicolor virginica
  setosa         33         17         0
  versicolor      1         29        20
  virginica       0         39        11

n = 4, k = 71
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         41         9
  virginica       0         19        31

n = 5, k = 71
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         38        11
  virginica       0         16        34

n = 6, k = 71
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0         13        37

n = 7, k = 71
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         14        36

n = 8, k = 71
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0         11        39

n = 9, k = 71
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0         11        39

n = 10, k = 71
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         11        39

n = 2, k = 72
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 72
            knn.fit
label        setosa versicolor virginica
  setosa         32         18         0
  versicolor      1         29        20
  virginica       0         39        11

n = 4, k = 72
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         38        12
  virginica       0         25        25

n = 5, k = 72
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         38        11
  virginica       0         15        35

n = 6, k = 72
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0         13        37

n = 7, k = 72
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         15        35

n = 8, k = 72
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         11        39

n = 9, k = 72
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0         11        39

n = 10, k = 72
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         13        37

n = 2, k = 73
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 73
            knn.fit
label        setosa versicolor virginica
  setosa         32         18         0
  versicolor      1         29        20
  virginica       0         39        11

n = 4, k = 73
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         39        11
  virginica       0         30        20

n = 5, k = 73
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         38        11
  virginica       0         16        34

n = 6, k = 73
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0         15        35

n = 7, k = 73
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         17        33

n = 8, k = 73
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0         13        37

n = 9, k = 73
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0         12        38

n = 10, k = 73
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         12        38

n = 2, k = 74
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 74
            knn.fit
label        setosa versicolor virginica
  setosa         31         19         0
  versicolor      1         29        20
  virginica       0         39        11

n = 4, k = 74
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         37        13
  virginica       0         29        21

n = 5, k = 74
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         37        12
  virginica       0         21        29

n = 6, k = 74
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         41         8
  virginica       0         12        38

n = 7, k = 74
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         17        33

n = 8, k = 74
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         11        39

n = 9, k = 74
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         42         7
  virginica       0         13        37

n = 10, k = 74
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         13        37

n = 2, k = 75
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 75
            knn.fit
label        setosa versicolor virginica
  setosa         25         25         0
  versicolor      1         29        20
  virginica       0         39        11

n = 4, k = 75
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         34        16
  virginica       0         29        21

n = 5, k = 75
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      2         36        12
  virginica       0         26        24

n = 6, k = 75
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         39        10
  virginica       0         13        37

n = 7, k = 75
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         17        33

n = 8, k = 75
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0         14        36

n = 9, k = 75
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0         12        38

n = 10, k = 75
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         42         7
  virginica       0         12        38

n = 2, k = 76
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 76
            knn.fit
label        setosa versicolor virginica
  setosa         25         25         0
  versicolor      1         29        20
  virginica       0         39        11

n = 4, k = 76
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         33        17
  virginica       0         29        21

n = 5, k = 76
            knn.fit
label        setosa versicolor virginica
  setosa         47          3         0
  versicolor      1         37        12
  virginica       0         26        24

n = 6, k = 76
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         38        11
  virginica       0         12        38

n = 7, k = 76
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         40         9
  virginica       0         14        36

n = 8, k = 76
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         12        38

n = 9, k = 76
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0         11        39

n = 10, k = 76
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         42         7
  virginica       0         12        38

n = 2, k = 77
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 77
            knn.fit
label        setosa versicolor virginica
  setosa         24         26         0
  versicolor      1         29        20
  virginica       0         39        11

n = 4, k = 77
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         33        17
  virginica       0         29        21

n = 5, k = 77
            knn.fit
label        setosa versicolor virginica
  setosa         46          4         0
  versicolor      2         36        12
  virginica       0         32        18

n = 6, k = 77
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         39        10
  virginica       0         13        37

n = 7, k = 77
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         41         8
  virginica       0         14        36

n = 8, k = 77
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         42         7
  virginica       0         12        38

n = 9, k = 77
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0         14        36

n = 10, k = 77
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         42         7
  virginica       0         13        37

n = 2, k = 78
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 78
            knn.fit
label        setosa versicolor virginica
  setosa         23         27         0
  versicolor      1         29        20
  virginica       0         39        11

n = 4, k = 78
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      0         32        18
  virginica       0         29        21

n = 5, k = 78
            knn.fit
label        setosa versicolor virginica
  setosa         44          6         0
  versicolor      2         36        12
  virginica       0         39        11

n = 6, k = 78
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         39        10
  virginica       0         20        30

n = 7, k = 78
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         40         9
  virginica       0         14        36

n = 8, k = 78
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         12        38

n = 9, k = 78
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         42         7
  virginica       0         12        38

n = 10, k = 78
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0         12        38

n = 2, k = 79
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 79
            knn.fit
label        setosa versicolor virginica
  setosa         20         30         0
  versicolor      2         28        20
  virginica       0         39        11

n = 4, k = 79
            knn.fit
label        setosa versicolor virginica
  setosa         48          2         0
  versicolor      0         32        18
  virginica       0         29        21

n = 5, k = 79
            knn.fit
label        setosa versicolor virginica
  setosa         43          7         0
  versicolor      2         36        12
  virginica       0         41         9

n = 6, k = 79
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         38        11
  virginica       0         20        30

n = 7, k = 79
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         39        10
  virginica       0         14        36

n = 8, k = 79
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         44         5
  virginica       0         14        36

n = 9, k = 79
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         42         7
  virginica       0         13        37

n = 10, k = 79
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         42         7
  virginica       0         13        37

n = 2, k = 80
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 80
            knn.fit
label        setosa versicolor virginica
  setosa         20         30         0
  versicolor      2         28        20
  virginica       0         39        11

n = 4, k = 80
            knn.fit
label        setosa versicolor virginica
  setosa         44          6         0
  versicolor      0         32        18
  virginica       0         29        21

n = 5, k = 80
            knn.fit
label        setosa versicolor virginica
  setosa         37         13         0
  versicolor      2         36        12
  virginica       0         41         9

n = 6, k = 80
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      2         38        10
  virginica       0         28        22

n = 7, k = 80
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         36        13
  virginica       0         21        29

n = 8, k = 80
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         42         7
  virginica       0         14        36

n = 9, k = 80
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         42         7
  virginica       0         14        36

n = 10, k = 80
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0         13        37

n = 2, k = 81
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 81
            knn.fit
label        setosa versicolor virginica
  setosa         19         31         0
  versicolor      2         28        20
  virginica       0         39        11

n = 4, k = 81
            knn.fit
label        setosa versicolor virginica
  setosa         40         10         0
  versicolor      0         32        18
  virginica       0         29        21

n = 5, k = 81
            knn.fit
label        setosa versicolor virginica
  setosa         36         14         0
  versicolor      2         36        12
  virginica       0         40        10

n = 6, k = 81
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      2         37        11
  virginica       0         37        13

n = 7, k = 81
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         36        13
  virginica       0         27        23

n = 8, k = 81
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         40         9
  virginica       0         12        38

n = 9, k = 81
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         39        10
  virginica       0         14        36

n = 10, k = 81
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         40         9
  virginica       0         13        37

n = 2, k = 82
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 82
            knn.fit
label        setosa versicolor virginica
  setosa         17         33         0
  versicolor      3         27        20
  virginica       0         39        11

n = 4, k = 82
            knn.fit
label        setosa versicolor virginica
  setosa         37         13         0
  versicolor      1         30        19
  virginica       0         29        21

n = 5, k = 82
            knn.fit
label        setosa versicolor virginica
  setosa         33         17         0
  versicolor      2         35        13
  virginica       0         40        10

n = 6, k = 82
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      2         37        11
  virginica       0         35        15

n = 7, k = 82
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         37        12
  virginica       0         27        23

n = 8, k = 82
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         42         7
  virginica       0         21        29

n = 9, k = 82
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         39        10
  virginica       0         15        35

n = 10, k = 82
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         39        10
  virginica       0         13        37

n = 2, k = 83
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 83
            knn.fit
label        setosa versicolor virginica
  setosa         17         33         0
  versicolor      2         28        20
  virginica       0         39        11

n = 4, k = 83
            knn.fit
label        setosa versicolor virginica
  setosa         35         15         0
  versicolor      1         30        19
  virginica       0         29        21

n = 5, k = 83
            knn.fit
label        setosa versicolor virginica
  setosa         29         21         0
  versicolor      2         35        13
  virginica       0         40        10

n = 6, k = 83
            knn.fit
label        setosa versicolor virginica
  setosa         48          2         0
  versicolor      2         36        12
  virginica       0         34        16

n = 7, k = 83
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         37        12
  virginica       0         27        23

n = 8, k = 83
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         41         8
  virginica       0         21        29

n = 9, k = 83
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         39        10
  virginica       0         18        32

n = 10, k = 83
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         38        11
  virginica       0         12        38

n = 2, k = 84
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 84
            knn.fit
label        setosa versicolor virginica
  setosa         17         33         0
  versicolor      2         28        20
  virginica       0         39        11

n = 4, k = 84
            knn.fit
label        setosa versicolor virginica
  setosa         34         16         0
  versicolor      1         30        19
  virginica       0         29        21

n = 5, k = 84
            knn.fit
label        setosa versicolor virginica
  setosa         28         22         0
  versicolor      2         35        13
  virginica       0         40        10

n = 6, k = 84
            knn.fit
label        setosa versicolor virginica
  setosa         48          2         0
  versicolor      2         36        12
  virginica       0         36        14

n = 7, k = 84
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         34        15
  virginica       0         25        25

n = 8, k = 84
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         43         6
  virginica       0         23        27

n = 9, k = 84
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         36        13
  virginica       0         21        29

n = 10, k = 84
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      2         38        10
  virginica       0         12        38

n = 2, k = 85
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 85
            knn.fit
label        setosa versicolor virginica
  setosa         17         33         0
  versicolor      2         28        20
  virginica       0         39        11

n = 4, k = 85
            knn.fit
label        setosa versicolor virginica
  setosa         31         19         0
  versicolor      1         30        19
  virginica       0         29        21

n = 5, k = 85
            knn.fit
label        setosa versicolor virginica
  setosa         24         26         0
  versicolor      2         35        13
  virginica       0         40        10

n = 6, k = 85
            knn.fit
label        setosa versicolor virginica
  setosa         44          6         0
  versicolor      2         36        12
  virginica       0         39        11

n = 7, k = 85
            knn.fit
label        setosa versicolor virginica
  setosa         48          2         0
  versicolor      1         35        14
  virginica       0         24        26

n = 8, k = 85
            knn.fit
label        setosa versicolor virginica
  setosa         47          3         0
  versicolor      1         40         9
  virginica       0         26        24

n = 9, k = 85
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      1         36        13
  virginica       0         22        28

n = 10, k = 85
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      2         38        10
  virginica       0         13        37

n = 2, k = 86
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 86
            knn.fit
label        setosa versicolor virginica
  setosa         17         33         0
  versicolor      2         28        20
  virginica       0         39        11

n = 4, k = 86
            knn.fit
label        setosa versicolor virginica
  setosa         27         23         0
  versicolor      2         29        19
  virginica       0         29        21

n = 5, k = 86
            knn.fit
label        setosa versicolor virginica
  setosa         22         28         0
  versicolor      2         35        13
  virginica       0         40        10

n = 6, k = 86
            knn.fit
label        setosa versicolor virginica
  setosa         41          9         0
  versicolor      2         36        12
  virginica       0         39        11

n = 7, k = 86
            knn.fit
label        setosa versicolor virginica
  setosa         47          3         0
  versicolor      1         33        16
  virginica       0         24        26

n = 8, k = 86
            knn.fit
label        setosa versicolor virginica
  setosa         46          4         0
  versicolor      1         37        12
  virginica       0         29        21

n = 9, k = 86
            knn.fit
label        setosa versicolor virginica
  setosa         49          1         0
  versicolor      1         36        13
  virginica       0         29        21

n = 10, k = 86
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      2         38        10
  virginica       0         17        33

n = 2, k = 87
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 87
            knn.fit
label        setosa versicolor virginica
  setosa         17         33         0
  versicolor      2         28        20
  virginica       0         39        11

n = 4, k = 87
            knn.fit
label        setosa versicolor virginica
  setosa         26         24         0
  versicolor      2         29        19
  virginica       0         29        21

n = 5, k = 87
            knn.fit
label        setosa versicolor virginica
  setosa         21         29         0
  versicolor      2         35        13
  virginica       0         40        10

n = 6, k = 87
            knn.fit
label        setosa versicolor virginica
  setosa         38         12         0
  versicolor      2         36        12
  virginica       0         39        11

n = 7, k = 87
            knn.fit
label        setosa versicolor virginica
  setosa         45          5         0
  versicolor      1         32        17
  virginica       0         23        27

n = 8, k = 87
            knn.fit
label        setosa versicolor virginica
  setosa         43          7         0
  versicolor      1         38        11
  virginica       0         37        13

n = 9, k = 87
            knn.fit
label        setosa versicolor virginica
  setosa         46          4         0
  versicolor      1         36        13
  virginica       0         35        15

n = 10, k = 87
            knn.fit
label        setosa versicolor virginica
  setosa         50          0         0
  versicolor      2         38        10
  virginica       0         20        30

n = 2, k = 88
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 88
            knn.fit
label        setosa versicolor virginica
  setosa         17         33         0
  versicolor      2         28        20
  virginica       0         39        11

n = 4, k = 88
            knn.fit
label        setosa versicolor virginica
  setosa         26         24         0
  versicolor      1         30        19
  virginica       0         29        21

n = 5, k = 88
            knn.fit
label        setosa versicolor virginica
  setosa         19         31         0
  versicolor      2         35        13
  virginica       0         40        10

n = 6, k = 88
            knn.fit
label        setosa versicolor virginica
  setosa         35         15         0
  versicolor      2         36        12
  virginica       0         39        11

n = 7, k = 88
            knn.fit
label        setosa versicolor virginica
  setosa         43          7         0
  versicolor      2         30        18
  virginica       0         23        27

n = 8, k = 88
            knn.fit
label        setosa versicolor virginica
  setosa         43          7         0
  versicolor      1         38        11
  virginica       0         35        15

n = 9, k = 88
            knn.fit
label        setosa versicolor virginica
  setosa         44          6         0
  versicolor      1         34        15
  virginica       0         34        16

n = 10, k = 88
            knn.fit
label        setosa versicolor virginica
  setosa         49          1         0
  versicolor      2         37        11
  virginica       0         29        21

n = 2, k = 89
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 89
            knn.fit
label        setosa versicolor virginica
  setosa         17         33         0
  versicolor      1         28        21
  virginica       0         39        11

n = 4, k = 89
            knn.fit
label        setosa versicolor virginica
  setosa         26         24         0
  versicolor      1         30        19
  virginica       0         29        21

n = 5, k = 89
            knn.fit
label        setosa versicolor virginica
  setosa         19         31         0
  versicolor      2         35        13
  virginica       0         40        10

n = 6, k = 89
            knn.fit
label        setosa versicolor virginica
  setosa         35         15         0
  versicolor      2         36        12
  virginica       0         39        11

n = 7, k = 89
            knn.fit
label        setosa versicolor virginica
  setosa         37         13         0
  versicolor      1         30        19
  virginica       0         23        27

n = 8, k = 89
            knn.fit
label        setosa versicolor virginica
  setosa         42          8         0
  versicolor      1         37        12
  virginica       0         36        14

n = 9, k = 89
            knn.fit
label        setosa versicolor virginica
  setosa         43          7         0
  versicolor      1         33        16
  virginica       0         34        16

n = 10, k = 89
            knn.fit
label        setosa versicolor virginica
  setosa         46          4         0
  versicolor      2         38        10
  virginica       0         36        14

n = 2, k = 90
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 90
            knn.fit
label        setosa versicolor virginica
  setosa         17         33         0
  versicolor      2         28        20
  virginica       0         39        11

n = 4, k = 90
            knn.fit
label        setosa versicolor virginica
  setosa         26         24         0
  versicolor      1         30        19
  virginica       0         29        21

n = 5, k = 90
            knn.fit
label        setosa versicolor virginica
  setosa         19         31         0
  versicolor      2         35        13
  virginica       0         40        10

n = 6, k = 90
            knn.fit
label        setosa versicolor virginica
  setosa         33         17         0
  versicolor      2         36        12
  virginica       0         39        11

n = 7, k = 90
            knn.fit
label        setosa versicolor virginica
  setosa         36         14         0
  versicolor      1         30        19
  virginica       0         23        27

n = 8, k = 90
            knn.fit
label        setosa versicolor virginica
  setosa         41          9         0
  versicolor      1         38        11
  virginica       0         34        16

n = 9, k = 90
            knn.fit
label        setosa versicolor virginica
  setosa         42          8         0
  versicolor      1         32        17
  virginica       0         32        18

n = 10, k = 90
            knn.fit
label        setosa versicolor virginica
  setosa         45          5         0
  versicolor      2         35        13
  virginica       0         36        14

n = 2, k = 91
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 91
            knn.fit
label        setosa versicolor virginica
  setosa         17         33         0
  versicolor      1         28        21
  virginica       0         39        11

n = 4, k = 91
            knn.fit
label        setosa versicolor virginica
  setosa         26         24         0
  versicolor      1         30        19
  virginica       0         29        21

n = 5, k = 91
            knn.fit
label        setosa versicolor virginica
  setosa         17         33         0
  versicolor      2         35        13
  virginica       0         40        10

n = 6, k = 91
            knn.fit
label        setosa versicolor virginica
  setosa         30         20         0
  versicolor      2         36        12
  virginica       0         39        11

n = 7, k = 91
            knn.fit
label        setosa versicolor virginica
  setosa         34         16         0
  versicolor      2         28        20
  virginica       0         23        27

n = 8, k = 91
            knn.fit
label        setosa versicolor virginica
  setosa         38         12         0
  versicolor      1         37        12
  virginica       0         35        15

n = 9, k = 91
            knn.fit
label        setosa versicolor virginica
  setosa         40         10         0
  versicolor      1         31        18
  virginica       0         32        18

n = 10, k = 91
            knn.fit
label        setosa versicolor virginica
  setosa         44          6         0
  versicolor      2         38        10
  virginica       0         37        13

n = 2, k = 92
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 92
            knn.fit
label        setosa versicolor virginica
  setosa         17         33         0
  versicolor      1         28        21
  virginica       0         39        11

n = 4, k = 92
            knn.fit
label        setosa versicolor virginica
  setosa         26         24         0
  versicolor      1         29        20
  virginica       0         29        21

n = 5, k = 92
            knn.fit
label        setosa versicolor virginica
  setosa         17         33         0
  versicolor      2         34        14
  virginica       0         40        10

n = 6, k = 92
            knn.fit
label        setosa versicolor virginica
  setosa         28         22         0
  versicolor      2         36        12
  virginica       0         39        11

n = 7, k = 92
            knn.fit
label        setosa versicolor virginica
  setosa         34         16         0
  versicolor      2         27        21
  virginica       0         23        27

n = 8, k = 92
            knn.fit
label        setosa versicolor virginica
  setosa         36         14         0
  versicolor      1         37        12
  virginica       0         35        15

n = 9, k = 92
            knn.fit
label        setosa versicolor virginica
  setosa         31         19         0
  versicolor      1         31        18
  virginica       0         32        18

n = 10, k = 92
            knn.fit
label        setosa versicolor virginica
  setosa         40         10         0
  versicolor      2         35        13
  virginica       0         38        12

n = 2, k = 93
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 93
            knn.fit
label        setosa versicolor virginica
  setosa         17         33         0
  versicolor      2         28        20
  virginica       0         39        11

n = 4, k = 93
            knn.fit
label        setosa versicolor virginica
  setosa         26         24         0
  versicolor      1         29        20
  virginica       0         29        21

n = 5, k = 93
            knn.fit
label        setosa versicolor virginica
  setosa         17         33         0
  versicolor      2         34        14
  virginica       0         40        10

n = 6, k = 93
            knn.fit
label        setosa versicolor virginica
  setosa         27         23         0
  versicolor      2         36        12
  virginica       0         39        11

n = 7, k = 93
            knn.fit
label        setosa versicolor virginica
  setosa         30         20         0
  versicolor      1         28        21
  virginica       0         23        27

n = 8, k = 93
            knn.fit
label        setosa versicolor virginica
  setosa         35         15         0
  versicolor      1         37        12
  virginica       0         35        15

n = 9, k = 93
            knn.fit
label        setosa versicolor virginica
  setosa         27         23         0
  versicolor      1         31        18
  virginica       0         32        18

n = 10, k = 93
            knn.fit
label        setosa versicolor virginica
  setosa         36         14         0
  versicolor      2         36        12
  virginica       0         36        14

n = 2, k = 94
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 94
            knn.fit
label        setosa versicolor virginica
  setosa          6         33        11
  versicolor      1         28        21
  virginica       0         39        11

n = 4, k = 94
            knn.fit
label        setosa versicolor virginica
  setosa         26         24         0
  versicolor      1         29        20
  virginica       0         29        21

n = 5, k = 94
            knn.fit
label        setosa versicolor virginica
  setosa         17         33         0
  versicolor      2         34        14
  virginica       0         40        10

n = 6, k = 94
            knn.fit
label        setosa versicolor virginica
  setosa         28         22         0
  versicolor      2         34        14
  virginica       0         39        11

n = 7, k = 94
            knn.fit
label        setosa versicolor virginica
  setosa         29         21         0
  versicolor      1         28        21
  virginica       0         23        27

n = 8, k = 94
            knn.fit
label        setosa versicolor virginica
  setosa         35         15         0
  versicolor      1         37        12
  virginica       0         34        16

n = 9, k = 94
            knn.fit
label        setosa versicolor virginica
  setosa         25         25         0
  versicolor      1         31        18
  virginica       0         32        18

n = 10, k = 94
            knn.fit
label        setosa versicolor virginica
  setosa         32         18         0
  versicolor      2         33        15
  virginica       0         37        13

n = 2, k = 95
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 95
            knn.fit
label        setosa versicolor virginica
  setosa          0         33        17
  versicolor      0         28        22
  virginica       0         39        11

n = 4, k = 95
            knn.fit
label        setosa versicolor virginica
  setosa         26         24         0
  versicolor      1         29        20
  virginica       0         29        21

n = 5, k = 95
            knn.fit
label        setosa versicolor virginica
  setosa         17         33         0
  versicolor      2         34        14
  virginica       0         40        10

n = 6, k = 95
            knn.fit
label        setosa versicolor virginica
  setosa         26         24         0
  versicolor      2         34        14
  virginica       0         39        11

n = 7, k = 95
            knn.fit
label        setosa versicolor virginica
  setosa         23         27         0
  versicolor      1         28        21
  virginica       0         23        27

n = 8, k = 95
            knn.fit
label        setosa versicolor virginica
  setosa         32         18         0
  versicolor      1         37        12
  virginica       0         33        17

n = 9, k = 95
            knn.fit
label        setosa versicolor virginica
  setosa         23         27         0
  versicolor      1         31        18
  virginica       0         32        18

n = 10, k = 95
            knn.fit
label        setosa versicolor virginica
  setosa         29         21         0
  versicolor      2         34        14
  virginica       0         37        13

n = 2, k = 96
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 96
            knn.fit
label        setosa versicolor virginica
  setosa          0         33        17
  versicolor      0         28        22
  virginica       0         39        11

n = 4, k = 96
            knn.fit
label        setosa versicolor virginica
  setosa         26         24         0
  versicolor      1         29        20
  virginica       0         29        21

n = 5, k = 96
            knn.fit
label        setosa versicolor virginica
  setosa         17         33         0
  versicolor      3         33        14
  virginica       0         40        10

n = 6, k = 96
            knn.fit
label        setosa versicolor virginica
  setosa         23         27         0
  versicolor      2         34        14
  virginica       0         39        11

n = 7, k = 96
            knn.fit
label        setosa versicolor virginica
  setosa         22         28         0
  versicolor      1         28        21
  virginica       0         23        27

n = 8, k = 96
            knn.fit
label        setosa versicolor virginica
  setosa         31         19         0
  versicolor      1         37        12
  virginica       0         33        17

n = 9, k = 96
            knn.fit
label        setosa versicolor virginica
  setosa         23         27         0
  versicolor      1         31        18
  virginica       0         32        18

n = 10, k = 96
            knn.fit
label        setosa versicolor virginica
  setosa         29         21         0
  versicolor      2         34        14
  virginica       0         36        14

n = 2, k = 97
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 97
            knn.fit
label        setosa versicolor virginica
  setosa          0         33        17
  versicolor      0         28        22
  virginica       0         39        11

n = 4, k = 97
            knn.fit
label        setosa versicolor virginica
  setosa         26         24         0
  versicolor      1         29        20
  virginica       0         29        21

n = 5, k = 97
            knn.fit
label        setosa versicolor virginica
  setosa         17         33         0
  versicolor      3         33        14
  virginica       0         40        10

n = 6, k = 97
            knn.fit
label        setosa versicolor virginica
  setosa         23         27         0
  versicolor      2         34        14
  virginica       0         39        11

n = 7, k = 97
            knn.fit
label        setosa versicolor virginica
  setosa         22         28         0
  versicolor      1         28        21
  virginica       0         23        27

n = 8, k = 97
            knn.fit
label        setosa versicolor virginica
  setosa         31         19         0
  versicolor      1         37        12
  virginica       0         33        17

n = 9, k = 97
            knn.fit
label        setosa versicolor virginica
  setosa         22         28         0
  versicolor      1         30        19
  virginica       0         32        18

n = 10, k = 97
            knn.fit
label        setosa versicolor virginica
  setosa         27         23         0
  versicolor      2         34        14
  virginica       0         36        14

n = 2, k = 98
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 98
            knn.fit
label        setosa versicolor virginica
  setosa          0         33        17
  versicolor      0         28        22
  virginica       0         39        11

n = 4, k = 98
            knn.fit
label        setosa versicolor virginica
  setosa         26         24         0
  versicolor      2         28        20
  virginica       0         29        21

n = 5, k = 98
            knn.fit
label        setosa versicolor virginica
  setosa         17         33         0
  versicolor      3         33        14
  virginica       0         40        10

n = 6, k = 98
            knn.fit
label        setosa versicolor virginica
  setosa         23         27         0
  versicolor      2         34        14
  virginica       0         39        11

n = 7, k = 98
            knn.fit
label        setosa versicolor virginica
  setosa         21         29         0
  versicolor      1         27        22
  virginica       0         23        27

n = 8, k = 98
            knn.fit
label        setosa versicolor virginica
  setosa         27         23         0
  versicolor      1         37        12
  virginica       0         33        17

n = 9, k = 98
            knn.fit
label        setosa versicolor virginica
  setosa         18         32         0
  versicolor      1         30        19
  virginica       0         32        18

n = 10, k = 98
            knn.fit
label        setosa versicolor virginica
  setosa         27         23         0
  versicolor      2         34        14
  virginica       0         36        14

n = 2, k = 99
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 99
            knn.fit
label        setosa versicolor virginica
  setosa          0         33        17
  versicolor      0         28        22
  virginica       0         39        11

n = 4, k = 99
            knn.fit
label        setosa versicolor virginica
  setosa         26         24         0
  versicolor      2         28        20
  virginica       0         29        21

n = 5, k = 99
            knn.fit
label        setosa versicolor virginica
  setosa         17         33         0
  versicolor      3         33        14
  virginica       0         40        10

n = 6, k = 99
            knn.fit
label        setosa versicolor virginica
  setosa         23         27         0
  versicolor      2         34        14
  virginica       0         39        11

n = 7, k = 99
            knn.fit
label        setosa versicolor virginica
  setosa         21         29         0
  versicolor      1         26        23
  virginica       0         23        27

n = 8, k = 99
            knn.fit
label        setosa versicolor virginica
  setosa         27         23         0
  versicolor      1         36        13
  virginica       0         34        16

n = 9, k = 99
            knn.fit
label        setosa versicolor virginica
  setosa         16         34         0
  versicolor      1         30        19
  virginica       0         32        18

n = 10, k = 99
            knn.fit
label        setosa versicolor virginica
  setosa         23         27         0
  versicolor      2         34        14
  virginica       0         37        13

n = 2, k = 100
            knn.fit
label        setosa versicolor virginica
  setosa          0         24        26
  versicolor      0         22        28
  virginica       0         29        21

n = 3, k = 100
            knn.fit
label        setosa versicolor virginica
  setosa          0         33        17
  versicolor      0         28        22
  virginica       0         39        11

n = 4, k = 100
            knn.fit
label        setosa versicolor virginica
  setosa         26         24         0
  versicolor      2         27        21
  virginica       0         29        21

n = 5, k = 100
            knn.fit
label        setosa versicolor virginica
  setosa         17         33         0
  versicolor      3         33        14
  virginica       0         40        10

n = 6, k = 100
            knn.fit
label        setosa versicolor virginica
  setosa         23         27         0
  versicolor      2         34        14
  virginica       0         39        11

n = 7, k = 100
            knn.fit
label        setosa versicolor virginica
  setosa         22         28         0
  versicolor      1         26        23
  virginica       0         23        27

n = 8, k = 100
            knn.fit
label        setosa versicolor virginica
  setosa         26         24         0
  versicolor      1         36        13
  virginica       0         34        16

n = 9, k = 100
            knn.fit
label        setosa versicolor virginica
  setosa         16         34         0
  versicolor      1         29        20
  virginica       0         32        18

n = 10, k = 100
            knn.fit
label        setosa versicolor virginica
  setosa         23         27         0
  versicolor      2         35        13
  virginica       0         34        16
There were 50 or more warnings (use warnings() to see the first 50)
> 
> warnings()
Warning messages:
1: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 76 exceeds number 75 of patterns
2: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 76 exceeds number 75 of patterns
3: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 77 exceeds number 75 of patterns
4: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 77 exceeds number 75 of patterns
5: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 78 exceeds number 75 of patterns
6: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 78 exceeds number 75 of patterns
7: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 79 exceeds number 75 of patterns
8: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 79 exceeds number 75 of patterns
9: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 80 exceeds number 75 of patterns
10: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 80 exceeds number 75 of patterns
11: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 81 exceeds number 75 of patterns
12: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 81 exceeds number 75 of patterns
13: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 82 exceeds number 75 of patterns
14: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 82 exceeds number 75 of patterns
15: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 83 exceeds number 75 of patterns
16: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 83 exceeds number 75 of patterns
17: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 84 exceeds number 75 of patterns
18: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 84 exceeds number 75 of patterns
19: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 85 exceeds number 75 of patterns
20: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 85 exceeds number 75 of patterns
21: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 86 exceeds number 75 of patterns
22: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 86 exceeds number 75 of patterns
23: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 87 exceeds number 75 of patterns
24: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 87 exceeds number 75 of patterns
25: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 88 exceeds number 75 of patterns
26: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 88 exceeds number 75 of patterns
27: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 89 exceeds number 75 of patterns
28: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 89 exceeds number 75 of patterns
29: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 90 exceeds number 75 of patterns
30: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 90 exceeds number 75 of patterns
31: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 91 exceeds number 75 of patterns
32: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 91 exceeds number 75 of patterns
33: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 92 exceeds number 75 of patterns
34: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 92 exceeds number 75 of patterns
35: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 93 exceeds number 75 of patterns
36: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 93 exceeds number 75 of patterns
37: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 94 exceeds number 75 of patterns
38: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 94 exceeds number 75 of patterns
39: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 95 exceeds number 75 of patterns
40: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 95 exceeds number 75 of patterns
41: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 96 exceeds number 75 of patterns
42: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 96 exceeds number 75 of patterns
43: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 97 exceeds number 75 of patterns
44: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 97 exceeds number 75 of patterns
45: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 98 exceeds number 75 of patterns
46: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 98 exceeds number 75 of patterns
47: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 99 exceeds number 75 of patterns
48: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 99 exceeds number 75 of patterns
49: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 100 exceeds number 75 of patterns
50: In knn(train = train.data, test = test.data, cl = train.labels,  ... :
  k = 100 exceeds number 75 of patterns
> results <- data.frame(rep(2:max.n, max.k), err.rates)
> names(results) <- c('n', 'err.rate', 'k')
> results$k <- as.factor(results$k)
> title <- paste('knn results')
> 
> results.plot <- ggplot(results, aes(x=n, y=err.rate)) + geom_line(aes(colour=k))
> results.plot <- results.plot + ggtitle(title)
> results$n <- as.factor(results$n)
> results$k <- as.numeric(results$k)
> results.plot2 <- ggplot(results, aes(x=k, y=err.rate)) + geom_line(aes(colour=n))
> grid.arrange(results.plot, results.plot2)
> 
> head(err.rates)
          V1 V2
1 0.07333333  1
2 0.04666667  1
3 0.04666667  1
4 0.05333333  1
5 0.04666667  1
6 0.04666667  1
> 
> nrow(err.rates)
[1] 900
> 
> g.err <- mean(err.rates$V1)
> 
> paste(g.err)
[1] "0.188340740740741"
> 
> # n-fold generalisation error = average over all iterations
> 
> 