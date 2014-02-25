## Notes
### Alan Arnholt

Consider the following data


```r
stuff <- c(110, 277, 50, 163, 302, 63)
mat <- matrix(data = stuff, nrow = 2, byrow = TRUE)
mat
```

```
     [,1] [,2] [,3]
[1,]  110  277   50
[2,]  163  302   63
```

```r
dimnames(mat) <- list(Gender = c("Male", "Female"), Happy = c("Very Happy", "Pretty Happy", "Not to Happy"))
mat
```

```
        Happy
Gender   Very Happy Pretty Happy Not to Happy
  Male          110          277           50
  Female        163          302           63
```

```r
addmargins(mat)
```

```
        Happy
Gender   Very Happy Pretty Happy Not to Happy Sum
  Male          110          277           50 437
  Female        163          302           63 528
  Sum           273          579          113 965
```

```r
prop.table(mat)
```

```
        Happy
Gender   Very Happy Pretty Happy Not to Happy
  Male       0.1140        0.287      0.05181
  Female     0.1689        0.313      0.06528
```

```r
addmargins(prop.table(mat))
```

```
        Happy
Gender   Very Happy Pretty Happy Not to Happy    Sum
  Male       0.1140        0.287      0.05181 0.4528
  Female     0.1689        0.313      0.06528 0.5472
  Sum        0.2829        0.600      0.11710 1.0000
```

```r
110/965
```

```
[1] 0.114
```

```r
.4528497*.2829016
```

```
[1] 0.1281
```



```r
chisq.test(mat)
```

```

	Pearson's Chi-squared test

data:  mat
X-squared = 4.322, df = 2, p-value = 0.1152
```

```r
chisq.test(mat)$obs
```

```
        Happy
Gender   Very Happy Pretty Happy Not to Happy
  Male          110          277           50
  Female        163          302           63
```

```r
chisq.test(mat)$exp
```

```
        Happy
Gender   Very Happy Pretty Happy Not to Happy
  Male        123.6        262.2        51.17
  Female      149.4        316.8        61.83
```

```r
chisq.test(mat)$obs - chisq.test(mat)$exp
```

```
        Happy
Gender   Very Happy Pretty Happy Not to Happy
  Male       -13.63         14.8       -1.172
  Female      13.63        -14.8        1.172
```

```r
sum((chisq.test(mat)$obs - chisq.test(mat)$exp)^2/chisq.test(mat)$exp)
```

```
[1] 4.321
```

## Permutation Test


```r
maT <- as.table(mat)
MAT <- as.data.frame(maT)
MATflat <- vcdExtra::expand.dft(MAT)
head(MATflat)
```

```
  Gender      Happy
1   Male Very Happy
2   Male Very Happy
3   Male Very Happy
4   Male Very Happy
5   Male Very Happy
6   Male Very Happy
```

```r
set.seed(2)
N <- 10^4 - 1  # Change this for slower computers
result <- numeric(N)
for (i in 1:N) {
    T2 <- xtabs(~sample(Gender) + Happy, data = MATflat)
    result[i] <- chisq.test(T2)$statistic
}
obs <- chisq.test(xtabs(~Gender + Happy, data = MATflat))$statistic
pvalue <- (sum(result >= obs) + 1)/(N + 1)
pvalue
```

```
[1] 0.1192
```

