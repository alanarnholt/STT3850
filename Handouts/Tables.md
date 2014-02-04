# Working with Tables and `xtable`

## Alan Arnholt

Suppose we have some results we would like to display in a table such as
the results from the following code chunk:


```
                     Ease
Treatment             Difficult Easy Impossible
  Hamstring Stretch          63  100          8
  Traditional Sitting        51  107         13
```


To create a Table with `xtable`, first load the `xtable` package.


```r
library(xtable)
print(xtable(T1, caption = "Table 1: Treatment versus Ease"), type = "html")
```

<!-- html table generated in R 3.0.1 by xtable 1.7-1 package -->
<!-- Tue Feb 04 11:29:15 2014 -->
<TABLE border=1>
<CAPTION ALIGN="bottom"> Table 1: Treatment versus Ease </CAPTION>
<TR> <TH>  </TH> <TH> Difficult </TH> <TH> Easy </TH> <TH> Impossible </TH>  </TR>
  <TR> <TD align="right"> Hamstring Stretch </TD> <TD align="right"> 63.00 </TD> <TD align="right"> 100.00 </TD> <TD align="right"> 8.00 </TD> </TR>
  <TR> <TD align="right"> Traditional Sitting </TD> <TD align="right"> 51.00 </TD> <TD align="right"> 107.00 </TD> <TD align="right"> 13.00 </TD> </TR>
   </TABLE>



```r
tapply(EPIDURALf$kg, list(EPIDURALf$Ease, EPIDURALf$Doctor), mean)
```

```
                A      B     C      D
Difficult   89.65  91.93 98.62  96.67
Easy        78.10  79.04 79.25  79.53
Impossible 124.00 111.00    NA 124.00
```



<!-- html table generated in R 3.0.1 by xtable 1.7-1 package -->
<!-- Tue Feb 04 11:29:15 2014 -->
<TABLE border=1>
<CAPTION ALIGN="bottom"> Table 2: Pithy Caption---Cease work on that superflous banter and pious jargon </CAPTION>
<TR> <TH>  </TH> <TH> A </TH> <TH> B </TH> <TH> C </TH> <TH> D </TH>  </TR>
  <TR> <TD align="right"> Difficult </TD> <TD align="right"> 89.65 </TD> <TD align="right"> 91.93 </TD> <TD align="right"> 98.62 </TD> <TD align="right"> 96.67 </TD> </TR>
  <TR> <TD align="right"> Easy </TD> <TD align="right"> 78.10 </TD> <TD align="right"> 79.04 </TD> <TD align="right"> 79.25 </TD> <TD align="right"> 79.53 </TD> </TR>
  <TR> <TD align="right"> Impossible </TD> <TD align="right"> 124.00 </TD> <TD align="right"> 111.00 </TD> <TD align="right">  </TD> <TD align="right"> 124.00 </TD> </TR>
   </TABLE>

Suppose we want the results from a regression.


```r
summary(lm(cm ~ kg, data = EPIDURALf))$coefficients
```

```
            Estimate Std. Error t value   Pr(>|t|)
(Intercept) 153.4443    1.62250  94.573 3.022e-246
kg            0.1335    0.01844   7.239  3.034e-12
```


<!-- html table generated in R 3.0.1 by xtable 1.7-1 package -->
<!-- Tue Feb 04 11:29:15 2014 -->
<TABLE border=1>
<CAPTION ALIGN="bottom"> Table 3: Put Something Here </CAPTION>
<TR> <TH>  </TH> <TH> Estimate </TH> <TH> Std. Error </TH> <TH> t value </TH> <TH> Pr(&gt |t|) </TH>  </TR>
  <TR> <TD align="right"> (Intercept) </TD> <TD align="right"> 153.44 </TD> <TD align="right"> 1.62 </TD> <TD align="right"> 94.57 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> kg </TD> <TD align="right"> 0.13 </TD> <TD align="right"> 0.02 </TD> <TD align="right"> 7.24 </TD> <TD align="right"> 0.00 </TD> </TR>
   </TABLE>


Next we have an ANOVA table:

<!-- html table generated in R 3.0.1 by xtable 1.7-1 package -->
<!-- Tue Feb 04 11:29:15 2014 -->
<TABLE border=1>
<CAPTION ALIGN="bottom"> Table 4: Put Something Here Again </CAPTION>
<TR> <TH>  </TH> <TH> Df </TH> <TH> Sum Sq </TH> <TH> Mean Sq </TH> <TH> F value </TH> <TH> Pr(&gt F) </TH>  </TR>
  <TR> <TD> Ease </TD> <TD align="right"> 2.0000 </TD> <TD align="right"> 49.5102 </TD> <TD align="right"> 24.7551 </TD> <TD align="right"> 0.6132 </TD> <TD align="right"> 0.5422 </TD> </TR>
  <TR> <TD> Residuals </TD> <TD align="right"> 339.0000 </TD> <TD align="right"> 13685.9167 </TD> <TD align="right"> 40.3714 </TD> <TD align="right">  </TD> <TD align="right">  </TD> </TR>
   </TABLE>

