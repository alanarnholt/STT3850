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

<!-- html table generated in R 3.0.2 by xtable 1.7-1 package -->
<!-- Thu Jan 30 13:15:48 2014 -->
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
##                 A      B     C      D
## Difficult   89.65  91.93 98.62  96.67
## Easy        78.10  79.04 79.25  79.53
## Impossible 124.00 111.00    NA 124.00
```



<!-- html table generated in R 3.0.2 by xtable 1.7-1 package -->
<!-- Thu Jan 30 13:15:48 2014 -->
<TABLE border=1>
<CAPTION ALIGN="bottom"> Table 2: Put Something Here </CAPTION>
<TR> <TH>  </TH> <TH> A </TH> <TH> B </TH> <TH> C </TH> <TH> D </TH>  </TR>
  <TR> <TD align="right"> Difficult </TD> <TD align="right"> 89.65 </TD> <TD align="right"> 91.93 </TD> <TD align="right"> 98.62 </TD> <TD align="right"> 96.67 </TD> </TR>
  <TR> <TD align="right"> Easy </TD> <TD align="right"> 78.10 </TD> <TD align="right"> 79.04 </TD> <TD align="right"> 79.25 </TD> <TD align="right"> 79.53 </TD> </TR>
  <TR> <TD align="right"> Impossible </TD> <TD align="right"> 124.00 </TD> <TD align="right"> 111.00 </TD> <TD align="right">  </TD> <TD align="right"> 124.00 </TD> </TR>
   </TABLE>

