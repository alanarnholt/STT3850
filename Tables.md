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


To create a Table with caption we need to first load the `xtable` package.


```r
library(xtable)
print(xtable(T1, caption = "You put something here"), type = "html")
```

<!-- html table generated in R 3.0.2 by xtable 1.7-1 package -->
<!-- Thu Jan 30 09:11:36 2014 -->
<TABLE border=1>
<CAPTION ALIGN="bottom"> You put something here </CAPTION>
<TR> <TH>  </TH> <TH> Difficult </TH> <TH> Easy </TH> <TH> Impossible </TH>  </TR>
  <TR> <TD align="right"> Hamstring Stretch </TD> <TD align="right"> 63.00 </TD> <TD align="right"> 100.00 </TD> <TD align="right"> 8.00 </TD> </TR>
  <TR> <TD align="right"> Traditional Sitting </TD> <TD align="right"> 51.00 </TD> <TD align="right"> 107.00 </TD> <TD align="right"> 13.00 </TD> </TR>
   </TABLE>


