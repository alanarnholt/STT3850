## Alan Arnholt

### Working with Data





This section presents a data set that shows how different data types should be read into `R` as well as several functions that are useful for working with different types of `R` objects.  Consider the data stored as a CSV file at

http://www1.appstate.edu/~arnholta/PASWR/CD/data/Poplar3.CSV


The following description of the data is from `Minitab 15`:

```
In an effort to maximize yield, researchers designed an experiment to determine how two factors, Site and Treatment, influence the Weight of four-year-old poplar clones. They planted trees on two sites: Site 1 is a moist site with rich soil, and Site 2 is a dry, sandy site. They applied four different treatments to the trees: Treatment 1 was the control (no treatment);   Treatment 2 used fertilizer; Treatment 3 used irrigation; and Treatment 4 use both fertilizer and irrigation. To account for a variety of weather conditions, the researchers replicated the data by planting half the trees in Year 1, and the other half in Year 2.
```

The data from Poplar3.CSV is read into the data frame poplar using the `read.csv()` function, and the first five rows of the data frame are shown using the function `head()` with the argument `n = 5` to show the first five rows of the data frame instead of the default `n = 6` rows.



```r
site <- "http://www1.appstate.edu/~arnholta/PASWR/CD/data/Poplar3.CSV"
poplar <- read.csv(file = url(site))
head(poplar, n = 5)  # show first five rows
```

```
  Site Year Treatment Diameter Height Weight Age
1    1    1         1     2.23   3.76   0.17   3
2    1    1         1     2.12   3.15   0.15   3
3    1    1         1     1.06   1.85   0.02   3
4    1    1         1     2.12   3.64   0.16   3
5    1    1         1     2.99   4.64   0.37   3
```


When dealing with imported data sets, it is always good to examine their contents using functions such as `str()` and `summary()`, which show the structure and provide appropriate summaries, respectively, for different types of objects.


```r
str(poplar)
```

```
'data.frame':	298 obs. of  7 variables:
 $ Site     : int  1 1 1 1 1 1 1 1 1 2 ...
 $ Year     : int  1 1 1 1 1 1 1 1 1 1 ...
 $ Treatment: int  1 1 1 1 1 1 1 1 1 1 ...
 $ Diameter : num  2.23 2.12 1.06 2.12 2.99 4.01 2.41 2.75 2.2 4.09 ...
 $ Height   : num  3.76 3.15 1.85 3.64 4.64 5.25 4.07 4.72 4.17 5.73 ...
 $ Weight   : num  0.17 0.15 0.02 0.16 0.37 0.73 0.22 0.3 0.19 0.78 ...
 $ Age      : int  3 3 3 3 3 3 3 3 3 3 ...
```

```r
summary(poplar)
```

```
      Site           Year        Treatment       Diameter          Height      
 Min.   :1.00   Min.   :1.00   Min.   :1.00   Min.   :-99.00   Min.   :-99.00  
 1st Qu.:1.00   1st Qu.:1.00   1st Qu.:2.00   1st Qu.:  3.60   1st Qu.:  5.50  
 Median :2.00   Median :2.00   Median :2.50   Median :  5.17   Median :  6.91  
 Mean   :1.51   Mean   :1.51   Mean   :2.50   Mean   :  3.86   Mean   :  5.90  
 3rd Qu.:2.00   3rd Qu.:2.00   3rd Qu.:3.75   3rd Qu.:  6.23   3rd Qu.:  8.75  
 Max.   :2.00   Max.   :2.00   Max.   :4.00   Max.   :  8.26   Max.   : 10.90  
     Weight            Age      
 Min.   :-99.00   Min.   :3.00  
 1st Qu.:  0.60   1st Qu.:3.00  
 Median :  1.64   Median :4.00  
 Mean   :  1.10   Mean   :3.51  
 3rd Qu.:  3.44   3rd Qu.:4.00  
 Max.   :  6.93   Max.   :4.00  
```


From typing `str(poplar)` at the `R` prompt, one can see that all seven variables are either integer or numeric.  From the description, the variables `Site` and `Treatment` are factors.  Further investigation into the experiment reveals that `year` and `Age` are factors as well. Recall that factors are an extension of vectors designed for storing categorical information.   The results of `summary(poplar)` indicate the minimum values for `Diameter`, `Height`, and `Weight` are all `-99`, which does not make sense unless one is told that a value of `-99` for these variables represents a missing value.  Once one understands that the variables `Site`, `Year`, `Treatment`, and `Age` are factors and that the value `-99` has been used to represent missing values for the variables `Diameter`, `Height`, and `Weight`, appropriate arguments to `read.csv()` can be entered.  The data is now read into the object `poplarC` using `na.strings = "-99"` to store the `NA` values correctly.  The argument `colClasses=` requires a vector that indicates the desired class of each column.


```r
poplarC <- read.csv(file = url(site), na.strings = "-99", colClasses = c(rep("factor", 
    3), rep("numeric", 3), "factor"))
str(poplarC)
```

```
'data.frame':	298 obs. of  7 variables:
 $ Site     : Factor w/ 2 levels "1","2": 1 1 1 1 1 1 1 1 1 2 ...
 $ Year     : Factor w/ 2 levels "1","2": 1 1 1 1 1 1 1 1 1 1 ...
 $ Treatment: Factor w/ 4 levels "1","2","3","4": 1 1 1 1 1 1 1 1 1 1 ...
 $ Diameter : num  2.23 2.12 1.06 2.12 2.99 4.01 2.41 2.75 2.2 4.09 ...
 $ Height   : num  3.76 3.15 1.85 3.64 4.64 5.25 4.07 4.72 4.17 5.73 ...
 $ Weight   : num  0.17 0.15 0.02 0.16 0.37 0.73 0.22 0.3 0.19 0.78 ...
 $ Age      : Factor w/ 2 levels "3","4": 1 1 1 1 1 1 1 1 1 1 ...
```


In the event different values (999, 99, 9999) for different variables (`var1`, `var2`, `var3`) are used to represent missing values in a data set, the argument \verb|na.strings=| will no longer be able to solve the problem directly.  Although one can pass a vector of the form `na.strings = c(999, 99, 9999)`, this will simply replace all values that are 999,  99, or 9999 with `NA`s.  If the first variable has a legitimate value of 99, then it too would be replaced with an `NA` value.  One solution for this problem in general is to read the data set into a data frame (`DF`), to assign the data frame to a different name so that the cleaned up data set is not confused with the original data, and to use filtering to assign `NA`s to values of `var1`, `var2`, and `var3` that have entries of 999, 99, and 999, respectively.


```r
DF <- read.table(file = url(site), header = TRUE)
df <- DF
df[df$var1 == 999, "var1"] = NA
df[df$var2 == 99, "var2"] = NA
df[df$var3 == 9999, "var3"] = NA
```


Once a variable has its class changed from `int` to `factor`, labeling the levels of the factor can be accomplished without difficulties.  To facilitate analysis of the `poplarC` data, labels for the levels of the variables `Site` and `Treatment` are assigned.


```r
levels(poplarC$Site) <- c("Moist", "Dry")
TreatmentLevels <- c("Control", "Fertilizer", "Irrigation", "FertIrriga")
levels(poplarC$Treatment) <- TreatmentLevels
str(poplarC$Treatment)
```

```
 Factor w/ 4 levels "Control","Fertilizer",..: 1 1 1 1 1 1 1 1 1 1 ...
```


Another way to accomplish the previous labeling that makes clear the assignment of labels to levels is given below.  The reader should make sure to that the variable being labeled is a factor before using either the `labels=` or `levels=` argument to assign labels to levels.


```r
poplarC$Site <- factor(poplarC$Site, labels = c("Moist", "Dry"))
str(poplarC$Site)
```

```
 Factor w/ 2 levels "Moist","Dry": 1 1 1 1 1 1 1 1 1 2 ...
```


If the argument `levels = c("Moist", "Dry")` is applied to a non-factor variable (as `Site` is in the original `poplar` data frame), the levels of `Site` are converted to `NA` values as seen below.


```r
poplar$Site <- factor(poplar$Site, levels = c("Moist", "Dry"))
str(poplar$Site)
```

```
 Factor w/ 2 levels "Moist","Dry": NA NA NA NA NA NA NA NA NA NA ...
```

