---
output:
  html_document:
    keep_md: true
    github_document: true 
    theme: readable
    highlight: tango
    toc: yes
---



#  Goodness of Fit Tests 

**Rudy Meza**

---------------
 
**Packages**


```r
library( dplyr )
library( pander )
library( ggplot2 )
library(tidyverse)
```



# Research Question
An American roulette wheel contains 38 slots: 18 red, 18 black, and 2 green.  A casino has purchased a new wheel and they want to know if there is any evidence that the wheel is unfair. They spin the wheel 100 times and it lands on red 44 times, black 49 times, and green 7 times.



```r
r_slots<-c("red", "black", "green")
expected<-c(100*18/38,100*18/38,100*2/38)
observed<-c(44,49,7)
cols<-cbind(observed, expected)
rownames(cols)<-r_slots
prop<-c(1/3,1/3,1/3)
```



All the expected counts are greater than 5.

```r
cols%>%pander()
```


---------------------------------
  &nbsp;     observed   expected 
----------- ---------- ----------
  **red**       44       47.37   

 **black**      49       47.37   

 **green**      7        5.263   
---------------------------------

```r
X<-sum((observed-expected)^2/expected)
k=3
df<-k-1
```


```r
p_value = pchisq(X,2, lower.tail = F)
p_value
```

```
## [1] 0.6476244
```

# Answer:

Since the **P-Value** is greater than .05. We can't conclude with strong evidence that the roulette wheel is unfair.

We can also use the chisq.test function to show the same thing.


```r
r_tibble<-bind_cols(r_slots, observed,expected, prop)
columns<-c("Slots","Observed","Expected","prop")

colnames(r_tibble)<-columns

r_tibble%>%
  select(Observed)%>%
  chisq.test(p = c(18/38,18/38,2/38))
```

```
## 
## 	Chi-squared test for given probabilities
## 
## data:  .
## X-squared = 0.86889, df = 2, p-value = 0.6476
```




# Research Question:
When randomly selecting a card from a deck with replacement, are we equally likely to select a heart, diamond, spade, and club?

The following is an equal proportions test.

```r
suit<-c("Hearts","Diamonds","Spades","Clubs")
count<-c(13,8,8,11)

table<-bind_cols(suit,count)
names<-c("Suit","Count")

colnames(table)<-names
table
```

```
## # A tibble: 4 x 2
##   Suit     Count
##   <chr>    <dbl>
## 1 Hearts      13
## 2 Diamonds     8
## 3 Spades       8
## 4 Clubs       11
```


```r
s=40
new_df<-table%>%
  mutate(t_prop = .25,
         exp = t_prop*s,
         C.to_X2 = (count-exp)^2/exp)%>%
  as_tibble()
new_df
```

```
## # A tibble: 4 x 5
##   Suit     Count t_prop   exp C.to_X2
##   <chr>    <dbl>  <dbl> <dbl>   <dbl>
## 1 Hearts      13   0.25    10     0.9
## 2 Diamonds     8   0.25    10     0.4
## 3 Spades       8   0.25    10     0.4
## 4 Clubs       11   0.25    10     0.1
```

```r
X<-sum(new_df$C.to_X2)
k=length(suit)
df<-k-1

p_value = pchisq(X,df, lower.tail = F)
round(p_value,4)
```

```
## [1] 0.6149
```

# Answer:

Since the **P-Value** is greater than .05. We can't conclude that the proportions are different in the population. Below is an example using the chisq.test function.


```r
new_df%>%
  select(Count)%>%
  chisq.test()
```

```
## 
## 	Chi-squared test for given probabilities
## 
## data:  .
## X-squared = 1.8, df = 3, p-value = 0.6149
```


