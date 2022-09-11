note06
================

# 6 시계열 분해

## 6.2 이동평균

``` r
library(fpp2)
library(ggplot2)
theme_set(theme_gray(base_family = "NanumGothic"))
```

``` r
autoplot(elecsales) +
  xlab("Year") +
  ylab("GWh") +
  ggtitle("연간 전력 판매: 남 호주")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note06_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
mean(elecsales[1:5])
```

    ## [1] 2381.53

``` r
cbind(elecsales, ma(elecsales, order = 5))
```

    ## Time Series:
    ## Start = 1989 
    ## End = 2008 
    ## Frequency = 1 
    ##      elecsales ma(elecsales, order = 5)
    ## 1989   2354.34                       NA
    ## 1990   2379.71                       NA
    ## 1991   2318.52                 2381.530
    ## 1992   2468.99                 2424.556
    ## 1993   2386.09                 2463.758
    ## 1994   2569.47                 2552.598
    ## 1995   2575.72                 2627.700
    ## 1996   2762.72                 2750.622
    ## 1997   2844.50                 2858.348
    ## 1998   3000.70                 3014.704
    ## 1999   3108.10                 3077.300
    ## 2000   3357.50                 3144.520
    ## 2001   3075.70                 3188.700
    ## 2002   3180.60                 3202.320
    ## 2003   3221.60                 3216.940
    ## 2004   3176.20                 3307.296
    ## 2005   3430.60                 3398.754
    ## 2006   3527.48                 3485.434
    ## 2007   3637.89                       NA
    ## 2008   3655.00                       NA

``` r
autoplot(elecsales, series = "Data") +
  autolayer(ma(elecsales, 5), series = "5-MA") +
  xlab("Year") + ylab("GWh") +
  ggtitle("연간 전력 판매량: 남 호주") +
  scale_colour_manual(
    values = c("Data" = "grey50", "5-MA" = "red"),
    breaks = c("Data", "5-MA")
  ) +
  guides(color = guide_legend(title = " "))
```

    ## Warning: Removed 4 row(s) containing missing values (geom_path).

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note06_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
autoplot(elecsales, series = "Data") +
  autolayer(ma(elecsales, 3), series = "3-MA") +
  xlab("Year") + ylab("GWh") +
  ggtitle("연간 전력 판매량: 남 호주") +
  scale_colour_manual(
    values = c("Data" = "grey50", "3-MA" = "red"),
    breaks = c("Data", "3-MA")
  ) +
  guides(color = guide_legend(title = " "))
```

    ## Warning: Removed 2 row(s) containing missing values (geom_path).

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note06_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
autoplot(elecsales, series = "Data") +
  autolayer(ma(elecsales, 7), series = "7-MA") +
  xlab("Year") + ylab("GWh") +
  ggtitle("연간 전력 판매량: 남 호주") +
  scale_colour_manual(
    values = c("Data" = "grey50", "7-MA" = "red"),
    breaks = c("Data", "7-MA")
  ) +
  guides(color = guide_legend(title = " "))
```

    ## Warning: Removed 6 row(s) containing missing values (geom_path).

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note06_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
autoplot(elecsales, series = "Data") +
  autolayer(ma(elecsales, 9), series = "9-MA") +
  xlab("Year") + ylab("GWh") +
  ggtitle("연간 전력 판매량: 남 호주") +
  scale_colour_manual(
    values = c("Data" = "grey50", "9-MA" = "red"),
    breaks = c("Data", "9-MA")
  ) +
  guides(color = guide_legend(title = " "))
```

    ## Warning: Removed 8 row(s) containing missing values (geom_path).

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note06_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
beer2 <- window(ausbeer, start = 1992)
ma4 <- ma(beer2, order = 4, centre = FALSE)
ma2x4 <- ma(beer2, order = 4, centre = TRUE)
ma2x4_ <- ma(beer2, order = 4)
cbind(beer2, ma4, ma2x4, ma2x4_)
```

    ##         beer2    ma4   ma2x4  ma2x4_
    ## 1992 Q1   443     NA      NA      NA
    ## 1992 Q2   410 451.25      NA      NA
    ## 1992 Q3   420 448.75 450.000 450.000
    ## 1992 Q4   532 451.50 450.125 450.125
    ## 1993 Q1   433 449.00 450.250 450.250
    ## 1993 Q2   421 444.00 446.500 446.500
    ## 1993 Q3   410 448.00 446.000 446.000
    ## 1993 Q4   512 438.00 443.000 443.000
    ## 1994 Q1   449 441.25 439.625 439.625
    ## 1994 Q2   381 446.00 443.625 443.625
    ## 1994 Q3   423 440.25 443.125 443.125
    ## 1994 Q4   531 447.00 443.625 443.625
    ## 1995 Q1   426 445.25 446.125 446.125
    ## 1995 Q2   408 442.50 443.875 443.875
    ## 1995 Q3   416 438.25 440.375 440.375
    ## 1995 Q4   520 435.75 437.000 437.000
    ## 1996 Q1   409 431.25 433.500 433.500
    ## 1996 Q2   398 428.00 429.625 429.625
    ## 1996 Q3   398 433.75 430.875 430.875
    ## 1996 Q4   507 433.75 433.750 433.750
    ## 1997 Q1   432 435.75 434.750 434.750
    ## 1997 Q2   398 440.50 438.125 438.125
    ## 1997 Q3   406 439.50 440.000 440.000
    ## 1997 Q4   526 439.25 439.375 439.375
    ## 1998 Q1   428 438.50 438.875 438.875
    ## 1998 Q2   397 436.25 437.375 437.375
    ## 1998 Q3   403 438.00 437.125 437.125
    ## 1998 Q4   517 434.50 436.250 436.250
    ## 1999 Q1   435 439.75 437.125 437.125
    ## 1999 Q2   383 440.75 440.250 440.250
    ## 1999 Q3   424 437.25 439.000 439.000
    ## 1999 Q4   521 442.00 439.625 439.625
    ## 2000 Q1   421 439.50 440.750 440.750
    ## 2000 Q2   402 434.25 436.875 436.875
    ## 2000 Q3   414 441.75 438.000 438.000
    ## 2000 Q4   500 436.25 439.000 439.000
    ## 2001 Q1   451 436.75 436.500 436.500
    ## 2001 Q2   380 434.75 435.750 435.750
    ## 2001 Q3   416 429.00 431.875 431.875
    ## 2001 Q4   492 436.00 432.500 432.500
    ## 2002 Q1   428 433.50 434.750 434.750
    ## 2002 Q2   408 437.00 435.250 435.250
    ## 2002 Q3   406 438.75 437.875 437.875
    ## 2002 Q4   506 431.75 435.250 435.250
    ## 2003 Q1   435 435.50 433.625 433.625
    ## 2003 Q2   380 431.50 433.500 433.500
    ## 2003 Q3   421 431.50 431.500 431.500
    ## 2003 Q4   490 434.00 432.750 432.750
    ## 2004 Q1   435 431.75 432.875 432.875
    ## 2004 Q2   390 422.75 427.250 427.250
    ## 2004 Q3   412 418.00 420.375 420.375
    ## 2004 Q4   454 421.25 419.625 419.625
    ## 2005 Q1   416 420.25 420.750 420.750
    ## 2005 Q2   403 427.25 423.750 423.750
    ## 2005 Q3   408 432.75 430.000 430.000
    ## 2005 Q4   482 428.50 430.625 430.625
    ## 2006 Q1   438 427.75 428.125 428.125
    ## 2006 Q2   386 430.00 428.875 428.875
    ## 2006 Q3   405 427.25 428.625 428.625
    ## 2006 Q4   491 426.50 426.875 426.875
    ## 2007 Q1   427 423.75 425.125 425.125
    ## 2007 Q2   383 419.25 421.500 421.500
    ## 2007 Q3   394 417.50 418.375 418.375
    ## 2007 Q4   473 419.25 418.375 418.375
    ## 2008 Q1   420 423.25 421.250 421.250
    ## 2008 Q2   390 427.00 425.125 425.125
    ## 2008 Q3   410 425.75 426.375 426.375
    ## 2008 Q4   488 427.75 426.750 426.750
    ## 2009 Q1   415 430.00 428.875 428.875
    ## 2009 Q2   398 430.00 430.000 430.000
    ## 2009 Q3   419 429.75 429.875 429.875
    ## 2009 Q4   488 423.75 426.750 426.750
    ## 2010 Q1   414     NA      NA      NA
    ## 2010 Q2   374     NA      NA      NA

``` r
mean(c(mean(beer2[1:4]), mean(beer2[2:5])))
```

    ## [1] 450

``` r
elecequip |>
  autoplot(series = "Data") +
  autolayer(ma(elecequip, 12), series = "12-MA") +
  xlab("연도") + ylab("신규 주문 지수") +
  ggtitle("전자 장비 제조 (유럽 지역)") +
  scale_colour_manual(
    values = c("데이터" = "grey", "12-MA" = "red"),
    breaks = c("데이터", "12-MA")
  ) +
  guides(colour = guide_legend(title = " "))
```

    ## Warning: Removed 12 row(s) containing missing values (geom_path).

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note06_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## 6.3 고전적인 분해법

``` r
elecequip |>
  decompose(type = "multiplicative") |>
  autoplot() +
  xlab("Year") +
  ggtitle("전자 장비 지수의 고전적 곱셈 분해")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note06_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
elecequip |>
  decompose() |>
  autoplot() +
  xlab("Year") +
  ggtitle("전자 장비 지수의 고전적 덧셈 분해")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note06_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

## 6.4 X11 분해

``` r
# install.packages("seasonal")
library(seasonal)
```

``` r
elecequip |>
  seas(x11 = "") ->
fit
autoplot(fit) +
  ggtitle("전자 장비 지수의 X11 분해")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note06_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
autoplot(elecequip, series = "Data") +
  autolayer(trendcycle(fit), series = "Trend") +
  autolayer(seasadj(fit), series = "Seasonal adjusted") +
  xlab("연도") +
  ylab("신규 주문 지수") +
  ggtitle("전자 장비 제조 (유럽 지역)") +
  scale_color_manual(
    values = c("gray", "blue", "red"),
    breaks = c("Data", "Seasonal adjusted", "Trend")
  )
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note06_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
fit |>
  seasonal() |>
  ggsubseriesplot() +
  ylab("계절성")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note06_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

## 6.5 SEATS 분해

``` r
elecequip |>
  seas() |>
  autoplot() +
  ggtitle("전자 장비 지수의 SEATS 분해")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note06_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

## 6.6 STL 분해

``` r
elecequip |>
  stl(t.window = 13, s.window = "periodic", robust = TRUE) |>
  autoplot()
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note06_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

## 6.7 추세와 계절성의 강도를 측정하기

## 6.8 분해법으로 예측하기

``` r
fit <- stl(elecequip, t.window = 13, s.window = "periodic", robust = TRUE)
fit |>
  seasadj() |>
  naive() |>
  autoplot() +
  ylab("신규 구매 지수") +
  ggtitle("계절성으로 조정된 데이터의 단순 예측값")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note06_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
fit |>
  forecast(method = "naive") |>
  autoplot() +
  ylab("신규 구매 지수") +
  ggtitle("STL과 확률보행으로 얻은 예측값")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note06_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
fcast <- stlf(elecequip, method = "naive")
fcast |>
  forecast() |>
  autoplot()
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note06_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

## 6.9 연습문제
