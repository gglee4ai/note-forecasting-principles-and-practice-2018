note11
================

``` r
library(fpp2)
library(ggplot2)
theme_set(theme_gray(base_family = "NanumGothic"))
```

# 11 고급 예측 기법

## 11.1 복잡한 계절성

``` r
p1 <- autoplot(calls) +
  ylab("통화량") +
  xlab("주차") +
  scale_x_continuous(breaks = seq(1, 33, by = 2))
```

    ## Scale for 'x' is already present. Adding another scale for 'x', which will
    ## replace the existing scale.

``` r
p2 <- autoplot(window(calls, end = 4)) +
  scale_x_continuous(minor_breaks = seq(1, 4, by = 0.2))
```

    ## Scale for 'x' is already present. Adding another scale for 'x', which will
    ## replace the existing scale.

``` r
gridExtra::grid.arrange(p1, p2)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note11_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
calls |>
  mstl() |>
  autoplot()
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note11_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
calls |>
  stlf() |>
  autoplot() +
  xlab("주차")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note11_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
fit <- auto.arima(calls,
  seasonal = FALSE, lambda = 0,
  xreg = fourier(calls, K = c(10, 10))
)
fit
```

    ## Series: calls 
    ## Regression with ARIMA(3,1,1) errors 
    ## Box Cox transformation: lambda= 0 
    ## 
    ## Coefficients:
    ##          ar1     ar2     ar3      ma1  S1-169   C1-169  S2-169   C2-169   S3-169
    ##       0.1372  0.0477  0.0006  -0.8326  0.2884  -0.5151  0.1153  -0.2302  -0.0405
    ## s.e.  0.0091  0.0078  0.0074   0.0069  0.0046   0.0046  0.0024   0.0024   0.0017
    ##        C3-169   S4-169   C4-169   S5-169   C5-169   S6-169  C6-169   S7-169
    ##       -0.0887  -0.0480  -0.0460  -0.0208  -0.0068  -0.0100  0.0089  -0.0052
    ## s.e.   0.0017   0.0014   0.0014   0.0013   0.0013   0.0012  0.0012   0.0011
    ##       C7-169  S8-169   C8-169   S9-169  C9-169  S10-169  C10-169  S1-845
    ##       0.0017  -5e-04  -0.0033  -0.0047  0.0029  -0.0048   0.0083  0.0176
    ## s.e.  0.0011   1e-03   0.0010   0.0010  0.0010   0.0010   0.0010  0.0224
    ##        C1-845  S2-845   C2-845  S3-845   C3-845  S4-845   C4-845   S6-845
    ##       -0.0092  0.0011  -0.0133  0.0092  -0.0035  0.0064  -0.0017  -0.0034
    ## s.e.   0.0224  0.0112   0.0112  0.0075   0.0075  0.0057   0.0057   0.0038
    ##        C6-845   S7-845   C7-845  S8-845   C8-845  S9-845   C9-845
    ##       -0.0060  -0.0008  -0.0039  0.0080  -0.0014  0.0065  -0.0062
    ## s.e.   0.0038   0.0033   0.0033  0.0029   0.0029  0.0026   0.0026
    ## 
    ## sigma^2 = 0.009104:  log likelihood = 25809.97
    ## AIC=-51537.94   AICc=-51537.82   BIC=-51200.52

``` r
fit |>
  forecast(xreg = fourier(calls, K = c(10, 10), h = 2 * 169)) |>
  autoplot(include = 5 * 169) +
  ylab("통화량") +
  xlab("주차")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note11_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
calls %>%
  subset(start = length(calls) - 2000) %>%
  tbats() -> fit2
fc2 <- forecast(fit2, h = 2 * 169)
autoplot(fc2, include = 5 * 169) +
  ylab("통화량") + xlab("주차")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note11_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
elecdemand[, c("Demand", "Temperature")] |>
  autoplot(facets = TRUE) +
  scale_x_continuous(
    minor_breaks = NULL,
    breaks = 2014 +
      cumsum(c(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30)) / 365,
    labels = month.abb
  ) +
  xlab("시간") + ylab("")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note11_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
elecdemand |>
  as.data.frame() |>
  ggplot(aes(Temperature, Demand)) +
  geom_point(alpha = 0.1) +
  xlab("기온 (섭씨)") +
  ylab("수요 (GW)")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note11_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
cooling <- pmax(elecdemand[, "Temperature"], 18)
fit <- auto.arima(elecdemand[, "Demand"],
  xreg = cbind(fourier(elecdemand, c(10, 10, 0)),
    heating = elecdemand[, "Temperature"],
    cooling = cooling
  )
)
```

``` r
temps <- subset(elecdemand[, "Temperature"],
  start = NROW(elecdemand) - 2 * 48 + 1
)
fc <- forecast(fit,
  xreg = cbind(fourier(temps, c(10, 10, 0)),
    heating = temps, cooling = pmax(temps, 18)
  )
)
```

    ## Warning in forecast.forecast_ARIMA(fit, xreg = cbind(fourier(temps, c(10, : xreg
    ## contains different column names from the xreg used in training. Please check that
    ## the regressors are in the same order.

``` r
autoplot(fc, include = 14 * 48)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note11_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
checkresiduals(fc)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note11_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from Regression with ARIMA(5,1,4) errors
    ## Q* = 738474, df = 3495, p-value < 2.2e-16
    ## 
    ## Model df: 9.   Total lags used: 3504

## 11.2 벡터 자기회귀

``` r
library(vars)
```

    ## Loading required package: MASS

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

    ## The following objects are masked from 'package:fma':
    ## 
    ##     cement, housing, petrol

    ## Loading required package: strucchange

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## Loading required package: sandwich

    ## 
    ## Attaching package: 'strucchange'

    ## The following object is masked from 'package:stringr':
    ## 
    ##     boundary

    ## Loading required package: lmtest

``` r
VARselect(uschange[, 1:2],
  lag.max = 8,
  type = "const"
)[["selection"]]
```

    ## AIC(n)  HQ(n)  SC(n) FPE(n) 
    ##      5      1      1      5

``` r
var1 <- VAR(uschange[, 1:2], p = 1, type = "const")
serial.test(var1, lags.pt = 10, type = "PT.asymptotic")
```

    ## 
    ##  Portmanteau Test (asymptotic)
    ## 
    ## data:  Residuals of VAR object var1
    ## Chi-squared = 49.102, df = 36, p-value = 0.07144

``` r
var2 <- VAR(uschange[, 1:2], p = 2, type = "const")
serial.test(var2, lags.pt = 10, type = "PT.asymptotic")
```

    ## 
    ##  Portmanteau Test (asymptotic)
    ## 
    ## data:  Residuals of VAR object var2
    ## Chi-squared = 47.741, df = 32, p-value = 0.03633

``` r
var3 <- VAR(uschange[, 1:2], p = 3, type = "const")
serial.test(var3, lags.pt = 10, type = "PT.asymptotic")
```

    ## 
    ##  Portmanteau Test (asymptotic)
    ## 
    ## data:  Residuals of VAR object var3
    ## Chi-squared = 33.617, df = 28, p-value = 0.2138

``` r
forecast(var3) |>
  autoplot() +
  xlab("Year")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note11_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

## 11.3 신경망 모델

``` r
fit <- nnetar(sunspotarea, lambda = 0)
autoplot(forecast(fit, h = 30))
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note11_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
sim <- ts(matrix(0, nrow = 30L, ncol = 9L), start = end(sunspotarea)[1L] + 1L)
for (i in seq(9)) {
  sim[, i] <- simulate(fit, nsim = 30L)
}
autoplot(sunspotarea) +
  autolayer(sim)
```

    ## For a multivariate time series, specify a seriesname for each time series. Defaulting to column names.

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note11_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
fcast <- forecast(fit, PI = TRUE, h = 30)
autoplot(fcast)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note11_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

## 11.4 붓스트랩과 배깅

``` r
bootseries <-
  bld.mbb.bootstrap(debitcards, 10) |>
  as.data.frame() |>
  ts(start = 2000, frequency = 12)
str(bootseries)
```

    ##  Time-Series [1:164, 1:10] from 2000 to 2014: 7.2 7.33 7.81 7.41 9.14 ...
    ##  - attr(*, "dimnames")=List of 2
    ##   ..$ : NULL
    ##   ..$ : chr [1:10] "structure.c.7.204..7.335..7.812..7.413..9.136..8.725..8.751.." "structure.c.8.23194529785207..6.63011469360548..8.07028152325618.." "structure.c.7.56514975774105..6.95861426150603..7.71107899986725.." "structure.c.7.32620656611209..7.39637565475836..7.2894338799725.." ...

``` r
autoplot(debitcards) +
  autolayer(bootseries, colour = TRUE) +
  autolayer(debitcards, colour = FALSE) +
  ylab("붓스트랩한 시계열") +
  guides(color = "none")
```

    ## For a multivariate time series, specify a seriesname for each time series. Defaulting to column names.

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note11_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
nsim <- 1000L
sim <- bld.mbb.bootstrap(debitcards, nsim)
```

``` r
h <- 36L
future <- matrix(0, nrow = nsim, ncol = h)
for (i in seq(nsim)) {
  future[i, ] <- simulate(ets(sim[[i]]), nsim = h)
}
```

``` r
start <- tsp(debitcards)[2] + 1 / 12
simfc <- structure(list(
  mean = ts(colMeans(future), start = start, frequency = 12),
  lower = ts(apply(future, 2, quantile, prob = 0.025),
    start = start, frequency = 12
  ),
  upper = ts(apply(future, 2, quantile, prob = 0.975),
    start = start, frequency = 12
  ),
  level = 95
),
class = "forecast"
)
```

``` r
etsfc <- forecast(ets(debitcards), h = h, level = 95)
autoplot(debitcards) +
  ggtitle("아이슬란드의 월별 소매 직불 카드 사용량") +
  xlab("연도") + ylab("백만 ISK") +
  autolayer(simfc, series = "모사한 것") +
  autolayer(etsfc, series = "ETS")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note11_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

``` r
sim <- bld.mbb.bootstrap(debitcards, 10) %>%
  as.data.frame() %>%
  ts(frequency = 12, start = 2000)
fc <- purrr::map(
  as.list(sim),
  function(x) {
    forecast(ets(x))[["mean"]]
  }
) %>%
  as.data.frame() %>%
  ts(frequency = 12, start = start)
autoplot(debitcards) +
  autolayer(sim, colour = TRUE) +
  autolayer(fc, colour = TRUE) +
  autolayer(debitcards, colour = FALSE) +
  ylab("붓스트랩한 시계열") +
  guides(colour = "none")
```

    ## For a multivariate time series, specify a seriesname for each time series. Defaulting to column names.
    ## For a multivariate time series, specify a seriesname for each time series. Defaulting to column names.

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note11_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

``` r
etsfc <- debitcards %>%
  ets() %>%
  forecast(h = 36)
baggedfc <- debitcards %>%
  baggedETS() %>%
  forecast(h = 36)
autoplot(debitcards) +
  autolayer(baggedfc, series = "배깅한ETS", PI = FALSE) +
  autolayer(etsfc, series = "ETS", PI = FALSE) +
  guides(colour = guide_legend(title = "예측값"))
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note11_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

## 11.5 연습 문제
