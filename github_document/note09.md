note09
================

``` r
library(fpp2)
library(ggplot2)
theme_set(theme_gray(base_family = "NanumGothic"))
```

# 9 동적 회귀 모델

## 9.1 추정

## 9.2 R에서 ARIMA 오차를 고려하는 회귀

``` r
uschange[, 1:2] |>
  autoplot(, facets = TRUE) +
  xlab("연도") +
  ylab("") +
  ggtitle("미국 소비와 개인소득의 분기별 변화")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note09_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
fit <- auto.arima(uschange[, "Consumption"], xreg = uschange[, "Income"])
fit
```

    ## Series: uschange[, "Consumption"] 
    ## Regression with ARIMA(1,0,2) errors 
    ## 
    ## Coefficients:
    ##          ar1      ma1     ma2  intercept    xreg
    ##       0.6922  -0.5758  0.1984     0.5990  0.2028
    ## s.e.  0.1159   0.1301  0.0756     0.0884  0.0461
    ## 
    ## sigma^2 = 0.3219:  log likelihood = -156.95
    ## AIC=325.91   AICc=326.37   BIC=345.29

``` r
cbind(
  "Regression Erros" = residuals(fit, type = "regression"),
  "ARIMA errors" = residuals(fit, type = "innovation")
) |>
  autoplot(facets = TRUE)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note09_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
checkresiduals(fit)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note09_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from Regression with ARIMA(1,0,2) errors
    ## Q* = 5.8916, df = 5, p-value = 0.3169
    ## 
    ## Model df: 3.   Total lags used: 8

## 9.3 예측하기

``` r
fcast <- forecast(fit, xreg = rep(mean(uschange[, 2]), 8))
autoplot(fcast) + xlab("연도")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note09_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
elecdaily[, c("Demand", "Temperature")] |>
  autoplot(facets = TRUE)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note09_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
xreg <-
  cbind(
    MaxTemp = elecdaily[, "Temperature"],
    MaxTempSq = elecdaily[, "Temperature"]^2,
    Workdoay = elecdaily[, "WorkDay"]
  )
fit <- auto.arima(elecdaily[, "Demand"], xreg = xreg)
checkresiduals(fit)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note09_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from Regression with ARIMA(2,1,2)(2,0,0)[7] errors
    ## Q* = 28.229, df = 8, p-value = 0.0004326
    ## 
    ## Model df: 6.   Total lags used: 14

``` r
fcast <- forecast(
  fit,
  xreg = cbind(
    MaxTemp = rep(26, 14),
    MaxTempSq = rep(26^2, 14),
    Workday = c(0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1)
  )
)
```

    ## Warning in forecast.forecast_ARIMA(fit, xreg = cbind(MaxTemp = rep(26, 14), :
    ## xreg contains different column names from the xreg used in training. Please check
    ## that the regressors are in the same order.

``` r
autoplot(fcast) +
  ylab("전력 수요(GW)")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note09_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

## 9.4 확률적 추세와 확정적 추세

``` r
autoplot(austa) +
  xlab("연도") +
  ylab("백만명") +
  ggtitle("호주에 입국하는 전체 연간 국제선 방문객")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note09_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
trend <- seq_along(austa)
fit1 <- auto.arima(austa, d = 0, xreg = trend)
fit1
```

    ## Series: austa 
    ## Regression with ARIMA(2,0,0) errors 
    ## 
    ## Coefficients:
    ##          ar1      ar2  intercept    xreg
    ##       1.1127  -0.3805     0.4156  0.1710
    ## s.e.  0.1600   0.1585     0.1897  0.0088
    ## 
    ## sigma^2 = 0.02979:  log likelihood = 13.6
    ## AIC=-17.2   AICc=-15.2   BIC=-9.28

``` r
fit2 <- auto.arima(austa, d = 1)
fit2
```

    ## Series: austa 
    ## ARIMA(0,1,1) with drift 
    ## 
    ## Coefficients:
    ##          ma1   drift
    ##       0.3006  0.1735
    ## s.e.  0.1647  0.0390
    ## 
    ## sigma^2 = 0.03376:  log likelihood = 10.62
    ## AIC=-15.24   AICc=-14.46   BIC=-10.57

``` r
fc1 <- forecast(fit1, xreg = length(austa) + 1:10)
fc2 <- forecast(fit2, h = 10)
autoplot(austa) +
  autolayer(fc2, series = "확률적 추세") +
  autolayer(fc1, series = "확정적 추세") +
  ggtitle("확정적 추세 모델과 확률적 추세 모델에서 얻은 예측값") +
  xlab("연도") +
  ylab("호주 방문객 (백만)") +
  guides(color = guide_legend(title = "예측값"))
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note09_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

## 9.5 동적 조화 회귀

``` r
cafe04 <- window(auscafe, start = 2004)
plots <- list()
for (i in seq(6)) {
  fit <- auto.arima(cafe04,
    xreg = fourier(cafe04, K = i),
    seasonal = FALSE, lambda = 0
  )
  plots[[i]] <- autoplot(
    forecast(
      fit,
      xreg = fourier(cafe04, K = i, h = 24)
    )
  ) +
    xlab(paste("K=", i, "   AICc=", round(fit[["aicc"]], 2))) +
    ylab("") + ylim(1.5, 4.7)
}

gridExtra::grid.arrange(
  plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]],
  nrow = 3
)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note09_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

## 9.6 시차 예측변수

``` r
insurance |>
  autoplot(facets = TRUE) +
  xlab("Year") +
  ylab("") +
  ggtitle("보험 광고와 견적")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note09_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
Advert <- cbind(
  AdLag0 = insurance[, "TV.advert"],
  AdLag1 = stats::lag(insurance[, "TV.advert"], -1),
  AdLag2 = stats::lag(insurance[, "TV.advert"], -2),
  AdLag3 = stats::lag(insurance[, "TV.advert"], -3)
) |>
  head(NROW(insurance))
Advert
```

    ##             AdLag0    AdLag1    AdLag2   AdLag3
    ## Jan 2002  7.212725        NA        NA       NA
    ## Feb 2002  9.443570  7.212725        NA       NA
    ## Mar 2002  7.534250  9.443570  7.212725       NA
    ## Apr 2002  7.212725  7.534250  9.443570 7.212725
    ## May 2002  9.443570  7.212725  7.534250 9.443570
    ## Jun 2002  6.415215  9.443570  7.212725 7.534250
    ## Jul 2002  5.806990  6.415215  9.443570 7.212725
    ## Aug 2002  6.203600  5.806990  6.415215 9.443570
    ## Sep 2002  7.586430  6.203600  5.806990 6.415215
    ## Oct 2002  8.004935  7.586430  6.203600 5.806990
    ## Nov 2002  8.834980  8.004935  7.586430 6.203600
    ## Dec 2002  8.957255  8.834980  8.004935 7.586430
    ## Jan 2003  9.532990  8.957255  8.834980 8.004935
    ## Feb 2003  9.392950  9.532990  8.957255 8.834980
    ## Mar 2003  8.918560  9.392950  9.532990 8.957255
    ## Apr 2003  8.374120  8.918560  9.392950 9.532990
    ## May 2003  9.844505  8.374120  8.918560 9.392950
    ## Jun 2003  9.849390  9.844505  8.374120 8.918560
    ## Jul 2003  8.402730  9.849390  9.844505 8.374120
    ## Aug 2003  7.920675  8.402730  9.849390 9.844505
    ## Sep 2003  7.436085  7.920675  8.402730 9.849390
    ## Oct 2003  6.340490  7.436085  7.920675 8.402730
    ## Nov 2003  6.939995  6.340490  7.436085 7.920675
    ## Dec 2003  6.977100  6.939995  6.340490 7.436085
    ## Jan 2004  8.010201  6.977100  6.939995 6.340490
    ## Feb 2004  9.565460  8.010201  6.977100 6.939995
    ## Mar 2004  6.272510  9.565460  8.010201 6.977100
    ## Apr 2004  5.707495  6.272510  9.565460 8.010201
    ## May 2004  7.963540  5.707495  6.272510 9.565460
    ## Jun 2004  8.494221  7.963540  5.707495 6.272510
    ## Jul 2004  9.789085  8.494221  7.963540 5.707495
    ## Aug 2004  8.692825  9.789085  8.494221 7.963540
    ## Sep 2004  8.057230  8.692825  9.789085 8.494221
    ## Oct 2004  7.588995  8.057230  8.692825 9.789085
    ## Nov 2004  8.244881  7.588995  8.057230 8.692825
    ## Dec 2004  6.675540  8.244881  7.588995 8.057230
    ## Jan 2005  9.219604  6.675540  8.244881 7.588995
    ## Feb 2005 10.963800  9.219604  6.675540 8.244881
    ## Mar 2005 10.456290 10.963800  9.219604 6.675540
    ## Apr 2005  8.728600 10.456290 10.963800 9.219604

``` r
fit1 <- auto.arima(insurance[4:40, 1], xreg = Advert[4:40, 1], stationary = TRUE)
fit2 <- auto.arima(insurance[4:40, 1], xreg = Advert[4:40, 1:2], stationary = TRUE)
fit3 <- auto.arima(insurance[4:40, 1], xreg = Advert[4:40, 1:3], stationary = TRUE)
fit4 <- auto.arima(insurance[4:40, 1], xreg = Advert[4:40, 1:4], stationary = TRUE)
```

``` r
c(fit1[["aicc"]], fit2[["aicc"]], fit3[["aicc"]], fit4[["aicc"]])
```

    ## [1] 68.49968 60.02357 62.83253 65.45747

``` r
fit <- auto.arima(insurance[, 1], xreg = Advert[, 1:2], stationary = TRUE)
fit
```

    ## Series: insurance[, 1] 
    ## Regression with ARIMA(3,0,0) errors 
    ## 
    ## Coefficients:
    ##          ar1      ar2     ar3  intercept  AdLag0  AdLag1
    ##       1.4117  -0.9317  0.3591     2.0393  1.2564  0.1625
    ## s.e.  0.1698   0.2545  0.1592     0.9931  0.0667  0.0591
    ## 
    ## sigma^2 = 0.2165:  log likelihood = -23.89
    ## AIC=61.78   AICc=65.4   BIC=73.43

``` r
xreg <- cbind(
  AdLag0 = rep(8, 20), AdLag1 = c(Advert[40, 1], rep(8, 19))
)
xreg
```

    ##        AdLag0 AdLag1
    ## AdLag0      8 8.7286
    ##             8 8.0000
    ##             8 8.0000
    ##             8 8.0000
    ##             8 8.0000
    ##             8 8.0000
    ##             8 8.0000
    ##             8 8.0000
    ##             8 8.0000
    ##             8 8.0000
    ##             8 8.0000
    ##             8 8.0000
    ##             8 8.0000
    ##             8 8.0000
    ##             8 8.0000
    ##             8 8.0000
    ##             8 8.0000
    ##             8 8.0000
    ##             8 8.0000
    ##             8 8.0000

``` r
fc8 <- forecast(fit, h = 20, xreg = xreg)
fc8 |>
  autoplot() +
  ylab("견적") +
  ggtitle("미래 광고를 8로 두고 견적 예측")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note09_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

## 9.7 연습문제
