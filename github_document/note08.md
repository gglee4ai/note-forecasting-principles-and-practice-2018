note08
================

``` r
library(fpp2)
library(ggplot2)
theme_set(theme_gray(base_family = "NanumGothic"))
```

# 8 ARIMA 모델

## 8.1 정상성과 차분

``` r
goog200 |>
  autoplot()
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
diff(goog200) |>
  autoplot()
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
Box.test(diff(goog200), lag = 10, type = "Ljung-Box")
```

    ## 
    ##  Box-Ljung test
    ## 
    ## data:  diff(goog200)
    ## X-squared = 11.031, df = 10, p-value = 0.3551

``` r
cbind(
  "판매량 (백만 달러)" = a10,
  "월별 로그 판매량" = log(a10),
  "로그 눈금에서 연간 변동" = diff(log(a10), 12)
) |>
  autoplot(facets = TRUE) +
  xlab("연도") +
  ylab("") +
  ggtitle("당뇨병 약 판매량")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
cbind(
  "10억 kWh" = usmelec,
  "로그" = log(usmelec),
  "계절성 차분 로그값" = diff(log(usmelec), 12),
  "두 번 차분을 구한 로그값" = diff(diff(log(usmelec), 12), 1)
) |>
  autoplot(facets = TRUE)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
library(urca)
```

``` r
goog |>
  ur.kpss() |>
  summary()
```

    ## 
    ## ####################### 
    ## # KPSS Unit Root Test # 
    ## ####################### 
    ## 
    ## Test is of type: mu with 7 lags. 
    ## 
    ## Value of test-statistic is: 10.7223 
    ## 
    ## Critical value for a significance level of: 
    ##                 10pct  5pct 2.5pct  1pct
    ## critical values 0.347 0.463  0.574 0.739

``` r
goog |>
  diff() |>
  ur.kpss() |>
  summary()
```

    ## 
    ## ####################### 
    ## # KPSS Unit Root Test # 
    ## ####################### 
    ## 
    ## Test is of type: mu with 7 lags. 
    ## 
    ## Value of test-statistic is: 0.0324 
    ## 
    ## Critical value for a significance level of: 
    ##                 10pct  5pct 2.5pct  1pct
    ## critical values 0.347 0.463  0.574 0.739

``` r
ndiffs(goog)
```

    ## [1] 1

``` r
usmelec |>
  log() |>
  nsdiffs()
```

    ## [1] 1

``` r
usmelec |>
  log() |>
  diff(lag = 12) |>
  ndiffs()
```

    ## [1] 1

## 8.2 후방이동 기호

## 8.3 자기회귀 모델

## 8.4 이동 평균 모델

## 8.5 비-계절성 ARIMA 모델

``` r
autoplot(uschange[, "Consumption"]) +
  xlab("연도") +
  ylab("분기별 백분율 변화")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
fit <- auto.arima(uschange[, "Consumption"], seasonal = FALSE)
fit
```

    ## Series: uschange[, "Consumption"] 
    ## ARIMA(1,0,3) with non-zero mean 
    ## 
    ## Coefficients:
    ##          ar1      ma1     ma2     ma3    mean
    ##       0.5885  -0.3528  0.0846  0.1739  0.7454
    ## s.e.  0.1541   0.1658  0.0818  0.0843  0.0930
    ## 
    ## sigma^2 = 0.3499:  log likelihood = -164.81
    ## AIC=341.61   AICc=342.08   BIC=361

``` r
coef(fit)
```

    ##         ar1         ma1         ma2         ma3   intercept 
    ##  0.58853911 -0.35278959  0.08456844  0.17389966  0.74540707

``` r
fit |>
  forecast(h = 10) |>
  autoplot(include = 80) +
  ylab("소비")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
ggAcf(uschange[, "Consumption"])
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
ggPacf(uschange[, "Consumption"])
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
fit2 <- Arima(uschange[, "Consumption"], order = c(3, 0, 0))
fit2
```

    ## Series: uschange[, "Consumption"] 
    ## ARIMA(3,0,0) with non-zero mean 
    ## 
    ## Coefficients:
    ##          ar1     ar2     ar3    mean
    ##       0.2274  0.1604  0.2027  0.7449
    ## s.e.  0.0713  0.0723  0.0712  0.1029
    ## 
    ## sigma^2 = 0.3494:  log likelihood = -165.17
    ## AIC=340.34   AICc=340.67   BIC=356.5

``` r
fit3 <- auto.arima(uschange[, "Consumption"],
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE
)
fit3
```

    ## Series: uschange[, "Consumption"] 
    ## ARIMA(3,0,0) with non-zero mean 
    ## 
    ## Coefficients:
    ##          ar1     ar2     ar3    mean
    ##       0.2274  0.1604  0.2027  0.7449
    ## s.e.  0.0713  0.0723  0.0712  0.1029
    ## 
    ## sigma^2 = 0.3494:  log likelihood = -165.17
    ## AIC=340.34   AICc=340.67   BIC=356.5

## 8.6 추정과 차수 선택

## 8.7 R에서 ARIMA 모델링

``` r
elecequip |>
  stl(s.window = "periodic") |>
  seasadj() ->
eeadj
autoplot(eeadj)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
eeadj |>
  diff() |>
  ggtsdisplay()
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
fit <- Arima(eeadj, order = c(3, 1, 1))
fit
```

    ## Series: eeadj 
    ## ARIMA(3,1,1) 
    ## 
    ## Coefficients:
    ##          ar1     ar2     ar3      ma1
    ##       0.0044  0.0916  0.3698  -0.3921
    ## s.e.  0.2201  0.0984  0.0669   0.2426
    ## 
    ## sigma^2 = 9.577:  log likelihood = -492.69
    ## AIC=995.38   AICc=995.7   BIC=1011.72

``` r
checkresiduals(fit)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from ARIMA(3,1,1)
    ## Q* = 24.034, df = 20, p-value = 0.2409
    ## 
    ## Model df: 4.   Total lags used: 24

``` r
autoplot(forecast(fit))
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
auto.arima(eeadj)
```

    ## Series: eeadj 
    ## ARIMA(3,1,0) 
    ## 
    ## Coefficients:
    ##           ar1      ar2     ar3
    ##       -0.3418  -0.0426  0.3185
    ## s.e.   0.0681   0.0725  0.0682
    ## 
    ## sigma^2 = 9.639:  log likelihood = -493.8
    ## AIC=995.6   AICc=995.81   BIC=1008.67

``` r
auto.arima(eeadj, approximation = FALSE)
```

    ## Series: eeadj 
    ## ARIMA(3,1,1) 
    ## 
    ## Coefficients:
    ##          ar1     ar2     ar3      ma1
    ##       0.0044  0.0916  0.3698  -0.3921
    ## s.e.  0.2201  0.0984  0.0669   0.2426
    ## 
    ## sigma^2 = 9.577:  log likelihood = -492.69
    ## AIC=995.38   AICc=995.7   BIC=1011.72

``` r
autoplot(fit)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

## 8.8 예측하기

## 8.9 계절성 ARIMA 모델들

``` r
euretail |>
  autoplot() +
  xlab("연도") +
  ylab("소매 지수")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

``` r
euretail |>
  diff(lag = 4) |>
  ggtsdisplay()
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

``` r
euretail |>
  diff(lag = 4) |>
  diff() |>
  ggtsdisplay()
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

``` r
euretail |>
  Arima(order = c(0, 1, 1), seasonal = c(0, 1, 1)) |>
  residuals() |>
  ggtsdisplay()
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

``` r
fit3 <- Arima(euretail, order = c(0, 1, 3), seasonal = c(0, 1, 1))
checkresiduals(fit3)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from ARIMA(0,1,3)(0,1,1)[4]
    ## Q* = 0.51128, df = 4, p-value = 0.9724
    ## 
    ## Model df: 4.   Total lags used: 8

``` r
fit3 |>
  forecast(h = 12) |>
  autoplot()
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

``` r
auto.arima(euretail, approximation = FALSE)
```

    ## Series: euretail 
    ## ARIMA(0,1,3)(0,1,1)[4] 
    ## 
    ## Coefficients:
    ##          ma1     ma2     ma3     sma1
    ##       0.2630  0.3694  0.4200  -0.6636
    ## s.e.  0.1237  0.1255  0.1294   0.1545
    ## 
    ## sigma^2 = 0.156:  log likelihood = -28.63
    ## AIC=67.26   AICc=68.39   BIC=77.65

``` r
autoplot(h02)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

``` r
lh02 <- log(h02)
cbind(
  "H02 판매량 (백만 처방전)" = h02,
  "로그 H02 판매량" = lh02
) |>
  autoplot(facets = TRUE) +
  xlab("연도") +
  ylab("")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

``` r
lh02 |>
  diff(lag = 12) |>
  ggtsdisplay(xlab = "연도", main = "계절성 차분을 구한 H02 처방전 데이터")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

``` r
fit <- Arima(h02, order = c(3, 0, 1), seasonal = c(0, 1, 2), lambda = 0)
fit
```

    ## Series: h02 
    ## ARIMA(3,0,1)(0,1,2)[12] 
    ## Box Cox transformation: lambda= 0 
    ## 
    ## Coefficients:
    ##           ar1     ar2     ar3     ma1     sma1     sma2
    ##       -0.1603  0.5481  0.5678  0.3827  -0.5222  -0.1768
    ## s.e.   0.1636  0.0878  0.0942  0.1895   0.0861   0.0872
    ## 
    ## sigma^2 = 0.004278:  log likelihood = 250.04
    ## AIC=-486.08   AICc=-485.48   BIC=-463.28

``` r
checkresiduals(fit, lag = 36)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from ARIMA(3,0,1)(0,1,2)[12]
    ## Q* = 50.712, df = 30, p-value = 0.01045
    ## 
    ## Model df: 6.   Total lags used: 36

``` r
auto.arima(lh02)
```

    ## Series: lh02 
    ## ARIMA(2,1,1)(0,1,2)[12] 
    ## 
    ## Coefficients:
    ##           ar1      ar2     ma1     sma1     sma2
    ##       -1.1358  -0.5753  0.3683  -0.5318  -0.1817
    ## s.e.   0.1608   0.0965  0.1884   0.0838   0.0881
    ## 
    ## sigma^2 = 0.004278:  log likelihood = 248.25
    ## AIC=-484.51   AICc=-484.05   BIC=-465

``` r
auto.arima(lh02, approximation = FALSE)
```

    ## Series: lh02 
    ## ARIMA(2,1,3)(2,1,2)[12] 
    ## 
    ## Coefficients:
    ##           ar1      ar2     ma1     ma2      ma3    sar1     sar2     sma1
    ##       -1.1321  -0.9757  0.3868  0.2764  -0.6116  0.7035  -0.3116  -1.2957
    ## s.e.   0.0271   0.0237  0.0659  0.0695   0.0812  0.2682   0.1332   0.2709
    ##         sma2
    ##       0.5631
    ## s.e.  0.2149
    ## 
    ## sigma^2 = 0.004022:  log likelihood = 253.87
    ## AIC=-487.75   AICc=-486.52   BIC=-455.22

``` r
h02 |>
  Arima(order = c(3, 0, 1), seasonal = c(0, 1, 2), lambda = 0) |>
  forecast() |>
  autoplot() +
  ylab("H02 판매량 (백만 처방전)") + xlab("연도") +
  ggtitle("ARIMA(3,0,1)(0,1,2)로 얻은 예측값")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

## 8.10 ARIMA vs ETS

``` r
fets <- function(x, h) {
  forecast(ets(x), h = h)
}

farima <- function(x, h) {
  forecast(auto.arima(x), h = h)
}
```

``` r
air <- window(ausair, start=1990)
e1 <- tsCV(air, fets, h = 1)
e2 <- tsCV(air, farima, h = 1)
```

``` r
mean(e1^2, na.rm = TRUE)
```

    ## [1] 7.864374

``` r
mean(e2^2, na.rm = TRUE)
```

    ## [1] 9.622164

``` r
air |>
  ets() |>
  forecast() |>
  autoplot()
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-46-1.png)<!-- -->

``` r
air |>
  auto.arima() |>
  forecast() |>
  autoplot()
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-46-2.png)<!-- -->

``` r
cement <- window(qcement, start = 1988)
cement |>
  autoplot()
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-47-1.png)<!-- -->

``` r
train <- window(cement, end = c(2007, 4))
fit.arima <- auto.arima(train)
fit.arima
```

    ## Series: train 
    ## ARIMA(1,0,1)(2,1,1)[4] with drift 
    ## 
    ## Coefficients:
    ##          ar1      ma1   sar1     sar2     sma1   drift
    ##       0.8886  -0.2366  0.081  -0.2345  -0.8979  0.0105
    ## s.e.  0.0842   0.1334  0.157   0.1392   0.1780  0.0029
    ## 
    ## sigma^2 = 0.01146:  log likelihood = 61.47
    ## AIC=-108.95   AICc=-107.3   BIC=-92.63

``` r
checkresiduals(fit.arima)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-49-1.png)<!-- -->

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from ARIMA(1,0,1)(2,1,1)[4] with drift
    ## Q* = 0.78311, df = 3, p-value = 0.8535
    ## 
    ## Model df: 5.   Total lags used: 8

``` r
fit.ets <- ets(train)
fit.ets
```

    ## ETS(M,N,M) 
    ## 
    ## Call:
    ##  ets(y = train) 
    ## 
    ##   Smoothing parameters:
    ##     alpha = 0.7341 
    ##     gamma = 1e-04 
    ## 
    ##   Initial states:
    ##     l = 1.6439 
    ##     s = 1.031 1.0439 1.0103 0.9148
    ## 
    ##   sigma:  0.0581
    ## 
    ##        AIC       AICc        BIC 
    ## -2.1967020 -0.6411464 14.4774845

``` r
checkresiduals(fit.ets)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-51-1.png)<!-- -->

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from ETS(M,N,M)
    ## Q* = 6.3457, df = 3, p-value = 0.09595
    ## 
    ## Model df: 6.   Total lags used: 9

``` r
a1 <- fit.arima |>
  forecast(h = 4 * (2013 - 2007) + 1) |>
  accuracy(qcement)
a1[, c("RMSE", "MAE", "MAPE", "MASE")]
```

    ##                   RMSE        MAE     MAPE      MASE
    ## Training set 0.1001195 0.07988903 4.372443 0.5458078
    ## Test set     0.1996098 0.16882205 7.719241 1.1534049

``` r
a2 <- fit.ets |>
  forecast(h = 4 * (2013 - 2007) + 1) |>
  accuracy(qcement)
a2[, c("RMSE", "MAE", "MAPE", "MASE")]
```

    ##                   RMSE        MAE     MAPE      MASE
    ## Training set 0.1022079 0.07958478 4.371823 0.5437292
    ## Test set     0.1838791 0.15395141 6.986077 1.0518075

``` r
cement |>
  ets() |>
  forecast(h = 12) |>
  autoplot()
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note08_files/figure-gfm/unnamed-chunk-54-1.png)<!-- -->

## 8.11 연습문제
