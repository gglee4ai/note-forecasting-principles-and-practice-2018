note07
================

``` r
library(fpp2)
library(ggplot2)
theme_set(theme_gray(base_family = "NanumGothic"))
```

# 7 지수평활

## 7.1 단순 지수평활

``` r
oildata <- oil |> window(start = 1996)
autoplot(oildata) +
  ylab("원유 (백만 톤)") + xlab("연도")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note07_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
fc <- ses(oildata, h = 5)
round(accuracy(fc), 2)
```

    ##               ME  RMSE   MAE MPE MAPE MASE  ACF1
    ## Training set 6.4 28.12 22.26 1.1 4.61 0.93 -0.03

``` r
autoplot(fc) +
  autolayer(fitted(fc), series = "Fitted") +
  ylab("원유 (백만 톤)") + xlab("연도") +
  ggtitle("단순 지수평활로 얻은 예측값")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note07_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
cbind(oildata, fitted(fc))
```

    ## Time Series:
    ## Start = 1996 
    ## End = 2013 
    ## Frequency = 1 
    ##       oildata fitted(fc)
    ## 1996 445.3641   446.5868
    ## 1997 453.1950   445.5672
    ## 1998 454.4096   451.9280
    ## 1999 422.3789   453.9974
    ## 2000 456.0371   427.6311
    ## 2001 440.3866   451.3186
    ## 2002 425.1944   442.2025
    ## 2003 486.2052   428.0196
    ## 2004 500.4291   476.5400
    ## 2005 521.2759   496.4609
    ## 2006 508.9476   517.1539
    ## 2007 488.8889   510.3108
    ## 2008 509.8706   492.4472
    ## 2009 456.7229   506.9764
    ## 2010 473.8166   465.0705
    ## 2011 525.9509   472.3638
    ## 2012 549.8338   517.0495
    ## 2013 542.3405   544.3880

## 7.2 추세 기법

``` r
air <- window(ausair, start = 1990)
fc <- holt(air, h = 5)
```

``` r
autoplot(fc, series = "Data") +
  autolayer(fitted(fc), series = "Fitted")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note07_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
fc <- holt(air, h = 15)
fc2 <- holt(air, damped = TRUE, phi = 0.90, h = 15)
autoplot(air) +
  autolayer(fc, series = "홀트 기법", PI = FALSE) +
  autolayer(fc2, series = "감쇠 홀트 기법", PI = FALSE) +
  ggtitle("홀트 기법으로 얻은 예측값") + xlab("연도") +
  ylab("호주 항공객 (백만 명)") +
  guides(colour = guide_legend(title = "예측값"))
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note07_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
livestock |>
  autoplot()
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note07_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
e1 <- tsCV(livestock, ses, h = 1)
e2 <- tsCV(livestock, holt, h = 1)
e3 <- tsCV(livestock, holt, damped = TRUE, h = 1)
```

``` r
mean(e1^2, na.rm = TRUE)
```

    ## [1] 178.2531

``` r
mean(e2^2, na.rm = TRUE)
```

    ## [1] 173.365

``` r
mean(e3^2, na.rm = TRUE)
```

    ## [1] 162.6274

``` r
mean(abs(e1), na.rm = TRUE)
```

    ## [1] 8.53246

``` r
mean(abs(e2), na.rm = TRUE)
```

    ## [1] 8.803058

``` r
mean(abs(e3), na.rm = TRUE)
```

    ## [1] 8.024192

``` r
fc <- holt(livestock, damped = TRUE)
fc$model
```

    ## Damped Holt's method 
    ## 
    ## Call:
    ##  holt(y = livestock, damped = TRUE) 
    ## 
    ##   Smoothing parameters:
    ##     alpha = 0.9999 
    ##     beta  = 3e-04 
    ##     phi   = 0.9798 
    ## 
    ##   Initial states:
    ##     l = 223.35 
    ##     b = 6.9046 
    ## 
    ##   sigma:  12.8435
    ## 
    ##      AIC     AICc      BIC 
    ## 427.6370 429.7370 438.7379

``` r
autoplot(fc) +
  xlab("연도") + ylab("아시아의 양 목축 (단위: 백만)") +
  ggtitle("감쇠 홀트 기법으로 얻은 예측값")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note07_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

## 7.3 홀트-윈터스의 계절성 기법

``` r
aust <- window(austourists, start = 2005)
fit1 <- hw(aust, seasonal = "additive")
fit2 <- hw(aust, seasonal = "multiplicative")
autoplot(aust) +
  autolayer(fit1, series = "HW 덧셈 예측", PI = FALSE) +
  autolayer(fit2, series = "HW 곱셈 예측", PI = FALSE) +
  xlab("연도") +
  ylab("호주 국제선 여행객 숙박일 (단일: 백만)") +
  guides(color = guide_legend(title = "예측"))
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note07_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
data <- subset(hyndsight, end = length(hyndsight) - 35)
fc <- hw(data, damped = TRUE, seasonal = "multiplicative", h = 35)
autoplot(hyndsight) +
  autolayer(fc, series = "HW 곱셈 감쇠", PI = FALSE) +
  guides(color = guide_legend(title = "일별 예측값"))
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note07_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

## 7.4 지수 평활 기법 분류 체계

## 7.5 지수 평활에 대한 혁신 상태 공간 모델

## 7.6 추정과 모델 선택

``` r
aust <- austourists |> window(start = 2005)
fit <- ets(aust)
summary(fit)
```

    ## ETS(M,A,M) 
    ## 
    ## Call:
    ##  ets(y = aust) 
    ## 
    ##   Smoothing parameters:
    ##     alpha = 0.1908 
    ##     beta  = 0.0392 
    ##     gamma = 2e-04 
    ## 
    ##   Initial states:
    ##     l = 32.3679 
    ##     b = 0.9281 
    ##     s = 1.0218 0.9628 0.7683 1.2471
    ## 
    ##   sigma:  0.0383
    ## 
    ##      AIC     AICc      BIC 
    ## 224.8628 230.1569 240.9205 
    ## 
    ## Training set error measures:
    ##                      ME     RMSE     MAE        MPE     MAPE     MASE      ACF1
    ## Training set 0.04836907 1.670893 1.24954 -0.1845609 2.692849 0.409454 0.2005962

``` r
autoplot(fit) +
  ggtitle("ETS(M,A,M) 기법의 성분")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note07_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
cbind(
  "잔차" = residuals(fit),
  "예측오차" = residuals(fit, type = "response")
) |>
  autoplot(facet = TRUE) +
  xlab("Year") +
  ylab("")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note07_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

## 7.7 ETS 모델로 예측하기

``` r
fit |>
  forecast(h = 8) |>
  autoplot() +
  ylab("호주 국제선 여행객 숙박일 수 (단위: 백만)") +
  ggtitle("ETS(M, A, M)으로 얻은 예측값")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note07_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

## 7.8 연습문제
