note03
================

# 3 예측가의 도구 상자

## 3.1 몇 가지 단순한 예측 기법

``` r
library(fpp2)
library(ggplot2)
```

``` r
beer2 <- ausbeer |> window(start = 1992, end = c(2007, 4))
autoplot(beer2) +
  autolayer(meanf(beer2, h = 11), series = "평균", PI = FALSE) +
  autolayer(naive(beer2, h = 11), series = "단순", PI = FALSE) +
  autolayer(snaive(beer2, h = 11), series = "계절성 단순", PI = FALSE) +
  ggtitle("분기별 맥주 생산량 예측값") +
  xlab("연도") +
  ylab("단위: 백만 리터") +
  guides(color = guide_legend(title = "예측"))
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note03_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
goog200 |>
  autoplot() +
  autolayer(meanf(goog200, h = 40), series = "평균", PI = FALSE) +
  autolayer(rwf(goog200, h = 40), series = "나이브", PI = FALSE) +
  autolayer(rwf(goog200, drift = TRUE, h = 40), series = "표류", PI = FALSE) +
  ggtitle("구글 주식 (2013년 12월 6일까지)") +
  xlab("날짜") + ylab("종가(미국 달러)") +
  guides(colour = guide_legend(title = "예측"))
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note03_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## 3.2 변환과 조정

``` r
dframe <- cbind(Monthly = milk, DailyAverage = milk / monthdays(milk))
dframe |>
  `colnames<-`(c("월별", "일별 평균")) |>
  autoplot(facet = TRUE) +
  xlab("연도") +
  ylab("파운드") +
  ggtitle("젖소별 우유 생산량")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note03_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
monthdays(milk)
```

    ##      Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
    ## 1962  31  28  31  30  31  30  31  31  30  31  30  31
    ## 1963  31  28  31  30  31  30  31  31  30  31  30  31
    ## 1964  31  29  31  30  31  30  31  31  30  31  30  31
    ## 1965  31  28  31  30  31  30  31  31  30  31  30  31
    ## 1966  31  28  31  30  31  30  31  31  30  31  30  31
    ## 1967  31  28  31  30  31  30  31  31  30  31  30  31
    ## 1968  31  29  31  30  31  30  31  31  30  31  30  31
    ## 1969  31  28  31  30  31  30  31  31  30  31  30  31
    ## 1970  31  28  31  30  31  30  31  31  30  31  30  31
    ## 1971  31  28  31  30  31  30  31  31  30  31  30  31
    ## 1972  31  29  31  30  31  30  31  31  30  31  30  31
    ## 1973  31  28  31  30  31  30  31  31  30  31  30  31
    ## 1974  31  28  31  30  31  30  31  31  30  31  30  31
    ## 1975  31  28  31  30  31  30  31  31  30  31  30  31

``` r
lambda <- BoxCox.lambda(elec)
lambda
```

    ## [1] 0.2654076

``` r
autoplot(BoxCox(elec, lambda))
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note03_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
fc <- eggs |> rwf(drift = TRUE, lambda = 0, h = 50, level = 80)
fc2 <- eggs |> rwf(drift = TRUE, lambda = 0, h = 50, level = 80, biasadj = TRUE)

autoplot(eggs) +
  autolayer(fc, series = "단순 역변환") +
  autolayer(fc2, series = "편향 조정", PI = FALSE)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note03_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

## 3.3 잔차 진단

``` r
goog200 |>
  autoplot() +
  xlab("날짜") + ylab("종가(미국 달러)") +
  ggtitle("구글 주식 일별 가격(2013년 12월 6일까지)")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note03_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
res <- goog200 |>
  naive() |>
  residuals()
autoplot(res) +
  xlab("날짜") + ylab("") +
  ggtitle("나이브 기법에서 얻은 잔차")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note03_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
gghistogram(res) + ggtitle("잔차의 히스토그램")
```

    ## Warning: Removed 1 rows containing non-finite values (stat_bin).

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note03_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
ggAcf(res) + ggtitle("잔차의 ACF")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note03_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
# lag=h, fitdf=K
Box.test(res, lag = 10, fitdf = 0)
```

    ## 
    ##  Box-Pierce test
    ## 
    ## data:  res
    ## X-squared = 10.611, df = 10, p-value = 0.3886

``` r
Box.test(res, lag = 10, fitdf = 0, type = "Lj")
```

    ## 
    ##  Box-Ljung test
    ## 
    ## data:  res
    ## X-squared = 11.031, df = 10, p-value = 0.3551

``` r
checkresiduals(naive(goog200))
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note03_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from Naive method
    ## Q* = 11.031, df = 10, p-value = 0.3551
    ## 
    ## Model df: 0.   Total lags used: 10

## 3.4 예측 정확도 평가

``` r
beer2 <- ausbeer |> window(start = 1992, end = c(2007, 4))
beerfit1 <- meanf(beer2, h = 10)
beerfit2 <- rwf(beer2, h = 10)
beerfit3 <- snaive(beer2, h = 10)
beerfit4 <- rwf(beer2, h = 10, drift = TRUE)
autoplot(window(ausbeer, start = 1992)) +
  autolayer(beerfit1, series = "meanf", PI = FALSE) +
  autolayer(beerfit2, series = "rwf", PI = FALSE) +
  autolayer(beerfit3, series = "snaive", PI = FALSE) +
  autolayer(beerfit4, series = "drift", PI = FALSE)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note03_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
beer3 <- ausbeer |> window(start = 2008)
accuracy(beerfit1, beer3)
```

    ##                   ME     RMSE      MAE        MPE     MAPE     MASE        ACF1
    ## Training set   0.000 43.62858 35.23438 -0.9365102 7.886776 2.463942 -0.10915105
    ## Test set     -13.775 38.44724 34.82500 -3.9698659 8.283390 2.435315 -0.06905715
    ##              Theil's U
    ## Training set        NA
    ## Test set      0.801254

``` r
accuracy(beerfit2, beer3)
```

    ##                       ME     RMSE      MAE         MPE     MAPE     MASE
    ## Training set   0.4761905 65.31511 54.73016  -0.9162496 12.16415 3.827284
    ## Test set     -51.4000000 62.69290 57.40000 -12.9549160 14.18442 4.013986
    ##                     ACF1 Theil's U
    ## Training set -0.24098292        NA
    ## Test set     -0.06905715  1.254009

``` r
accuracy(beerfit3, beer3)
```

    ##                     ME     RMSE  MAE        MPE     MAPE      MASE       ACF1
    ## Training set -2.133333 16.78193 14.3 -0.5537713 3.313685 1.0000000 -0.2876333
    ## Test set      5.200000 14.31084 13.4  1.1475536 3.168503 0.9370629  0.1318407
    ##              Theil's U
    ## Training set        NA
    ## Test set      0.298728

``` r
accuracy(beerfit4, beer3)
```

    ##                         ME     RMSE      MAE        MPE     MAPE     MASE
    ## Training set -7.218212e-15 65.31337 54.76795  -1.026695 12.17879 3.829927
    ## Test set     -5.401905e+01 64.90129 58.87619 -13.582171 14.57749 4.117216
    ##                     ACF1 Theil's U
    ## Training set -0.24098292        NA
    ## Test set     -0.07410793  1.299447

``` r
googfc1 <- goog200 |> meanf(h = 40)
googfc2 <- goog200 |> rwf(h = 40)
googfc3 <- goog200 |> rwf(h = 40, drift = TRUE)
autoplot(subset(goog, end = 240)) +
  autolayer(googfc1, PI = FALSE, series = "meanf") +
  autolayer(googfc2, PI = FALSE, series = "rwf") +
  autolayer(googfc3, PI = FALSE, series = "drift")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note03_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
googtest <- window(goog, start = 201, end = 240)
accuracy(googfc1, googtest)
```

    ##                        ME      RMSE       MAE        MPE     MAPE      MASE
    ## Training set 1.534772e-14  36.91961  26.86941 -0.6596884  5.95376  7.182995
    ## Test set     1.132697e+02 114.21375 113.26971 20.3222979 20.32230 30.280376
    ##                   ACF1 Theil's U
    ## Training set 0.9668981        NA
    ## Test set     0.8104340  13.92142

``` r
accuracy(googfc2, googtest)
```

    ##                      ME      RMSE       MAE       MPE      MAPE     MASE
    ## Training set  0.6967249  6.208148  3.740697 0.1426616 0.8437137 1.000000
    ## Test set     24.3677328 28.434837 24.593517 4.3171356 4.3599811 6.574582
    ##                     ACF1 Theil's U
    ## Training set -0.06038617        NA
    ## Test set      0.81043397  3.451903

``` r
accuracy(googfc3, googtest)
```

    ##                         ME      RMSE       MAE         MPE      MAPE     MASE
    ## Training set -5.762002e-15  6.168928  3.824406 -0.01570676 0.8630093 1.022378
    ## Test set      1.008487e+01 14.077291 11.667241  1.77566103 2.0700918 3.119002
    ##                     ACF1 Theil's U
    ## Training set -0.06038617        NA
    ## Test set      0.64732736  1.709275

``` r
goog200
```

    ## Time Series:
    ## Start = 1 
    ## End = 200 
    ## Frequency = 1 
    ##   [1] 392.8300 392.5121 397.3059 398.0113 400.4902 408.0957 416.5905 413.0038
    ##   [9] 413.6099 413.0734 414.7127 411.1310 409.9884 408.1156 404.5190 401.2850
    ##  [17] 403.0386 404.7227 403.0088 402.5369 402.2040 403.5851 398.7366 394.5290
    ##  [25] 398.0063 403.8931 400.4952 394.9661 388.9950 384.9214 386.3124 392.5369
    ##  [33] 392.6412 392.4724 388.4386 394.1216 388.7516 380.4803 397.3506 397.4698
    ##  [41] 401.3397 404.0967 401.9358 398.1206 406.8836 409.6208 407.5642 412.1245
    ##  [49] 420.1275 427.9913 425.8453 433.9923 432.9243 437.2710 435.9297 440.6838
    ##  [57] 454.9857 449.0146 451.6524 451.3295 450.5546 441.8363 438.5427 433.8383
    ##  [65] 437.7876 431.3495 432.5666 432.7951 431.0117 426.7742 427.0723 429.5263
    ##  [73] 437.0226 442.2337 437.0623 433.1726 435.6664 434.6927 440.2615 447.4001
    ##  [81] 447.4299 439.5114 437.6187 432.0847 430.3013 434.0022 435.7012 437.3405
    ##  [89] 441.0713 438.3043 440.3510 443.8581 449.6206 449.6952 450.0677 457.1467
    ##  [97] 458.5178 459.3573 456.8337 456.3072 452.3976 445.4031 452.4075 448.9798
    ## [105] 448.5327 440.9818 439.8144 438.2844 442.5814 441.0067 449.1885 450.3559
    ## [113] 449.5759 445.3882 442.4473 443.4458 442.3281 439.8939 437.7777 432.0946
    ## [121] 427.0524 425.6863 430.0281 429.9138 431.8562 434.0320 432.2933 430.3957
    ## [129] 422.3282 421.5333 424.9511 420.7137 427.4101 432.9987 436.9381 436.9481
    ## [137] 441.1557 441.4637 445.1994 443.6445 441.6624 441.0117 440.1920 448.7414
    ## [145] 446.2923 448.6371 440.3857 440.5546 435.7807 436.2476 435.3634 435.1249
    ## [153] 440.6341 441.1259 435.2144 433.3564 430.0728 424.0768 425.1647 431.3147
    ## [161] 433.1776 435.2243 438.1552 446.1135 441.5233 502.4371 498.4083 500.2464
    ## [169] 512.3725 509.4615 504.3199 504.2205 514.7719 511.8807 511.9602 510.2016
    ## [177] 509.7396 507.4595 508.0705 500.7183 504.7322 502.0298 502.6209 512.8991
    ## [185] 514.2701 513.4406 512.4421 509.2876 507.8519 513.6939 512.6110 519.5856
    ## [193] 525.7853 528.1201 526.3715 523.8329 523.2269 525.6710 525.2537 531.4783

``` r
e <- tsCV(goog200, rwf, drift = TRUE, h = 1)
sqrt(mean(e^2, na.rm = TRUE))
```

    ## [1] 6.233245

``` r
sqrt(mean(residuals(rwf(goog200, drift = TRUE))^2, na.rm = TRUE))
```

    ## [1] 6.168928

``` r
e <-
  goog200 |>
  tsCV(forecastfunction = rwf, drift = TRUE, h = 1)
e^2 |>
  mean(na.rm = TRUE) |>
  sqrt()
```

    ## [1] 6.233245

``` r
goog200 |>
  rwf(drift = TRUE) |>
  residuals() -> res
res^2 |>
  mean(na.rm = TRUE) |>
  sqrt()
```

    ## [1] 6.168928

``` r
e <- goog200 |> tsCV(forecastfunction = naive, h = 8)
mse <- colMeans(e^2, na.rm = TRUE)
data.frame(h = 1:8, MSE = mse) |>
  ggplot(aes(h, MSE)) +
  geom_point()
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note03_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

## 3.5 예측 구간

``` r
naive(goog200)
```

    ##     Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
    ## 201       531.4783 523.5222 539.4343 519.3105 543.6460
    ## 202       531.4783 520.2267 542.7298 514.2705 548.6861
    ## 203       531.4783 517.6980 545.2586 510.4031 552.5534
    ## 204       531.4783 515.5661 547.3904 507.1428 555.8138
    ## 205       531.4783 513.6880 549.2686 504.2704 558.6862
    ## 206       531.4783 511.9900 550.9666 501.6735 561.2830
    ## 207       531.4783 510.4285 552.5280 499.2854 563.6711
    ## 208       531.4783 508.9751 553.9814 497.0627 565.8939
    ## 209       531.4783 507.6101 555.3465 494.9750 567.9815
    ## 210       531.4783 506.3190 556.6375 493.0005 569.9561

``` r
autoplot(naive(goog200))
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note03_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

``` r
naive(goog200, bootstrap = TRUE)
```

    ##     Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
    ## 201       531.4783 525.7393 537.4780 522.8631 541.0597
    ## 202       531.4783 523.3274 539.2353 519.6329 546.0318
    ## 203       531.4783 521.2311 541.1579 516.7141 550.2375
    ## 204       531.4783 519.4433 543.0886 514.1419 554.5838
    ## 205       531.4783 517.5972 544.5708 511.9564 569.5856
    ## 206       531.4783 516.1895 546.2369 509.8087 580.5718
    ## 207       531.4783 515.0102 547.6506 507.9360 584.2709
    ## 208       531.4783 513.7513 548.6745 505.7770 585.6771
    ## 209       531.4783 512.3599 550.0394 503.7788 586.3218
    ## 210       531.4783 510.8417 551.2435 502.1582 586.2408

``` r
autoplot(naive(goog200, bootstrap = TRUE))
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note03_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

## 3.6 R의 forecast 패키지

``` r
forecast(ausbeer, h = 4)
```

    ##         Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
    ## 2010 Q3       404.6025 385.8695 423.3355 375.9529 433.2521
    ## 2010 Q4       480.3982 457.5283 503.2682 445.4216 515.3748
    ## 2011 Q1       417.0367 396.5112 437.5622 385.6456 448.4277
    ## 2011 Q2       383.0996 363.5063 402.6930 353.1341 413.0651
