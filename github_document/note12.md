note12
================

``` r
library(fpp2)
library(ggplot2)
theme_set(theme_gray(base_family = "NanumGothic"))
```

# 12 몇 가지 실제 예측 문제

## 12.1 주별, 일별, 그리고 일별 이하의 데이터

``` r
gasoline |>
  stlf() |>
  autoplot()
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note12_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
bestfit <- list(aicc = Inf)
for (K in seq(25)) {
  fit <- auto.arima(gasoline,
    xreg = fourier(gasoline, K = K),
    seasonal = FALSE
  )
  if (fit[["aicc"]] < bestfit[["aicc"]]) {
    bestfit <- fit
    bestK <- K
  }
}
```

``` r
fc <- forecast(bestfit,
  xreg = fourier(gasoline, K = bestK, h = 104)
)
autoplot(fc)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note12_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## 12.2 개수를 세서 만든 시계열

``` r
productC |>
  croston() |>
  autoplot()
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note12_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## 12.3 예측값이 반드시 범위 안에 있도록 만들기

``` r
eggs |>
  ets(model = "AAN", damped = FALSE, lambda = 0) |>
  forecast(h = 50, biasadj = TRUE) |>
  autoplot()
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note12_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
a <- 50
b <- 400

fit <- log((eggs - a) / (b - eggs)) |>
  ets(model = "AAN", damped = FALSE)

fc <- forecast(fit, h = 50)
fc[["mean"]] <- (b - a) * exp(fc[["mean"]]) /
  (1 + exp(fc[["mean"]])) + a
fc[["lower"]] <- (b - a) * exp(fc[["lower"]]) /
  (1 + exp(fc[["lower"]])) + a
fc[["upper"]] <- (b - a) * exp(fc[["upper"]]) /
  (1 + exp(fc[["upper"]])) + a
fc[["x"]] <- eggs
autoplot(fc)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note12_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## 12.4 예측조합

``` r
train <- window(auscafe, end = c(2012, 9))
h <- length(auscafe) - length(train)
ETS <- forecast(ets(train), h = h)
ARIMA <- forecast(auto.arima(train, lambda = 0, biasadj = TRUE), h = h)
STL <- stlf(train, lambda = 0, h = h, biasadj = TRUE)
NNAR <- forecast(nnetar(train), h = h)
TBATS <- forecast(tbats(train, biasadj = TRUE), h = h)
Combination <- (ETS[["mean"]] + ARIMA[["mean"]] + STL[["mean"]] +
  NNAR[["mean"]] + +TBATS[["mean"]]) / 5
```

``` r
autoplot(auscafe) +
  autolayer(ETS, series = "ETS", PI = FALSE) +
  autolayer(ARIMA, series = "ARIMA", PI = FALSE) +
  autolayer(STL, series = "STL", PI = FALSE) +
  autolayer(NNAR, series = "NNAR", PI = FALSE) +
  autolayer(TBATS, series = "TBATS", PI = FALSE) +
  autolayer(Combination, series = "Combination") +
  xlab("연도") + ylab("10억 $") +
  ggtitle("호주 월별 외식비 지출")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note12_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
c(
  ETS = accuracy(ETS, auscafe)["Test set", "RMSE"],
  ARIMA = accuracy(ARIMA, auscafe)["Test set", "RMSE"],
  `STL-ETS` = accuracy(STL, auscafe)["Test set", "RMSE"],
  NNAR = accuracy(NNAR, auscafe)["Test set", "RMSE"],
  TBATS = accuracy(TBATS, auscafe)["Test set", "RMSE"],
  Combination = accuracy(Combination, auscafe)["Test set", "RMSE"]
)
```

    ##         ETS       ARIMA     STL-ETS        NNAR 
    ##  0.13699696  0.15919858  0.19310148  0.29769286 
    ##       TBATS Combination 
    ##  0.09406039  0.07127258

## 12.5 합산 값에 대한 예측 구간

``` r
fit <- ets(gas / 1000)
fc <- forecast(fit, h = 6)
nsim <- 10000
h <- 6
sim <- numeric(nsim)
for (i in seq_len(nsim)) {
  sim[i] <- sum(simulate(fit, future = TRUE, nsim = h))
}
meanagg <- mean(sim)
```

``` r
sum(fc[["mean"]][1:6])
```

    ## [1] 281.8006

``` r
meanagg
```

    ## [1] 281.7485

``` r
quantile(sim, prob = c(0.1, 0.9))
```

    ##      10%      90% 
    ## 262.9937 301.2593

``` r
quantile(sim, prob = c(0.025, 0.975))
```

    ##     2.5%    97.5% 
    ## 253.2881 311.4890

## 12.6 과거 재구성

``` r
reverse_ts <- function(y) {
  ts(rev(y), start = tsp(y)[1L], frequency = frequency(y))
}
```

``` r
reverse_forecast <- function(object) {
  h <- length(object[["mean"]])
  f <- frequency(object[["mean"]])
  object[["x"]] <- reverse_ts(object[["x"]])
  object[["mean"]] <- ts(rev(object[["mean"]]),
    end = tsp(object[["x"]])[1L] - 1 / f, frequency = f
  )
  object[["lower"]] <- object[["lower"]][h:1L, ]
  object[["upper"]] <- object[["upper"]][h:1L, ]
  return(object)
}
```

``` r
# Backcast example
euretail %>%
  reverse_ts() %>%
  auto.arima() %>%
  forecast() %>%
  reverse_forecast() -> bc
autoplot(bc) +
  ggtitle(paste(bc[["method"]], "로 재구성한 결과"))
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note12_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

## 12.7 아주 긴 시계열과 아주 짧은 시계열

``` r
# install.packages("Mcomp")
library(Mcomp)
library(purrr)
```

``` r
n <- map_int(M1, function(x) {
  length(x[["x"]])
})
M1[n < 20] |>
  map_int(function(u) {
    u[["x"]] |>
      auto.arima() |>
      coefficients() |>
      length()
  }) |>
  table()
```

    ## 
    ##  0  1  2  3 
    ## 54 73 15  2

## 12.8 학습 데이터와 테스트 데이터 예측하기

``` r
training <- subset(auscafe, end = length(auscafe) - 61)
test <- subset(auscafe, start = length(auscafe) - 60)
cafe.train <- Arima(training,
  order = c(2, 1, 1),
  seasonal = c(0, 1, 2), lambda = 0
)
cafe.train |>
  forecast(h = 60) |>
  autoplot() +
  autolayer(test)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note12_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
autoplot(training, series = "학습 데이터") +
  autolayer(fitted(cafe.train, h = 12), series = "12단계 적합값")
```

    ## Warning: Removed 25 row(s) containing missing values
    ## (geom_path).

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note12_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
cafe.test <- Arima(test, model = cafe.train)
accuracy(cafe.test)
```

    ##                        ME       RMSE        MAE
    ## Training set -0.002622226 0.04591291 0.03412845
    ##                      MPE     MAPE      MASE
    ## Training set -0.07300775 1.002202 0.1898996
    ##                     ACF1
    ## Training set -0.05704137

## 12.9 결측값과 이상값 다루기

``` r
sum(is.na(gold))
```

    ## [1] 34

``` r
gold2 <- na.interp(gold)
autoplot(gold2, series = "내삽한 것") +
  autolayer(gold, series = "원본") +
  scale_colour_manual(
    values = c(`내삽한 것` = "red", `원본` = "gray")
  ) +
  xlab("시간")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note12_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
tsoutliers(gold)
```

    ## $index
    ## [1] 770
    ## 
    ## $replacements
    ## [1] 494.9

``` r
gold[768:772]
```

    ## [1] 495.00 502.75 593.70 487.05 487.75

``` r
gold %>%
  tsclean() %>%
  ets() %>%
  forecast(h = 50) %>%
  autoplot()
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note12_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->
