note05
================

# 5 시계열 회귀 모델

## 5.1 선형 모델

``` r
library(fpp2)
library(ggplot2)
theme_set(theme_grey(base_family = "NanumGothic"))
```

``` r
autoplot(uschange[, c("Consumption", "Income")]) +
  ylab("% 변화") + xlab("연도") +
  guides(colour = guide_legend(title = " "))
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note05_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

``` r
uschange |>
  as.data.frame() |>
  ggplot(aes(Income, Consumption)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("소비 (분기별 % 변화)") +
  xlab("소득 (분기별 % 변화)")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note05_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

``` r
tslm(Consumption ~ Income, data = uschange)
```

    ## 
    ## Call:
    ## tslm(formula = Consumption ~ Income, data = uschange)
    ## 
    ## Coefficients:
    ## (Intercept)       Income  
    ##      0.5451       0.2806

``` r
uschange |>
  autoplot(facets = TRUE)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note05_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

``` r
uschange |>
  as.data.frame() |>
  GGally::ggpairs()
```

    ## plot: [1,1] [=>-------------------------------------------------] 4% est: 0s
    ## plot: [1,2] [===>-----------------------------------------------] 8% est: 0s
    ## plot: [1,3] [=====>---------------------------------------------] 12% est: 0s
    ## plot: [1,4] [=======>-------------------------------------------] 16% est: 0s
    ## plot: [1,5] [=========>-----------------------------------------] 20% est: 0s
    ## plot: [2,1] [===========>---------------------------------------] 24% est: 0s
    ## plot: [2,2] [=============>-------------------------------------] 28% est: 0s
    ## plot: [2,3] [===============>-----------------------------------] 32% est: 0s
    ## plot: [2,4] [=================>---------------------------------] 36% est: 0s
    ## plot: [2,5] [===================>-------------------------------] 40% est: 0s
    ## plot: [3,1] [=====================>-----------------------------] 44% est: 0s
    ## plot: [3,2] [=======================>---------------------------] 48% est: 0s
    ## plot: [3,3] [==========================>------------------------] 52% est: 0s
    ## plot: [3,4] [============================>----------------------] 56% est: 0s
    ## plot: [3,5] [==============================>--------------------] 60% est: 0s
    ## plot: [4,1] [================================>------------------] 64% est: 0s
    ## plot: [4,2] [==================================>----------------] 68% est: 0s
    ## plot: [4,3] [====================================>--------------] 72% est: 0s
    ## plot: [4,4] [======================================>------------] 76% est: 0s
    ## plot: [4,5] [========================================>----------] 80% est: 0s
    ## plot: [5,1] [==========================================>--------] 84% est: 0s
    ## plot: [5,2] [============================================>------] 88% est: 0s
    ## plot: [5,3] [==============================================>----] 92% est: 0s
    ## plot: [5,4] [================================================>--] 96% est: 0s
    ## plot: [5,5] [===================================================]100% est: 0s

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note05_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

## 5.2 최소 제곱 추정

``` r
fit.consMR <-
  uschange |>
  tslm(Consumption ~ Income + Production + Unemployment + Savings, data = _)
summary(fit.consMR)
```

    ## 
    ## Call:
    ## tslm(formula = Consumption ~ Income + Production + Unemployment + 
    ##     Savings, data = uschange)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.88296 -0.17638 -0.03679  0.15251  1.20553 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   0.26729    0.03721   7.184 1.68e-11 ***
    ## Income        0.71449    0.04219  16.934  < 2e-16 ***
    ## Production    0.04589    0.02588   1.773   0.0778 .  
    ## Unemployment -0.20477    0.10550  -1.941   0.0538 .  
    ## Savings      -0.04527    0.00278 -16.287  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3286 on 182 degrees of freedom
    ## Multiple R-squared:  0.754,  Adjusted R-squared:  0.7486 
    ## F-statistic: 139.5 on 4 and 182 DF,  p-value: < 2.2e-16

``` r
autoplot(uschange[, "Consumption"], series = "Data") +
  autolayer(fitted(fit.consMR), series = "Fitted") +
  xlab("연도") + ylab("") +
  ggtitle("미국 소비 지출의 백분율 변화") +
  guides(colour = guide_legend(title = " "))
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note05_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

``` r
cbind(
  Data = uschange[, "Consumption"],
  Fitted = fitted(fit.consMR)
) |>
  as.data.frame() |>
  ggplot(aes(Data, Fitted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  ylab("적합값 (예측된 값)") +
  xlab("데이터 (예측된 값)") +
  ggtitle("미국 소비 지출의 백분율 변화")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note05_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

## 5.3 회귀 모델 평가

``` r
checkresiduals(fit.consMR)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note05_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

    ## 
    ##  Breusch-Godfrey test for serial correlation of order up to 8
    ## 
    ## data:  Residuals from Linear regression model
    ## LM test = 14.874, df = 8, p-value = 0.06163

``` r
df <- as.data.frame(uschange)
df[, "Residuals"] <- as.numeric(residuals(fit.consMR))
p1 <- ggplot(df, aes(Income, Residuals)) +
  geom_point()
p2 <- ggplot(df, aes(Production, Residuals)) +
  geom_point()
p3 <- ggplot(df, aes(Savings, Residuals)) +
  geom_point()
p4 <- ggplot(df, aes(Unemployment, Residuals)) +
  geom_point()
gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note05_files/figure-gfm/unnamed-chunk-44-1.png)<!-- -->

``` r
cbind(
  Fitted = fitted(fit.consMR),
  Residuals = residuals(fit.consMR)
) |>
  as.data.frame() |>
  ggplot(aes(Fitted, Residuals)) +
  geom_point()
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note05_files/figure-gfm/unnamed-chunk-45-1.png)<!-- -->

``` r
aussies <- ausair |> window(end = 2011)
fit <- tslm(aussies ~ guinearice)
summary(fit)
```

    ## 
    ## Call:
    ## tslm(formula = aussies ~ guinearice)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.9448 -1.8917 -0.3272  1.8620 10.4210 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   -7.493      1.203  -6.229 2.25e-07 ***
    ## guinearice    40.288      1.337  30.135  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.239 on 40 degrees of freedom
    ## Multiple R-squared:  0.9578, Adjusted R-squared:  0.9568 
    ## F-statistic: 908.1 on 1 and 40 DF,  p-value: < 2.2e-16

``` r
checkresiduals(fit)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note05_files/figure-gfm/unnamed-chunk-47-1.png)<!-- -->

    ## 
    ##  Breusch-Godfrey test for serial correlation of order up to 8
    ## 
    ## data:  Residuals from Linear regression model
    ## LM test = 28.813, df = 8, p-value = 0.000342

## 5.4 몇 가지 유용한 예측변수

``` r
beer2 <- ausbeer |> window(start = 1992)
autoplot(beer2) +
  xlab("연도") + ylab("백만 리터")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note05_files/figure-gfm/unnamed-chunk-48-1.png)<!-- -->

``` r
fit.beer <- tslm(beer2 ~ trend + season)
summary(fit.beer)
```

    ## 
    ## Call:
    ## tslm(formula = beer2 ~ trend + season)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -42.903  -7.599  -0.459   7.991  21.789 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 441.80044    3.73353 118.333  < 2e-16 ***
    ## trend        -0.34027    0.06657  -5.111 2.73e-06 ***
    ## season2     -34.65973    3.96832  -8.734 9.10e-13 ***
    ## season3     -17.82164    4.02249  -4.430 3.45e-05 ***
    ## season4      72.79641    4.02305  18.095  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 12.23 on 69 degrees of freedom
    ## Multiple R-squared:  0.9243, Adjusted R-squared:  0.9199 
    ## F-statistic: 210.7 on 4 and 69 DF,  p-value: < 2.2e-16

``` r
autoplot(beer2, series = "데이터") +
  autolayer(fitted(fit.beer), series = "적합값") +
  xlab("연도") + ylab("백만 리터") +
  guides(colour = guide_legend(title = " ")) +
  ggtitle("분기별 맥주 생산량")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note05_files/figure-gfm/unnamed-chunk-50-1.png)<!-- -->

``` r
cycle(beer2)
```

    ##      Qtr1 Qtr2 Qtr3 Qtr4
    ## 1992    1    2    3    4
    ## 1993    1    2    3    4
    ## 1994    1    2    3    4
    ## 1995    1    2    3    4
    ## 1996    1    2    3    4
    ## 1997    1    2    3    4
    ## 1998    1    2    3    4
    ## 1999    1    2    3    4
    ## 2000    1    2    3    4
    ## 2001    1    2    3    4
    ## 2002    1    2    3    4
    ## 2003    1    2    3    4
    ## 2004    1    2    3    4
    ## 2005    1    2    3    4
    ## 2006    1    2    3    4
    ## 2007    1    2    3    4
    ## 2008    1    2    3    4
    ## 2009    1    2    3    4
    ## 2010    1    2

``` r
beer2 |>
  cbind(Data = _, Fitted = fitted(fit.beer)) |>
  as.data.frame() |>
  ggplot(aes(Data, Fitted, color = as.factor(cycle(beer2)))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  scale_color_brewer(palette = "Dark2", name = "분기") +
  xlab("측정값") +
  ylab("적합값") +
  ggtitle("분기별 맥주 생산량")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note05_files/figure-gfm/unnamed-chunk-52-1.png)<!-- -->

``` r
fourier.beer <- tslm(beer2 ~ trend + fourier(beer2, K = 2))
summary(fourier.beer)
```

    ## 
    ## Call:
    ## tslm(formula = beer2 ~ trend + fourier(beer2, K = 2))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -42.903  -7.599  -0.459   7.991  21.789 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               446.87920    2.87321 155.533  < 2e-16 ***
    ## trend                      -0.34027    0.06657  -5.111 2.73e-06 ***
    ## fourier(beer2, K = 2)S1-4   8.91082    2.01125   4.430 3.45e-05 ***
    ## fourier(beer2, K = 2)C1-4  53.72807    2.01125  26.714  < 2e-16 ***
    ## fourier(beer2, K = 2)C2-4  13.98958    1.42256   9.834 9.26e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 12.23 on 69 degrees of freedom
    ## Multiple R-squared:  0.9243, Adjusted R-squared:  0.9199 
    ## F-statistic: 210.7 on 4 and 69 DF,  p-value: < 2.2e-16

``` r
fourier.beer <- tslm(beer2 ~ trend + season)
summary(fourier.beer)
```

    ## 
    ## Call:
    ## tslm(formula = beer2 ~ trend + season)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -42.903  -7.599  -0.459   7.991  21.789 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 441.80044    3.73353 118.333  < 2e-16 ***
    ## trend        -0.34027    0.06657  -5.111 2.73e-06 ***
    ## season2     -34.65973    3.96832  -8.734 9.10e-13 ***
    ## season3     -17.82164    4.02249  -4.430 3.45e-05 ***
    ## season4      72.79641    4.02305  18.095  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 12.23 on 69 degrees of freedom
    ## Multiple R-squared:  0.9243, Adjusted R-squared:  0.9199 
    ## F-statistic: 210.7 on 4 and 69 DF,  p-value: < 2.2e-16

## 5.5 예측변수 선택

``` r
# uschange
rbind(
  CV(tslm(Consumption ~ Income + Production + Savings + Unemployment, data = uschange)),
  CV(tslm(Consumption ~ Income + Savings + Unemployment, data = uschange)),
  CV(tslm(Consumption ~ Income + Production + Savings, data = uschange)),
  CV(tslm(Consumption ~ Income + Savings, data = uschange))
) |>
  round(digits = 3)
```

    ##         CV      AIC     AICc      BIC AdjR2
    ## [1,] 0.116 -409.298 -408.831 -389.911 0.749
    ## [2,] 0.116 -408.094 -407.763 -391.939 0.746
    ## [3,] 0.118 -407.467 -407.135 -391.311 0.745
    ## [4,] 0.129 -388.727 -388.507 -375.803 0.716

## 5.6 회귀로 예측하기

``` r
beer2 <- ausbeer |> window(start = 1992)
fit.beer <- tslm(beer2 ~ trend + season)
fcast <- forecast(fit.beer)
autoplot(fcast) +
  ggtitle("회귀를 이용한 맥주 생산량 예측값") +
  xlab("연도") + ylab("백만 리터")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note05_files/figure-gfm/unnamed-chunk-56-1.png)<!-- -->

``` r
fit.consBest <- tslm(Consumption ~ Income + Savings + Unemployment,
  data = uschange
)
fit.consBest
```

    ## 
    ## Call:
    ## tslm(formula = Consumption ~ Income + Savings + Unemployment, 
    ##     data = uschange)
    ## 
    ## Coefficients:
    ##  (Intercept)        Income       Savings  Unemployment  
    ##      0.28102       0.73050      -0.04599      -0.34135

``` r
h <- 4
newdata <- data.frame(
  Income = c(1, 1, 1, 1),
  Savings = c(0.5, 0.5, 0.5, 0.5),
  Unemployment = c(0, 0, 0, 0)
)
fcast.up <- forecast(fit.consBest, newdata = newdata)
newdata <- data.frame(
  Income = rep(-1, h),
  Savings = rep(-0.5, h),
  Unemployment = rep(0, h)
)
fcast.down <- forecast(fit.consBest, newdata = newdata)
```

``` r
autoplot(uschange[, 1]) +
  autolayer(fcast.up, PI = TRUE, series = "Up") +
  autolayer(fcast.down, PI = TRUE, series = "Down") +
  xlab("Year") +
  ylab("미국 소비 지출 % 변화")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note05_files/figure-gfm/unnamed-chunk-59-1.png)<!-- -->

``` r
fit.cons <- tslm(Consumption ~ Income, data = uschange)
h <- 4
fcast.ave <- forecast(fit.cons,
  newdata = data.frame(
    Income = rep(mean(uschange[, "Income"]), h)
  )
)

fcast.up <- forecast(fit.cons,
  newdata = data.frame(
    Income = rep(5, h)
  )
)
autoplot(uschange[, "Consumption"]) +
  autolayer(fcast.ave, series = "평균적인 증가") +
  autolayer(fcast.up, series = "극단적인 증가") +
  guides(color = guide_legend(title = "시나리오")) +
  xlab("연도") +
  ylab("미국 소비 % 변화")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note05_files/figure-gfm/unnamed-chunk-60-1.png)<!-- -->

## 5.7 행렬 정식화

## 5.8 비선형 회귀

``` r
h <- 10

fit.lin <- tslm(marathon ~ trend)
fcasts.lin <- forecast(fit.lin, h = h)
fcasts.lin
```

    ##      Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
    ## 2017       124.3707 116.3797 132.3618 112.0919 136.6496
    ## 2018       124.0784 116.0841 132.0728 111.7946 136.3623
    ## 2019       123.7861 115.7884 131.7838 111.4971 136.0751
    ## 2020       123.4938 115.4927 131.4949 111.1996 135.7879
    ## 2021       123.2015 115.1970 131.2060 110.9020 135.5009
    ## 2022       122.9091 114.9012 130.9171 110.6044 135.2139
    ## 2023       122.6168 114.6053 130.6283 110.3066 134.9270
    ## 2024       122.3245 114.3094 130.3396 110.0088 134.6402
    ## 2025       122.0322 114.0134 130.0509 109.7109 134.3535
    ## 2026       121.7399 113.7174 129.7623 109.4129 134.0669

``` r
fit.exp <- tslm(marathon ~ trend, lambda = 0)
fcasts.exp <- forecast(fit.exp, h = h)
```

``` r
t <- time(marathon)
t.break1 <- 1940
t.break2 <- 1980

tb1 <- ts(pmax(0, t - t.break1), start = 1897)
tb2 <- ts(pmax(0, t - t.break2), start = 1897)

fit.pw <- tslm(marathon ~ t + tb1 + tb2)

t.new <- t[length(t)] + seq(h)
tb1.new <- tb1[length(tb1)] + seq(h)
tb2.new <- tb2[length(tb2)] + seq(h)

newdata <- data.frame(
  t = t.new,
  tb1 = tb1.new,
  tb2 = tb2.new
)
fcasts.pw <- forecast(fit.pw, newdata = newdata)
```

``` r
fit.spline <- tslm(marathon ~ t + I(t^2) + I(t^3) + I(tb1^3) + I(tb2^3))
fcasts.spl <- forecast(fit.spline, newdata = newdata)
```

``` r
autoplot(marathon) +
  autolayer(fitted(fit.lin), series = "Linear") +
  autolayer(fitted(fit.exp), series = "Exponential") +
  autolayer(fitted(fit.pw), series = "Piece-wise") +
  autolayer(fitted(fit.spline), series = "Spline") +
  autolayer(fcasts.pw, series = "Piece-wise") +
  autolayer(fcasts.lin, series = "Linear", PI = FALSE) +
  autolayer(fcasts.exp, series = "Exponential", PI = FALSE) +
  autolayer(fcasts.spl, series = "Spline", PI = FALSE) +
  xlab("Year") +
  ylab("우승 기록 (분)") +
  guides(color = guide_legend(title = " "))
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note05_files/figure-gfm/unnamed-chunk-65-1.png)<!-- -->

``` r
marathon |>
  splinef(lambda = 0) |>
  autoplot() +
  xlab("연도")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note05_files/figure-gfm/unnamed-chunk-66-1.png)<!-- -->

``` r
marathon %>%
  splinef(lambda = 0) %>%
  checkresiduals()
```

    ## Warning in modeldf.default(object): Could not find appropriate degrees of freedom
    ## for this model.

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note05_files/figure-gfm/unnamed-chunk-67-1.png)<!-- -->

## 5.9 상관관계, 인과관계, 그리고 예측

## 5.10 연습문제
