note02
================

``` r
library(ggplot2)
library(fpp2)
theme_set(theme_grey(base_family = "NanumGothic"))
```

# 시계열 시각화

## 2.1 ts 객체

``` r
y <- ts(c(123, 39, 78, 52, 110), start = 2012) # 기본이 year
```

``` r
ts(rnorm(100, mean = 100, sd = 10), start = 2003, frequency = 12)
```

    ##            Jan       Feb       Mar       Apr       May       Jun       Jul
    ## 2003  91.51028 101.23218  93.80316  82.86279 107.15828  90.84473 125.98402
    ## 2004 103.48251  97.09509 115.76622 108.46284  90.80636 104.00551 103.54369
    ## 2005  99.14777 105.01589  92.74798 111.05369  93.67428  98.02801 108.51536
    ## 2006  89.34047 115.72125  92.69551 104.50516 105.64419  97.78223  95.30687
    ## 2007 102.11918  86.16286 105.11681  88.41084 106.71755  94.52029  96.62656
    ## 2008  98.81130 101.79860  93.39241  81.12080 113.45975  87.71453 104.06367
    ## 2009 116.99350 105.18437  81.39550 107.39199  91.03834 101.85919  88.21712
    ## 2010 110.05982 117.61158 111.88018  89.25138  87.65062  92.03298  79.40237
    ## 2011  87.87353  82.68198  87.35041  90.03081                              
    ##            Aug       Sep       Oct       Nov       Dec
    ## 2003  94.88387  91.33646 103.87218 109.74056 103.99185
    ## 2004 116.57768 111.14789  92.59384 120.10885  99.74390
    ## 2005  94.33845  98.47769  95.71087 122.18703  79.66420
    ## 2006 105.72690  87.06788 102.61328 101.00172 102.69557
    ## 2007 101.15061  92.07140 104.90014 102.42903 101.18965
    ## 2008  91.27814 110.45128 113.50268  94.98702  80.37794
    ## 2009  95.97977  97.62680  98.78366  89.19034 115.22309
    ## 2010  96.59596  98.05280  92.95516 115.02012  85.08081
    ## 2011

``` r
ts(rnorm(100, mean = 100, sd = 10), start = 2003, frequency = 4)
```

    ##           Qtr1      Qtr2      Qtr3      Qtr4
    ## 2003  92.34407  98.73634  90.47116  94.28006
    ## 2004 103.03251  83.87342  83.20949 104.01179
    ## 2005  79.46767  91.45666 111.22517  98.49445
    ## 2006 107.41526 105.94205  88.97810  83.19455
    ## 2007 100.46658  90.29904  76.75272  90.89961
    ## 2008 102.32596  96.26171  80.94553  96.92911
    ## 2009  84.43655  95.83716  93.27392 110.75197
    ## 2010 101.22462  98.29621 116.48091  82.25550
    ## 2011  88.12505 114.11069 105.82233 112.71146
    ## 2012  89.01044  85.41366 104.22431  93.13640
    ## 2013  76.30368 115.93847 112.99468 102.76673
    ## 2014  94.76275  98.44653  97.27176 106.40624
    ## 2015  87.25081  98.37202  98.01420 104.02529
    ## 2016 111.15533  97.40826 112.55731  85.44601
    ## 2017 104.95843 103.34972  88.89143 100.65167
    ## 2018 107.18876 101.12939 109.51800  94.14630
    ## 2019  90.51986 108.76035  80.48952  93.05482
    ## 2020 108.61935  97.82787 113.43761  97.45082
    ## 2021 121.55576 118.94728 113.52995  94.58497
    ## 2022 107.84476  94.55600  97.36523 115.99170
    ## 2023  92.49083 101.87020  94.48706  95.15004
    ## 2024  92.09069 100.28980 121.72181  95.92008
    ## 2025  89.39414 107.72971  82.60013  89.58196
    ## 2026 107.34486  87.72996 110.32342  90.85655
    ## 2027 104.85559  92.91449  75.79756  97.23051

``` r
ts(rnorm(100, mean = 100, sd = 10), start = 2003, frequency = 1)
```

    ## Time Series:
    ## Start = 2003 
    ## End = 2102 
    ## Frequency = 1 
    ##   [1]  92.73349 112.58510  92.50637 106.84927 102.74890  97.38680  91.17915
    ##   [8]  91.29821  87.88735  96.05099  82.73362  85.94393 103.27321  96.34395
    ##  [15] 124.36746  92.05748 116.60703  86.29329 111.76076  97.23215  92.86696
    ##  [22]  97.00560  92.95986  99.11446 104.92926  71.95928  90.41493 114.93467
    ##  [29] 105.73750 110.50224  95.25652 105.24750  89.25381 102.21661  82.76659
    ##  [36]  96.12315 102.78994 126.75863  98.70713  95.76382  99.47291  99.44707
    ##  [43] 108.95321 108.99355  94.80360  93.34385  98.12887 107.72234 116.29927
    ##  [50]  81.87720  92.42266 101.92122 114.15329  99.68712 117.17432  93.94661
    ##  [57] 105.92474 101.41924  91.23721 101.22771  89.62793 103.67321 114.98441
    ##  [64] 110.85488  81.41459  99.56599 105.60633  85.56325 101.38813 109.02809
    ##  [71] 106.39036 101.45769  88.87208 107.23301  89.02018 105.18419  94.00825
    ##  [78] 104.00371 102.17977  94.40458 109.69766  98.93792  96.53491  89.21865
    ##  [85] 103.78361 119.61309 110.02693  99.23452 109.68205 120.91298 108.47520
    ##  [92]  87.77130  94.09624 110.85277  97.14415  93.92098 119.43498  74.46054
    ##  [99]  98.27286  89.78245

``` r
ts(rnorm(100, mean = 100, sd = 10), start = 2003, frequency = 52)
```

    ## Time Series:
    ## Start = c(2003, 1) 
    ## End = c(2004, 48) 
    ## Frequency = 52 
    ##   [1] 101.21338  88.26824 101.81679  95.06387  90.42931  98.24285  86.06767
    ##   [8] 102.30731 106.34021  84.73203  82.83434 101.67302  88.78911  77.39875
    ##  [15]  72.86605  90.95217 106.54606 104.66626  96.83607 118.69712 117.70342
    ##  [22]  98.61027  87.72820 102.87556 100.10876 116.60767 106.84731  99.94236
    ##  [29]  97.47407 109.51957 105.82993  98.64441 116.41165  92.41577  91.73576
    ##  [36] 103.26451 114.58287  99.92104 107.22879  92.34325 108.36006  92.20381
    ##  [43]  91.79839  80.90067 116.33330 101.73586 118.40666  89.34865 112.48647
    ##  [50]  86.59833  67.84457 104.32928 119.23586 129.20857  91.65278 100.53738
    ##  [57] 106.88884 107.48723 104.37305  95.23583  81.44596  95.17226  84.93326
    ##  [64] 104.76157 106.16304 101.84445 102.21749 105.90964 110.45627 104.88998
    ##  [71]  63.57990 100.82827  98.77774 105.31331  97.27296 112.45762  86.33223
    ##  [78]  83.45992  98.99656 107.86510  85.15909 107.24053 102.32069 118.37451
    ##  [85]  91.40481  81.59996  95.71182 123.39744 104.70718  73.76084  76.85501
    ##  [92] 104.88944  86.58198 108.84540  89.83820 110.55741  97.65850 113.11155
    ##  [99]  96.59353 111.76995

## 2.2 시간 그래프

``` r
autoplot(melsyd[, "Economy.Class"]) +
  ggtitle("이코노미석 탑승객: 멜버른-시드니") +
  xlab("연도") +
  ylab("탑승객(단위: 1000명)")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note02_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
autoplot(a10) +
  ggtitle("당뇨병 약 매출") +
  ylab("매출(단위: 백만 달러)") +
  xlab("연도")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note02_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

## 2.4 계절성 그래프

``` r
a10 |>
  ggseasonplot(year.labels = TRUE, year.labels.left = TRUE) +
  ylab("백만 달러") +
  xlab("월") +
  ggtitle("계절성 그래프: 당뇨병 약 매출")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note02_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
ggseasonplot(a10, polar = TRUE) +
  ylab("백만 달러") +
  xlab("월") +
  ggtitle("계절성 극좌표 그래프: 당뇨병 약 매출")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note02_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

## 2.5 계절성 부시계열 그래프

``` r
a10 |>
  ggsubseriesplot() +
  ylab("백만 달러") +
  xlab("월") +
  ggtitle("계절성 부시계열 그래프: 당뇨병 약 매출")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note02_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## 2.6 산점도

``` r
autoplot(elecdemand[, c("Demand", "Temperature")], facets = TRUE) +
  xlab("연도: 2014") + ylab("") +
  ggtitle("호주 빅토리아 주의 30분 단위의 전력 수요")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note02_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
qplot(Temperature, Demand, data = as.data.frame(elecdemand)) +
  ylab("수요 (단위: 기가 와트GW)") + xlab("기온 (섭씨)")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note02_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
autoplot(visnights[, 1:5], facets = TRUE) +
  ylab("각 분기별 여행자 숙박일 수(단위: 백만 명)")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note02_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
GGally::ggpairs(as.data.frame(visnights[, 1:5]))
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

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note02_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

## 2.7 시차 그래프

``` r
beer2 <- window(ausbeer, start = 1992)
gglagplot(beer2) +
  guides(color = guide_legend(title = "분기")) +
  ggtitle("")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note02_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

## 2.8 자기상관

``` r
ggAcf(beer2)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note02_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
aelec <- window(elec, start = 1980)
autoplot(aelec) +
  xlab("연도") + ylab("기가와트시(GWh)")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note02_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
ggAcf(aelec, lag = 48)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note02_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

## 2.9 백색잡음

``` r
set.seed(30)
y <- ts(rnorm(50))
autoplot(y) + ggtitle("백색잡음")
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note02_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
ggAcf(y)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note02_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

## 2.10 연습문제

``` r
autoplot(gold)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note02_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
autoplot(woolyrnq)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note02_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
autoplot(gas)
```

![](/Users/gglee/github/note/note-forecasting-principles-and-practice-2018/github_document/note02_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
frequency(gold)
```

    ## [1] 1

``` r
frequency(woolyrnq)
```

    ## [1] 4

``` r
gas |> frequency()
```

    ## [1] 12

``` r
gold |> max(na.rm = TRUE)
```

    ## [1] 593.7

``` r
gold |> which.max()
```

    ## [1] 770