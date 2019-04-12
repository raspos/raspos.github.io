Forbes 프로스포츠 리그 가치 평가
================
Won
4/13/2019

## 시작은 이 기사였다.

[기사
바로가기](https://sports.yahoo.com/mlb-nba-team-values-lebron-james-mike-trout-230337318.html)

\#포브스는 매년 미국 4대 스포츠 리그의 가치를 평가해 발표한다. \#이 기사는 올해 처음 NBA의 구단 평균 가치가 MLB를
넘어섰다고 썼다. 직접 확인해보고 싶었다.

## 크롤링

\#크롤링을 위해 포브스 홈페이지의 이 페이지 :
<https://www.forbes.com/nba-valuations/list/> 를 살펴보니 다행히 JSON 형식 서버 페이지를
볼 수 있었다.

[JSON
페이지](https://www.forbes.com/ajax/list/data?year=2019&uri=nba-valuations&type=organization)

\#2012년 데이터부터 확인할 수 있었으며, url 안에 리그 이름만 바꿔주면 쉽게 데이터를 불러 올 수 있게 돼 있었다.

``` r
league <- c("mlb", "nba", "nfl", "nhl")

value_usapro <- NULL
for(i in league){
  if(i == "mlb" | i == "nba"){
    year <- 2012:2019
  } else {
    year <- 2012:2018
  }
    for(t in year){
      a <- fromJSON(str_c('https://www.forbes.com/ajax/list/data?year=', t , '&uri=', i, '-valuations&type=organization'))
      a <- a[,c(1:10)]
      a <- a %>%
        mutate(year = t, league = i)
      a <- a[,c(4, 11, 12, 6:10, 2)]
      value_usapro <- rbind(value_usapro, a)
      print(str_c(i, '의 ', t, '시즌을 탐색 중입니다'))
    }
}
```

``` r
value_usapro %>%  head(10)
```

    ##                     uri year league operatingIncome revenue debtValue
    ## 1  arizona-diamondbacks 2012    mlb            27.2     186        39
    ## 2        atlanta-braves 2012    mlb            20.7     203         0
    ## 3     baltimore-orioles 2012    mlb            12.9     179        33
    ## 4        boston-red-sox 2012    mlb            25.4     310        24
    ## 5          chicago-cubs 2012    mlb            28.1     266        66
    ## 6     chicago-white-sox 2012    mlb            10.7     214         7
    ## 7       cincinnati-reds 2012    mlb            17.1     185        10
    ## 8     cleveland-indians 2012    mlb            30.1     178        27
    ## 9      colorado-rockies 2012    mlb            14.4     193        15
    ## 10       detroit-tigers 2012    mlb             8.2     217        39
    ##    oneYearValueChange valueList rank
    ## 1                  13       447   23
    ## 2                   5       508   15
    ## 3                  12       460   19
    ## 4                  10      1000    3
    ## 5                  14       879    4
    ## 6                  14       600   10
    ## 7                  13       424   24
    ## 8                  16       410   26
    ## 9                  12       464   18
    ## 10                 24       478   17

# 2012년부터 2019년까지 각 리그별 가치의 합계와 평균 값을 구해봤다.

``` r
value_sum <- value_usapro %>%
  group_by(league, year) %>%
  summarise(sum = sum(valueList), teams = n()) 

value_sum <- value_sum %>% 
  mutate(ave = sum / teams) 

value_sum$ave <- round(value_sum$ave)
value_sum   
```

    ## # A tibble: 30 x 5
    ## # Groups:   league [4]
    ##    league  year   sum teams   ave
    ##    <chr>  <int> <int> <int> <dbl>
    ##  1 mlb     2012 18153    30   605
    ##  2 mlb     2013 22307    30   744
    ##  3 mlb     2014 24327    30   811
    ##  4 mlb     2015 35985    30  1200
    ##  5 mlb     2016 38630    30  1288
    ##  6 mlb     2017 46105    30  1537
    ##  7 mlb     2018 49335    30  1644
    ##  8 mlb     2019 53280    30  1776
    ##  9 nba     2012 11776    30   393
    ## 10 nba     2013 15271    30   509
    ## # … with 20 more rows

# mlb와 nba만 따로 뽑아 비교해봤다.

``` r
value_sum_vs <- value_sum %>%
  filter(league == "nba" | league == "mlb")
value_sum_vs %>% head(16)
```

    ## # A tibble: 16 x 5
    ## # Groups:   league [2]
    ##    league  year   sum teams   ave
    ##    <chr>  <int> <int> <int> <dbl>
    ##  1 mlb     2012 18153    30   605
    ##  2 mlb     2013 22307    30   744
    ##  3 mlb     2014 24327    30   811
    ##  4 mlb     2015 35985    30  1200
    ##  5 mlb     2016 38630    30  1288
    ##  6 mlb     2017 46105    30  1537
    ##  7 mlb     2018 49335    30  1644
    ##  8 mlb     2019 53280    30  1776
    ##  9 nba     2012 11776    30   393
    ## 10 nba     2013 15271    30   509
    ## 11 nba     2014 19029    30   634
    ## 12 nba     2015 33185    30  1106
    ## 13 nba     2016 37360    30  1245
    ## 14 nba     2017 40660    30  1355
    ## 15 nba     2018 49545    30  1652
    ## 16 nba     2019 56050    30  1868

# nba와 mlb의 연도별 평균 가치를 바그래프로 그려봤다.

``` r
value_sum$year <- as.character(value_sum$year)
```

``` r
ggplot(data = value_sum_vs, aes(x = year, y = ave, fill = league)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_smooth(method = loess) +
  geom_text(aes(label = ave), size = 2.5, vjust = -0.5, position = position_dodge(width = 1)) +
  theme_set(theme_fivethirtyeight(base_family = "AppleGothic"))
```

![](github_2019-04-13_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

#그런데 이미 2018년에 nba의 구단 평균 가치가 mlb의 구단 평균 가치를 넘어선 것을 알 수 있다. 최초는 아니다. 참고로 mlb 구단 가치는 이틀 전에 나왔다.

#재작년에 이 기사<https://news.joins.com/article/21100242>를 쓸 때 엑셀 시트에 숫자를 일일 적었던 기억이 난다. R을 배우니 이렇게 편하네.

#데이를 좀 더 만져봐야겠다.
