Conspecific density correlations?
================
eleanorjackson
19 February, 2024

``` r
library("tidyverse")
library("here")
library("ggpubr")
```

``` r
readRDS(here::here("data", "clean", "fruit_drop_data.rds")) %>%
  ggplot(aes(x = repro_connectivity, y = non_repro_connectivity)) +
  geom_point() +
  ggpubr::stat_cor() +
  geom_smooth(method = "lm", 
              colour = "blue") +
  labs(title = "Fruit set")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](figures/2024-02-16_density-pairs/unnamed-chunk-1-1.png)<!-- -->

without outliers:

``` r
readRDS(here::here("data", "clean", "fruit_drop_data.rds")) %>%
  ggplot(aes(x = repro_connectivity, y = non_repro_connectivity)) +
  geom_point() +
  xlim(0, 5000) +
  ggpubr::stat_cor() +
  geom_smooth(method = "lm", 
              colour = "blue") +
  labs(title = "Fruit set") 
```

    ## Warning: Removed 12 rows containing non-finite values (`stat_cor()`).

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 12 rows containing non-finite values (`stat_smooth()`).

    ## Warning: Removed 12 rows containing missing values (`geom_point()`).

![](figures/2024-02-16_density-pairs/unnamed-chunk-2-1.png)<!-- -->

``` r
readRDS(here::here("data", "clean", "fruit_drop_data_short.rds")) %>%
  ggplot(aes(x = repro_connectivity, y = non_repro_connectivity)) +
  geom_point() +
  ggpubr::stat_cor() +
  geom_smooth(method = "lm", 
              colour = "blue") +
  labs(title = "Fruit drop (short)")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](figures/2024-02-16_density-pairs/unnamed-chunk-3-1.png)<!-- -->

without outliers:

``` r
readRDS(here::here("data", "clean", "fruit_drop_data_short.rds")) %>%
  ggplot(aes(x = repro_connectivity, y = non_repro_connectivity)) +
  geom_point() +
  xlim(0, 5000) +
  ggpubr::stat_cor() +
  geom_smooth(method = "lm", 
              colour = "blue") +
  labs(title = "Fruit drop (short)")
```

    ## Warning: Removed 26 rows containing non-finite values (`stat_cor()`).

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 26 rows containing non-finite values (`stat_smooth()`).

    ## Warning: Removed 26 rows containing missing values (`geom_point()`).

![](figures/2024-02-16_density-pairs/unnamed-chunk-4-1.png)<!-- -->

``` r
readRDS(here::here("data", "clean", "fruit_drop_data.rds")) %>%
  ggplot(aes(x = repro_connectivity, y = non_repro_connectivity)) +
  geom_point() +
  ggpubr::stat_cor() +
  geom_smooth(method = "lm", 
              colour = "blue") +
  labs(title = "Fruit drop (long)")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](figures/2024-02-16_density-pairs/unnamed-chunk-5-1.png)<!-- -->

without outliers:

``` r
readRDS(here::here("data", "clean", "fruit_drop_data.rds")) %>%
  ggplot(aes(x = repro_connectivity, y = non_repro_connectivity)) +
  geom_point() +
  xlim(0, 5000) +
  ggpubr::stat_cor() +
  geom_smooth(method = "lm", 
              colour = "blue") +
  labs(title = "Fruit drop (long)")
```

    ## Warning: Removed 12 rows containing non-finite values (`stat_cor()`).

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 12 rows containing non-finite values (`stat_smooth()`).

    ## Warning: Removed 12 rows containing missing values (`geom_point()`).

![](figures/2024-02-16_density-pairs/unnamed-chunk-6-1.png)<!-- -->
