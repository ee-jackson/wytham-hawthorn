Conspecific density correlations?
================
eleanorjackson
05 September, 2024

``` r
library("tidyverse")
library("here")
library("ggpubr")
library("patchwork")
```

``` r
readRDS(here::here("data", "clean", "connectivity_data.rds")) %>%
  filter(plot %% 1 == 0) %>%  
  ggplot(aes(x = repro_connectivity, y = non_repro_connectivity)) +
  geom_point(shape = 16, alpha = 0.8) +
  ggpubr::stat_cor() +
  geom_smooth(method = "lm") +
  labs(
       x = "Reproductive conspecific density", 
       y = "Non-reproductive conspecific density") +
  theme_classic(base_size = 12)
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](figures/2024-02-16_density-pairs/unnamed-chunk-1-1.png)<!-- -->
