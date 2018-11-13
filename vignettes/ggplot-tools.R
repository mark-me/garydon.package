## ----setup, include = FALSE----------------------------------------------
library(ggplot2)
library(tidyverse)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(graydon.package)

## ---- warning=FALSE------------------------------------------------------
ggplot(mtcars,
       aes(x = qsec, y = hp, col = as.factor(carb))) +
  geom_point() +
  guides(fill = FALSE, col = FALSE) +
  scale_color_graydon() +
  theme_graydon("grid")

## ---- message=FALSE, warning=FALSE---------------------------------------
ggplot(mtcars,
       aes(x = gear, y = qsec, fill = factor(carb))) +
  geom_col() +
  guides(fill = FALSE, col = FALSE) +
  scale_fill_graydon() +
  theme_graydon("horizontal")

## ---- message=FALSE, warning=FALSE---------------------------------------
ggplot(mtcars,
       aes(x = gear, y = qsec, fill = factor(cyl))) +
  geom_col() +
  guides(fill = FALSE, col = FALSE) +
  coord_flip() +
  scale_fill_graydon() +
  theme_graydon("vertical")

## ---- message=FALSE, fig.width = 5---------------------------------------
ggplot(mtcars, aes(x = hp, y = wt, fill = qsec, col = qsec)) +
  geom_jitter(size = 15, shape = 22) +
  guides(fill = FALSE, col = FALSE) +
  scale_color_gradient(low = col_graydon[8], high = col_graydon[4]) +
  scale_fill_gradient(low = col_graydon[2], high = col_graydon[1]) +
  theme_graydon("blank")

## ---- warning=FALSE, fig.width = 7---------------------------------------
diamonds %>% 
  group_by(color, cut) %>% 
  summarise(price_median = median(price)) %>% 
ggplot(aes(x = cut, y = price_median, col = color)) +
  geom_line(aes(group = color), size = 1, alpha = 0.7) +
  geom_point(size = 5) +
  labs(x = "Cut quality", y = "Price", col = "Color") +
  scale_color_graydon() +
  scale_y_numeric(format_EN = TRUE) +
  theme_graydon("grid")

## ---- warning=FALSE, fig.width = 7---------------------------------------
diamonds %>% 
  group_by(color, cut) %>% 
  summarise(price_median = median(price)) %>% 
ggplot(aes(x = cut, y = price_median, col = color)) +
  geom_line(aes(group = color), size = 1, alpha = 0.7) +
  geom_point(size = 5) +
  labs(x = "Cut quality", y = "Price", col = "Color") +
  scale_color_graydon() +
  scale_y_currency(currency = "EUR", scale = "k", number_decimals = 1) +
  theme_graydon("grid")

## ---- warning=FALSE, fig.width = 7---------------------------------------
diamonds %>% 
  mutate(depth = depth / 100) %>% 
  group_by(cut) %>% 
  summarise(price_median = median(price),
            depth_median = median(depth)) %>% 
ggplot(aes(x = depth_median, y = price_median, col = cut)) +
  geom_point(size = 7, alpha = 0.7) +
  labs(x = "Depth", y = "Price", col = "Cut") +
  scale_color_graydon() +
  scale_x_percent(format_EN = TRUE) +
  scale_y_currency(currency = "GBP") +
  theme_graydon("grid")

## ---- fig.width = 5------------------------------------------------------

p_abstract <- ggplot(mtcars, aes(x = hp, y = wt, fill = qsec, col = qsec)) +
  geom_jitter(size = 15, shape = 22, alpha = 0.7) +
  guides(fill = FALSE, col = FALSE) +
  scale_color_gradient(low = col_graydon[8], high = col_graydon[4]) +
  scale_fill_gradient(low = col_graydon[2], high = col_graydon[1]) +
  theme_graydon("blank")

save_plot_to_png(p_abstract, "abstract_art", squared = TRUE)

p_abstract

