## ----setup, include = FALSE----------------------------------------------
library(ggplot2)
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

## ---- message=FALSE------------------------------------------------------
ggplot(mtcars, aes(x = hp, y = wt, fill = qsec, col = qsec)) +
  geom_jitter(size = 15, shape = 22) +
  guides(fill = FALSE, col = FALSE) +
  scale_color_gradient(low = col_graydon[8], high = col_graydon[4]) +
  scale_fill_gradient(low = col_graydon[2], high = col_graydon[1]) +
  theme_graydon("blank")

