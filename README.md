# Saunders
R package adds functionality to linear regression graphing functions in ggplot. Named after Professor Garrett Saunders at Brigham Young University - Idaho.

## How to Use

This package can be downloaded using `install_github('Saunders','jamesbeeson01')` in the `devtools` package.

Example:

```
install.packages("devtools")
devtools::install_github('jamesbeeson01/Saunders')

library(Saunders)
```

Functions can them be added to ggplots with + like other geom layers

```
lm.displ2 <- lm(hwy ~ displ + I(displ^2), mpg)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_fit(lm.displ2)
```

See test.Rmd for more examples

## TODO

- Remove geom_confidence (superseded by geom_fit)
- Change function to geom_transform
- Add se = FALSE
- Match the transformation to boxCox
  - In other words, allow a number, such as 0.5
- rename lm argument to model

geom_fit
- Coloring is not intuitive - does not match legend or chart aesthetic
- Does not play with ggplot aesthetics (ie color)
- Does not facet
- Does not intuit
     By this I mean I would like it to infer columns that aren't provided
     It could take the smallest value of a column, or unique values of every remaining column
