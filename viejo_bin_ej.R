

```{r}
# generate data
# set.seed(1)
# x = rnorm(100, 12, 4)             ## sample x from a normal distribution
# mu = 1 + 0.5*x                   ## linear predictor
# prob = 1 / (1 + exp(-mu))         ## transform linear predictor with inverse of "logit" link
# y = rbinom(100, 1, prob = prob)   ## sample y from probabilities with binomial error distribution
# hist(y)
# plot(x,y)
```

```{r}
binomial_dat <- rio::import("https://raw.githubusercontent.com/juanchiem/agro_data/master/binomial.csv")
binomial_dat %>% head
# llamemos a este tipo de dataset: "binomial desplegado"
with(binomial_dat, plot(x,y))
```

```{r}
mod_lm_2 <- lm(y ~ x, data=binomial_dat)
plot_model(mod_lm_2, type='pred', show.data=T, ci.lvl=NA)
```

```{r}
glm1 <- glm(y~x, 
            family = binomial(link = 'logit'), 
            data=binomial_dat)
```

```{r}
plot_model(glm1, type='pred', show.data=T, terms='x [all]')
```