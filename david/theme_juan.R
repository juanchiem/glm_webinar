dat_fit = dat %>% 
  group_by(exp) %>%
  do(fits = glm(inf_plants/n_plants ~ days * type,
                weights=n_plants, 
                family="binomial", 
                data=.) )

group_by(dat, exp) %>% 
  do(tidy(car::Anova(glm(inf_plants/n_plants ~ days * type,
                         weights=n_plants, 
                         family="binomial", 
                         data=.)))) %>% View

dat <- within(dat, type <- relevel(type, ref = "Check"))
