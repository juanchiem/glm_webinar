bacteria <- MASS::bacteria
head(bacteria)

# Challenge 1 - Solution

# Fit models (full to most parsimonious)
model.bact1 <- glm(y ~ trt * week, data = bacteria, family = binomial)
model.bact2 <- glm(y ~ trt + week, data = bacteria, family = binomial)
model.bact3 <- glm(y ~ week, data = bacteria, family = binomial)

# Let's compare these models using a likelihood ratio test (LRT).
anova(model.bact1, model.bact2, model.bact3, test = "LRT")
