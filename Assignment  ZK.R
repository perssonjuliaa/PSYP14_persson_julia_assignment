# Assignment 1
# ----- section 1 ----
data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")
print(data_sample_1)

data_sample_5 <- data_sample_1[-c(93), ]
all(complete.cases(data_sample_1))

# ----- section 2 ----
model1=lm(pain ~ age + sex, data = data_sample_5)
summary(model1)

# ----- section 3 ----
model2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva + cortisol_serum, data = data_sample_5)
summary(model2)


# ----- section 4 ----

AIC(model1)

AIC(model2)

anova(model1, model2)

# ----- section 5 ----
lm.beta(model1)
lm.beta(model2)
confint(model1)
confint(model2)

# Assignment 2 
# ----- section 1 ----
data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")
print(data_sample_1)

model3 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data_sample_5)
summary(model3)
AIC(model3)

model4 = stepAIC(model3, direction = "backward")
summary(model4)
AIC(model4)

# ----- section 2 ----

backward_model = lm(pain ~ sex + age + pain_cat + mindfulness + cortisol_serum + household_income, data_sample_5)
summary(backward_model)
theory_based_model = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva + cortisol_serum, data = data_sample_5)
summary(theory_based_model)

AIC(theory_based_model)
AIC(backward_model)
anova(theory_based_model, backward_model)
lm.beta(backward_model)
confint(backward_model)


# ----- section 3 ----
data_sample_2 = read.csv("https://tinyurl.com/ha-dataset2")
all(complete.cases(data_sample_2))

backward_model = lm(pain ~ sex + age + pain_cat + mindfulness  + cortisol_serum + household_income, data_sample_2)
summary(backward_model)
theory_based_model = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva + cortisol_serum, data = data_sample_2)
summary(theory_based_model)

pain_theory_based_model = predict(theory_based_model, data = data_sample_2)
pain_backward_model = predict(backward_model, data = data_sample_2)

pain_real = data_sample_2[,2]

pain_diff_theory_based_model = sum(abs(pain_real - pain_theory_based_model))
pain_diff_backward_model = sum(abs(pain_real - pain_backward_model))

AIC(backward_model)
AIC(theory_based_model)
# ----- section 4 ----

hist(abs(pain_real - pain_backward_model))

# Assignment 3 
# ----- section 1 ----

data_sample_3 = read.csv("https://tinyurl.com/ha-dataset3")
data_sample_4 = read.csv("https://tinyurl.com/ha-dataset4")

all(complete.cases(data_sample_3))
all(complete.cases(data_sample_4))

model_5 = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva + cortisol_serum + (1 | hospital), data = data_sample_3)


summary(model_5)

confint(model_5)
confint(model2)
#coef(model_5)

pain_model_5 = predict(model_5, data = data_sample_4)

r.squaredGLMM(pain_model_5)

model_6 = lmer(pain ~ STAI_trait + (1 | hospital), data = data_sample_3)
summary(model_6)
