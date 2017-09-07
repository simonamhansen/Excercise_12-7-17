# Enter data
pitch = c(233,204,242,130,112,142)
sex = c(rep("female",3),rep("male",3))

# To make datafram
my.df=data.frame(sex, pitch)
my.df

# To make a linear model
model1 = lm(pitch ~ sex, my.df)
summary(model1)

# To get mean female pitch
mean(my.df[my.df$sex=="female",]$pitch)
mean(my.df[my.df$sex=="male",]$pitch)

# To make new dataset and model
age = c(14,23,35,48,52,67)
pitch = c(252,244,240,233,212,204)
new.df = data.frame(age,pitch)
model2 = lm(pitch ~ age, new.df)
summary(model2)

# To make dataset with a subtracted mean 
new.df$age_sub = new.df$age - mean(new.df$age)
model3 = lm(pitch ~ age_sub, new.df)
summary(model3)

# To make plot
plot(fitted(model2), residuals(model2))

# A plot to visualise Homoskedasticity
plot(rnorm(100),rnorm(100))

# To acess normality (maybe not that important)
hist(residuals(model2))
qqnorm(residuals(model2))

# To check for influential datapoints
dfbeta(model2)

# To retrieve relevant libraries
library(lme4)

# To load data
data= read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")
summary(data)

# To find missing data
which(is.na(data)==T)

# To make boxplot to examine random effect
boxplot(frequency ~ attitude*gender,
        col=c("white","lightgray"),data)

# To make a mixed effect model
politeness.model = lmer(frequency ~ attitude + (1|subject) + (1|scenario), data=data) 
politeness.model

politeness.model = lmer(frequency ~ attitude +
                          gender + (1|subject) +
                          (1|scenario), data=data)
politeness.model


#To check the significance of the model
politeness.null = lmer(frequency ~ gender +
                         (1|subject) + (1|scenario), data=data,
                       REML=FALSE)
politeness.null

politeness.model = lmer(frequency ~ attitude +
                          gender +
                          (1|subject) + (1|scenario), data=data,
                        REML=FALSE)

anova(politeness.null, politeness.model)

# To get coefficients from model
coef(politeness.model)

# To add random slope
politeness.model = lmer(frequency ~ attitude +
                          gender + (1+attitude|subject) +
                          (1+attitude|scenario),
                        data=data,
                        REML=FALSE)
coef(politeness.model)

# To make null model and check signficance of model
politeness.null = lmer(frequency ~ gender +
                         (1+attitude|subject) + (1+attitude|scenario),
                       data=data, REML=FALSE)

anova(politeness.null, politeness.model)
