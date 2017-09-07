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
