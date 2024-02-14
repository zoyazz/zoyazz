library(survival)
library(ggplot2)
data("cancer",package = 'survival')

summary(cancer)
table(cancer$ph.ecog)
table(cancer$sex)

#############creating a function for normalizing will be used later####################
standardize <- function(x) {
  mean_val <- mean(x, na.rm = TRUE)
  sd_val <- sd(x, na.rm = TRUE)
  
  if (sd_val == 0) {
    # If standard deviation is 0, return NA to avoid division by zero
    return(NA)
  } else {
    return((x - mean_val) / sd_val)
  }
}

#creating dataframe of cancer data treating ph.ecog as factor
attach(death0)
death0 = within(cancer,{
  status = status -1
  TimeDeath = Surv(cancer$time,status)
  sex = sex-1
  sex[sex == 0] = "male"
  sex[sex == 1] = "female"
  sex = factor(sex)
  contrasts(sex) = contr.treatment(2,2)
  }) 

death0$ph.ecog[death0$ph.ecog == 3] = NA #only one value removing it

ph.ecog = factor(ph.ecog)
table(death0$ph.ecog)
table(death0$sex)


head(death0)

#creating dataframe of cancer data treating ph.ecog as quantitative 
death1$ph.ecog[death1$ph.ecog == 3] = NA
death1 = within(cancer,{
  status = status -1
  TimeDeath = Surv(cancer$time,status)
  sex[sex == 1] = "male"
  sex[sex == 2] = "female"
  sex = factor(sex)
  contrasts(sex) = contr.treatment(2,2)}) #male reference




#################### using martingale residuals to determine power of variable ####################

mod0 <- coxph(death0$TimeDeath ~ 1)
mart0 <- residuals(mod0, type = 'martingale')
plot(sex,mart0,ylab = 'Martingale Residuals', col = "purple"); title('Gender') #plotting martingale residuals for gender

plot(age, mart0, ylab = 'Martingale Residuals'); title('Age')
lines(lowess(age,mart0)) #slight bend possible quadratic or cubic term

plot(ph.ecog,mart0, ylab = 'Martingale Residuals')

plot(pat.karno,mart0, ylab = 'Martingale Residuals')
lines(lowess(pat.karno[!is.na(pat.karno)],mart0[!is.na(pat.karno)])) # possible x^2

plot(meal.cal,mart0, ylab = 'Martingale Residuals'); title('Meal')
lines(lowess(meal.cal[!is.na(meal.cal)],mart0[!is.na(meal.cal)])) #linear

plot(wt.loss,mart0, ylab = 'Martingle Residuals'); title('Weight Loss')
lines(lowess((wt.loss[!is.na(wt.loss)]), mart0[!is.na(wt.loss)])) # will try till x^4. 



################################## creating model #######################################
age2 <- age^2; pat.karno2 <- pat.karno^2; wt.loss2 <- wt.loss^2; wt.loss3 <- wt.loss^3; wt.loss4 <- wt.loss^4;
fullCanc <- coxph(TimeDeath ~ age + age2 + pat.karno + pat.karno2 + meal.cal + wt.loss + wt.loss2 + wt.loss3 + wt.loss4 + 
        sex + ph.ecog)
summary(fullCanc)

#age not significant removing. wt.loss^3 and wt.loss^4 significant will keep. 
redCanc1 <- coxph(TimeDeath ~ pat.karno + pat.karno2 + meal.cal + wt.loss3 + wt.loss4 + 
                    sex + ph.ecog); summary(redCanc1)

# weight loss has large and varied values. Normalizing.
wt.loss4_norm <- standardize((wt.loss4)); wt.loss3_norm <- standardize((wt.loss3));
wt.loss4_norm

# removing meal.cal. do not need two type of physician recommendation so removing pat.karno
redCanc2 <- coxph(TimeDeath ~ wt.loss3_norm + wt.loss4_norm + sex + ph.ecog); summary(redCanc2)
redCanc3 <- coxph(TimeDeath ~ wt.loss3_norm + sex + ph.ecog); summary(redCanc3) #overfitting data
redCanc4 <- coxph(TimeDeath ~ wt.loss + sex + ph.ecog); summary(redCanc4) #removing weight loss
redCanc5 <- coxph(TimeDeath ~ sex + ph.ecog); summary(redCanc5)

###################### testing if ph.ecog should be used as quantitaive or qualitative. ###########
source("http://www.utstat.toronto.edu/~brunner/Rfunctions/Wtest.txt")
L1 <- cbind(0,2,-1)
thetahat <- redCanc5$coefficients
Vhat <- vcov(redCanc5)
Wtest(L=L1,Tn= thetahat, Vn = Vhat)

# p value is insiginficant can use as either quantitaive or qualitive. we do not have enough evidence
# to suggest that either option is better. Final model to use: y = sex + ph.ecog. 
# Through this project, we aim to contribute to the understanding of factors influencing 
# survival outcomes in cancer patients. By leveraging survival analysis techniques 
# and Cox proportional hazards modeling, we seek to develop robust predictive models 
# that can assist clinicians in prognosis and treatment decision-making. The insights 
# gained from this analysis may have implications for personalized medicine and patient care in oncology.
