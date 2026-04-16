library(tidyverse)

#data read in
copter = read_csv(file="Experiment,Run,Length,Paper,Response
20,1,4 inches,Cardstock,0.87
10,2,3 inches,Cardstock,1.05
6,3,3 inches,Printer,0.99
31,4,5 inches,Cardstock,1.12
15,5,4 inches,Printer,1.71
24,6,4 inches,Cardstock,1.06
3,7,3 inches,Printer,0.99
19,8,4 inches,Cardstock,1.3
12,9,3 inches,Cardstock,0.75
29,10,5 inches,Printer,1.25
5,11,3 inches,Printer,1.51
21,12,4 inches,Cardstock,1.12
33,13,5 inches,Cardstock,1.32
11,14,3 inches,Cardstock,1.19
1,15,3 inches,Printer,1.32
27,16,5 inches,Printer,0.94
16,17,4 inches,Printer,1.52
35,18,5 inches,Cardstock,1.27
8,19,3 inches,Cardstock,1.06
25,20,5 inches,Printer,1
13,21,4 inches,Printer,0.99
30,22,5 inches,Printer,0.86
36,23,5 inches,Cardstock,2.03
4,24,3 inches,Printer,1.97
22,25,4 inches,Cardstock,1.06
17,26,4 inches,Printer,1.98
32,27,5 inches,Cardstock,1.38
7,28,3 inches,Cardstock,1.2
28,29,5 inches,Printer,1.06
14,30,4 inches,Printer,1.19
23,31,4 inches,Cardstock,1.44
2,32,3 inches,Printer,1.27
18,33,4 inches,Printer,1.83
34,34,5 inches,Cardstock,1.19
26,35,5 inches,Printer,2.04
9,36,3 inches,Cardstock,1.25
")

#power analysis
power.anova.test(
  groups = 6,          
  n = 6,               
  between.var = 0.16,  
  within.var = 1.0,    
  sig.level = 0.05
)

#power analysis
library(pwr4exp)
crd2 <- designCRD(
 treatments = c(2,3),
 label=list(Paper=c("Cardstock","Printer"),Length=c("3 inches", "4 inches", "5 inches")),
 replicates = 60, #number for each trt, here 36
 means = c(1.08,1.34,1.14,1.54,1.38,1.19), #see below
 sigma2 = 0.1
) 

pwr.anova(crd2)


glimpse(copter)
#assigning factors
copter = copter |>
  mutate(Length=factor(Length, levels=c("3 inches", "4 inches", "5 inches")),
         Paper=factor(Paper, levels=c("Cardstock", "Printer")))

#anova model
model = aov(Response~Length+Paper+Length*Paper,data=copter)
anova(model)

#Check for assumptions
resids = resid(model)

#check for independence with an index plot
plot(resids, type="p")
abline(h=0)

#Check normality with qq plot
qqnorm(resids)
qqline(resids)
shapiro.test(resids)
hist(resids)
hist(log(resids))

#check for constant variance
card3 = filter(copter, Paper == 'Cardstock', Length =='3 inches')
card4 = filter(copter, Paper == 'Cardstock', Length =='4 inches')
card5 = filter(copter, Paper == 'Cardstock', Length =='5 inches')
print3 = filter(copter, Paper == 'Printer', Length =='3 inches')
print4 = filter(copter, Paper == 'Printer', Length =='4 inches')
print5 = filter(copter, Paper == 'Printer', Length =='5 inches')

sd(card3$Response)
sd(card4$Response)
sd(card5$Response)
sd(print3$Response)
sd(print4$Response)
sd(print5$Response)

ratio = sd(print5$Response)/sd(card3$Response)
ratio

# We need a mean of 0
mean(resids)

#interaction plot
interaction.plot(x.factor = copter$Length,
                 trace.factor = copter$Paper, response = copter$Response)

#pairwise comparisons
TukeyHSD(model)

#Means and Standard Deviations
means <- copter |> 
        group_by(Paper, Length) |> 
        summarize(mean = mean(Response), sd = sd(Response), n = n())
means

mean(c(0.182, 0.201, 0.329, 0.368, 0.383, 0.436))^2





