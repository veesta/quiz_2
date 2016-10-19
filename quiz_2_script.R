library(tidyverse)
library(apaTables)


bfi_data <- psych::bfi

#View(bfi_data)


str(bfi_data)

#create categorical variables for gender, education 

bfi_data$gender <- as.factor(bfi_data$gender)
levels(bfi_data$gender) <- list("Male"=1, "Female"=2)


#group the adjectives under their titles and then check for out of range values
#select raw data for ALL VARIABLES 

gender <- select(bfi_data, gender)
education <- select(bfi_data, education)
age <- select(bfi_data, age)

agreeable_items <- select(bfi_data, A1, A2, A3, A4, A5)
neuroticism_items <- select(bfi_data, N1, N2, N3, N4, N5)
extraversion_items <- select(bfi_data, E1, E2, E3, E4, E5) 


agreeable_items <- mutate(agreeable_items,A1=7-A1)
extraversion_items <- mutate(extraversion_items,E1=7-E1)
extraversion_items <- mutate(extraversion_items,E2=7-E2)


#obtain a scaled score - calc mean for each person based on items 
#this is where you relable to "this data file should have the following final variable names" 


## To obtain scale scores:

agreeableness <- psych::alpha(as.data.frame(agreeable_items),check.keys=FALSE)$scores
neuroticism <- psych::alpha(as.data.frame(neuroticism_items),check.keys=FALSE)$scores
extraversion <- psych::alpha(as.data.frame(extraversion_items),check.keys=FALSE)$scores

analytic_data <- cbind(age,education,gender,neuroticism,extraversion,agreeableness)

# View(analytic_data)

write_csv(analytic_data,path="analytic_data.csv")

str(analytic_data)

#View(analytic_data)


#load apa tables
library(apaTables)

## Create 3 correlation tables

analytic_data_nosex <- select(analytic_data, -gender)

apa.cor.table(analytic_data_nosex,filename="Table1.doc",table.number=1)


#View(analytic_data_nosex)


analytic_data_over40 <- filter(analytic_data, gender=="Male") 
analytic_data_over40 <- filter(analytic_data, age>40)

apa.cor.table(analytic_data_over40,filename="Table2.doc",table.number=2)

## Create NA-Neur Scatter Plot
my.scatter <- qplot(x=agreeableness,y=extraversion,data=analytic_data_over40)

my.scatter <- my.scatter + labs(x="Agreeableness",y="Extraversion")
my.scatter <- my.scatter + theme_classic()
my.scatter <- my.scatter + theme(axis.line.x = element_line(colour = 'black',size=0.5, linetype='solid'),
                                 axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
my.scatter <- my.scatter + coord_cartesian(xlim=c(1,6), ylim=c(1,6))

print(my.scatter)
ggsave(filename="Figure1.pdf", plot=my.scatter, width=6,height=6, units="in")

cor.test(x=analytic_data_over40$agreeableness,y=analytic_data_over40$extraversion)
