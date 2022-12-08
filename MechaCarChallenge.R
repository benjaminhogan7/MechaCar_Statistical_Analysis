#1
library(dplyr)
?read.csv()
?lm()
# load csv file
lin_reg_table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
#linear regression model
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=lin_reg_table)

# Using the summary() function p-value and r-squared value for model
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=lin_reg_table))

#2

# Import suspension coil csv
sus_coil_table <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)
?summarize()

total_summary <- sus_coil_table %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups = 'keep')

# Create a lot summary using group_by() function
?group_by()
lot_summary <-  sus_coil_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups = 'keep')

#3
# Use the t.test() function to determine the PSI across all manufacturing lots.
t.test(sus_coil_table$PSI, mu=1500)

# three more t.test()s for each lot's psi

# Lot 1
lot1 <- sus_coil_table %>% subset(Manufacturing_Lot=="Lot1")
t.test(lot1$PSI, mu=1500)

# Lot 2
lot2 <- sus_coil_table %>% subset(Manufacturing_Lot=="Lot2")
t.test(lot2$PSI, mu=1500)

# Lot 3
lot3 <- sus_coil_table %>% subset(Manufacturing_Lot=="Lot3")
t.test(lot3$PSI, mu=1500)
