
## Project:  Weather a R response 
# Located:   Class folder on ELSA
# File Name: FILL THIS OUT
# Date:      FILL THIS OUT
# Who:       FILL THIS OUT


####################################################################################
############              Pre-Analysis: settings, packages, and data    ############
####################################################################################


### Settings + Packages
library(dplyr)
library(psych)

#install.packages("dplyr")
#install.packages("psych")

### Load data 
GSS <- read.csv("GSS2022.csv")


####################################################################################
############              PHASE 1: CLEAN DATA FOR ANALYSIS              ############
####################################################################################


## Steps of cleaning variables Clear vars
# Step 1: Examine variable and coding schema: Table() / summary() 
# Step 2: Recode (if necessary/warrented): mutate(), ifelse(), etc
# Step 3: Confirm: table() / summary()



############                  DEPENDENT VARIABLE                ############
############             Working mothers and their bond with their child               ############

# STEP 1: Examine variable and coding schema 
table(GSS$fechld) 
# STEP 2: Recode if necessary or justify if not neccesary
GSS <- mutate(GSS, sa_strngbond = ifelse(fechld == 1, 1, 0))
GSS <- mutate(GSS, a_strngbond = ifelse(fechld == 2, 1, 0))
GSS <- mutate(GSS, da_strngbond = ifelse(fechld == 3, 1, 0))
GSS <- mutate(GSS, sda_strngbond = ifelse(fechld == 4, 1, 0))

              
# STEP 3: Confirm creation (if necessary)
table(GSS$fechld, GSS$sa_strngbond) 
table(GSS$fechld, GSS$a_strngbond) 
table(GSS$fechld, GSS$da_strngbond)
table(GSS$fechld, GSS$sda_strngbond)
      
############                  INDEPENDENT VARIABLE                    ############
############              Gender of respondents               ############

# STEP 1: Examine variable and coding schema 
table(GSS$sex)
# STEP 2: Recode if necessary or justify if not neccesary
GSS <- mutate(GSS, male = ifelse(sex == 1, 1, 0))
GSS <- mutate(GSS, female = ifelse(sex == 2, 1, 0))

# STEP 3: Confirm creation (if necessary)
table(GSS$sex, GSS$male) 
table(GSS$sex, GSS$female) 



####################################################################################
############              PHASE 2: CREATE MY DATASET                    ############
####################################################################################

### STEP 1: Create a list of variables to keep
my_varlist <- c("fechld","sa_strngbond","a_strngbond", "da_strngbond","sda_strngbond", 
                "sex", "male", "female" )


### STEP 2: create a new dataset with only your variables and complete case
my_dataset <- GSS %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

### STEP 3: Gather Summary Statistics and confirm valid dataset construction
describe(my_dataset)




####################################################################################
############              PHASE 3: Descriptive Statistics     ############
####################################################################################
# TABLE 1: DESCRIPTIVE STATISTICS HERE
describe(my_dataset)



####################################################################################
############              PHASE 4: Contingency Table + Chi2                  ############
####################################################################################


# TABLE 2: CONTINGENCY TABLE HERE
table(my_dataset$fechld)
table(my_dataset$sex)
table(my_dataset$fechld,my_dataset$sex)

