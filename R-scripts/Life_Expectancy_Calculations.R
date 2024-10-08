##### Uploafd packages ####
source('R-scripts/upload_packages.R')
#### Uploafd functions ####
source('R-scripts/functions.R')
#### Upload life tables functions  from Stelter & Alburez-Gutierrez (2022) ####
source("MortalitySmooth/LifeTableFUN_II.R") 
#### Upload functions from MortalitySmooth package from Camarda (2013) ####
source("MortalitySmooth/cleversearch.R") # 
source("MortalitySmooth/Mort2Dsmooth.R") 
source("MortalitySmooth/Mort2Dsmooth_checker.R") 
source("MortalitySmooth/Mort2Dsmooth_estimate.R") 
source("MortalitySmooth/Mort2Dsmooth_optimize.R") 
source("MortalitySmooth/Mort2Dsmooth_se.R") 
source("MortalitySmooth/Mort2Dsmooth_update.R") 
source("MortalitySmooth/print.Mort2Dsmooth.R") 
source("MortalitySmooth/print.summary.Mort2Dsmooth.R") 
source("MortalitySmooth/residuals.Mort2Dsmooth.R") 
source("MortalitySmooth/summary.Mort2Dsmooth.R") 
source("MortalitySmooth/MortSmooth_bbase.R") 
source("MortalitySmooth/MortSmooth_tpower.R") 
source("MortalitySmooth/MortSmooth_BcoefB.R") 
source("MortalitySmooth/MortSmooth_BWB.R") 

#### Genealogical data ####

load('Cleaned_Datasets/data_focal.RData')

data_mortality = data_focal %>%
  filter(!(is.na(country_birth_final)),
         !(is.na(country_death_final)),
         !(is.na(birth_year)),
         !(is.na(death_year)),
         !(is.na(age_death))) %>%
  mutate(completeness_level=case_when(!(is.na(birth_month)) & !(is.na(death_month)) ~"high",
                                      TRUE~"low"),
         completeness_level = factor(completeness_level,levels=c("low","high")))

data_mortality_low = filter(data_mortality,completeness_level=="low")

data_mortality_high = filter(data_mortality,completeness_level=="high")


pop_deaths_low = deaths_calculation(data_mortality_low,"SWEDEN",1751,1900)  %>%
  filter(!is.na(age_class)) %>%
  mutate(age=case_when(age_class %in% c("0","1-4","5-9") ~ as.numeric(substr(age_class,1,1)),
                       age_class %in% "80+" ~ 80,
                       TRUE ~ as.numeric(substr(age_class,1,2))))


pop_deaths_high = deaths_calculation(data_mortality_high,"SWEDEN",1751,1900)  %>%
  filter(!is.na(age_class)) %>%
  mutate(age=case_when(age_class %in% c("0","1-4","5-9") ~ as.numeric(substr(age_class,1,1)),
                       age_class %in% "80+" ~ 80,
                       TRUE ~ as.numeric(substr(age_class,1,2))))

pop_counts_low = pop_pyramid_calculation(data_mortality_low,"SWEDEN",1750,1900)

pop_exposure_low = pop_exposure(pop_counts_low) %>% filter(years>=1751) %>%
  mutate(age=case_when(age_class %in% c("0","1-4","5-9") ~ as.numeric(substr(age_class,1,1)),
                       age_class %in% "80+" ~ 80,
                       TRUE ~ as.numeric(substr(age_class,1,2))))



pop_counts_high = pop_pyramid_calculation(data_mortality_high,"SWEDEN",1750,1900)


pop_exposure_high = pop_exposure(pop_counts_high) %>% filter(years>=1751) %>%
  mutate(age=case_when(age_class %in% c("0","1-4","5-9") ~ as.numeric(substr(age_class,1,1)),
                       age_class %in% "80+" ~ 80,
                       TRUE ~ as.numeric(substr(age_class,1,2))))


#### Life Expectancy at birth ####

#### Total ####

# death counts low quality 

D_low_total = pop_deaths_low %>%
  filter(gender=="total") %>%
  ungroup() %>%
  select(years,age,deaths) %>%
  pivot_wider(names_from = "years",values_from = "deaths") %>%
  select(-age) %>%
  as.matrix()

D_low_total[is.na(D_low_total)] = 0


D_low_total_30 = pop_deaths_low %>%
  filter(gender=="total",age>=30) %>%
  ungroup() %>%
  select(years,age,deaths) %>%
  pivot_wider(names_from = "years",values_from = "deaths") %>%
  select(-age) %>%
  as.matrix()

D_low_total_30[is.na(D_low_total_30)] = 0

# death counts high quality

D_high_total = pop_deaths_high %>%
  filter(gender=="total") %>%
  ungroup() %>%
  select(years,age,deaths) %>%
  pivot_wider(names_from = "years",values_from = "deaths") %>%
  select(-age) %>%
  as.matrix()


D_high_total_30 = pop_deaths_high %>%
  filter(gender=="total",age>=30) %>%
  ungroup() %>%
  select(years,age,deaths) %>%
  pivot_wider(names_from = "years",values_from = "deaths") %>%
  select(-age) %>%
  as.matrix()

# population counts low quality


E_low_total = pop_exposure_low %>%
  filter(gender=="total") %>%
  ungroup() %>%
  select(years,age,expos) %>%
  pivot_wider(names_from = "years",values_from = "expos") %>%
  select(-age) %>%
  as.matrix()



E_low_total_30 = pop_exposure_low %>%
  filter(gender=="total",age>=30) %>%
  ungroup() %>%
  select(years,age,expos) %>%
  pivot_wider(names_from = "years",values_from = "expos") %>%
  select(-age) %>%
  as.matrix()


# population counts high quality


E_high_total = pop_exposure_high %>%
  filter(gender=="total") %>%
  ungroup() %>%
  select(years,age,expos) %>%
  pivot_wider(names_from = "years",values_from = "expos") %>%
  select(-age) %>%
  as.matrix()

E_high_total_30 = pop_exposure_high %>%
  filter(gender=="total",age>=30) %>%
  ungroup() %>%
  select(years,age,expos) %>%
  pivot_wider(names_from = "years",values_from = "expos") %>%
  select(-age) %>%
  as.matrix()



#### Total Pop + Low Quality : Life Exp at Birth ####

t = 1751:1900
Age5 = c(0,1,seq(5,80,5))
fit_low_total <- Mort2Dsmooth(x=1:length(Age5), y=t, Z=D_low_total, offset = log(E_low_total))
pred_mort_low_total <- exp(fit_low_total$logmortality)
expected_deaths_low_total <- pred_mort_low_total*E_low_total
expected_deathrates_low_total <- expected_deaths_low_total/E_low_total

lex_low_female
x <- c("t", "exs", "exms", "CIexlows", "CIexhighs")
lex_low_total <- matrix(nrow = length(t), ncol = length(x))
colnames(lex_low_total) <- x    
lex_low_total <- as.data.frame(lex_low_total)
lex_low_total$t <- t


# Register parallel backend

num_cores = 4
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Function to compute CIex for a given j
compute_CIex <- function(j) {
  data1 <- as.data.frame(Age5)
  data1$N <- E_low_total[1:length(Age5),j]
  data1$Ds <- expected_deaths_low_total[1:length(Age5),j]
  data2 <- subset(data1, data1$N>0)
  
  CIdata <- CIex(x=data2$Age5, Nx=data2$N, Dx=data2$Ds, which.x = 0, ns=1000, level=0.95)
  
  return(list(
    exs = CIdata$ex,
    exms = CIdata$mean_ex,
    CIexlows = CIdata$CI_ex[1],
    CIexhighs = CIdata$CI_ex[2]
  ))
}

# Perform parallel computation
results <- foreach(j = 1:length(t), .combine = rbind) %dopar% {
  compute_CIex(j)
}

# Stop the parallel backend
stopCluster(cl)
lex_low_total$exs <- results[, "exs"]
lex_low_total$exms <- results[, "exms"]
lex_low_total$CIexlows <- results[, "CIexlows"]
lex_low_total$CIexhighs <- results[, "CIexhighs"]





#### Total Pop + Low Quality : Life Exp at Age 30 ####

x <- c("t", "exs", "exms", "CIexlows", "CIexhighs")
Age5 <- seq(30,80,5)
t <- 1751:1900
lex_low_total_30 <- matrix(nrow = length(t), ncol = length(x))
colnames(lex_low_total_30) <- x    
lex_low_total_30 <- as.data.frame(lex_low_total_30)
lex_low_total_30$t <- t
fit_low_total_30 <- Mort2Dsmooth(x=1:length(Age5), y=t, Z=D_low_total_30, offset = log(E_low_total_30))
pred_mort_low_total_30 <- exp(fit_low_total_30$logmortality)
expected_deaths_low_total_30 <- pred_mort_low_total_30*E_low_total_30
expected_deathrates_low_total_30 <- expected_deaths_low_total_30/E_low_total_30

for (j in 1:length(t)) {
  data1 <- as.data.frame(Age5)
  data1$N <- E_low_total_30[1:length(Age5),j]
  data1$Ds <- expected_deaths_low_total_30[1:length(Age5),j]
  data2 <- subset(data1, data1$N>0)
  
  CIdata <- CIex(x=data2$Age5, Nx=data2$N, Dx=data2$Ds, which.x = 30, ns=1000, level=0.95)
  lex_low_total_30$exs <- ifelse(lex_low_total_30$t==t[j], CIdata$ex, lex_low_total_30$exs) 
  lex_low_total_30$exms <- ifelse(lex_low_total_30$t==t[j], CIdata$mean_ex, lex_low_total_30$exms) 
  lex_low_total_30$CIexlows <- ifelse(lex_low_total_30$t==t[j],CIdata$CI_ex[1], lex_low_total_30$CIexlows) 
  lex_low_total_30$CIexhighs <- ifelse(lex_low_total_30$t==t[j],CIdata$CI_ex[2], lex_low_total_30$CIexhighs) 
  
}   


# Register parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Function to compute CIex for a given j
compute_CIex <- function(j) {
  data1 <- as.data.frame(Age5)
  data1$N <- E_low_total_30[1:length(Age5),j]
  data1$Ds <- expected_deaths_low_total_30[1:length(Age5),j]
  data2 <- subset(data1, data1$N>0)
  
  CIdata <- CIex(x=data2$Age5, Nx=data2$N, Dx=data2$Ds, which.x = 30, ns=1000, level=0.95)
  
  return(list(
    exs = CIdata$ex,
    exms = CIdata$mean_ex,
    CIexlows = CIdata$CI_ex[1],
    CIexhighs = CIdata$CI_ex[2]
  ))
}

# Perform parallel computation
results <- foreach(j = 1:length(t), .combine = rbind) %dopar% {
  compute_CIex(j)
}

# Stop the parallel backend
stopCluster(cl)
lex_low_total_30$exs <- results[, "exs"]
lex_low_total_30$exms <- results[, "exms"]
lex_low_total_30$CIexlows <- results[, "CIexlows"]
lex_low_total_30$CIexhighs <- results[, "CIexhighs"]





#### Total Pop + High Quality : Life Exp at Birth ####
t <- 1751:1900
Age5 <- c(0,1,seq(5,80,5))
fit_high_total <- Mort2Dsmooth(x=1:length(Age5), y=t, Z=D_high_total, offset = log(E_high_total))
pred_mort_high_total <- exp(fit_high_total$logmortality)
expected_deaths_high_total <- pred_mort_high_total*E_high_total
expected_deathrates_high_total <- expected_deaths_high_total/E_high_total


x <- c("t", "exs", "exms", "CIexlows", "CIexhighs")
lex_high_total <- matrix(nrow = length(t), ncol = length(x))
colnames(lex_high_total) <- x    
lex_high_total <- as.data.frame(lex_high_total)
lex_high_total$t <- t


# Register parallel backend
cl <- makeCluster(num_cores)

registerDoParallel(cl)

# Function to compute CIex for a given j
compute_CIex <- function(j) {
  data1 <- as.data.frame(Age5)
  data1$N <- E_high_total[1:length(Age5),j]
  data1$Ds <- expected_deaths_high_total[1:length(Age5),j]
  data2 <- subset(data1, data1$N>0)
  
  CIdata <- CIex(x=data2$Age5, Nx=data2$N, Dx=data2$Ds, which.x = 0, ns=1000, level=0.95)
  
  return(list(
    exs = CIdata$ex,
    exms = CIdata$mean_ex,
    CIexlows = CIdata$CI_ex[1],
    CIexhighs = CIdata$CI_ex[2]
  ))
}

# Perform parallel computation
results <- foreach(j = 1:length(t), .combine = rbind) %dopar% {
  compute_CIex(j)
}

# Stop the parallel backend
stopCluster(cl)
lex_high_total$exs <- results[, "exs"]
lex_high_total$exms <- results[, "exms"]
lex_high_total$CIexlows <- results[, "CIexlows"]
lex_high_total$CIexhighs <- results[, "CIexhighs"]


#### Total Pop + High Quality : Life Exp at Age 30 ####

x <- c("t", "exs", "exms", "CIexlows", "CIexhighs")
Age5 <- seq(30,80,5)
t <- 1751:1900
lex_high_total_30 <- matrix(nrow = length(t), ncol = length(x))
colnames(lex_high_total_30) <- x    
lex_high_total_30 <- as.data.frame(lex_high_total_30)
lex_high_total_30$t <- t
fit_high_total_30 <- Mort2Dsmooth(x=1:length(Age5), y=t, Z=D_high_total_30, offset = log(E_high_total_30))
pred_mort_high_total_30 <- exp(fit_high_total_30$logmortality)
expected_deaths_high_total_30 <- pred_mort_high_total_30*E_high_total_30
expected_deathrates_high_total_30 <- expected_deaths_high_total_30/E_high_total_30



# Register parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Function to compute CIex for a given j
compute_CIex <- function(j) {
  data1 <- as.data.frame(Age5)
  data1$N <- E_high_total_30[1:length(Age5),j]
  data1$Ds <- expected_deaths_high_total_30[1:length(Age5),j]
  data2 <- subset(data1, data1$N>0)
  
  CIdata <- CIex(x=data2$Age5, Nx=data2$N, Dx=data2$Ds, which.x = 30, ns=1000, level=0.95)
  
  return(list(
    exs = CIdata$ex,
    exms = CIdata$mean_ex,
    CIexlows = CIdata$CI_ex[1],
    CIexhighs = CIdata$CI_ex[2]
  ))
}

# Perform parallel computation
results <- foreach(j = 1:length(t), .combine = rbind) %dopar% {
  compute_CIex(j)
}

# Stop the parallel backend
stopCluster(cl)

lex_high_total_30$exs <- results[, "exs"]
lex_high_total_30$exms <- results[, "exms"]
lex_high_total_30$CIexlows <- results[, "CIexlows"]
lex_high_total_30$CIexhighs <- results[, "CIexhighs"]




#### Male ####

# death counts low quality 

D_low_male = pop_deaths_low %>%
  filter(gender=="male") %>%
  ungroup() %>%
  select(years,age,deaths) %>%
  pivot_wider(names_from = "years",values_from = "deaths") %>%
  select(-age) %>%
  as.matrix()

D_low_male[is.na(D_low_male)] = 0


D_low_male_30 = pop_deaths_low %>%
  filter(gender=="male",age>=30) %>%
  ungroup() %>%
  select(years,age,deaths) %>%
  pivot_wider(names_from = "years",values_from = "deaths") %>%
  select(-age) %>%
  as.matrix()

D_low_male_30[is.na(D_low_male_30)] = 0


# death counts high quality

D_high_male = pop_deaths_high %>%
  filter(gender=="male") %>%
  ungroup() %>%
  select(years,age,deaths) %>%
  pivot_wider(names_from = "years",values_from = "deaths") %>%
  select(-age) %>%
  as.matrix()



D_high_male_30 = pop_deaths_high %>%
  filter(gender=="male",age>=30) %>%
  ungroup() %>%
  select(years,age,deaths) %>%
  pivot_wider(names_from = "years",values_from = "deaths") %>%
  select(-age) %>%
  as.matrix()

# population counts low quality


E_low_male = pop_exposure_low %>%
  filter(gender=="male") %>%
  ungroup() %>%
  select(years,age,expos) %>%
  pivot_wider(names_from = "years",values_from = "expos") %>%
  select(-age) %>%
  as.matrix()



E_low_male_30 = pop_exposure_low %>%
  filter(gender=="male",age>=30) %>%
  ungroup() %>%
  select(years,age,expos) %>%
  pivot_wider(names_from = "years",values_from = "expos") %>%
  select(-age) %>%
  as.matrix()

# population counts high quality


E_high_male = pop_exposure_high %>%
  filter(gender=="male") %>%
  ungroup() %>%
  select(years,age,expos) %>%
  pivot_wider(names_from = "years",values_from = "expos") %>%
  select(-age) %>%
  as.matrix()


E_high_male_30 = pop_exposure_high %>%
  filter(gender=="male",age>=30) %>%
  ungroup() %>%
  select(years,age,expos) %>%
  pivot_wider(names_from = "years",values_from = "expos") %>%
  select(-age) %>%
  as.matrix()



#### Male Pop + Low Quality : Life Exp at Birth ####
t = 1751:1900
Age5 = c(0,1,seq(5,80,5))
fit_low_male <- Mort2Dsmooth(x=1:length(Age5), y=t, Z=D_low_male, offset = log(E_low_male))
pred_mort_low_male <- exp(fit_low_male$logmortality)
expected_deaths_low_male <- pred_mort_low_male*E_low_male
expected_deathrates_low_male <- expected_deaths_low_male/E_low_male


x <- c("t", "exs", "exms", "CIexlows", "CIexhighs")
lex_low_male <- matrix(nrow = length(t), ncol = length(x))
colnames(lex_low_male) <- x    
lex_low_male <- as.data.frame(lex_low_male)
lex_low_male$t <- t


# Set the number of cores to be used (adjust the number as per your system)
num_cores <- 4

# Register parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Function to compute CIex for a given j
compute_CIex <- function(j) {
  data1 <- as.data.frame(Age5)
  data1$N <- E_low_male[1:length(Age5), j]
  data1$Ds <- expected_deaths_low_male[1:length(Age5), j]
  data2 <- subset(data1, data1$N > 0)
  
  CIdata <- CIex(x = data2$Age5, Nx = data2$N, Dx = data2$Ds, which.x = 0, ns = 1000, level = 0.95)
  
  return(list(
    exs = CIdata$ex,
    exms = CIdata$mean_ex,
    CIexlows = CIdata$CI_ex[1],
    CIexhighs = CIdata$CI_ex[2]
  ))
}

# Perform parallel computation
results <- foreach(j = 1:length(t), .combine = rbind) %dopar% {
  compute_CIex(j)
}

# Stop the parallel backend
stopCluster(cl)
lex_low_male$exs <- results[, "exs"]
lex_low_male$exms <- results[, "exms"]
lex_low_male$CIexlows <- results[, "CIexlows"]
lex_low_male$CIexhighs <- results[, "CIexhighs"]




#### Male Pop + Low Quality : Life Exp at Age 30 ####

x <- c("t", "exs", "exms", "CIexlows", "CIexhighs")
Age5 <- seq(30,80,5)
t <- 1751:1900
lex_low_male_30 <- matrix(nrow = length(t), ncol = length(x))
colnames(lex_low_male_30) <- x    
lex_low_male_30 <- as.data.frame(lex_low_male_30)
lex_low_male_30$t <- t
fit_low_male_30 <- Mort2Dsmooth(x=1:length(Age5), y=t, Z=D_low_male_30, offset = log(E_low_male_30))
pred_mort_low_male_30 <- exp(fit_low_male_30$logmortality)
expected_deaths_low_male_30 <- pred_mort_low_male_30*E_low_male_30
expected_deathrates_low_male_30 <- expected_deaths_low_male_30/E_low_male_30

# Register parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Function to compute CIex for a given j
compute_CIex <- function(j) {
  data1 <- as.data.frame(Age5)
  data1$N <- E_low_male_30[1:length(Age5), j]
  data1$Ds <- expected_deaths_low_male_30[1:length(Age5), j]
  data2 <- subset(data1, data1$N > 0)
  
  CIdata <- CIex(x = data2$Age5, Nx = data2$N, Dx = data2$Ds, which.x = 30, ns = 1000, level = 0.95)
  
  return(list(
    exs = CIdata$ex,
    exms = CIdata$mean_ex,
    CIexlows = CIdata$CI_ex[1],
    CIexhighs = CIdata$CI_ex[2]
  ))
}

# Perform parallel computation
results <- foreach(j = 1:length(t), .combine = rbind) %dopar% {
  compute_CIex(j)
}

# Stop the parallel backend
stopCluster(cl)
lex_low_male_30$exs <- results[, "exs"]
lex_low_male_30$exms <- results[, "exms"]
lex_low_male_30$CIexlows <- results[, "CIexlows"]
lex_low_male_30$CIexhighs <- results[, "CIexhighs"]




#### Male Pop + High Quality : Life Exp at Birth ####

t <- 1751:1900
Age5 <- c(0,1,seq(5,80,5))
fit_high_male <- Mort2Dsmooth(x=1:length(Age5), y=t, Z=D_high_male, offset = log(E_high_male))
pred_mort_high_male <- exp(fit_high_male$logmortality)
expected_deaths_high_male <- pred_mort_high_male*E_high_male
expected_deathrates_high_male <- expected_deaths_high_male/E_high_male



x <- c("t", "exs", "exms", "CIexlows", "CIexhighs")
lex_high_male <- matrix(nrow = length(t), ncol = length(x))
colnames(lex_high_male) <- x    
lex_high_male <- as.data.frame(lex_high_male)
lex_high_male$t <- t

# Set the number of cores to be used (adjust the number as per your system)
num_cores <- 4

# Register parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Function to compute CIex for a given j
compute_CIex <- function(j) {
  data1 <- as.data.frame(Age5)
  data1$N <- E_high_male[1:length(Age5), j]
  data1$Ds <- expected_deaths_high_male[1:length(Age5), j]
  data2 <- subset(data1, data1$N > 0)
  
  CIdata <- CIex(x = data2$Age5, Nx = data2$N, Dx = data2$Ds, which.x = 0, ns = 1000, level = 0.95)
  
  return(list(
    exs = CIdata$ex,
    exms = CIdata$mean_ex,
    CIexlows = CIdata$CI_ex[1],
    CIexhighs = CIdata$CI_ex[2]
  ))
}

# Perform parallel computation
results <- foreach(j = 1:length(t), .combine = rbind) %dopar% {
  compute_CIex(j)
}

# Stop the parallel backend
stopCluster(cl)

lex_high_male$exs <- results[, "exs"]
lex_high_male$exms <- results[, "exms"]
lex_high_male$CIexlows <- results[, "CIexlows"]
lex_high_male$CIexhighs <- results[, "CIexhighs"]



#### Male Pop + High Quality : Life Exp at Age 30 ####

x <- c("t", "exs", "exms", "CIexlows", "CIexhighs")
Age5 <- seq(30,80,5)
t <- 1751:1900
lex_high_male_30 <- matrix(nrow = length(t), ncol = length(x))
colnames(lex_high_male_30) <- x    
lex_high_male_30 <- as.data.frame(lex_high_male_30)
lex_high_male_30$t <- t
fit_high_male_30 <- Mort2Dsmooth(x=1:length(Age5), y=t, Z=D_high_male_30, offset = log(E_high_male_30))
pred_mort_high_male_30 <- exp(fit_high_male_30$logmortality)
expected_deaths_high_male_30 <- pred_mort_high_male_30*E_high_male_30
expected_deathrates_high_male_30 <- expected_deaths_high_male_30/E_high_male_30


# Set the number of cores to be used (adjust the number as per your system)
num_cores <- 4

# Register parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Function to compute CIex for a given j
compute_CIex <- function(j) {
  data1 <- as.data.frame(Age5)
  data1$N <- E_high_male_30[1:length(Age5), j]
  data1$Ds <- expected_deaths_high_male_30[1:length(Age5), j]
  data2 <- subset(data1, data1$N > 0)
  
  CIdata <- CIex(x = data2$Age5, Nx = data2$N, Dx = data2$Ds, which.x = 30, ns = 1000, level = 0.95)
  
  return(list(
    exs = CIdata$ex,
    exms = CIdata$mean_ex,
    CIexlows = CIdata$CI_ex[1],
    CIexhighs = CIdata$CI_ex[2]
  ))
}

# Perform parallel computation
results <- foreach(j = 1:length(t), .combine = rbind) %dopar% {
  compute_CIex(j)
}

# Stop the parallel backend
stopCluster(cl)
lex_high_male_30$exs <- results[, "exs"]
lex_high_male_30$exms <- results[, "exms"]
lex_high_male_30$CIexlows <- results[, "CIexlows"]
lex_high_male_30$CIexhighs <- results[, "CIexhighs"]


#### Female ####

# death counts low quality 

D_low_female = pop_deaths_low %>%
  filter(gender=="female") %>%
  ungroup() %>%
  select(years,age,deaths) %>%
  pivot_wider(names_from = "years",values_from = "deaths") %>%
  select(-age) %>%
  as.matrix()

D_low_female[is.na(D_low_female)] = 0


D_low_female_30 = pop_deaths_low %>%
  filter(gender=="female",age>=30) %>%
  ungroup() %>%
  select(years,age,deaths) %>%
  pivot_wider(names_from = "years",values_from = "deaths") %>%
  select(-age) %>%
  as.matrix()

D_low_female_30[is.na(D_low_female_30)] = 0


# death counts high quality

D_high_female = pop_deaths_high %>%
  filter(gender=="female") %>%
  ungroup() %>%
  select(years,age,deaths) %>%
  pivot_wider(names_from = "years",values_from = "deaths") %>%
  select(-age) %>%
  as.matrix()


D_high_female_30 = pop_deaths_high %>%
  filter(gender=="female",age>=30) %>%
  ungroup() %>%
  select(years,age,deaths) %>%
  pivot_wider(names_from = "years",values_from = "deaths") %>%
  select(-age) %>%
  as.matrix()

# population counts low quality


E_low_female = pop_exposure_low %>%
  filter(gender=="female") %>%
  ungroup() %>%
  select(years,age,expos) %>%
  pivot_wider(names_from = "years",values_from = "expos") %>%
  select(-age) %>%
  as.matrix()



E_low_female_30 = pop_exposure_low %>%
  filter(gender=="female",age>=30) %>%
  ungroup() %>%
  select(years,age,expos) %>%
  pivot_wider(names_from = "years",values_from = "expos") %>%
  select(-age) %>%
  as.matrix()

# population counts high quality


E_high_female = pop_exposure_high %>%
  filter(gender=="female") %>%
  ungroup() %>%
  select(years,age,expos) %>%
  pivot_wider(names_from = "years",values_from = "expos") %>%
  select(-age) %>%
  as.matrix()

E_high_female_30 = pop_exposure_high %>%
  filter(gender=="female",age>=30) %>%
  ungroup() %>%
  select(years,age,expos) %>%
  pivot_wider(names_from = "years",values_from = "expos") %>%
  select(-age) %>%
  as.matrix()



#### female Pop + Low Quality : Life Exp at Birth ####
t = 1751:1900
Age5 = c(0,1,seq(5,80,5))
fit_low_female <- Mort2Dsmooth(x=1:length(Age5), y=t, Z=D_low_female, offset = log(E_low_female))
pred_mort_low_female <- exp(fit_low_female$logmortality)
expected_deaths_low_female <- pred_mort_low_female*E_low_female
expected_deathrates_low_female <- expected_deaths_low_female/E_low_female


x <- c("t", "exs", "exms", "CIexlows", "CIexhighs")
lex_low_female <- matrix(nrow = length(t), ncol = length(x))
colnames(lex_low_female) <- x    
lex_low_female <- as.data.frame(lex_low_female)
lex_low_female$t <- t



# Register parallel backend

cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Function to compute CIex for a given j
compute_CIex <- function(j) {
  data1 <- as.data.frame(Age5)
  data1$N <- E_low_female[1:length(Age5),j]
  data1$Ds <- expected_deaths_low_female[1:length(Age5),j]
  data2 <- subset(data1, data1$N>0)
  
  CIdata <- CIex(x=data2$Age5, Nx=data2$N, Dx=data2$Ds, which.x = 0, ns=1000, level=0.95)
  
  return(list(
    exs = CIdata$ex,
    exms = CIdata$mean_ex,
    CIexlows = CIdata$CI_ex[1],
    CIexhighs = CIdata$CI_ex[2]
  ))
}

# Perform parallel computation
results <- foreach(j = 1:length(t), .combine = rbind) %dopar% {
  compute_CIex(j)
}

# Stop the parallel backend
stopCluster(cl)
lex_low_female$exs <- results[, "exs"]
lex_low_female$exms <- results[, "exms"]
lex_low_female$CIexlows <- results[, "CIexlows"]
lex_low_female$CIexhighs <- results[, "CIexhighs"]





#### female Pop + Low Quality : Life Exp at Age 30 ####

x <- c("t", "exs", "exms", "CIexlows", "CIexhighs")
Age5 <- seq(30,80,5)
t <- 1751:1900
lex_low_female_30 <- matrix(nrow = length(t), ncol = length(x))
colnames(lex_low_female_30) <- x    
lex_low_female_30 <- as.data.frame(lex_low_female_30)
lex_low_female_30$t <- t
fit_low_female_30 <- Mort2Dsmooth(x=1:length(Age5), y=t, Z=D_low_female_30, offset = log(E_low_female_30))
pred_mort_low_female_30 <- exp(fit_low_female_30$logmortality)
expected_deaths_low_female_30 <- pred_mort_low_female_30*E_low_female_30
expected_deathrates_low_female_30 <- expected_deaths_low_female_30/E_low_female_30

# Register parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Function to compute CIex for a given j
compute_CIex <- function(j) {
  data1 <- as.data.frame(Age5)
  data1$N <- E_low_female_30[1:length(Age5),j]
  data1$Ds <- expected_deaths_low_female_30[1:length(Age5),j]
  data2 <- subset(data1, data1$N>0)
  
  CIdata <- CIex(x=data2$Age5, Nx=data2$N, Dx=data2$Ds, which.x = 30, ns=1000, level=0.95)
  
  return(list(
    exs = CIdata$ex,
    exms = CIdata$mean_ex,
    CIexlows = CIdata$CI_ex[1],
    CIexhighs = CIdata$CI_ex[2]
  ))
}

# Perform parallel computation
results <- foreach(j = 1:length(t), .combine = rbind) %dopar% {
  compute_CIex(j)
}

# Stop the parallel backend
stopCluster(cl)
lex_low_female_30$exs <- results[, "exs"]
lex_low_female_30$exms <- results[, "exms"]
lex_low_female_30$CIexlows <- results[, "CIexlows"]
lex_low_female_30$CIexhighs <- results[, "CIexhighs"]


#### female Pop + High Quality : Life Exp at Birth ####

t <- 1751:1900
Age5 <- c(0,1,seq(5,80,5))
fit_high_female <- Mort2Dsmooth(x=1:length(Age5), y=t, Z=D_high_female, offset = log(E_high_female))
pred_mort_high_female <- exp(fit_high_female$logmortality)
expected_deaths_high_female <- pred_mort_high_female*E_high_female
expected_deathrates_high_female <- expected_deaths_high_female/E_high_female


x <- c("t", "exs", "exms", "CIexlows", "CIexhighs")
lex_high_female <- matrix(nrow = length(t), ncol = length(x))
colnames(lex_high_female) <- x    
lex_high_female <- as.data.frame(lex_high_female)
lex_high_female$t <- t


# Register parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Function to compute CIex for a given j
compute_CIex <- function(j) {
  data1 <- as.data.frame(Age5)
  data1$N <- E_high_female[1:length(Age5),j]
  data1$Ds <- expected_deaths_high_female[1:length(Age5),j]
  data2 <- subset(data1, data1$N>0)
  
  CIdata <- CIex(x=data2$Age5, Nx=data2$N, Dx=data2$Ds, which.x = 0, ns=1000, level=0.95)
  
  return(list(
    exs = CIdata$ex,
    exms = CIdata$mean_ex,
    CIexlows = CIdata$CI_ex[1],
    CIexhighs = CIdata$CI_ex[2]
  ))
}

# Perform parallel computation
results <- foreach(j = 1:length(t), .combine = rbind) %dopar% {
  compute_CIex(j)
}

# Stop the parallel backend
stopCluster(cl)
lex_high_female$exs <- results[, "exs"]
lex_high_female$exms <- results[, "exms"]
lex_high_female$CIexlows <- results[, "CIexlows"]
lex_high_female$CIexhighs <- results[, "CIexhighs"]



#### Female Pop + High Quality : Life Exp at Age 30 ####

x <- c("t", "exs", "exms", "CIexlows", "CIexhighs")
Age5 <- seq(30,80,5)
t <- 1751:1900
lex_high_female_30 <- matrix(nrow = length(t), ncol = length(x))
colnames(lex_high_female_30) <- x    
lex_high_female_30 <- as.data.frame(lex_high_female_30)
lex_high_female_30$t <- t
fit_high_female_30 <- Mort2Dsmooth(x=1:length(Age5), y=t, Z=D_high_female_30, offset = log(E_high_female_30))
pred_mort_high_female_30 <- exp(fit_high_female_30$logmortality)
expected_deaths_high_female_30 <- pred_mort_high_female_30*E_high_female_30
expected_deathrates_high_female_30 <- expected_deaths_high_female_30/E_high_female_30


# Set the number of cores to be used (adjust the number as per your system)
num_cores <- 4

# Register parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Function to compute CIex for a given j
compute_CIex <- function(j) {
  data1 <- as.data.frame(Age5)
  data1$N <- E_high_female_30[1:length(Age5),j]
  data1$Ds <- expected_deaths_high_female_30[1:length(Age5),j]
  data2 <- subset(data1, data1$N>0)
  
  CIdata <- CIex(x=data2$Age5, Nx=data2$N, Dx=data2$Ds, which.x = 30, ns=1000, level=0.95)
  
  return(list(
    exs = CIdata$ex,
    exms = CIdata$mean_ex,
    CIexlows = CIdata$CI_ex[1],
    CIexhighs = CIdata$CI_ex[2]
  ))
}

# Perform parallel computation
results <- foreach(j = 1:length(t), .combine = rbind) %dopar% {
  compute_CIex(j)
}

# Stop the parallel backend
stopCluster(cl)
lex_high_female_30$exs <- results[, "exs"]
lex_high_female_30$exms <- results[, "exms"]
lex_high_female_30$CIexlows <- results[, "CIexlows"]
lex_high_female_30$CIexhighs <- results[, "CIexhighs"]

#### HMD Life Tables ####

Sweden_LifeTables <- read.csv("HMD_Data/Sweden_LifeTables.txt", sep="")
Sweden_LifeTables_Female <- read.csv("HMD_Data/Sweden_LifeTables_Female.txt", sep="")
Sweden_LifeTables_Male <- read.csv("HMD_Data/Sweden_LifeTables_Male.txt", sep="")

Sweden_LifeTables_hmd  = rbind(Sweden_LifeTables %>%
                                 filter(Age=="0",Year<=1900) %>%
                                 mutate(sex="Total") %>%
                                 dplyr::select(Year,sex,ex),
                               Sweden_LifeTables_Female %>%
                                 filter(Age=="0",Year<=1900) %>%
                                 mutate(sex="Female") %>%
                                 dplyr::select(Year,sex,ex),
                               Sweden_LifeTables_Male %>%
                                 filter(Age=="0",Year<=1900) %>%
                                 mutate(sex="Male") %>%
                                 dplyr::select(Year,sex,ex)) %>%
  rename(t=Year) %>%
  mutate(ex=as.numeric(ex))

Sweden_LifeTables_hmd_30  = rbind(Sweden_LifeTables %>%
                                    filter(Age=="30-34",Year<=1900) %>%
                                    mutate(sex="Total") %>%
                                    dplyr::select(Year,sex,ex),
                                  Sweden_LifeTables_Female %>%
                                    filter(Age=="30-34",Year<=1900) %>%
                                    mutate(sex="Female") %>%
                                    dplyr::select(Year,sex,ex),
                                  Sweden_LifeTables_Male %>%
                                    filter(Age=="30-34",Year<=1900) %>%
                                    mutate(sex="Male") %>%
                                    dplyr::select(Year,sex,ex)) %>%
  rename(t=Year) %>%
  mutate(ex=as.numeric(ex))

#### FamiLinx results ####

life_exp_birth_familinx = rbind(lex_high_female %>%
                         mutate(sex="Female",quality="Birth AND Death months"),
                       lex_high_male %>%
                         mutate(sex="Male",quality="Birth AND Death months"),
                       lex_high_total %>%
                         mutate(sex="Total",quality="Birth AND Death months"),
                       lex_low_female %>%
                         mutate(sex="Female",quality="Birth OR Death months"),
                       lex_low_male %>%
                         mutate(sex="Male",quality="Birth OR Death months"),
                       lex_low_total %>%
                         mutate(sex="Total",quality="Birth OR Death months")) 


life_exp_30_familinx = rbind(lex_high_female_30 %>%
                      mutate(sex="Female",quality="Birth AND Death months"),
                    lex_high_male_30 %>%
                      mutate(sex="Male",quality="Birth AND Death months"),
                    lex_high_total_30 %>%
                      mutate(sex="Total",quality="Birth AND Death months"),
                    lex_low_female_30 %>%
                      mutate(sex="Female",quality="Birth OR Death months"),
                    lex_low_male_30 %>%
                      mutate(sex="Male",quality="Birth OR Death months"),
                    lex_low_total_30 %>%
                      mutate(sex="Total",quality="Birth OR Death months")) 

save(Sweden_LifeTables_hmd,Sweden_LifeTables_hmd_30,
     life_exp_birth_familinx,life_exp_30_familinx,
     file='Results/results_mortality_analysis.RData')

