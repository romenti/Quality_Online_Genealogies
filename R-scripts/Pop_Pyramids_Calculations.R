#### Install and upload libraries ####
source('R-scripts/upload_packages.R')
source('R-scripts/functions.R')

#### Population counts ####

load('Cleaned_Datasets/data_red_var.RData')

# data: data from online genealogies

# country_name: string for the name of the country of interest

# absolute population counts by year, sex and country from Human Mortality Database

Sweden_Population <- read.csv("HMD_Data/Sweden_Population.txt", sep="")

### SWEDEN 1751 ###

sweden_1751=Sweden_Population %>%
  filter(Year==1751)%>%
  pivot_longer(!c("Year","Age"), names_to="gender", values_to="counts") %>%
  mutate(Age=ifelse(substr(Age,1,3)=="110",110,as.numeric(Age)))%>%
  mutate(Age=case_when(Age<5 ~ "0-4",
                       Age>=5 & Age<10 ~ "5-9",
                       Age>=10 & Age<15 ~ "10-14",
                       Age>=15 & Age<20 ~ "15-19",
                       Age>=20 & Age<25 ~ "20-24",
                       Age>=25 & Age<30 ~ "25-29",
                       Age>=30 & Age<35 ~ "30-34",
                       Age>=35 & Age<40 ~ "35-39",
                       Age>=40 & Age<45 ~ "40-44",
                       Age>=45 & Age<50 ~ "45-49",
                       Age>=50 & Age<55 ~ "50-54",
                       Age>=55 & Age<60 ~ "55-59",
                       Age>=60 & Age<65 ~ "60-64",
                       Age>=65 & Age<70 ~ "65-69",
                       Age>=70 & Age<75 ~ "70-74",
                       Age>=75 & Age<80 ~ "75-79",
                       Age>=80 ~ "80+")) %>%
  mutate(Age=factor(Age, levels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54",
                                  "55-59","60-64","65-69","70-74","75-79","80+")))%>%
  group_by(Year,Age,gender)%>%
  summarise(counts=sum(counts)) %>%
  filter(gender!="Total")%>%
  ungroup()%>%
  group_by(Year)%>%
  mutate(pop_tot=sum(counts),
         pop_perc=(counts/pop_tot)*100) %>%
  mutate(gender=tolower(gender))



### Sweden 1751, available year and month of birth and death
# subset of sample with only observations with available year and month for birth and death

subset_complete_dates= data_red_var %>%
  filter(!is.na(birth_month) & !is.na(death_month)) %>%
  rename(country_birth_final=birth_country,
         country_death_final=death_country)
 

sweden_1751_complete_dates=pop_pyramid_calculation(subset_complete_dates,"Sweden",year_min=1751,year_max=1751) %>%
  filter(gender!="total")%>%
  group_by(years)%>%
  mutate(pop_tot_complete_dates=sum(counts),
         pop_perc_complete_dates=(counts/pop_tot_complete_dates)*100)%>%
  rename(counts_complete_dates=counts)

### Sweden 1751, available year and missing month birth or death
# subset of sample with at least one date uncomplete
subset_uncomplete_dates = data_red_var %>%
  filter(!is.na(birth_year) & !is.na(death_year) & (is.na(birth_month) | is.na(death_month))) %>%
  rename(country_birth_final=birth_country,
         country_death_final=death_country)

sweden_1751_uncomplete_dates = pop_pyramid_calculation(subset_uncomplete_dates,"Sweden",year_min=1751,year_max=1751) %>%
  filter(gender!="total")%>%
  group_by(years)%>%
  mutate(pop_tot_uncomplete_dates=sum(counts),
         pop_perc_uncomplete_dates=(counts/pop_tot_uncomplete_dates)*100) %>%
  rename(counts_uncomplete_dates=counts)

# merge three values: real data, complete dates, uncomplete dates
sweden_1751=sweden_1751 %>%
  select(Year, Age, gender, counts, pop_tot, pop_perc) %>%
  left_join(sweden_1751_complete_dates %>%
              select(years, age_class, gender, counts_complete_dates, pop_tot_complete_dates, pop_perc_complete_dates) %>%
              rename(Year=years, Age=age_class), by=c("Year", "Age", "gender")) %>%
  left_join(sweden_1751_uncomplete_dates %>%
              select(years, age_class, gender, counts_uncomplete_dates, pop_tot_uncomplete_dates, pop_perc_uncomplete_dates) %>%
              rename(Year=years, Age=age_class), by=c("Year", "Age", "gender"))

### SWEDEN 1800 ###
sweden_1800=Sweden_Population %>%
  filter(Year==1800)%>%
  pivot_longer(!c("Year","Age"), names_to="gender", values_to="counts") %>%
  mutate(Age=ifelse(substr(Age,1,3)=="110",110,as.numeric(Age)))%>%
  mutate(Age=case_when(Age<5 ~ "0-4",
                       Age>=5 & Age<10 ~ "5-9",
                       Age>=10 & Age<15 ~ "10-14",
                       Age>=15 & Age<20 ~ "15-19",
                       Age>=20 & Age<25 ~ "20-24",
                       Age>=25 & Age<30 ~ "25-29",
                       Age>=30 & Age<35 ~ "30-34",
                       Age>=35 & Age<40 ~ "35-39",
                       Age>=40 & Age<45 ~ "40-44",
                       Age>=45 & Age<50 ~ "45-49",
                       Age>=50 & Age<55 ~ "50-54",
                       Age>=55 & Age<60 ~ "55-59",
                       Age>=60 & Age<65 ~ "60-64",
                       Age>=65 & Age<70 ~ "65-69",
                       Age>=70 & Age<75 ~ "70-74",
                       Age>=75 & Age<80 ~ "75-79",
                       Age>=80 ~ "80+")) %>%
  mutate(Age=factor(Age, levels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54",
                                  "55-59","60-64","65-69","70-74","75-79","80+")))%>%
  group_by(Year,Age,gender)%>%
  summarise(counts=sum(counts)) %>%
  filter(gender!="Total")%>%
  ungroup()%>%
  group_by(Year)%>%
  mutate(pop_tot=sum(counts),
         pop_perc=(counts/pop_tot)*100) %>%
  mutate(gender=tolower(gender))



### Sweden 1800, available year and month of birth and death
# subset of sample with only observations with available year and month for birth and death
subset_complete_dates= data_red_var %>%
  filter(!is.na(birth_month) & !is.na(death_month)) %>%
  rename(country_birth_final=birth_country,
         country_death_final=death_country)

sweden_1800_complete_dates=pop_pyramid_calculation(subset_complete_dates,"Sweden",year_min=1800,year_max=1800) %>%
  filter(gender!="total")%>%
  group_by(years)%>%
  mutate(pop_tot_complete_dates=sum(counts),
         pop_perc_complete_dates=(counts/pop_tot_complete_dates)*100)%>%
  rename(counts_complete_dates=counts) 

### Sweden 1800, available year and missing month birth or death
# subset of sample with at least one date uncomplete
subset_uncomplete_dates = data_red_var %>%
  filter(!is.na(birth_year) & !is.na(death_year) & (is.na(birth_month) | is.na(death_month))) %>%
  rename(country_birth_final=birth_country,
         country_death_final=death_country)

sweden_1800_uncomplete_dates=pop_pyramid_calculation(subset_uncomplete_dates,"Sweden",year_min=1800,year_max=1800) %>%
  filter(gender!="total")%>%
  group_by(years)%>%
  mutate(pop_tot_uncomplete_dates=sum(counts),
         pop_perc_uncomplete_dates=(counts/pop_tot_uncomplete_dates)*100) %>%
  rename(counts_uncomplete_dates=counts)

# merge three values: real data, complete dates, uncomplete dates
sweden_1800=sweden_1800 %>%
  select(Year, Age, gender, counts, pop_tot, pop_perc) %>%
  left_join(sweden_1800_complete_dates %>%
              select(years, age_class, gender, counts_complete_dates, pop_tot_complete_dates, pop_perc_complete_dates) %>%
              rename(Year=years, Age=age_class), by=c("Year", "Age", "gender")) %>%
  left_join(sweden_1800_uncomplete_dates %>%
              select(years, age_class, gender, counts_uncomplete_dates, pop_tot_uncomplete_dates, pop_perc_uncomplete_dates) %>%
              rename(Year=years, Age=age_class), by=c("Year", "Age", "gender"))

### SWEDEN 1850 ###
sweden_1850=Sweden_Population %>%
  filter(Year==1850)%>%
  pivot_longer(!c("Year","Age"), names_to="gender", values_to="counts") %>%
  mutate(Age=ifelse(substr(Age,1,3)=="110",110,as.numeric(Age)))%>%
  mutate(Age=case_when(Age<5 ~ "0-4",
                       Age>=5 & Age<10 ~ "5-9",
                       Age>=10 & Age<15 ~ "10-14",
                       Age>=15 & Age<20 ~ "15-19",
                       Age>=20 & Age<25 ~ "20-24",
                       Age>=25 & Age<30 ~ "25-29",
                       Age>=30 & Age<35 ~ "30-34",
                       Age>=35 & Age<40 ~ "35-39",
                       Age>=40 & Age<45 ~ "40-44",
                       Age>=45 & Age<50 ~ "45-49",
                       Age>=50 & Age<55 ~ "50-54",
                       Age>=55 & Age<60 ~ "55-59",
                       Age>=60 & Age<65 ~ "60-64",
                       Age>=65 & Age<70 ~ "65-69",
                       Age>=70 & Age<75 ~ "70-74",
                       Age>=75 & Age<80 ~ "75-79",
                       Age>=80 ~ "80+")) %>%
  mutate(Age=factor(Age, levels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54",
                                  "55-59","60-64","65-69","70-74","75-79","80+")))%>%
  group_by(Year,Age,gender)%>%
  summarise(counts=sum(counts)) %>%
  filter(gender!="Total")%>%
  ungroup()%>%
  group_by(Year)%>%
  mutate(pop_tot=sum(counts),
         pop_perc=(counts/pop_tot)*100) %>%
  mutate(gender=tolower(gender))



### Sweden 1850, available year and month of birth and death
# subset of sample with only observations with available year and month for birth and death
subset_complete_dates = data_red_var %>%
  filter(!is.na(birth_month) & !is.na(death_month)) %>%
  rename(country_birth_final=birth_country,
         country_death_final=death_country)

sweden_1850_complete_dates=pop_pyramid_calculation(subset_complete_dates,"Sweden",year_min=1850,year_max=1850) %>%
  filter(gender!="total")%>%
  group_by(years)%>%
  mutate(pop_tot_complete_dates=sum(counts),
         pop_perc_complete_dates=(counts/pop_tot_complete_dates)*100)%>%
  rename(counts_complete_dates=counts)

### Sweden 1850, available year and missing month birth or death
# subset of sample with at least one date uncomplete
subset_uncomplete_dates=data_var_red %>%
  filter(!is.na(birth_year) & !is.na(death_year) & (is.na(birth_month) | is.na(death_month))) %>%
  rename(country_birth_final=birth_country,
         country_death_final=death_country)

sweden_1850_uncomplete_dates=pop_pyramid_calculation(subset_uncomplete_dates,"Sweden",year_min=1850,year_max=1850) %>%
  filter(gender!="total")%>%
  group_by(years)%>%
  mutate(pop_tot_uncomplete_dates=sum(counts),
         pop_perc_uncomplete_dates=(counts/pop_tot_uncomplete_dates)*100) %>%
  rename(counts_uncomplete_dates=counts)

# merge three values: real data, complete dates, uncomplete dates
sweden_1850=sweden_1850 %>%
  select(Year, Age, gender, counts, pop_tot, pop_perc) %>%
  left_join(sweden_1850_complete_dates %>%
              select(years, age_class, gender, counts_complete_dates, pop_tot_complete_dates, pop_perc_complete_dates) %>%
              rename(Year=years, Age=age_class), by=c("Year", "Age", "gender")) %>%
  left_join(sweden_1850_uncomplete_dates %>%
              select(years, age_class, gender, counts_uncomplete_dates, pop_tot_uncomplete_dates, pop_perc_uncomplete_dates) %>%
              rename(Year=years, Age=age_class), by=c("Year", "Age", "gender"))


### SWEDEN 1900 ###
sweden_1900=Sweden_Population %>%
  filter(Year==1900)%>%
  pivot_longer(!c("Year","Age"), names_to="gender", values_to="counts") %>%
  mutate(Age=ifelse(substr(Age,1,3)=="110",110,as.numeric(Age)))%>%
  mutate(Age=case_when(Age<5 ~ "0-4",
                       Age>=5 & Age<10 ~ "5-9",
                       Age>=10 & Age<15 ~ "10-14",
                       Age>=15 & Age<20 ~ "15-19",
                       Age>=20 & Age<25 ~ "20-24",
                       Age>=25 & Age<30 ~ "25-29",
                       Age>=30 & Age<35 ~ "30-34",
                       Age>=35 & Age<40 ~ "35-39",
                       Age>=40 & Age<45 ~ "40-44",
                       Age>=45 & Age<50 ~ "45-49",
                       Age>=50 & Age<55 ~ "50-54",
                       Age>=55 & Age<60 ~ "55-59",
                       Age>=60 & Age<65 ~ "60-64",
                       Age>=65 & Age<70 ~ "65-69",
                       Age>=70 & Age<75 ~ "70-74",
                       Age>=75 & Age<80 ~ "75-79",
                       Age>=80 ~ "80+")) %>%
  mutate(Age=factor(Age, levels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54",
                                  "55-59","60-64","65-69","70-74","75-79","80+")))%>%
  group_by(Year,Age,gender)%>%
  summarise(counts=sum(counts)) %>%
  filter(gender!="Total")%>%
  ungroup()%>%
  group_by(Year)%>%
  mutate(pop_tot=sum(counts),
         pop_perc=(counts/pop_tot)*100) %>%
  mutate(gender=tolower(gender))



### Sweden 1900, available year and month of birth and death
# subset of sample with only observations with available year and month for birth and death
subset_complete_dates= data_red_var %>%
  filter(!is.na(birth_month) & !is.na(death_month)) %>%
  rename(country_birth_final=birth_country,
         country_death_final=death_country)

sweden_1900_complete_dates=pop_pyramid_calculation(subset_complete_dates,"Sweden",year_min=1900,year_max=1900) %>%
  filter(gender!="total")%>%
  group_by(years)%>%
  mutate(pop_tot_complete_dates=sum(counts),
         pop_perc_complete_dates=(counts/pop_tot_complete_dates)*100)%>%
  rename(counts_complete_dates=counts)

### Sweden 1900, available year and missing month birth or death
# subset of sample with at least one date uncomplete
subset_uncomplete_dates= data_red_var %>%
  filter(!is.na(birth_year) & !is.na(death_year) & (is.na(birth_month) | is.na(death_month))) %>%
  rename(country_birth_final=birth_country,
         country_death_final=death_country)

sweden_1900_uncomplete_dates=pop_pyramid_calculation(subset_uncomplete_dates,"Sweden",year_min=1900,year_max=1900) %>%
  filter(gender!="total")%>%
  group_by(years)%>%
  mutate(pop_tot_uncomplete_dates=sum(counts),
         pop_perc_uncomplete_dates=(counts/pop_tot_uncomplete_dates)*100) %>%
  rename(counts_uncomplete_dates=counts)

# merge three values: real data, complete dates, uncomplete dates
sweden_1900=sweden_1900 %>%
  select(Year, Age, gender, counts, pop_tot, pop_perc) %>%
  left_join(sweden_1900_complete_dates %>%
              select(years, age_class, gender, counts_complete_dates, pop_tot_complete_dates, pop_perc_complete_dates) %>%
              rename(Year=years, Age=age_class), by=c("Year", "Age", "gender")) %>%
  left_join(sweden_1900_uncomplete_dates %>%
              select(years, age_class, gender, counts_uncomplete_dates, pop_tot_uncomplete_dates, pop_perc_uncomplete_dates) %>%
              rename(Year=years, Age=age_class), by=c("Year", "Age", "gender"))

save(sweden_1751, file = "Results/sweden_1751.RData")
save(sweden_1800, file = "Results/sweden_1800.RData")
save(sweden_1850, file = "Results/sweden_1850.RData")
save(sweden_1900, file = "Results/sweden_1900.RData")


### all years from 1751-1900
sweden_1751_1900 = Sweden_Population %>%
  filter(Year>=1751 & Year<=1900)%>%
  pivot_longer(!c("Year","Age"), names_to="gender", values_to="counts") %>%
  mutate(Age=ifelse(substr(Age,1,3)=="110",110,as.numeric(Age)))%>%
  mutate(Age=case_when(Age<15 ~ "0-14",
                       Age>=15 & Age<65 ~ "15-64",
                       Age>=65 ~ "65+")) %>%
  mutate(Age=factor(Age, levels=c("0-14","15-64","65+")))%>%
  group_by(Year,Age,gender)%>%
  summarise(counts=sum(counts)) %>%
  filter(gender!="Total")%>%
  ungroup()%>%
  group_by(Year)%>%
  mutate(pop_tot=sum(counts),
         pop_perc=(counts/pop_tot)*100) %>%
  mutate(gender=tolower(gender))


sweden_1751_1900_complete_dates =pop_pyramid_calculation_2(subset_complete_dates,"Sweden",year_min=1751,year_max=1900) %>%
  filter(gender!="total")%>%
  group_by(years)%>%
  mutate(pop_tot=sum(counts),
         pop_perc_complete_dates=(counts/pop_tot)*100)


sweden_1751_1900_uncomplete_dates = pop_pyramid_calculation_2(subset_uncomplete_dates,"Sweden",year_min=1751,year_max=1900) %>%
  filter(gender!="total")%>%
  group_by(years)%>%
  mutate(pop_tot=sum(counts),
         pop_perc_uncomplete_dates=(counts/pop_tot)*100)

sweden_1751_1900=sweden_1751_1900 %>%
  dplyr::select(Year, Age, gender, pop_perc) %>%
  left_join(sweden_1751_1900_complete_dates %>%
              dplyr::select(years, age_class, gender, pop_perc_complete_dates) %>%
              rename(Year=years, Age=age_class), by=c("Year", "Age", "gender")) %>%
  left_join(sweden_1751_1900_uncomplete_dates %>%
              dplyr::select(years, age_class, gender, pop_perc_uncomplete_dates) %>%
              rename(Year=years, Age=age_class), by=c("Year", "Age", "gender"))

save(sweden_1751_1900,file='Results/differences_pyramid.RData')
