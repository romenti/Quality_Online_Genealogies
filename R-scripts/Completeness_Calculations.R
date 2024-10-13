#### Install and upload libraries and useful functions ####
source('R-scripts/upload_packages.R')
source('R-scripts/functions.R')

#### load data ####
load('Cleaned_Datasets/data_red_var.RData')
load('Cleaned_Datasets/data_focal.RData')


# completeness demographic information measured as mean of non-missing information in  the selected demographic variables

# completeness in the initial data set
complete_sample = data_red_var %>%
  mutate(missing_birth_year=ifelse(!is.na(birth_year),1,0),
         missing_death_year=ifelse(!is.na(death_year),1,0),
         missing_birth_place=ifelse(!is.na(birth_country),1,0),
         missing_death_place=ifelse(!is.na(death_country),1,0),
         missing_gender=ifelse(!is.na(gender),1,0)) %>%
  summarise(completeness_birth_date=mean(missing_birth_year),
            completeness_death_date=mean(missing_death_year),
            completeness_birth_country=mean(missing_birth_place),
            completeness_death_country=mean(missing_death_place),
            completeness_gender=mean(missing_gender))

# completeness in the analytical sample

reduced_sample= data_focal %>%
  mutate(missing_birth_year=ifelse(!is.na(birth_year),1,0),
         missing_death_year=ifelse(!is.na(death_year),1,0),
         missing_birth_place=ifelse(!is.na(country_birth_final),1,0),
         missing_death_place=ifelse(!is.na(country_death_final),1,0),
         missing_gender=ifelse(!is.na(gender),1,0)) %>%
  summarise(completeness_birth_date=mean(missing_birth_year),
            completeness_death_date=mean(missing_death_year),
            completeness_birth_country=mean(missing_birth_place),
            completeness_death_country=mean(missing_death_place),
            completeness_gender=mean(missing_gender))


data_spider_chart1 =data.frame(completeness_birth_date=c(1,0),
                                   completeness_death_date=c(1,0),
                                   completeness_birth_country=c(1,0),
                                   completeness_death_country=c(1,0),
                                   completeness_gender=c(1,0)) %>%
  rbind(complete_sample, reduced_sample)

rownames(data_spider_chart1)=c("Max","Min","Initial dataset", "Analytical sample")
colnames(data_spider_chart1)=c("Completeness year of birth", "Completeness year of death",
                                    "Completeness place of birth", "Completeness place of death",
                                    "Completeness gender")





### spider chart conditional on having some demographic information available ###

# non-missing gender
complete_sample_gender= data_red_var %>%
  mutate(missing_birth_year=ifelse(!is.na(birth_year),1,0),
         missing_death_year=ifelse(!is.na(death_year),1,0),
         missing_birth_place=ifelse(!is.na(birth_country),1,0),
         missing_death_place=ifelse(!is.na(death_country),1,0),
         missing_gender=ifelse(!is.na(gender),1,0)) %>%
  filter(!is.na(gender))%>%
  summarise(completeness_birth_date=mean(missing_birth_year),
            completeness_death_date=mean(missing_death_year),
            completeness_birth_country=mean(missing_birth_place),
            completeness_death_country=mean(missing_death_place),
            completeness_gender=mean(missing_gender))

# non-missing year of birth
complete_sample_birth_date= data_red_var %>%
  mutate(missing_birth_year=ifelse(!is.na(birth_year),1,0),
         missing_death_year=ifelse(!is.na(death_year),1,0),
         missing_birth_place=ifelse(!is.na(birth_country),1,0),
         missing_death_place=ifelse(!is.na(death_country),1,0),
         missing_gender=ifelse(!is.na(gender),1,0)) %>%
  filter(!is.na(birth_year))%>%
  summarise(completeness_birth_date=mean(missing_birth_year),
            completeness_death_date=mean(missing_death_year),
            completeness_birth_country=mean(missing_birth_place),
            completeness_death_country=mean(missing_death_place),
            completeness_gender=mean(missing_gender))

# non-missing year of death
complete_sample_death_date= data_red_var %>%
  mutate(missing_birth_year=ifelse(!is.na(birth_year),1,0),
         missing_death_year=ifelse(!is.na(death_year),1,0),
         missing_birth_place=ifelse(!is.na(birth_country),1,0),
         missing_death_place=ifelse(!is.na(death_country),1,0),
         missing_gender=ifelse(!is.na(gender),1,0)) %>%
  filter(!is.na(death_year))%>%
  summarise(completeness_birth_date=mean(missing_birth_year),
            completeness_death_date=mean(missing_death_year),
            completeness_birth_country=mean(missing_birth_place),
            completeness_death_country=mean(missing_death_place),
            completeness_gender=mean(missing_gender))

# non-missing country of birth
complete_sample_birth_country = data_red_var %>%
  mutate(missing_birth_year=ifelse(!is.na(birth_year),1,0),
         missing_death_year=ifelse(!is.na(death_year),1,0),
         missing_birth_place=ifelse(!is.na(birth_country),1,0),
         missing_death_place=ifelse(!is.na(death_country),1,0),
         missing_gender=ifelse(!is.na(gender),1,0)) %>%
  filter(!is.na(birth_country))%>%
  summarise(completeness_birth_date=mean(missing_birth_year),
            completeness_death_date=mean(missing_death_year),
            completeness_birth_country=mean(missing_birth_place),
            completeness_death_country=mean(missing_death_place),
            completeness_gender=mean(missing_gender))

# non-missing country of death

complete_sample_death_country = data_red_var %>%
  mutate(missing_birth_year=ifelse(!is.na(birth_year),1,0),
         missing_death_year=ifelse(!is.na(death_year),1,0),
         missing_birth_place=ifelse(!is.na(birth_country),1,0),
         missing_death_place=ifelse(!is.na(death_country),1,0),
         missing_gender=ifelse(!is.na(gender),1,0)) %>%
  filter(!is.na(death_country))%>%
  summarise(completeness_birth_date=mean(missing_birth_year),
            completeness_death_date=mean(missing_death_year),
            completeness_birth_country=mean(missing_birth_place),
            completeness_death_country=mean(missing_death_place),
            completeness_gender=mean(missing_gender))

data_spider_chart2 =data.frame(completeness_birth_date=c(1,0),
                                    completeness_death_date=c(1,0),
                                    completeness_birth_country=c(1,0),
                                    completeness_death_country=c(1,0),
                                    completeness_gender=c(1,0)) %>%
  rbind(complete_sample_gender,complete_sample_birth_date,complete_sample_death_date,complete_sample_birth_country,complete_sample_death_country)

rownames(data_spider_chart2)=c("Max","Min","Gender known","Year of birth known","Year of death known","Place of birth known","Place of death known")
colnames(data_spider_chart2)=c("Completeness year of birth", "Completeness year of death",
                                     "Completeness place of birth", "Completeness place of death",
                                     "Completeness gender")


#### Non-missing demographic variables by country of birth in initial sample ####

# USA
reduced_sample_USA = data_red_var%>%
  filter(birth_country=="USA")%>%
  mutate(missing_birth_year=ifelse(!is.na(birth_year),1,0),
         missing_death_year=ifelse(!is.na(death_year),1,0),
         missing_birth_place=ifelse(!is.na(birth_country),1,0),
         missing_death_place=ifelse(!is.na(death_country),1,0),
         missing_gender=ifelse(!is.na(gender),1,0)) %>%
  summarise(completeness_birth_date=mean(missing_birth_year),
            completeness_death_date=mean(missing_death_year),
            completeness_death_country=mean(missing_death_place),
            completeness_gender=mean(missing_gender))

# GERMANY
reduced_sample_Germany = data_red_var%>%
  filter(birth_country=="Germany") %>%
  mutate(missing_birth_year=ifelse(!is.na(birth_year),1,0),
         missing_death_year=ifelse(!is.na(death_year),1,0),
         missing_birth_place=ifelse(!is.na(birth_country),1,0),
         missing_death_place=ifelse(!is.na(death_country),1,0),
         missing_gender=ifelse(!is.na(gender),1,0)) %>%
  summarise(completeness_birth_date=mean(missing_birth_year),
            completeness_death_date=mean(missing_death_year),
            completeness_death_country=mean(missing_death_place),
            completeness_gender=mean(missing_gender))  

# SWEDEN
reduced_sample_Sweden = data_red_var %>%
  filter(birth_country=="Sweden")%>%
  mutate(missing_birth_year=ifelse(!is.na(birth_year),1,0),
         missing_death_year=ifelse(!is.na(death_year),1,0),
         missing_birth_place=ifelse(!is.na(birth_country),1,0),
         missing_death_place=ifelse(!is.na(death_country),1,0),
         missing_gender=ifelse(!is.na(gender),1,0)) %>%
  summarise(completeness_birth_date=mean(missing_birth_year),
            completeness_death_date=mean(missing_death_year),
            completeness_death_country=mean(missing_death_place),
            completeness_gender=mean(missing_gender)) 

# CANADA
reduced_sample_Canada = data_red_var %>%
  filter(birth_country=="Canada")%>%
  mutate(missing_birth_year=ifelse(!is.na(birth_year),1,0),
         missing_death_year=ifelse(!is.na(death_year),1,0),
         missing_birth_place=ifelse(!is.na(birth_country),1,0),
         missing_death_place=ifelse(!is.na(death_country),1,0),
         missing_gender=ifelse(!is.na(gender),1,0)) %>%
  summarise(completeness_birth_date=mean(missing_birth_year),
            completeness_death_date=mean(missing_death_year),
            completeness_death_country=mean(missing_death_place),
            completeness_gender=mean(missing_gender)) 

# UK
reduced_sample_UK = data_red_var %>%
  filter(birth_country=="UK")%>%
  mutate(missing_birth_year=ifelse(!is.na(birth_year),1,0),
         missing_death_year=ifelse(!is.na(death_year),1,0),
         missing_birth_place=ifelse(!is.na(birth_country),1,0),
         missing_death_place=ifelse(!is.na(death_country),1,0),
         missing_gender=ifelse(!is.na(gender),1,0)) %>%
  summarise(completeness_birth_date=mean(missing_birth_year),
            completeness_death_date=mean(missing_death_year),
            completeness_death_country=mean(missing_death_place),
            completeness_gender=mean(missing_gender)) 

data_spider_chart3 =data.frame(completeness_birth_date=c(1,0),
                          completeness_death_date=c(1,0),
                          completeness_death_country=c(1,0),
                          completeness_gender=c(1,0)) %>%
  rbind(reduced_sample_USA,reduced_sample_Germany,reduced_sample_Sweden,reduced_sample_Canada,reduced_sample_UK)

rownames(data_spider_chart3)=c("Max","Min","USA","GERMANY","SWEDEN","CANADA","UK")
colnames(data_spider_chart3)=c("Completeness year of birth", "Completeness year of death",
                           "Completeness place of death",
                           "Completeness gender")



#### Non-missing demographic variables by country of birth in analytical sample ####


complete_sample_USA = data_focal %>%
  filter(country_birth_final=="USA")%>%
  mutate(missing_birth_year=ifelse(!is.na(birth_year),1,0),
         missing_death_year=ifelse(!is.na(death_year),1,0),
         missing_birth_place=ifelse(!is.na(country_birth_final),1,0),
         missing_death_place=ifelse(!is.na(country_death_final),1,0),
         missing_gender=ifelse(!is.na(gender),1,0)) %>%
  summarise(completeness_birth_date=mean(missing_birth_year),
            completeness_death_date=mean(missing_death_year),
            completeness_death_country=mean(missing_death_place),
            completeness_gender=mean(missing_gender))

complete_sample_Germany = data_focal %>%
  filter(country_birth_final=="GERMANY") %>%
  mutate(missing_birth_year=ifelse(!is.na(birth_year),1,0),
         missing_death_year=ifelse(!is.na(death_year),1,0),
         missing_birth_place=ifelse(!is.na(country_birth_final),1,0),
         missing_death_place=ifelse(!is.na(country_death_final),1,0),
         missing_gender=ifelse(!is.na(gender),1,0)) %>%
  summarise(completeness_birth_date=mean(missing_birth_year),
            completeness_death_date=mean(missing_death_year),
            completeness_death_country=mean(missing_death_place),
            completeness_gender=mean(missing_gender))  

complete_sample_Sweden = data_focal %>%
  filter(country_birth_final=="SWEDEN")%>%
  mutate(missing_birth_year=ifelse(!is.na(birth_year),1,0),
         missing_death_year=ifelse(!is.na(death_year),1,0),
         missing_birth_place=ifelse(!is.na(country_birth_final),1,0),
         missing_death_place=ifelse(!is.na(country_death_final),1,0),
         missing_gender=ifelse(!is.na(gender),1,0)) %>%
  summarise(completeness_birth_date=mean(missing_birth_year),
            completeness_death_date=mean(missing_death_year),
            completeness_death_country=mean(missing_death_place),
            completeness_gender=mean(missing_gender)) 

complete_sample_Canada = data_focal %>%
  filter(country_birth_final=="CANADA")%>%
  mutate(missing_birth_year=ifelse(!is.na(birth_year),1,0),
         missing_death_year=ifelse(!is.na(death_year),1,0),
         missing_birth_place=ifelse(!is.na(country_birth_final),1,0),
         missing_death_place=ifelse(!is.na(country_death_final),1,0),
         missing_gender=ifelse(!is.na(gender),1,0)) %>%
  summarise(completeness_birth_date=mean(missing_birth_year),
            completeness_death_date=mean(missing_death_year),
            completeness_death_country=mean(missing_death_place),
            completeness_gender=mean(missing_gender)) 

complete_sample_UK = data_focal %>%
  filter(country_birth_final=="UK")%>%
  mutate(missing_birth_year=ifelse(!is.na(birth_year),1,0),
         missing_death_year=ifelse(!is.na(death_year),1,0),
         missing_birth_place=ifelse(!is.na(country_birth_final),1,0),
         missing_death_place=ifelse(!is.na(country_death_final),1,0),
         missing_gender=ifelse(!is.na(gender),1,0)) %>%
  summarise(completeness_birth_date=mean(missing_birth_year),
            completeness_death_date=mean(missing_death_year),
            completeness_death_country=mean(missing_death_place),
            completeness_gender=mean(missing_gender)) 

data_spider_chart4=data.frame(completeness_birth_date=c(1,0),
                                   completeness_death_date=c(1,0),
                                   completeness_death_country=c(1,0),
                                   completeness_gender=c(1,0)) %>%
  rbind(complete_sample_USA,complete_sample_Germany,complete_sample_Sweden,complete_sample_Canada,complete_sample_UK)

rownames(data_spider_chart4)=c("Max","Min","USA","Germany","Sweden","Canada","UK")
colnames(data_spider_chart4)=c("Completeness year of birth", "Completeness year of death",
                                    "Completeness place of death",
                                    "Completeness gender")



save(data_spider_chart1,data_spider_chart2,data_spider_chart3,data_spider_chart4,file='Results/spider_chart_data.RData')
