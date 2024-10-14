#### Install and upload libraries and useful functions ####
source('R-scripts/upload_packages.R')
source('R-scripts/functions.R')

#### Load data #####
load('Cleaned_Datasets/data_focal.RData')
load('Cleaned_Datasets/data_red_var.RData')
links <- data.table::fread(file = "Raw_Data/relations-anon.txt",na.strings = "*")


#### Distributions of births and deaths by country

ids_link = unique((unlist(links)))

births = data_focal %>%
  mutate(non_missing_births = ifelse(!is.na(birth_year),1,0)) %>%
  filter(!is.na(country_birth_final)) %>%
  group_by(country_birth_final) %>%
  summarize(births=sum(non_missing_births)) %>%
  arrange(desc(births)) %>%
  rename(country=country_birth_final) 

deaths = data_focal %>%
  mutate(non_missing_deaths = ifelse(!is.na(death_year),1,0)) %>%
  filter(!is.na(country_death_final)) %>%
  group_by(country_death_final) %>%
  summarize(deaths=sum(non_missing_deaths)) %>%
  arrange(desc(deaths)) %>%
  rename(country=country_death_final)

table_countries = inner_join(births,deaths,by=c('country'))

table_data_focal = list(gender_focal= data_focal %>%
  count(gender),
   birth_date_focal = data_focal %>%
   mutate(birth_year_non_missing=ifelse(!is.na(birth_year) & is.na(birth_month) & is.na(birth_day) ,1,0),
          birth_month_non_missing=ifelse(!is.na(birth_year) & !is.na(birth_month) & is.na(birth_day),1,0),
          birth_day_non_missing=ifelse(!is.na(birth_year) & !is.na(birth_month) & !is.na(birth_day),1,0),
          birth_year_missing=ifelse(is.na(birth_year),1,0)) %>%
   summarise(birth_year_non_missing=sum(birth_year_non_missing),
             birth_month_non_missing=sum(birth_month_non_missing),
             birth_day_non_missing=sum(birth_day_non_missing),
             birth_year_missing=sum(birth_year_missing)),
  death_date_focal = data_focal %>%
    mutate(death_year_non_missing=ifelse(!is.na(death_year) & is.na(death_month) & is.na(death_day) ,1,0),
           death_month_non_missing=ifelse(!is.na(death_year) & !is.na(death_month) & is.na(death_day),1,0),
           death_day_non_missing=ifelse(!is.na(death_year) & !is.na(death_month) & !is.na(death_day),1,0),
           death_year_missing=ifelse(is.na(death_year),1,0)) %>%
    summarise(death_year_non_missing=sum(death_year_non_missing),
              death_month_non_missing=sum(death_month_non_missing),
              death_day_non_missing=sum(death_day_non_missing),
              death_year_missing=sum(death_year_missing)),
  birth_country_focal = data_focal %>%
     mutate(birth_country=ifelse(!is.na(country_birth_final),1,0)) %>%
     summarise(birth_country_non_missing=sum(birth_country),
               birth_country_missing=sum(1-birth_country)),
  death_country_focal = data_focal %>%
    mutate(death_country=ifelse(!is.na(country_death_final),1,0)) %>%
    summarise(death_country_non_missing=sum(death_country),
              death_country_missing=sum(1-death_country))) 


table_data_focal = list(gender_focal= data_focal %>%
                          count(gender),
                        birth_date_focal = data_focal %>%
                          mutate(birth_year_non_missing=ifelse(!is.na(birth_year) & is.na(birth_month) & is.na(birth_day) ,1,0),
                                 birth_month_non_missing=ifelse(!is.na(birth_year) & !is.na(birth_month) & is.na(birth_day),1,0),
                                 birth_day_non_missing=ifelse(!is.na(birth_year) & !is.na(birth_month) & !is.na(birth_day),1,0),
                                 birth_year_missing=ifelse(is.na(birth_year),1,0)) %>%
                          summarise(birth_year_non_missing=sum(birth_year_non_missing),
                                    birth_month_non_missing=sum(birth_month_non_missing),
                                    birth_day_non_missing=sum(birth_day_non_missing),
                                    birth_year_missing=sum(birth_year_missing)),
                        death_date_focal = data_focal %>%
                          mutate(death_year_non_missing=ifelse(!is.na(death_year) & is.na(death_month) & is.na(death_day) ,1,0),
                                 death_month_non_missing=ifelse(!is.na(death_year) & !is.na(death_month) & is.na(death_day),1,0),
                                 death_day_non_missing=ifelse(!is.na(death_year) & !is.na(death_month) & !is.na(death_day),1,0),
                                 death_year_missing=ifelse(is.na(death_year),1,0)) %>%
                          summarise(death_year_non_missing=sum(death_year_non_missing),
                                    death_month_non_missing=sum(death_month_non_missing),
                                    death_day_non_missing=sum(death_day_non_missing),
                                    death_year_missing=sum(death_year_missing)),
                        birth_country_focal = data_focal %>%
                          mutate(birth_country=ifelse(!is.na(country_birth_final),1,0)) %>%
                          summarise(birth_country_non_missing=sum(birth_country),
                                    birth_country_missing=sum(1-birth_country)),
                        death_country_focal = data_focal %>%
                          mutate(death_country=ifelse(!is.na(country_death_final),1,0)) %>%
                          summarise(death_country_non_missing=sum(death_country),
                                    death_country_missing=sum(1-death_country)),
                        non_missing_link=nrow(data_focal[data_focal$profileid %in% ids_link,])) 



table_data_complete = list(gender_focal= data_red_var %>%
                                         count(gender),
                          birth_date_focal = data_red_var %>%
                          mutate(birth_year_non_missing=ifelse(!is.na(birth_year) & is.na(birth_month) & is.na(birth_day) ,1,0),
                                 birth_month_non_missing=ifelse(!is.na(birth_year) & !is.na(birth_month) & is.na(birth_day),1,0),
                                 birth_day_non_missing=ifelse(!is.na(birth_year) & !is.na(birth_month) & !is.na(birth_day),1,0),
                                 birth_year_missing=ifelse(is.na(birth_year),1,0)) %>%
                          summarise(birth_year_non_missing=sum(birth_year_non_missing),
                                    birth_month_non_missing=sum(birth_month_non_missing),
                                    birth_day_non_missing=sum(birth_day_non_missing),
                                    birth_year_missing=sum(birth_year_missing)),
                        death_date_focal = data_red_var %>%
                          mutate(death_year_non_missing=ifelse(!is.na(death_year) & is.na(death_month) & is.na(death_day) ,1,0),
                                 death_month_non_missing=ifelse(!is.na(death_year) & !is.na(death_month) & is.na(death_day),1,0),
                                 death_day_non_missing=ifelse(!is.na(death_year) & !is.na(death_month) & !is.na(death_day),1,0),
                                 death_year_missing=ifelse(is.na(death_year),1,0)) %>%
                          summarise(death_year_non_missing=sum(death_year_non_missing),
                                    death_month_non_missing=sum(death_month_non_missing),
                                    death_day_non_missing=sum(death_day_non_missing),
                                    death_year_missing=sum(death_year_missing)),
                        birth_country_focal = data_red_var %>%
                          mutate(birth_country=ifelse(!is.na(birth_country),1,0)) %>%
                          summarise(birth_country_non_missing=sum(birth_country),
                                    birth_country_missing=sum(1-birth_country)),
                        death_country_focal = data_red_var %>%
                          mutate(death_country=ifelse(!is.na(death_country),1,0)) %>%
                          summarise(death_country_non_missing=sum(death_country),
                                    death_country_missing=sum(1-death_country)),
                        non_missing_link=data.frame(non_missing_link=nrow(data_red_var[data_red_var$profileid %in% ids_link,]),
                                                    missing_link=nrow(data_red_var[!(data_red_var$profileid %in% ids_link),]))) 


save(table_countries,table_data_focal,
     table_data_complete,file='Results/tables_appendix.RData')



