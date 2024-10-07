#### upload packages ####
source('R-scripts/upload_packages.R')

#### upload functions ####

source('R-scripts/functions.R')

#### Focal data set ####

load('Cleaned_Datasets/data_focal.RData')

#### Focal - Birth Cohorts ####

data_focal_birth_cohorts = data_focal %>%
  filter(birth_year>=1600,birth_year<=1900) %>%
  mutate(birth_cohorts = case_when(birth_year>=1600 & birth_year<1625 ~ "1600-1624",
                                   birth_year>=1625 & birth_year<1650 ~ "1625-1649",
                                   birth_year>=1650 & birth_year<1675 ~ "1650-1674",
                                   birth_year>=1675 & birth_year<1700 ~ "1675-1699",
                                   birth_year>=1700 & birth_year<1725 ~ "1700-1724",
                                   birth_year>=1725 & birth_year<1750 ~ "1725-1749",
                                   birth_year>=1750 & birth_year<1775 ~ "1750-1774",
                                   birth_year>=1775 & birth_year<1800 ~ "1775-1799",
                                   birth_year>=1800 & birth_year<1825 ~ "1800-1824",
                                   birth_year>=1825 & birth_year<1850 ~ "1825-1849",
                                   birth_year>=1850 & birth_year<1875 ~ "1850-1874",
                                   birth_year>=1875 & birth_year<=1900 ~ "1875-1900",
                                   TRUE ~ NA)) %>%
  mutate(birth_cohorts = factor(birth_cohorts,levels = c("1600-1624","1625-1649","1650-1674",
                                                         "1675-1699","1700-1724","1725-1749",
                                                         "1750-1774","1775-1799","1800-1824",
                                                         "1825-1849","1850-1874","1875-1900")))

#### Focal - Death Cohorts ####

data_focal_death_cohorts = indicator_demo %>%
  filter(death_year>=1600,
         death_year<=1900,
         !(is.na(death_year))) %>%
  mutate(death_cohorts = case_when(death_year>=1600 & death_year<1625 ~ "1600-1624",
                                   death_year>=1625 & death_year<1650 ~ "1625-1649",
                                   death_year>=1650 & death_year<1675 ~ "1650-1674",
                                   death_year>=1675 & death_year<1700 ~ "1675-1699",
                                   death_year>=1700 & death_year<1725 ~ "1700-1724",
                                   death_year>=1725 & death_year<1750 ~ "1725-1749",
                                   death_year>=1750 & death_year<1775 ~ "1750-1774",
                                   death_year>=1775 & death_year<1800 ~ "1775-1799",
                                   death_year>=1800 & death_year<1825 ~ "1800-1824",
                                   death_year>=1825 & death_year<1850 ~ "1825-1849",
                                   death_year>=1850 & death_year<1875 ~ "1850-1874",
                                   death_year>=1875 & death_year<=1900 ~ "1875-1900",
                                   TRUE ~ NA)) %>%
  mutate(death_cohorts = factor(death_cohorts,levels = c("1600-1624","1625-1649","1650-1674",
                                                         "1675-1699","1700-1724","1725-1749",
                                                         "1750-1774","1775-1799","1800-1824",
                                                         "1825-1849","1850-1874","1875-1900")))




#### Parents #####

load('Cleaned_Datasets/parents_data_set.RData')

data_parents_birthyear = parents_data_set  %>%
  filter(!(is.na(f) & is.na(m) & is.na(p_unknown))) %>%
  dplyr::select(profileid,f,m,p_unknown,birth_year,m_birth_year,f_birth_year,p_unknown_birth_year) %>%
  pivot_longer(!c("profileid","f","m","p_unknown","birth_year"),names_to="parents",values_to = "birth_year_parents") %>%
  filter(!(p_unknown==0 & parents=="p_unknown_birth_year"),
         !(f==0 & parents=="f_birth_year"),
         !(m==0 & parents=="m_birth_year")) %>%
  mutate(complete_birthyear_parents = ifelse(!(is.na(birth_year_parents)),1,0),
         complete_birthyear_focal = ifelse(!(is.na(birth_year)),1,0)
  )  %>%
  ungroup() %>%
  dplyr::select(profileid,complete_birthyear_parents,complete_birthyear_focal) %>%
  group_by(profileid,complete_birthyear_focal) %>%
  summarize(nr_parents = n(),
            prop_parents_complete_birthyear = sum(complete_birthyear_parents)/nr_parents,
            parents_birthyear = sum(complete_birthyear_parents)) %>%
  mutate(parents_birth_year = ifelse(prop_parents_complete_birthyear>0,1,0))


# Negative binomial model with offset

model_parents_completeness_birthyear_offset = glm.nb(parents_birthyear~complete_birthyear_focal+offset(log(nr_parents)),
                                    data=data_parents_birthyear)


# Binomial model

model_parents_completeness_birthyear_binomial = glm(cbind(parents_birthyear,nr_parents)~complete_birthyear_focal,
                                       family=binomial("logit"),
                                       data=data_parents_birthyear)


# Logistic model with outcome (at least one relative with non-missing demographic variable)


model_parents_completeness_birthyear_logistic = glm(parents_birth_year~complete_birthyear_focal+nr_parents,data=data_parents_birthyear,family = "binomial")



data_parents_birthdate = parents_data_set  %>%
  filter(!(is.na(f) & is.na(m) & is.na(p_unknown))) %>%
  filter(!(is.na(f_death_year))|!(is.na(m_death_year)) | !(is.na(p_unknown_death_year))) %>%
  filter(!(is.na(birth_year))) %>%
  dplyr::select(profileid,f,m,p_unknown,m_birth_month,f_birth_month,p_unknown_birth_month) %>%
  pivot_longer(!c("profileid","f","m","p_unknown"),names_to="parents",values_to = "birth_month_parents") %>%
  filter(!(p_unknown==0 & parents=="p_unknown_birth_month"),
         !(f==0 & parents=="f_birth_month"),
         !(m==0 & parents=="m_birth_month")) %>%
  left_join(data_focal_birth_cohorts %>%
              filter(!(is.na(birth_year)),
                     birth_year>=1600,
                     birth_year<=1900) %>%
              ungroup() %>%
              dplyr::select(profileid,birth_month,birth_cohorts),by="profileid") %>%
  mutate(complete_birthmonth_parents = ifelse(!(is.na(birth_month_parents)),1,0),
         complete_birthmonth_focal = ifelse(!(is.na(birth_month)),1,0)
  )  %>%
  ungroup() %>%
  dplyr::select(profileid,complete_birthmonth_parents,complete_birthmonth_focal,birth_cohorts) %>%
  group_by(profileid,complete_birthmonth_focal,birth_cohorts) %>%
  summarize(nr_parents = n(),
            prop_parents_complete_birthdate = sum(complete_birthmonth_parents)/nr_parents,
            parents_birthdate = sum(complete_birthmonth_parents)) %>%
  mutate(parents_birth_date = ifelse(prop_parents_complete_birthdate>0,1,0))


# Negative binomial model with offset

model_parents_quality_birthdate_offset = glm.nb(parents_birthdate~complete_birthmonth_focal+birth_cohorts+offset(log(nr_parents)),
                               data=data_parents_birthdate)


# Binomial model

model_parents_quality_birthdate_binomial = glm(cbind(parents_birthdate,nr_parents)~complete_birthmonth_focal+birth_cohorts,
                                  family=binomial("logit"),
                                  data=data_parents_birthdate)

# Logistic model with outcome (at least one relative with non-missing demographic variable)

model_parents_quality_birthdate_logistic = glm(parents_birth_date~complete_birthmonth_focal+birth_cohorts+nr_parents,data=data_parents_birthdate,family = "binomial")


data_parents_birth_location = parents_data_set  %>%
  filter(!(is.na(f) & is.na(m) & is.na(p_unknown))) %>%
  dplyr::select(profileid,f,m,p_unknown,m_birth_country ,f_birth_country ,p_unknown_birth_country,country_birth_final ) %>%
  pivot_longer(!c("profileid","f","m","p_unknown","country_birth_final"),names_to="parents",values_to = "birth_country_parents") %>%
  filter(!(p_unknown==0 & parents=="p_unknown_birth_country"),
         !(f==0 & parents=="f_birth_country"),
         !(m==0 & parents=="m_birth_country")) %>%
  mutate(complete_country_birth_parents = ifelse(!(is.na(birth_country_parents)),1,0),
         complete_country_birth_focal = ifelse(!(is.na(country_birth_final)),1,0)
  )  %>%
  ungroup() %>%
  dplyr::select(profileid,complete_country_birth_parents,complete_country_birth_focal) %>%
  group_by(profileid,complete_country_birth_focal) %>%
  summarize(nr_parents = n(),
            prop_parents_complete_birthcountry = sum(complete_country_birth_parents)/nr_parents,
            parents_birthcountry = sum(complete_country_birth_parents)) %>%
  mutate(parents_complete_birthcountry = ifelse(prop_parents_complete_birthcountry>0,1,0))

# Negative binomial model with offset

model_parents_completeness_birthcountry_offset = glm.nb(parents_birthcountry~complete_country_birth_focal+offset(log(nr_parents)),
                                                 data=data_parents_birth_location)


# Binomial model


model_parents_completeness_birthcountry_binomial = glm(cbind(parents_birthcountry,nr_parents)~complete_country_birth_focal,
                                                    family=binomial("logit"),
                                                    data=data_parents_birth_location)



# Logistic model with outcome (at least one relative with non-missing demographic variable)

model_parents_completeness_birthcountry_logistic = glm(parents_complete_birthcountry~complete_country_birth_focal+nr_parents,data=data_parents_birth_location,family = "binomial")




data_parents_death_location = parents_data_set  %>%
  filter(!(is.na(f) & is.na(m) & is.na(p_unknown))) %>%
  dplyr::select(profileid,f,m,p_unknown,m_death_country ,f_death_country ,p_unknown_death_country,country_death_final ) %>%
  pivot_longer(!c("profileid","f","m","p_unknown","country_death_final"),names_to="parents",values_to = "death_country_parents") %>%
  filter(!(p_unknown==0 & parents=="p_unknown_death_country"),
         !(f==0 & parents=="f_death_country"),
         !(m==0 & parents=="m_death_country")) %>%
  mutate(complete_country_death_parents = ifelse(!(is.na(death_country_parents)),1,0),
         complete_country_death_focal = ifelse(!(is.na(country_death_final)),1,0))  %>%
  ungroup() %>%
  dplyr::select(profileid,complete_country_death_parents,complete_country_death_focal) %>%
  group_by(profileid,complete_country_death_focal) %>%
  summarize(nr_parents = n(),
            prop_parents_complete_deathcountry = sum(complete_country_death_parents)/nr_parents,
            parents_deathcountry = sum(complete_country_death_parents)) %>%
  mutate(parents_complete_deathcountry = ifelse(prop_parents_complete_deathcountry>0,1,0))

# Negative binomial model with offset

model_parents_completeness_deathcountry_offset = glm.nb(parents_deathcountry~complete_country_death_focal+offset(log(nr_parents)),
                                                data=data_parents_death_location)

# Binomial model

model_parents_completeness_deathcountry_binomial = glm(cbind(parents_deathcountry,nr_parents)~complete_country_death_focal,
                                                    family=binomial("logit"),
                                                    data=data_parents_death_location)


# Logistic model with outcome (at least one relative with non-missing demographic variable)

model_parents_completeness_deathcountry_logistic = glm(parents_complete_deathcountry~complete_country_death_focal+nr_parents,data=data_parents_death_location,family = "binomial")


data_parents_deathyear = parents_data_set  %>%
  filter(!(is.na(f) & is.na(m) & is.na(p_unknown))) %>%
  dplyr::select(profileid,f,m,p_unknown,death_year,m_death_year,f_death_year,p_unknown_death_year) %>%
  pivot_longer(!c("profileid","f","m","p_unknown","death_year"),names_to="parents",values_to = "death_year_parents") %>%
  filter(!(p_unknown==0 & parents=="p_unknown_death_year"),
         !(f==0 & parents=="f_death_year"),
         !(m==0 & parents=="m_death_year")) %>%
  mutate(complete_deathyear_parents = ifelse(!(is.na(death_year_parents)),1,0),
         complete_deathyear_focal = ifelse(!(is.na(death_year)),1,0))  %>%
  ungroup() %>%
  dplyr::select(profileid,complete_deathyear_parents,complete_deathyear_focal) %>%
  group_by(profileid,complete_deathyear_focal) %>%
  summarize(nr_parents = n(),
            prop_parents_complete_deathyear = sum(complete_deathyear_parents)/nr_parents,
            parents_deathyear = sum(complete_deathyear_parents)) %>%
  mutate(parents_death_year = ifelse(prop_parents_complete_deathyear>0,1,0))

# Negative binomial model with offset

model_parents_completeness_deathyear_offset = glm.nb(parents_deathyear~complete_deathyear_focal+offset(log(nr_parents)),
                                          data=data_parents_deathyear)


# Binomial model

model_parents_completeness_deathyear_binomial = glm(cbind(parents_deathyear,nr_parents)~complete_deathyear_focal,
                                             family=binomial("logit"),
                                             data=data_parents_deathyear)


# Logistic model with outcome (at least one relative with non-missing demographic variable)

model_parents_completeness_deathyear_logistic = glm(parents_death_year~complete_deathyear_focal+nr_parents,data=data_parents_deathyear,family = "binomial")



data_parents_deathdate = parents_data_set %>%
  filter(!(is.na(f) & is.na(m) & is.na(p_unknown))) %>%
  filter(!(is.na(f_death_year))|!(is.na(m_death_year)) | !(is.na(p_unknown_death_year))) %>%
  dplyr::select(profileid,f,m,p_unknown,m_death_month,f_death_month,p_unknown_death_month) %>%
  pivot_longer(!c("profileid","f","m","p_unknown"),names_to="parents",values_to = "death_month_parents") %>%
  filter(!(p_unknown==0 & parents=="p_unknown_death_month"),
         !(f==0 & parents=="f_death_month"),
         !(m==0 & parents=="m_death_month")) %>%
  left_join(indicator_demo_deaths %>%
              filter(!(is.na(death_year)),
                     death_year>=1600,
                     death_year<=1900) %>%
              ungroup() %>%
              dplyr::select(profileid,death_month,death_cohorts),by="profileid") %>%
  filter(!(is.na(death_cohorts))) %>%
  mutate(complete_deathmonth_parents = ifelse(!(is.na(death_month_parents)),1,0),
         complete_deathmonth_focal = ifelse(!(is.na(death_month)),1,0)
  )  %>%
  ungroup() %>%
  dplyr::select(profileid,complete_deathmonth_parents,complete_deathmonth_focal,death_cohorts) %>%
  group_by(profileid,complete_deathmonth_focal,death_cohorts) %>%
  summarize(nr_parents = n(),
            prop_parents_complete_deathdate = sum(complete_deathmonth_parents)/nr_parents,
            parents_deathdate = sum(complete_deathmonth_parents)) %>%
  mutate(parents_death_date = ifelse(prop_parents_complete_deathdate>0,1,0))


# Negative binomial model with offset

model_parents_quality_deathdate_offset = glm.nb(parents_deathdate~complete_deathmonth_focal+death_cohorts+offset(log(nr_parents)),
                                     data=data_parents_deathdate)


# Binomial model


model_parents_quality_deathdate_binomial = glm(cbind(parents_deathdate,nr_parents)~complete_deathmonth_focal+death_cohorts,
                                                     family=binomial("logit"),
                                                     data=data_parents_deathdate)


# Logistic model with outcome (at least one relative with non-missing demographic variable)

model_parents_quality_deathdate_logistic = glm(parents_death_date~complete_deathmonth_focal+death_cohorts+nr_parents,data=data_parents_deathdate,family = "binomial")



model_parents_completeness_birthyear_offset = coef(summary(model_parents_completeness_birthyear_offset))
model_parents_completeness_birthyear_binomial = coef(summary(model_parents_completeness_birthyear_binomial))
model_parents_completeness_birthyear_logistic = coef(summary(model_parents_completeness_birthyear_logistic))
model_parents_completeness_deathyear_offset = coef(summary(model_parents_completeness_deathyear_offset))
model_parents_completeness_deathyear_binomial = coef(summary(model_parents_completeness_deathyear_binomial))
model_parents_completeness_deathyear_logistic = coef(summary(model_parents_completeness_deathyear_logistic))
model_parents_completeness_birthcountry_offset = coef(summary(model_parents_completeness_birthcountry_offset))
model_parents_completeness_birthcountry_binomial = coef(summary(model_parents_completeness_birthcountry_binomial))
model_parents_completeness_birthcountry_logistic = coef(summary(model_parents_completeness_birthcountry_logistic))
model_parents_completeness_deathcountry_offset = coef(summary(model_parents_completeness_deathcountry_offset))
model_parents_completeness_deathcountry_binomial = coef(summary(model_parents_completeness_deathcountry_binomial))
model_parents_completeness_deathcountry_logistic = coef(summary(model_parents_completeness_deathcountry_logistic))
model_parents_quality_birthdate_offset = coef(summary(model_parents_quality_birthdate_offset))
model_parents_quality_birthdate_binomial = coef(summary(model_parents_quality_birthdate_binomial))
model_parents_quality_birthdate_logistic = coef(summary(model_parents_quality_birthdate_logistic))
model_parents_quality_deathdate_offset = coef(summary(model_parents_quality_deathdate_offset))
model_parents_quality_deathdate_binomial = coef(summary(model_parents_quality_deathdate_binomial))
model_parents_quality_deathdate_logistic = coef(summary(model_parents_quality_deathdate_logistic))


save(model_parents_completeness_birthyear_offset,model_parents_completeness_deathyear_offset,
     model_parents_completeness_birthcountry_offset,model_parents_completeness_deathcountry_offset,
     model_parents_quality_birthdate_offset,model_parents_quality_deathdate_offset,
     model_parents_completeness_birthyear_logistic,model_parents_completeness_deathyear_logistic,
     model_parents_completeness_birthcountry_logistic,model_parents_completeness_deathcountry_logistic,
     model_parents_quality_birthdate_logistic,model_parents_quality_deathdate_logistic,
     model_parents_completeness_birthyear_binomial,model_parents_completeness_deathyear_binomial,
     model_parents_completeness_birthcountry_binomial,model_parents_completeness_deathcountry_binomial,
     model_parents_quality_birthdate_binomial,model_parents_quality_deathdate_binomial,
     file="Results/model_parents_sensitivity_analyses.RData")


#### Children ####

load('Cleaned_Datasets/children_data_set.RData')

data_child_birthyear = children_data_set %>%
  filter(!(is.na(child_id))) %>%
  mutate(complete_birthyear_child = ifelse(!(is.na(birth_year_child)),1,0),
         complete_birthyear_focal = ifelse(!(is.na(birth_year)),1,0)) %>%
  ungroup() %>%
  dplyr::select(profileid,complete_birthyear_child,complete_birthyear_focal) %>%
  group_by(profileid,complete_birthyear_focal) %>%
  summarize(nr_children = n(),
            prop_child_complete_birthyear = sum(complete_birthyear_child)/nr_children,
            children_birthyear=sum(complete_birthyear_child)) %>%
  mutate(child_birth_year = ifelse(prop_child_complete_birthyear>0,1,0)) %>%
  filter(nr_children<=20)

# Negative binomial model with offset


model_children_completeness_birthyear_offset = glm.nb(children_birthyear~complete_birthyear_focal+offset(log(nr_children)),
                                     data=data_child_birthyear)


# Binomial model

model_children_completeness_birthyear_binomial = glm(cbind(children_birthyear,nr_children)~complete_birthyear_focal,
                                        family=binomial('logit'),
                                        data=data_child_birthyear)


# Logistic model with outcome (at least one relative with non-missing demographic variable)


model_children_completeness_birthyear_logistic = glm(child_birth_year~complete_birthyear_focal+nr_children,data=data_child_birthyear,family = "binomial")




data_child_birthdate = children_data_set %>%
  filter(!(is.na(child_id)),
         !(is.na(birth_year)),
         !(is.na(birth_year_child))) %>%
  left_join(data_focal_birth_cohorts %>%
              filter(!(is.na(birth_year)),
                     birth_year>=1600,
                     birth_year<=1900) %>%
              ungroup() %>%
              dplyr::select(profileid,birth_day,birth_cohorts),by="profileid") %>%
  mutate(complete_birthdate_child = ifelse(!(is.na(birth_month_child)),1,0),
         complete_birthdate_focal = ifelse(!(is.na(birth_month)),1,0)
  )  %>%
  ungroup() %>%
  filter(!(is.na(birth_cohorts))) %>%
  dplyr::select(profileid,complete_birthdate_child,complete_birthdate_focal,birth_cohorts) %>%
  group_by(profileid,complete_birthdate_focal,birth_cohorts) %>%
  summarize(nr_children = n(),
            prop_child_complete_birthdate = sum(complete_birthdate_child)/nr_children,
            children_birthdate=sum(complete_birthdate_child)) %>%
  mutate(child_birth_date = ifelse(prop_child_complete_birthdate>0,1,0)) %>%
  filter(nr_children<=20)

# Negative binomial model with offset

model_children_quality_birthdate_offset = glm.nb(children_birthdate~complete_birthdate_focal+birth_cohorts+offset(log(nr_children)),data=data_child_birthdate)


# Binomial model

model_children_quality_birthdate_binomial = glm(cbind(children_birthdate,nr_children)~complete_birthdate_focal+birth_cohorts,
                                   family=binomial('logit'),
                                   data=data_child_birthdate)


# Logistic model with outcome (at least one relative with non-missing demographic variable)


model_children_quality_birthdate_logistic = glm(child_birth_date~complete_birthdate_focal+birth_cohorts+nr_children,data=data_child_birthdate,family = "binomial")


data_child_birthcountry = children_data_set %>%
  filter(!(is.na(child_id))) %>%
  mutate(complete_birthcountry_child = ifelse(!(is.na(birth_country_child)),1,0),
         complete_birthcountry_focal = ifelse(!(is.na(country_birth_final)),1,0)
  ) %>%
  ungroup() %>%
  dplyr::select(profileid,complete_birthcountry_child,complete_birthcountry_focal) %>%
  group_by(profileid,complete_birthcountry_focal) %>%
  summarize(nr_children = n(),
            prop_child_complete_birthcountry = sum(complete_birthcountry_child)/nr_children,
            children_birthcountry = sum(complete_birthcountry_child)) %>%
  mutate(child_complete_birthcountry = ifelse(prop_child_complete_birthcountry>0,1,0)) %>%
  filter(nr_children<=20)

# Negative binomial model with offset

model_children_completeness_birthcountry_offset = glm.nb(children_birthcountry~complete_birthcountry_focal+offset(log(nr_children)),data=data_child_birthcountry)


# Binomial model


model_children_completeness_birthcountry_binomial = glm(cbind(children_birthcountry,nr_children)~complete_birthcountry_focal,
                                                     family=binomial('logit'),
                                                     data=data_child_birthcountry)

# Logistic model with outcome (at least one relative with non-missing demographic variable)

model_children_completeness_birthcountry_logistic = glm(child_complete_birthcountry~complete_birthcountry_focal+nr_children,data=data_child_birthcountry,family = "binomial")



data_child_deathcountry = children_data_set %>%
  filter(!(is.na(child_id))) %>%
  mutate(complete_deathcountry_child = ifelse(!(is.na(death_country_child)),1,0),
         complete_deathcountry_focal = ifelse(!(is.na(country_death_final)),1,0)) %>%
  ungroup() %>%
  dplyr::select(profileid,complete_deathcountry_child,complete_deathcountry_focal) %>%
  group_by(profileid,complete_deathcountry_focal) %>%
  summarize(nr_children = n(),
            prop_child_complete_deathcountry = sum(complete_deathcountry_child)/nr_children,
            children_deathcountry = sum(complete_deathcountry_child)) %>%
  mutate(child_complete_deathcountry = ifelse(prop_child_complete_deathcountry>0,1,0)) %>%
  filter(nr_children<=20)

# Negative binomial model with offset

model_children_completeness_deathcountry_offset = glm.nb(children_deathcountry~complete_deathcountry_focal+offset(log(nr_children)),data=data_child_deathcountry)


# Binomial model

model_children_completeness_deathcountry_binomial = glm(cbind(children_deathcountry,nr_children)~complete_deathcountry_focal,
                                                     family=binomial('logit'),
                                                     data=data_child_deathcountry)

# Logistic model with outcome (at least one relative with non-missing demographic variable)

model_children_completeness_deathcountry_logistic = glm(child_complete_deathcountry~complete_deathcountry_focal+nr_children,data=data_child_deathcountry,family = "binomial")



data_child_deathdate = children_data_set %>%
  filter(!(is.na(child_id)),
         !(is.na(death_year)),
         !(is.na(death_year_child))) %>%
  left_join(data_focal_death_cohorts %>%
              filter(!(is.na(death_year)),
                     death_year>=1600,
                     death_year<=1900) %>%
              ungroup() %>%
              dplyr::select(profileid,death_day,death_cohorts),by="profileid") %>%
  mutate(complete_deathdate_child = ifelse(!(is.na(death_month_child)),1,0),
         complete_deathdate_focal = ifelse(!(is.na(death_month)),1,0))  %>%
  ungroup() %>%
  dplyr::select(profileid,complete_deathdate_child,complete_deathdate_focal,death_cohorts) %>%
  group_by(profileid,complete_deathdate_focal,death_cohorts) %>%
  filter(!(is.na(death_cohorts))) %>%
  summarize(nr_children = n(),
            prop_child_complete_deathdate = sum(complete_deathdate_child)/nr_children,
            children_deathdate = sum(complete_deathdate_child)) %>%
  mutate(child_death_date = ifelse(prop_child_complete_deathdate>0,1,0)) %>%
  filter(nr_children<=20)

# Negative binomial model with offset

model_children_quality_deathdate_offset = glm.nb(children_deathdate~complete_deathdate_focal+death_cohorts+offset(log(nr_children)),data=data_child_deathdate)


# Binomial model

model_children_quality_deathdate_binomial = glm(cbind(children_deathdate,nr_children)~complete_deathdate_focal+death_cohorts,
                                         family=binomial('logit'),
                                         data=data_child_deathdate)
# Logistic model with outcome (at least one relative with non-missing demographic variable)
model_children_quality_deathdate_logistic = glm(child_death_date~complete_deathdate_focal+death_cohorts+nr_children,data=data_child_deathdate,family = "binomial")


data_child_deathyear = children_data_set %>%
  filter(!(is.na(child_id))) %>%
  mutate(complete_deathyear_child = ifelse(!(is.na(death_year_child)),1,0),
         complete_deathyear_focal = ifelse(!(is.na(death_year)),1,0)
  ) %>%
  ungroup() %>%
  dplyr::select(profileid,complete_deathyear_child,complete_deathyear_focal) %>%
  group_by(profileid,complete_deathyear_focal) %>%
  summarize(nr_children = n(),
            prop_child_complete_deathyear = sum(complete_deathyear_child)/nr_children,
            children_deathyear = sum(complete_deathyear_child)) %>%
  mutate(child_death_year = ifelse(prop_child_complete_deathyear>0,1,0)) %>%
  filter(nr_children<=20)

# Negative binomial model with offset

model_children_completeness_deathyear_offset = glm.nb(children_deathyear~complete_deathyear_focal+offset(log(nr_children)),data=data_child_deathyear)

# Binomial model

model_children_completeness_deathyear_binomial = glm(cbind(children_deathyear,nr_children)~complete_deathyear_focal,
                                              family=binomial('logit'),
                                              data=data_child_deathyear)

# Logistic model with outcome (at least one relative with non-missing demographic variable)

model_children_completeness_deathyear_logistic = glm(child_death_year~complete_deathyear_focal+nr_children,data=data_child_deathyear,family = "binomial")



model_children_completeness_birthyear_offset = coef(summary(model_children_completeness_birthyear_offset))
model_children_completeness_birthyear_binomial = coef(summary(model_children_completeness_binomial))
model_children_completeness_birthyear_logistic = coef(summary(model_children_completeness_logistic))
model_children_completeness_deathyear_offset = coef(summary(model_children_completeness_death_offset))
model_children_completeness_deathyear_binomial = coef(summary(model_children_completeness_death_binomial))
model_children_completeness_deathyear_logistic = coef(summary(model_children_completeness_death_logistic))
model_children_completeness_birthcountry_offset = coef(summary(model_children_completeness_birthcountry_offset))
model_children_completeness_birthcountry_binomial  = coef(summary(model_children_completeness_birthcountry_binomial))
model_children_completeness_birthcountry_logistic  = coef(summary(model_children_completeness_birthcountry_logistic))
model_children_completeness_deathcountry_offset = coef(summary(model_children_completeness_deathcountry_offset))
model_children_completeness_deathcountry_binomial = coef(summary(model_children_completeness_deathcountry_binomial))
model_children_completeness_deathcountry_logistic = coef(summary(model_children_completeness_deathcountry_logistic))
model_children_quality_birthdate_offset = coef(summary(model_children_quality_birthdate_offset))
model_children_quality_birthdate_binomial = coef(summary(model_children_quality_birthdate_binomial))
model_children_quality_birthdate_logistic = coef(summary(model_children_quality_birthdate_logistic))
model_children_quality_deathdate_offset = coef(summary(model_children_quality_deathdate_offset))
model_children_quality_deathdate_binomial = coef(summary(model_children_quality_deathdate_binomial))
model_children_quality_deathdate_logistic = coef(summary(model_children_quality_deathdate_logistic))



save(model_children_completeness_birthyear_offset,model_children_completeness_deathyear_offset,
     model_children_completeness_birthcountry_offset,model_children_completeness_deathcountry_offset,
     model_children_quality_birthdate_offset,model_children_quality_deathdate_offset,
     model_children_completeness_birthyear_binomial,model_children_completeness_deathyear_binomial,
     model_children_completeness_birthcountry_binomial,model_children_completeness_deathcountry_binomial,
     model_children_quality_birthdate_binomial,model_children_quality_deathdate_binomial,
     model_children_completeness_birthyear_logistic,model_children_completeness_deathyear_logistic,
     model_children_completeness_birthcountry_logistic,model_children_completeness_deathcountry_logistic,
     model_children_quality_birthdate_logistic,model_children_quality_deathdate_logistic,
     file="Results/model_children_sensitivity_analyses.RData")

#### Grandchildren ####

#### load the data

load("Cleaned_Datasets/grandchildren_data_set.RData")


data_grandchild_birthyear = grandchildren_data_set %>%
  filter(!(is.na(grandchild_id))) %>%
  mutate(complete_birthyear_grandchild = ifelse(!(is.na(birth_year_grandchild)),1,0),
         complete_birthyear_focal = ifelse(!(is.na(birth_year)),1,0)
  ) %>%
  ungroup() %>%
  dplyr::select(profileid,complete_birthyear_grandchild,complete_birthyear_focal) %>%
  group_by(profileid,complete_birthyear_focal) %>%
  summarize(nr_grandchildren = n(),
            prop_grandchild_complete_birthyear = sum(complete_birthyear_grandchild)/nr_grandchildren,
            grandchild_birthyear = sum(complete_birthyear_grandchild)) %>%
  mutate(grandchild_birth_year = ifelse(prop_grandchild_complete_birthyear>0,1,0))

# Negative binomial model with offset

model_grandchild_completeness_birthyear_offset = glm.nb(grandchild_birthyear~complete_birthyear_focal+offset(log(nr_grandchildren)),
                                             data=data_grandchild_birthyear)

# Binomial model

model_grandchild_completeness_birthyear_binomial = glm(cbind(grandchild_birthyear,nr_grandchildren)~complete_birthyear_focal,
                                               family=binomial('logit'),
                                               data=data_grandchild_birthyear)

# Logistic model with outcome (at least one relative with non-missing demographic variable)
model_grandchild_completeness_birthyear_logistic = glm(grandchild_birth_year~complete_birthyear_focal+nr_grandchildren,data=data_grandchild_birthyear,family = "binomial")


data_grandchild_deathyear = grandchildren_data_set %>%
  filter(!(is.na(grandchild_id))) %>%
  mutate(complete_deathyear_grandchild = ifelse(!(is.na(death_year_grandchild)),1,0),
         complete_deathyear_focal = ifelse(!(is.na(death_year)),1,0)) %>%
  ungroup() %>%
  dplyr::select(profileid,complete_deathyear_grandchild,complete_deathyear_focal) %>%
  group_by(profileid,complete_deathyear_focal) %>%
  summarize(nr_grandchildren = n(),
            prop_grandchild_complete_deathyear = sum(complete_deathyear_grandchild)/nr_grandchildren,
            grandchild_deathyear = sum(complete_deathyear_grandchild)) %>%
  mutate(grandchild_death_year = ifelse(prop_grandchild_complete_deathyear>0,1,0))

# Negative binomial model with offset
model_grandchild_completeness_deathyear_offset = glm.nb(grandchild_deathyear~complete_deathyear_focal+offset(log(nr_grandchildren)),
                                             data=data_grandchild_deathyear)

# Binomial model

model_grandchild_completeness_deathyear_binomial = glm(cbind(grandchild_deathyear,nr_grandchildren)~complete_deathyear_focal,
                                               family=binomial('logit'),
                                               data=data_grandchild_deathyear)

# Logistic model with outcome (at least one relative with non-missing demographic variable)

model_grandchild_completeness_deathyear_logistic = glm(grandchild_death_year~complete_deathyear_focal+nr_grandchildren,data=data_grandchild_deathyear,family = "binomial")





data_grandchild_birthcountry = grandchildren_data_set %>%
  filter(!(is.na(grandchild_id))) %>%
  mutate(complete_birthcountry_grandchild = ifelse(!(is.na(country_birth_grandchild)),1,0),
         complete_birthcountry_focal = ifelse(!(is.na(birth_country)),1,0)
  ) %>%
  ungroup() %>%
  dplyr::select(profileid,complete_birthcountry_grandchild,complete_birthcountry_focal) %>%
  group_by(profileid,complete_birthcountry_focal) %>%
  summarize(nr_grandchildren = n(),
            prop_grandchild_complete_birthcountry = sum(complete_birthcountry_grandchild)/nr_grandchildren,
            grandchild_birthcountry = sum(complete_birthcountry_grandchild)) %>%
  mutate(grandchild_complete_birthcountry = ifelse(prop_grandchild_complete_birthcountry>0,1,0))

# Negative binomial model with offset

model_grandchild_completeness_birthcountry_offset = glm.nb(grandchild_birthcountry~complete_birthcountry_focal+offset(log(nr_grandchildren)),
                                                    data=data_grandchild_birthcountry)

# Binomial model

model_grandchild_completeness_birthcountry_binomial = glm(cbind(grandchild_birthcountry,nr_grandchildren)~complete_birthcountry_focal,
                                                      family=binomial('logit'),
                                                      data=data_grandchild_birthcountry)

# Logistic model with outcome (at least one relative with non-missing demographic variable)


model_grandchild_completeness_birthcountry_logistic = glm(grandchild_complete_birthcountry~complete_birthcountry_focal+nr_grandchildren,
                                                 data=data_grandchild_birthcountry,
                                                 family = "binomial")



data_grandchild_deathcountry = grandchildren_data_set %>%
  filter(!(is.na(grandchild_id))) %>%
  mutate(complete_deathcountry_grandchild = ifelse(!(is.na(country_death_grandchild)),1,0),
         complete_deathcountry_focal = ifelse(!(is.na(death_country)),1,0)) %>%
  ungroup() %>%
  dplyr::select(profileid,complete_deathcountry_grandchild,complete_deathcountry_focal) %>%
  group_by(profileid,complete_deathcountry_focal) %>%
  summarize(nr_grandchildren = n(),
            prop_grandchild_complete_deathcountry = sum(complete_deathcountry_grandchild)/nr_grandchildren,
            grandchild_deathcountry = sum(complete_deathcountry_grandchild)) %>%
  mutate(grandchild_complete_deathcountry = ifelse(prop_grandchild_complete_deathcountry>0,1,0))

# Negative binomial model with offset
model_grandchild_completeness_deathcountry_offset = glm.nb(grandchild_deathcountry~complete_deathcountry_focal+offset(log(nr_grandchildren)),
                                                    data=data_grandchild_deathcountry)

# Binomial model

model_grandchild_completeness_deathcountry_binomial = glm(cbind(grandchild_deathcountry,nr_grandchildren)~complete_deathcountry_focal,
                                                          family=binomial('logit'),
                                                          data=data_grandchild_deathcountry)

# Logistic model with outcome (at least one relative with non-missing demographic variable)


model_grandchild_completeness_deathcountry_logistic = glm(grandchild_complete_deathcountry~complete_deathcountry_focal+nr_grandchildren,
                                                 data=data_grandchild_deathcountry,
                                                 family = "binomial")


data_grandchild_birthdate = grandchildren_data_set %>%
  filter(!(is.na(grandchild_id)),
         !(is.na(birth_year)),
         !(is.na(birth_year_grandchild)),
         birth_year>=1600,
         birth_year<=1900) %>%
  mutate(birth_cohorts = case_when(birth_year>=1600 & birth_year<1625 ~ "1600-1624",
                                   birth_year>=1625 & birth_year<1650 ~ "1625-1649",
                                   birth_year>=1650 & birth_year<1675 ~ "1650-1674",
                                   birth_year>=1675 & birth_year<1700 ~ "1675-1699",
                                   birth_year>=1700 & birth_year<1725 ~ "1700-1724",
                                   birth_year>=1725 & birth_year<1750 ~ "1725-1749",
                                   birth_year>=1750 & birth_year<1775 ~ "1750-1774",
                                   birth_year>=1775 & birth_year<1800 ~ "1775-1799",
                                   birth_year>=1800 & birth_year<1825 ~ "1800-1824",
                                   birth_year>=1825 & birth_year<1850 ~ "1825-1849",
                                   birth_year>=1850 & birth_year<1875 ~ "1850-1874",
                                   birth_year>=1875 & birth_year<=1900 ~ "1875-1900",
                                   TRUE ~ NA)) %>%
  mutate(birth_cohorts = factor(birth_cohorts,levels = c("1600-1624","1625-1649","1650-1674",
                                                         "1675-1699","1700-1724","1725-1749",
                                                         "1750-1774","1775-1799","1800-1824",
                                                         "1825-1849","1850-1874","1875-1900"))) %>%
  mutate(complete_birthdate_grandchild = ifelse(!(is.na(birth_month_grandchild)),1,0),
         complete_birthdate_focal = ifelse(!(is.na(birth_month)),1,0))  %>%
  ungroup() %>%
  dplyr::select(profileid,complete_birthdate_grandchild,complete_birthdate_focal,birth_cohorts) %>%
  group_by(profileid,complete_birthdate_focal,birth_cohorts) %>%
  summarize(nr_grandchildren = n(),
            prop_grandchild_complete_birthdate = sum(complete_birthdate_grandchild)/nr_grandchildren,
            grandchild_birthdate = sum(complete_birthdate_grandchild)) %>%
  mutate(grandchild_birth_date = ifelse(prop_grandchild_complete_birthdate>0,1,0))

# Negative binomial model with offset
model_grandchild_quality_birthdate_offset = glm.nb(grandchild_birthdate~complete_birthdate_focal+birth_cohorts+offset(log(nr_grandchildren)),
                                            data=data_grandchild_birthdate)


# Binomial model

model_grandchild_quality_birthdate_binomial = glm(cbind(grandchild_birthdate,nr_grandchildren)~complete_birthdate_focal+birth_cohorts,
                                              family=binomial('logit'),
                                              data=data_grandchild_birthdate)


# Logistic model with outcome (at least one relative with non-missing demographic variable)

model_grandchild_quality_birthdate_logistic = glm(grandchild_birth_date~complete_birthdate_focal+birth_cohorts+nr_grandchildren,data=data_grandchild_birthdate,family = "binomial")


data_grandchild_deathdate = grandchildren_data_set %>%
  filter(!(is.na(grandchild_id)),
         !(is.na(death_year)),
         !(is.na(death_year_grandchild)),
         death_year>=1600,
         death_year<=1900) %>%
  mutate(death_cohorts = case_when(death_year>=1600 & death_year<1625 ~ "1600-1624",
                                   death_year>=1625 & death_year<1650 ~ "1625-1649",
                                   death_year>=1650 & death_year<1675 ~ "1650-1674",
                                   death_year>=1675 & death_year<1700 ~ "1675-1699",
                                   death_year>=1700 & death_year<1725 ~ "1700-1724",
                                   death_year>=1725 & death_year<1750 ~ "1725-1749",
                                   death_year>=1750 & death_year<1775 ~ "1750-1774",
                                   death_year>=1775 & death_year<1800 ~ "1775-1799",
                                   death_year>=1800 & death_year<1825 ~ "1800-1824",
                                   death_year>=1825 & death_year<1850 ~ "1825-1849",
                                   death_year>=1850 & death_year<1875 ~ "1850-1874",
                                   death_year>=1875 & death_year<=1900 ~ "1875-1900",
                                   TRUE ~ NA)) %>%
  mutate(death_cohorts = factor(death_cohorts,levels = c("1600-1624","1625-1649","1650-1674",
                                                         "1675-1699","1700-1724","1725-1749",
                                                         "1750-1774","1775-1799","1800-1824",
                                                         "1825-1849","1850-1874","1875-1900"))) %>%
  mutate(complete_deathdate_grandchild = ifelse(!(is.na(death_month_grandchild)),1,0),
         complete_deathdate_focal = ifelse(!(is.na(death_month)),1,0)
  )  %>%
  ungroup() %>%
  dplyr::select(profileid,complete_deathdate_grandchild,complete_deathdate_focal,death_cohorts) %>%
  group_by(profileid,complete_deathdate_focal,death_cohorts) %>%
  summarize(nr_grandchildren = n(),
            prop_grandchild_complete_deathdate = sum(complete_deathdate_grandchild)/nr_grandchildren,
            grandchild_deathdate = sum(complete_deathdate_grandchild)) %>%
  mutate(grandchild_death_date = ifelse(prop_grandchild_complete_deathdate>0,1,0))

# Negative binomial model with offset
model_grandchild_quality_deathdate_offset = glm.nb(grandchild_deathdate~complete_deathdate_focal+death_cohorts+offset(log(nr_grandchildren)),
                                            data=data_grandchild_deathdate)

# Binomial model

model_grandchild_quality_deathdate_binomial = glm(cbind(grandchild_deathdate,nr_grandchildren)~complete_deathdate_focal+death_cohorts,
                                              family=binomial('logit'),
                                              data=data_grandchild_deathdate)


# Logistic model with outcome (at least one relative with non-missing demographic variable)


model_grandchild_quality_deathdate_logistic = glm(grandchild_death_date~complete_deathdate_focal+death_cohorts+nr_grandchildren,data=data_grandchild_deathdate,family = "binomial")



model_grandchild_completeness_birthyear_offset = coef(summary(model_grandchild_completeness_birthyear_offset))
model_grandchild_completeness_birthyear_binomial = coef(summary(model_grandchild_completeness_birthyear_binomial))
model_grandchild_completeness_birthyear_logistic = coef(summary(model_grandchild_completeness_birthyear_logistic))
model_grandchild_completeness_deathyear_offset = coef(summary(model_grandchild_completeness_deathyear_offset))
model_grandchild_completeness_deathyear_binomial = coef(summary(model_grandchild_completeness_deathyear_binomial))
model_grandchild_completeness_deathyear_logistic = coef(summary(model_grandchild_completeness_deathyear_logistic))
model_grandchild_completeness_birthcountry_offset = coef(summary(model_grandchild_completeness_birthcountry_offset))
model_grandchild_completeness_birthcountry_binomial = coef(summary(model_grandchild_completeness_birthcountry_binomial))
model_grandchild_completeness_birthcountry_logistic = coef(summary(model_grandchild_completeness_birthcountry_logistic))
model_grandchild_completeness_deathcountry_offset = coef(summary(model_grandchild_completeness_deathcountry_offset))
model_grandchild_completeness_deathcountry_binomial = coef(summary(model_grandchild_completeness_deathcountry_binomial))
model_grandchild_completeness_deathcountry_logistic = coef(summary(model_grandchild_completeness_deathcountry_logistic))
model_grandchild_quality_birthdate_offset = coef(summary(model_grandchild_quality_birthdate_offset))
model_grandchild_quality_birthdate_binomial = coef(summary(model_grandchild_quality_birthdate_binomial))
model_grandchild_quality_birthdate_logistic = coef(summary(model_grandchild_quality_birthdate_logistic))
model_grandchild_quality_deathdate_offset = coef(summary(model_grandchild_quality_deathdate_offset))
model_grandchild_quality_deathdate_binomial = coef(summary(model_grandchild_quality_deathdate_binomial))
model_grandchild_quality_deathdate_logistic = coef(summary(model_grandchild_quality_deathdate_logistic))

save(model_grandchild_completeness_birthyear_offset,
     model_grandchild_completeness_deathyear_offset,
     model_grandchild_completeness_birthcountry_offset,
     model_grandchild_completeness_deathcountry_offset,
     model_grandchild_quality_deathdate_offset,
     model_grandchild_quality_birthdate_offset,
     model_grandchild_completeness_birthyear_binomial,
     model_grandchild_completeness_deathyear_binomial,
     model_grandchild_completeness_birthcountry_binomial,
     model_grandchild_completeness_deathcountry_binomial,
     model_grandchild_quality_deathdate_binomial,
     model_grandchild_quality_birthdate_binomial,
     model_grandchild_completeness_birthyear_logistic,
     model_grandchild_completeness_deathyear_logistic,
     model_grandchild_completeness_birthcountry_logistic,
     model_grandchild_completeness_deathcountry_logistic,
     model_grandchild_quality_deathdate_logistic,
     model_grandchild_quality_birthdate_logistic,
     file="Results/model_grandchild_sensitivity_analyses.RData")



#### Siblings ####

load("data_siblings_nolist.RData")

data_siblings_birthyear = data_siblings_nolist %>%
  filter(!(is.na(siblings_id))) %>%
  mutate(complete_birthyear_siblings = ifelse(!(is.na(birth_year_siblings)),1,0),
         complete_birthyear_focal = ifelse(!(is.na(birth_year)),1,0)
  ) %>%
  ungroup() %>%
  dplyr::select(profileid,complete_birthyear_siblings,complete_birthyear_focal) %>%
  group_by(profileid,complete_birthyear_focal) %>%
  summarize(nr_siblings = n(),
            prop_siblings_complete_birthyear = sum(complete_birthyear_siblings)/nr_siblings,
            siblings_birthyear = sum(complete_birthyear_siblings)) %>%
  mutate(siblings_birth_year = ifelse(prop_siblings_complete_birthyear>0,1,0))


# Negative binomial model with offset
model_siblings_completeness_birthyear_offset = glm.nb(siblings_birthyear~complete_birthyear_focal+offset(log(nr_siblings)),
                                     data=data_siblings_birthyear)


# Binomial model

model_siblings_completeness_birthyear_binomial = glm(cbind(siblings_birthyear,nr_siblings)~complete_birthyear_focal,
                                       family=binomial('logit'),
                                       data=data_siblings_birthyear)


# Logistic model with outcome (at least one relative with non-missing demographic variable)

model_siblings_completeness_birthyear_logistic = glm(siblings_birth_year~complete_birthyear_focal+nr_siblings,data=data_siblings_birthyear,family = "binomial")




data_siblings_birthdate = data_siblings_nolist %>%
  filter(!(is.na(siblings_id)),
         !(is.na(birth_year_siblings))) %>%
  left_join(data_focal_birth_cohorts %>%
              filter(!(is.na(birth_year)),
                     birth_year>=1600,
                     birth_year<=1900) %>%
              ungroup() %>%
              dplyr::select(profileid,birth_month,birth_cohorts),by="profileid") %>%
  mutate(complete_birthdate_siblings = ifelse(!(is.na(birth_month_siblings)),1,0),
         complete_birthdate_focal = ifelse(!(is.na(birth_month)),1,0))  %>%
  filter(!(is.na(birth_year_siblings))) %>%
  ungroup() %>%
  dplyr::select(profileid,complete_birthdate_siblings,complete_birthdate_focal,birth_cohorts) %>%
  filter(!(is.na(birth_cohorts))) %>%
  group_by(profileid,complete_birthdate_focal,birth_cohorts) %>%
  summarize(nr_siblings = n(),
            prop_siblings_complete_birthdate = sum(complete_birthdate_siblings)/nr_siblings,
            siblings_birthdate = sum(complete_birthdate_siblings)) %>%
  mutate(siblings_birth_date = ifelse(prop_siblings_complete_birthdate>0,1,0))

# Negative binomial model with offset
model_siblings_quality_birthdate_offset = glm.nb(siblings_birthdate~complete_birthdate_focal+birth_cohorts+offset(log(nr_siblings)),
                                data=data_siblings_birthdate)

# Binomial model

model_siblings_quality_birthdate_binomial = glm(cbind(siblings_birthdate,nr_siblings)~complete_birthdate_focal+birth_cohorts,
                                               family=binomial('logit'),
                                               data=data_siblings_birthdate)

# Logistic model with outcome (at least one relative with non-missing demographic variable)

model_siblings_quality_birthdate_logistic = glm(siblings_birth_date~complete_birthdate_focal+birth_cohorts+nr_siblings,data=data_siblings_birthdate,family = "binomial")


data_siblings_deathyear = data_siblings_nolist %>%
  filter(!(is.na(siblings_id))) %>%
  mutate(complete_deathyear_siblings = ifelse(!(is.na(death_year_siblings)),1,0),
         complete_deathyear_focal = ifelse(!(is.na(death_year)),1,0)) %>%
  ungroup() %>%
  dplyr::select(profileid,complete_deathyear_siblings,complete_deathyear_focal) %>%
  group_by(profileid,complete_deathyear_focal) %>%
  summarize(nr_siblings = n(),
            prop_siblings_complete_deathyear = sum(complete_deathyear_siblings)/nr_siblings,
            siblings_deathyear = sum(complete_deathyear_siblings)) %>%
  mutate(siblings_death_year = ifelse(prop_siblings_complete_deathyear>0,1,0))


# Negative binomial model with offset
model_siblings_completeness_death_offset = glm.nb(siblings_deathyear~complete_deathyear_focal+offset(log(nr_siblings)),
                                           data=data_siblings_deathyear)


# Binomial model

model_siblings_completeness_deathyear_binomial = glm(cbind(siblings_deathyear,nr_siblings)~complete_deathyear_focal,
                                                family=binomial('logit'),
                                                data=data_siblings_deathyear)

# Logistic model with outcome (at least one relative with non-missing demographic variable)

model_sibling_completeness_deathyear_logistic = glm(siblings_death_year~complete_deathyear_focal+nr_siblings,data=data_siblings_deathyear,family = "binomial")



siblings_data_deathcountry = data_siblings_nolist %>%
  filter(!(is.na(siblings_id))) %>%
  mutate(complete_deathcountry_siblings = ifelse(!(is.na(death_country_siblings)),1,0),
         complete_deathcountry_focal = ifelse(!(is.na(country_death_final)),1,0)) %>%
  ungroup() %>%
  group_by(profileid,complete_deathcountry_focal) %>%
  summarize(nr_siblings = n(),
            prop_siblings_complete_deathcountry = sum(complete_deathcountry_siblings)/nr_siblings,
            siblings_deathcountry = sum(complete_deathcountry_siblings)) %>%
  mutate(siblings_complete_deathcountry=ifelse(prop_siblings_complete_deathcountry>0,1,0))

# Negative binomial model with offset
model_siblings_completeness_deathcountry_offset = glm.nb(siblings_deathcountry~complete_deathcountry_focal+offset(log(nr_siblings)),
                                                 data=siblings_data_deathcountry)

# Binomial model

model_siblings_completeness_deathcountry_logistic = glm(cbind(siblings_deathcountry,nr_siblings)~complete_deathcountry_focal,
                                                    family=binomial('logit'),
                                                    data=siblings_data_deathcountry)

# Logistic model with outcome (at least one relative with non-missing demographic variable)

model_siblings_completeness_deathcountry_logistic = glm(siblings_complete_deathcountry~complete_deathcountry_focal+nr_siblings,data=siblings_data_deathcountry,family = "binomial")



siblings_data_birthcountry = data_siblings_nolist %>%
  filter(!(is.na(siblings_id))) %>%
  mutate(complete_birthcountry_siblings = ifelse(!(is.na(birth_country_siblings)),1,0),
         complete_birthcountry_focal = ifelse(!(is.na(country_birth_final)),1,0)
  ) %>%
  ungroup() %>%
  group_by(profileid,complete_birthcountry_focal) %>%
  summarize(nr_siblings = n(),
            prop_siblings_complete_birthcountry = sum(complete_birthcountry_siblings)/nr_siblings) %>%
  mutate(siblings_complete_birthcountry=ifelse(prop_siblings_complete_birthcountry>0,1,0))

# Negative binomial model with offset
model_siblings_completeness_birthcountry_offset = glm.nb(siblings_complete_birthcountry~complete_birthcountry_focal+offset(log(nr_siblings)),
                                                 data=siblings_data_birthcountry)


# Binomial model
model_siblings_completeness_birthcountry_binomial = glm(cbind(siblings_complete_birthcountry,nr_siblings)~complete_birthcountry_focal,
                                                    family=binomial('logit'),
                                                    data=siblings_data_birthcountry)


# Logistic model with outcome (at least one relative with non-missing demographic variable)

model_siblings_completeness_birthcountry_logistic = glm(siblings_complete_birthcountry~complete_birthcountry_focal+nr_siblings,data=siblings_data_birthcountry,family = "binomial")


data_siblings_deathdate = data_siblings_nolist %>%
  filter(!(is.na(siblings_id)),
         !(is.na(death_year_siblings))) %>%
  left_join(data_focal_death_cohorts %>%
              filter(!(is.na(death_year)),
                     death_year>=1600,
                     death_year<=1900) %>%
              ungroup() %>%
              dplyr::select(profileid,death_month,death_cohorts),by="profileid") %>%
  mutate(complete_deathdate_siblings = ifelse(!(is.na(death_month_siblings)),1,0),
         complete_deathdate_focal = ifelse(!(is.na(death_month)),1,0)
  )  %>%
  filter(!(is.na(death_year_siblings))) %>%
  ungroup() %>%
  dplyr::select(profileid,complete_deathdate_siblings,complete_deathdate_focal,death_cohorts) %>%
  filter(!(is.na(death_cohorts))) %>%
  group_by(profileid,complete_deathdate_focal,death_cohorts) %>%
  summarize(nr_siblings = n(),
            prop_siblings_complete_deathdate = sum(complete_deathdate_siblings)/nr_siblings,
            siblings_deathdate =  sum(complete_deathdate_siblings)) %>%
  mutate(siblings_death_date = ifelse(prop_siblings_complete_deathdate>0,1,0))

# Negative binomial model with offset
model_siblings_quality_death_offset = glm.nb(siblings_deathdate~complete_deathdate_focal+death_cohorts+offset(log(nr_siblings)),
                                      data=data_siblings_deathdate)


# Binomial model
model_sibling_quality_deathdate_binomial = glm(cbind(siblings_deathdate,nr_siblings)~complete_deathdate_focal+death_cohorts,
                                        family=binomial('logit'),
                                        data=data_siblings_deathdate)

# Logistic model with outcome (at least one relative with non-missing demographic variable)

model_sibling_quality_deathdate_logistic = glm(siblings_death_date~complete_deathdate_focal+death_cohorts+nr_siblings,data=data_siblings_deathdate,family = "binomial")




model_siblings_completeness_birthyear_offset = coef(summary(model_siblings_completeness_birthyear_offset))
model_siblings_completeness_birthyear_binomial = coef(summary(model_siblings_completeness_birthyear_binomial))
model_siblings_completeness_birthyear_logistic = coef(summary(model_siblings_completeness_birthyear_logistic))
model_siblings_completeness_deathyear_offset = coef(summary(model_siblings_completeness_deathyear_offset))
model_siblings_completeness_deathyear_binomial = coef(summary(model_siblings_completeness_deathyear_binomial))
model_siblings_completeness_deathyear_logistic = coef(summary(model_siblings_completeness_deathyear_logistic))
model_siblings_completeness_birthcountry_offset = coef(summary(model_siblings_completeness_birthcountry_offset))
model_siblings_completeness_birthcountry_binomial = coef(summary(model_siblings_completeness_birthcountry_binomial))
model_siblings_completeness_birthcountry_logistic = coef(summary(model_siblings_completeness_birthcountry_logistic))
model_siblings_completeness_deathcountry_offset = coef(summary(model_siblings_completeness_deathcountry_offset))
model_siblings_completeness_deathcountry_binomial = coef(summary(model_siblings_completeness_deathcountry_binomial))
model_siblings_completeness_deathcountry_logistic = coef(summary(model_siblings_completeness_deathcountry_logistic))
model_siblings_quality_birthdate_offset = coef(summary(model_siblings_quality_birthdate_offset))
model_siblings_quality_birthdate_binomial = coef(summary(model_siblings_quality_birthdate_binomial))
model_siblings_quality_birthdate_logistic = coef(summary(model_siblings_quality_birthdate_logistic))
model_siblings_quality_deathdate_offset = coef(summary(model_siblings_quality_deathdate_offset))
model_siblings_quality_deathdate_binomial = coef(summary(model_siblings_quality_deathdate_binomial))
model_siblings_quality_deathdate_logistic = coef(summary(model_siblings_quality_deathdate_logistic))





save(model_siblings_completeness_birthyear_offset,model_siblings_completeness_deathyear_offset,
     model_siblings_completeness_birthcountry_offset,model_siblings_completeness_deathcountry_offset,
     model_siblings_quality_birthdate_offset,model_siblings_quality_deathdate_offset,
     model_siblings_completeness_birthyear_binomial,model_siblings_completeness_deathyear_binomial,
     model_siblings_completeness_birthcountry_binomial,model_siblings_completeness_deathcountry_binomial,
     model_siblings_quality_birthdate_binomial,model_siblings_quality_deathdate_binomial,
     model_siblings_completeness_birthyear_logistic,model_siblings_completeness_deathyear_logistic,
     model_siblings_completeness_birthcountry_logistic,model_siblings_completeness_deathcountry_logistic,
     model_siblings_quality_birthdate_logistic,model_siblings_quality_deathdate_logistic,
     file="Results/model_siblings_sensitivity_analyses.RData")

#### Grandparents ####


load("Cleaned_Datasets/paternal_grandparents_data_set.RData")

load("Cleaned_Datasets/maternal_grandparents_data_set.RData")

data_grandparents_birthyear = rbind(paternal_grandparents_data_set %>%
                                      filter(!(is.na(f_gm) & is.na(f_gf) & is.na(f_gp_unknown))) %>%
                                      dplyr::select(profileid,birth_year,f_gm,f_gf,f_gp_unknown,f_gm_birth_year,f_gf_birth_year,f_gp_unknown_birth_year) %>%
                                      pivot_longer(!c("profileid","f_gm","f_gf","f_gp_unknown","birth_year"),names_to="f_gparents",values_to = "birth_year_f_gparents") %>%
                                      filter(!(is.na(f_gp_unknown) & f_gparents=="f_gp_unknown_birth_year"),
                                             !(is.na(f_gm) & f_gparents=="f_gm_birth_year"),
                                             !(is.na(f_gf) & f_gparents=="f_gf_birth_year")) %>%
                                      mutate(complete_birthyear_gparents = ifelse(!(is.na(birth_year_f_gparents)),1,0),
                                             complete_birthyear_focal = ifelse(!(is.na(birth_year)),1,0)) %>%
                                      dplyr::select(profileid,complete_birthyear_focal,complete_birthyear_gparents),
                                    maternal_grandparents_data_set %>%
                                      filter(!(is.na(m_gm) & is.na(m_gf) & is.na(m_gp_unknown))) %>%
                                      dplyr::select(profileid,birth_year,m_gm,m_gf,m_gp_unknown,m_gm_birth_year,m_gf_birth_year,m_gp_unknown_birth_year) %>%
                                      pivot_longer(!c("profileid","m_gm","m_gf","m_gp_unknown","birth_year"),names_to="m_gparents",values_to = "birth_year_m_gparents") %>%
                                      filter(!(is.na(m_gp_unknown) & m_gparents=="m_gp_unknown_birth_year"),
                                             !(is.na(m_gm) & m_gparents=="m_gm_birth_year"),
                                             !(is.na(m_gf) & m_gparents=="m_gf_birth_year")) %>%
                                      mutate(complete_birthyear_gparents = ifelse(!(is.na(birth_year_m_gparents)),1,0),
                                             complete_birthyear_focal = ifelse(!(is.na(birth_year)),1,0)) %>%
                                      dplyr::select(profileid,complete_birthyear_focal,complete_birthyear_gparents))  %>%
  ungroup() %>%
  group_by(profileid,complete_birthyear_focal) %>%
  summarize(nr_gparents = n(),
            prop_gparents_complete_birthyear = sum(complete_birthyear_gparents)/nr_gparents,
            gparents_birthyear = sum(complete_birthyear_gparents)) %>%
  mutate(complete_birthyear_gparents = ifelse(prop_gparents_complete_birthyear>0,1,0))



# Negative Binomial with offset for number of relatives
model_gparents_completeness_birthyear_offset = glm.nb(gparents_birthyear~complete_birthyear_focal+offset(log(nr_gparents)),
                                     data=data_grandparents_birthyear)

# Binomial model

model_gparents_completeness_birthyear_binomial = glm(cbind(gparents_birthyear,nr_gparents)~complete_birthyear_focal,
                                        family=binomial('logit'),
                                        data=data_grandparents_birthyear)

# Logistic model with outcome (at least one relative with non-missing demographic variable)

model_gparents_completeness_birthyear_logistic = glm(complete_birthyear_gparents~complete_birthyear_focal+nr_gparents,data=data_grandparents_birthyear,family = "binomial")




data_grandparents_birth_quality = rbind(paternal_grandparents_data_set %>%
                                          filter(!(is.na(f_gm_birth_year))|!(is.na(f_gf_birth_year))|!(is.na(f_gp_unknown_birth_year))) %>%
                                          filter(!(is.na(f_gm) & is.na(f_gf) & is.na(f_gp_unknown))) %>%
                                          dplyr::select(profileid,f_gm,f_gf,f_gp_unknown,f_gm_birth_month,f_gf_birth_month,f_gp_unknown_birth_month) %>%
                                          pivot_longer(!c("profileid","f_gm","f_gf","f_gp_unknown"),names_to="f_gparents",values_to = "birth_month_f_gparents") %>%
                                          filter(!(is.na(f_gp_unknown) & f_gparents=="f_gp_unknown_birth_month"),
                                                 !(is.na(f_gm) & f_gparents=="f_gm_birth_month"),
                                                 !(is.na(f_gf) & f_gparents=="f_gf_birth_month")) %>%
                                          left_join(data_focal_birth_cohorts %>%
                                                      filter(!(is.na(birth_year)),
                                                             birth_year>=1600,
                                                             birth_year<=1900) %>%
                                                      ungroup() %>%
                                                      dplyr::select(profileid,birth_month,birth_cohorts),by="profileid") %>%
                                          mutate(complete_birthmonth_gparents = ifelse(!(is.na(birth_month_f_gparents)),1,0),
                                                 complete_birthmonth_focal = ifelse(!(is.na(birth_month)),1,0)) %>%
                                          dplyr::select(profileid,complete_birthmonth_focal,complete_birthmonth_gparents,birth_cohorts),
                                        maternal_grandparents_data_set %>%
                                          filter(!(is.na(m_gm_birth_year))|!(is.na(m_gf_birth_year))|!(is.na(m_gp_unknown_birth_year))) %>%
                                          filter(!(is.na(m_gm) & is.na(m_gf) & is.na(m_gp_unknown))) %>%
                                          dplyr::select(profileid,m_gm,m_gf,m_gp_unknown,m_gm_birth_month,m_gf_birth_month,m_gp_unknown_birth_month) %>%
                                          pivot_longer(!c("profileid","m_gm","m_gf","m_gp_unknown"),names_to="m_gparents",values_to = "birth_month_m_gparents") %>%
                                          left_join(data_focal_birth_cohorts %>%
                                                      filter(!(is.na(birth_year)),
                                                             birth_year>=1600,
                                                             birth_year<=1900) %>%
                                                      ungroup() %>%
                                                      dplyr::select(profileid,birth_month,birth_cohorts),by="profileid") %>%
                                          filter(!(is.na(m_gp_unknown) & m_gparents=="m_gp_unknown_birth_month"),
                                                 !(is.na(m_gm) & m_gparents=="m_gm_birth_month"),
                                                 !(is.na(m_gf) & m_gparents=="m_gf_birth_month")) %>%
                                          mutate(complete_birthmonth_gparents = ifelse(!(is.na(birth_month_m_gparents)),1,0),
                                                 complete_birthmonth_focal = ifelse(!(is.na(birth_month)),1,0)) %>%
                                          dplyr::select(profileid,complete_birthmonth_focal,complete_birthmonth_gparents,birth_cohorts))  %>%
  ungroup() %>%
  filter(!(is.na(birth_cohorts))) %>%
  group_by(profileid,complete_birthmonth_focal,birth_cohorts) %>%
  summarize(nr_gparents = n(),
            prop_gparents_complete_birthmonth = sum(complete_birthmonth_gparents)/nr_gparents,
            gparents_birthmonth = sum(complete_birthmonth_gparents)) %>%
  mutate(complete_birthmonth_gparents = ifelse(prop_gparents_complete_birthmonth>0,1,0))

# Negative Binomial with offset for number of relatives
model_gparents_quality_birthdate_offset = glm.nb(gparents_birthmonth~complete_birthmonth_focal+
                                  birth_cohorts+nr_gparents,data=data_grandparents_birth_quality)

# Binomial model
model_gparents_quality_birthdate_binomial = glm(cbind(gparents_birthmonth,nr_gparents)~complete_birthmonth_focal+birth_cohorts,
                                   family=binomial('logit'),
                                   data=data_grandparents_birth_quality)
# Logistic model with outcome (at least one relative with non-missing demographic variable)
model_gparents_quality_birthdate_logistic = glm(complete_birthmonth_gparents~complete_birthmonth_focal+birth_cohorts+nr_gparents,data=data_grandparents_birth_quality,family = "binomial")



data_grandparents_deathyear = rbind(paternal_grandparents_data_set %>%
                                      filter(!(is.na(f_gm) & is.na(f_gf) & is.na(f_gp_unknown))) %>%
                                      dplyr::select(profileid,death_year,f_gm,f_gf,f_gp_unknown,f_gm_death_year,f_gf_death_year,f_gp_unknown_death_year) %>%
                                      pivot_longer(!c("profileid","f_gm","f_gf","f_gp_unknown","death_year"),names_to="f_gparents",values_to = "death_year_f_gparents") %>%
                                      filter(!(is.na(f_gp_unknown) & f_gparents=="f_gp_unknown_death_year"),
                                             !(is.na(f_gm) & f_gparents=="f_gm_death_year"),
                                             !(is.na(f_gf) & f_gparents=="f_gf_death_year")) %>%
                                      mutate(complete_deathyear_gparents = ifelse(!(is.na(death_year_f_gparents)),1,0),
                                             complete_deathyear_focal = ifelse(!(is.na(death_year)),1,0)) %>%
                                      dplyr::select(profileid,complete_deathyear_focal,complete_deathyear_gparents),
                                    maternal_grandparents_data_set %>%
                                      filter(!(is.na(m_gm) & is.na(m_gf) & is.na(m_gp_unknown))) %>%
                                      dplyr::select(profileid,death_year,m_gm,m_gf,m_gp_unknown,m_gm_death_year,m_gf_death_year,m_gp_unknown_death_year) %>%
                                      pivot_longer(!c("profileid","m_gm","m_gf","m_gp_unknown","death_year"),names_to="m_gparents",values_to = "death_year_m_gparents") %>%
                                      filter(!(is.na(m_gp_unknown) & m_gparents=="m_gp_unknown_death_year"),
                                             !(is.na(m_gm) & m_gparents=="m_gm_death_year"),
                                             !(is.na(m_gf) & m_gparents=="m_gf_death_year")) %>%
                                      mutate(complete_deathyear_gparents = ifelse(!(is.na(death_year_m_gparents)),1,0),
                                             complete_deathyear_focal = ifelse(!(is.na(death_year)),1,0)) %>%
                                      dplyr::select(profileid,complete_deathyear_focal,complete_deathyear_gparents))  %>%
  ungroup() %>%
  group_by(profileid,complete_deathyear_focal) %>%
  summarize(nr_gparents = n(),
            prop_gparents_complete_deathyear = sum(complete_deathyear_gparents)/nr_gparents,
            gparents_deathyear = sum(complete_deathyear_gparents)) %>%
  mutate(complete_deathyear_gparents = ifelse(prop_gparents_complete_deathyear>0,1,0))

# Negative Binomial with offset for number of relatives
model_gparents_completeness_deathyear_offset = glm.nb(complete_deathyear_gparents~complete_deathyear_focal+offset(log(nr_gparents)),
                                           data=data_grandparents_deathyear)

# Binomial model
model_gparents_completeness_deathyear_binomial = glm(cbind(gparents_deathyear,nr_gparents)~complete_deathyear_focal,
                                              family=binomial('logit'),
                                              data=data_grandparents_deathyear)

# Logistic model with outcome (at least one relative with non-missing demographic variable)

model_gparents_completeness_deathyear_logistic = glm(complete_deathyear_gparents~complete_deathyear_focal+nr_gparents,data=data_grandparents_deathyear,family = "binomial")




data_grandparents_death_quality = rbind(paternal_grandparents_data_set %>%
                                          filter(!(is.na(f_gm) & is.na(f_gf) & is.na(f_gp_unknown))) %>%
                                          filter(!(is.na(f_gm_death_year))|!(is.na(f_gf_death_year))|!(is.na(f_gp_unknown_death_year))) %>%
                                          dplyr::select(profileid,f_gm,f_gf,f_gp_unknown,f_gm_death_month,f_gf_death_month,f_gp_unknown_death_month) %>%
                                          pivot_longer(!c("profileid","f_gm","f_gf","f_gp_unknown"),names_to="f_gparents",values_to = "death_month_f_gparents") %>%
                                          filter(!(is.na(f_gp_unknown) & f_gparents=="f_gp_unknown_death_month"),
                                                 !(is.na(f_gm) & f_gparents=="f_gm_death_month"),
                                                 !(is.na(f_gf) & f_gparents=="f_gf_death_month")) %>%
                                          left_join(data_focal_death_cohorts %>%
                                                      filter(!(is.na(death_year)),
                                                             death_year>=1600,
                                                             death_year<=1900) %>%
                                                      ungroup() %>%
                                                      dplyr::select(profileid,death_month,death_cohorts),by="profileid") %>%
                                          mutate(complete_deathmonth_gparents = ifelse(!(is.na(death_month_f_gparents)),1,0),
                                                 complete_deathmonth_focal = ifelse(!(is.na(death_month)),1,0)) %>%
                                          dplyr::select(profileid,complete_deathmonth_focal,complete_deathmonth_gparents,death_cohorts),
                                        maternal_grandparents_data_set %>%
                                          filter(!(is.na(m_gm) & is.na(m_gf) & is.na(m_gp_unknown))) %>%
                                          filter(!(is.na(m_gm_death_year))|!(is.na(m_gf_death_year))|!(is.na(m_gp_unknown_death_year))) %>%
                                          dplyr::select(profileid,m_gm,m_gf,m_gp_unknown,m_gm_death_month,m_gf_death_month,m_gp_unknown_death_month) %>%
                                          pivot_longer(!c("profileid","m_gm","m_gf","m_gp_unknown"),names_to="m_gparents",values_to = "death_month_m_gparents") %>%
                                          left_join(data_focal_death_cohorts %>%
                                                      filter(!(is.na(death_year)),
                                                             death_year>=1600,
                                                             death_year<=1900) %>%
                                                      ungroup() %>%
                                                      dplyr::select(profileid,death_month,death_cohorts),by="profileid") %>%
                                          filter(!(is.na(m_gp_unknown) & m_gparents=="m_gp_unknown_death_month"),
                                                 !(is.na(m_gm) & m_gparents=="m_gm_death_month"),
                                                 !(is.na(m_gf) & m_gparents=="m_gf_death_month")) %>%
                                          mutate(complete_deathmonth_gparents = ifelse(!(is.na(death_month_m_gparents)),1,0),
                                                 complete_deathmonth_focal = ifelse(!(is.na(death_month)),1,0)) %>%
                                          dplyr::select(profileid,complete_deathmonth_focal,complete_deathmonth_gparents,death_cohorts))  %>%
  ungroup() %>%
  filter(!(is.na(death_cohorts))) %>%
  group_by(profileid,complete_deathmonth_focal,death_cohorts) %>%
  summarize(nr_gparents = n(),
            prop_gparents_complete_deathmonth = sum(complete_deathmonth_gparents)/nr_gparents,
            gparents_deathmonth = sum(complete_deathmonth_gparents)) %>%
  mutate(complete_deathmonth_gparents = ifelse(prop_gparents_complete_deathmonth>0,1,0)) 

# Negative Binomial with offset for number of relatives
model_gparents_quality_deathdate_offset = glm.nb(gparents_deathmonth~complete_deathmonth_focal+death_cohorts+offset(log(nr_gparents)),
                                      data=data_grandparents_death_quality)



# Binomial model

model_gparents_quality_deathdate_binomial = glm(cbind(gparents_deathmonth,nr_gparents)~complete_deathmonth_focal+death_cohorts,
                                         family=binomial('logit'),
                                         data=data_grandparents_death_quality)


# Logistic model with outcome (at least one relative with non-missing demographic variable)

model_gparents_quality_deathdate_logistic = glm(complete_deathmonth_gparents~complete_deathmonth_focal+death_cohorts+nr_gparents,data=data_grandparents_death_quality,family = "binomial")


data_gparents_countrybirth = rbind(paternal_grandparents_data_set %>%
                                     filter(!(is.na(f_gm) & is.na(f_gf) & is.na(f_gp_unknown))) %>%
                                     dplyr::select(profileid,f_gm,f_gf,f_gp_unknown,f_gm_birth_country,f_gf_birth_country,f_gp_unknown_birth_country,country_birth_final) %>%
                                     pivot_longer(!c("profileid","f_gm","f_gf","f_gp_unknown","country_birth_final"),names_to="gparents",values_to = "birth_country_gparents") %>%
                                     filter(!(is.na(f_gp_unknown) & gparents=="f_gp_unknown_birth_country"),
                                            !(is.na(f_gm) & gparents=="f_gm_birth_country"),
                                            !(is.na(f_gf) & gparents=="f_gf_birth_country")) %>%
                                     mutate(complete_countrybirth_gparents = ifelse(!(is.na(birth_country_gparents)),1,0),
                                            complete_countrybirth_focal = ifelse(!(is.na(country_birth_final)),1,0)) %>%
                                     ungroup() %>%
                                     dplyr::select(profileid,complete_countrybirth_gparents,complete_countrybirth_focal),
                                   maternal_grandparents_data_set %>%
                                     filter(!(is.na(m_gm) & is.na(m_gf) & is.na(m_gp_unknown))) %>%
                                     dplyr::select(profileid,m_gm,m_gf,m_gp_unknown,m_gm_birth_country,m_gf_birth_country,m_gp_unknown_birth_country,country_birth_final) %>%
                                     pivot_longer(!c("profileid","m_gm","m_gf","m_gp_unknown","country_birth_final"),names_to="gparents",values_to = "birth_country_gparents") %>%
                                     filter(!(is.na(m_gp_unknown) & gparents=="m_gp_unknown_birth_country"),
                                            !(is.na(m_gm) & gparents=="m_gm_birth_country"),
                                            !(is.na(m_gf) & gparents=="m_gf_birth_country")) %>%
                                     mutate(complete_countrybirth_gparents = ifelse(!(is.na(birth_country_gparents)),1,0),
                                            complete_countrybirth_focal = ifelse(!(is.na(country_birth_final)),1,0)) %>%
                                     ungroup() %>%
                                     dplyr::select(profileid,complete_countrybirth_gparents,complete_countrybirth_focal)) %>%
  ungroup() %>%
  group_by(profileid,complete_countrybirth_focal) %>%
  summarize(nr_gparents = n(),
            prop_gparents_complete_countrybirth = sum(complete_countrybirth_gparents)/nr_gparents,
            gparents_countrybirth = sum(complete_countrybirth_gparents)) %>%
  mutate(gparents_complete_countrybirth = ifelse(prop_gparents_complete_countrybirth>0,1,0))


# Negative Binomial with offset for number of relatives
model_gparents_completeness_birthcountry_offset = glm.nb(gparents_countrybirth~complete_countrybirth_focal+offset(log(nr_gparents)),data=data_gparents_countrybirth)

# Binomial model
model_gparents_completeness_birthcountry_binomial = glm(cbind(gparents_countrybirth,nr_gparents)~complete_countrybirth_focal,
                                                     family=binomial('logit'),
                                                     data=data_gparents_countrybirth)


# Logistic model with outcome (at least one relative with non-missing demographic variable)

model_gparents_completeness_birthcountry_logistic = glm(gparents_complete_countrybirth~complete_countrybirth_focal+nr_gparents,data=data_gparents_countrybirth,family = "binomial")




data_gparents_countrydeath = rbind(paternal_grandparents_data_set %>%
                                     filter(!(is.na(f_gm) & is.na(f_gf) & is.na(f_gp_unknown))) %>%
                                     dplyr::select(profileid,f_gm,f_gf,f_gp_unknown,f_gm_death_country,f_gf_death_country,f_gp_unknown_death_country,country_death_final) %>%
                                     pivot_longer(!c("profileid","f_gm","f_gf","f_gp_unknown","country_death_final"),names_to="gparents",values_to = "death_country_gparents") %>%
                                     filter(!(is.na(f_gp_unknown) & gparents=="f_gp_unknown_death_country"),
                                            !(is.na(f_gm) & gparents=="f_gm_death_country"),
                                            !(is.na(f_gf) & gparents=="f_gf_death_country")) %>%
                                     mutate(complete_countrydeath_gparents = ifelse(!(is.na(death_country_gparents)),1,0),
                                            complete_countrydeath_focal = ifelse(!(is.na(country_death_final)),1,0)) %>%
                                     ungroup() %>%
                                     dplyr::select(profileid,complete_countrydeath_gparents,complete_countrydeath_focal),
                                   maternal_grandparents_data_set %>%
                                     filter(!(is.na(m_gm) & is.na(m_gf) & is.na(m_gp_unknown))) %>%
                                     dplyr::select(profileid,m_gm,m_gf,m_gp_unknown,m_gm_death_country,m_gf_death_country,m_gp_unknown_death_country,country_death_final) %>%
                                     pivot_longer(!c("profileid","m_gm","m_gf","m_gp_unknown","country_death_final"),names_to="gparents",values_to = "death_country_gparents") %>%
                                     filter(!(is.na(m_gp_unknown) & gparents=="m_gp_unknown_death_country"),
                                            !(is.na(m_gm) & gparents=="m_gm_death_country"),
                                            !(is.na(m_gf) & gparents=="m_gf_death_country")) %>%
                                     mutate(complete_countrydeath_gparents = ifelse(!(is.na(death_country_gparents)),1,0),
                                            complete_countrydeath_focal = ifelse(!(is.na(country_death_final)),1,0)) %>%
                                     ungroup() %>%
                                     dplyr::select(profileid,complete_countrydeath_gparents,complete_countrydeath_focal)) %>%
  ungroup() %>%
  group_by(profileid,complete_countrydeath_focal) %>%
  summarize(nr_gparents = n(),
            prop_gparents_complete_countrydeath = sum(complete_countrydeath_gparents)/nr_gparents,
            gparents_countrydeath = sum(complete_countrydeath_gparents)) %>%
  mutate(gparents_complete_countrydeath = ifelse(prop_gparents_complete_countrydeath>0,1,0))


# Negative Binomial with offset for number of relatives
model_gparents_completeness_deathcountry_offset = glm.nb(gparents_countrydeath~complete_countrydeath_focal+offset(log(nr_gparents)),data=data_gparents_countrydeath)

# Binomial model
model_gparents_completeness_deathcountry_binomial = glm(cbind(gparents_countrydeath,nr_gparents)~complete_countrydeath_focal,
                                                        family=binomial('logit'),
                                                        data=data_gparents_countrydeath)

# Logistic model with outcome (at least one relative with non-missing demographic variable)

model_gparents_completeness_deathcountry_logistic = glm(gparents_complete_countrydeath~complete_countrydeath_focal+nr_gparents,data=data_gparents_countrydeath,family = "binomial")



model_gparents_completeness_birthyear_offset = coef(summary(model_gparents_completeness_birthyear_offset))
model_gparents_completeness_birthyear_binomial = coef(summary(model_gparents_completeness_birthyear_binomial))
model_gparents_completeness_birthyear_logistic = coef(summary(model_gparents_completeness_birthyear_binomial))
model_gparents_completeness_deathyear_offset = coef(summary(model_gparents_completeness_deathyear_offset))
model_gparents_completeness_deathyear_binomial = coef(summary(model_gparents_completeness_deathyear_binomial))
model_gparents_completeness_deathyear_logistic = coef(summary(model_gparents_completeness_deathyear_logistic))
model_gparents_completeness_birthcountry_offset = coef(summary(model_gparents_completeness_birthcountry_offset))
model_gparents_completeness_birthcountry_binomial = coef(summary(model_gparents_completeness_birthcountry_binomial))
model_gparents_completeness_birthcountry_logistic = coef(summary(model_gparents_completeness_birthcountry_logistic))
model_gparents_completeness_deathcountry_offset = coef(summary(model_gparents_completeness_deathcountry_offset))
model_gparents_completeness_deathcountry_binomial = coef(summary(model_gparents_completeness_deathcountry_binomial))
model_gparents_completeness_deathcountry_logistic = coef(summary(model_gparents_completeness_deathcountry_logistic))
model_gparents_quality_birthdate_offset = coef(summary(model_gparents_quality_birthdate_offset))
model_gparents_quality_birthdate_binomial = coef(summary(model_gparents_quality_birthdate_binomial))
model_gparents_quality_birthdate_logistic = coef(summary(model_gparents_quality_birthdate_logistic))
model_gparents_quality_deathdate_offset = coef(summary(model_gparents_quality_deathdate_offset))
model_gparents_quality_deathdate_binomial = coef(summary(model_gparents_quality_deathdate_binomial))
model_gparents_quality_deathdate_logistic = coef(summary(model_gparents_quality_deathdate_logistic))



save(model_gparents_completeness_birthyear_offset,model_gparents_completeness_deathyear_offset,
     model_gparents_completeness_birthcountry_offset,model_gparents_completeness_deathcountry_offset,
     model_gparents_quality_birthdate_offset,model_gparents_quality_deathdate_offset,
     model_gparents_completeness_birthyear_binomial,model_gparents_completeness_deathyear_binomial,
     model_gparents_completeness_birthcountry_binomial,model_gparents_completeness_deathcountry_binomial,
     model_gparents_quality_birthdate_binomial,model_gparents_quality_deathdate_binomial,
     model_gparents_completeness_birthyear_logistic,model_gparents_completeness_deathyear_logistic,
     model_gparents_completeness_birthcountry_logistic,model_gparents_completeness_deathcountry_logistic,
     model_gparents_quality_birthdate_logistic,model_gparents_quality_deathdate_logistic,
     file="Results/model_grandparents_sensitivity_analyses.RData")


#### Aunts and Uncles ####

load('Cleaned_Datasets/uncles_aunts_data_set.RData')

data_aunts_uncles_birthyear = data_aunts_uncles %>%
  filter(!(is.na(aunts_uncles_id))) %>%
  mutate(complete_birthyear_aunts_uncles = ifelse(!(is.na(birth_year_aunts_uncles)),1,0),
         complete_birthyear_focal = ifelse(!(is.na(birth_year)),1,0)
  ) %>%
  ungroup() %>%
  dplyr::select(profileid,complete_birthyear_aunts_uncles,complete_birthyear_focal) %>%
  group_by(profileid,complete_birthyear_focal) %>%
  summarize(nr_aunts_uncles = n(),
            prop_aunts_uncles_complete_birthyear = sum(complete_birthyear_aunts_uncles)/nr_aunts_uncles,
            birthyear_aunts = sum(complete_birthyear_aunts_uncles)) %>%
  mutate(aunts_uncles_birth_year = ifelse(prop_aunts_uncles_complete_birthyear>0,1,0))

# Negative Binomial with offset

model_aunts_uncles_completeness_birthyear_offset = glm.nb(birthyear_aunts~complete_birthyear_focal+offset(log(nr_aunts_uncles)),
                                               data=data_aunts_uncles_birthyear)


# Binomial model

model_aunts_uncles_completeness_birthyear_binomial = glm(cbind(birthyear_aunts,nr_aunts_uncles)~complete_birthyear_focal,
                                                 family=binomial('logit'),
                                                 data=data_aunts_uncles_birthyear)


# Logistic model with outcome (at least one relative with non-missing demographic variable)

model_aunts_uncles_completeness_birthyear_logistic = glm(aunts_uncles_birth_year~complete_birthyear_focal+nr_aunts_uncles,data=data_aunts_uncles_birthyear,family = "binomial")


data_aunts_uncles_deathyear = data_aunts_uncles %>%
  filter(!(is.na(aunts_uncles_id))) %>%
  mutate(complete_deathyear_aunts_uncles = ifelse(!(is.na(death_year_aunts_uncles)),1,0),
         complete_deathyear_focal = ifelse(!(is.na(death_year)),1,0)) %>%
  ungroup() %>%
  dplyr::select(profileid,complete_deathyear_aunts_uncles,complete_deathyear_focal) %>%
  group_by(profileid,complete_deathyear_focal) %>%
  summarize(nr_aunts_uncles = n(),
            prop_aunts_uncles_complete_deathyear = sum(complete_deathyear_aunts_uncles)/nr_aunts_uncles,
            deathyear_aunts_uncles = sum(complete_deathyear_aunts_uncles)) %>%
  mutate(aunts_uncles_death_year = ifelse(prop_aunts_uncles_complete_deathyear>0,1,0))


# Negative Binomial with offset
model_aunts_uncles_completeness_deathyear_offset = glm.nb(deathyear_aunts_uncles~complete_deathyear_focal+offset(log(nr_aunts_uncles)),
                                               data=data_aunts_uncles_deathyear)


# Binomial model

model_aunts_uncles_completeness_deathyear_binomial = glm(cbind(deathyear_aunts_uncles,nr_aunts_uncles)~complete_deathyear_focal,
                                                 family=binomial('logit'),
                                                 data=data_aunts_uncles_deathyear)


# Logistic model with outcome (at least one relative with non-missing demographic variable)

model_aunts_uncles_completeness_deathyear_logistic = glm(aunts_uncles_death_year~complete_deathyear_focal+nr_aunts_uncles,data=data_aunts_uncles_deathyear,family = "binomial")


data_aunts_uncles_birthcountry = data_aunts_uncles %>%
  filter(!(is.na(aunts_uncles_id))) %>%
  mutate(complete_birthcountry_aunts_uncles = ifelse(!(is.na(birth_country_aunts_uncles)),1,0),
         complete_birthcountry_focal = ifelse(!(is.na(birth_country)),1,0)
  ) %>%
  ungroup() %>%
  dplyr::select(profileid,complete_birthcountry_aunts_uncles,complete_birthcountry_focal) %>%
  group_by(profileid,complete_birthcountry_focal) %>%
  summarize(nr_aunts_uncles = n(),
            prop_aunts_uncles_complete_birthcountry = sum(complete_birthcountry_aunts_uncles)/nr_aunts_uncles,
            uncles_complete_birthcountry = sum(complete_birthcountry_aunts_uncles)) %>%
  mutate(aunts_uncles_complete_birthcountry = ifelse(prop_aunts_uncles_complete_birthcountry>0,1,0))

# Negative Binomial with offset

model_aunts_uncles_completeness_birthcountry_offset = glm.nb(uncles_complete_birthcountry~complete_birthcountry_focal+offset(log(nr_aunts_uncles)),
                                                      data=data_aunts_uncles_birthcountry)


# Binomial model

model_aunts_uncles_completeness_birthcountry_binomial = glm(cbind(uncles_complete_birthcountry,nr_aunts_uncles)~complete_birthcountry_focal,
                                                         family=binomial('logit'),
                                                         data=data_aunts_uncles_birthcountry)


# Logistic model with outcome (at least one relative with non-missing demographic variable)

model_aunts_uncles_completeness_birthcountry_logistic = glm(aunts_uncles_complete_birthcountry~complete_birthcountry_focal+nr_aunts_uncles,
                                                   data=data_aunts_uncles_birthcountry,
                                                   family = "binomial")


data_aunts_uncles_deathcountry = data_aunts_uncles %>%
  filter(!(is.na(aunts_uncles_id))) %>%
  mutate(complete_deathcountry_aunts_uncles = ifelse(!(is.na(death_country_aunts_uncles)),1,0),
         complete_deathcountry_focal = ifelse(!(is.na(death_country)),1,0)
  ) %>%
  ungroup() %>%
  dplyr::select(profileid,complete_deathcountry_aunts_uncles,complete_deathcountry_focal) %>%
  group_by(profileid,complete_deathcountry_focal) %>%
  summarize(nr_aunts_uncles = n(),
            prop_aunts_uncles_complete_deathcountry = sum(complete_deathcountry_aunts_uncles)/nr_aunts_uncles,
            aunts_uncles_deathcountry = sum(complete_deathcountry_aunts_uncles)) %>%
  mutate(aunts_uncles_complete_deathcountry = ifelse(prop_aunts_uncles_complete_deathcountry>0,1,0))

# Negative Binomial with offset

model_aunts_uncles_completeness_deathcountry_offset = glm.nb(aunts_uncles_deathcountry~complete_deathcountry_focal+offset(log(nr_aunts_uncles)),
                                                      data=data_aunts_uncles_deathcountry)

# Binomial model

model_aunts_uncles_completeness_deathcountry_binomial = glm(cbind(aunts_uncles_deathcountry,nr_aunts_uncles)~complete_deathcountry_focal,
                                                         family=binomial('logit'),
                                                         data=data_aunts_uncles_deathcountry)

# Logistic model with outcome (at least one relative with non-missing demographic variable)


model_aunts_uncles_completeness_deathcountry_logistic = glm(aunts_uncles_complete_deathcountry~complete_deathcountry_focal+nr_aunts_uncles,
                                                   data=data_aunts_uncles_deathcountry,
                                                   family = "binomial")



data_aunts_uncles_birthdate = data_aunts_uncles %>%
  filter(!(is.na(aunts_uncles_id)),
         !(is.na(birth_year)),
         !(is.na(birth_year_aunts_uncles)),
         birth_year>=1600,
         birth_year<=1900) %>%
  mutate(birth_cohorts = case_when(birth_year>=1600 & birth_year<1625 ~ "1600-1624",
                                   birth_year>=1625 & birth_year<1650 ~ "1625-1649",
                                   birth_year>=1650 & birth_year<1675 ~ "1650-1674",
                                   birth_year>=1675 & birth_year<1700 ~ "1675-1699",
                                   birth_year>=1700 & birth_year<1725 ~ "1700-1724",
                                   birth_year>=1725 & birth_year<1750 ~ "1725-1749",
                                   birth_year>=1750 & birth_year<1775 ~ "1750-1774",
                                   birth_year>=1775 & birth_year<1800 ~ "1775-1799",
                                   birth_year>=1800 & birth_year<1825 ~ "1800-1824",
                                   birth_year>=1825 & birth_year<1850 ~ "1825-1849",
                                   birth_year>=1850 & birth_year<1875 ~ "1850-1874",
                                   birth_year>=1875 & birth_year<=1900 ~ "1875-1900",
                                   TRUE ~ NA)) %>%
  mutate(birth_cohorts = factor(birth_cohorts,levels = c("1600-1624","1625-1649","1650-1674",
                                                         "1675-1699","1700-1724","1725-1749",
                                                         "1750-1774","1775-1799","1800-1824",
                                                         "1825-1849","1850-1874","1875-1900"))) %>%
  mutate(complete_birthdate_aunts_uncles = ifelse(!(is.na(birth_month_aunts_uncles)),1,0),
         complete_birthdate_focal = ifelse(!(is.na(birth_month)),1,0)
  )  %>%
  ungroup() %>%
  dplyr::select(profileid,complete_birthdate_aunts_uncles,complete_birthdate_focal,birth_cohorts) %>%
  group_by(profileid,complete_birthdate_focal,birth_cohorts) %>%
  summarize(nr_aunts_uncles = n(),
            prop_aunts_uncles_complete_birthdate = sum(complete_birthdate_aunts_uncles)/nr_aunts_uncles,
            aunts_uncles_birthdate = sum(complete_birthdate_aunts_uncles)) %>%
  mutate(aunts_uncles_birth_date = ifelse(prop_aunts_uncles_complete_birthdate>0,1,0))

# Negative Binomial with offset

model_aunts_uncles_quality_birthdate_offset = glm.nb(aunts_uncles_birthdate~complete_birthdate_focal+birth_cohorts+offset(log(nr_aunts_uncles)),
                                              data=data_aunts_uncles_birthdate)

# Binomial model

model_aunts_uncles_quality_birthdate_binomial = glm(cbind(aunts_uncles_birthdate,nr_aunts_uncles)~complete_birthdate_focal+birth_cohorts,
                                                family=binomial('logit'),
                                                data=data_aunts_uncles_birthdate)


# Logistic model with outcome (at least one relative with non-missing demographic variable)


model_aunts_uncles_quality_birthdate_logistic = glm(aunts_uncles_birth_date~complete_birthdate_focal+birth_cohorts+nr_aunts_uncles,data=data_aunts_uncles_birthdate,family = "binomial")



data_aunts_uncles_deathdate = data_aunts_uncles %>%
  filter(!(is.na(aunts_uncles_id)),
         !(is.na(death_year)),
         !(is.na(death_year_aunts_uncles)),
         death_year>=1600,
         death_year<=1900) %>%
  mutate(death_cohorts = case_when(death_year>=1600 & death_year<1625 ~ "1600-1624",
                                   death_year>=1625 & death_year<1650 ~ "1625-1649",
                                   death_year>=1650 & death_year<1675 ~ "1650-1674",
                                   death_year>=1675 & death_year<1700 ~ "1675-1699",
                                   death_year>=1700 & death_year<1725 ~ "1700-1724",
                                   death_year>=1725 & death_year<1750 ~ "1725-1749",
                                   death_year>=1750 & death_year<1775 ~ "1750-1774",
                                   death_year>=1775 & death_year<1800 ~ "1775-1799",
                                   death_year>=1800 & death_year<1825 ~ "1800-1824",
                                   death_year>=1825 & death_year<1850 ~ "1825-1849",
                                   death_year>=1850 & death_year<1875 ~ "1850-1874",
                                   death_year>=1875 & death_year<=1900 ~ "1875-1900",
                                   TRUE ~ NA)) %>%
  mutate(death_cohorts = factor(death_cohorts,levels = c("1600-1624","1625-1649","1650-1674",
                                                         "1675-1699","1700-1724","1725-1749",
                                                         "1750-1774","1775-1799","1800-1824",
                                                         "1825-1849","1850-1874","1875-1900"))) %>%
  mutate(complete_deathdate_aunts_uncles = ifelse(!(is.na(death_month_aunts_uncles)),1,0),
         complete_deathdate_focal = ifelse(!(is.na(death_month)),1,0)
  )  %>%
  ungroup() %>%
  dplyr::select(profileid,complete_deathdate_aunts_uncles,complete_deathdate_focal,death_cohorts) %>%
  group_by(profileid,complete_deathdate_focal,death_cohorts) %>%
  summarize(nr_aunts_uncles = n(),
            prop_aunts_uncles_complete_deathdate = sum(complete_deathdate_aunts_uncles)/nr_aunts_uncles,
            aunts_uncles_deathdate = sum(complete_deathdate_aunts_uncles)) %>%
  mutate(aunts_uncles_death_date = ifelse(prop_aunts_uncles_complete_deathdate>0,1,0))

# Negative Binomial with offset

model_aunts_uncles_quality_deathdate_offset = glm.nb(aunts_uncles_deathdate~complete_deathdate_focal+death_cohorts+offset(log(nr_aunts_uncles)),
                                              data=data_aunts_uncles_deathdate)

# Binomial model

model_aunts_uncles_quality_deathdate_binomial = glm(cbind(aunts_uncles_deathdate,nr_aunts_uncles)~complete_deathdate_focal+death_cohorts,
                                                family=binomial('logit'),
                                                data=data_aunts_uncles_deathdate)


# Logistic model with outcome (at least one relative with non-missing demographic variable)


model_aunts_uncles_quality_deathdate_logistic = glm(aunts_uncles_death_date~complete_deathdate_focal+death_cohorts+nr_aunts_uncles,data=data_aunts_uncles_deathdate,family = "binomial")





model_aunts_uncles_completeness_birthyear_offset = coef(summary(model_aunts_uncles_completeness_birthyear_offset))
model_aunts_uncles_completeness_birthyear_binomial = coef(summary(model_aunts_uncles_completeness_birthyear_binomial))
model_aunts_uncles_completeness_birthyear_logistic = coef(summary(model_aunts_uncles_completeness_birthyear_logistic))
model_aunts_uncles_completeness_deathyear_offset = coef(summary(model_aunts_uncles_completeness_deathyear_offset))
model_aunts_uncles_completeness_deathyear_binomial = coef(summary(model_aunts_uncles_completeness_deathyear_binomial))
model_aunts_uncles_completeness_deathyear_logistic = coef(summary(model_aunts_uncles_completeness_deathyear_logistic))
model_aunts_uncles_completeness_birthcountry_offset = coef(summary(model_aunts_uncles_completeness_birthcountry_offset))
model_aunts_uncles_completeness_birthcountry_binomial = coef(summary(model_aunts_uncles_completeness_birthcountry_binomial))
model_aunts_uncles_completeness_birthcountry_logistic = coef(summary(model_aunts_uncles_completeness_birthcountry_logistic))
model_aunts_uncles_completeness_deathcountry_offset = coef(summary(model_aunts_uncles_completeness_deathcountry_offset))
model_aunts_uncles_completeness_deathcountry_binomial = coef(summary(model_aunts_uncles_completeness_deathcountry_binomial))
model_aunts_uncles_completeness_deathcountry_logistic = coef(summary(model_aunts_uncles_completeness_deathcountry_logistic))
model_aunts_uncles_quality_birthdate_offset = coef(summary(model_aunts_uncles_quality_birthdate_offset))
model_aunts_uncles_quality_birthdate_binomial = coef(summary(model_aunts_uncles_quality_birthdate_binomial))
model_aunts_uncles_quality_birthdate_logistic = coef(summary(model_aunts_uncles_quality_birthdate_logistic))
model_aunts_uncles_quality_deathdate_offset = coef(summary(model_aunts_uncles_quality_deathdate_offset))
model_aunts_uncles_quality_deathdate_binomial = coef(summary(model_aunts_uncles_quality_deathdate_binomial))
model_aunts_uncles_quality_deathdate_logistic = coef(summary(model_aunts_uncles_quality_deathdate_logistic))

save(model_aunts_uncles_completeness_birthyear_offset,model_aunts_uncles_completeness_deathyear_offset,
     model_aunts_uncles_completeness_birthcountry_offset,model_aunts_uncles_completeness_deathcountry_offset,
     model_aunts_uncles_quality_birthdate_offset,model_aunts_uncles_quality_deathdate_offset,
     model_aunts_uncles_completeness_birthyear_binomial,model_aunts_uncles_completeness_deathyear_binomial,
     model_aunts_uncles_completeness_birthcountry_binomial,model_aunts_uncles_completeness_deathcountry_binomial,
     model_aunts_uncles_quality_birthdate_binomial,model_aunts_uncles_quality_deathdate_binomial,
     model_aunts_uncles_completeness_birthyear_logistic,model_aunts_uncles_completeness_deathyear_logistic,
     model_aunts_uncles_completeness_birthcountry_logistic,model_aunts_uncles_completeness_deathcountry_logistic,
     model_aunts_uncles_quality_birthdate_logistic,model_aunts_uncles_quality_deathdate_logistic,
     file="Results/model_aunts_uncles_sensitivity_analyses.RData")


#### Cousins ####

load('Cleaned_Datasets/cousins_data_set.RData')


data_cousins_birthyear = cousins_data_set %>%
  filter(!(is.na(cousins_id))) %>%
  mutate(complete_birthyear_cousins = ifelse(!(is.na(birth_year_cousins)),1,0),
         complete_birthyear_focal = ifelse(!(is.na(birth_year)),1,0)
  ) %>%
  ungroup() %>%
  dplyr::select(profileid,complete_birthyear_cousins,complete_birthyear_focal) %>%
  group_by(profileid,complete_birthyear_focal) %>%
  summarize(nr_cousins = n(),
            prop_cousins_complete_birthyear = sum(complete_birthyear_cousins)/nr_cousins,
            cousins_birthyear = sum(complete_birthyear_cousins)) %>%
  mutate(cousins_birth_year = ifelse(prop_cousins_complete_birthyear>0,1,0))


# Negative binomial model with offset
model_cousins_completeness_birthyear_offset = glm.nb(cousins_birthyear~complete_birthyear_focal+offset(log(nr_cousins)),data=data_cousins_birthyear)
# Binomial model
model_cousins_completeness_birthyear_binomial = glm(cbind(cousins_birthyear,nr_cousins)~complete_birthyear_focal,
                                            family=binomial('logit'),
                                            data=data_cousins_birthyear)
# Logistic model with outcome (at least one relative with non-missing demographic variable)
model_cousins_completeness_birthyear_logistic = glm(cousins_birth_year~complete_birthyear_focal+nr_cousins,data=data_cousins_birthyear,family = "binomial")



data_cousins_deathyear = cousins_data_set %>%
  filter(!(is.na(cousins_id))) %>%
  mutate(complete_deathyear_cousins = ifelse(!(is.na(death_year_cousins)),1,0),
         complete_deathyear_focal = ifelse(!(is.na(death_year)),1,0)
  ) %>%
  ungroup() %>%
  dplyr::select(profileid,complete_deathyear_cousins,complete_deathyear_focal) %>%
  group_by(profileid,complete_deathyear_focal) %>%
  summarize(nr_cousins = n(),
            prop_cousins_complete_deathyear = sum(complete_deathyear_cousins)/nr_cousins,
            cousins_deathyear = sum(complete_deathyear_cousins)) %>%
  mutate(cousins_death_year = ifelse(prop_cousins_complete_deathyear>0,1,0))


# Negative binomial model with offset
model_cousins_completeness_deathyear_offset = glm.nb(cousins_deathyear~complete_deathyear_focal+offset(log(nr_cousins)),data=data_cousins_deathyear)
# Binomial model
model_cousins_completeness_deathyear_binomial = glm(cbind(cousins_deathyear,nr_cousins)~complete_deathyear_focal,
                                            family=binomial('logit'),
                                            data=data_cousins_deathyear)
# Logistic model with outcome (at least one relative with non-missing demographic variable)
model_cousins_completeness_deathyear_logistic = glm(cousins_death_year~complete_deathyear_focal+nr_cousins,data=data_cousins_deathyear,family = "binomial")



data_cousins_birthcountry = cousins_data_set %>%
  filter(!(is.na(cousins_id))) %>%
  mutate(complete_birthcountry_cousins = ifelse(!(is.na(birth_country_cousins)),1,0),
         complete_birthcountry_focal = ifelse(!(is.na(birth_country)),1,0)
  ) %>%
  ungroup() %>%
  dplyr::select(profileid,complete_birthcountry_cousins,complete_birthcountry_focal) %>%
  group_by(profileid,complete_birthcountry_focal) %>%
  summarize(nr_cousins = n(),
            prop_cousins_complete_birthcountry = sum(complete_birthcountry_cousins)/nr_cousins,
            cousins_birthcountry = sum(complete_birthcountry_cousins)) %>%
  mutate(cousins_complete_birthcountry = ifelse(prop_cousins_complete_birthcountry>0,1,0))


# Negative binomial model with offset
model_cousins_completeness_birthcountry_offset = glm.nb(cousins_birthcountry~complete_birthcountry_focal+offset(log(nr_cousins)),
                                                 data=data_cousins_birthcountry)

# Binomial model
model_cousins_completeness_birthcountry_binomial = glm(cbind(cousins_birthcountry,nr_cousins)~complete_birthcountry_focal,
                                                   family=binomial('logit'),
                                                   data=data_cousins_birthcountry)

# Logistic model with outcome (at least one relative with non-missing demographic variable)

model_cousins_completeness_birthcountry_logistic = glm(cousins_complete_birthcountry~complete_birthcountry_focal+nr_cousins,
                                              data=data_cousins_birthcountry,
                                              family = "binomial")



data_cousins_deathcountry = cousins_data_set %>%
  filter(!(is.na(cousins_id))) %>%
  mutate(complete_deathcountry_cousins = ifelse(!(is.na(death_country_cousins)),1,0),
         complete_deathcountry_focal = ifelse(!(is.na(death_country)),1,0)
  ) %>%
  ungroup() %>%
  dplyr::select(profileid,complete_deathcountry_cousins,complete_deathcountry_focal) %>%
  group_by(profileid,complete_deathcountry_focal) %>%
  summarize(nr_cousins = n(),
            prop_cousins_complete_deathcountry = sum(complete_deathcountry_cousins)/nr_cousins,
            cousins_deathcountry = sum(complete_deathcountry_cousins)) %>%
  mutate(cousins_complete_deathcountry = ifelse(prop_cousins_complete_deathcountry>0,1,0))


# Negative binomial model with offset
model_cousins_completeness_deathcountry_offset = glm.nb(cousins_deathcountry~complete_deathcountry_focal+offset(log(nr_cousins)),
                                                 data=data_cousins_deathcountry)

# Binomial model

model_cousins_completeness_deathcountry_binomial = glm(cbind(cousins_deathcountry,nr_cousins)~complete_deathcountry_focal,
                                                   family=binomial('logit'),
                                                   data=data_cousins_deathcountry)

# Logistic model with outcome (at least one relative with non-missing demographic variable)

model_cousins_completeness_deathcountry_logistic = glm(cousins_complete_deathcountry~complete_deathcountry_focal+nr_cousins,
                                              data=data_cousins_deathcountry,
                                              family = "binomial")




data_cousins_birthdate = cousins_data_set %>%
  filter(!(is.na(cousins_id)),
         !(is.na(birth_year)),
         !(is.na(birth_year_cousins)),
         birth_year>=1600,
         birth_year<=1900) %>%
  mutate(birth_cohorts = case_when(birth_year>=1600 & birth_year<1625 ~ "1600-1624",
                                   birth_year>=1625 & birth_year<1650 ~ "1625-1649",
                                   birth_year>=1650 & birth_year<1675 ~ "1650-1674",
                                   birth_year>=1675 & birth_year<1700 ~ "1675-1699",
                                   birth_year>=1700 & birth_year<1725 ~ "1700-1724",
                                   birth_year>=1725 & birth_year<1750 ~ "1725-1749",
                                   birth_year>=1750 & birth_year<1775 ~ "1750-1774",
                                   birth_year>=1775 & birth_year<1800 ~ "1775-1799",
                                   birth_year>=1800 & birth_year<1825 ~ "1800-1824",
                                   birth_year>=1825 & birth_year<1850 ~ "1825-1849",
                                   birth_year>=1850 & birth_year<1875 ~ "1850-1874",
                                   birth_year>=1875 & birth_year<=1900 ~ "1875-1900",
                                   TRUE ~ NA)) %>%
  mutate(birth_cohorts = factor(birth_cohorts,levels = c("1600-1624","1625-1649","1650-1674",
                                                         "1675-1699","1700-1724","1725-1749",
                                                         "1750-1774","1775-1799","1800-1824",
                                                         "1825-1849","1850-1874","1875-1900"))) %>%
  mutate(complete_birthdate_cousins = ifelse(!(is.na(birth_month_cousins)),1,0),
         complete_birthdate_focal = ifelse(!(is.na(birth_month)),1,0))  %>%
  ungroup() %>%
  dplyr::select(profileid,complete_birthdate_cousins,complete_birthdate_focal,birth_cohorts) %>%
  group_by(profileid,complete_birthdate_focal,birth_cohorts) %>%
  summarize(nr_cousins = n(),
            prop_cousins_complete_birthdate = sum(complete_birthdate_cousins)/nr_cousins,
            cousins_birthdate = sum(complete_birthdate_cousins)) %>%
  mutate(cousins_birth_date = ifelse(prop_cousins_complete_birthdate>0,1,0))



# Negative binomial model with offset
model_cousins_quality_birthdate_offset = glm.nb(cousins_birthdate~complete_birthdate_focal+birth_cohorts+offset(log(nr_cousins)),
                                         data=data_cousins_birthdate)

# Binomial model
model_cousins_quality_birthdate_binomial = glm(cbind(cousins_birthdate,nr_cousins)~complete_birthdate_focal+birth_cohorts,
                                           family=binomial('logit'),
                                           data=data_cousins_birthdate)

# Logistic model with outcome (at least one relative with non-missing demographic variable)
model_cousins_quality_birthdate_logistic = glm(cousins_birth_date~complete_birthdate_focal+birth_cohorts+nr_cousins,data=data_cousins_birthdate,family = "binomial")


data_cousins_deathdate = cousins_data_set %>%
  filter(!(is.na(cousins_id)),
         !(is.na(death_year)),
         !(is.na(death_year_cousins)),
         death_year>=1600,
         death_year<=1900) %>%
  mutate(death_cohorts = case_when(death_year>=1600 & death_year<1625 ~ "1600-1624",
                                   death_year>=1625 & death_year<1650 ~ "1625-1649",
                                   death_year>=1650 & death_year<1675 ~ "1650-1674",
                                   death_year>=1675 & death_year<1700 ~ "1675-1699",
                                   death_year>=1700 & death_year<1725 ~ "1700-1724",
                                   death_year>=1725 & death_year<1750 ~ "1725-1749",
                                   death_year>=1750 & death_year<1775 ~ "1750-1774",
                                   death_year>=1775 & death_year<1800 ~ "1775-1799",
                                   death_year>=1800 & death_year<1825 ~ "1800-1824",
                                   death_year>=1825 & death_year<1850 ~ "1825-1849",
                                   death_year>=1850 & death_year<1875 ~ "1850-1874",
                                   death_year>=1875 & death_year<=1900 ~ "1875-1900",
                                   TRUE ~ NA)) %>%
  mutate(death_cohorts = factor(death_cohorts,levels = c("1600-1624","1625-1649","1650-1674",
                                                         "1675-1699","1700-1724","1725-1749",
                                                         "1750-1774","1775-1799","1800-1824",
                                                         "1825-1849","1850-1874","1875-1900"))) %>%
  mutate(complete_deathdate_cousins = ifelse(!(is.na(death_month_cousins)),1,0),
         complete_deathdate_focal = ifelse(!(is.na(death_month)),1,0)
  )  %>%
  ungroup() %>%
  dplyr::select(profileid,complete_deathdate_cousins,complete_deathdate_focal,death_cohorts) %>%
  group_by(profileid,complete_deathdate_focal,death_cohorts) %>%
  summarize(nr_cousins = n(),
            prop_cousins_complete_deathdate = sum(complete_deathdate_cousins)/nr_cousins,
            cousins_deathdate = sum(complete_deathdate_cousins)) %>%
  mutate(cousins_death_date = ifelse(prop_cousins_complete_deathdate>0,1,0))


# Negative binomial model with offset

model_cousins_quality_deathdate_offset = glm.nb(cousins_deathdate~complete_deathdate_focal+death_cohorts+offset(log(nr_cousins)),
                                         data=data_cousins_deathdate)


# Binomial model
model_cousins_quality_deathdate_binomial = glm(cbind(cousins_deathdate,nr_cousins)~complete_deathdate_focal+death_cohorts,
                                           family=binomial('logit'),
                                           data=data_cousins_deathdate)


# Logistic model with outcome (at least one relative with non-missing demographic variable)

model_cousins_quality_deathdate_logistic = glm(cousins_death_date~complete_deathdate_focal+death_cohorts+nr_cousins,data=data_cousins_deathdate,family = "binomial")



model_cousins_completeness_birthyear_offset = coef(summary(model_cousins_completeness_birthyear_offset))
model_cousins_completeness_birthyear_binomial =  coef(summary(model_cousins_completeness_birthyear_binomial))
model_cousins_completeness_birthyear_logistic =  coef(summary(model_cousins_completeness_birthyear_logistic))
model_cousins_completeness_deathyear_offset = coef(summary(model_cousins_completeness_deathyear_offset))
model_cousins_completeness_deathyear_binomial = coef(summary(model_cousins_completeness_deathyear_binomial))
model_cousins_completeness_deathyear_logistic = coef(summary(model_cousins_completeness_deathyear_logistic))
model_cousins_completeness_birthcountry_offset = coef(summary(model_cousins_completeness_birthcountry_offset))
model_cousins_completeness_birthcountry_binomial = coef(summary(model_cousins_completeness_birthcountry_binomial))
model_cousins_completeness_birthcountry_logistic = coef(summary(model_cousins_completeness_birthcountry_logistic))
model_cousins_completeness_deathcountry_offset = coef(summary(model_cousins_completeness_deathcountry_offset))
model_cousins_completeness_deathcountry_binomial = coef(summary(model_cousins_completeness_deathcountry_binomial))
model_cousins_completeness_deathcountry_logistic = coef(summary(model_cousins_completeness_deathcountry_logistic))
model_cousins_quality_birthdate_offset = coef(summary(model_cousins_quality_birthdate_offset))
model_cousins_quality_birthdate_binomial = coef(summary(model_cousins_quality_birthdate_binomial))
model_cousins_quality_birthdate_logistic = coef(summary(model_cousins_quality_birthdate_logistic))
model_cousins_quality_deathdate_offset = coef(summary(model_cousins_quality_deathdate_offset))
model_cousins_quality_deathdate_binomial = coef(summary(model_cousins_quality_deathdate_binomial))
model_cousins_quality_deathdate_logistic = coef(summary(model_cousins_quality_deathdate_logistic))

save(model_cousins_completeness_birthyear_offset,model_cousins_completeness_deathyear_offset,
     model_cousins_completeness_birthcountry_offset,model_cousins_completeness_deathcountry_offset,
     model_cousins_quality_birthdate_offset,model_cousins_quality_deathdate_offset,
     model_cousins_completeness_birthyear_binomial,model_cousins_completeness_deathyear_binomial,
     model_cousins_completeness_birthcountry_binomial,model_cousins_completeness_deathcountry_binomial,
     model_cousins_quality_birthdate_binomial,model_cousins_quality_deathdate_binomial,
     model_cousins_completeness_birthyear_logistic,model_cousins_completeness_deathyear_logistic,
     model_cousins_completeness_birthcountry_logistic,model_cousins_completeness_deathcountry_logistic,
     model_cousins_quality_birthdate_logistic,model_cousins_quality_deathdate_logistic,
     file="Results/model_cousins_sensitivity_analyses.RData")




