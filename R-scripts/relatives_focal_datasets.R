##### Find kin for a focal #####

# upload all packages
source('R-scripts/upload_packages.R')
# upload all functions
source('R-scripts/functions.R')


#### read the Focal data set ####

data_focal = load('Cleaned_Datasets/data_focal.RData')

#### read the complete FamiLinx data set ####

data_var_red = load('Cleaned_Datasets/data_var_red.RData')

# select relevant variable

data_focal = data_focal %>%
  select(profileid,m,f,p_unknown,
         gender,birth_year,birth_month,birth_day,
         death_year,death_month,death_day,
         country_birth_coord_based,country_birth_regex,
         country_birth_code,country_birth_final,
         country_death_final,age_death,
         century)

# links <- data.table::fread(file = "Raw_Data/relations-anon.txt")

parents = load('Cleaned_Datasets/parents.RData')

#### Parents-Focal data ####

parents_data_set = data_focal %>%
  select(profileid,gender,birth_year,death_year,
         m,f,p_unknown,country_birth_final,country_death_final,century) %>%
  left_join(data_var_red %>%
              rename_at(vars(-profileid),function(x) paste0("m_",x)) %>%
              rename(m=profileid), by="m") %>%
  left_join(data_var_red %>%
              rename_at(vars(-profileid),function(x) paste0("f_",x)) %>%
              rename(f=profileid), by="f")  %>%
  left_join(data_var_red %>%
              rename_at(vars(-profileid),function(x) paste0("p_unknown_",x)) %>%
              rename(p_unknown=profileid), by="p_unknown") 

save(parents_data_set,file="Cleaned_Datasets/parents_data_set.RData")

####  Maternal Grandparents - Focal data ####

maternal_grandparents_data_set = data_focal %>%
  select(profileid,gender,birth_year,death_year,
         m,f,p_unknown,country_birth_final,country_death_final,century) %>%
  left_join(parents %>%
              mutate(mother = ifelse(mother==0,NA,mother),
                     father = ifelse(father==0,NA,father),
                     parent_sex_unknown = ifelse(parent_sex_unknown==0,NA,parent_sex_unknown)) %>% 
              dplyr::rename(m=profileid,
                            m_gm=mother,
                            m_gf=father,
                            m_gp_unknown=parent_sex_unknown),
            by="m"
  ) %>%
  left_join(data_var_red %>%
              rename_at(vars(-profileid),function(x) paste0("m_gm_",x)) %>%
              rename(m_gm=profileid), by="m_gm") %>%
  left_join(data_var_red %>%
              rename_at(vars(-profileid),function(x) paste0("m_gf_",x)) %>%
              rename(m_gf=profileid), by="m_gf")  %>%
  left_join(data_var_red %>%
              rename_at(vars(-profileid),function(x) paste0("m_gp_unknown_",x)) %>%
              rename(m_gp_unknown=profileid), by="m_gp_unknown")

save(maternal_grandparents_data_set,file="Cleaned_Datasets/maternal_grandparents_data_set.RData")


#### Parental Grandparents - Focal data set ####


paternal_grandparents_data_set = data_focal %>%
  select(profileid,gender,birth_year,death_year,
         m,f,p_unknown,country_birth_final,country_death_final,century) %>%
  left_join(parents %>%
              mutate(mother = ifelse(mother==0,NA,mother),
                     father = ifelse(father==0,NA,father),
                     parent_sex_unknown = ifelse(parent_sex_unknown==0,NA,parent_sex_unknown)) %>% 
              dplyr::rename(f=profileid,
                            f_gm=mother,
                            f_gf=father,
                            f_gp_unknown=parent_sex_unknown),
            by="f"
  ) %>%
  left_join(data_var_red %>%
              rename_at(vars(-profileid),function(x) paste0("f_gm_",x)) %>%
              rename(f_gm=profileid), by="f_gm") %>%
  left_join(data_var_red %>%
              rename_at(vars(-profileid),function(x) paste0("f_gf_",x)) %>%
              rename(f_gf=profileid), by="f_gf")  %>%
  left_join(data_var_red %>%
              rename_at(vars(-profileid),function(x) paste0("f_gp_unknown_",x)) %>%
              rename(f_gp_unknown=profileid), by="f_gp_unknown") 


save(paternal_grandparents_data_set,file="Cleaned_Datasets/paternal_grandparents_data_set.RData")


#### Siblings - Focal data set ####

siblings_data_set = data_focal %>%
  select(profileid,gender,birth_year,death_year,
         m,f,p_unknown,country_birth_final,country_death_final,century) %>%
  left_join(parents %>%
              mutate(father = ifelse(father==0,NA,father),
                     mother = ifelse(mother==0,NA,mother),
                     parent_sex_unknown = ifelse(parent_sex_unknown == 0, NA, parent_sex_unknown)) %>%
              filter(!(is.na(mother)),!(is.na(father))) %>%
              select(profileid,m=mother,f=father),by=c("m","f")) %>%
  filter(profileid.x!=profileid.y) %>%
  rename(profileid = profileid.x,
         siblings_id = profileid.y) %>%
  group_by(profileid) %>%
  left_join(data_var_red %>%
              select(siblings_id = profileid,
                     siblings_gender = gender,
                     birth_year_siblings = birth_year, 
                     birth_month_siblings = birth_month, 
                     birth_day_siblings = birth_day,
                     death_year_siblings = death_year, 
                     death_month_siblings = death_month, 
                     death_day_siblings = death_day,
                     birth_country_siblings = birth_country,
                     death_country_siblings = death_country,
                     age_death_siblings = age_death,
                     age_death_cl_siblings = age_death_cl,
                     century_siblings = century),
            by="siblings_id") 


save(siblings_data_set,file="Cleaned_Datasets/siblings_data_set.RData")


#### Children - Focal data set ####

children_data_set = data_focal %>%
  select(profileid,gender,birth_year,death_year,
         m,f,p_unknown,country_birth_final,country_death_final,century) %>%
  left_join(links %>%
              rename(profileid=parent,child_id = child),by="profileid") %>%
  left_join(data_var_red %>%
              select(child_id = profileid,
                     child_gender = gender,
                     birth_year_child = birth_year, 
                     birth_month_child = birth_month, 
                     birth_day_child = birth_day,
                     death_year_child = death_year, 
                     death_month_child = death_month, 
                     death_day_child = death_day,
                     birth_country_child = birth_country,
                     death_country_child = death_country,
                     age_death_child = age_death,
                     age_death_cl_child = age_death_cl,
                     century_child = century),
            by="child_id") 


save(children_data_set,file="Cleaned_Datasets/paternal_grandparents_data_set.RData")

#### Grandchildren - Focal data set ####

grandchildren_data_set = data_focal %>%
  select(profileid,gender,birth_year,death_year,
         m,f,p_unknown,country_birth_final,country_death_final,century) %>%
  left_join(links %>%
              rename(profileid=parent,child_id = child),by="profileid") %>%
  left_join(links %>%
              rename(child_id=parent,grandchild_id = child),by="profileid") %>%
  left_join(data_red_var %>%
              select(grandchild_id = profileid,
                     grandchild_gender = gender,
                     birth_year_grandchild = birth_year, 
                     birth_month_grandchild = birth_month, 
                     birth_day_grandchild = birth_day,
                     death_year_grandchild = death_year, 
                     death_month_grandchild = death_month, 
                     death_day_grandchild = death_day,
                     birth_country_grandchild = birth_country,
                     death_country_grandchild = death_country,
                     age_death_grandchild = age_death,
                     age_death_cl_grandchild = age_death_cl,
                     century_grandchild = century),
            by="grandchild_id") 

save(grandchildren_data_set,file="Cleaned_Datasets/paternal_grandparents_data_set.RData")



#### Aunts & Uncles - Focal ####


maternal_aunts_data_set = select(maternal_grandparents,profileid,m,m_gm,m_gf) %>%
  left_join(parents %>%
              filter(!(is.na(mother)),!(is.na(father))) %>%
              rename(m=profileid,m_gm=mother,m_gf=father) %>%
              select(-parent_sex_unknown),by=c("m_gm","m_gf")) %>%
  filter(m.x!=m.y) %>%
  rename(m = m.x,
         aunts_id = m.y) %>%
  group_by(profileid) %>%
  left_join(data_var_red %>%
              select(aunts_id = profileid,
                     aunts_gender = gender,
                     birth_year_aunts = birth_year, 
                     birth_month_aunts = birth_month, 
                     birth_day_aunts = birth_day,
                     death_year_aunts = death_year, 
                     death_month_aunts = death_month, 
                     death_day_aunts = death_day,
                     birth_country_aunts = birth_country,
                     death_country_aunts = death_country,
                     age_death_aunts = age_death,
                     age_death_cl_aunts = age_death_cl,
                     century_aunts = century),
            by="aunts_id") 


paternal_aunts_data_set = select(paternal_grandparents,profileid,f,f_gm,f_gf) %>%
  left_join(parents %>%
              filter(!(is.na(mother)),!(is.na(father))) %>%
              rename(f=profileid,f_gm=mother,f_gf=father) %>%
              select(-parent_sex_unknown),by=c("f_gm","f_gf")) %>%
  filter(f.x!=f.y) %>%
  rename(f = f.x,
         aunts_id = f.y) %>%
  group_by(profileid) %>%
  left_join(data_var_red %>%
              select(aunts_id = profileid,
                     aunts_gender = gender,
                     birth_year_aunts = birth_year, 
                     birth_month_aunts = birth_month, 
                     birth_day_aunts = birth_day,
                     death_year_aunts = death_year, 
                     death_month_aunts = death_month, 
                     death_day_aunts = death_day,
                     birth_country_aunts = birth_country,
                     death_country_aunts = death_country,
                     age_death_aunts = age_death,
                     age_death_cl_aunts = age_death_cl,
                     century_aunts = century),
            by="aunts_id") 

uncles_aunts_data_set = rbind(maternal_aunts_data_set,paternal_aunts_data_set)

save(uncles_aunts_data_set,file="Cleaned_Datasets/uncles_aunts_data_set.RData")

####  Cousins - Focal data set ####

maternal_cousins_data_set = select(maternal_aunts,profileid,aunts_id) %>%
  left_join(links %>%
              rename(aunts_id=parent,
                     cousins_id=child),by="aunts_id") %>%
  group_by(profileid) %>%
  left_join(data_var_red %>%
              select(cousins_id = profileid,
                     cousins_gender = gender,
                     birth_year_cousins = birth_year, 
                     birth_month_cousins = birth_month, 
                     birth_day_cousins = birth_day,
                     death_year_cousins = death_year, 
                     death_month_cousins = death_month, 
                     death_day_cousins = death_day,
                     birth_country_cousins = birth_country,
                     death_country_cousins = death_country,
                     age_death_cousins = age_death,
                     age_death_cl_cousins = age_death_cl,
                     century_cousins = century),
            by="cousins_id") 




parental_cousins_data_set = select(parental_aunts,profileid,aunts_id) %>%
  left_join(links  %>%
              rename(aunts_id=parent,
                     cousins_id=child),by="aunts_id") %>%
  group_by(profileid) %>%
  left_join(data_var_red %>%
              select(parental_cousins_id = profileid,
                     parental_cousins_gender = gender,
                     birth_year_cousins = birth_year, 
                     birth_month_cousins = birth_month, 
                     birth_day_cousins = birth_day,
                     death_year_cousins = death_year, 
                     death_month_cousins = death_month, 
                     death_day_cousins = death_day,
                     birth_country_cousins = birth_country,
                     death_country_cousins = death_country,
                     age_death_cousins = age_death,
                     age_death_cl_cousins = age_death_cl,
                     century_cousins = century),
            by="cousins_id") 

cousins_data_set = rbind(maternal_cousins_data_set,parental_cousins_data_set)

save(cousins_data_set,file="Cleaned_Datasets/cousins_data_set.RData")

