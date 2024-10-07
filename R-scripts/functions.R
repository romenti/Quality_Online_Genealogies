#### Functions ####

single_parents = function(df) {
  times = matrix(table(df$child))
  single = as.vector(times==1) 
  return(single)
}


find_second_parent = function(df, ref) {
  parents = ref[ref$child %in% df$child, ]
  new = setdiff(parents, df)
  new_parents = new$parent
  return(new_parents)
}


read_coordinates <- function(profiles, birth_coord=T,
                             baptism_coord=T,burial_coord=T,death_coord=T){
  
  if(birth_coord){
    profiles$birth_location_latitude = ifelse(is.na(profiles$birth_location_longitude) |
                                                profiles$birth_location_latitude==0,
                                              NA,
                                              profiles$birth_location_latitude)
    
    profiles$birth_location_longitude = ifelse(is.na(profiles$birth_location_latitude) |
                                                 profiles$birth_location_longitude==0,NA,
                                               profiles$birth_location_longitude)
    
    data_temp = profiles %>%
      filter(!(is.na(birth_location_longitude)),
             !(is.na(birth_location_latitude))) 
    
    data_temp$birth_coord_country = map.where(database = "world",
                                              data_temp$birth_location_longitude,
                                              data_temp$birth_location_latitude)
    profiles = profiles %>% left_join(data_temp %>%
                                        select(profileid,birth_coord_country),by="profileid")
  }
  if(baptism_coord){
    profiles$baptism_location_latitude = ifelse(is.na(profiles$baptism_location_longitude) |
                                                  profiles$baptism_location_latitude==0,
                                                NA,
                                                profiles$baptism_location_latitude)
    
    profiles$baptism_location_longitude = ifelse(is.na(profiles$baptism_location_latitude) |
                                                   profiles$baptism_location_longitude==0,
                                                 NA,
                                                 profiles$baptism_location_longitude)
    
    data_temp = profiles %>%
      filter(!(is.na(baptism_location_longitude)),
             !(is.na(baptism_location_latitude))) 
    
    data_temp$baptism_coord_country = map.where(database = "world",
                                                data_temp$baptism_location_longitude,
                                                data_temp$baptism_location_latitude)
    profiles = profiles %>% left_join(data_temp %>%
                                        select(profileid,baptism_coord_country),by="profileid")
  }
  
  if(death_coord){
    profiles$death_location_latitude = ifelse(is.na(profiles$death_location_longitude) |
                                                profiles$death_location_latitude==0,
                                              NA,
                                              profiles$death_location_latitude)
    
    profiles$death_location_longitude = ifelse(is.na(profiles$death_location_latitude) |
                                                 profiles$death_location_longitude==0,
                                               NA,
                                               profiles$death_location_longitude)
    
    data_temp = profiles %>%
      filter(!(is.na(death_location_longitude)),
             !(is.na(death_location_latitude))) 
    
    data_temp$death_coord_country = map.where(database = "world",
                                              data_temp$death_location_longitude,
                                              data_temp$death_location_latitude)
    profiles = profiles %>% left_join(data_temp %>%
                                        select(profileid,death_coord_country),by="profileid")
  }
  if(burial_coord){
    profiles$burial_location_latitude = ifelse(is.na(profiles$burial_location_longitude) |
                                                 profiles$burial_location_latitude==0,
                                               NA,
                                               profiles$burial_location_latitude)
    
    profiles$burial_location_longitude = ifelse(is.na(profiles$burial_location_latitude) |
                                                  profiles$burial_location_longitude==0,
                                                NA,
                                                profiles$burial_location_longitude)
    
    data_temp = profiles %>%
      filter(!(is.na(burial_location_longitude)),
             !(is.na(burial_location_latitude))) 
    
    data_temp$burial_coord_country = map.where(database = "world",
                                               data_temp$burial_location_longitude,
                                               data_temp$burial_location_latitude)
    profiles = profiles %>% left_join(data_temp %>%
                                        select(profileid,burial_coord_country),by="profileid")
  }
  return(profiles)
}

subset_links = function(ids, l, add_linking_relatives = T,
                        coherce_both_parents = T) {
  
  # browser()
  keep_df = data.frame(
    ids = ids, 
    linking_rel = F, 
    missing_parent = F
  )
  
  # 1. Subset on original ids
  
  if(add_linking_relatives) {
    
    keep = (l$parent %in% ids) | (l$child %in% ids)
    
    ids_link = unique((unlist(l[keep, ])))
    linking_rel = ids_link[!(ids_link %in% ids)]
    
    if(length(linking_rel) > 0) {
      linking_df = data.frame(
        ids = linking_rel, 
        linking_rel = T, 
        missing_parent = F
      ) 
      
      keep_df = rbind(keep_df, linking_df)
      
    }
    
  }
  
  # This keeps 
  if(coherce_both_parents) {
    
    #DF of 'links' records where current individuals
    # were reported as children
    # We need this to determine whether both parents are 
    # known
    current_df = l[match(keep_df$ids, l$child), ] %>% 
      na.omit
    
    single_rows = single_parents(current_df)
    single_c = current_df[single_rows, ]
    missing_parents = find_second_parent(single_c, l)
    
    if(length(missing_parents) > 0) {
      parents_df = data.frame(
        ids = missing_parents, 
        linking_rel = T, 
        missing_parent = F
      )
      
      keep_df = rbind(keep_df, parents_df)
    }
  }
  
  keep = (l$parent %in% keep_df$ids) | (l$child %in% keep_df$ids)
  out = l[keep, c("child", "parent")]
  return(out)
  
}


subset_profiles = function(links, profile) {
  
  ids = unique(unlist(links))
  prof_sub = profile[ profile$profileid %in% ids, ]
  return(prof_sub)
  
}


recode_profile = function(profiles, recode_birth = T, recode_death = T, 
                          recode_baptism = T, recode_burial = T, recode_NA_values = F) {
  
  print(paste0("Recoding variables..."))
  
  cols_base = c("_location_country_code", "_location_country", "_location_place_name",
                "_location_city","_location_state")
  
  
  
  if(recode_birth) {
    print("on birth")
    
    cols_old = paste0("birth", cols_base)  
    cols_new = paste0(cols_old, "2")
    
    for(n in seq_along(cols_old)) {
      
      profiles[ , cols_new[n] := toupper(get(cols_old[n]))] # data.table
    }
    
  }
  
  if(recode_baptism) {
    print("on baptism")
    
    cols_old = paste0("baptism", cols_base)  
    cols_new = paste0(cols_old, "2")
    
    for(n in seq_along(cols_old)) {
      # profiles[ , cols_new[n]] = toupper(profiles[ , cols_old[n]]) 
      profiles[ , cols_new[n] := toupper(get(cols_old[n]))] # data.table
    }
  }
  
  if(recode_death) {
    print("on death")
    
    cols_old = paste0("death", cols_base)  
    cols_new = paste0(cols_old, "2")
    
    for(n in seq_along(cols_old)) {
      # profiles[ , cols_new[n]] = toupper(profiles[ , cols_old[n]]) 
      profiles[ , cols_new[n] := toupper(get(cols_old[n]))] # data.table
    }
    
  }
  
  if(recode_burial) {
    print("on burial")
    
    cols_old = paste0("burial", cols_base)  
    cols_new = paste0(cols_old, "2")
    
    for(n in seq_along(cols_old)) {
      
      profiles[ , cols_new[n] := toupper(get(cols_old[n]))] # data.table
    }
  }
  
  
  profiles = select(profiles,-ends_with("_location_country_code"),-ends_with("_location_country"),
                    -ends_with("_location_place_name"),-ends_with("_location_city"),-ends_with("_location_state"))
  return(profiles)
  
}


parents_cols = function(links, profiles, infer_gender = T) {
  # browser()
  # To add gender of parent
  l = merge(
    links,
    profiles %>% select(profileid, gender),
    by.x = 'parent',
    by.y = 'profileid'
  ) 
  
  parents = reshape2::dcast(
    l
    , ... ~ gender
    , value.var = 'parent'
    , fun.aggregate = sum
    , na.rm = F
  )
  
  colnames(parents) = c("profileid", "mother", "father", "parent_sex_unknown")
  
  parents[parents==0] = NA
  
  # Fill in missing parents if their gender can be inferred
  if(infer_gender) {
    
    add_f =  !is.na(parents$parent_sex_unknown) & 
      is.na(parents$father) & !is.na(parents$mother)
    
    add_m =  !is.na(parents$parent_sex_unknown) & 
      !is.na(parents$father) & is.na(parents$mother)
    
    parents$father[add_f] = parents$parent_sex_unknown[add_f]
    parents$mother[add_m] = parents$parent_sex_unknown[add_m]
    
    parents$parent_sex_unknown[add_f | add_m] = NA
  }
  
  parents = merge(
    profiles
    , parents
    , by.x = 'profileid'
    , by.y = 'child'
    , all.x = T
  ) %>% 
    dplyr::select(father, mother, parent_sex_unknown)
  
  return(parents)
  
}

filter_country = function(profiles,
                          links,
                          country_codes = NA,
                          regex_codes = NA,
                          filter_on_birth = T,
                          filter_on_death = T,
                          exact_matching = T,
                          regex_matching = T,
                          coord_matching = T,
                          country_name = NA,
                          add_linking_relatives = T, 
                          coherce_both_parents = T){
  print(paste0("Filter only profiles: ", paste(country_codes, collapse = ";")))
  country_codes = toupper(country_codes)
  
  if(exact_matching){
    print("(1/5) Matching location of events using country codes:")
    if(all(is.na(country_codes))){
      stop("You must provide a vector of country codes.")
    } else{
      if(filter_on_birth){
        keep_birth = profiles$birth_location_country_code2 %in% country_codes |
          profiles$baptism_location_country_code2 %in% country_codes 
        profiles$country_birth_code = NA
        profiles$country_birth_code[keep_birth] = country_name
      }
      if(filter_on_death){
        keep_death = profiles$death_location_country_code2 %in% country_codes |
          profiles$burial_location_country_code2 %in% country_codes 
        profiles$country_death_code = NA
        profiles$country_death_code[keep_death] = country_name
      }
      
      
    }
  }
  if(regex_matching){
    print("(1/5) Matching location of events using regex codes:")
    # Use regular expressions for matching country codes or names
    if(all(is.na(regex_codes))) {
      stop("You must provide a vector of regex codes.")
    } else {
      
      regex_codes = toupper(paste0(regex_codes, collapse = "|"))
      
      if(filter_on_birth) {
        print("- matching births")
        keep_birth = 
          grepl(regex_codes , profiles$birth_location_country2) |
          grepl(regex_codes, profiles$birth_location_place_name2) |
          grepl(regex_codes , profiles$baptism_location_country2) |
          grepl(regex_codes, profiles$baptism_location_place_name2)
        
        profiles$country_birth_regex = NA
        profiles$country_birth_regex[keep_birth] = country_name
        
      }
      if(filter_on_death){
        keep_death = 
          grepl(regex_codes , profiles$death_location_country2) |
          grepl(regex_codes, profiles$death_location_place_name2) |
          grepl(regex_codes , profiles$burial_location_country2) |
          grepl(regex_codes, profiles$burial_location_place_name2)
        
        profiles$country_death_regex = NA
        profiles$country_death_regex[keep_death] = country_name
      }
      
    }
    
  }
  if(coord_matching){
    print("(1/5) Matching location of events using countries inferred from coordinates:")
    if(filter_on_birth){
      keep_birth = toupper(profiles$birth_coord_country) %in% country_name |
        toupper(profiles$baptism_coord_country) %in% country_name
      profiles$country_birth_coord_based = NA
      profiles$country_birth_coord_based[keep_birth] = country_name
      
    }
    if(filter_on_death){
      
      keep_death = toupper(profiles$death_coord_country) %in% country_name |
        toupper(profiles$burial_coord_country) %in% country_name
      profiles$country_death_coord_based = NA
      profiles$country_death_coord_based[keep_death] = country_name
      
    }
    
    
  }
  ids_sub = profiles[!(is.na(country_birth_coord_based)) | !(is.na(country_birth_code)) | 
                       !(is.na(country_birth_regex)) | 
                       !(is.na(country_death_coord_based)) |
                       !(is.na(country_death_regex)) |
                       !(is.na(country_death_code))]$profileid
  
  #2. Subset by links first to mantain integrity of the data
  print(paste0("(3/5) Subsetting links..."))
  
  links_sub = subset_links(
    ids = ids_sub
    , l = links
    , add_linking_relatives = add_linking_relatives
    , coherce_both_parents = coherce_both_parents
  )
  
  # 3. Subset the profiles using the link subsetting in step 1
  print("(4/5) Subsetting profiles...")
  prof_sub = subset_profiles(links_sub, profiles)
  
  print("(5/5) Exporting...")
  
  out = list(prof_sub, links_sub)
  
  return(out)
  
}




format_profiles = function(profiles, l, min_year = 1400, max_year = 2100) {
  # browser()
  
  # Transform year columns to numeric and create cohort column
  
  breaks = seq(min_year, max_year, 100)
  labels = ((min_year/100+1):(max_year/100+1))[-length(breaks)]
  
  profiles = 
    profiles %>%
    dplyr::mutate_at(vars(ends_with("_year")), as.numeric) %>%
    mutate(birth_year=ifelse(birth_year<1400 | birth_year>2100,NA,birth_year),
           death_year=ifelse(death_year<1400 |  death_year>2100,NA,death_year),
           birth_month=ifelse(is.na(birth_year) ,NA,birth_month),
           death_month=ifelse(is.na(death_year) ,NA,death_month),
           birth_day=ifelse(is.na(birth_year)|is.na(birth_month),NA,birth_day),
           death_day=ifelse(is.na(death_year)|is.na(death_month),NA,death_day),
           
           age_death=ifelse((death_year-birth_year)<0|(death_year-birth_year)>110,NA,(death_year-birth_year))) %>% 
    # dplyr::filter(dplyr::between(birth_year, min_year, max_year) & 
    #                 dplyr::between(death_year, min_year, max_year)) %>% 
    dplyr::mutate(
      century = cut(birth_year, breaks, include.lowest = TRUE, 
                    right = F, labels = labels)
      # , gender = plyr::mapvalues(gender, c("female", "male"), c("Female", "Male"))
    )
  
  # Recode NAS
  profiles[profiles == "*" | profiles == "unknown"] = NA
  
  
  # ************************
  # TEMP TO AVOID
  # Error:
  # Some vertex names in edge list are not listed in vertex data frame
  # Remove casses that were not filtered right
  # This shoudl be fixed when data are extracted from full familinx!!
  
  ids = unique(unlist(l))
  missing = ! ids %in% profiles$profileid
  miss_id = ids[missing]
  missing_rows = l$child %in% miss_id | l$parent %in% miss_id 
  l = l[!missing_rows, ]
  
  # ************************
  
  # add father and mother column to profile df
  parents_col = parents_cols(
    links = l
    , profiles = profiles
    , infer_gender = T
  )
  
  profiles = cbind(profiles, parents_col) %>% 
    select(profileid, father, mother, parent_sex_unknown, everything())
  
  return(profiles)
  
}

format_profiles_complete = function(profiles, l, min_year = 1400, max_year = 2100) {
  # browser()
  
  # Transform year columns to numeric and create cohort column
  
  breaks = seq(min_year, max_year, 100)
  labels = ((min_year/100+1):(max_year/100+1))[-length(breaks)]
  
  profiles = 
    profiles %>%
    dplyr::mutate_at(vars(ends_with("_year")), as.numeric) %>%
    mutate(birth_year=ifelse(birth_year<1400 | birth_year>2100,NA,birth_year),
           death_year=ifelse(death_year<1400 |  death_year>2100,NA,death_year),
           birth_month=ifelse(is.na(birth_year) ,NA,birth_month),
           death_month=ifelse(is.na(death_year) ,NA,death_month),
           birth_day=ifelse(is.na(birth_year)|is.na(birth_month),NA,birth_day),
           death_day=ifelse(is.na(death_year)|is.na(death_month),NA,death_day),
           
           age_death=ifelse((death_year-birth_year)<0|(death_year-birth_year)>110,NA,(death_year-birth_year))) %>% 
    # dplyr::filter(dplyr::between(birth_year, min_year, max_year) & 
    #                 dplyr::between(death_year, min_year, max_year)) %>% 
    dplyr::mutate(
      century = cut(birth_year, breaks, include.lowest = TRUE, 
                    right = F, labels = labels)
      # , gender = plyr::mapvalues(gender, c("female", "male"), c("Female", "Male"))
    )
  
  # Recode NAS
  profiles[profiles == "*" | profiles == "unknown"] = NA
  
  
  # ************************
  # TEMP TO AVOID
  # Error:
  # Some vertex names in edge list are not listed in vertex data frame
  # Remove casses that were not filtered right
  # This shoudl be fixed when data are extracted from full familinx!!
  
  
  # ************************
  
  # add father and mother column to profile df
  parents_col = parents_cols(
    links = l
    , profiles = profiles
    , infer_gender = T
  )
  
  profiles = cbind(profiles, parents_col) %>% 
    select(profileid, father, mother, parent_sex_unknown, everything())
  
  return(profiles)
  
}

pop_pyramid_calculation = function(data,country_name,year_min=NA,year_max=NA){
  
  if(is.na(country_name) | is.null(data)){
    return(print("Enter the genealogical data set and the country name"))
  } else{
    pop_final = NULL
    pop_final = data %>%
      filter(!(is.na(gender)),
             gender!="unknown",
             !(is.na(birth_year)),
             !(is.na(death_year)),
             (death_year-birth_year)>=0,
             (death_year-birth_year)<=110,
             country_birth_final==country_name,
             country_birth_final==country_death_final) %>%
      mutate(years = purrr::map2(birth_year,death_year,~seq(.x,.y,by=1))) %>%
      unnest(years) %>%
      filter(years>=year_min,
             years<=year_max) %>%
      mutate(age = years-birth_year,
             age_class = case_when(age==0  ~ "0",
                                   age>=1 & age<=4 ~ "1-4",
                                   age>=5 & age<=9 ~ "5-9",
                                   age>=10 & age<=14 ~ "10-14",
                                   age>=15 & age<=19 ~ "15-19",
                                   age>=20 & age<=24 ~ "20-24",
                                   age>=25 & age<=29 ~ "25-29",
                                   age>=30 & age<=34 ~ "30-34",
                                   age>=35 & age<=39 ~ "35-39",
                                   age>=40 & age<=44 ~ "40-44",
                                   age>=45 & age<=49 ~ "45-49",
                                   age>=50 & age<=54 ~ "50-54",
                                   age>=55 & age<=59 ~ "55-59",
                                   age>=60 & age<=64 ~ "60-64",
                                   age>=65 & age<=69 ~ "65-69",
                                   age>=70 & age<=74  ~ "70-74",
                                   age>=75 & age<=79 ~ "75-79",
                                   age>=80  ~ "80+"
             )) %>%
      mutate(age_class = factor(age_class, levels = c("0","1-4","5-9","10-14","15-19",
                                                      "20-24","25-29","30-34","35-39",
                                                      "40-44","45-49","50-54","55-59",
                                                      "60-64","65-69","70-74","75-79",
                                                      "80+")),
             gender = factor(gender,levels = c("male","female"))) %>%
      group_by(years,age_class,gender,.drop=FALSE) %>%
      dplyr::tally() %>%
      #complete(years,age_class,gender, fill = list(n = 0)) %>%
      mutate(country=country_name) %>%
      pivot_wider(names_from = "gender", values_from = "n") %>%
      mutate(total = male+female) %>%
      pivot_longer(!c("years","country","age_class"),names_to = "gender", values_to = "counts")
    return(pop_final)
    
  }
  
}

pop_exposure = function(pop){
  if(is.null(pop)){
    return(print("Return population pyramid"))
  } else{
    data_exp = pop %>%
      ungroup() %>%
      group_by(age_class,country,gender) %>%
      mutate(expos = (counts+lag(counts))/2)
    return(data_exp)
  }
  
}

deaths_calculation = function(data_death,country_name,year_min=NA,year_max=NA){
  if(is.na(country_name) | is.null(data)){
    return(print("Enter the genealogical data set and the country name"))
  } else{
    deaths = NULL
    deaths = data_death %>%
      filter(country_death_final==country_name,
             death_year>=year_min,
             death_year<=year_max,
             !(is.na(gender))) %>%
      rename(country=country_death_final) %>%
      mutate(age_death = death_year-birth_year) %>%
      mutate(age_class = case_when(age_death==0 ~ "0",
                                   age_death>=1 & age_death<=4  ~ "1-4",
                                   age_death>=90 ~ "90+",
                                   age_death>=5 & age_death<=9 ~ "5-9",
                                   age_death>=10 & age_death<=14 ~ "10-14",
                                   age_death>=15 & age_death<=19 ~ "15-19",
                                   age_death>=20 & age_death<=24 ~ "20-24",
                                   age_death>=25 & age_death<=29 ~ "25-29",
                                   age_death>=30 & age_death<=34 ~ "30-34",
                                   age_death>=35 & age_death<=39 ~ "35-39",
                                   age_death>=40 & age_death<=44 ~ "40-44",
                                   age_death>=45 & age_death<=49 ~ "45-49",
                                   age_death>=50 & age_death<=54 ~ "50-54",
                                   age_death>=55 & age_death<=59 ~ "55-59",
                                   age_death>=60 & age_death<=64 ~ "60-64",
                                   age_death>=65 & age_death<=69 ~ "65-69",
                                   age_death>=70 & age_death<=74  ~ "70-74",
                                   age_death>=75 & age_death<=79 ~ "75-79",
                                   age_death>=80 & age_death<=84 ~ "80+"
      )) %>%
      mutate(age_class = factor(age_class, levels = c("0","1-4","5-9","10-14","15-19",
                                                      "20-24","25-29","30-34","35-39",
                                                      "40-44","45-49","50-54","55-59",
                                                      "60-64","65-69","70-74","75-79",
                                                      "80+"))) %>%
      group_by(country,age_class,death_year,gender) %>%
      dplyr::tally() %>%
      pivot_wider(names_from = "gender", values_from = "n") %>%
      mutate(male = ifelse(is.na(male),0,male),
             female = ifelse(is.na(female),0,female)) %>%
      mutate(total = female+male) %>%
      mutate(country=country_name) %>%
      pivot_longer(!c("death_year","country","age_class"),names_to = "gender", values_to = "deaths") %>%
      dplyr::select(country,years=death_year,age_class,gender,deaths)
    return(deaths)
  }
}


#function for only three age classes
pop_pyramid_calculation_2 = function(data,country_name,year_min=NA,year_max=NA){
  
  if(is.na(country_name) | is.null(data)){
    return(print("Enter the genealogical data set and the country name"))
  } else{
    pop_final = NULL
    pop_final = data %>%
      filter(!(is.na(gender)),
             gender!="unknown",
             !(is.na(birth_year)),
             !(is.na(death_year)),
             (death_year-birth_year)>=0,
             (death_year-birth_year)<=110,
             birth_country==country_name,
             birth_country==death_country) %>%
      mutate(years = purrr::map2(birth_year,death_year,~seq(.x,.y,by=1))) %>%
      unnest(years) %>%
      filter(years>=year_min,
             years<=year_max) %>%
      mutate(age = years-birth_year,
             age_class = case_when(age>=0 & age<15 ~ "0-14",
                                   age>=15 & age<65 ~ "15-64",
                                   age>=65  ~ "65+")) %>%
      mutate(age_class = factor(age_class, levels = c("0-14","15-64","65+")),
             gender = factor(gender,levels = c("male","female"))) %>%
      group_by(years,age_class,gender,.drop=FALSE) %>%
      dplyr::tally() %>%
      #complete(years,age_class,gender, fill = list(n = 0)) %>%
      mutate(country=country_name) %>%
      pivot_wider(names_from = "gender", values_from = "n") %>%
      mutate(total = male+female) %>%
      pivot_longer(!c("years","country","age_class"),names_to = "gender", values_to = "counts")
    return(pop_final)
    
  }
  
}







