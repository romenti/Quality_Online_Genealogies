#### Install and upload libraries ####

source('R-scripts/upload_packages.R')
source('R-scripts/functions.R')

#### Complete Data set ####

load('Cleaned_Datasets/data_red_var.RData')

#### Birth Year Heaping by country ####

year_heaping_uncomplete = data_red_var %>%
  filter(!is.na(birth_year),
         birth_year>=1600,
         birth_year<=1899) %>%
  mutate(birth_year_cohort=case_when(birth_year>=1600 & birth_year<=1624 ~ "1600-1624",
                                     birth_year>=1625 & birth_year<=1649 ~ "1625-1649",
                                     birth_year>=1650 & birth_year<=1674 ~ "1650-1674",
                                     birth_year>=1675 & birth_year<=1699 ~ "1675-1699",
                                     birth_year>=1700 & birth_year<=1724 ~ "1700-1724",
                                     birth_year>=1725 & birth_year<=1749 ~ "1725-1749",
                                     birth_year>=1750 & birth_year<=1774 ~ "1750-1774",
                                     birth_year>=1775 & birth_year<=1799 ~ "1775-1799",
                                     birth_year>=1800 & birth_year<=1824 ~ "1800-1824",
                                     birth_year>=1825 & birth_year<=1849 ~ "1825-1849",
                                     birth_year>=1850 & birth_year<=1874 ~ "1850-1874",
                                     birth_year>=1875 & birth_year<=1899 ~ "1875-1899")) %>%
  filter(birth_country %in% c("USA", "Germany", "Sweden", "Canada", "UK"),
         is.na(birth_month)) %>%
  group_by(birth_country,birth_year_cohort) %>%
  summarise(count=n(),
            birth_heaping=sum(birth_year %% 10 == 0 | birth_year %% 10 == 5),
            perc_birth_heaping_uncomplete=(birth_heaping/count)*100)

year_heaping_complete=data_red_var %>%
  filter(!is.na(birth_year),
         birth_year>=1600,
         birth_year<=1899) %>%
  mutate(birth_year_cohort=case_when(birth_year>=1600 & birth_year<=1624 ~ "1600-1624",
                                     birth_year>=1625 & birth_year<=1649 ~ "1625-1649",
                                     birth_year>=1650 & birth_year<=1674 ~ "1650-1674",
                                     birth_year>=1675 & birth_year<=1699 ~ "1675-1699",
                                     birth_year>=1700 & birth_year<=1724 ~ "1700-1724",
                                     birth_year>=1725 & birth_year<=1749 ~ "1725-1749",
                                     birth_year>=1750 & birth_year<=1774 ~ "1750-1774",
                                     birth_year>=1775 & birth_year<=1799 ~ "1775-1799",
                                     birth_year>=1800 & birth_year<=1824 ~ "1800-1824",
                                     birth_year>=1825 & birth_year<=1849 ~ "1825-1849",
                                     birth_year>=1850 & birth_year<=1874 ~ "1850-1874",
                                     birth_year>=1875 & birth_year<=1899 ~ "1875-1899")) %>%
  filter(birth_country %in% c("USA", "Germany", "Sweden", "Canada", "UK"),
         !is.na(birth_month)) %>%
  group_by(birth_country,birth_year_cohort) %>%
  summarise(count=n(),
            birth_heaping=sum(birth_year %% 10 == 0 | birth_year %% 10 == 5),
            perc_birth_heaping_complete=(birth_heaping/count)*100)


birth_year_heaping=year_heaping_uncomplete %>%
  dplyr::select(birth_country,birth_year_cohort, perc_birth_heaping_uncomplete) %>%
  left_join(year_heaping_complete %>%
              dplyr::select(birth_country, birth_year_cohort, perc_birth_heaping_complete),
            by=c("birth_country","birth_year_cohort"))


#### Death Year Heaping by country ####

death_year_heaping_uncomplete = data_red_var %>%
  filter(!is.na(death_year),
         death_year>=1600,
         death_year<=1899) %>%
  mutate(death_year_cohort=case_when(death_year>=1600 & death_year<=1624 ~ "1600-1624",
                                     death_year>=1625 & death_year<=1649 ~ "1625-1649",
                                     death_year>=1650 & death_year<=1674 ~ "1650-1674",
                                     death_year>=1675 & death_year<=1699 ~ "1675-1699",
                                     death_year>=1700 & death_year<=1724 ~ "1700-1724",
                                     death_year>=1725 & death_year<=1749 ~ "1725-1749",
                                     death_year>=1750 & death_year<=1774 ~ "1750-1774",
                                     death_year>=1775 & death_year<=1799 ~ "1775-1799",
                                     death_year>=1800 & death_year<=1824 ~ "1800-1824",
                                     death_year>=1825 & death_year<=1849 ~ "1825-1849",
                                     death_year>=1850 & death_year<=1874 ~ "1850-1874",
                                     death_year>=1875 & death_year<=1899 ~ "1875-1899")) %>%
  filter(birth_country %in% c("USA", "Germany", "Sweden", "Canada", "UK"),
         is.na(death_month)) %>%
  group_by(birth_country,death_year_cohort) %>%
  summarise(count=n(),
            death_heaping=sum(death_year %% 10 == 0 | death_year %% 10 == 5),
            perc_death_heaping_uncomplete=(death_heaping/count)*100)

death_year_heaping_complete = data_red_var %>%
  filter(!is.na(death_year),
         death_year>=1600,
         death_year<=1899) %>%
  mutate(death_year_cohort=case_when(death_year>=1600 & death_year<=1624 ~ "1600-1624",
                                     death_year>=1625 & death_year<=1649 ~ "1625-1649",
                                     death_year>=1650 & death_year<=1674 ~ "1650-1674",
                                     death_year>=1675 & death_year<=1699 ~ "1675-1699",
                                     death_year>=1700 & death_year<=1724 ~ "1700-1724",
                                     death_year>=1725 & death_year<=1749 ~ "1725-1749",
                                     death_year>=1750 & death_year<=1774 ~ "1750-1774",
                                     death_year>=1775 & death_year<=1799 ~ "1775-1799",
                                     death_year>=1800 & death_year<=1824 ~ "1800-1824",
                                     death_year>=1825 & death_year<=1849 ~ "1825-1849",
                                     death_year>=1850 & death_year<=1874 ~ "1850-1874",
                                     death_year>=1875 & death_year<=1899 ~ "1875-1899")) %>%
  filter(birth_country %in% c("USA", "Germany", "Sweden", "Canada", "UK"),
         !is.na(death_month)) %>%
  group_by(birth_country,death_year_cohort) %>%
  summarise(count=n(),
            death_heaping=sum(death_year %% 10 == 0 | death_year %% 10 == 5),
            perc_death_heaping_complete=(death_heaping/count)*100)


death_year_heaping = death_year_heaping_uncomplete %>%
  dplyr::select(birth_country,death_year_cohort, perc_death_heaping_uncomplete) %>%
  left_join(death_year_heaping_complete %>%
              dplyr::select(birth_country, death_year_cohort, perc_death_heaping_complete),
            by=c("birth_country","death_year_cohort"))


# year heaping over time, without distinguishing by country of birth

# BIRTH YEAR HEAPING
year_heaping_uncomplete_total = data_red_var %>%
  filter(!is.na(birth_year),
         birth_year>=1600,
         birth_year<=1899) %>%
  mutate(birth_year_cohort=case_when(birth_year>=1600 & birth_year<=1624 ~ "1600-1624",
                                     birth_year>=1625 & birth_year<=1649 ~ "1625-1649",
                                     birth_year>=1650 & birth_year<=1674 ~ "1650-1674",
                                     birth_year>=1675 & birth_year<=1699 ~ "1675-1699",
                                     birth_year>=1700 & birth_year<=1724 ~ "1700-1724",
                                     birth_year>=1725 & birth_year<=1749 ~ "1725-1749",
                                     birth_year>=1750 & birth_year<=1774 ~ "1750-1774",
                                     birth_year>=1775 & birth_year<=1799 ~ "1775-1799",
                                     birth_year>=1800 & birth_year<=1824 ~ "1800-1824",
                                     birth_year>=1825 & birth_year<=1849 ~ "1825-1849",
                                     birth_year>=1850 & birth_year<=1874 ~ "1850-1874",
                                     birth_year>=1875 & birth_year<=1899 ~ "1875-1899")) %>%
  filter(is.na(birth_month)) %>%
  group_by(birth_year_cohort) %>%
  summarise(count=n(),
            birth_heaping=sum(birth_year %% 10 == 0 | birth_year %% 10 == 5),
            perc_birth_heaping_uncomplete=(birth_heaping/count)*100)

year_heaping_complete_total = data_red_var %>%
  filter(!is.na(birth_year),
         birth_year>=1600,
         birth_year<=1899) %>%
  mutate(birth_year_cohort=case_when(birth_year>=1600 & birth_year<=1624 ~ "1600-1624",
                                     birth_year>=1625 & birth_year<=1649 ~ "1625-1649",
                                     birth_year>=1650 & birth_year<=1674 ~ "1650-1674",
                                     birth_year>=1675 & birth_year<=1699 ~ "1675-1699",
                                     birth_year>=1700 & birth_year<=1724 ~ "1700-1724",
                                     birth_year>=1725 & birth_year<=1749 ~ "1725-1749",
                                     birth_year>=1750 & birth_year<=1774 ~ "1750-1774",
                                     birth_year>=1775 & birth_year<=1799 ~ "1775-1799",
                                     birth_year>=1800 & birth_year<=1824 ~ "1800-1824",
                                     birth_year>=1825 & birth_year<=1849 ~ "1825-1849",
                                     birth_year>=1850 & birth_year<=1874 ~ "1850-1874",
                                     birth_year>=1875 & birth_year<=1899 ~ "1875-1899")) %>%
  filter(!is.na(birth_month)) %>%
  group_by(birth_year_cohort) %>%
  summarise(count=n(),
            birth_heaping=sum(birth_year %% 10 == 0 | birth_year %% 10 == 5),
            perc_birth_heaping_complete=(birth_heaping/count)*100)

birth_year_heaping_total = year_heaping_uncomplete_total %>%
  dplyr::select(birth_year_cohort, perc_birth_heaping_uncomplete) %>%
  left_join(year_heaping_complete_total %>%
              dplyr::select(birth_year_cohort, perc_birth_heaping_complete),
            by="birth_year_cohort")

# DEATH YEAR HEAPING
death_year_heaping_uncomplete_total = data_red_var %>%
  filter(!is.na(death_year),
         death_year>=1600,
         death_year<=1899) %>%
  mutate(death_year_cohort=case_when(death_year>=1600 & death_year<=1624 ~ "1600-1624",
                                     death_year>=1625 & death_year<=1649 ~ "1625-1649",
                                     death_year>=1650 & death_year<=1674 ~ "1650-1674",
                                     death_year>=1675 & death_year<=1699 ~ "1675-1699",
                                     death_year>=1700 & death_year<=1724 ~ "1700-1724",
                                     death_year>=1725 & death_year<=1749 ~ "1725-1749",
                                     death_year>=1750 & death_year<=1774 ~ "1750-1774",
                                     death_year>=1775 & death_year<=1799 ~ "1775-1799",
                                     death_year>=1800 & death_year<=1824 ~ "1800-1824",
                                     death_year>=1825 & death_year<=1849 ~ "1825-1849",
                                     death_year>=1850 & death_year<=1874 ~ "1850-1874",
                                     death_year>=1875 & death_year<=1899 ~ "1875-1899")) %>%
  filter(is.na(death_month)) %>%
  group_by(death_year_cohort) %>%
  summarise(count=n(),
            death_heaping=sum(death_year %% 10 == 0 | death_year %% 10 == 5),
            perc_death_heaping_uncomplete=(death_heaping/count)*100)

death_year_heaping_complete_total = data_red_var %>%
  filter(!is.na(death_year),
         death_year>=1600,
         death_year<=1899) %>%
  mutate(death_year_cohort=case_when(death_year>=1600 & death_year<=1624 ~ "1600-1624",
                                     death_year>=1625 & death_year<=1649 ~ "1625-1649",
                                     death_year>=1650 & death_year<=1674 ~ "1650-1674",
                                     death_year>=1675 & death_year<=1699 ~ "1675-1699",
                                     death_year>=1700 & death_year<=1724 ~ "1700-1724",
                                     death_year>=1725 & death_year<=1749 ~ "1725-1749",
                                     death_year>=1750 & death_year<=1774 ~ "1750-1774",
                                     death_year>=1775 & death_year<=1799 ~ "1775-1799",
                                     death_year>=1800 & death_year<=1824 ~ "1800-1824",
                                     death_year>=1825 & death_year<=1849 ~ "1825-1849",
                                     death_year>=1850 & death_year<=1874 ~ "1850-1874",
                                     death_year>=1875 & death_year<=1899 ~ "1875-1899")) %>%
  filter(!is.na(death_month)) %>%
  group_by(death_year_cohort) %>%
  summarise(count=n(),
            death_heaping=sum(death_year %% 10 == 0 | death_year %% 10 == 5),
            perc_death_heaping_complete=(death_heaping/count)*100)


death_year_heaping_total = death_year_heaping_uncomplete_total %>%
  dplyr::select(death_year_cohort, perc_death_heaping_uncomplete) %>%
  left_join(death_year_heaping_complete_total %>%
              dplyr::select(death_year_cohort, perc_death_heaping_complete),
            by="death_year_cohort")


year_heaping_total = birth_year_heaping_total %>%
  left_join(death_year_heaping_total %>%
              rename(birth_year_cohort=death_year_cohort),
            by="birth_year_cohort")



save(year_heaping_total,death_year_heaping,birth_year_heaping,
     file='Results/heaping_results.RData')
