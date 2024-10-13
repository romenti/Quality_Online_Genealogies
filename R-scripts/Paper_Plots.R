#### Install and upload libraries and useful functions ####
source('R-scripts/upload_packages.R')
source('R-scripts/functions.R')


#### Spider plots ####

#### Analytical vs Initial Samples ####

load("Results/spider_chart_data.RData")

radarchart(data_spider_chart1,
           vlcex=2.2,
           cglty = 1,       # Grid line type
           cglcol = "gray", # Grid line color
           cglwd = 2,       # Line width of the grid
           pcol = c("#d7191c","#1b7837"),        # Color of the line
           plwd = 4,        # Width of the line
           plty = 1,
           axistype = 1,
           calcex = 2.5,
           axislabcol = "black") 

legend(x=-2.3,y=1.4,
       legend = rownames(data_spider_chart1[-c(1,2),]),
       bty = "n", pch = 20, col = c("#d7191c","#1b7837"),
       text.col = "grey25", cex=2,pt.cex = 5)


### spider chart conditional on having some demographic information available ###


radarchart(data_spider_chart2,
           vlcex=2.2,
           cglty = 1,       # Grid line type
           cglcol = "gray", # Grid line color
           cglwd = 2,       # Line width of the grid
           pcol = c("#d7191c","#fdae61","#1b7837","#abd9e9","#2c7bb6"),       # Color of the line
           plwd = 4,        # Width of the line
           plty = 1,
           axistype = 1,
           calcex = 2.5,
           axislabcol = "black") + 
  legend(x=1.3, y=1.4,
         legend = rownames(data_spider_chart2[-c(1,2),]),
         bty = "n", pch = 20, col = c("#d7191c","#fdae61","#1b7837","#abd9e9","#2c7bb6"),
         text.col = "grey25",cex = 2, pt.cex = 5)


# Non-missing demographic variables by country of birth in focal sample

radarchart(data_spider_chart3,
           vlcex=2.2,     
           cglty = 1,       # Grid line type
           cglcol = "gray", # Grid line color
           cglwd = 2,       # Line width of the grid
           pcol = c("#d7191c","#fdae61","#1b7837","#abd9e9","#2c7bb6"),        # Color of the line
           plwd = 4,        # Width of the line
           plty = 1,
           axistype = 1,
           calcex = 2.5,
           axislabcol = "black") 
legend(x=-2.3, y=1.4,
      legend = rownames(sample_country_complete[-c(1,2),]),
      bty = "n", pch = 20, col = c("#d7191c","#fdae61","#1b7837","#abd9e9","#2c7bb6"),
      text.col = "grey25",cex=2, pt.cex = 5)

# Non-missing demographic variables by country of birth in inital sample

radarchart(data_spider_chart4,
           vlcex=2.2,     
           cglty = 1,       # Grid line type
           cglcol = "gray", # Grid line color
           cglwd = 2,       # Line width of the grid
           pcol = c("#d7191c","#fdae61","#1b7837","#abd9e9","#2c7bb6"),        # Color of the line
           plwd = 4,        # Width of the line
           plty = 1,
           axistype = 1,
           calcex = 2.5,
           axislabcol = "black") 
legend(x=-2.3, y=1.4,
       legend = rownames(data_spider_chart4[-c(1,2),]),
       bty = "n", pch = 20, col = c("#d7191c","#fdae61","#1b7837","#abd9e9","#2c7bb6"),
       text.col = "grey25",cex=2, pt.cex = 5)


#### Quality plot ####

laod('Results/heaping_results.RData')

# percentage of birth year heaping by country of birth

birth_heaping_country = birth_year_heaping %>%
  pivot_longer(cols=starts_with("perc"), names_to = "dataset", values_to = "perc_birth_heaping")%>%
  ggplot(aes(x=birth_year_cohort, y=perc_birth_heaping, linetype=dataset, color=birth_country,
             group=interaction(birth_country, dataset))) + 
  scale_color_manual(name="Country of birth", values = c("#abd9e9","#fdae61","#1b7837","#2c7bb6","#d7191c"))+
  scale_linetype_discrete(name="Completeness dates", labels=c("Year and month", "Only year"))+
  geom_hline(yintercept=20, 
             color = "black", linewidth=1.5)+
  geom_line(linewidth=1.5)  +
  theme_bw()+
  theme(plot.title = element_text(face="bold",size=18,hjust=0.5),
        axis.text.x = element_text(angle = 45, vjust = 0.5,size=14),
        axis.text.y = element_text(size=14),
        strip.text.x = element_text(face="bold",size=18),
        strip.text.y = element_text(face="bold",size=18),
        axis.title.y = element_text(face="bold",size=14),
        axis.title.x = element_text(face="bold",size=18),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 14))+
  xlab("Birth cohort")+
  ylab("Percentage of years of birth ending with 0 or 5")

ggsave("Figures/plotA2_paper.jpeg", birth_heaping_country, height = 20, width = 40, units = "cm",dpi=700)

# percentage of death year heaping by country of birth

death_heaping_country = death_year_heaping %>%
  pivot_longer(cols=starts_with("perc"), names_to = "dataset", values_to = "perc_death_heaping")%>%
  ggplot(aes(x=death_year_cohort, y=perc_death_heaping, linetype=dataset, color=birth_country,
             group=interaction(birth_country, dataset))) + 
  scale_color_manual(name="Country of birth", values = c("#abd9e9","#fdae61","#1b7837","#2c7bb6","#d7191c"))+
  scale_linetype_discrete(name="Completeness dates", labels=c("Year and month", "Only year"))+
  geom_hline(yintercept=20, 
             color = "black", linewidth=1.5)+
  geom_line(linewidth=1.5)  +
  theme_bw()+
  theme(plot.title = element_text(face="bold",size=18,hjust=0.5),
        axis.text.x = element_text(angle = 45, vjust = 0.5,size=14),
        axis.text.y = element_text(size=14),
        strip.text.x = element_text(face="bold",size=18),
        strip.text.y = element_text(face="bold",size=18),
        axis.title.y = element_text(face="bold",size=14),
        axis.title.x = element_text(face="bold",size=18),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 14))+
  xlab("Death cohort")+
  ylab("Percentage of years of death ending with 0 or 5")    

ggsave("plotA3_paper.jpeg", death_heaping_country, height = 20, width = 40, units = "cm",dpi=700)

# year heaping over time, without distinguishing by country of birth

year_heaping_plot = year_heaping_total %>%
  rename(birth_uncomplete=perc_birth_heaping_uncomplete,
         birth_complete=perc_birth_heaping_complete,
         death_uncomplete=perc_death_heaping_uncomplete,
         death_complete=perc_death_heaping_complete) %>%
  pivot_longer(
    cols = -birth_year_cohort,
    names_to = c("event", "date_completeness"),
    names_sep = "_",
    values_to = "percentage"
  ) %>%
  mutate(event=recode(event, birth="Birth", death="Death")) %>%
  ggplot(aes(x=birth_year_cohort, y=percentage, color=date_completeness,
             group=date_completeness)) + 
  scale_color_manual(name="Completeness dates", labels=c("Year and month", "Only year"), values = c("#4575b4","#d73027"))+
  geom_hline(yintercept=20, 
             color = "black", linewidth=1.5, linetype="dashed")+
  geom_line(linewidth=1.5)  +
  geom_point(size=3.5)+
  theme_bw()+
  facet_wrap(~event)+
  theme(plot.title = element_text(face="bold",size=18,hjust=0.5),
        axis.text.x = element_text(angle = 45, vjust = 0.5,size=12),
        axis.text.y = element_text(size=12),
        strip.text.x = element_text(face="bold",size=16),
        strip.text.y = element_text(face="bold",size=16),
        axis.title.y = element_text(face="bold",size=14),
        axis.title.x = element_text(face="bold",size=14),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 14))+
  xlab("Year")+
  ylab("Percentage of years ending with 0 or 5") 

ggsave("plot3_paper.jpeg", year_heaping_plot, height = 20, width = 40, units = "cm",dpi=700)


#### Regression Plots ####

load('Results/neg_binom_aunts_uncles_results.RData')
load('Results/neg_binom_children_results.RData')
load('Results/neg_binom_cousins_results.RData')
load('Results/neg_binom_grandchild_results.RData')
load('Results/neg_binom_parents_results.RData')
load('Results/neg_binom_grandparents_results.RData')
load('Results/neg_binom_siblings_results.RData')


coef_completeness_birthyear = c(model_aunts_uncles_completeness_birthyear[2,1],
                                model_grandchild_completeness_birthyear[2,1],
                                model_gparents_completeness_birthyear[2,1],
                                model_siblings_completeness_birthyear[2,1],
                                model_cousins_completeness_birthyear[2,1],
                                model_children_completeness_birthyear[2,1],
                                model_parents_completeness_birthyear[2,1])


coef_completeness_deathyear = c(model_aunts_uncles_completeness_deathyear[2,1],
                                model_grandchild_completeness_deathyear[2,1],
                                model_gparents_completeness_deathyear[2,1],
                                model_siblings_completeness_deathyear[2,1],
                                model_cousins_completeness_deathyear[2,1],
                                model_children_completeness_deathyear[2,1],
                                model_parents_completeness_deathyear[2,1])


coef_completeness_birthcountry = c(model_aunts_uncles_completeness_birthcountry[2,1],
                                   model_grandchild_completeness_birthcountry[2,1],
                                   model_gparents_completeness_birthcountry[2,1],
                                   model_siblings_completeness_birthcountry[2,1],
                                   model_cousins_completeness_birthcountry[2,1],
                                   model_children_completeness_birthcountry[2,1],
                                   model_parents_completeness_birthcountry[2,1])


coef_completeness_deathcountry = c(model_aunts_uncles_completeness_deathcountry[2,1],
                                   model_grandchild_completeness_deathcountry[2,1],
                                   model_gparents_completeness_deathcountry[2,1],
                                   model_siblings_completeness_deathcountry[2,1],
                                   model_cousins_completeness_deathcountry[2,1],
                                   model_children_completeness_deathcountry[2,1],
                                   model_parents_completeness_deathcountry[2,1])


coef_quality_birthdate = c(model_aunts_uncles_quality_birthdate[2,1],
                           model_grandchild_quality_birthdate[2,1],
                           model_gparents_quality_birthdate[2,1],
                           model_siblings_quality_birthdate[2,1],
                           model_cousins_quality_birthdate[2,1],
                           model_children_quality_birthdate[2,1],
                           model_parents_quality_birthdate[2,1])

coef_quality_deathdate = c(model_aunts_uncles_quality_deathdate[2,1],
                           model_grandchild_quality_deathdate[2,1],
                           model_gparents_quality_deathdate[2,1],
                           model_siblings_quality_deathdate[2,1],
                           model_cousins_quality_deathdate[2,1],
                           model_children_quality_deathdate[2,1],
                           model_parents_quality_deathdate[2,1])



data_plot_completeness = data.frame(relatives = rep(c("Aunts & uncles","Grandchildren","Grandparents",
                                                      "Siblings","Cousins","Children","Parents"),4),
                                    coefficients = c(coef_completeness_birthcountry,
                                                     coef_completeness_birthyear,
                                                     coef_completeness_deathyear,
                                                     coef_completeness_deathcountry),
                                    labels = rep(c("Birth Country","Birth Year","Death Year","Death Country"),each=7))

data_plot_quality = data.frame(relatives = rep(c("Aunts & uncles","Grandchildren","Grandparents",
                                                 "Siblings","Cousins","Children","Parents"),2),
                               coefficients = c(coef_quality_birthdate,
                                                coef_quality_deathdate),
                               labels = rep(c("Birth date","Death date"),each=7))

data_plot_completeness$exp_coefficients = exp(data_plot_completeness$coefficients)

data_plot_quality$exp_coefficients = exp(data_plot_quality$coefficients)




plot_completeness = data_plot_completeness %>%
  mutate(relatives = factor(relatives,levels=c("Aunts & uncles","Grandchildren","Grandparents","Siblings","Cousins","Children","Parents")),
         labels = factor(labels,levels=c("Birth Year","Death Year","Birth Country","Death Country"),
                         labels=c("Birth year","Death year","Birth country","Death country"))) %>%
  ggplot(aes(x=labels,y=exp_coefficients,color=relatives,shape=relatives))+
  geom_point(position=position_dodge(width=0.5),size=4.5)  +
  ylim(c(0,4))+
  coord_flip() +
  scale_color_manual(values=c("#FF9933", "#3399FF", "#00FF33","#9900CC","#FF0000","#99CC00","#FF0099"),
                     name="Type of Relative",
                     labels=c("Aunts & uncles","Grandchildren","Grandparents","Siblings","Cousins","Children","Parents")) +
  scale_shape_manual(values=c(0,2,3,9,7,8,6),  name="Type of Relative", 
                     labels=c("Aunts & uncles","Grandchildren","Grandparents","Siblings","Cousins","Children","Parents"))+
  theme_bw()+
  geom_hline(yintercept = 1, size = I(0.2), color = I("black")) +
  ylab("Exponentiated regression coefficients") +
  xlab("Demographic variables") +
  theme(plot.title = element_text(face="bold",size=18,hjust=0.5),
        axis.text.x = element_text(angle = 45, vjust = 0.5,size=22),
        axis.text.y = element_text(size=22,face="bold"),
        strip.text.x = element_text(face="bold",size=22),
        strip.text.y = element_text(face="bold",size=22),
        axis.title.y = element_text(face="bold",size=22),
        axis.title.x = element_text(face="bold",size=22),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 25),
        legend.text = element_text(size = 25),
        aspect.ratio = 1) 


ggsave("Figures/plot2_paper.jpeg", plot_completeness, height = 20, width = 40, units = "cm",dpi=700)

plot_quality = data_plot_quality %>%
  mutate(relatives = factor(relatives,levels=c("Aunts & uncles","Grandchildren","Grandparents","Siblings","Cousins","Children","Parents")),
         labels = factor(labels,levels=c("Birth date","Death date"),
                         labels=c("Birth month","Death month"))) %>%
  ggplot(aes(x=labels,y=exp_coefficients,color=relatives,shape=relatives))+
  geom_point(position=position_dodge(width=0.5),size=4.5)  +
  coord_flip() +
  scale_color_manual(values=c("#FF9933", "#3399FF", "#00FF33","#9900CC","#FF0000","#99CC00","#FF0099"),
                     name="Type of Relative",
                     labels=c("Aunts & uncles","Grandchildren","Grandparents","Siblings","Cousins","Children","Parents")) +
  scale_shape_manual(values=c(0,2,3,9,7,8,6),  name="Type of Relative", 
                     labels=c("Aunts & uncles","Grandchildren","Grandparents","Siblings","Cousins","Children","Parents"))+
  ylim(c(0,+4))+
  #scale_y_continuous(breaks = seq(-1,4,.5))+
  theme_bw()+
  geom_hline(yintercept = 1, size = I(0.2), color = I("black")) +
  ylab("Expoentiated regression coefficients") +
  xlab("Demographic variables") +
  theme(plot.title = element_text(face="bold",size=18,hjust=0.5),
        axis.text.x = element_text(angle = 45, vjust = 0.5,size=22),
        axis.text.y = element_text(size=22,face="bold"),
        strip.text.x = element_text(face="bold",size=22),
        strip.text.y = element_text(face="bold",size=22),
        axis.title.y = element_text(face="bold",size=22),
        axis.title.x = element_text(face="bold",size=22),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 25),
        legend.text = element_text(size = 25),
        aspect.ratio = 1) 

ggsave("Figures/plot4_paper.jpeg", plot_quality, height = 20, width = 40, units = "cm",dpi=700)

#### Population Pyramid Plots ####

load('Results/sweden_1751.RData')
load('Results/sweden_1800.RData')
load('Results/sweden_1850.RData')
load('Results/sweden_1900.RData')

data_plot_pyramid = rbind(sweden_1751,sweden_1800,sweden_1850,sweden_1900) %>%
  dplyr::select(-pop_perc_complete_dates,-pop_perc_uncomplete_dates,-counts,-pop_tot_complete_dates,
         -pop_tot_uncomplete_dates) %>%
  pivot_longer(!c("Year","Age","pop_perc","gender","pop_tot"),names_to = "type",values_to = "pop_gen") %>%
  ungroup() %>%
  group_by(Year,type) %>%
  mutate(pop_gen_tot = sum(pop_gen)) %>%
  mutate(pop_gen_exp = pop_gen_tot*pop_perc/100) 

## Population Pyramids in 1751, 1800, 1850, 1900

plot_pyramid = data_plot_pyramid %>%
  mutate(pop_gen=ifelse(gender=="female",-1*pop_gen,pop_gen)) %>%
  mutate(type=factor(type,levels=c("counts_complete_dates","counts_uncomplete_dates"),labels=c("Complete date","Incomplete date"))) %>%
  ggplot()+  # default x-axis is age in years;
  
  # case data graph
  geom_col(mapping = aes(
    x = Age,
    y = pop_gen,
    fill = gender),         
    colour = "white")+       # white around each bar
  geom_step(data =  data_plot_pyramid %>% mutate(type=factor(type,levels=c("counts_complete_dates","counts_uncomplete_dates"),labels=c("Complete date","Incomplete date")),label= 'register population') %>% filter(gender == "female", Year %in% c(1751,1800,1850,1900)), 
            aes(x = Age, y= (-1)*pop_gen_exp,color=label,group=1),size=1) +
  geom_step(data =  data_plot_pyramid %>% mutate(type=factor(type,levels=c("counts_complete_dates","counts_uncomplete_dates"),labels=c("Complete date","Incomplete date")),label= 'register population') %>% filter(gender == "male", Year %in% c(1751,1800,1850,1900)), 
            aes(x = Age, y = pop_gen_exp,color=label,group=1),size=1) +
  scale_y_continuous(labels=scales::percent)+
  # flip the X and Y axes to make pyramid vertical
  coord_flip()+
  scale_fill_manual(name = "Gender",
                    values = c("female" = "#f1a340",
                               "male" = "#7fbf7b"),
                    labels = c("Female", "Male"))+
  scale_color_manual(values = c('register population' = "black"),name="",labels = c("Expected population from HMD"))+
  theme_bw()+
  scale_y_continuous(labels = abs
  )+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,size=18),
        plot.title = element_text(face="bold",size=45,hjust=0.5),
        axis.text.y =  element_text(size=18),
        legend.position = 'bottom',
        axis.title.y = element_text(face="bold",size=25),
        axis.title.x = element_text(face="bold",size=25),
        strip.text.x = element_text(face="bold",size=25), 
        strip.text.y = element_text(face="bold",size=25),
        legend.key.size = unit(1.5, 'cm'), #change legend key size
        legend.key.height = unit(1.5, 'cm'), #change legend key height
        legend.key.width = unit(1.5, 'cm'), #change legend key width
        legend.title = element_text(size=25,face="bold"), #change legend title font size
        legend.text = element_text(size=25,face='bold'))+
  facet_wrap(type~Year,scales =  "free",nrow=2)+
  ylab("Genealogical population")+
  xlab("Age")

ggsave("Figures/plot5_paper.jpeg", plot_pyramid, height = 50, width = 50, units = "cm",dpi=700)

# Difference between the age-sex distribution in percentage between the Swedish population from FamiLinx by quality level and the registered Swedish population over the years 1751, 1800, 1850 and 1900. 

difference_years = sweden_1751 %>%
  rbind(sweden_1800,sweden_1850,sweden_1900)%>%
  mutate(delta_complete=pop_perc_complete_dates-pop_perc,
         delta_uncomplete=pop_perc_uncomplete_dates-pop_perc)%>%
  pivot_longer(cols=starts_with("delta"), names_to = "dataset", values_to = "delta")%>%
  ggplot(aes(x=Age, y=delta, linetype=dataset, color=gender,
             group=interaction(gender, dataset))) + 
  scale_color_manual(name="Gender", values = c("#f1a340","#7fbf7b"))+
  scale_linetype_discrete(name="Completeness dates", labels=c("Year and month", "Only year"))+
  geom_hline(yintercept=0, 
             color = "red", linewidth=1.5)+
  geom_line(linewidth=1.5)  +
  theme_bw()+
  theme(plot.title = element_text(face="bold",size=18,hjust=0.5),
        axis.text.x = element_text(angle = 45, vjust = 0.5,size=14),
        axis.text.y = element_text(size=14),
        strip.text.x = element_text(face="bold",size=18),
        strip.text.y = element_text(face="bold",size=18),
        axis.title.y = element_text(face="bold",size=18),
        axis.title.x = element_text(face="bold",size=18),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 18),
        legend.text = element_text(size = 18))+
  xlab("Age")+
  facet_wrap(~Year)+
  ylab("Difference in %")

ggsave("Figures/figuraA5_paper.jpeg", difference_years, height = 20, width = 40, units = "cm",dpi=700)



# Difference between the age-sex distribution in percentage between the Swedish population from FamiLinx by quality level and the registered Swedish population over the historical period 1751-1900. 

load('Results/differences_pyramid.RData')

difference_ages = sweden_1751_1900 %>%
  mutate(delta_complete=pop_perc_complete_dates-pop_perc,
         delta_uncomplete=pop_perc_uncomplete_dates-pop_perc)%>%
  pivot_longer(cols=starts_with("delta"), names_to = "dataset", values_to = "delta")%>%
  ggplot(aes(x=Year, y=delta, linetype=dataset, color=gender,
             group=interaction(gender, dataset))) + 
  scale_color_manual(name="Gender", values = c("#f1a340","#7fbf7b"))+
  scale_linetype_discrete(name="Completeness dates", labels=c("Year and month", "Only year"))+
  geom_hline(yintercept=0, 
             color = "red", linewidth=1.5)+
  geom_line(linewidth=1.5)  +
  theme_bw()+
  theme(plot.title = element_text(face="bold",size=18,hjust=0.5),
        axis.text.x = element_text(angle = 45, vjust = 0.5,size=14),
        axis.text.y = element_text(size=14),
        strip.text.x = element_text(face="bold",size=18),
        strip.text.y = element_text(face="bold",size=18),
        axis.title.y = element_text(face="bold",size=18),
        axis.title.x = element_text(face="bold",size=18),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 18),
        legend.text = element_text(size = 18))+
  xlab("Year")+
  facet_wrap(~Age)+
  ylab("Difference in %")

ggsave("Figures/plotA4_paper.jpeg", difference_ages, height = 20, width = 40, units = "cm",dpi=700)


#### Life Expectancy Plots ####

load("Results/results_mortality_analysis.RData")

plot_life_exp_30 = life_exp_30_familinx %>%
  mutate(exs=as.numeric(exs),
         CIexlows = as.numeric(CIexlows),
         CIexhighs = as.numeric(CIexhighs)) %>%
  filter(t<=1900) %>%
  ggplot()+
  geom_point(mapping=aes(x=t,y = exs,color=quality,group=quality,shape="16"),lwd=2,size=4)+
  geom_line(mapping=aes(x=t,y = exs,color=quality,group=quality,shape="16"),lwd=1.5,size=4,show.legend=F)+
  geom_ribbon(aes(x=t,ymin=CIexlows,ymax=CIexhighs,fill=quality,shape="16"),alpha=0.3,show.legend = F)+
  facet_wrap(~sex,nrow=3)+
  scale_color_manual(values=c( "#CC0033", "#000099"),labels = c("Birth AND death months","Birth OR death months"),
                     name="Knowledge of date")+
  geom_point(filter(Sweden_LifeTables_hmd_30,t<=1900),mapping=aes(x=t,y=ex,shape="8"),color="black")+
  scale_shape_manual(values=c(16,8),labels=c("FamiLinx","HMD"),name="Data source")+
  ylab("Life expectancy at age 30") +
  xlab("Year")+
  xlim(c(1750,1900))+
  scale_x_continuous(breaks=c(1751,seq(1760,1900,10)))+
  theme_bw() +
  theme(plot.title = element_text(face="bold",size=18,hjust=0.5),
        axis.text.x = element_text(angle = 45, vjust = 0.5,size=18),
        axis.text.y = element_text(size=18),
        strip.text.x = element_text(face="bold",size=22),
        strip.text.y = element_text(face="bold",size=22),
        axis.title.y = element_text(face="bold",size=22),
        axis.title.x = element_text(face="bold",size=22),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 22),
        legend.text = element_text(size=22)) 


ggsave("Figures/plot6_paper.jpeg",plot_life_exp_30,height = 40, width = 40, units = "cm",dpi=700)

plot_life_exp_birth = life_exp_birth_familinx %>%
  mutate(exs=as.numeric(exs),
         CIexlows = as.numeric(CIexlows),
         CIexhighs = as.numeric(CIexhighs)) %>%
  filter(t<=1900) %>%
  ggplot()+
  geom_point(mapping=aes(x=t,y = exs,color=quality,group=quality,shape="16"),size=4)+
  geom_line(mapping=aes(x=t,y = exs,color=quality,group=quality,shape="16"),lwd=1.5,show.legend=F)+
  geom_ribbon(aes(x=t,ymin=CIexlows,ymax=CIexhighs,fill=quality,shape="16"),alpha=0.3,show.legend = F)+
  facet_wrap(~sex,nrow=3)+
  scale_color_manual(values=c( "#CC0033", "#000099"),labels = c("Birth AND death months","Birth OR death months"),
                     name="Knowledge of date")+
  geom_point(filter(Sweden_LifeTables_hmd,t<=1900),mapping=aes(x=t,y=ex,shape="8"),color="black",size=4)+
  scale_shape_manual(values=c(16,8),labels=c("FamiLinx","HMD"),name="Data source")+
  ylab("Life expectancy at birth") +
  xlab("Year")+
  #labs(color = "Completeness")+
  ylim(c(20,80))+
  xlim(c(1750,1900))+
  scale_x_continuous(breaks=c(1751,seq(1760,1900,10)))+
  theme_bw() +
  theme(plot.title = element_text(face="bold",size=22,hjust=0.5),
        axis.text.x = element_text(angle = 45, vjust = 0.5,size=18),
        axis.text.y = element_text(size=18),
        strip.text.x = element_text(face="bold",size=22),
        strip.text.y = element_text(face="bold",size=22),
        axis.title.y = element_text(face="bold",size=22),
        axis.title.x = element_text(face="bold",size=22),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key = element_blank(),
        legend.title = element_text(face = "bold", size = 22),
        legend.text = element_text(size=22)) 

ggsave("Figures/plot6_paper_appendix.jpeg",plot_life_exp_birth,height = 40, width = 40, units = "cm",dpi=700)


