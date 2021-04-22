
# generating summary stats of neighborhood change and travel behaviour in the Toronto region



library(tidyverse)
library(moments)

setwd("~/Dropbox/PhD/paper_neigh_change/analysis_V3")

# loading data

df <- read.csv("data/all_data_2016_CTs_V3.csv")
df <- subset(df,df$cma == 535) # subset for just the Toronto CMA

dfs <- subset(df, df$dw > 0)
dfs$dw_num <- as.numeric(dfs$dw)




# summary of census data
census_summary <- df %>%
  group_by(year) %>%
  summarise(
    population = sum(pop, na.rm = T), 
    dwellings = sum(pop, na.rm = T),
    jobs = sum(d_N_jobs_adult),
    average_income_wm = weighted.mean(inc_avg_inc_16, dw, na.rm = T),
    inc_lico_p_wm = weighted.mean(inc_lico_p, pop, na.rm = T),
    inc_total30p_wm = weighted.mean(inc_total30p, dw, na.rm = T),
    pop_immig_wm = weighted.mean(pop_immig, pop, na.rm = T),
    pop_unemployed_wm = weighted.mean(pop_unemployed, pop, na.rm = T),
    pop_no_high_school_prop_wm = weighted.mean(pop_no_high_school_prop, pop,na.rm = T),
    dw_majorrep_wm = weighted.mean(dw_majorrep, dw, na.rm = T),
    dw_lone_prop_wm = weighted.mean(dw_lone_prop, dw, na.rm = T)
)
census_summary

# write.csv(census_summary, "output_tables/year_summary_census.csv")




#  summary stats of transport / travel behaviour data

df$Ai_combine <- 0.5 * df$Ai_activities_transit / max(df$Ai_activities_transit) + 0.5 * df$Ai_jobs_transit / max(df$Ai_jobs_transit)
df$Ai_activities_transit_scale <- scale(df$Ai_activities_transit)
df$Ai_jobs_transit_scale <- scale(df$Ai_jobs_transit)

tts_summary <- df %>%
  group_by(year) %>%
  summarise(
    T_percent_no_car = weighted.mean(h_N_no_car_hhld / h_N_hhld, h_N_hhld, na.rm = T),
    T_t_access_combine = weighted.mean(Ai_combine, p_N_person_adult, na.rm = T),
    T_t_access_jobs = weighted.mean(Ai_jobs_transit, p_N_person_adult, na.rm = T),
    T_t_access_activities = weighted.mean(Ai_activities_transit, p_N_person_adult, na.rm = T),
    T_r_access_jobs = weighted.mean(Ai_jobs_transit / Ai_jobs_auto, p_N_person_adult, na.rm = T),
    T_r_access_activities = weighted.mean(Ai_activities_transit / Ai_activities_auto, p_N_person_adult, na.rm = T),
    T_commute_duration = weighted.mean(mean_commute_time_adult, p_N_person_adult, na.rm = T),
    T_nonwork_triptime = weighted.mean(mean_nonworktrip_time_adult, p_N_person_adult, na.rm = T),
    T_trips = weighted.mean(p_N_trips_adult / p_N_person_adult, p_N_person_adult, na.rm = T),
    T_activites = weighted.mean(p_N_activities_adult_scaled / p_N_person_adult, p_N_person_adult, na.rm = T)
  )
tts_summary

# write.csv(tts_summary, "output_tables/year_summary_tts.csv")






# estimating the moment for the SES PCA result

temp <- rnorm(1000)
hist(temp)
skewness(temp)

dfpca <- read.csv("data/PCA_SES_Toronto.csv")
temp <- dfpca$SESi
hist(temp)
skewness(temp)



# some plotting 

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


p_pop <- ggplot() + geom_point(aes(census_summary$year, census_summary$population / 1000000)) +
  geom_line(aes(census_summary$year, census_summary$population / 1000000)) +
  scale_x_continuous(breaks = c(1991,1996,2001,2006,2011,2016), minor_breaks = NULL) +
  xlab("") + ylab("Population (millions)") +
  theme_minimal() + 
  theme(text = element_text(size=8))

p_dw <- ggplot() + geom_point(aes(census_summary$year, census_summary$dwellings / 1000000)) +
  geom_line(aes(census_summary$year, census_summary$dwellings / 1000000)) +
  scale_x_continuous(breaks = c(1991,1996,2001,2006,2011,2016), minor_breaks = NULL) +
  xlab("") + ylab("Dwellings (millions)") +
  theme_minimal() + 
  theme(text = element_text(size=8))

p_jobs <- ggplot() + geom_point(aes(census_summary$year, census_summary$jobs / 1000000)) +
  geom_line(aes(census_summary$year, census_summary$jobs / 1000000)) +
  scale_x_continuous(breaks = c(1991,1996,2001,2006,2011,2016), minor_breaks = NULL) +
  xlab("") + ylab("Jobs (millions)") +
  theme_minimal() + 
  theme(text = element_text(size=8))


# increasing in pop, dwellings and jobs over time
multiplot(p_pop,p_dw,p_jobs, cols=3)



# setting up data to plot zero car hhlds by year
summary_plot  <- df %>%
  group_by(year) %>%
  summarise(
    T_percent_no_car = weighted.mean(h_N_no_car_hhld / h_N_hhld, h_N_hhld, na.rm = T),
    population = sum(pop, na.rm = T)
  )

# percent of pop in zero car hhlds
ggplot() + geom_point(aes(summary_plot$year, summary_plot$T_percent_no_car)) +
  geom_line(aes(summary_plot$year, summary_plot$T_percent_no_car)) +
  scale_x_continuous(breaks = c(1991,1996,2001,2006,2011,2016), minor_breaks = NULL) +
  xlab("") + ylab("Percent of population in zero-car households") +
  scale_y_continuous(limits = c(0,0.3)) +
  theme_minimal() 

# total pop in zero car hhlds
ggplot() + geom_point(aes(summary_plot$year, summary_plot$T_percent_no_car * summary_plot$population)) +
  #geom_line(aes(summary_plot$year, summary_plot$T_percent_no_car)) +
  scale_x_continuous(breaks = c(1991,1996,2001,2006,2011,2016), minor_breaks = NULL) +
  xlab("") + ylab("Percent of population in zero-car households") +
  #scale_y_continuous(limits = c(0,0.3)) +
  theme_minimal() 
