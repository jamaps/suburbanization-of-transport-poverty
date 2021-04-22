
# plotting change in variables with respect to transit accessibility (a proxy for urban form or suburbanization)

library(tidyverse)

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

setwd("~/Dropbox/PhD/paper_neigh_change/analysis_V3")

df <- read.csv("data/var_betas.csv")

dfs <- read.csv("data/all_data_2016_CTs_V3.csv")
dfa <- dfs %>% group_by(ctuid) %>%
  summarise(
    Ai_combine_mean = mean(Ai_combine, na.rm = T)
  )


df <- merge(dfa,df, by = "ctuid")

df$Ai_combine_mean <- df$Ai_combine_mean / max(df$Ai_combine_mean)


p1 <- ggplot() + geom_point(aes(x = df$Ai_combine_mean, y = df$SESi_beta), size = 0.5, color = "darkgrey") + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x = df$Ai_combine_mean, y = df$SESi_beta), color = "red", se = TRUE) +
  scale_y_continuous(limits = c(-0.2, 0.2)) + 
  scale_x_continuous(limits = c(0, 1)) + 
  xlab("") + 
  theme_minimal()

p2 <- ggplot() + geom_point(aes(x = df$Ai_combine_mean, y = df$dw_no_car_beta), size = 0.5, color = "darkgrey") + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x = df$Ai_combine_mean, y = df$dw_no_car_beta), color = "red", se = TRUE) +
  scale_y_continuous(limits = c(-0.02, 0.02)) + 
  scale_x_continuous(limits = c(0, 1)) + 
  xlab("") + 
  theme_minimal()

p3 <- ggplot() + geom_point(aes(x = df$Ai_combine_mean, y = df$mean_commute_time_adult_beta), size = 0.5, color = "darkgrey") + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x = df$Ai_combine_mean, y = df$mean_commute_time_adult_beta), color = "red", se = TRUE) +
  scale_y_continuous(limits = c(-1, 1)) + 
  scale_x_continuous(limits = c(0, 1)) + 
  xlab("") + 
  theme_minimal()

p4 <- ggplot() + geom_point(aes(x = df$Ai_combine_mean, y = df$R_activities_per_day_beta), size = 0.5, color = "darkgrey") + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x = df$Ai_combine_mean, y = df$R_activities_per_day_beta), color = "red", se = TRUE) +
  scale_y_continuous(limits = c(-0.04, 0.04)) + 
  scale_x_continuous(limits = c(0, 1)) + 
  xlab("") + 
  theme_minimal()

multiplot(p1,p3,p2,p4,cols=2)






# combine to get an overall variable for transport poverty

df$bg_SES <- scale(df$SESi_beta)
df$bg_Ai_combine <- scale(df$Ai_combine_beta)
df$bg_dw_no_car <- scale(df$dw_no_car_beta)
df$bg_R_activities_per_day <- scale(df$R_activities_per_day_beta)
df$bg_R_mean_commute_time_adult <- scale(df$mean_commute_time_adult_beta)

df$bg_scale <- df$bg_SES + df$bg_Ai_combine - df$bg_dw_no_car + df$bg_R_activities_per_day - df$bg_R_mean_commute_time_adult



dfo <- df[,c("ctuid","bg_mean","bg_median", "bg_scale")]

# write.csv(dfo,"data/beta_groups_counts.csv")

df$Ai_combine_mean <- df$Ai_combine_mean / max(df$Ai_combine_mean)

ggplot() + geom_point(aes(x = df$Ai_combine_mean, y = df$bg_scale), size = 0.5, color = "darkgrey") + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x = df$Ai_combine_mean, y = df$bg_scale), color = "red", se = TRUE) +
  scale_y_continuous(limits = c(-10, 10)) + 
  scale_x_continuous(limits = c(0, 1)) + 
  xlab("") + 
  theme_minimal()
