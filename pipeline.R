allpackages <- function() {
  if(!require(pacman)){install.packages("pacman")}
  pacman::p_load(foreach, ggplot2, plyr, psych, likert)
  library(dplyr)
  library(foreach)
  library(ggplot2)
  library(likert)
  library(psych)
  
  update.packages(ask = FALSE, repos = 'http://cran.rstudio.org')
  return()
}

allpackages() #install all packages for work from my_helper.R
source(here::here("my_helper.R"))  # Helper functions

setwd("/Users/dinatal/Documents/Work/FH OOE/AITentive/Log data from Alex")    
df_main = read.csv('switching_scores_bD202305031621.csv')
df_main <- df_main %>% 
  rename(episode = EpisodeId,
         reaction_t = ReactionTime,
         t_ontask = TimeOnTask,
         friction = DragValue,
         
         joystickX = JoystickAxisX,
         joystickY = JoystickAxisY,
         
         platform_nonX = PlatformAngleSourceX,
         platform_nonY = PlatformAngleSourceY,
         platform_nonZ = PlatformAngleSourceZ,
         
         vel_nonX = BallVelocitySourceX,
         vel_nonY = BallVelocitySourceY,
         vel_nonZ = BallVelocitySourceZ,
         pos_nonX = BallPositionSourceX,
         pos_nonY = BallPositionSourceY,
         pos_nonZ = BallPositionSourceZ,
         
         vel_actX = BallVelocityTargetX,
         vel_actY = BallVelocityTargetY,
         vel_actZ = BallVelocityTargetZ,
         pos_actX = BallPositionTargetX,
         pos_actY = BallPositionTargetY,
         pos_actZ = BallPositionTargetZ
         )

df=df_main %>% select(episode:pos_actZ)
library(stringr)
df$ModelName <- str_replace(df$ModelName, "AUIAUI1solosessionOptS11AUI", "OptModel")
df$ModelName <- str_replace(df$ModelName, "AUIAUI1solosession11k_ad1", "CogModel")

ggplot(df, aes(x=ModelName, y=Duration)) + 
  geom_boxplot(notch = TRUE) +
  geom_signif(comparisons = list(c("NoSupervisor", "CogModel"),
                                 c("OptModel", "CogModel"),
                                 c("RandomSupervisor", "CogModel"),
                                 c("OptModel", "NoSupervisor")),
              test = "t.test",
              test.args=list(paired=TRUE),
              map_signif_level=TRUE,
              y_position = c(255, 270, 285, 240))

pairwise.t.test(df$Duration, df$ModelName)
hist(df$Duration, col=rgb(0,0,1,1/4), main="Distributi")

#df_wide <- spread(df, ModelName, Duration)

#apply(df[ , c('CogModel', 'OptModel', 'NoSupervisor', 'RandomSupervisor')], 2, function(x) exp(mean(log(x))))

df <- df %>% select (PlayerName, Duration, ModelName)
mean_table <- df %>% group_by(PlayerName, ModelName) %>% mutate(mean_time = mean(Duration)) %>% ungroup()  %>% select(-c(Duration)) %>% distinct()
geomean_table <- df %>% group_by(PlayerName, ModelName)  %>% mutate(geomean_time = exp(mean(log(Duration)))) %>% ungroup()  %>% select(-c(Duration)) %>% distinct()
both_means <- mean_table
both_means$geomean_time <- geomean_table$geomean_time
both_means$log_mean_time <- log(both_means$mean_time)
both_means$log_geo_time <- log(both_means$geomean_time)

par(mfrow=c(2,2)) 
hist(both_means$mean_time, col=rgb(0,0,1,1/4), main="Arithmetic mean")
hist(both_means$geomean_time, col=rgb(1,0,0,1/4), main="Geometric mean") 
hist(both_means$log_mean_time, col=rgb(0,1,1,1/4), main="Log AM")
hist(both_means$log_geo_time, col=rgb(1,1,1,1/4), main="Log GM")


data_wide <- spread(mean_table, ModelName, mean_time)
#data_long <- gather(data_wide, ModelName, mean_time, CogModel:RandomSupervisor, factor_key=TRUE)

anova.res <- aov(mean_time ~ ModelName, data = mean_table)
summary(anova.res)
TukeyHSD(anova.res, conf.level=.95) 
plot(TukeyHSD(anova.res, conf.level=.95))

# library(DescTools)
# ScheffeTest(anova.res)

pairwise.t.test(mean_table$mean_time, mean_table$ModelName, p.adjust.method="BH", paired=TRUE)
pairwise.t.test(geomean_table$geomean_time, geomean_table$ModelName, p.adjust.method="BH", paired = TRUE)

pairwise_wilcox_test(both_means, mean_time ~ ModelName, p.adjust.method="BH", paired=TRUE)
pairwise_wilcox_test(both_means, geomean_time ~ ModelName, p.adjust.method="BH", paired=TRUE)

pairwise.t.test(both_means$log_geo_time, both_means$ModelName, p.adjust.method="BH", paired = TRUE)
pairwise.t.test(both_means$log_mean_time, both_means$ModelName, p.adjust.method="BH", paired = TRUE)


ggplot(both_means, aes(x=ModelName, y=geomean_time)) + 
  geom_boxplot(notch = TRUE) +
  geom_signif(comparisons = list(c("NoSupervisor", "CogModel"),
                                 c("OptModel", "CogModel"),
                                 c("RandomSupervisor", "CogModel"),
                                 c("OptModel", "NoSupervisor")),
              #test = "t.test",
              test = "wilcox.test",
              
              test.args=list(paired=TRUE),
              map_signif_level=F,
              y_position = c(245, 260, 275, 230))

#write.csv(data_wide, "data_wide.csv", row.names=FALSE)

both_means %>% group_by(ModelName) %>% get_summary_stats(geomean_time, type = "mean_sd")

both_means %>% group_by(ModelName) %>% identify_outliers(geomean_time)

both_means %>% group_by(ModelName) %>% shapiro_test(geomean_time)

res.aov <- anova_test(data = both_means, dv = geomean_time, wid = PlayerName, within = ModelName)
get_anova_table(res.aov)

data_wide$average <- apply(data_wide %>% select(CogModel:RandomSupervisor), 1, sd, na.rm=TRUE)

