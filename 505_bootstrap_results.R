# Compile results from bootstraped models of future suitable habitat area


 
##### LOAD OBJECTS


bootstrap_files <- list.files("R_objects/0605/") %>%
  str_prefix("R_objects/0605/")
bootstrap <- lapply(bootstrap_files, readRDS)
names(bootstrap) <- substr(bootstrap_files, 26,29)
bootstrap <- bind_rows(bootstrap, .id = "Species")


scenarios <- readRDS("R_objects/scenarios_ch3.RDS")
scenarios <- lapply(scenarios, unwrap)

species_data <- readRDS("R_objects/species_data_ch3_long.RDS")


##### PREP DATA


#add full species names and edit column names

full_names <- data.frame(Species = c("AMGP", "BASA", "BBPL", "BBSA", "DUNL", "PESA", "REPH", "SESA", "STSA", "WRSA"),
                         full_names = c("American Golden-Plover",
                                        "Baird's Sandpiper",
                                        "Black-bellied Plover",
                                        "Buff-breasted Sandpiper",
                                        "Dunlin",
                                        "Pectoral Sandpiper",
                                        "Red Phalarope",
                                        "Semipalmated Sandpiper",
                                        "Stilt Sandpiper", 
                                        "White-rumped Sandpiper"))  





# relevel factors for models and figures
# create factors for identifying different combinations of modelling decisions 
# identify max trend
# edit Kappa statistic so it is never <0 for modelling purposes (rare and quite close to 0)


bootstrap <- merge(bootstrap, full_names) %>%
  mutate(log_trend = log(trend  + 0.00001),
         abs_log_trend = abs(log_trend) ,
         pool = factor(pool, levels = c("climate+additional", "climate-only")),
         selection = factor(selection, levels = c("conventional", "spatial", "none")),
         model = factor(model, levels = c("rf", "glm")),
         dispersal = factor(dispersal, levels = c("5k", "20k", "e"))) %>%
  unite("combination_species", all_of(c("pool", "selection", "model", "scenario", "gcm", "dispersal", "Species")), remove = FALSE) %>%
  unite("combination", all_of(c("pool", "selection", "model", "scenario", "gcm", "dispersal")), remove = FALSE) %>%
  unite("sdm", all_of(c("pool", "selection", "model")), remove = FALSE) %>%
  unite("input", all_of(c("scenario", "gcm", "dispersal")), remove = FALSE) %>%
  group_by(Species) %>%
  mutate(is_max = trend == max(trend)) %>%
  ungroup() %>%
  mutate(Kappa = ifelse(Kappa < 0, 0, Kappa))


# adding probabiloty of half/twice as much habitat

bootstrap <- bootstrap %>%
  mutate(half = ifelse(trend < 0.5, 1, 0),
         twice = ifelse(trend > 2, 1, 0)) %>%
  mutate(pool = relevel(pool, ref = "climate-only"),
         selection = relevel(selection, ref = "none"))




##### MODELS


# all modelling decisions

m1 <- lm(log_trend ~ Species + combination, data = bootstrap, contrasts = list(Species = "contr.sum", combination = "contr.sum"))

x1 <- anova(m1)

x1 <- x1 %>%
  mutate(prop = `Sum Sq` / sum(`Sum Sq`)) %>%
  select(Df, `Sum Sq`, prop)

sum(x1$`Sum Sq`)

write.csv(x1, "model1.csv")



# individual modelling decisions

m2 <- lmer(log_trend ~ (1|Species) + scenario + gcm + dispersal + pool + selection * model, 
           data = bootstrap, contrasts = list(scenario = "contr.sum", 
                                              gcm = "contr.sum",
                                              scenario = "contr.sum",
                                              pool = "contr.sum",
                                              selection = "contr.sum",
                                              model = "contr.sum"))


x2 <- anova(m2) 

x2 <- x2 %>%
  mutate(prop = `Sum Sq` / sum(`Sum Sq`)) %>%
  select(NumDF, `Sum Sq`, prop) %>%
  rename(Df = NumDF) 


sum(x2$`Sum Sq`)

write.csv(x2, "model2.csv")


# probability of half/twice as much habitat


m4 <- glmer(half ~ (1|Species) + scenario + gcm + dispersal + pool + selection * model, 
            data = bootstrap, family = "binomial")

m5 <- glmer(twice ~ (1|Species) + scenario + gcm + dispersal + pool + selection * model, 
            data = bootstrap, family = "binomial")


x4 <- anova(m4)

x4 <- x4 %>%
  mutate(prop = `Sum Sq` / sum(`Sum Sq`)) %>%
  select(npar, `Sum Sq`, prop)

sum(x4$`Sum Sq`)

write.csv(x4, "model4.csv")


x5 <- anova(m5)

x5 <- x5 %>%
  mutate(prop = `Sum Sq` / sum(`Sum Sq`)) %>%
  select(npar, `Sum Sq`, prop)

sum(x5$`Sum Sq`)

write.csv(x5, "model5.csv")





# adding predictions from models to bootstrap data set


bootstrap$predicted_half <- predict(m4, bootstrap, type = "response")
bootstrap$predicted_twice <- predict(m5, bootstrap, type = "response")

saveRDS(bootstrap, "R_objects/bootstrap.RDS")

bootstrap <- readRDS("R_objects/bootstrap.RDS")






##### DESCRIPTIVE STATISTICS



# total plots surveyed
n_plots <- n_unique(species_data$Plot)


# number of plots visited multiple times
n_repeated <- table(species_data[species_data$Species == "AMGP",]$n_visits >1)


# total surveys
n_surveys <- n_unique(species_data$Plot_year)


# number and proportion of plots with species present

prevalence <- species_data %>%
  group_by(Species) %>%
  mutate(presence = ifelse(presence == "no", 0, 1)) %>%
  summarize(plots_present = sum(presence)) %>%
  mutate(prop_present = plots_present/n_surveys)

#number of plots where none of these birds were recorded
plot_count <- species_data %>%
  group_by(Plot) %>%
  summarize(total = sum(max_birds))

sum(plot_count$total == 0)/n_plots

#average number of individuals observed per plot

mean(plot_count$total)

# describe trends

max_trend <- bootstrap %>%
  group_by(full_names) %>%
  summarize(max_trend = max(log_trend))


min_trend <- bootstrap %>%
  group_by(full_names) %>%
  summarize(max_trend = min(log_trend))


median_trend <- bootstrap %>%
  group_by(full_names) %>%
  summarize(median_trend = median(trend))

sd_trend <- bootstrap %>%
  group_by(Species) %>%
  summarize(sd_trend = sd(log_trend))


# relationships between sd and prevalence

sd_trend <- merge(sd_trend, prevalence)

summary(lm(sd_trend ~ plots_present, data = sd_trend))



# Kappa statistic

Kappa_species <- bootstrap %>%
  group_by(Species) %>%
  summarize(Kappa = median(Kappa))

mean(Kappa_species$Kappa)

Kappa_selection_model <- bootstrap %>%
  group_by(selection, model) %>%
  summarize(Kappa = median(Kappa))

write.csv(Kappa_selection_model, "Kappa_selection_model.csv")



#### probability of having half/twice as much habitat

# probability of having half habitat

pl50 <- bootstrap %>%
  mutate(pl50 = ifelse(trend < 0.5, 1, 0)) %>%
  group_by(full_names) %>%
  summarize(prob_2x_less = mean(pl50))

# probability of having twice habitat

pg200 <- bootstrap %>%
  mutate(pg200 = ifelse(trend > 2, 1, 0)) %>%
  group_by(full_names) %>%
  summarize(prob_2x_more = mean(pg200))

# probability of habitat decreasing

pdecrease <- bootstrap %>%
  mutate(p1 = ifelse(trend < 1, 1, 0)) %>%
  group_by(full_names) %>%
  summarize(prob_decrease = mean(p1))

prob_table <- merge(pdecrease, pl50)
prob_table <- merge(prob_table, pg200)

prob_table <- merge(prob_table, full_names)

write.csv(prob_table, "prob_table_0704.csv")






##### PLOTS

#probability distribution plot

#manual log scale so that I can see 0s
#density plot, fixed y axis

pd <- ggplot(data = bootstrap, aes(x = log_trend)) +
  stat_density() +
  theme(legend.position="none",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  facet_wrap(~full_names, ncol = 2) +
  geom_vline(xintercept = 0, linewidth = 0.75) +
  geom_vline(xintercept = log(1/2), colour = "red", linetype = "dotted", linewidth = 0.75) +
  geom_vline(xintercept = log(2), colour = "red", linetype = "dotted", linewidth = 0.75) +
  geom_vline(xintercept = -11.51293, colour = "grey", linewidth = 0.75) +
  geom_vline(data = max_trend, aes(xintercept = max_trend), colour = "grey", linewidth = 0.75) +
  labs(x = "Proportional Change", y = "Probability Distribution") + 
  scale_x_continuous(limits = c(-11.51293, 11.01), breaks = c(-11.51293, -6.907755, -log(10), -log(2), log(2), log(10), 6.907755, 11.0021), labels = c("0", "1/1000", "1/10", "1/2", "2", "10", "1000", "60,000"))


pd

ggsave("probability_distribution.tiff",
       plot = pd, 
       units = "in",
       width = 10, height = 5.5,
       bg = 'white')





# bar plot showing the results of the ANOVAs

x1 <- x1 %>%
  mutate(decision = row.names(x1),
         decision = factor(decision, levels = c("combination", "Species", "Residuals")),
         comparison = c("All modelling decisions effect on proportional change in suitable habitat"))

x2 <- x2 %>%
  mutate(decision = row.names(x2),
         decision = factor(decision, levels = c("pool", "model", "selection", "gcm", "scenario", "dispersal", "selection*model")),
         comparison = c("Individual modelling decisions effect on proportional change in suitable habitat"))

x4 <- x4 %>%
  mutate(decision = row.names(x4),
         decision = factor(decision, levels = c("pool", "model", "selection", "gcm", "scenario", "dispersal", "selection*model")),
         comparison = c("Individual modelling decisions effect on probability of half suitable habitat"))

x5 <- x5 %>%
  mutate(decision = row.names(x4),
         decision = factor(decision, levels = c("pool", "model", "selection", "gcm", "scenario", "dispersal", "selection*model")),
         comparison = c("Individual modelling decisions effect on probability of twice suitable habitat "))



all_plot <- ggplot(x1, aes(y = prop, x = comparison, fill = decision)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(labels = c("Model Configuration", "Species", "Residuals"), 
                    values = c("#5c0101", "#0dd7de", "#fac20a"))  +
  theme(axis.title.x=element_blank(),
        axis.text.x= element_text(face = "bold", size = 11),
        legend.title=element_blank(),
        legend.position="left") +
  ylab("Proportion of Sum of Squares") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  ggtitle("A")

  



individual_plot <- ggplot(x2, aes(y = prop, x = comparison, fill = decision)) +
  geom_bar(position="stack", stat="identity") +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(labels = c("Pool of Variables", "Modelling Algorithm", "Selection of Variables", "Global Circulation Model", 
                               "Carbon Emissions Scenario", "Tree Line Dispersal", "Interaction: Algorithm * Variable Selection"), 
                    values = c("#e85a5a", "#d11111","#5c0101", "#b690e8", "#8e3ef7", "#360675")) +
  theme(axis.title.x=element_blank(),
        axis.text.x= element_text(face = "bold", size = 11),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title=element_blank(),
        legend.position="none") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  ggtitle("B")




ss_half_plot <- ggplot(x4, aes(y = prop, x = comparison, fill = decision)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(labels = c("Pool of Variables", "Modelling Algorithm", "Selection of Variables", "Global Circulation Model", 
                               "Carbon Emissions Scenario", "Tree Line Dispersal", "Interaction: Algorithm * Variable Selection"), 
                    values = c("#e85a5a", "#d11111","#5c0101", "#b690e8", "#8e3ef7", "#360675"))  +
  theme(axis.title.x=element_blank(),
        axis.text.x= element_text(face = "bold", size = 11),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title=element_blank(),
        legend.position="none") +
  ylab("Proportion of Sum of Squares") + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  ggtitle("C")



ss_twice_plot <- ggplot(x5, aes(y = prop, x = comparison, fill = decision)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(labels = c("Pool of Variables", "Modelling Algorithm", "Selection of Variables", "Global Circulation Model", 
                               "Carbon Emissions Scenario", "Tree Line Dispersal", "Interaction: Algorithm * Variable Selection"), 
                    values = c("#e85a5a", "#d11111","#5c0101", "#b690e8", "#8e3ef7", "#360675"))  +
  theme(axis.title.x=element_blank(),
        axis.text.x= element_text(face = "bold", size = 11),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        legend.title=element_blank(),
        legend.position="right")  +
  ylab("Proportion of Sum of Squares") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  ggtitle("D")


ss_plot <- all_plot | individual_plot | ss_half_plot | ss_twice_plot





# bar plots showing effect of modelling decision on half / twice

#half
h1 <- ggplot(bootstrap, aes(x = pool, y = predicted_half)) +
  geom_boxplot() +
  labs(y = str_wrap("Predicted probability of half suitable habitat", width = 25),
       x = "Pool of candidate variables") +
  scale_x_discrete(labels = c("Climate-only", "Climate+additional")) +
  theme(axis.title.y = element_blank())

h2 <- ggplot(bootstrap, aes(x = model, y = predicted_half)) +
  geom_boxplot() +
  labs(y = str_wrap("Predicted probability of half suitable habitat", width = 25),
       x = "Modelling algorithm") +
  scale_x_discrete(labels = c("Random Forest", "GLM")) +
  theme(axis.title.y = element_blank())

h3 <- ggplot(bootstrap, aes(x = selection, y = predicted_half)) +
  geom_boxplot() +
  labs(y = str_wrap("Predicted probability of half suitable habitat", width = 25),
       x = "Selection of included variables")  +
  scale_x_discrete(labels = c("All variables", "Random CV", "Spatial CV")) +
  theme(axis.title.y = element_blank())


h4 <- ggplot(bootstrap, aes(x = gcm, y = predicted_half)) +
  geom_boxplot() +
  labs(y = str_wrap("Predicted probability of half suitable habitat", width = 25),
       x = "Global Circulation Model")  +
  scale_x_discrete(labels = c("ACCESS-CM2", "CanESM5", "HadGEM3")) +
  theme(axis.title.y = element_blank())


h5 <- ggplot(bootstrap, aes(x = scenario, y = predicted_half)) +
  geom_boxplot() +
  labs(y = str_wrap("Predicted probability of half suitable habitat", width = 25),
       x = "Carbon emissions scenario")  +
  scale_x_discrete(labels = c("SSP 245", "SSP 585")) +
  theme(axis.title.y = element_blank())


h6 <- ggplot(bootstrap, aes(x = dispersal, y = predicted_half)) +
  geom_boxplot() +
  labs(y = str_wrap("Predicted probability of half suitable habitat", width = 25),
       x = "Maximum tree line dispersal")  +
  scale_x_discrete(labels = c("5 km", "20 km", "Unrestricted")) +
  theme(axis.title.y = element_blank())


box_plots_half <- grid.arrange(
  patchworkGrob(h1 / h2 / h3 / h4 / h5 / h6 + plot_layout(guides = "collect")), 
  left = "Predicted probability of half suitable habitat")


#twice
t1 <- ggplot(bootstrap, aes(x = pool, y = predicted_twice)) +
  geom_boxplot() +
  labs(y = str_wrap("Predicted probability of twice suitable habitat", width = 25),
       x = "Pool of candidate variables") +
  scale_x_discrete(labels = c("Climate-only", "Climate+additional"))  +
  theme(axis.title.y = element_blank())


t2 <- ggplot(bootstrap, aes(x = model, y = predicted_twice)) +
  geom_boxplot() +
  labs(y = str_wrap("Predicted probability of twice suitable habitat", width = 25),
       x = "Modelling algorithm") +
  scale_x_discrete(labels = c("Random Forest", "GLM")) +
  theme(axis.title.y = element_blank())

t3 <- ggplot(bootstrap, aes(x = selection, y = predicted_twice)) +
  geom_boxplot() +
  labs(y = str_wrap("Predicted probability of twice suitable habitat", width = 25),
       x = "Selection of included variables")  +
  scale_x_discrete(labels = c("All variables", "Random CV", "Spatial CV")) +
  theme(axis.title.y = element_blank())


t4 <- ggplot(bootstrap, aes(x = gcm, y = predicted_twice)) +
  geom_boxplot() +
  labs(y = str_wrap("Predicted probability of twice suitable habitat", width = 25),
       x = "Global Circulation Model")  +
  scale_x_discrete(labels = c("ACCESS-CM2", "CanESM5", "HadGEM3")) +
  theme(axis.title.y = element_blank())


t5 <- ggplot(bootstrap, aes(x = scenario, y = predicted_twice)) +
  geom_boxplot() +
  labs(y = str_wrap("Predicted probability of twice suitable habitat", width = 25),
       x = "Carbon emissions scenario")  +
  scale_x_discrete(labels = c("SSP 245", "SSP 585")) +
  theme(axis.title.y = element_blank())


t6 <- ggplot(bootstrap, aes(x = dispersal, y = predicted_twice)) +
  geom_boxplot() +
  labs(y = str_wrap("Predicted probability of twice suitable habitat", width = 25),
       x = "Maximum tree line dispersal") +
  scale_x_discrete(labels = c("5 km", "20 km", "Unrestricted")) +
  theme(axis.title.y = element_blank())

box_plots_twice <- grid.arrange(
  patchworkGrob(t1 / t2 / t3 / t4 / t5 / t6 + plot_layout(guides = "collect")), 
  left = "Predicted probability of twice suitable habitat")


#combine them together

boxplots_half_twice <- grid.arrange(box_plots_half, box_plots_twice, nrow = 1)

# 1000 tall 750 wide is a good size when saving





##### Carbon Emissions scenarios

# proportional change

carbon_scenarios_trend <- bootstrap %>%
  group_by(Species, scenario) %>%
  summarize(median_trend = median(trend))

carbon_scenarios_trend_dif <- carbon_scenarios_trend %>%
  ungroup() %>%
  pivot_wider(names_from = scenario, values_from = median_trend) %>%
  mutate(dif_trend = (future585 - future245)*100,
         percent_dif_trend = (future585 - future245)/mean(c(future585, future245))*100)

mean(carbon_scenarios_trend_dif$dif_trend)


# probability of half/twice

carbon_scenarios_prob <- bootstrap %>%
  group_by(Species, scenario) %>%
  summarize(mean_half = mean(half),
            mean_twice = mean(twice))

carbon_scenarios_prob_dif <- carbon_scenarios_prob %>%
  ungroup() %>%
  pivot_wider(names_from = scenario, values_from = c(mean_half, mean_twice)) %>%
  mutate(dif_half = mean_half_future585 - mean_half_future245,
         dif_twice = mean_twice_future585 - mean_twice_future245)

mean(carbon_scenarios_prob_dif$dif_half)*100
mean(carbon_scenarios_prob_dif$dif_twice)*100


# what is the mean temp for each of the carbon emission scenarios

mean_annual_temp <- lapply(scenarios, function(x) x <- x$mean_annual_temp)
mean_temps <- lapply(mean_annual_temp, global, fun = "mean", na.rm = TRUE)
mean_temps <- bind_rows(mean_temps, .id = "scenario")
mean_temps$scenario <- substr(mean_temps$scenario, 1, 9)

mean_temps2 <- mean_temps %>%
  group_by(scenario) %>%
  summarize(mean_temp_all = mean(mean),
            sd = sd(mean))


present_temp <- mean_temps2$mean_temp_all[mean_temps2$scenario == "present"]
future245_temp <- mean_temps2$mean_temp_all[mean_temps2$scenario == "future245"]
future585_temp <- mean_temps2$mean_temp_all[mean_temps2$scenario == "future585"]


