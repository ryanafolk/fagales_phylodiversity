########################
# Load data sets
########################

library(dplyr)
library(readr)
# Load environmental data; choosing PD as the most high resolution reported variable
env <- list.files(path="./climate_data/spatial_data_climate/PD_P_50km", full.names = TRUE) %>% lapply(read_csv) %>% bind_rows 
env <- data.frame(env)
env[env == -9999] <- NA
env$x <- round(env$x, digit = 1)
env$y <- round(env$y, digit = 1)
env %>% group_by(x, y) %>% summarize_if(is.numeric, mean, na.rm = TRUE) -> env
env <- as.data.frame(env)

# Add RPD randomizations

rand_RPD <- read.csv("./Fagales_CSVs_ToShare/rand_RPD_50km.csv")
rand_RPD$x <- round(rand_RPD$x, digit = 1)
rand_RPD$y <- round(rand_RPD$y, digit = 1)
rand_RPD %>% group_by(x, y) %>% summarize_if(is.numeric, mean, na.rm = TRUE) -> rand_RPD
rand_RPD <- as.data.frame(rand_RPD)
# Add significance column
# Note this is just significantly low RPD
rand_RPD$RPD_significance <- as.factor(ifelse(rand_RPD$value < 0.05, "Low", ifelse(rand_RPD$value > 0.95, "High", "NS")))
rand_RPD$value <- NULL

combined <- merge(rand_RPD, env, by = c("x", "y"))

# Add RPD
RPD <- read.csv("./Fagales_CSVs_ToShare/RPD_50km.csv")
names(RPD) <- c("x", "y", "RPD")
RPD$x <- round(RPD$x, digit = 1)
RPD$y <- round(RPD$y, digit = 1)
RPD %>% group_by(x, y) %>% summarize_if(is.numeric, mean, na.rm = TRUE) -> RPD
RPD <- as.data.frame(RPD)

combined <- merge(combined, RPD, by = c("x", "y"))

# Add CANAPE
CANAPE <- read.csv("./Fagales_CSVs_ToShare/CANAPE.csv")
names(CANAPE) <- c("x", "y", "CANAPE")
CANAPE$x <- round(CANAPE$x, digit = 1)
CANAPE$y <- round(CANAPE$y, digit = 1)
CANAPE %>% group_by(x, y) %>% summarize_if(is.character, max) -> CANAPE
CANAPE <- as.data.frame(CANAPE)
CANAPE$CANAPE <- as.factor(CANAPE$CANAPE)

combined <- merge(combined, CANAPE, by = c("x", "y"))

## Add SR
## This merge drops too many cells so this was only used to test species richness filtering in the models
#SR <- read.csv("./Fagales_CSVs_ToShare/richness_50km.csv")
#names(SR) <- c("x", "y", "SR")
#SR$x <- round(SR$x, digit = 1)
#SR$y <- round(SR$y, digit = 1)
#SR %>% group_by(x, y) %>% summarize_if(is.numeric, mean, na.rm = TRUE) -> SR
#SR <- as.data.frame(SR)

#combined <- merge(combined, SR, by = c("x", "y"))


# Normalize entire data frame
combined.temp <- combined
combined.scaled <- rapply(combined.temp, scale, c("numeric","integer"), how="replace")
combined.scaled$SR <- combined$SR
combined.scaled <- as.data.frame(combined.scaled)
combined.scaled$y <- combined.temp$y
combined.scaled$x <- combined.temp$x


########################
# Model
########################


# RPD model
linear_model_complex <- lm(RPD ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_12 + BIOCLIM_7 + BIOCLIM_17 + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced, data = combined.scaled)
# Top 5 predictors by GLM normalized coefficient
linear_model_simple <- lm(RPD ~ BIOCLIM_1 + BIOCLIM_12 + BIOCLIM_7 + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced, data = combined.scaled)
library(lme4)
mixed_model_complex <- lmer(RPD ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_12 + BIOCLIM_7 + BIOCLIM_17 + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced + (1 | y) + (1 | x), na.action = na.omit, data = combined.scaled)
# Top 5 predictors by LMM normalized coefficient
mixed_model_simple <- lmer(RPD ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_12 + BIOCLIM_17 + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced + (1 | y) + (1 | x), na.action = na.omit, data = combined.scaled)
mixed_model_noenvironment <- lmer(RPD ~ (1 | y) + (1 | x), na.action = na.omit, data = combined.scaled)

AIC(linear_model_complex)
AIC(linear_model_simple)
AIC(mixed_model_complex)
AIC(mixed_model_simple)
AIC(mixed_model_noenvironment)
# Complex mixed model favored

summary(mixed_model_complex)
# read out version with p-values
library(lmerTest)
summary(lmer(RPD ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_12 + BIOCLIM_7 + BIOCLIM_17 + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced + (1 | y) + (1 | x), na.action = na.omit, data = combined.scaled))

library(MuMIn)
r.squaredGLMM(mixed_model_complex)



# CANAPE significance model
combined.scaled$CANAPE_significant <- combined.scaled$CANAPE
levels(combined.scaled$CANAPE_significant)[levels(combined.scaled$CANAPE_significant)=="Neo"] <-"Sig"
levels(combined.scaled$CANAPE_significant)[levels(combined.scaled$CANAPE_significant)=="Mixed"] <-"Sig"
levels(combined.scaled$CANAPE_significant)[levels(combined.scaled$CANAPE_significant)=="Paleo"] <-"Sig"
levels(combined.scaled$CANAPE_significant)
# Reduce factor to 2 levels; otherwise glm guesses and does this silently
# Top 5 variables in simple models chosen based on coefficients in complex logit model
logit_complex <- glm(CANAPE_significant ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_12 + BIOCLIM_7 + BIOCLIM_17 + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced, family = binomial(link='logit'), data = combined.scaled)
logit_simple <- glm(CANAPE_significant ~ aridity_index_UNEP + BIOCLIM_12 + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced, family=binomial(link='logit'), data = combined.scaled)
mixed_model_complex <- glmer(CANAPE_significant ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_12 + BIOCLIM_7 + BIOCLIM_17 + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced + (1 | y) + (1 | x), family=binomial(link='logit'), na.action = na.omit, data = combined.scaled, control = glmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 2e5))) # Stronger likelihood search options per warnings + documentation
mixed_model_simple <- glmer(CANAPE_significant ~ aridity_index_UNEP + BIOCLIM_12 + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced + (1 | y) + (1 | x), family=binomial(link='logit'), na.action = na.omit, data = combined.scaled, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun = 2e5)))
mixed_model_noenvironment <- glmer(CANAPE_significant ~ (1 | y) + (1 | x), family=binomial(link='logit'), na.action = na.omit, data = combined.scaled, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun = 2e5)))

AIC(logit_complex)
AIC(logit_simple)
AIC(mixed_model_complex)
AIC(mixed_model_simple)
AIC(mixed_model_noenvironment)

# Mixed model complex favored
# this summary already has p-values
summary(mixed_model_complex)

library(MuMIn)
r.squaredGLMM(mixed_model_complex)



########################
# Some plots
########################

library(ggplot2)



# Violin plots
ggplot(combined, aes(x = RPD_significance, y = ISRICSOILGRIDS_new_average_phx10percent_reduced, fill = RPD_significance)) + geom_violin(trim=FALSE) + geom_boxplot(width=0.1, fill="white") + labs(title="Aridity vs. RPD significance", x="RPD_significance", y = "UNEP aridity index") + ylim(quantile(combined$ISRICSOILGRIDS_new_average_phx10percent_reduced, 0.025, na.rm = TRUE), quantile(combined$ISRICSOILGRIDS_new_average_phx10percent_reduced, 0.975, na.rm = TRUE))
ggplot(combined, aes(x = CANAPE, y = aridity_index_UNEP, fill = CANAPE)) + geom_violin(trim=FALSE) + geom_boxplot(width=0.1, fill="white") + labs(title="Aridity vs. RPD significance", x="RPD_significance", y = "UNEP aridity index") + ylim(quantile(combined$aridity_index_UNEP, 0.025, na.rm = TRUE), quantile(combined$aridity_index_UNEP, 0.975, na.rm = TRUE))

# Hex plots
# Aridity
ggplot(combined, aes(x = aridity_index_UNEP, y = RPD)) + geom_hex(bins = 35) + scale_fill_continuous(type = "viridis") + theme_bw() + geom_smooth(method='lm', formula = y ~ x) + labs(title="Aridity vs. RPD", x="Aridity index", y = "RPD")
# Latitude
ggplot(combined, aes(x = y, y = RPD)) + geom_hex(bins = 30) + scale_fill_continuous(type = "viridis") + theme_bw() + geom_smooth(method='lm', formula = y ~ x) + labs(title="RPD vs. latitude", y="RPD", x = "Latitude")


# North South Hemisphere low RPD comparison
ggplot(combined[combined$y > 0, ], aes(x = RPD_significance, y = aridity_index_UNEP, fill = RPD_significance)) + geom_violin(trim=FALSE) + geom_boxplot(width=0.1, fill="white") + labs(title="Aridity vs. RPD significance", x="RPD_significance", y = "UNEP aridity index") + ylim(quantile(combined$aridity_index_UNEP, 0.025, na.rm = TRUE), quantile(combined$aridity_index_UNEP, 0.975, na.rm = TRUE))
ggplot(combined[combined$y < 0, ], aes(x = RPD_significance, y = aridity_index_UNEP, fill = RPD_significance)) + geom_violin(trim=FALSE) + geom_boxplot(width=0.1, fill="white") + labs(title="Aridity vs. RPD significance", x="RPD_significance", y = "UNEP aridity index") + ylim(quantile(combined$aridity_index_UNEP, 0.025, na.rm = TRUE), quantile(combined$aridity_index_UNEP, 0.975, na.rm = TRUE))

ggplot(combined[combined$y > 0, ], aes(x = RPD_significance, y = BIOCLIM_1, fill = RPD_significance)) + geom_violin(trim=FALSE) + geom_boxplot(width=0.1, fill="white") + labs(title="Mean annual temperature vs. \nRPD significance", x="RPD_significance", y = "Mean annual temperature") + ylim(quantile(combined$BIOCLIM_1, 0.025, na.rm = TRUE), quantile(combined$BIOCLIM_1, 0.975, na.rm = TRUE))
ggplot(combined[combined$y < 0, ], aes(x = RPD_significance, y = BIOCLIM_1, fill = RPD_significance)) + geom_violin(trim=FALSE) + geom_boxplot(width=0.1, fill="white") + labs(title="Mean annual temperature vs. \nRPD significance", x="RPD_significance", y = "Mean annual temperature") + ylim(quantile(combined$BIOCLIM_1, 0.025, na.rm = TRUE), quantile(combined$BIOCLIM_1, 0.975, na.rm = TRUE))

# Violin figure p-values. Note the p-values contain RPD significance, not RPD, so we need an alternative model class compared to raw RPD models noted above
library(lmerTest)
# north hemisphere
summary(glmer(RPD_significance ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_12 + BIOCLIM_7 + BIOCLIM_17 + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced + (1 | y) + (1 | x), family=binomial(link='logit'), na.action = na.omit, data = combined.scaled[combined.scaled$y > 0, ], control = glmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 2e5))))
# south hemisphere
summary(glmer(RPD_significance ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_12 + BIOCLIM_7 + BIOCLIM_17 + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced + (1 | y) + (1 | x), family=binomial(link='logit'), na.action = na.omit, data = combined.scaled[combined.scaled$y < 0, ], control = glmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 2e5))))



#library(merTools)
#pred <- cbind(combined.scaled[combined.scaled$SR > 3,], predictInterval(mixed_model_complex, combined.scaled[combined.scaled$SR > 3,]))
#
#ggplot(pred) + 
#  geom_line(aes(x, fit)) +
#  geom_ribbon(aes(x, ymin = lwr, ymax = upr), alpha = .2) +
#  geom_point(aes(x, y = RPD))


########################
# Load nodulation data
########################

proportion_nodulating_env <- list.files(path="./climate_data/spatial_data_climate/proportion_nodulating", full.names = TRUE) %>% lapply(read_csv) %>% bind_rows 
proportion_nodulating_env <- data.frame(proportion_nodulating_env)
proportion_nodulating <- read.csv("./Fagales_CSVs_ToShare/proportion_nodulating.csv")

combined_nod <- merge(proportion_nodulating, proportion_nodulating_env, by = c("x", "y"))
combined_nod[combined_nod == -9999] <- NA

combined_nod$x <- round(combined_nod$x, digit = 1)
combined_nod$y <- round(combined_nod$y, digit = 1)
combined_nod %>% group_by(x, y) %>% summarize_if(is.numeric, mean, na.rm = TRUE) -> combined_nod
combined_nod <- as.data.frame(combined_nod)

## Add SR
## Omit; too many dropped cells
#combined_nod <- merge(combined_nod, SR, by = c("x", "y"))


# Normalize entire data frame
# Exclude grid cells without nodulation to address zero-inflation
combined_nod.scaled <- rapply(combined_nod[combined_nod$prop_nod > 0,], scale, c("numeric","integer"), how="replace")
#combined_nod.scaled$SR <- combined_nod[combined_nod$prop_nod > 0,]$SR
combined_nod.scaled$x <- combined_nod[combined_nod$prop_nod > 0,]$x
combined_nod.scaled$y <- combined_nod[combined_nod$prop_nod > 0,]$y
combined_nod.scaled <- as.data.frame(combined_nod.scaled)


########################
# Nodule model
########################

linear_model_complex <- lm(prop_nod ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_12 + BIOCLIM_7 + BIOCLIM_17 + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced, data = combined_nod.scaled)
# Top 5 predictors by GLM normalized coefficient
linear_model_simple <- lm(prop_nod ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_12 + BIOCLIM_7 + ISRICSOILGRIDS_new_average_phx10percent_reduced, data = combined_nod.scaled)
library(lme4)
mixed_model_complex <- lmer(prop_nod ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_12 + BIOCLIM_7 + BIOCLIM_17 + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced + (1 | y) + (1 | x), na.action = na.omit, data = combined_nod.scaled)
# Top 5 predictors by LMM normalized coefficient
mixed_model_simple <- lmer(prop_nod ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_12 + BIOCLIM_7 + ISRICSOILGRIDS_new_average_phx10percent_reduced + (1 | y) + (1 | x), na.action = na.omit, data = combined_nod.scaled)

mixed_model_noenvironment <- lmer(prop_nod ~ (1 | y) + (1 | x), na.action = na.omit, data = combined_nod.scaled)

AIC(linear_model_complex)
AIC(linear_model_simple)
AIC(mixed_model_complex)
AIC(mixed_model_simple)
AIC(mixed_model_noenvironment)
# Complex mixed model favored

summary(mixed_model_complex)
library(lmerTest)
summary(lmer(prop_nod ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_12 + BIOCLIM_7 + BIOCLIM_17 + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced + (1 | y) + (1 | x), na.action = na.omit, data = combined_nod.scaled))
# southern hemisphere
summary(lmer(prop_nod ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_12 + BIOCLIM_7 + BIOCLIM_17 + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced + (1 | y) + (1 | x), na.action = na.omit, data = combined_nod.scaled[combined_nod.scaled$y < 0, ]))


library(MuMIn)
r.squaredGLMM(mixed_model_complex)
# First number is marginal (fixed effects only) and conditional (entire model)


# Get slopes of northern and southern hemisphere aridity relationships
# Northern hemisphere
summary(lm(prop_nod ~ aridity_index_UNEP, data = combined_nod[combined_nod$y > 0, ]))
# Southern hemisphere
summary(lm(prop_nod ~ aridity_index_UNEP, data = combined_nod[combined_nod$y < 0, ]))
# Manuscript results for this hemisphere contrast report data with zeros because there are otherwise too few datapoints left for the southern hemisphere


########################
# Nodulation plots
########################

# Bin size control + color palette
library(ggplot2)
ggplot(combined_nod[combined_nod$prop_nod > 0,], aes(x = aridity_index_UNEP, y = prop_nod) ) + geom_hex(bins = 28) + scale_fill_continuous(type = "viridis") + theme_bw() + ylim(0, 0.9999) + geom_smooth(method='lm', formula = y ~ x) + labs(title="Aridity vs.\nproportion nodulating", x="Aridity index", y = "Proportion nodulating")

ggplot(combined_nod[combined_nod$prop_nod > 0 & combined_nod$y < 0,], aes(x = aridity_index_UNEP, y = prop_nod) ) + geom_hex(bins = 25) + scale_fill_continuous(type = "viridis") + theme_bw() + ylim(0, 0.9999) + geom_smooth(method='lm', formula = y ~ x) + labs(title="Aridity vs.\nproportion nodulating,\nSouthern Hemisphere", x="Aridity index", y = "Proportion nodulating")


ggplot(combined_nod[combined_nod$prop_nod > 0,], aes(x = ISRICSOILGRIDS_new_average_phx10percent_reduced/10, y = prop_nod) ) + geom_hex(bins = 28) + scale_fill_continuous(type = "viridis") + theme_bw() + ylim(0, 1) + geom_smooth(method='lm', formula = y ~ x) + labs(title="Soil pH vs.\nproportion nodulating", x="pH", y = "Proportion nodulating")


#ggplot(combined_nod, aes(x = ISRICSOILGRIDS_new_average_phx10percent_reduced, y = prop_nod) ) + geom_hex(bins = 70) + scale_fill_continuous(type = "viridis") + theme_bw() + ylim(0.01, 0.75) + geom_smooth(method='lm', formula = y ~ x) + labs(title="pH vs.\nproportion nodulating", x="pH", y = "Proportion nodulating")



combined_nod$nod_category <- as.factor(ifelse(combined_nod$prop_nod > 0, "Present", "Absent"))
ggplot(combined_nod[combined_nod$y > 0, ], aes(x = nod_category, y = aridity_index_UNEP, fill = nod_category)) + geom_violin(trim=FALSE) + geom_boxplot(width=0.1, fill="white") + labs(title="Aridity vs. presence of nodulators,\nNorthern Hemisphere", x="Nodulator presence", y = "UNEP aridity index") + ylim(quantile(combined$aridity_index_UNEP, 0.025, na.rm = TRUE), quantile(combined$aridity_index_UNEP, 0.975, na.rm = TRUE))
ggplot(combined_nod[combined_nod$y < 0, ], aes(x = nod_category, y = aridity_index_UNEP, fill = nod_category)) + geom_violin(trim=FALSE) + geom_boxplot(width=0.1, fill="white") + labs(title="Aridity vs. presence of nodulators,\nSouthern Hemisphere", x="Nodulator presence", y = "UNEP aridity index") + ylim(quantile(combined$aridity_index_UNEP, 0.025, na.rm = TRUE), quantile(combined$aridity_index_UNEP, 0.975, na.rm = TRUE))

ggplot(combined_nod[combined_nod$y > 0, ], aes(x = nod_category, y = BIOCLIM_1/10, fill = nod_category)) + geom_violin(trim=FALSE) + geom_boxplot(width=0.1, fill="white") + labs(title="Mean annual temperature vs. \npresence of nodulators,\nSouthern Hemisphere", x="Nodulator presence", y = "Mean annual temperature")
ggplot(combined_nod[combined_nod$y < 0, ], aes(x = nod_category, y = BIOCLIM_1/10, fill = nod_category)) + geom_violin(trim=FALSE) + geom_boxplot(width=0.1, fill="white") + labs(title="Mean annual temperature vs. \npresence of nodulators,\nSouthern Hemisphere", x="Nodulator presence", y = "Mean annual temperature")

# Violin figure p-values. Note the p-values contain RPD significance, not RPD, so we need an alternative model class compared to raw RPD models noted above
combined_nod.scaled$nod_category <- as.factor(ifelse(combined_nod.scaled$prop_nod > 0, "Present", "Absent"))
# north hemisphere
summary(glmer(nod_category ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_12 + BIOCLIM_7 + BIOCLIM_17 + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced + (1 | y) + (1 | x), family=binomial(link='logit'), na.action = na.omit, data = combined_nod.scaled[combined_nod.scaled$y > 0, ], control = glmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 2e5))))
# south hemisphere
summary(glmer(nod_category ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_12 + BIOCLIM_7 + BIOCLIM_17 + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced + (1 | y) + (1 | x), family=binomial(link='logit'), na.action = na.omit, data = combined_nod.scaled[combined_nod.scaled$y < 0, ], control = glmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 2e5))))



#########################
## Investigation of prop_nod/RPD correlation (datasets above do not provide this)
#########################

# Reload proportion nodulating and RPD, merge on a harder lat/long rounding to improve dataset overlap 
proportion_nodulating <- read.csv("./Fagales_CSVs_ToShare/proportion_nodulating.csv")
proportion_nodulating$x <- round(proportion_nodulating$x, digit = 0)
proportion_nodulating$y <- round(proportion_nodulating$y, digit = 0)
proportion_nodulating %>% group_by(x, y) %>% summarize_if(is.numeric, mean, na.rm = TRUE) -> proportion_nodulating
proportion_nodulating <- as.data.frame(proportion_nodulating)

RPD <- read.csv("./Fagales_CSVs_ToShare/RPD_50km.csv")
names(RPD) <- c("x", "y", "RPD")
RPD$x <- round(RPD$x, digit = 0)
RPD$y <- round(RPD$y, digit = 0)
RPD %>% group_by(x, y) %>% summarize_if(is.numeric, mean, na.rm = TRUE) -> RPD
RPD <- as.data.frame(RPD)

nod_rpd <- merge(proportion_nodulating, RPD, by = c("x", "y"))

summary(lm(RPD ~ prop_nod, data = nod_rpd))