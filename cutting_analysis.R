# Author: Jeremy Boyd (jboyd@ucsd.edu)
# Date: November 5, 2014
# Summary: Analysis of HDP 181 Experiment 1B data: effects of driver SES
# on cutting behavior at all-way stops. NOTE: Datapoint me10 was missing its
# time, so I estimated time as being in between datapoints me09 and me11.

# Load libraries
library(ggplot2)
library(plotrix)
library(timeDate)
library(lme4)
library(plyr)
library(gridExtra)

# Read in data.
cuts = read.delim('Experiment 1B data 140122.txt', header = TRUE)

################################################################
# CLEAN UP DATA
################################################################

# Make cutoff codes uniform
cuts$cutoff = factor(ifelse((cuts$cutoff == 'no' | cuts$cutoff == 'No' | cuts$cutoff == 'N' | cuts$cutoff == 'n'), 'no', ifelse((cuts$cutoff == 'y' | cuts$cutoff == 'Y' | cuts$cutoff == 'yes' | cuts$cutoff == 'Yes'), 'yes', 'other')))

# Make driverSex codes uniform
cuts$driverSex = factor(ifelse((cuts$driverSex == 'f' | cuts$driverSex == 'F' | cuts$driverSex == 'w'), 'f', ifelse((cuts$driverSex == 'm' | cuts $driverSex == 'M'), 'm', 'other')))

# Exclude datapoints with 'other' in the driverSex, driverAge, traffic, and cutoff columns. This amounts to 19 datapoints, or 2.44% of the data (778 originally - 19 = 759.
cuts = cuts[cuts$driverSex != 'other',]
cuts = cuts[cuts$driverAge != 'other',]
cuts = cuts[cuts$traffic != 'other',]
cuts = cuts[cuts$cutoff != 'other',]

# Drop unused 'other' levels from variables.
cuts$driverSex = cuts$driverSex[, drop = TRUE]
cuts$driverAge = cuts$driverAge[, drop = TRUE]
cuts$traffic = cuts$traffic[, drop = TRUE]
cuts$cutoff = cuts$cutoff[, drop = TRUE]
cuts$vehicleID = cuts$vehicleID[, drop = TRUE]

# Make driverAge and traffic numeric
cuts$driverAge = as.numeric(cuts$driverAge)
cuts$traffic = as.numeric(cuts$traffic)

# Make zip code categorical
cuts$zip = factor(cuts$zip)

# Recode time as hours past midnight
cuts$time2 = difftime(timeDate(as.character(cuts$time), format = '%H:%M:%S' ), timeDate('00:00:00', format = '%H:%M:%S'))

# Recode cutoff as numeric: 1 for yes; 0 for no.
cuts$cutoff2 = as.numeric(ifelse(cuts$cutoff == 'yes', 1, 0))

################################################################
# SUMMARIZE USING FIGURES
################################################################

# Interaction of vehicleStatus and traffic, with traffic coded as low (1-2) vs. high (3-6). This gives us 421 datapoints in the low category (55%), and 338 in the high category (45%).
cuts$traffic2 = factor(ifelse(cuts$traffic <= 2, 'low', 'high'))
cuts.sum4 = ddply(cuts, c('vehicleStatus', 'traffic2'), summarise, mean = mean(cutoff2) * 100, se = std.error(cutoff2) * 100)
ggplot(cuts.sum4, aes(x = factor(vehicleStatus), y = mean, color = traffic2, group = traffic2)) + geom_errorbar(aes(ymax = mean + se, ymin = mean - se), color = 'black', width = .15) + geom_line() + geom_point() + scale_x_discrete(name = 'Vehicle Value') + scale_y_continuous(name = 'Cut Likelihood (%)', limits = c(0, 100), breaks = seq(0, 100, 20)) + scale_color_manual(name = 'Traffic', labels = c('Heavy', 'Light'), values = c('firebrick2', 'deepskyblue3')) + theme(legend.position = 'top')

# Create png and pdf versions of the figure.
ggsave(filename = 'vehicleStatusByTraffic.png', width = 3.5, height = 3, dpi = 400)
ggsave(filename = 'vehicleStatusByTraffic.pdf', width = 3.5, height = 3)

# Figure of vehicleStatus predicting proportion of cuts.
cuts.sum1 = ddply(cuts, 'vehicleStatus', summarise, mean = mean(cutoff2), se = std.error(cutoff2))
cuts.sum1$group = 'group'
ggplot(cuts.sum1, aes(x = factor(vehicleStatus), y = mean, group = group)) + geom_line() + geom_point() + geom_errorbar(aes(ymax = mean + se, ymin = mean - se), width = .15) + scale_x_discrete(name = 'Vehicle Status') + scale_y_continuous(name = 'P(Cut)', limits = c(0, 1), breaks = seq(0, 1, .2))

# Make png and pdf of figure
ggsave(filename = 'vehicleStatus.png', width = 6, height = 4.5, dpi = 400)
ggsave(filename = 'vehicleStatus.pdf', width = 6, height = 4.5)

# Figure of traffic predicting proportion of cuts.
cuts.sum7 = ddply(cuts, 'traffic', summarise, mean = mean(cutoff2) * 100, se = std.error(cutoff2) * 100, n = length(cutoff2))
cuts.sum7$group = 'group'
ggplot(cuts.sum7, aes(x = factor(traffic), y = mean, group = group)) + geom_line() + geom_point() + geom_errorbar(aes(ymax = mean + se, ymin = mean - se), width = .15) + scale_x_discrete(name = 'Traffic') + scale_y_continuous(name = 'Cut Likelihood (%)', limits = c(0, 100)) + geom_text(aes(label = n, y = mean + se + .06))

# Make png and pdf of figure.
ggsave(filename = 'traffic.png', width = 6, height = 4.5, dpi = 400)
ggsave(filename = 'traffic.pdf', width = 6, height = 4.5)

# Piff's low and high cut numbers: 10-30
# so vehicleStatus 1=10, 2-15, 3=20, 4=25, 5=30
# Make mine like this to center in figure better: 1=30, 2=35, 3=40, 4=45, 5=50
# For no interaction figure, just add/subtract 10 for each value

# Make a prediction figure for no interaction
noInter = data.frame(vehicleStatus = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5), traffic = c('high', 'low', 'high', 'low', 'high', 'low', 'high', 'low', 'high', 'low'), mean = c(40, 20, 46, 26, 52, 32, 58, 38, 64, 44))
pred1 = ggplot(noInter, aes(x = factor(vehicleStatus), y = mean, color = traffic, group = traffic)) + geom_line() + geom_point() + scale_x_discrete(name = 'Driver SES') + scale_y_continuous(name = 'Cut Likelihood', limits = c(0, 100), breaks = seq(0, 100, 20)) + scale_color_manual(name = 'Traffic', labels = c('Heavy', 'Light'), values = c('firebrick2', 'deepskyblue3')) + theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank()) +  ggtitle("Ethics")

# Make interaction prediction figure
inter = data.frame(vehicleStatus = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5), traffic = c('high', 'low', 'high', 'low', 'high', 'low', 'high', 'low', 'high', 'low'), mean = c(30, 30, 40, 32, 50, 34, 60, 36, 70, 38))
pred2 = ggplot(inter, aes(x = factor(vehicleStatus), y = mean, color = traffic, group = traffic)) + geom_line() + geom_point() + scale_x_discrete(name = 'Driver SES') + scale_y_continuous(name = 'Cut Likelihood', limits = c(0, 100), breaks = seq(0, 100, 20)) + scale_color_manual(name = 'Traffic', labels = c('Heavy',  'Light'), values = c('firebrick2', 'deepskyblue3')) + theme(axis.text.y = element_blank(), axis.text.x = element_blank(), legend.position = 'none') + ggtitle("Attention")

# Combine into 1 x 2 plot
pdf('predictions.pdf', width = 7.25, height = 2.3)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 200)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(pred2, vp = vplayout(1, 1:87))
print(pred1, vp = vplayout(1, 88:200))
dev.off()

################################################################
# FIGURES/TABLES SUMMARIZING THE DATA
################################################################

# Amount of data at each level of status x traffic.
xtabs(~ vehicleStatus + traffic, cuts)

# Counts and proportions of data based on sex.
xtabs(~ driverSex, cuts)
xtabs(~ driverSex, cuts) / length(cuts$driverSex)

# Counts and proportions of data based on age
xtabs(~ driverAge, cuts)
xtabs(~ driverAge, cuts) / length(cuts$driverAge)

# Set font sizes for figures
fontSize = 3

# Frequency distribution of vehicle status. Shows a median vehicle status of 3 (n = 220).
figA = ggplot(cuts, aes(x = factor(vehicleStatus))) + geom_histogram(binwidth = 1, color = 'black', fill = 'gray50', width = 1) + geom_text(size = fontSize, stat = 'bin', binwidth = 1, aes(y = ..count.., label = sprintf("%.1f%%", ..count.. / 759 * 100)), vjust = -.5) + scale_x_discrete(name = 'Vehicle Value') + scale_y_continuous(name = 'Frequency', limits = c(0, 250), breaks = seq(0, 250, 50)) + ggtitle('A.')

# Frequency distribution of traffic. Shows a median of 2 (n = 257) with decreasing amounts of data after that. Maximum traffic for these data was 6 (n = 3).
figB = ggplot(cuts, aes(x = factor(traffic))) + geom_histogram(binwidth = 1, color = 'black', fill = 'gray50', width = 1) + geom_text(size = fontSize, stat = 'bin', binwidth = 1, aes(y = ..count.., label = sprintf("%.1f%%", ..count.. / 759 * 100)), vjust = -.5) + scale_x_discrete(name = 'Traffic') + scale_y_continuous(name = 'Frequency', limits = c(0, 290), breaks = seq(0, 260, 50)) + ggtitle('B.')

# Frequency distribution of driverSex. Shows 39% of drivers were female, 61% were male.
figC = ggplot(cuts, aes(x = driverSex)) + geom_histogram(binwidth = 1, color = 'black', fill = 'gray50', width = 1)  + geom_text(size = fontSize, stat = 'bin', binwidth = 1, aes(y = ..count.., label = sprintf("%.1f%%", ..count.. / 759 * 100)), vjust = -.5) + scale_x_discrete(name = 'Driver Sex', labels = c('Female', 'Male')) + scale_y_continuous(name = 'Frequency', limits = c(0, 520), breaks = seq(0, 500, 100)) + ggtitle('C.')

# Frequency distribution of driverAge. The majority of drivers (n = 407 were perceived to be between 16-35 years old.
figD = ggplot(cuts, aes(x = factor(driverAge))) + geom_histogram(binwidth = 1, color = 'black', fill = 'gray50', width = 1) + geom_text(size = fontSize, stat = 'bin', binwidth = 1, aes(y = ..count.., label = sprintf("%.1f%%", ..count.. / 759 * 100)), vjust = -.5) + scale_x_discrete(name = 'Driver Age (Years)', labels = c('16-35', '36-55', '56+')) + scale_y_continuous(name = 'Frequency', limits = c(0, 460), breaks = seq(0, 420, 100)) + ggtitle('D.')

# Frequency distribution of time of day. Shows a median from 18:00-18:59 (6-6:59pm), with most data collected between 12:00 and 19:00.
cuts$time3 = as.numeric(cuts$time2)
figE = ggplot(cuts, aes(x = time3)) + geom_histogram(binwidth = 1, color = 'black', fill = 'gray50') + scale_x_continuous(name = 'Time of Day (24-Hour Clock)', limits = c(0, 23), breaks = seq(0, 23, 1)) + scale_y_continuous(name = 'Frequency', limits = c(0, 120), breaks = seq(0, 120, 20)) + ggtitle('E.')

# Arrange all figures in same image file.
pdf('dists.pdf', width = 7.25, height = 6)
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 2)))
print(figA, vp = vplayout(1, 1))
print(figB, vp = vplayout(1, 2))
print(figC, vp = vplayout(2, 1))
print(figD, vp = vplayout(2, 2))
print(figE, vp = vplayout(3, 1:2))
dev.off()

################################################################
# MIXED MODEL WITH RANDOM EFFECTS FOR RATERS & INTERSECTIONS
################################################################

# NOTE THAT NEWEST VERSION OF LME4 (V. 1.1-6) IS THROWING SOME WARNING MESSAGES THAT IT DIDN'T USE BEFORE. ALSO, WHILE BETAS ARE THE SAME AS BEFORE, Z AND P VALUES ARE SUBTLY DIFFERENT.

# Center continuous predictors: vehicleStatus, traffic, driverAge, and time2.
cuts$cVehicleStatus = cuts$vehicleStatus - mean(cuts$vehicleStatus)
cuts$cTraffic = cuts$traffic - mean(cuts$traffic)
cuts$cDriverAge = cuts$driverAge - mean(cuts$driverAge)
cuts$cTime2 = cuts$time2 - mean(cuts$time2)

# Adjust contrasts to balance driverSex. 39% of data from females, 61% from males.
xtabs(~ driverSex, cuts) / length(cuts$rater)
contrasts(cuts$driverSex) = cbind('MvF' = c(-.61, .39))

# Adjust contrasts to balance interType. 28% of data from 3-way stops, 72% from 4-way stops.
#xtabs(~ interType, cuts) / length(cuts$rater)
#contrasts(cuts$interType) = cbind('4v3' = c(-.72, .28))

# Start out with a model with just driver sex, age, and time, with the maximal random effects structure. Shows null effects of driverSex, B = 0.097, z = 0.48, p = 0.63, cDriverAge, B = 0.059, z = 0.44, p = 0.66, and cTime2, B = -0.029, z = -0.69, p = 0.49.
#cuts.glmer11 = glmer(cutoff == 'yes' ~ driverSex + cDriverAge + cTime2 + (1 + (driverSex + cDriverAge + cTime2)|rater) + (1 + (driverSex + cDriverAge + cTime2)|interID), data = cuts, family = 'binomial')

#******** Based on the results for cuts.glmer11 we can drop driverSex, cDriverAge and cTime2 from the main model. That gives us cuts.glmer12 (below), which includes fixed effects for vehicle status, traffic, and their interaction, and the full random effects structure for raters and intersections (random intercepts plus random slopes for all fixed effects). The model shows the expected effects of vehicle status, B = 0.21, z = 2.44, p = 0.015, traffic, B = 0.25, z = 2.93, p = 0.0034, but no interaction between the two, B = -0.085, z = -1.21, p = 0.22.
#cuts.glmer12 = glmer(cutoff == 'yes' ~ cVehicleStatus + cTraffic + cVehicleStatus:cTraffic + (1 + (cVehicleStatus + cTraffic + cVehicleStatus:cTraffic)|rater) + (1 + (cVehicleStatus + cTraffic + cVehicleStatus:cTraffic)|interID), data = cuts, family = 'binomial')

# Try fullest model with new version of lme4. This converges with statsig effects of vehicle status and traffic, but no status x traffic interaction. Note that this is using the newest version of the lme4 package, so parameter estimates are slightly different. Also, it seems to be more warning-happy: throws 5 warnings but still gives sensible output. Online comments from Ben Bolker (who's in charge of lme development) say that these warnings are probably in error and they're working to fix them. Makes writeup simpler because we only have to report results for a single model with max ranef.
cuts.glmer19 = glmer(cutoff == 'yes' ~ cVehicleStatus + cTraffic + cVehicleStatus:cTraffic + driverSex + cDriverAge + cTime2 + (1 + (cVehicleStatus + cTraffic + cVehicleStatus:cTraffic + driverSex + cDriverAge + cTime2)|rater) + (1 + (cVehicleStatus + cTraffic + cVehicleStatus:cTraffic + driverSex + cDriverAge + cTime2)|interID), data = cuts, family = 'binomial')

# May have to tweat above model a bit to get rid of errors...

# Warning messages:
#     1: In commonArgs(par, fn, control, environment()) :
#     maxfun < 10 * length(par)^2 is not recommended.
# 2: In optwrap(optimizer, devfun, start, rho$lower, control = control,  :
#                   convergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations exceeded
#               3: In (function (fn, par, lower = rep.int(-Inf, n), upper = rep.int(Inf,  :
#                                                                                       failure to converge in 10000 evaluations
#                                                                                   4: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                                                                                                       Model failed to converge with max|grad| = 0.0750656 (tol = 0.001, component 15)
#                                                                                                   5: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                                                                                                                       Model failed to converge: degenerate  Hessian with 1 negative eigenvalues










# Print variance-covariance matrix. This is required for submission to Arcchives of Scientific Psychology.
vcov(cuts.glmer19)

# Calculate 95% CIs for the vehicle status and traffic parameters. This is required for submission to Personality and Social Psychology Bulletin.
nranpars = length(getME(cuts.glmer19, "theta"))
nfixpars = length(fixef(cuts.glmer19))

# Calculate CIs using bootstrapping method. Takes forever! And for just 200 simulations it's not giving CIs that match with p-values. Try with nsim = 1000 over weekend.
#ci.boot = confint(cuts.glmer19, level = 0.95, method = "boot", nsim = 1000, parm = (nranpars + 2):(nranpars + 7), parallel = "multicore", ncpus = 4)

# Calculate CIs using profile method. Throws an error: "Error in profile.merMod(object, which = parm, signames = oldNames, ...) : Profiling over both the residual variance and fixed effects is not numerically consistent with profiling over the fixed effects only"
#ci.profile = confint(cuts.glmer19, level = 0.95, method = "profile", .progress = "txt")

# Calculate CIs using Wald method. Very fast! And gives CIs that match up with p-values.
ci.wald = confint(cuts.glmer19, level = 0.95, method = "Wald", .progress = "txt")