# Author: Jeremy Boyd (jboyd@ucsd.edu)
# Date: June 26, 2015
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
library(dplyr)
library(extrafont)

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
pd <- position_dodge(0.16)
ggplot(cuts.sum4, aes(x = factor(vehicleStatus), y = mean, color = traffic2, group = traffic2)) + geom_line(position = pd) + geom_errorbar(aes(ymax = mean + se, ymin = mean - se), color = 'black', width = .15, position = pd) + geom_point(position = pd) + scale_x_discrete(name = 'Vehicle Value') + scale_y_continuous(name = 'Cut Likelihood (%)', limits = c(0, 100), breaks = seq(0, 100, 20)) + scale_color_manual(name = 'Traffic', labels = c('Heavy', 'Light'), values = c('gray60', 'black')) + theme(legend.position = c(.86, .807), legend.background = element_rect(fill = "transparent"), legend.key = element_rect(fill = "transparent", color = "transparent"), legend.title.align = .5)

# Create png, pdf, and jpeg versions of the figure.
ggsave(filename = 'figure4.png', width = 3.5, height = 3, dpi = 400)
ggsave(filename = 'figure4.pdf', width = 3.5, height = 3)
ggsave(filename = 'figure4.jpeg', width = 3.5, height = 3, dpi = 1000, units = "in")

# Same figure as above, but use colors and classic theme.
ggplot(cuts.sum4, aes(x = factor(vehicleStatus), y = mean, color = traffic2, group = traffic2)) + geom_line(position = pd) + geom_errorbar(aes(ymax = mean + se, ymin = mean - se), color = 'black', width = .15, position = pd) + geom_point(position = pd) + scale_x_discrete(name = 'Vehicle Value') + scale_y_continuous(name = 'Cut Likelihood (%)', limits = c(0, 100), breaks = seq(0, 100, 20)) + scale_color_manual(name = 'Traffic', labels = c('Heavy', 'Light'), values = c('firebrick', 'deepskyblue')) + theme_classic() + theme(legend.position = c(.85, .80), legend.background = element_rect(fill = "transparent"), legend.key = element_rect(fill = "transparent", color = "transparent"), legend.title.align = .5)
ggsave(filename = "figure4_color.svg", width = 3.5, height = 3)
ggsave(filename = "figure4_color.pdf", width = 3.5, height = 3)
ggsave(filename = "figure4_color.png", width = 3.5, height = 2.6, dpi = 400)

# Make EPS version of figure.
loadfonts(device = "postscript")
postscript(file = "Fig4.eps", width = 3.5, height = 2.6, paper = "special", horizontal = FALSE, onefile = FALSE, fonts = "Arial")
ggplot(cuts.sum4, aes(x = factor(vehicleStatus), y = mean, color = traffic2, group = traffic2)) + geom_line(position = pd) + geom_errorbar(aes(ymax = mean + se, ymin = mean - se), color = 'black', width = .15, position = pd) + geom_point(position = pd) + scale_x_discrete(name = 'Vehicle Value') + scale_y_continuous(name = 'Cut Likelihood (%)', limits = c(0, 100), breaks = seq(0, 100, 20)) + scale_color_manual(name = 'Traffic', labels = c('Heavy', 'Light'), values = c('firebrick', 'deepskyblue')) + theme_classic() + theme(legend.position = c(.85, .80), legend.background = element_rect(fill = "transparent"), legend.key = element_rect(fill = "transparent", color = "transparent"), legend.title.align = .5)
dev.off()

# Embed fonts
embed_fonts("./Fig4.eps", outfile = "./Fig4-embed.eps", options = "-dEPSCrop")

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
pred1 = ggplot(noInter, aes(x = factor(vehicleStatus), y = mean, color = traffic, group = traffic)) + geom_line() + geom_point() + scale_x_discrete(name = 'Driver SES') + scale_y_continuous(name = 'Cut Likelihood', limits = c(0, 100), breaks = seq(0, 100, 20)) + scale_color_manual(name = 'Traffic', labels = c('Heavy', 'Light'), values = c('firebrick', 'deepskyblue')) + theme_classic() + theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank()) +  ggtitle("Ethics")

# Make interaction prediction figure
inter = data.frame(vehicleStatus = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5), traffic = c('high', 'low', 'high', 'low', 'high', 'low', 'high', 'low', 'high', 'low'), mean = c(30, 30, 40, 32, 50, 34, 60, 36, 70, 38))
pred2 = ggplot(inter, aes(x = factor(vehicleStatus), y = mean, color = traffic, group = traffic)) + geom_line() + geom_point() + scale_x_discrete(name = 'Driver SES') + scale_y_continuous(name = 'Cut Likelihood', limits = c(0, 100), breaks = seq(0, 100, 20)) + scale_color_manual(name = 'Traffic', labels = c('Heavy',  'Light'), values = c('firebrick', 'deepskyblue')) + theme_classic() + theme(axis.text.y = element_blank(), axis.text.x = element_blank(), legend.position = 'none') + ggtitle("Attention")

# Combine into 1 x 2 plot
pdf('figure1.pdf', width = 7.25, height = 2.3)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 200)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(pred2, vp = vplayout(1, 1:87))
print(pred1, vp = vplayout(1, 88:200))
dev.off()

# Same as above but jpeg format
jpeg('figure1.jpeg', width = 7.25, height = 2.3, units = "in", quality = 100, res = 1000)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 200)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(pred2, vp = vplayout(1, 1:87))
print(pred1, vp = vplayout(1, 88:200))
dev.off()

# SVG format
svg('figure1.svg', width = 7.25, height = 2.3)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 200)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(pred2, vp = vplayout(1, 1:87))
print(pred1, vp = vplayout(1, 88:200))
dev.off()

# EPS format
postscript(file = "Fig1.eps", width = 7.25, height = 2.3, paper = "special", horizontal = FALSE, onefile = FALSE, fonts = "Arial")
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 200)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(pred2, vp = vplayout(1, 1:87))
print(pred1, vp = vplayout(1, 88:200))
dev.off()

# Embed fonts
embed_fonts("./Fig1.eps", outfile = "./Fig1-embed.eps", options = "-dEPSCrop")

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
figA = ggplot(cuts, aes(x = factor(vehicleStatus))) + geom_histogram(binwidth = 1, color = 'black', fill = 'gray50', width = 1) + geom_text(size = fontSize, stat = 'bin', binwidth = 1, aes(y = ..count.., label = sprintf("%.1f%%", ..count.. / 759 * 100)), vjust = -.5) + scale_x_discrete(name = 'Vehicle Value') + scale_y_continuous(name = 'Frequency', limits = c(0, 250), breaks = seq(0, 250, 50)) + ggtitle('A.') + theme_classic()

# Frequency distribution of traffic. Shows a median of 2 (n = 257) with decreasing amounts of data after that. Maximum traffic for these data was 6 (n = 3).
figB = ggplot(cuts, aes(x = factor(traffic))) + geom_histogram(binwidth = 1, color = 'black', fill = 'gray50', width = 1) + geom_text(size = fontSize, stat = 'bin', binwidth = 1, aes(y = ..count.., label = sprintf("%.1f%%", ..count.. / 759 * 100)), vjust = -.5) + scale_x_discrete(name = 'Traffic') + scale_y_continuous(name = 'Frequency', limits = c(0, 290), breaks = seq(0, 260, 50)) + ggtitle('B.') + theme_classic()

# Frequency distribution of driverSex. Shows 39% of drivers were female, 61% were male.
figC = ggplot(cuts, aes(x = driverSex)) + geom_histogram(binwidth = 1, color = 'black', fill = 'gray50', width = 1)  + geom_text(size = fontSize, stat = 'bin', binwidth = 1, aes(y = ..count.., label = sprintf("%.1f%%", ..count.. / 759 * 100)), vjust = -.5) + scale_x_discrete(name = 'Driver Sex', labels = c('Female', 'Male')) + scale_y_continuous(name = 'Frequency', limits = c(0, 520), breaks = seq(0, 500, 100)) + ggtitle('C.') + theme_classic()

# Frequency distribution of driverAge. The majority of drivers (n = 407 were perceived to be between 16-35 years old.
figD = ggplot(cuts, aes(x = factor(driverAge))) + geom_histogram(binwidth = 1, color = 'black', fill = 'gray50', width = 1) + geom_text(size = fontSize, stat = 'bin', binwidth = 1, aes(y = ..count.., label = sprintf("%.1f%%", ..count.. / 759 * 100)), vjust = -.5) + scale_x_discrete(name = 'Driver Age (Years)', labels = c('16-35', '36-55', '56+')) + scale_y_continuous(name = 'Frequency', limits = c(0, 460), breaks = seq(0, 420, 100)) + ggtitle('D.') + theme_classic()

# Frequency distribution of time of day. Shows a median from 18:00-18:59 (6-6:59pm), with most data collected between 12:00 and 19:00.
cuts$time3 = as.numeric(cuts$time2)
figE = ggplot(cuts, aes(x = time3)) + geom_histogram(binwidth = 1, color = 'black', fill = 'gray50') + scale_x_continuous(name = 'Time of Day (24-Hour Clock)', limits = c(0, 23), breaks = seq(0, 23, 1)) + scale_y_continuous(name = 'Frequency', limits = c(0, 120), breaks = seq(0, 120, 20)) + ggtitle('E.') + theme_classic()

# Arrange all figures in same image file.
pdf('figure3.pdf', width = 7.25, height = 6)
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 2)))
print(figA, vp = vplayout(1, 1))
print(figB, vp = vplayout(1, 2))
print(figC, vp = vplayout(2, 1))
print(figD, vp = vplayout(2, 2))
print(figE, vp = vplayout(3, 1:2))
dev.off()

# JPG version of above image
jpeg('figure3.jpeg', width = 7.25, height = 6, units = "in", quality = 100, res = 1000)
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 2)))
print(figA, vp = vplayout(1, 1))
print(figB, vp = vplayout(1, 2))
print(figC, vp = vplayout(2, 1))
print(figD, vp = vplayout(2, 2))
print(figE, vp = vplayout(3, 1:2))
dev.off()

# SVG of above image
svg('figure3.svg', width = 7.25, height = 6)
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 2)))
print(figA, vp = vplayout(1, 1))
print(figB, vp = vplayout(1, 2))
print(figC, vp = vplayout(2, 1))
print(figD, vp = vplayout(2, 2))
print(figE, vp = vplayout(3, 1:2))
dev.off()

# EPS of above image
postscript(file = "Fig3.eps", width = 7.25, height = 6, paper = "special", horizontal = FALSE, onefile = FALSE, fonts = "Arial")
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 2)))
print(figA, vp = vplayout(1, 1))
print(figB, vp = vplayout(1, 2))
print(figC, vp = vplayout(2, 1))
print(figD, vp = vplayout(2, 2))
print(figE, vp = vplayout(3, 1:2))
dev.off()

# Embed fonts
embed_fonts("./Fig3.eps", outfile = "./Fig3-embed.eps", options = "-dEPSCrop")

################################################################
# MIXED MODEL WITH RANDOM EFFECTS FOR RATERS & INTERSECTIONS
################################################################

# Convert time2 to numeric
cuts$time2 <- as.numeric(cuts$time2)

# Make subset with only those variables that will be used in model.
cuts2 <- select(cuts, rater, interID, vehicleStatus, traffic, driverSex, driverAge, time2, cutoff)

# Convert continuous predictors to z-scores using scale(). This is done in an effort to get rid of warnings when I try to fit a glmer model below.
cuts2$vehicleStatus <- scale(cuts2$vehicleStatus)
cuts2$traffic <- scale(cuts2$traffic)
cuts2$driverAge <- scale(cuts2$driverAge)
cuts2$time2 <- scale(cuts2$time2)

# Adjust contrasts to balance driverSex. Male is the default, so the model coefficient will indicate whether males are more/less likely to cut than females.
xtabs(~ driverSex, cuts2) / length(cuts2$rater)
contrasts(cuts2$driverSex) = cbind("Male" = c(-.61, .39))

# Model with maximal random effects structure. Throws multiple convergence warnings.
cuts2.glmer1 <- glmer(cutoff == "yes" ~ vehicleStatus + traffic + vehicleStatus:traffic + driverSex + driverAge + time2 + (1 + (vehicleStatus + traffic + vehicleStatus:traffic + driverSex + driverAge + time2)|rater) + (1 + (vehicleStatus + traffic + vehicleStatus:traffic + driverSex + driverAge + time2)|interID), data = cuts2, family = "binomial")

# Restart from previous fit, but now bumping up the maximum number of iterations and using the bobyqa optimizer for both phases.
ss <- getME(cuts2.glmer1, c("theta","fixef"))
cuts2.glmer2 <- update(cuts2.glmer1, start = ss, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e6)))

# Calculate 95% CIs for fixed effects parameters using the Wald method.
ci.wald <- confint(cuts2.glmer2, level = 0.95, method = "Wald", .progress = "txt")

# Print variance-covariance matrix. This is required for submission to some journals.
vcov(cuts2.glmer2)