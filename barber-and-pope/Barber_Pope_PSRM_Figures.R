#This file produces all figures in paper and appendix of Barber and Pope "Does Issue Importance Attenuate Partisan Cue-Taking?"
library(foreign)
library(readstata13)
library(plyr)

#Read in the data files
data <- read.dta13("long_data_ready_for_analysis.dta")
data <- data[order(data$caseid, data$Question),]

#remove "allow torture" question because there wasn't a salience question on the first wave. 
data <- data[data$Question != "\"Allow Torture\"",]
data$Question  <- as.character(data$Question)

data <- data[!is.na(data$Support),]


#########################################################
#Figure A1: baseline level of support among control group
#########################################################

#Create data
#create color palette:
coul = c("#5c0be8", "#afafa0", "#fc5858")

#Transform this data in %
a <- tapply(data$Question[data$libtrump == 0 & data$contrump == 0], 
            data$Support[data$libtrump == 0 & data$contrump == 0], function(x) table(x))

aa <- as.data.frame(rbind(a$`1`, a$`0`, a$`-1`))

data_percentage=apply(aa, 2, function(x){x*100/sum(x,na.rm=T)})
rownames(data_percentage) <- c("liberal", "dontknow", "conservative")
data_percentage <- as.data.frame(data_percentage)

data_perc <- data_percentage[,order(data_percentage[1,])]
data_perc <- as.matrix(data_perc)

#Make a stacked barplot--> it will be in %!
zz <- colnames(data_perc)
zz <- gsub(zz, pattern = "\"", replacement = "")

pdf("figurea1.pdf")

par(mar = c(4, 10, 4, 3))
barplot(data_perc, col=coul, border="white", xlab="", horiz = T, axes = F, ylab = "", 
        names.arg=zz, las = 2, cex.names = .7, main = "Opinions among Control Group")
axis(1, at = seq(0, 105, 10))
box()
text(15, .75, "% Liberal Position", col = "white", cex = .8)
text(45, .75, "% Don't Know", col = "white", cex = .8)
text(80, .75, "% Conservative Position", col = "white", cex = .8)

dev.off() 




####################################
#Figure 1: Distribution of Issue Salience in Survey
####################################

#Create data
#create color palette:
library(RColorBrewer)
coul = rev(brewer.pal(5, "Reds"))

#Transform this data in %
a <- tapply(data$Question, data$issue_salience, function(x) table(x))

aa <- as.data.frame(rbind(a$`1`, a$`2`, a$`3`, a$`4`, a$`5` ))

data_percentage=apply(aa, 2, function(x){x*100/sum(x,na.rm=T)})
rownames(data_percentage) <- c("notatall", "notmuch", "neutral", "somewhat", "verymuch")
data_percentage <- as.data.frame(data_percentage)

data_perc <- data_percentage[,order(data_percentage[5,])]
data_perc <- data_perc[order(data_perc$`"Abortion"`, decreasing = T),]
data_perc <- as.matrix(data_perc)

#Make a stacked barplot--> it will be in %!
zz <- colnames(data_perc)
zz <- gsub(zz, pattern = "\"", replacement = "")

pdf("figure1.pdf")

par(mar = c(4, 10, 4, 3))
barplot(data_perc, col=coul, border="white", xlab="", horiz = T, axes = F, ylab = "", 
        names.arg=zz, las = 2, cex.names = .7, main = "Levels of Issue Importance", ylim = c(-1, 23.5))
axis(1, at = seq(0, 105, 10))
box()
text(15, .75, "Very Much", col = "white", cex = .8)
text(50, .75, "Somewhat", col = "white", cex = .8)
text(85, .75, "Neutral", col = "white", cex = .8)
text(85, -.5, "Not Very Much", col = "black", cex = .8)
text(90, -1.2, "Not At All", col = "black", cex = .8)
segments(x0 = 94, x1 = 95, y0 = -.5, y1 = .5, lwd = 2)
segments(x0 = 96, x1 = 99, y0 = -1.2, y1 = .5, lwd = 2)
abline(h = 0)

dev.off() 


####################################
#Figure 2: Salience Figures
####################################
#conservative treatment
summary(model1 <- lm(Support ~ libtrump*issue_salience + contrump*issue_salience, data = data))
nobs(model1)

#control
newdata = as.data.frame(cbind(rep(0, 5), rep(0, 5), seq(1, 5, 1)))
colnames(newdata) <- c("contrump", "libtrump", "issue_salience")

#conservative 
newdata1 = as.data.frame(cbind(rep(1, 5), rep(0, 5), seq(1, 5, 1)))
colnames(newdata1) <- c("contrump", "libtrump", "issue_salience")

plx0 <- predict(model1, newdata = newdata, type = "response", se = T)
plx1 <- predict(model1, newdata = newdata1, type = "response", se = T)

diff.cons <- plx1$fit - plx0$fit
diff.cons.se <- sqrt(plx1$se.fit^2 + plx0$se.fit^2)

upper.cons <- diff.cons + 1.96*diff.cons.se
lower.cons <- diff.cons - 1.96*diff.cons.se

pdf("figure2.pdf")

#dev.new(width = 4.5, height = 7)
plot(seq(1, 5, 1), diff.cons, ylim = c(-.25, .05), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Level of Individual Issue Salience", axes = F, main = "Conservative Trump Treatment")
segments(x0 = c(seq(1, 5, 1), x1 = seq(1, 5, 1)), y0 = lower.cons, y1 = upper.cons, col = "#d17b7b80", lwd = 2)
points(seq(1, 5, 1), diff.cons, pch = 16)
abline(h = 0)
axis(1, at = seq(1, 5, 1), labels = c("Not at all", "Not very much", "Neutral", "Somewhat", "Very much"), cex.axis = .8)
axis(2, at = seq(-.45, .25, .05), las = 2)
box()


#liberal treatment
newdata1 = as.data.frame(cbind(rep(0, 5), rep(1, 5), seq(1, 5, 1)))
colnames(newdata1) <- c("contrump", "libtrump", "issue_salience")

plx0 <- predict(model1, newdata = newdata, type = "response", se = T)
plx1 <- predict(model1, newdata = newdata1, type = "response", se = T)

diff.lib <- plx1$fit - plx0$fit
diff.lib.se <- sqrt(plx1$se.fit^2 + plx0$se.fit^2)

upper.lib <- diff.lib + 1.96*diff.lib.se
lower.lib <- diff.lib - 1.96*diff.lib.se

#dev.new(width = 4.5, height = 7)
plot(seq(1, 5, 1), diff.lib, ylim = c(-.25, .25), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Level of Individual Issue Salience", axes = F, main = "Liberal Trump Treatment")
segments(x0 = c(seq(1, 5, 1), x1 = seq(1, 5, 1)), y0 = lower.lib, y1 = upper.lib, col = "#4286f480", lwd = 2)
points(seq(1, 5, 1), diff.lib, pch = 16)
abline(h = 0)
axis(1, at = seq(1, 5, 1), labels = c("Not at all", "Not very much", "Neutral", "Somewhat", "Very much"), cex.axis = .8)
axis(2, at = seq(-.25, .45, .05), las = 2)
box()

dev.off() 




##########################################################################################
#Figure 3: Party Interaction Plot
###########################################################################################
#Models Run Previously in Stata and margins saved as .csv file
margins <- read.csv("party_interaction_models_margins.csv")

pdf("figure3.pdf")

plot(seq(1, 5, 1), margins$coefficient[11:15], ylim = c(.0, .45), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Level of Individual Issue Salience", axes = F, main = "Liberal Trump Treatment \n Republicans")
segments(x0 = c(seq(1, 5, 1), x1 = seq(1, 5, 1)), y0 = margins$lower[11:15], y1 = margins$upper[11:15], col = "#4286f480", lwd = 2)
points(seq(1, 5, 1), margins$coefficient[11:15], pch = 16)
abline(h = 0)
axis(1, at = seq(1, 5, 1), labels = c("Not at all", "", "Neutral", "", "Very much"), cex.axis = .8)
axis(2, at = seq(0, .4, .1), las = 2)
box()


margins <- read.csv("party_interaction_models_margins_conservative.csv")
plot(seq(1, 5, 1), margins$coefficient[11:15], ylim = c(-.35, .10), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Level of Individual Issue Salience", axes = F, main = "Conservative Trump Treatment \n Republicans")
segments(x0 = c(seq(1, 5, 1), x1 = seq(1, 5, 1)), y0 = margins$lower[11:15], y1 = margins$upper[11:15], col = "#d17b7b80", lwd = 2)
points(seq(1, 5, 1), margins$coefficient[11:15], pch = 16)
abline(h = 0)
axis(1, at = seq(1, 5, 1), labels = c("Not at all", "", "Neutral", "", "Very much"), cex.axis = .8)
axis(2, at = seq(-.3, .1, .1), labels = c(-.3, -.2, -.1, 0, .1), las = 2)
box()

dev.off()


#############################################
#Figure A2: Distributions of Control Variables
##############################################

pdf("figurea2.pdf")

knowledge <- tapply(X = data$knowledge, INDEX = data$caseid, FUN = mean)
plot(0:8, table(knowledge), cex = 0, ylim = c(0, 500), axes = F, xlab = "Political Knowledge Levels", ylab = "Frequency", main = "Distribution of Political Knowledge")
segments(x0=0:8, x1=0:8, y0=0, y1=table(knowledge), lwd = 10, lend = 1)
axis(side = 1, at = seq(0, 8, 1))
axis(side = 2, at = seq(0, 500, 100))
box()

partisan.strength <- tapply(X = data$party_strength, INDEX = data$caseid, FUN = mean)
plot(1:4, table(partisan.strength), cex = 0, ylim = c(0, 500), axes = F, xlab = "Partisan Strength Levels", ylab = "Frequency", main = "Distribution of Partisan Strength")
segments(x0=1:4, x1=1:4, y0=0, y1=table(partisan.strength), lwd = 10, lend = 1)
axis(side = 1, at = seq(1, 4, 1), labels = c("Ind", "Lean", "Weak", "Strong"))
axis(side = 2, at = seq(0, 500, 100))
box()

trump.approve <- tapply(X = data$trump_favor, INDEX = data$caseid, FUN = mean)
plot(1:4, table(trump.approve), cex = 0, ylim = c(0, 500), axes = F, xlab = "Trump Approval Levels", ylab = "Frequency", main = "Distribution of Trump Approval")
segments(x0=1:4, x1=1:4, y0=0, y1=table(trump.approve), lwd = 10, lend = 1)
axis(side = 1, at = seq(1, 4, 1), labels = c("Strong\nDissaprove", "Somewhat\nDissaprove", "Somewhat\nApprove", "Strong\nApprove"))
axis(side = 2, at = seq(0, 500, 100))
box()

ideology <- tapply(X = data$ideo5b, INDEX = data$caseid, FUN = mean)
plot(1:5, table(ideology), cex = 0, ylim = c(0, 500), axes = F, xlab = "Ideology Levels", ylab = "Frequency", main = "Distribution of Ideology")
segments(x0=1:5, x1=1:5, y0=0, y1=table(ideology), lwd = 10, lend = 1)
axis(side = 1, at = seq(1, 5, 1), labels = c("Very Liberal", "Liberal", "Moderate", "Conservative", "Very Conservative"))
axis(side = 2, at = seq(0, 500, 100))
box()

dev.off()



#############################################
#Figure A3: Salience Figures
##############################################
#conservative treatment
summary(model1 <- lm(Support ~ libtrump*issue_salience + contrump*issue_salience, data = data))
nobs(model1)

#control
newdata = as.data.frame(cbind(rep(0, 5), rep(0, 5), seq(1, 5, 1)))
colnames(newdata) <- c("contrump", "libtrump", "issue_salience")

#conservative 
newdata1 = as.data.frame(cbind(rep(1, 5), rep(0, 5), seq(1, 5, 1)))
colnames(newdata1) <- c("contrump", "libtrump", "issue_salience")

plx0 <- predict(model1, newdata = newdata, type = "response", se = T)
plx1 <- predict(model1, newdata = newdata1, type = "response", se = T)

diff.cons <- plx1$fit - plx0$fit
diff.cons.se <- sqrt(plx1$se.fit^2 + plx0$se.fit^2)

upper.cons <- diff.cons + 1.96*diff.cons.se
lower.cons <- diff.cons - 1.96*diff.cons.se

pdf("figurea3.pdf")

#dev.new(width = 4.5, height = 7)
plot(seq(1, 5, 1), diff.cons, ylim = c(-.25, .05), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Level of Individual Issue Salience", axes = F, main = "Conservative Trump Treatment")
segments(x0 = c(seq(1, 5, 1), x1 = seq(1, 5, 1)), y0 = lower.cons, y1 = upper.cons, col = "#d17b7b80", lwd = 2)
points(seq(1, 5, 1), diff.cons, pch = 16)
abline(h = 0)
axis(1, at = seq(1, 5, 1), labels = c("Not at all", "Not very much", "Neutral", "Somewhat", "Very much"), cex.axis = .8)
axis(2, at = seq(-.45, .25, .05), las = 2)
box()


#liberal treatment
newdata1 = as.data.frame(cbind(rep(0, 5), rep(1, 5), seq(1, 5, 1)))
colnames(newdata1) <- c("contrump", "libtrump", "issue_salience")

plx0 <- predict(model1, newdata = newdata, type = "response", se = T)
plx1 <- predict(model1, newdata = newdata1, type = "response", se = T)

diff.lib <- plx1$fit - plx0$fit
diff.lib.se <- sqrt(plx1$se.fit^2 + plx0$se.fit^2)

upper.lib <- diff.lib + 1.96*diff.lib.se
lower.lib <- diff.lib - 1.96*diff.lib.se

#dev.new(width = 4.5, height = 7)
plot(seq(1, 5, 1), diff.lib, ylim = c(-.25, .25), pch = 16, ylab = "Estimated Average Treatment Effect", xlab = "Level of Individual Issue Salience", axes = F, main = "Liberal Trump Treatment")
segments(x0 = c(seq(1, 5, 1), x1 = seq(1, 5, 1)), y0 = lower.lib, y1 = upper.lib, col = "#4286f480", lwd = 2)
points(seq(1, 5, 1), diff.lib, pch = 16)
abline(h = 0)
axis(1, at = seq(1, 5, 1), labels = c("Not at all", "Not very much", "Neutral", "Somewhat", "Very much"), cex.axis = .8)
axis(2, at = seq(-.25, .45, .05), las = 2)
box()

dev.off()
