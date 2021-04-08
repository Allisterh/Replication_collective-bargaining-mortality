### Wage setting analysis in R


library(foreign)
library(causalsens)
library(arm)
library(extrafont)
library(mediation)



#### Preparing the script for causal sensitivity analysis

plot.causalsens <- function(x, type = "r.squared", ...) {
  m <- match.call(expand.dots = TRUE)
  m[[1L]] <- quote(graphics::plot)
  if (m$type == "r.squared") {
    m$x <- sign(x$sens$alpha) * x$sens$rsqs
    if (is.null(m$xlab)) {
      m$xlab <- "Variance explained by confounding"
    }
  } else if (type == "raw") {
    m$x <- x$sens$alpha
    if (is.null(m$xlab)) {
      m$xlab <- "Amount of confounding"
    }
  } else {
    stop("type must be 'r.squared' or 'raw'")
  }
  if (is.null(m$ylim)) {
    m$ylim <- c(min(x$sens$lower), max(x$sens$upper))
  }
  if (is.null(m$ylab)) {
    m$ylab <- "Estimated effect"
  }
  m$y <- x$sens$estimate
  m$type <- "l"
  eval(m, parent.frame())
  ## plot(x = xpoints, y = x$sens$estimate, type = "l", ylim = ylim,
  ##      xlab = xlab, ylab = "Estimated effect", ...)
  abline(h = 0, col = "grey")
  abline(v = 0, col = "grey")
  polygon(x = c(m$x, rev(m$x)),
          y = c(x$sens$lower, rev(x$sens$upper)),
          col =rgb(0.5, 0.5, 0.5, alpha = 0.5), border = NA)
  if (type == "r.squared") {
    points(x = x$partial.r2[1:17], y = rep(0, length(x$partial.r2[1:17])), pch = 4)
    points(x = -x$partial.r2[1:17], y = rep(0, length(x$partial.r2[1:17])), pch = 4)
  }
  invisible()
}



### Causal sensitivity SMR

setwd("C:/Users/areeves/Dropbox/Wage setting and health/")
d1 <- read.csv("Replication/sens_smr.csv")


m1 <- lm(log_smr_adult_wbi ~ wage4_new2 + wage4_new3  + ud + unioncent + wcoord2 + wcoord3 + wcoord4 
         + wcoord5 + cent + gdppp_cap + sstran + leftseat + health + govexp 
         + tmedcv  + log_pop + plt15_pc + po65_pc + idn1 + idn2 + idn3 + idn4 + idn5 + idn6 + 
           idn7 + idn8 + idn9 + idn10 + idn11 + idn12 + idn13 + idn14 + idn15 + idn16 + idn17 
         + idn18 + idn19 + idn20 + idn21 + year2 + year3 + year4 + year5 + year6 + year7 + 
           year8 + year9 + year10 + year11 + year12+ year13+ year14+ year15+ year16+ year17+
           year18+ year19+ year20+ year21+ year22+ year23+ year24+ year25+ year26+ year27+ 
           year28+ year29+ year30+ year31+ year32+ year33+ year34+ year35+ year36+ year37+
           year38+ year39+ year40+ year41+ year42+ year43+ year44+ year45+ year46+ year47 
         +year48+year49+year50+year51+year52, data=d1)
#display(m1, digits=4, detail=TRUE)
m2 <- lm(wage4_new3 ~ wage4_new2 + ud + unioncent + wcoord2 + wcoord3 + wcoord4 + wcoord5 + cent 
         + gdppp_cap + sstran + leftseat + health + govexp 
         + tmedcv  + log_pop + plt15_pc + po65_pc + year, data=d1)
#display(m2, digits=4, detail=TRUE)



alpha <- seq(-.2, .2, by = 0.01)
ll.sens <- causalsens(m1, m2, ~  wage4_new2 + ud + unioncent + wcoord2 + wcoord3 + wcoord4 + wcoord5 + cent 
                      + gdppp_cap + sstran + leftseat + health + govexp 
                      + tmedcv  + log_pop + plt15_pc + po65_pc, data = d1, alpha = alpha, confound = one.sided)



plot(ll.sens, type = "r.squared", bty = "n", xlim=c(-.2, .2), ylim=c(-.15,.05))






#### Causal sensitivity life expectancy 

#######
d3 <- read.csv("Replication/sens_life.csv")


m1 <- lm(log_lifexp_wbi ~ wage4_new2 + wage4_new3  + ud + unioncent + wcoord2 + wcoord3 + wcoord4 
         + wcoord5 + cent + gdppp_cap + sstran + leftseat + health + govexp 
         + tmedcv  + log_pop + plt15_pc + po65_pc + idn1 + idn2 + idn3 + idn4 + idn5 + idn6 + 
           idn7 + idn8 + idn9 + idn10 + idn11 + idn12 + idn13 + idn14 + idn15 + idn16 + idn17 
         + idn18 + idn19 + idn20 + idn21 + year2 + year3 + year4 + year5 + year6 + year7 + 
           year8 + year9 + year10 + year11 + year12+ year13+ year14+ year15+ year16+ year17+
           year18+ year19+ year20+ year21+ year22+ year23+ year24+ year25+ year26+ year27+ 
           year28+ year29+ year30+ year31+ year32+ year33+ year34+ year35+ year36+ year37+
           year38+ year39+ year40+ year41+ year42+ year43+ year44+ year45+ year46+ year47 
         +year48+year49+year50+year51+year52, data=d3)
display(m1)
m2 <- lm(wage4_new3 ~ wage4_new2 + ud + unioncent + wcoord2 + wcoord3 + wcoord4 + wcoord5 + cent 
         + gdppp_cap + sstran + leftseat + health + govexp 
         + tmedcv  + log_pop + plt15_pc + po65_pc + year, data=d3)
display(m2)


alpha <- seq(-0.4, 0.4, by = 0.01)
ll.sens <- causalsens(m1, m2, ~  wage4_new2 + ud + unioncent + wcoord2 + wcoord3 + wcoord4 + wcoord5 + cent 
                      + gdppp_cap + sstran + leftseat + health + govexp 
                      + tmedcv  + log_pop + plt15_pc + po65_pc, data = d3, alpha = alpha, confound = one.sided)


plot(ll.sens, type = "r.squared", bty = "n", xlim=c(-0.4, 0.4), ylim=c(-.05,.05))



#### Causal sensitivity Infant mortality 

#######
d2 <- read.csv("Replication/sens_life.csv")

m1 <- lm(log_smr_under5_wbi ~ wage4_new2 + wage4_new3  + ud + unioncent + wcoord2 + wcoord3 + wcoord4 
         + wcoord5 + cent + gdppp_cap + sstran + leftseat + health + govexp 
         + tmedcv  + log_pop + plt15_pc + po65_pc + idn1 + idn2 + idn3 + idn4 + idn5 + idn6 + 
           idn7 + idn8 + idn9 + idn10 + idn11 + idn12 + idn13 + idn14 + idn15 + idn16 + idn17 
         + idn18 + idn19 + idn20 + idn21 + year2 + year3 + year4 + year5 + year6 + year7 + 
           year8 + year9 + year10 + year11 + year12+ year13+ year14+ year15+ year16+ year17+
           year18+ year19+ year20+ year21+ year22+ year23+ year24+ year25+ year26+ year27+ 
           year28+ year29+ year30+ year31+ year32+ year33+ year34+ year35+ year36+ year37+
           year38+ year39+ year40+ year41+ year42+ year43+ year44+ year45+ year46+ year47 
         +year48+year49+year50+year51+year52, data=d2)
display(m1)
m2 <- lm(wage4_new3 ~ wage4_new2 + ud + unioncent + wcoord2 + wcoord3 + wcoord4 + wcoord5 + cent 
         + gdppp_cap + sstran + leftseat + health + govexp 
         + tmedcv  + log_pop + plt15_pc + po65_pc + year, data=d2)
display(m2)


alpha <- seq(-.4, .4, by = 0.01)
ll.sens <- causalsens(m1, m2, ~  wage4_new2 + ud + unioncent + wcoord2 + wcoord3 + wcoord4 + wcoord5 + cent 
                      + gdppp_cap + sstran + leftseat + health + govexp 
                      + tmedcv  + log_pop + plt15_pc + po65_pc, data = d2, alpha = alpha, confound = one.sided)

plot(ll.sens, type = "r.squared", bty = "n", xlim=c(-.4, .4), ylim=c(-.3,.1))



##################################################################
##################################################################

#### Mediation analysis


d1 <- read.csv("Replication/wageR2.csv", sep=",", check.names=FALSE)


######## SMR
## GINI
med.fit <- lmer(gini_net_v2 ~ wage4_new3 + wage4_new2 + ud + unioncent + as.factor(wcoord) + cent 
                + gdppp_cap + sstran + leftseat + health + govexp 
                + tmedcv  + log_pop + plt15_pc + po65_pc + as.factor(year) + (1 | idn), 
                data=subset(d1, log_smr_adult_wbi!="NA" & gini_net_v2!="NA"))
#display(med.fit)
out.fit <- lmer(log_smr_adult_wbi ~ gini_net_v2 + wage4_new3 + wage4_new2 + ud + unioncent + as.factor(wcoord) + cent 
                + gdppp_cap + sstran + leftseat + health + govexp 
                + tmedcv  + log_pop + plt15_pc + po65_pc + as.factor(year) + (1 | idn), data=d1)
#display(out.fit)
med.out <- mediate(med.fit, out.fit, treat = "wage4_new3", mediator = "gini_net_v2", sims = 800)
summary(med.out)
# ADE is sig. ACME is negative but not sig. 
#plot(med.out, group.plots=TRUE)



## SMR
med.fit <- lmer(top1share ~ wage4_new3 + wage4_new2 + ud + unioncent + as.factor(wcoord) + cent 
                + gdppp_cap + sstran + leftseat + health + govexp 
                + tmedcv  + log_pop + plt15_pc + po65_pc + as.factor(year) + (1 | idn), 
                data=subset(d1, log_smr_adult_wbi!="NA"))
#display(med.fit)
out.fit <- lmer(log_smr_adult_wbi ~ top1share + wage4_new3 + wage4_new2 + ud + unioncent + as.factor(wcoord) + cent 
                + gdppp_cap + sstran + leftseat + health + govexp 
                + tmedcv  + log_pop + plt15_pc + po65_pc + as.factor(year) + (1 | idn), data=d1)
#display(out.fit)
med.out <- mediate(med.fit, out.fit, treat = "wage4_new3", mediator = "top1share", sims = 800)
summary(med.out)
# ADE is sig. ACME is not sig. 

med.fit <- lmer(wages ~ wage4_new3 + wage4_new2 + ud + unioncent + as.factor(wcoord) + cent 
                + gdppp_cap + sstran + leftseat + health + govexp 
                + tmedcv  + log_pop + plt15_pc + po65_pc + as.factor(year) + (1 | idn), 
                data=subset(d1, log_smr_adult_wbi!="NA" & wages!="NA"))
display(med.fit)
out.fit <- lmer(log_smr_adult_wbi ~ wages + wage4_new3 + wage4_new2 + ud + unioncent + as.factor(wcoord) + cent 
                + gdppp_cap + sstran + leftseat + health + govexp 
                + tmedcv  + log_pop + plt15_pc + po65_pc + as.factor(year) + (1 | idn), data=d1)
display(out.fit)
med.out <- mediate(med.fit, out.fit, treat = "wage4_new3", mediator = "wages", sims = 400)
summary(med.out)


######### Life expectancy

## GINI
med.fit <- lmer(gini_net_v2 ~ wage4_new3 + wage4_new2 + ud + unioncent + as.factor(wcoord) + cent 
                + gdppp_cap + sstran + leftseat + health + govexp 
                + tmedcv  + log_pop + plt15_pc + po65_pc + as.factor(year) + (1 | idn), 
                data=subset(d1, log_lifexp_wbi!="NA"))
#display(med.fit)
out.fit <- lmer(log_lifexp_wbi ~ gini_net_v2 + wage4_new3 + wage4_new2 + ud + unioncent + as.factor(wcoord) + cent 
                + gdppp_cap + sstran + leftseat + health + govexp 
                + tmedcv  + log_pop + plt15_pc + po65_pc + as.factor(year) + (1 | idn), data=d1)
#display(out.fit)
med.out <- mediate(med.fit, out.fit, treat = "wage4_new3", mediator = "gini_net_v2", sims = 800)
summary(med.out)
# ADE is sig. ACME is sig.

## Top 1%
med.fit <- lmer(top1share ~ wage4_new3 + wage4_new2 + ud + unioncent + as.factor(wcoord) + cent 
                + gdppp_cap + sstran + leftseat + health + govexp 
                + tmedcv  + log_pop + plt15_pc + po65_pc + as.factor(year) + (1 | idn), 
                data=subset(d1, log_lifexp_wbi!="NA"))
#display(med.fit)
out.fit <- lmer(log_lifexp_wbi ~ top1share + wage4_new3 + wage4_new2 + ud + unioncent + as.factor(wcoord) + cent 
                + gdppp_cap + sstran + leftseat + health + govexp 
                + tmedcv  + log_pop + plt15_pc + po65_pc + as.factor(year) + (1 | idn), data=d1)
#display(out.fit)
med.out <- mediate(med.fit, out.fit, treat = "wage4_new3", mediator = "top1share", sims = 800)
summary(med.out)
# ADE is sig. ACME is not sig.


med.fit <- lmer(wages ~ wage4_new3 + wage4_new2 + ud + unioncent + as.factor(wcoord) + cent 
                + gdppp_cap + sstran + leftseat + health + govexp 
                + tmedcv  + log_pop + plt15_pc + po65_pc + as.factor(year) + (1 | idn), 
                data=subset(d1, log_lifexp_wbi!="NA" & wages!="NA"))
display(med.fit)
out.fit <- lmer(log_lifexp_wbi ~ wages + wage4_new3 + wage4_new2 + ud + unioncent + as.factor(wcoord) + cent 
                + gdppp_cap + sstran + leftseat + health + govexp 
                + tmedcv  + log_pop + plt15_pc + po65_pc + as.factor(year) + (1 | idn), data=d1)
display(out.fit)
med.out <- mediate(med.fit, out.fit, treat = "wage4_new3", mediator = "wages", sims = 600)
summary(med.out)


########## infant mortality

## GINI
med.fit <- lmer(gini_net_v2 ~ wage4_new3 + wage4_new2 + ud + unioncent + as.factor(wcoord) + cent 
                + gdppp_cap + sstran + leftseat + health + govexp 
                + tmedcv  + log_pop + plt15_pc + po65_pc + as.factor(year) + (1 | idn), 
                data=subset(d1, log_smr_under5_wbi!="NA"))
#display(med.fit)
out.fit <- lmer(log_smr_under5_wbi ~ gini_net_v2 + wage4_new3 + wage4_new2 + ud + unioncent + as.factor(wcoord) + cent 
                + gdppp_cap + sstran + leftseat + health + govexp 
                + tmedcv  + log_pop + plt15_pc + po65_pc + as.factor(year) + (1 | idn), data=d1)
#display(out.fit)
med.out <- mediate(med.fit, out.fit, treat = "wage4_new3", mediator = "gini_net_v2", sims = 800)
summary(med.out)
#sens.out <- medsens(med.out, rho.by = 0.1, effect.type = "indirect", sims = 100)
# ADE is sig. ACME is sig. 


## Top 1%
med.fit <- lmer(top1share ~ wage4_new3 + wage4_new2 + ud + unioncent + as.factor(wcoord) + cent 
                + gdppp_cap + sstran + leftseat + health + govexp 
                + tmedcv  + log_pop + plt15_pc + po65_pc + as.factor(year) + (1 | idn), 
                data=subset(d1, log_smr_under5_wbi!="NA"))
#display(med.fit)
out.fit <- lmer(log_smr_under5_wbi ~ top1share + wage4_new3 + wage4_new2 + ud + unioncent + as.factor(wcoord) + cent 
                + gdppp_cap + sstran + leftseat + health + govexp 
                + tmedcv  + log_pop + plt15_pc + po65_pc + as.factor(year) + (1 | idn), data=d1)
#display(out.fit)
med.out <- mediate(med.fit, out.fit, treat = "wage4_new3", mediator = "top1share", sims = 800)
summary(med.out)
# ADE is sig. ACME is not sig. 


med.fit <- lmer(wages ~ wage4_new3 + wage4_new2 + ud + unioncent + as.factor(wcoord) + cent 
                + gdppp_cap + sstran + leftseat + health + govexp 
                + tmedcv  + log_pop + plt15_pc + po65_pc + as.factor(year) + (1 | idn), 
                data=subset(d1, log_smr_under5_wbi!="NA" & wages!="NA"))
display(med.fit)
out.fit <- lmer(log_smr_under5_wbi ~ wages + wage4_new3 + wage4_new2 + ud + unioncent + as.factor(wcoord) + cent 
                + gdppp_cap + sstran + leftseat + health + govexp 
                + tmedcv  + log_pop + plt15_pc + po65_pc + as.factor(year) + (1 | idn), data=d1)
display(out.fit)
med.out <- mediate(med.fit, out.fit, treat = "wage4_new3", mediator = "wages", sims = 400)
summary(med.out)


#### mediation analysis without logged DV

######## SMR
## GINI
med.fit <- lmer(gini_net_v2 ~ wage4_new3 + wage4_new2 + ud + unioncent + as.factor(wcoord) + cent 
                + gdppp_cap + sstran + leftseat + health + govexp 
                + tmedcv  + log_pop + plt15_pc + po65_pc + as.factor(year) + (1 | idn), 
                data=subset(d1, smr_adult_wbi!="NA" & gini_net_v2!="NA"))
#display(med.fit)
out.fit <- lmer(smr_adult_wbi ~ gini_net_v2 + wage4_new3 + wage4_new2 + ud + unioncent + as.factor(wcoord) + cent 
                + gdppp_cap + sstran + leftseat + health + govexp 
                + tmedcv  + log_pop + plt15_pc + po65_pc + as.factor(year) + (1 | idn), data=d1)
#display(out.fit)
med.out <- mediate(med.fit, out.fit, treat = "wage4_new3", mediator = "gini_net_v2", sims = 800)
summary(med.out)
# ADE is sig. ACME is negative but not sig. 
#plot(med.out, group.plots=TRUE)



med.fit <- lmer(top1share ~ wage4_new3 + wage4_new2 + ud + unioncent + as.factor(wcoord) + cent 
                + gdppp_cap + sstran + leftseat + health + govexp 
                + tmedcv  + log_pop + plt15_pc + po65_pc + as.factor(year) + (1 | idn), 
                data=subset(d1, smr_adult_wbi!="NA"))
#display(med.fit)
out.fit <- lmer(smr_adult_wbi ~ top1share + wage4_new3 + wage4_new2 + ud + unioncent + as.factor(wcoord) + cent 
                + gdppp_cap + sstran + leftseat + health + govexp 
                + tmedcv  + log_pop + plt15_pc + po65_pc + as.factor(year) + (1 | idn), data=d1)
#display(out.fit)
med.out <- mediate(med.fit, out.fit, treat = "wage4_new3", mediator = "top1share", sims = 800)
summary(med.out)
# ADE is sig. ACME is not sig. 

med.fit <- lmer(wages ~ wage4_new3 + wage4_new2 + ud + unioncent + as.factor(wcoord) + cent 
                + gdppp_cap + sstran + leftseat + health + govexp 
                + tmedcv  + log_pop + plt15_pc + po65_pc + as.factor(year) + (1 | idn), 
                data=subset(d1, smr_adult_wbi!="NA" & wages!="NA"))
display(med.fit)
out.fit <- lmer(smr_adult_wbi ~ wages + wage4_new3 + wage4_new2 + ud + unioncent + as.factor(wcoord) + cent 
                + gdppp_cap + sstran + leftseat + health + govexp 
                + tmedcv  + log_pop + plt15_pc + po65_pc + as.factor(year) + (1 | idn), data=d1)
display(out.fit)
med.out <- mediate(med.fit, out.fit, treat = "wage4_new3", mediator = "wages", sims = 400)
summary(med.out)




