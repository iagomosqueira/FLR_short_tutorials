# 03-tutorial.R - DESC
# 03 - (gg)plot FLR objects with ggplotFL/03-tutorial.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


install.packages("ggplotFL", repos="https://flr-project.org/R")
install.packages("patchwork")

# Load required packages

library(FLCore)
library(ggplotFL)


# --- FLR classes default plots

# - FLStock

data(ple4)

# metrics: rec, ssb, catch, fbar

plot(ple4)

# Alternative metrics

plot(ple4, metrics=list(Biomass=stock, Landings=landings,
  Discards=discards, F=fapex))

# TODO: Plot your own metrics

# EXTRA: Create your own metric

pgroup <- function(x) {
  return((stock.n(x) * stock.wt(x))[dim(x)[1],] / stock(x))
}
plot(ple4, metrics=list(Recruits=rec, Plusgroup=pgroup))

# - FLQuant

# 1D

plot(catch(ple4))

# 2D

plot(log(catch.n(ple4))) + facet_grid(age~.)

# 3D

data(ple4sex)

plot(stock.n(ple4sex))

# RULE: data ~ year. age, season, area as facet. unit as group.

# iters: median, 33%, 50% quantiles

flq <- rlnorm(300, log(catch(ple4)), 0.3)

plot(flq)

# CHOOSE other quantiles: 50% and 95%

plot(flq, probs=c(0.025, 0.25, 0.50, 0.75, 0.975))

# 99%

plot(flq, probs=c(0.005, 0.50, 0.995))

# - FLQuants

plot(metrics(ple4))

plot(FLQuants(bootstrap=flq, mean=iterMeans(flq)))

plot(metrics(ple4)) + geom_line(aes(colour=qname)) +
  geom_point()

# - FLSR

data(nsher)

plot(nsher)


# --- ADDING elements with `+`

plot(catch(ple4)) +
  # y-axis label
  ylab("Catch (t)") +
  # Y axis limits
  ylim(c(0,NA))

# TODO: ADD a title (ggtitle)


# --- ggplot

# ggplot(FLQuant)

ggplot(catch(ple4), aes(x=date, y=data)) + geom_line()

# Remember conversion to data.frame for column names

head(as.data.frame(catch.n(ple4), date=TRUE, cohort=TRUE, timestep=TRUE))

# Use it to build your plot

ggplot(catch(ple4), aes(x=year, y=data / 1000)) + geom_line() + geom_point() +
  xlab("") + ylab(expression(Catch (t %.% 10^3)))

# HINT: ?plotmath for expressions and symbols inside expression()


# EXAMPLE: bubble plot

ggplot(catch.n(ple4), aes(year, as.factor(age), size=data)) +
  geom_point(shape=21) + 
  scale_size(range = c(1, 20)) +
  ylab("age") + theme(legend.position = "none")

# EXAMPLE: Using other geoms

ggplot(stock.n(ple4) * stock.wt(ple4), aes(x=factor(age), y=data)) +
  geom_boxplot() +
  xlab("Age") + ylab("Biomass (t)")

# --- geoms for FLR classes

# geom_flquantiles

# - calculates quantiles (probs) along iter dimension
# - aesthetics: fill, alpha, colour

ggplot(flq, aes(x=year, y=data)) +
  geom_flquantiles(probs=c(0.10, 0.50, 0.90))

ggplot(flq, aes(x=year, y=data)) +
  geom_flquantiles(probs=c(0.25, 0.50, 0.75), fill="#D55E00") +
  geom_flquantiles(probs=c(0.10, 0.50, 0.90), fill="#E69F00")

# geom_flpar: geom_hline for FLPar objects

# - aesthetics: x (required), all from line and text.

refpts <- FLPar(FMSY=0.275, `F0.1`=0.155)

# Single FLQuant

plot(fbar(ple4)) + ylim(c(0,NA)) + geom_flpar(data=refpts, x=1952)

# FLQuants

# TODO check with repo ggplotFL
plot(ple4, metrics=list(SSB=ssb, F=fbar)) +
  geom_flpar(data=FLPars(SSB=FLPar(Blim=300000, Bpa=230000),
  F=FLPar(FMSY=0.21)), x=c(2015, 2015, 1960))

# geom_worm: plot individual iterations

# TODO check with repo ggplotFL
plot(flq) + geom_worm(data=iter(flq, 1:3))


# --- cohcorrplot: correlation plot

cohcorrplot(stock.n(ple4))

# --- patchwork
# https://patchwork.data-imaginist.com/index.html

library(patchwork)

# A good way to combine ggplots

# Side by side

plot(ple4) +  plot(flq)

# On top

plot(ple4) /  plot(flq)

# Combined in (almost) any way

p1 <- ggplot(mat(ple4), aes(x=age, y=data)) + geom_line() +
  ylab("Maturity") + xlab("Age")

p2 <- ggplot(stock.wt(ple4), aes(x=age, y=data, group=year)) + geom_line() +
  facet_wrap(~(floor(year / 10) * 10)) + ylab("Weight (kg)") + xlab("Age")

p3 <- plot(ple4, metrics=list(SSB=ssb, F=fbar))

(p1 | p2) / p3


# --- 

mets <- FLQuants(`SSB (t)`=rlnorm(300, log(ssb(ple4)), 0.3),
  Catch=catch(ple4))

plot(mets) + geom_worm(data=lapply(mets, iter, 1:3)) +
  ggtitle("North sea plaice (ple.2740)") +
  annotate("rect", xmin = 2000, xmax = 2020, ymin = -Inf, ymax = Inf,
    fill = "#E69F00", alpha=0.1)

