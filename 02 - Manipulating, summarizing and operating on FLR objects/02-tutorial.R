# 02-tutorial.R - DESC
# 02 - Manipulating, summarizing and operating on FLR object/02-tutorial.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# Load required packages

library(FLCore)
library(ggplotFL)

# Load example data: ple4

data(ple4)

# Extract an FLQuant

flq <- stock.n(ple4)

# --- Subsetting objects

# Subset by position

flq[1, ]

# NOTE no need to write all dimensions, flq[1,,,,,,] and dimensions not dropped

flq[-1, ]

flq[, 1:10]

# or by name

flq[, "2000"]

# ac is a shortcut to as.character

flq[, ac(2000:2005)]

# [ works for FLStock too

ple4[1:3, ac(1980:1989)]

# window() subset and extends along the year dimension, arguments as names

window(flq, start=1980, end=1982)

window(flq, start=2017, end=2020)

window(ple4, end=2020)

# expand() can work along any dimension

expand(flq[,1], age=1:12)

expand(ple4, area=1:2)

bs <- expand(ple4, area=1:2)

# --- Appending

# append() joins objects along the year dimensions, fills missing ones

append(flq[, ac(2003:2005)], flq[, ac(2008:2010)])

# bind methods for each dimension

# quant (age) - qbind
qbind(flq[1,], flq[3,])

# year - ybind
ybind(flq[,1], flq[,3])

# unit - ubind
dim(ubind(flq[,1], flq[,1]))

# season - sbind
dim(sbind(flq[,1], flq[,1]))

# area - abind
dim(abind(flq[,1], flq[,1]))

# iter - ibind
dim(ibind(flq[,1], flq[,1]))

ab <- abind(flq[,1:2], flq[,1:2])

# --- Arithmetic operations

# Element by element operations

flq + flq

flq / (flq * 3)

stock.n(ple4) * stock.wt(ple4)

# Dimensions must match ...

fla <- FLQuant(runif(6), dim=c(3,2))
flb <- FLQuant(runif(9), dim=c(3,3))

fla * flb

# ... except for iter if 1 vs. N

fla <- FLQuant(runif(18), dim=c(3,3,1,1,1,200))
flb <- FLQuant(runif(9), dim=c(3,3,1,1,1,1))

fla * flb

# Arithmetic when just one dimension is different

ratio <- discards(ple4) / catch(ple4)

# This fails

stock.wt(ple4) * ratio

# But this works

stock.wt(ple4) %*% ratio

  
# --- Summarizing along dimensions

# Sum along the first dimensions ('quant')

quantSums(catch.n(ple4))

# Effectively does this

apply(catch.n(ple4), 2:6, sum, na.rm=TRUE)

# Mean along years

yearMeans(rec(ple4))

# Use it for geometric mean

exp(yearMeans(log(rec(ple4))))

# See the whole list at help("quantSums")


# --- Dealing with iterations

# An example FLQuant with iters

flqi <- rnorm(200, catch(ple4), 20000)

# plot() shows median, 50% and 90% quantiles

plot(flqi)

# Values in screen are median(MAD)

flqi

# Single iterations can be selected

flqi[,,,,,2]

iter(flqi, 2)

# and modified

flqi[,,,,,1] <- flqi[,,,,,6] * 1.01

# Arithmetic is special for iter, non-iter objects are extended

flqi / stock(ple4)

# And FLStock can have iters in only some slots, but always the same number

catch(ple4) <- flqi

summary(ple4)

# Objects can be extended along the iter dimensions using propagate(), either
# with content being copied

propagate(stock(ple4), 200)

# or with NA

flqp <- propagate(stock(ple4), 200, fill=FALSE)

iter(flqp, 2)

# combine() will merge objects across the iter dimension

combine(flqi, flqi)

# divide() will separate iters in a list

divide(iter(flqi, 1:3))

# R's RNG functions can work on FLQuant, e.g. rnorm(iter, mean, sd)

rnorm(200, catch(ple4), catch(ple4) / 10)

plot(rlnorm(200, log(stock(ple4)), 0.6))

# --- FLStock metrics

ssb(ple4)

metrics(ple4)

mets <- metrics(ple4, SSB=ssb, F=fapex, Discards=discards)

is(mets)

plot(mets)


# --- Applying functions to FLR classes

# apply

apply(flq, c(1, 3:6), median)

# qapply

qapply(ple4, sum)
qapply(ple4, sum)

astk <- qapply(ple4, `*`, 2)

plot(ple4, astk)

# lapply

lapply(mets, yearMeans)

# Map

ns <- metrics(ple4, C=catch.n, D=discards.n, L=landings.n)
wt <- metrics(ple4, C=catch.wt, D=discards.wt, L=landings.wt)

Map(function(x, y) quantSums(x * y), ns, wt)

# Reduce

lds <- metrics(ple4, L=landings, C=discards)

Reduce("+", lds)

lds[[1]] + lds[[2]]


# --- FLlst classes

# Each FLR classes has a 'plural' class

flqs <- FLQuants(SSB=ssb(ple4), F=fbar(ple4))

# FLQuants get plotted by default as horizontal facets

plot(flqs)

# Multiple FLStock objects fit into an FLStocks

flss <- FLStocks(PLE=ple4, SOM=astk)

plot(flss)

# lapply() and Map are your friends with any FL list

plot(lapply(flss, ssb))

lapply(flqs, units)
