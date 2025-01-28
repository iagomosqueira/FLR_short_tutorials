---
title: Training sessions on FLR
author: Iago Mosqueira
tags: [FLR R]
---

A series of training workshops will take place to help new and old members of staff getting started or improving their knowledge on the FLR toolset, but also on the ideas behind Management Strategy Evaluation (MSE). General issues of quantitative fisheries science could also be discussed and tested using the FLR tools.

The sessions are intended to run as a workshop, where a particular element in the FLR toolset is presented, deconstructed and analysed, so that then a series of examples can be run and extended.

We will deconstruct a full MSE analysis into its components, and learn how each of them work and can be modified. The basic tools of FLR will be also presented along the way.

# Goals

- Familiarise yourself with the structure and functioning of the FLR toolset.
- Understand the use of Management Strategy Evaluation.
- Apply the capabilities of the FLR advice packages to your own problem.
- Modify existing functions, create tailored modules, and develop new visualizations of inputs and outputs.
- Solve issues and report bugs.

# Organization

- A series of 11 sessions including.
  - A short presentation.
  - Examples and exercises.
  - Final assignment and discussion.
- To be run weekly starting 2 September 2024, 12.00-13.00.
- Physical meeting in IJmuiden, you can bring your lunch.
- MS Teams if necessary.
- Please enroll in the sessions you want to attend.
  - If no participants, session is cancelled.

# Sessions

## 0. Introduction and setup. A quick MSE demo.

- ** MSE analysis of recovery plans for ICES stocks**
- FLR ideas, packages and structure.
- Installing and updating packages.
- WKREBUILD2 MSE analysis as a TAF repository.
  - Conditioning an operating model.
  - Running ICES advice rule.
  - Evaluating alternative rules.
  - Comparing performance across candidate rules.

## 1. Creating and inspecting objects: FLQuant and FLStock

- **An FLStock: ple4**.
- Creating one from your data: from file to R to FLR (and back).
- Creating FLQuant objects: FLQuant(), as.FLQuant().
- The insides of FLQuant: quant, dim, dimnames, units.
- uom, standardUnits.
- Object validity (validObject) and verification (verify).
- Manipulating objects (more to come)
- Some other ways of loading data.
- accessors.
- as.data.frame, model.frame

## 2. Manipulating, summarizing and operating on FLR objects

- **FLStock, FLStocks, FLIndex**.
- subset, trim, window, expand
- Arith
- FLQuant %% operators
- sums, means, medians, totals and vars.
- dbind et al.
- iter, propagate.
- combine, divide.
- rnorm, rlnorm
- metrics, ssb, fbar, z.
- qapply.
- lapply and mapply

## 3. (gg)plot FLR objects with ggplotFL

- **My own plot from FLStock**
- plot()
- Modifying and adding to ggplots
- ggplot()
- geom_flquantiles
- geom_flpar
- geom_worm
- labels and numbers

## 4. Forecasting with FLasher

- **A long-term forecast**
- fwdWindow to setup the future
- The fwdControl class
- Calling fwd to project an FLStock
- Considering uncertainty and deviances
- Targets and limits.
- Relative targets.

## 5. Reference points - FLBRP, eqsim

- **Reference points for a stock**
- Equilibrium assumptions in FLBRP
- Biological and economic reference points
- Reference points vs. operational management points.
- An example use of eqsim

## 6. Stock assessment methods in FLR

- ICES shortcut
- FLSAM
- SS3
- FLa4a
- FLXSA
- JABBA & spict

## 7. Setting up and evaluating an MP

- **Shortcut, full feedback and model free MSE**
- Defining a new MP
- Running one or multiple MPs
- Implementing your own MP

## 8. Visualization and communication of results

- **An MSE dashboard**
- mseviz plots and tables
- ICES MSE outputs

## 9. Operating models with multiple biols and fisheries

- **A projection for a mixed fishery**
- FLFishery
- fwd(FLBiol, FLFishery)
- fwd(FLBiol, FLFisheries)
- fwd(FLBiols, FLFisheries)

## 10. Programming for FLR

- **A method**
- setMethod
- Inside the S4 classes
- coercion
- functions and methods
- lists of lists


# References

## FLR

## Advanced R

## ggplot2

## RMarkdown

- <https://rmd4sci.njtierney.com/>

