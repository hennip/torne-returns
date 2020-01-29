library(rjags)
library(runjags)
load.module("mix")
#library(xlsx)
library(tidyverse)
library(ggmcmc)
library(readxl)
library(xlsx)
library(forcats)
library(lubridate)
library(stringr)
require(gridExtra)



source("00-Functions/tidy-functions.r")
source("00-Functions/my-palette.r")


# Path for input data
pathIn<-"H:/Projects/torne-returns/data/orig/"

# Path for simulation output
#pathOut<-"H:/Projects/ISAMA/prg/output/Utsjoki-smolts/"


#source("01-Data/data-smolts-covariates.r")
#source("01-Data/data-simul.r")
#source("00-Functions/smolts-data-to-jags.r")

