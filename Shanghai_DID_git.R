setwd("C:/Users/hpan8/Desktop/Shanghai_Night/Paper Writing/RS_Revision/new_data")
rm(list=ls())

set.seed(1213)

library(ggplot2)
library(reshape2)
library(stargazer)
library(AER)
library(ivpack)
library(plm)
library(punitroots)
library(lmtest)
library(raster)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(rgdal)

# #my_data = read.csv("did_out.csv")
# 
# my_data = read.csv("rev_data.csv")
# dashi = raster("dashi_code.tif", values= T)
# dashi = as.matrix(dashi)
# access_hh = raster("HH10_ACC_NP.tif", values= T)
# access_hh = as.matrix(access_hh)
# access_nn = raster("NH13_ACC_NP.tif", values= T)
# access_nn = as.matrix(access_nn)
# light08 = raster("rev_d0809.tif", values= T) 
# light08 = as.matrix(light08)
# light09 = raster("rev_d0910.tif", values= T) 
# light09 = as.matrix(light09)
# light10 = raster("rev_d1011.tif.tif", values= T) 
# light10 = as.matrix(light10)
# # light11 = raster("rev_d0809.tif", values= T) 
# # light11 = as.matrix(light08)
# light12 = raster("rev_v121011.tif", values= T) 
# light12 = as.matrix(light12)
# light13 = raster("rev_v131011.tif", values= T) 
# light13 = as.matrix(light13)
# light14 = raster("rev_v140708.tif", values= T) 
# light14 = as.matrix(light14)
# sat = raster("rev_dsaturation.tif",  values= T)
# sat = as.matrix(sat)
# 
# load("rs_rev_design.RData")
# 
# unique(as.vector(dashi))
# 
# dashi = as.vector(dashi)
# access_hh = as.vector(access_hh)[which((is.na(dashi)) == F)]
# access_nn = as.vector(access_nn)[which((is.na(dashi)) == F)]
# light08 = as.vector(light08)[which((is.na(dashi)) == F)] 
# light09 = as.vector(light09)[which((is.na(dashi)) == F)]
# light10 = as.vector(light10)[which((is.na(dashi)) == F)]
# light12 = as.vector(light12)[which((is.na(dashi)) == F)]
# light13 = as.vector(light13)[which((is.na(dashi)) == F)]
# light14 = as.vector(light14)[which((is.na(dashi)) == F)]
# sat = as.vector(sat)[which((is.na(dashi)) == F)]
# dashi = as.vector(dashi)[which((is.na(dashi)) == F)]
# access_none = rep(0, length(access_hh))
# table(sat)
# my_data[,11:22] = NULL
# 
# load("rs_rev_design.RData")
# 
# data_08 = merge(x = data.frame(City_Code = dashi,light = light08,sat = sat, access = access_none), 
#                 y = my_data[which(my_data$Year == 2008), ], by = "City_Code")
# 
# data_09 = merge(x = data.frame(City_Code = dashi,light = light09,sat = sat, access = access_hh), 
#                 y = my_data[which(my_data$Year == 2009), ], by = "City_Code")
# 
# data_10 = merge(x = data.frame(City_Code = dashi,light = light10,sat = sat, access = access_hh), 
#                 y = my_data[which(my_data$Year == 2010), ], by = "City_Code")
# 
# 
# data_hh = rbind(data_08, data_09, data_10)
# 
# data_hh = data_hh[which(data_hh$sat == 0),]
# 
# data_hh$pop_access = data_hh$Population * data_hh$access
# data_hh$ser_rate = data_hh$Service / data_hh$Employment
# load("rs_rev_design.RData")
# 
# data_hh$GDP = data_hh$GDP/1000000
# data_hh$Income = data_hh$Income/1000000
# data_hh$RCT = data_hh$Rail/(data_hh$Rail+ data_hh$Road)

#load design matrix of data
load("rs_rev_design.RData")

######dmsp data for HH10########

# model 1 to model 4

mod1 = lm(light ~ as.factor(Year)*pop_access + GDP + Income + ser_rate + as.factor(City_Code),
          data = data_hh) 

mod2 = lm(light ~ as.factor(Year)*pop_access + GDP + Income + as.factor(City_Code),
          data = data_hh) 

mod3 = lm(light ~ as.factor(Year)*pop_access + GDP + as.factor(City_Code),
          data = data_hh) 

mod4 = lm(light ~ as.factor(Year)*pop_access + as.factor(City_Code),
          data = data_hh)

summary(mod1)

#iv 2sls model

mod_ivd = ivreg(light ~ pop_access+ GDP + Income + ser_rate +  as.factor(City_Code)+ as.factor(Year) |
                 GDP + Income + ser_rate + as.factor(City_Code) + RCT2 + as.factor(Year),
               data = data_hh) 

#vce model
data_hr = data_hh[sample(nrow(data_hh), 200000), ]



p.df <- pdata.frame(data_hr, index = c("GDP"), drop.index = F, row.names = T)

head(p.df)




pm1 = plm(light ~  pop_access ,
           data = p.df, model = "pooling", na.action = na.omit)



# compute Stata like df-adjustment
G <- length(unique(data_hr$GDP))
N <- length(data_hr$GDP)
dfa <- (G/(G - 1)) * (N - 1)/pm1$df.residual


city_vcov <- dfa * vcovHC(pm1, type = "HC0", cluster = "group", adjust = T)
coeftest(pm1, vcov = city_vcov)







#report  results

stargazer(mod1, mod2, mod3, mod4, mod_ivd, title="HH10 effects on light growth", align=TRUE, out="ligh_hh.html")



summary(mod_ivd, vcov = sandwich, df = Inf, diagnostics = TRUE)



######viir data for nh13########


# 
# data_12n = merge(x = data.frame(City_Code = dashi,light = light12,sat = sat, access = access_hh), 
#                 y = my_data[which(my_data$Year == 2012), ], by = "City_Code")
# 
# data_13n = merge(x = data.frame(City_Code = dashi,light = light13,sat = sat, access = access_nn), 
#                 y = my_data[which(my_data$Year == 2013), ], by = "City_Code")
# 
# data_14n = merge(x = data.frame(City_Code = dashi,light = light14,sat = sat, access = access_nn), 
#                 y = my_data[which(my_data$Year == 2014), ], by = "City_Code")
# 
# 
# data_nn = rbind(data_12n, data_13n, data_14n)
# 
# data_nn$pop_access = data_nn$Population * data_nn$access
# data_nn$ser_rate = data_nn$Service / data_nn$Employment
# 
# 
# data_nn$GDP = data_nn$GDP/1000000
# data_nn$Income = data_nn$Income/1000000
# data_nn$RCT = data_nn$Rail/(data_nn$Rail+ data_nn$Road)

#model 1 to 4

modn1 = lm(light ~ as.factor(Year)*pop_access + GDP + Income + ser_rate + as.factor(City_Code),
          data = data_nn) 
modn2 = lm(light ~ as.factor(Year)*pop_access + GDP + Income + as.factor(City_Code),
           data = data_nn)
modn3 = lm(light ~ as.factor(Year)*pop_access + GDP + as.factor(City_Code),
           data = data_nn)
modn4 = lm(light ~ as.factor(Year)*pop_access +  as.factor(City_Code),
           data = data_nn)

#iv 2sls model

modn_ivd = ivreg(light ~  pop_access + GDP + Income + ser_rate + as.factor(City_Code) |
                   as.factor(Year) + GDP + Income + ser_rate + as.factor(City_Code) + RCT,
                 data = data_nn) 


#vce model
data_nr = data_nn[sample(nrow(data_nn), 200000), ]



pn.df <- pdata.frame(data_nr, index = c("GDP"), drop.index = F, row.names = T)

head(pn.df)




pmn1 = plm(light ~  pop_access ,
          data = pn.df, model = "pooling", na.action = na.omit)

# compute Stata like df-adjustment
G <- length(unique(data_hr$GDP))
N <- length(data_hr$GDP)
dfa <- (G/(G - 1)) * (N - 1)/pmn1$df.residual


cityn_vcov <- dfa * vcovHC(pmn1, type = "HC0", cluster = "group", adjust = T)
coeftest(pmn1, vcov = cityn_vcov)

# compute Stata like df-adjustment
G <- length(unique(data_nr$GDP))
N <- length(data_nr$GDP)
dfa <- (G/(G - 1)) * (N - 1)/pmn1$df.residual




#report model results

stargazer(modn1, modn2, modn3, modn4, modn_ivd, title="NH13 effects on light growth", align=TRUE, out="ligh_nh.html")


summary(modn_ivd, vcov = sandwich, df = Inf, diagnostics = TRUE)


#summary(modn_iv, vcov = sandwich, df = Inf, diagnostics = TRUE)

