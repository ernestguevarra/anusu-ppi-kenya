rm(list = ls())
setwd("C:\\Users\\USER\\Documents\\Chamas\\PPI")

library(gtsummary)
library(rlang)
library(tinytex)
library(readxl)
library(epiDisplay)
library(rmarkdown)
library(knitr)
library(ggplot2) # for data visualization

library(knitr)
library(formattable)
library(plyr)
library(dplyr) # for data manipulation
library(kableExtra)
library(table1)
library(epiDisplay)

library(haven)
library(Rcpp)
library(flextable)
library(gt)
library(systemfonts)
library(ppitables)
#install.packages("ppitables")
library(ppicalc)
library(getable)
library(likert)

# IMPORT FROM EXCEL
anusu <- read_excel("C:\\Users\\USER\\Documents\\Chamas\\PPI\\data.xlsx")

## 2011 Purchasing power parity $3.20
#if("ppp320" %in% category) {
## County
anusu$county <- ifelse(anusu$county == "Uasin Gishu", 8,
                       ifelse(anusu$county == "Busia", 0,
                              ifelse(anusu$county == "Trans Nzoia", 6,
                                     ifelse(anusu$county == "Bungoma", 5, NA))))

##  What is the highest educational level that the female household head/spouse reached?
anusu$female_educ_level <- ifelse(anusu$female_educ_level == "Pre-primary, none, or other", 0,
                                  ifelse(anusu$female_educ_level == "Primary", 6,
                                         ifelse(anusu$female_educ_level == "Secondary or post-primary, vocational", 9,
                                                ifelse(anusu$female_educ_level == "College level or higher", 11,     
                                                       ifelse(anusu$female_educ_level == "There is no female household head/spouse", 18, NA)))))

##  What is the highest educational level that any member of the household reached?
anusu$fam_member_educ_level <- ifelse(anusu$fam_member_educ_level == "Pre-primary, none, or other", 0,
                                      ifelse(anusu$fam_member_educ_level == "Primary", 1,
                                             ifelse(anusu$fam_member_educ_level == "Secondary or post-primary, vocational", 0,
                                                    ifelse(anusu$fam_member_educ_level == "College level or higher", 4,NA))))

## . Over the past 7 days, did the household either purchase/consume/acquire any bread?
anusu$buy_bread <- ifelse(anusu$buy_bread == "Yes", 9,
                          ifelse(anusu$buy_bread == "No", 0, NA))

## . Over the past 7 days, did the household either purchase/consume/acquire any meat or fish?
anusu$buy_meat_fish <- ifelse(anusu$buy_meat_fish == "Yes", 11,
                              ifelse(anusu$buy_meat_fish == "No", 0, NA))

## Over the past 7 days, did the household either purchase/consume/acquire any ripe bananas?
anusu$buy_banana <- ifelse(anusu$buy_banana == "Yes", 9,
                           ifelse(anusu$buy_banana == "No", 0, NA))

##  Does your household own any towels?
anusu$own_towels <- ifelse(anusu$own_towels == "Yes", 9,
                           ifelse(anusu$own_towels == "No", 0, NA))

## 8. Does your household own any thermos flasks?
anusu$own_flask <- ifelse(anusu$own_flask == "Yes", 8,
                          ifelse(anusu$own_flask == "No", 0, NA))

## What is the predominant wall material of the main dwelling unit?
anusu$wall_material <- ifelse(anusu$wall_material == "Finished walls (cement, stone with lime/cement, bricks, cement blocks, covered adobe, or wood planks/shingles)", 9,
                              ifelse(anusu$wall_material == "Uncovered adobe wall, plywood, cardboard, reused wood, or corrugated iron sheets", 6,     
                                     ifelse(anusu$wall_material == "Natural walls (cane/palm/trunks, grass/reeds, or mud/cow dung), no walls, bamboo with mud, stone with mud, or other", 0,NA)))

## What is the predominant floormaterial of the main dwelling unit?
anusu$floor_material <- ifelse(anusu$floor_material == "Natural floor (earth/sand or dung) or palm/bamboo", 0,
                               ifelse(anusu$floor_material == "Other (including wood planks/shingles, parquet or polished wood, vinyl or asphalt strips, ceramic tiles, cement, or carpet)", 9, NA))

### ppi: total score
#mydata$ppi_score <- mydata$x1 + mydata$x2
anusu$ppi_score <- anusu$county + anusu$female_educ_level + anusu$fam_member_educ_level + anusu$buy_bread + anusu$buy_meat_fish +  anusu$buy_banana + anusu$own_towels + anusu$own_flask +  anusu$wall_material + anusu$floor_material
