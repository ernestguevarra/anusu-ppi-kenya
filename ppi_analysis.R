# Example PPI analysis ---------------------------------------------------------

## Load libraries ----
library(readxl)
library(ppitables)


## Read data ----
survey_data <- read_xlsx("Trial_ppi.xlsx")

## Score each PPI indicator ----

### ppi1: County of residence ----
survey_data$ppi1 <- ifelse(
  survey_data$county == "Uasin Gishu", 8,
  ifelse(
    survey_data$county == "Busia", 0,
    ifelse(
      survey_data$county == "Trans Nzoia", 6,
      ifelse(
        survey_data$county == "Bungoma", 5, NA
      )
    )
  )
)

### ppi2: highest education level of female head of household ----
survey_data$ppi2 <- ifelse(
  survey_data$female_educ_level == "Pre-primary, none, or other", 0,
  ifelse(
    survey_data$female_educ_level == "Primary", 6,
    ifelse(
      survey_data$female_educ_level == "Secondary or post-primary, vocational", 9,
      ifelse(
        survey_data$female_educ_level == "College level or higher", 11,     
        ifelse(
          survey_data$female_educ_level == "There is no female household head/spouse", 18, NA
        )
      )
    )
  )
)

### ppi3: highest education level of any member of household ----
survey_data$ppi3 <- ifelse(
  survey_data$fam_member_educ_level == "Pre-primary, none, or other", 0,
  ifelse(
    survey_data$fam_member_educ_level == "Primary", 1,
    ifelse(
      survey_data$fam_member_educ_level == "Secondary or post-primary, vocational", 0,
      ifelse(
        survey_data$fam_member_educ_level == "College level or higher", 4, NA
      )
    )
  )
)

### ppi4: purchase bread in past 7 days ----
survey_data$ppi4 <- ifelse(survey_data$buy_bread == "Yes", 9,
                          ifelse(survey_data$buy_bread == "No", 0, NA))

## . Over the past 7 days, did the household either purchase/consume/acquire any meat or fish?
survey_data$ppi5 <- ifelse(survey_data$buy_meat_fish == "Yes", 11,
                              ifelse(survey_data$buy_meat_fish == "No", 0, NA))

## Over the past 7 days, did the household either purchase/consume/acquire any ripe bananas?
survey_data$ppi6 <- ifelse(survey_data$buy_banana == "Yes", 9,
                           ifelse(survey_data$buy_banana == "No", 0, NA))

##  Does your household own any towels?
survey_data$ppi7 <- ifelse(survey_data$own_towels == "Yes", 9,
                           ifelse(survey_data$own_towels == "No", 0, NA))

## 8. Does your household own any thermos flasks?
survey_data$ppi8 <- ifelse(survey_data$own_flask == "Yes", 8,
                          ifelse(survey_data$own_flask == "No", 0, NA))

## What is the predominant wall material of the main dwelling unit?
survey_data$ppi9 <- ifelse(survey_data$wall_material == "Finished walls (cement, stone with lime/cement, bricks, cement blocks, covered adobe, or wood planks/shingles)", 9,
                              ifelse(survey_data$wall_material == "Uncovered adobe wall, plywood, cardboard, reused wood, or corrugated iron sheets", 6,     
                                     ifelse(survey_data$wall_material == "Natural walls (cane/palm/trunks, grass/reeds, or mud/cow dung), no walls, bamboo with mud, stone with mud, or other", 0,NA)))

## What is the predominant floormaterial of the main dwelling unit?
survey_data$ppi10 <- ifelse(survey_data$floor_material == "Natural floor (earth/sand or dung) or palm/bamboo", 0,
                               ifelse(survey_data$floor_material == "Other (including wood planks/shingles, parquet or polished wood, vinyl or asphalt strips, ceramic tiles, cement, or carpet)", 9, NA))


survey_data$ppi_score <- survey_data$ppi1 + survey_data$ppi2 + survey_data$ppi3 + 
  survey_data$ppi4 + survey_data$ppi5 +  survey_data$ppi6 + survey_data$ppi7 + 
  survey_data$ppi8 +  survey_data$ppi9 + survey_data$ppi10

survey_data$ppi_320 <- lapply(
  X = survey_data$ppi_score,
  FUN = function(x) ppiKEN2018$ppp320[ppiKEN2018$score == x]
) |> unlist()
