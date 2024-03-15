# Mapping project
# Ellen, Mary, Kayla, Chris
# 2/6/24

# Libraries----
library(tidyverse)
library(readxl)
library(car)
library(emmeans)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)
library(MASS)
library(sf)
library(multcompView)
library(multcomp)
library(plotrix)
library(arcgisbinding)


resize.win <- function(Width=6, Height=6)
{
  # works for windows
  dev.off(); # dev.new(width=6, height=6)
  windows(record=TRUE, width=Width, height=Height)
}
resize.win(14/14)

# Theme----
theme_ew <- function (base_size=16, font=NA) { 
  theme(axis.title.x = element_text(face="bold", size=25, vjust=-1.0),
        axis.text.x  = element_text(size=25, colour = "black"),
        plot.title=element_text(face="bold", size = 18,hjust=0.01),
        axis.title.y = element_text(face="bold",angle=90,size=25, vjust=2.0),
        axis.text.y  = element_text(size=25,colour = "black"),
        plot.background = element_rect(fill = NA ,colour = NA),
        axis.line=element_line(colour="black"),
        plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"),
        panel.background =   element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        legend.position = "right",
        legend.justification=c(1,1),
        legend.background =element_blank(),
        legend.key = element_blank(),
        legend.text =   element_text(size = rel(2.0)),
        legend.title =  element_text(size = rel(1.2), face = "bold", hjust = 0)) 
}
# Load in data----
getwd()


total_acreage<- read_excel("C:/Users/erika/OneDrive - The Ohio State University/PhD/Mapping paper/R data files/AcreageTotals.xlsx", 
                           col_names = T, na = ".")

acres_by_land_type<- read_excel("C:/Users/erika/OneDrive - The Ohio State University/PhD/Mapping paper/R data files/AllParcelsAreasbyLandType.xls",
                                col_names = T)
summary(acres_by_land_type)


####### Loading GIS Data via ArcGIS Online #########
# all parcels (use for tree cover & land cover analysis)
arc.check_product() # initializing connection to arcGIS
shape_income<-arc.open(path="https://services6.arcgis.com/tuxY7TQIaDhLWARO/arcgis/rest/services/AllParcels17_canopy_acs/FeatureServer/0")
# asking r to only incorporate the fields we are interested in (ie: taking down the column count)

shape_income_sel <- arc.select(shape_income, fields=c("PARCELPIN", "par_city", "TAX_LUC_DE", "Total_A", "Can_A", "Grass_A", "Soil_A", "Water_A","Build_A", "Road_A", "Paved_A", "Perv_A", "Imperv_A", "Can_P", "Grass_P", "Soil_P", "Water_P","Build_P", "Road_P", "Paved_P", "Perv_P", "Imperv_P", "GEOID20", "GEOID", "AREA_SQMI", "AREA_ACRES", "Median_inc")) 
head(shape_income_sel)
str(shape_income_sel)
unique(shape_income_sel$TAX_LUC_DE)

# vacant lots (use for tree cover & land cover for vacant lots only)
Vacant_lots<- arc.open(path="https://services6.arcgis.com/tuxY7TQIaDhLWARO/arcgis/rest/services/VacantLotAllVars/FeatureServer/0")
vl_sel <- arc.select(Vacant_lots, fields=c("PARCELPIN", "par_city", "TAX_LUC_DESCRIPTION", "Total_A", "Can_A", "Grass_A", "Soil_A", "Water_A", 
                                           "Build_A", "Road_A", "Paved_A", "Perv_A", "Imperv_A", "Can_P", "Grass_P", "Soil_P", "Water_P", 
                                           "Build_P", "Road_P", "Paved_P", "Perv_P", "Imperv_P", "GEOID20", "Median_inc", "Change_Area", 
                                           "Change_PercentChange", "TreeCanopy_2017_Area"))
head(vl_sel)
str(vl_sel)

# How can we make sure that all of the vacant lot parcels are contained within
  # the all parcels shapefile?
sum(vl_sel$Total_A, na.rm = TRUE)
shape_income_sel<- shape_income_sel %>%
  mutate(new_land_types = fct_collapse(TAX_LUC_DE,
                                       "vacant" = c("RES VACANT LAND",
                                                    "Commercial Vacant",
                                                    "VACANT AG LAND",
                                                    "VACANT AG LAND-CAUV",
                                                    "VAC INDUSTRIAL LAND",
                                                    "COMMERCIAL VAC LAND")))
shape_income_sub<- shape_income_sel %>%
  group_by(new_land_types) %>%
  summarise(sum_ac = sum(Total_A))

# There are more vacant lot acres in the shapefile with all parcels than the 
  # shapefile with only vacant lots. There are some vacant categories in the
  # all parcels shapefile that have the same/very similar names, but when I
  # remove the repeated classes (tried all combinations), the total vacant
  # acres still does not equal the total vacant acres in the vacant shapefile.

# CENSUS BG env vars (use for income analysis [aggregated at block group level])
census_vars <-arc.open(path="https://services6.arcgis.com/tuxY7TQIaDhLWARO/arcgis/rest/services/CuyahogaCounty_envvar_CENSUSBG19/FeatureServer/0")
census_vars_sel<-arc.select(census_vars, fields=c("GEOID", "Median_inc", "MoE", "SUM_Can_A", "SUM_Grass_", "SUM_Soil_A", "SUM_Water_", 
                                                  "SUM_Build_", "SUM_Road_A", "SUM_Paved_", "SUM_Perv_A", "SUM_Imperv", "PercImperv", "PercCanopy", "name", "SUM_Total_"))
head(census_vars_sel)

# Variable corrections----
acres_by_land_type$LandType<- as.factor(acres_by_land_type$LandType)
summary(acres_by_land_type)
# Reassign land type categories----
table(acres_by_land_type$LandType, exclude = NULL)

acres_by_land_type<- acres_by_land_type %>%
  mutate(new_land_types = fct_collapse(LandType,
                               "residential" = c("1-FAMILY PLATTED LOT", 
                                                 "2-FAMILY PLATTED LOT",
                                                 "3-FAMILY PLATTED LOT",
                                                 "4- 6 UNIT APARTMENTS",
                                                 "COMMERCIAL CONDOMINI",
                                                 "CONVALESCENT HOME",
                                                 "DORMITORY",
                                                 "ELEVATOR APTS 20-39U",
                                                 "ELEVATOR APTS 40+ U",
                                                 "ELEVATOR APTS 7-19 U",
                                                 "GARDEN APTS 20-39 U",
                                                 "GARDEN APTS 40+ U",
                                                 "MOBILE HOME PARK",
                                                 "OTHER COMM HSNG NEC",
                                                 "OTHER RES PLATTED",
                                                 "RESIDENTIAL CONDO",
                                                 "ROW HOUSING",
                                                 "SUBSIDIZED HOUSING",
                                                 "WALK-UP APTS 20-39 U",
                                                 "WALK-UP APTS 40+ U",
                                                 "WALK-UP APTS 7-19 U"),
                               "government" = c("LAND FILL", "MARINE SVC FACILITY", 
                                                "POST OFFICE",
                                                "VEHICLE RECYCLING YD"),
                               "commercial" = c("1-2 STORY OFFCE BLDG",
                                                "1-UNIT WHSE <75000SF",
                                                "1-UNIT WHSE >75000SF",
                                                "AIRCRAFT SALES & SVC",
                                                "AMUSEMENT PARK",
                                                "AUTO REPAIR GARAGE",
                                                "AUTO SALES & SERVICE",
                                                "BILLBOARD SITE(S)",
                                                "BLDG MATERIAL STGE",
                                                "BOWLING ALLEY",
                                                "BULK OIL STGE",
                                                "CAFETERIA",
                                                "COMM WHSE LOFT-TYPE",
                                                "COMMERCIAL COMN AREA",
                                                "COMMON AREA PLATTED",
                                                "COMMUNICATION FAC.",
                                                "COMMUNITY SHOP CNTR",
                                                "CONTRACT/ CONST SVCS",
                                                "DAY CARE CENTER",
                                                "DEPARTMENT STORE",
                                                "DETACHD STORE<7500SF",
                                                "DETACHED HEALTH SPA",
                                                "DISCNT/JR DEPT STORE",
                                                "DISTRIBUTION WHSE",
                                                "DRIVE-IN RESTAURANT",
                                                "DRYCLEAN PLANT/LNDRY",
                                                "ELEVATOR OFFCE >2 ST",
                                                "FOOD/DRINK PROC/STGE",
                                                "FRANCHISE AUTO SVC",
                                                "FRANCHISE FD COUNTER",
                                                "FRANCHISE FD SITDOWN",
                                                "FRANCHISE FOOD STORE",
                                                "FS DRIVETHRU CARWASH",
                                                "FULL SERVICE BANK",
                                                "FULL SVC GAS STATION",
                                                "FUNERAL HOME",
                                                "FURNITURE MART",
                                                "GAS STATION W/ KIOSK",
                                                "GNRL RETAIL+ 7500 SQ",
                                                "GOLF COURSE",
                                                "HEAVY MFG/ FOUNDRY",
                                                "HOME GARDEN CENTER",
                                                "HOME IMPRVMNT CENTER",
                                                "HOTELS",
                                                "ICE CREAM STAND",
                                                "INDUSTRIAL COMMON AR",
                                                "LIGHT MFG / ASSEMBLY",
                                                "LODGE HALL",
                                                "M & E YARD STGE",
                                                "MATERIAL YARD STGE",
                                                "MEDIUM MFG/ ASSEMBLY",
                                                "MINATURE GOLF/DR RNG",
                                                "MINES AND QUARRIES",
                                                "MINI-STORAGE WHSE",
                                                "MOTELS",
                                                "MULTI-TENANT WHSE",
                                                "NEIGHBORHOOD TAVERN",
                                                "NIGHTCLUB",
                                                "OFFICE CONDO",
                                                "OTHER COMMERCIAL NEC",
                                                "OTHER FOOD SVC NEC",
                                                "OTHER INDUSTRIAL NEC",
                                                "OTHER RETAIL NEC",
                                                "PARTY CENTER",
                                                "R & D FACILITY",
                                                "REGIONAL SHOP CENTER",
                                                "SALVAGE/ SCRAP YARD",
                                                "SAVINGS AND LOAN",
                                                "SELF-SVC CAR WASH",
                                                "SMALL SHOPS",
                                                "STORE W/ WALKUP OFFC",
                                                "STORE W/ WALKUP APTS",
                                                "STRIPCNTR 4+U>7500SF",
                                                "SUPERMARKET",
                                                "THEATRE",
                                                "TRUCK SALES & SVC",
                                                "USED CAR SALES",
                                                "UTILITY SERVICE FAC.",
                                                "WALKUP OFFICE >2 ST"),
                               "vacant" = c("COMMERCIAL VAC LAND",
                                            "RES VACANT LAND",
                                            "VAC INDUSTRIAL LAND",
                                            "VACANT AG LAND",
                                            "VACANT AG LAND-CAUV"),
                               "agriculture" = c("CASH GRAIN/GEN-CAUV",
                                                 "FRUIT/NUT FARM-CAUV",
                                                 "GRAIN ELEVATORS",
                                                 "GREENHOUSE",
                                                 "LIVESTOCK FARM-CAUV",
                                                 "NURSERY",
                                                 "OTHER AG NEC - CAUV",
                                                 "TIMBER - CAUV",
                                                 "TIMBER / FOREST LAND",
                                                 "VEGETABLE FARM",
                                                 "VEGGIE FARM - CAUV"),
                               "recreation" = c("COMM CAMPGROUNDS",
                                                "CULTRL/NATURE EXHIBT",
                                                "RAQTBALL/TENNIS CLUB",
                                                "SMALL BOAT MARINA",
                                                "SPORT/ PUBLC ASSMBLY"),
                               "health" = c("ANIMAL CLINIC/ HOSP",
                                            "MED CLINIC/ OFFICES",
                                            "MED CLNC/ OFFC CONDO",
                                            "HOSPITALS FOR PROFIT",
                                            "NURSING HOME"),
                               "transportation" = c("COMM TRUCK TERMINAL",
                                                    "ASSOCIATD PARKNG LOT",
                                                    "COMM PARKING GARAGE",
                                                    "COMM PARKING LOT",
                                                    "RR-NOT USED IN OPER.",
                                                    "RR-USED IN OPERATION",
                                                    "TRANSPORTATION FAC."),
                               "miscellanious" = c("0",
                                 "LISTED WITH"),
                               "municipal" = "municipal"))

acres_by_land_type_shape<- shape_income_sel %>%
  mutate(new_land_types = fct_collapse(TAX_LUC_DE,
                                       "residential" = c("1-FAMILY PLATTED LOT", 
                                                         "2-FAMILY PLATTED LOT",
                                                         "3-FAMILY PLATTED LOT",
                                                         "4- 6 UNIT APARTMENTS",
                                                         "COMMERCIAL CONDOMINI",
                                                         "CONVALESCENT HOME",
                                                         "DORMITORY",
                                                         "ELEVATOR APTS 20-39U",
                                                         "ELEVATOR APTS 40+ U",
                                                         "ELEVATOR APTS 7-19 U",
                                                         "GARDEN APTS 20-39 U",
                                                         "GARDEN APTS 40+ U",
                                                         "MOBILE HOME PARK",
                                                         "OTHER COMM HSNG NEC",
                                                         "OTHER RES PLATTED",
                                                         "RESIDENTIAL CONDO",
                                                         "ROW HOUSING",
                                                         "SUBSIDIZED HOUSING",
                                                         "WALK-UP APTS 20-39 U",
                                                         "WALK-UP APTS 40+ U",
                                                         "WALK-UP APTS 7-19 U"),
                                       "government" = c("LAND FILL", "MARINE SVC FACILITY", 
                                                        "POST OFFICE",
                                                        "VEHICLE RECYCLING YD"),
                                       "commercial" = c("1-2 STORY OFFCE BLDG",
                                                        "1-UNIT WHSE <75000SF",
                                                        "1-UNIT WHSE >75000SF",
                                                        "AIRCRAFT SALES & SVC",
                                                        "AMUSEMENT PARK",
                                                        "AUTO REPAIR GARAGE",
                                                        "AUTO SALES & SERVICE",
                                                        "BILLBOARD SITE(S)",
                                                        "BLDG MATERIAL STGE",
                                                        "BOWLING ALLEY",
                                                        "BULK OIL STGE",
                                                        "CAFETERIA",
                                                        "COMM WHSE LOFT-TYPE",
                                                        "COMMERCIAL COMN AREA",
                                                        "COMMON AREA PLATTED",
                                                        "COMMUNICATION FAC.",
                                                        "COMMUNITY SHOP CNTR",
                                                        "CONTRACT/ CONST SVCS",
                                                        "DAY CARE CENTER",
                                                        "DEPARTMENT STORE",
                                                        "DETACHD STORE<7500SF",
                                                        "DETACHED HEALTH SPA",
                                                        "DISCNT/JR DEPT STORE",
                                                        "DISTRIBUTION WHSE",
                                                        "DRIVE-IN RESTAURANT",
                                                        "DRYCLEAN PLANT/LNDRY",
                                                        "ELEVATOR OFFCE >2 ST",
                                                        "FOOD/DRINK PROC/STGE",
                                                        "FRANCHISE AUTO SVC",
                                                        "FRANCHISE FD COUNTER",
                                                        "FRANCHISE FD SITDOWN",
                                                        "FRANCHISE FOOD STORE",
                                                        "FS DRIVETHRU CARWASH",
                                                        "FULL SERVICE BANK",
                                                        "FULL SVC GAS STATION",
                                                        "FUNERAL HOME",
                                                        "FURNITURE MART",
                                                        "GAS STATION W/ KIOSK",
                                                        "GNRL RETAIL+ 7500 SQ",
                                                        "GOLF COURSE",
                                                        "HEAVY MFG/ FOUNDRY",
                                                        "HOME GARDEN CENTER",
                                                        "HOME IMPRVMNT CENTER",
                                                        "HOTELS",
                                                        "ICE CREAM STAND",
                                                        "INDUSTRIAL COMMON AR",
                                                        "LIGHT MFG / ASSEMBLY",
                                                        "LODGE HALL",
                                                        "M & E YARD STGE",
                                                        "MATERIAL YARD STGE",
                                                        "MEDIUM MFG/ ASSEMBLY",
                                                        "MINATURE GOLF/DR RNG",
                                                        "MINES AND QUARRIES",
                                                        "MINI-STORAGE WHSE",
                                                        "MOTELS",
                                                        "MULTI-TENANT WHSE",
                                                        "NEIGHBORHOOD TAVERN",
                                                        "NIGHTCLUB",
                                                        "OFFICE CONDO",
                                                        "OTHER COMMERCIAL NEC",
                                                        "OTHER FOOD SVC NEC",
                                                        "OTHER INDUSTRIAL NEC",
                                                        "OTHER RETAIL NEC",
                                                        "PARTY CENTER",
                                                        "R & D FACILITY",
                                                        "REGIONAL SHOP CENTER",
                                                        "SALVAGE/ SCRAP YARD",
                                                        "SAVINGS AND LOAN",
                                                        "SELF-SVC CAR WASH",
                                                        "SMALL SHOPS",
                                                        "STORE W/ WALKUP OFFC",
                                                        "STORE W/ WALKUP APTS",
                                                        "STRIPCNTR 4+U>7500SF",
                                                        "SUPERMARKET",
                                                        "THEATRE",
                                                        "TRUCK SALES & SVC",
                                                        "USED CAR SALES",
                                                        "UTILITY SERVICE FAC.",
                                                        "WALKUP OFFICE >2 ST"),
                                       "vacant" = c("COMMERCIAL VAC LAND",
                                                    "RES VACANT LAND",
                                                    "VAC INDUSTRIAL LAND",
                                                    "VACANT AG LAND",
                                                    "VACANT AG LAND-CAUV",
                                                    "Commercial Vacant"),
                                       "agriculture" = c("CASH GRAIN/GEN-CAUV",
                                                         "FRUIT/NUT FARM-CAUV",
                                                         "GRAIN ELEVATORS",
                                                         "GREENHOUSE",
                                                         "LIVESTOCK FARM-CAUV",
                                                         "NURSERY",
                                                         "OTHER AG NEC - CAUV",
                                                         "TIMBER - CAUV",
                                                         "TIMBER / FOREST LAND",
                                                         "VEGETABLE FARM",
                                                         "VEGGIE FARM - CAUV"),
                                       "recreation" = c("COMM CAMPGROUNDS",
                                                        "CULTRL/NATURE EXHIBT",
                                                        "RAQTBALL/TENNIS CLUB",
                                                        "SMALL BOAT MARINA",
                                                        "SPORT/ PUBLC ASSMBLY",
                                                        "Recreational"),
                                       "health" = c("ANIMAL CLINIC/ HOSP",
                                                    "MED CLINIC/ OFFICES",
                                                    "MED CLNC/ OFFC CONDO",
                                                    "HOSPITALS FOR PROFIT",
                                                    "NURSING HOME"),
                                       "transportation" = c("COMM TRUCK TERMINAL",
                                                            "ASSOCIATD PARKNG LOT",
                                                            "COMM PARKING GARAGE",
                                                            "COMM PARKING LOT",
                                                            "RR-NOT USED IN OPER.",
                                                            "RR-USED IN OPERATION",
                                                            "TRANSPORTATION FAC.",
                                                            "Transp/Util"),
                                       "miscellanious" = c("0",
                                                           "LISTED WITH",
                                                           "Easement",
                                                           "easement",
                                                           "Preserved"),
                                       "municipalpark" = "Municipal/Local Park"),
                                       "federalpark" = "Federal Park")
# Sum the area for each new land type----
land_type_summary<- acres_by_land_type %>% group_by(new_land_types) %>%
  summarise(sum_total_ac = sum(Total_Acreage),
            sum_parcels = sum(N_parcels),
            sum_canopy_ac = sum(Canopy_Acreage),
            sum_grass_ac = sum(Grass_Acreage),
            sum_soil_ac = sum(Soil_Acreage),
            sum_water_ac = sum(Water_Acreage),
            sum_building_ac = sum(Building_Acreage),
            sum_road_ac = sum(Road_Acreage),
            sum_pave_ac = sum(Pavement_Acreage),
            sum_pervious_ac = sum(Pervious_Acreage),
            sum_imperv_ac = sum(Impervious_Acreage))
# Calculate percent tree cover for each new land type
land_type_summary$sum_percent_canopy<- {land_type_summary$sum_canopy_ac/land_type_summary$sum_total_ac}*100
summary(land_type_summary)

land_type_summary$sum_percent_canopy_int<- as.integer(land_type_summary$sum_percent_canopy)
summary(land_type_summary)
land_type_summary$sum_percent_imperv<- {land_type_summary$sum_imperv_ac/land_type_summary$sum_total_ac}*100

land_type_summary$sum_percent_pervious<- {land_type_summary$sum_pervious_ac/land_type_summary$sum_total_ac}*100
 
# Graph percentages by land type

ggplot(land_type_summary, aes(x = new_land_types, y = sum_percent_canopy))+
  geom_bar(stat = "identity")+
  theme_ew()+
  xlab("Land Type")+
  ylab("% Canopy")+
  ylim(0, 100)+
  theme(axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))
# Stats-----
# Percent canopy ~ land type
canopy_lm<- lm(Existing_Percent_Canopy ~ new_land_types, data = acres_by_land_type)
#plot(canopy_lm)

canopy_lm_log<- lm(log(Existing_Percent_Canopy + 1) ~ new_land_types, data = acres_by_land_type)
#plot(canopy_lm_log)
summary(canopy_lm_log)
Anova(canopy_lm_log, type = "III")

# aov() works too and produces the same result as Anova() with lm().
aov_canopy_landtype<- aov(Existing_Percent_Canopy ~ new_land_types, data = acres_by_land_type)
summary(aov_canopy_landtype)

# There is a significant difference in tree canopy across land types!
# Let's run emmeans to do pairwise comparisons.
pairwise_landtype<- emmeans(canopy_lm_log, specs = pairwise ~ new_land_types)
summary(pairwise_landtype)
letters_pairwise_landtype<- cld(object = pairwise_landtype, adjust = "sidak",
                                Letters = letters, alpha = 0.05, 
                                which = seq_along(pairwise_landtype))
# Commercial is significantly lower than residential
# Commercial is significantly lower than ag
# Commercial is significantly lower than vacant
# Residential is significantly higher than transportation
# Health is significantly lower than vacant
# Transportation is significantly lower than ag, recreation, and vacant

# Percent impervious ~ land type
impervious_lm<- lm(Existing_Percent_Imperv ~ new_land_types, data = acres_by_land_type)
#plot(impervious_lm)
summary(impervious_lm)

impervious_lm_log<- lm(log(Existing_Percent_Imperv + 1)
                       ~ new_land_types, data = acres_by_land_type)
#plot(impervious_lm_log)
summary(impervious_lm_log)
# Data do not look more normal with log transformation. Maybe we need to 
  # specify a different distribution?

# Percent canopy ~ income
# Assign low, medium, or high income classes based on percentile of med inc.
grouped_income_df<- shape_income_sel %>%  
  mutate(income_class = case_when(Median_inc > quantile(Median_inc, 0.66) ~ "high", 
                                Median_inc < quantile(Median_inc, 0.33) ~ "low", 
                                between(Median_inc, 
                                        quantile(Median_inc, 0.33),
                                        quantile(Median_inc, 0.66)) ~ "medium"))

grouped_income_df$can_prop<- grouped_income_df$Can_P/100
grouped_income_df$imperv_prop<- grouped_income_df$Imperv_P/100

avg_inc_df<- grouped_income_df %>%
  group_by(income_class) %>%
  summarise(avg_inc = mean(Median_inc))

avg_can_df<- grouped_income_df %>%
  group_by(Median_inc) %>%
  summarise(avg_can = mean(can_prop), avg_imperv = mean(imperv_prop))

med_inc_lm<- lm(can_prop ~ Median_inc*imperv_prop, data = grouped_income_df)
summary(med_inc_lm)
#plot(med_inc_lm)
Anova(med_inc_lm, type = "III")

income_lm<- lm(can_prop ~ income_class*imperv_prop, data = grouped_income_df)
summary(income_lm)
#plot(income_lm)
Anova(income_lm, type = "III")
# qq plot looks a bit better when imperviousness is included, but other
  # residual plots still look odd.
ggplot(grouped_income_df, aes(x = income_class, y = can_prop)) +
  geom_boxplot() +
  theme_ew()

ggplot(avg_can_df, aes(x = Median_inc, y = avg_can, col = avg_imperv)) +
  geom_point() +
  theme_ew() +
  ylim(0.0, 1.0) +
  xlab("Median income (USD)") +
  ylab ("Mean prop. tree canopy") +
  labs(col = "Mean prop. imperviousness") +
  scale_color_continuous(name = "Mean prop. imperviousness", 
                         lim = c(0.0, 1.0))
# High income groups have the highest canopy by far, and low income has slightly
  # higher canopy than medium income.
         

