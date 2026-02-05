## Time 2 Carbon.R
## Version 1.0


## Chrisopher Tsz Hin Choi
## Christopher.Choi@colostate.edu

## 06/30/2023


## Import FIA db tables (csv)


#************************************Start*************************************#


##########################################
## Set Up
##########################################

## Set wd to script source
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path ))
print( getwd() )

## Removes scientific notation
options(scipen = 100)

library(dplyr)
library(DBI)


##########################################
## Data
##########################################

time2data <- read.csv('Data/tree_data_time2_CSU_plots_06122024.csv', header = T)


Table_S1a <- read.csv('Data/WO-GTR-104-Supp1/Table S1a_volib_coefs_spcd.csv', header = T)

Table_S2a <- read.csv('Data/WO-GTR-104-Supp1/Table S2a_volbk_coefs_spcd.csv', header = T)

Table_S5a <- read.csv('Data/WO-GTR-104-Supp1/Table S5a_rcumib_coefs_spcd.csv', header = T)

Table_S6a <- read.csv('Data/WO-GTR-104-Supp1/Table S6a_bark_biomass_coefs_spcd.csv', header = T)

Table_S7a <- read.csv('Data/WO-GTR-104-Supp1/Table S7a_branch_biomass_coefs_spcd.csv', header = T)

Table_S8a <- read.csv('Data/WO-GTR-104-Supp1/Table S8a_total_biomass_coefs_spcd.csv', header = T)



REF_SPECIES <- read.csv('Data/FIADB_REFERENCE/REF_SPECIES.csv', header = T)


##########################################
## Add Carbon Fractions
##########################################

time2data <- time2data|>
  dplyr::left_join(REF_SPECIES |> dplyr::select(SPCD, CARBON_RATIO_LIVE), by = 'SPCD')


##########################################
## DensProp & BarkProp (Harmon et al.2011)
##########################################

PropTable <- data.frame(SPClass = c('Hardwood','Hardwood','Hardwood','Hardwood','Hardwood','Softwood','Softwood','Softwood','Softwood','Softwood'),
                        DECAYCD = c(1,2,3,4,5,1,2,3,4,5),
                        DensProp = c(0.99, 0.80, 0.54, 0.43, 0.43, 0.97, 1.0,0.92, 0.55, 0.55),
                        BarkProp = c(1.0,0.8,0.5,0.2,0,1.0, 0.8,0.5,0.2,0),
                        BranchProp = c(1.0,0.5,0.1,0,0,1.0,0.5,0.1,0,0))


time2data2 <- time2data |>
  dplyr::mutate(SPClass = dplyr::if_else(SPCD < 300, 'Hardwood', 'Softwood')) |>
  dplyr::left_join(PropTable, by = c('SPClass', 'DECAYCD')) |>
  dplyr::mutate(DensProp = dplyr::if_else(STATUSCD == 1, 1, DensProp)) |>
  dplyr::mutate(BarkProp = dplyr::if_else(STATUSCD == 1, 1, BarkProp)) |>
  dplyr::mutate(BranchProp = dplyr::if_else(STATUSCD == 1, 1, BranchProp))

##########################################
## Add CULL (Temporary)
##########################################

## We're gonna use CULL from it's previous measurement, otherwise assume 0 CULL.
## This should be a conservative assumption

####################
## SQLITE database
####################

#database path
sqlite_path <- file.path('Data/SQLite_FIADB_CO/SQLite_FIADB_CO.db')

## connection to db
conn <- DBI::dbConnect(RSQLite::SQLite(), sqlite_path)

DBI::dbListTables(conn)
# DBI::dbListFields(conn, 'TREE')


## Tree table
######################################################
TREE <- DBI::dbReadTable(conn, "TREE")


## Disconnect from db
DBI::dbDisconnect(conn)


######################################################
TREE2 <- TREE |>
  dplyr::filter((PLOT == 82897 & SUBP == 1) |
                  (PLOT == 82897 & SUBP == 4) |
                  
                  (PLOT == 82199 & SUBP == 1) |
                  (PLOT == 82199 & SUBP == 2) |
                  (PLOT == 82199 & SUBP == 3) |
                  (PLOT == 82199 & SUBP == 4) |
                  
                  (PLOT == 85498 & SUBP == 1) |
                  (PLOT == 85498 & SUBP == 4) |
                  
                  (PLOT == 87545 & SUBP == 1) |
                  (PLOT == 87545 & SUBP == 2) |
                  (PLOT == 87545 & SUBP == 3) |
                  
                  (PLOT == 90710 & SUBP == 2) |
                  
                  (PLOT == 88772 & SUBP == 1) |
                  (PLOT == 88772 & SUBP == 2) |
                  (PLOT == 88772 & SUBP == 3) |
                  (PLOT == 88772 & SUBP == 4) |
                  
                  (PLOT == 84364 & SUBP == 1) |
                  (PLOT == 84364 & SUBP == 2) |
                  (PLOT == 84364 & SUBP == 3) |
                  
                  (PLOT == 87154 & SUBP == 1) |
                  (PLOT == 87154 & SUBP == 4) |
                  
                  (PLOT == 91181 & SUBP == 1) |
                  (PLOT == 91181 & SUBP == 2) |
                  (PLOT == 91181 & SUBP == 3) |
                  
                  (PLOT == 87420 & SUBP == 1) |
                  (PLOT == 87420 & SUBP == 4) |
                  
                  (PLOT == 80171 & SUBP == 1)
  ) |>
  dplyr::group_by(PLOT, SUBP, TREE) |>
  dplyr::filter(INVYR == max(INVYR)) |> ## This only keeps the most recent record of a tree being measured, instead of 2 records from 2 panels that makes joining to the time2 data complicated
  dplyr::ungroup()


time2data3 <- time2data2 |>
  dplyr::mutate(PREV_PLOT = dplyr::case_when(PLOT == 3023 ~ 82897,
                                             PLOT == 3001 ~ 82199,
                                             PLOT == 149 ~ 85498,
                                             PLOT == 104 ~ 87545,
                                             PLOT == 3031 ~ 90710,
                                             PLOT == 83 ~ 88772,
                                             PLOT == 66 ~ 84364,
                                             PLOT == 38 ~ 87154,
                                             PLOT == 37 ~ 91181,
                                             PLOT == 20 ~ 87420,
                                             PLOT == 3040 ~ 80171)) |>
  dplyr::left_join(TREE2 |> dplyr::select(PLOT, SUBP, TREE, CULL), 
                   by = c('PREV_PLOT' = 'PLOT', 'SUBP', 'TREE')) |>
  dplyr::mutate(CULL = dplyr::if_else(is.na(CULL) == T, 0,
                                      CULL))
  




#******************************************************************************#

## First order equations

##########################################
## AGBpredicted (Predicted aboveground biomass)
##########################################

AGBpredicted <- time2data3 |>
  dplyr::mutate(DIVISION = dplyr::case_when(SPCD == 108 ~ "M330",
                                            SPCD == 93 ~ "M330",
                                            SPCD == 19 ~ "",
                                            SPCD == 746 ~ "M330",
                                            SPCD == 113 ~ "",
                                            SPCD == 122 ~ "M330",
                                            SPCD == 202 ~ "M330"
  )) |>
  dplyr::left_join(Table_S8a, by = c('SPCD', 'DIVISION')) |>
  dplyr::mutate(AGBpredicted = a * DIA^b * HT^c) |>
  dplyr::select(STATECD, COUNTYCD, PLOT, INVYR, SUBP, TREE, SPCD,
                AGBpredicted)
  


##########################################
## Vtotib Gross (Total stem wood volume in cubic feet)
##########################################

# Vtotib Gross = a × k(b – b1) × Db1 × Hc

VtotibGross <- time2data3 |>
  dplyr::mutate(DIVISION = dplyr::case_when(SPCD == 108 ~ "M330",
                                            SPCD == 93 ~ "M330",
                                            SPCD == 19 ~ "M330",
                                            SPCD == 746 ~ "M330",
                                            SPCD == 113 ~ "",
                                            SPCD == 122 ~ "M330",
                                            SPCD == 202 ~ "M330"
  )) |>
  dplyr::left_join(Table_S1a, by = c('SPCD', 'DIVISION')) |>
  dplyr::mutate(k = dplyr::if_else(SPCD < 300, 9, 11), # k is dependent on hard/softwood type
                VtotibGross = a * k^(b-b1) * DIA^b1 * HT^c) |> # Total stem wood volume in cubic feet
  dplyr::select(STATECD, COUNTYCD, PLOT, INVYR, SUBP, TREE, SPCD,
                VtotibGross)


##########################################
## WDSG (Wood Density Specific Gravity)
##########################################

WDSG <- time2data3 |>
  dplyr::left_join(REF_SPECIES |> dplyr::select(SPCD, WOOD_SPGR_GREENVOL_DRYWT), 
                   by = 'SPCD') |>
  dplyr::rename(WDSG = WOOD_SPGR_GREENVOL_DRYWT) |> 
  dplyr::select(STATECD, COUNTYCD, PLOT, INVYR, SUBP, TREE, SPCD,
                WDSG)


##########################################
## Wtotbk (Total stem bark weight)
##########################################

Wtotbk <- time2data3 |>
  dplyr::mutate(DIVISION = dplyr::case_when(SPCD == 108 ~ "M330",
                                            SPCD == 93 ~ "",
                                            SPCD == 19 ~ "M330",
                                            SPCD == 746 ~ "M330",
                                            SPCD == 113 ~ "",
                                            SPCD == 122 ~ "",
                                            SPCD == 202 ~ "M330"
  )) |>
  dplyr::left_join(Table_S6a, by = c('SPCD', 'DIVISION')) |>
  dplyr::mutate(Wtotbk = a * DIA^b * HT^c) |> # Total stem bark volume in cubic feet
  dplyr::select(STATECD, COUNTYCD, PLOT, INVYR, SUBP, TREE, SPCD,
                Wtotbk)


##########################################
## Wbranch (Total branch weight)
##########################################

Wbranch <- time2data3 |>
  dplyr::mutate(DIVISION = dplyr::case_when(SPCD == 108 ~ "M330",
                                            SPCD == 93 ~ "M330",
                                            SPCD == 19 ~ "",
                                            SPCD == 746 ~ "M330",
                                            SPCD == 113 ~ "",
                                            SPCD == 122 ~ "M330",
                                            SPCD == 202 ~ "M330"
  )) |>
  dplyr::left_join(Table_S7a, by = c('SPCD', 'DIVISION')) |>
  dplyr::mutate(Wbranch = a * DIA^b * HT^c) |> # Total stem bark volume in cubic feet
  dplyr::select(STATECD, COUNTYCD, PLOT, INVYR, SUBP, TREE, SPCD,
                Wbranch)


##########################################
## R1 
##########################################

R1 <- time2data3 |>
  dplyr::mutate(DIVISION = dplyr::case_when(SPCD == 108 ~ "M330",
                                            SPCD == 93 ~ "M330",
                                            SPCD == 19 ~ "M330",
                                            SPCD == 746 ~ "M330",
                                            SPCD == 113 ~ "",
                                            SPCD == 122 ~ "M330",
                                            SPCD == 202 ~ "M330"
  )) |>
  dplyr::left_join(Table_S5a, by = c('SPCD', 'DIVISION')) |>
  dplyr::mutate(R1 = (1-(1-1/HT)^alpha)^beta) |>
  dplyr::select(STATECD, COUNTYCD, PLOT, INVYR, SUBP, TREE, SPCD, ##ACTUALHT, HT, alpha, beta,
                R1)

##########################################
## Rm (proportion of total stem volume from 1 foot stump to 4 inch top)
##########################################

Rm <- time2data3 |>
  dplyr::mutate(DIVISION = dplyr::case_when(SPCD == 108 ~ "M330",
                                            SPCD == 93 ~ "M330",
                                            SPCD == 19 ~ "M330",
                                            SPCD == 746 ~ "M330",
                                            SPCD == 113 ~ "",
                                            SPCD == 122 ~ "M330",
                                            SPCD == 202 ~ "M330"
  )) |>
  dplyr::left_join(Table_S5a, by = c('SPCD', 'DIVISION')) |>
  dplyr::mutate(ACTUALHT = dplyr::coalesce(ACTUALHT, HT, na.rm = T), # if value is NULL, ACTUALHT = HT
                Rm = (1-(1-ACTUALHT/HT)^alpha)^beta) |> # Total stem bark volume in cubic feet
  dplyr::select(STATECD, COUNTYCD, PLOT, INVYR, SUBP, TREE, SPCD, ##ACTUALHT, HT, alpha, beta,
                Rm)



####################################################################################
## Combining first order variables
####################################################################################

time2data4 <- time2data3 |>
  cbind(AGBpredicted |> dplyr::select(AGBpredicted)) |>
  cbind(WDSG |> dplyr::select(WDSG)) |>
  cbind(VtotibGross |> dplyr::select(VtotibGross)) |>
  cbind(Wtotbk |> dplyr::select(Wtotbk)) |>
  cbind(Wbranch |> dplyr::select(Wbranch)) |>
  cbind(R1 |> dplyr::select(R1)) |>
  cbind(Rm |> dplyr::select(Rm))



#******************************************************************************#

## Second order equations

##########################################
## Wtotib (Total stem wood)
##########################################

Wtotib <- time2data4 |> 
  dplyr::mutate(Wtotib = VtotibGross * WDSG * 62.4) |>
  dplyr::select(STATECD, COUNTYCD, PLOT, INVYR, SUBP, TREE, SPCD,
                Wtotib)

##########################################
## Wbranchred (Weight of branch reduction)
##########################################

Wbranchred <-time2data4 |>
  dplyr::mutate(Wbranchred = Wbranch*DensProp * BarkProp) |>
  dplyr::select(STATECD, COUNTYCD, PLOT, INVYR, SUBP, TREE, SPCD,
                Wbranchred)

##########################################
## Wtotbkred (Total weight of bark reduction)
##########################################

Wtotbkred <- time2data4 |>
  dplyr::mutate(Wtotbkred = Wtotbk*Rm*DensProp*BarkProp)|>
  dplyr::select(STATECD, COUNTYCD, PLOT, INVYR, SUBP, TREE, SPCD,
                Wtotbkred)


##########################################
## VstumpibGross (Stump wood volume)
##########################################

VstumpibGross <- time2data4 |>
  dplyr::mutate(VstumpibGross = R1 * VtotibGross)|>
  dplyr::select(STATECD, COUNTYCD, PLOT, INVYR, SUBP, TREE, SPCD,
                VstumpibGross)


##########################################
## VmeribSound (Volume of merchanable stem)
##########################################

VmeribSound <- time2data4 |>
  dplyr::mutate(VmeribSound = ((Rm * VtotibGross) - (R1 * VtotibGross)) * (1 - CULL/100)) |>
  dplyr::select(STATECD, COUNTYCD, PLOT, INVYR, SUBP, TREE, SPCD,
                VmeribSound)



####################################################################################
## Combining second order variables
####################################################################################

time2data5 <- time2data4 |>
  cbind(Wtotib |> dplyr::select(Wtotib)) |>
  cbind(Wbranchred |> dplyr::select(Wbranchred)) |>
  cbind(Wtotbkred |> dplyr::select(Wtotbkred)) |>
  cbind(VstumpibGross |> dplyr::select(VstumpibGross)) |>
  cbind(VmeribSound |> dplyr::select(VmeribSound))
  


#******************************************************************************#


## Third order equations


##########################################
## VstumpibSound (Sound stump wood)
##########################################

VstumpibSound <- time2data5 |>
  dplyr::mutate(VstumpibSound = VstumpibGross * (1-CULL/100)) |>
  dplyr::select(STATECD, COUNTYCD, PLOT, INVYR, SUBP, TREE, SPCD, #VstumpibGross, CULL,
                VstumpibSound)


####################################################################################
## Combining third order variables
####################################################################################

time2data6 <- time2data5 |>
  cbind(VstumpibSound |> dplyr::select(VstumpibSound))


#******************************************************************************#


## Fourth order equations

##########################################
## VtotibSound (Sound stump wood)
##########################################

VtotibSound <- time2data6 |>
  dplyr::mutate(VtotibSound = VmeribSound + VstumpibSound) |>
  dplyr::select(STATECD, COUNTYCD, PLOT, INVYR, SUBP, TREE, SPCD, #VmeribSound, VstumpibSound,
                VtotibSound)

####################################################################################
## Combining fourth order variables
####################################################################################

time2data7 <- time2data6 |>
  cbind(VtotibSound |> dplyr::select(VtotibSound))


#******************************************************************************#


## Fifth order equations

##########################################
## Wtotibred
## Live trees
##########################################

Wtotibred_live <- time2data7 |>
  dplyr::mutate(CULL = 1) |>
  dplyr::mutate(DensPropTemp = dplyr::if_else(SPClass == 'Hardwood', 0.54, 0.92)) |> ## assumed that CULL is decaycd = 3
  dplyr::mutate(Wtotibred_live = VtotibGross * (1 - CULL/100 * (1 - DensPropTemp) * WDSG * 62.4)) |>
  dplyr::select(STATECD, COUNTYCD, PLOT, INVYR, SUBP, TREE, SPCD, STATUSCD, #VtotibGross, CULL, 
                DensPropTemp,
                Wtotibred_live)


##########################################
## Wtotibred
## Dead trees
##########################################

Wtotibred_dead <- time2data7 |>
  dplyr::mutate(CULL = 1) |>
  dplyr::mutate(Wtotibred_dead = VtotibGross * (1 - CULL/100 * (1 - DensProp) * WDSG * 62.4)) |>
  dplyr::select(STATECD, COUNTYCD, PLOT, INVYR, SUBP, TREE, SPCD, STATUSCD, #VtotibGross, CULL, 
                DensProp,
                Wtotibred_dead)

##########################################
## Combine Wtotibred and match based on live/dead
##########################################

Wtotibred <- cbind(Wtotibred_live, Wtotibred_dead |> dplyr::select(Wtotibred_dead)) |> # dplyr::left_join(Wtotibred_live, Wtotibred_dead, by = c('STATECD', 'COUNTYCD', 'PLOT', 'SUBP', 'TREE')) |>
  dplyr::mutate(Wtotibred = dplyr::if_else(STATUSCD == 1, Wtotibred_live, Wtotibred_dead)) |> ## STATUSCD==2|3 is dead
  dplyr::select(STATECD, COUNTYCD, PLOT, INVYR, SUBP, TREE, SPCD, STATUSCD,
                Wtotibred)



####################################################################################
## Combining fifth order variables
####################################################################################

time2data8 <- time2data7 |>
  cbind(Wtotibred |> dplyr::select(Wtotibred))


#******************************************************************************#

## Sixth order equations

##########################################
## AGBcomponentred (Reduced total aboveground biomass)
##########################################

AGBcomponentred = time2data8 |>
  dplyr::mutate(AGBcomponentred = Wtotibred + Wtotbkred + Wbranchred) |>
  dplyr::select(STATECD, COUNTYCD, PLOT, INVYR, SUBP, TREE, SPCD,
                AGBcomponentred)


####################################################################################
## Combining sixth order variables
####################################################################################

time2data9 <- time2data8 |>
  cbind(AGBcomponentred |> dplyr::select(AGBcomponentred))
  


#******************************************************************************#

## Seventh order equations

##########################################
## AGBreduce (AGBpredicted with reduction factor)
##########################################

AGBreduce <- time2data9 |>
  dplyr::mutate(AGBreduce = AGBcomponentred/(Wtotib + Wtotbk + Wbranch))

####################################################################################
## Combining seventh order variables
####################################################################################

time2data10 <- time2data9 |>
  cbind(AGBreduce |> dplyr::select(AGBreduce))


#******************************************************************************#

## Eighth order equations

##########################################
## AGBpredictedred 
##########################################

AGBpredictedred <- time2data10 |>
  dplyr::mutate(AGBpredictedred = AGBpredicted * AGBreduce)


####################################################################################
## Combining Eighth order variables
####################################################################################

time2data11 <- time2data10 |>
  cbind(AGBpredictedred |> dplyr::select(AGBpredictedred))


#******************************************************************************#

## Ninth order equations

##########################################
## Carbon 
##########################################


Time2Carbon <- time2data11 |>
  dplyr::mutate(CARBON_AG = AGBpredicted * CARBON_RATIO_LIVE)



#******************************************************************************#

write.csv(Time2Carbon, 'Data/Time2Carbon.csv')



