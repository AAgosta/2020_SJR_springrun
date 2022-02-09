# Updated starting Jan 2022 ------------------------------
# Updated for actual 2020 full model ------------------------------

# The goals of this code are to 
# 1. truncate your data to only include the sites you are using for the model
# 2. Add site array codes to all of your receiver locations
# 3. Simplify detection history by reducing detections down to first and last detection at each location

# Gabe's original code is located :C:\Users\chause\Documents\GitHub\SJR_MS\script\Make_Visit_Site_Arrary_Code.R

library(tidyverse)
library(here)
library(readxl)


# First: reading in prelimiary dataset. Before predator filter has been ran


dets_all <- read_csv("data/backup files/qFishByRelGroup_AllRelGroups_updatedRKM_backup.csv")
#tag IDs are normal format (not in exponential) YAY!
#dets_all <- read_csv("data/qFishByRelGroup_2020_AllReleaseGroups.csv")

dets_up <- read_csv("data_output/2020Dets_raw_UpperRelease_updatedRKM.csv")

dets_ds <- read_csv("data_output/2020Dets_raw_LowerRelease_updatedRKM.csv")

dets_ft <- read_csv("data_output/2020Dets_raw_FTRelease_updatedRKM.csv")

# DOUBLE CHECK you have all recievers you want in here:
length(unique(dets_all$`GPS Names`)) # only 117  
#missing data for GG6 15045. but is all good because the tag info shows that it is likely a predator (went from FT to SAC at GEO site in less than 3 days, really fast to move upstream!!)

names <- unique(dets_all$`GPS Names`)
names[order(unique(dets_all$`GPS Names`))]
#one GG receiver missing info above

#### Only pull out the detections for the receivers we want in the full model
#1. Define which receivers are in the 2020 model

#------------------------------------------------------
#(see schematic in the documents folder)
gps_names <-(unique(dets_all$`GPS Names`))
gps_names[order(gps_names)]
model_recs <- c( "Benicia01",
                 "Benicia02",
                 "Benicia03",
                 "Benicia04", 
                 "Benicia05", 
                 "Benicia06",
                 "Benicia07",
                 "Benicia09",
                 "Benicia10",
                 "Benicia11", 
                 "Benicia12",
                 "Benicia13",
                 "Benicia14",
                 "Benicia15",
                 "Benicia16",
                 "CC_intake_J",
                 "CC_RGD_2_J",
                 "CC_RGU1_J",
                 "CC_RGU2_J",              
                 "Chipps1.1",              
                 "Chipps1.2",
                 "Chipps1.3",              
                 "Chipps1.4",              
                 "Chipps1.5",
                 "Chipps2.1",              
                 "Chipps2.3",              
                 "Chipps2.4",             
                 "Chipps2.5",              
                 "CVP_Tank2_J",            
                 "CVP_Tank3_J",           
                 "CVPD_J",                 
                 "CVPU_J",                
                 "Delt_TC_2_J",           
                 "Delt_TC_3_J",            
                 "Delt_TC_4_J",            
                 "FC1",                   
                 "FC2",                    
                 "FR1",                    
                 "FR2",                   
                 "FR3",                   
                 "FR4",                    
                 "FTOR1",                 
                 "FTOR2",                  
                 "FTOR3",                 
                 "FTOR4",                 
                 "FTOR6",                  
                 "GEO_1",                  
                 "GEO_2",                 
                 "GEO_3",                  
                 "GG2.1",                  
                 "GG7",                   
                 "HC1",                    
                 "HC2",                    
                 "HC3",                   
                 "HC4",                    
                 "I80-50_Br2",             
                 "JP_1_J",                
                 "JP_2_J",                
                 "JP_3_J",                 
                 "JP_4_J",                
                 "JP_5_J",                 
                 "JP_6_J",                 
                 "JP_7_J",                
                 "JP_8_J",                 
                 "MAC_2_J",                
                 "MAC_4_J",               
                 "OLD_2",                  
                 "OR Q1",                 
                 "OR Q2",                 
                 "OR_HOR_2_J",             
                 "OR_HOR_3_J",             
                 "OR_HOR_4_J",            
                 "OR_hwy4_1_J",            
                 "OR_hwy4_3_J",            
                 "OR_hwy4_4_J",           
                 "OR_MidR_1_J",            
                 "OR_MidR_2_J",            
                 "SBG_1",                 
                 "SBG_2",                  
                 "SBG_3",                  
                 "SBG_4",                 
                 "SJ_BCA_2_J",             
                 "SJ_Blw_Newman_1_J",      
                 "SJ_Blw_Newman_2_J",     
                 "SJ_Blw_Release_1_J",     
                 "SJ_Blw_Release_2_J",     
                 "SJ_BlwCrowsLanding_1_J",
                 "SJ_BlwCrowsLanding_2_J", 
                 "SJ_BlwWGrayson_1_J",     
                 "SJ_BlwWGrayson_2_J",    
                 "SJ_Durhamferry_1_J",     
                 "SJ_Durhamferry_2_J",     
                 "SJ_Hills_1_J",          
                 "SJ_Hills_2_J",           
                 "SJ_Hills_RT_1_US",       
                 "SJ_HOR_2_J",            
                 "SJ_HOR_3_J",             
                 "SJ_Howard_1_J",          
                 "SJ_Howard_2_J",         
                 "SJ_Moss_1_J",            
                 "SJ_SJG_1_J",             
                 "SJ_SJG_2_J",            
                 "SJ_SJG_3_J",             
                 "SJ_SJG_4_J",             
                 "SM1",                   
                 "SM2",                    
                 "SRVD_2",                 
                 "SRVD_3",                
                 "SRVD_4",                 
                 "SRVU_1",                 
                 "SRVU_3",                
                 "SRVU_4",                 
                 "SRVU_5",                
                 "Taylor_Sl",             
                 "TowerBr1",               
                 "TowerBr2",               
                 "TowerBr3")

# See which recievers are in our dataset but not on this list:
model_recs %in% gps_names
model_recs[model_recs %in% gps_names == F] # says which ones are in model_recs and not in our dataset. showing information from receivers we don't have??

# Pull out only the detections from the receivers in the model_recs string using the %in% function:
# Upper:----
truncated_dets_up<- dets_up %>% 
  filter(`GPS Names` %in% model_recs) # must do Detect_Location %in% model_recs bc the way this function works is it returns 
#a TRUE if there is a match on the LEFT side, so you have to put the list of things that you what you want to match up on the RIGHT
# you could also write the code out this way:
#truncated_up <- dets_up[dets_up$GeneralLocation %in% model_recs, ]

#delta:----
truncated_dets_ds <- dets_ds %>% 
  filter(`GPS Names` %in% model_recs)

#Franks Tract:----
truncated_dets_ft <- dets_ft %>% 
  filter(`GPS Names` %in% model_recs)

#all releases:----
truncated_dets_all <- dets_all %>% 
  filter(`GPS Names` %in% model_recs)

#### Adding Site Codes (THIS IS OLD...)
# below are the Detection_location to site code pairings:
# Rel1 - Upstream_Release
# Rel2 - SJ_Durhamferry_RelRec_J
# RelF - Franks_Tract_Release
# A1   - SJ_Blw_Release_1_J, SJ_Blw_Release_2_J,  
# A2   - SJ_Mud_SL_Conf_J
# A3   - SJ_Blw_Newman_1_J, SJ_Blw_Newman_2_J  
# A4   - SJ_Hills_1_J,SJ_Hills_2_J, SJ_Hills_RT_1_US,SJ_Hills_RT_2_DS 
# A5   - SJ_BlwCrowsLanding_1_J & SJ_BlwCrowsLanding_2_J
# A6   - SJ_BlwWGrayson_1_J & SJ_BlwWGrayson_2_J
# A7   - SJ_Durhamferry_1_J & SJ_Durhamferry_2_J
# A8   - SJ_BCA_1_J,  SJ_BCA_2_J,
# A9   - SJ_Moss_1_J, SJ_Moss_2_J
# A10  - SJ_HOR_1_J & SJ_HOR_2_J 
# B1   - OR_HOR_1_J,OR_HOR_2_J, OR_HOR_3_J & OR_HOR_4_J 
# A11  - SJ_Howard_1_J & SJ_Howard_2_J
# D1a  - CVPU_J
# D1b  - CVPD_J
# D1c  - CVP_Tank2_J, CVP_Tank3_J
# E1a  - CC_RGU1_J,CC_RGU2_J,   
# E1b  - CC_RGD_2_J
# E2   - CC_intake_J
# A12  - SJ_SJG_1_J & SJ_SJG_2_J, SJ_SJG_3_J & SJ_SJG_4_J
# B2   - OR_hwy4_1_J, "OR_hwy4_2_J", OR_hwy4_3_J,OR_hwy4_4_J 
# C1   - OR MR1 and 2
# C2   - MidR_hwy4_1, MidR_hwy4_2, MidR_hwy4_3, MidR_hwy4_4
# F1   - Delt_TC_2_J",Delt_TC_3_J,Delt_TC_4_J
# A13  - SJ_MAC_1_J, SJ_MAC_2_J, SJ_MAC_3_J & SJ_MAC_4_J
# A14a - JP 1-4
# A14b - JP 5-8
# A15a - Chipps 1.1-1.5 
# A15b - Chipps 2.1-2.5 (downstream)
# A16a - Benicia 10-16
# A16b - Benicia 1-8 (downstream)
# A17a - Golden Gate 7
# A17b - Golden Gate 2.1
# B3   - SM 1, SM2
# B4   - HC1, HC2, HC3, HC4
# B5   - OR Q1, OR Q2
# G1   - FTOR1, FTOR2, FTOR3, FTOR4, FTOR6
# G2   - FR1, FR2, FR3, FR4
# H1   - FC1 and FC2
# I1   - Taylor_Sl
# J1   - GEO_1, GEO_2
# K1   - I80-50_Br2
# L1   - OLD_2
# M1   - SBG_1, SBG_2, SBG_3, SBG_4
# N1b  - SRVD_2, SRVD_3, SRVD_4
# N1a  - SRVU_1, SRVU_3, SRVU_4, SRVU_5
# P1   - TowerBr1, TowerBr2, TowerBr3


# 2.Use function to add site code column to upper/delta detection files -------------------------------------------------------
# using subset [rows, columns] to filter rows using grep function (grep(pattern,where to look) and add "site.code" a the column. 
add_array_codes <- function(detection_file) {
  detection_file[grep('Upstream_Release', detection_file$`GPS Names`), 'site.code' ] <- 'REL'
  detection_file[grep('Delta Release', detection_file$`GPS Names`), 'site.code' ] <- 'REL'
  detection_file[grep('FT Release', detection_file$`GPS Names`), 'site.code' ] <- 'REL'
  detection_file[grep('SJ_Blw_Release_1_J', detection_file$`GPS Names`), 'site.code' ] <- 'A1'
  detection_file[grep('SJ_Blw_Release_2_J', detection_file$`GPS Names`), 'site.code' ] <- 'A1'
 # detection_file[grep('SJ_Mud_SL_Conf_J', detection_file$`GPS Names`), 'site.code' ] <- 'A2' # removed because no dets
  detection_file[grep('SJ_Blw_Newman_1_J', detection_file$`GPS Names`), 'site.code' ] <- 'A3'
  detection_file[grep('SJ_Blw_Newman_2_J', detection_file$`GPS Names`), 'site.code' ] <- 'A3'
  detection_file[grep('SJ_Hills_1_J', detection_file$`GPS Names`), 'site.code' ] <- 'A4'
  detection_file[grep('SJ_Hills_2_J', detection_file$`GPS Names`), 'site.code' ] <- 'A4'
  detection_file[grep('SJ_Hills_RT_1_US', detection_file$`GPS Names`), 'site.code' ] <- 'A4'
  detection_file[grep('SJ_BlwCrowsLanding_1_J', detection_file$`GPS Names`), 'site.code' ] <- 'A5'
  detection_file[grep('SJ_BlwCrowsLanding_2_J', detection_file$`GPS Names`), 'site.code' ] <- 'A5'
  detection_file[grep('SJ_BlwWGrayson_1_J', detection_file$`GPS Names`), 'site.code' ] <- 'A6'
  detection_file[grep('SJ_BlwWGrayson_2_J', detection_file$`GPS Names`), 'site.code' ] <- 'A6'
  detection_file[grep('SJ_Durhamferry_1_J', detection_file$`GPS Names`), 'site.code' ] <- 'A7'
  detection_file[grep('SJ_Durhamferry_2_J', detection_file$`GPS Names`), 'site.code' ] <- 'A7'
  #detection_file[grep('SJ_BCA_1_J', detection_file$`GPS Names`), 'site.code' ] <- 'A8'
      # no dets
  detection_file[grep('SJ_BCA_2_J', detection_file$`GPS Names`), 'site.code' ] <- 'A8'
  detection_file[grep('SJ_Moss_1_J', detection_file$`GPS Names`), 'site.code' ] <- 'A9'
  #detection_file[grep('SJ_Moss_2_J', detection_file$`GPS Names`), 'site.code' ] <- 'A9'
  # no dets
  detection_file[grep('SJ_HOR_2_J', detection_file$`GPS Names`), 'site.code' ] <- 'A10'
  detection_file[grep('SJ_HOR_3_J', detection_file$`GPS Names`), 'site.code' ] <- 'A10'
  #detection_file[grep('OR_HOR_1_J', detection_file$`GPS Names`), 'site.code' ] <- 'B1a'
        #no dets
  detection_file[grep('OR_HOR_2_J', detection_file$`GPS Names`), 'site.code' ] <- 'B1'
  detection_file[grep('OR_HOR_3_J', detection_file$`GPS Names`), 'site.code' ] <- 'B1'
  detection_file[grep('OR_HOR_4_J', detection_file$`GPS Names`), 'site.code' ] <- 'B1'
  detection_file[grep('OR_hwy4_1_J', detection_file$`GPS Names`), 'site.code' ] <- 'B2'
  detection_file[grep('OR_hwy4_3_J', detection_file$`GPS Names`), 'site.code' ] <- 'B2'
  #detection_file[grep('OR_hwy4_2_J', detection_file$`GPS Names`), 'site.code' ] <- 'B2'
        # no dets
  detection_file[grep('OR_hwy4_4_J', detection_file$`GPS Names`), 'site.code' ] <- 'B2'
  detection_file[grep('OR_MidR_1_J', detection_file$`GPS Names`), 'site.code' ] <- 'C1'
  detection_file[grep('OR_MidR_2_J', detection_file$`GPS Names`), 'site.code' ] <- 'C1'
  #detection_file[grep('MidR_hwy4_1', detection_file$`GPS Names`), 'site.code' ] <- 'C2'
  #detection_file[grep('MidR_hwy4_2', detection_file$`GPS Names`), 'site.code' ] <- 'C2'
  #detection_file[grep('MidR_hwy4_3', detection_file$`GPS Names`), 'site.code' ] <- 'C2'
  #detection_file[grep('MidR_hwy4_4', detection_file$`GPS Names`), 'site.code' ] <- 'C2'
        # no dets
  detection_file[grep('CVPU_J', detection_file$`GPS Names`), 'site.code' ] <- 'D1a'
  detection_file[grep('CVPD_J', detection_file$`GPS Names`), 'site.code' ] <- 'D1b'
  detection_file[grep('CVP_Tank2_J', detection_file$`GPS Names`), 'site.code' ] <- 'D1c'
  detection_file[grep('CVP_Tank3_J', detection_file$`GPS Names`), 'site.code' ] <- 'D1c'
  detection_file[grep('CC_RGU1_J', detection_file$`GPS Names`), 'site.code' ] <- 'E1a'
  detection_file[grep('CC_RGU2_J', detection_file$`GPS Names`), 'site.code' ] <- 'E1a'
  detection_file[grep('CC_RGD_2_J', detection_file$`GPS Names`), 'site.code' ] <- 'E1b'
  detection_file[grep('CC_intake_J', detection_file$`GPS Names`), 'site.code' ] <- 'E2'
  detection_file[grep('SJ_Howard_1_J', detection_file$`GPS Names`), 'site.code' ] <- 'A11'
  detection_file[grep('SJ_Howard_2_J', detection_file$`GPS Names`), 'site.code' ] <- 'A11'
  detection_file[grep('SJ_SJG_1_J', detection_file$`GPS Names`), 'site.code' ] <- 'A12'
  detection_file[grep('SJ_SJG_2_J', detection_file$`GPS Names`), 'site.code' ] <- 'A12'
  detection_file[grep('SJ_SJG_3_J', detection_file$`GPS Names`), 'site.code' ] <- 'A12'
  detection_file[grep('SJ_SJG_4_J', detection_file$`GPS Names`), 'site.code' ] <- 'A12'
  #detection_file[grep('MAC_1_J', detection_file$`GPS Names`), 'site.code' ] <- 'A13'
          # no dets
   detection_file[grep('MAC_2_J', detection_file$`GPS Names`), 'site.code' ] <- 'A13'
  #detection_file[grep('MAC_3_J', detection_file$`GPS Names`), 'site.code' ] <- 'A13'
          # no dets
  detection_file[grep('MAC_4_J', detection_file$`GPS Names`), 'site.code' ] <- 'A13'
  #detection_file[grep('Delt_TC_1', detection_file$`GPS Names`), 'site.code' ] <- 'F1'
          # no dets
  detection_file[grep('Delt_TC_2', detection_file$`GPS Names`), 'site.code' ] <- 'F1'
  detection_file[grep('Delt_TC_3', detection_file$`GPS Names`), 'site.code' ] <- 'F1'
  detection_file[grep('Delt_TC_4', detection_file$`GPS Names`), 'site.code' ] <- 'F1'
  detection_file[grep('JP_1_J', detection_file$`GPS Names`), 'site.code' ] <- 'A14a'
  detection_file[grep('JP_2_J', detection_file$`GPS Names`), 'site.code' ] <- 'A14a'
  detection_file[grep('JP_3_J', detection_file$`GPS Names`), 'site.code' ] <- 'A14a'
  detection_file[grep('JP_4_J', detection_file$`GPS Names`), 'site.code' ] <- 'A14a'
  detection_file[grep('JP_5_J', detection_file$`GPS Names`), 'site.code' ] <- 'A14b'
  detection_file[grep('JP_6_J', detection_file$`GPS Names`), 'site.code' ] <- 'A14b'
  detection_file[grep('JP_7_J', detection_file$`GPS Names`), 'site.code' ] <- 'A14b'
  detection_file[grep('JP_8_J', detection_file$`GPS Names`), 'site.code' ] <- 'A14b'
  detection_file[grep('Chipps1.1', detection_file$`GPS Names`), 'site.code' ] <- 'A15a'
  detection_file[grep('Chipps1.2', detection_file$`GPS Names`), 'site.code' ] <- 'A15a'
  detection_file[grep('Chipps1.3', detection_file$`GPS Names`), 'site.code' ] <- 'A15a'
  detection_file[grep('Chipps1.4', detection_file$`GPS Names`), 'site.code' ] <- 'A15a'
  detection_file[grep('Chipps1.5', detection_file$`GPS Names`), 'site.code' ] <- 'A15a'
  detection_file[grep('Chipps2.1', detection_file$`GPS Names`), 'site.code' ] <- 'A15b'
  #detection_file[grep('Chipps2.2', detection_file$`GPS Names`), 'site.code' ] <- 'A15b'
  detection_file[grep('Chipps2.3', detection_file$`GPS Names`), 'site.code' ] <- 'A15b'
  detection_file[grep('Chipps2.4', detection_file$`GPS Names`), 'site.code' ] <- 'A15b'
  detection_file[grep('Chipps2.5', detection_file$`GPS Names`), 'site.code' ] <- 'A15b'
  detection_file[grep('Benicia01', detection_file$`GPS Names`), 'site.code' ] <- 'A16b'
  detection_file[grep('Benicia02', detection_file$`GPS Names`), 'site.code' ] <- 'A16b'
  detection_file[grep('Benicia03', detection_file$`GPS Names`), 'site.code' ] <- 'A16b'
  detection_file[grep('Benicia04', detection_file$`GPS Names`), 'site.code' ] <- 'A16b'
  detection_file[grep('Benicia05', detection_file$`GPS Names`), 'site.code' ] <- 'A16b'
  detection_file[grep('Benicia06', detection_file$`GPS Names`), 'site.code' ] <- 'A16b'
  detection_file[grep('Benicia07', detection_file$`GPS Names`), 'site.code' ] <- 'A16b'
  #detection_file[grep('Benicia08', detection_file$`GPS Names`), 'site.code' ] <- 'A16b'
  detection_file[grep('Benicia09', detection_file$`GPS Names`), 'site.code' ] <- 'A16a'
  detection_file[grep('Benicia10', detection_file$`GPS Names`), 'site.code' ] <- 'A16a'
  detection_file[grep('Benicia11', detection_file$`GPS Names`), 'site.code' ] <- 'A16a'
  detection_file[grep('Benicia12', detection_file$`GPS Names`), 'site.code' ] <- 'A16a'
  detection_file[grep('Benicia13', detection_file$`GPS Names`), 'site.code' ] <- 'A16a'
  detection_file[grep('Benicia14', detection_file$`GPS Names`), 'site.code' ] <- 'A16a'
  detection_file[grep('Benicia15', detection_file$`GPS Names`), 'site.code' ] <- 'A16a'
  detection_file[grep('Benicia16', detection_file$`GPS Names`), 'site.code' ] <- 'A16a'
  #detection_file[grep('GG1', detection_file$`GPS Names`), 'site.code' ] <- 'A17b'
  #detection_file[grep('GG1.5', detection_file$`GPS Names`), 'site.code' ] <- 'A17b'
  #detection_file[grep('GG1.7', detection_file$`GPS Names`), 'site.code' ] <- 'A17b'
  detection_file[grep('GG2.1', detection_file$`GPS Names`), 'site.code' ] <- 'A17b'
  #detection_file[grep('GG2.5', detection_file$`GPS Names`), 'site.code' ] <- 'A17b'
  #detection_file[grep('GG3.1', detection_file$`GPS Names`), 'site.code' ] <- 'A17b'
  #detection_file[grep('GG3.5', detection_file$`GPS Names`), 'site.code' ] <- 'A17b'
  #detection_file[grep('GG4', detection_file$`GPS Names`), 'site.code' ] <- 'A17b'
  #detection_file[grep('GG4.5', detection_file$`GPS Names`), 'site.code' ] <- 'A17b'
  #detection_file[grep('GG5.1', detection_file$`GPS Names`), 'site.code' ] <- 'A17b'
  #detection_file[grep('GG5.5', detection_file$`GPS Names`), 'site.code' ] <- 'A17a'
  #detection_file[grep('GG6', detection_file$`GPS Names`), 'site.code' ] <- 'A17a'
  #detection_file[grep('GG6.5', detection_file$`GPS Names`), 'site.code' ] <- 'A17a'
  detection_file[grep('GG7', detection_file$`GPS Names`), 'site.code' ] <- 'A17a'
  #detection_file[grep('GG7.2', detection_file$`GPS Names`), 'site.code' ] <- 'A17a'
  #detection_file[grep('GG7.5', detection_file$`GPS Names`), 'site.code' ] <- 'A17a'
  #detection_file[grep('GG7.7', detection_file$`GPS Names`), 'site.code' ] <- 'A17a'
  #detection_file[grep('GG8', detection_file$`GPS Names`), 'site.code' ] <- 'A17a'
  #detection_file[grep('GG8.4', detection_file$`GPS Names`), 'site.code' ] <- 'A17a'
  #detection_file[grep('GG9', detection_file$`GPS Names`), 'site.code' ] <- 'A17a'
  detection_file[grep('FC1', detection_file$`GPS Names`), 'site.code' ] <- 'H1'
  detection_file[grep('FC2', detection_file$`GPS Names`), 'site.code' ] <- 'H1'
  detection_file[grep('FR1', detection_file$`GPS Names`), 'site.code' ] <- 'G2'
  detection_file[grep('FR2', detection_file$`GPS Names`), 'site.code' ] <- 'G2'
  detection_file[grep('FR3', detection_file$`GPS Names`), 'site.code' ] <- 'G2'
  detection_file[grep('FR4', detection_file$`GPS Names`), 'site.code' ] <- 'G2'
  detection_file[grep('FTOR1', detection_file$`GPS Names`), 'site.code' ] <- 'G1'
  detection_file[grep('FTOR2', detection_file$`GPS Names`), 'site.code' ] <- 'G1'
  detection_file[grep('FTOR3', detection_file$`GPS Names`), 'site.code' ] <- 'G1'
  detection_file[grep('FTOR4', detection_file$`GPS Names`), 'site.code' ] <- 'G1'
  #detection_file[grep('FTOR5', detection_file$`GPS Names`), 'site.code' ] <- 'G1'
          #no dets
  detection_file[grep('FTOR6', detection_file$`GPS Names`), 'site.code' ] <- 'G1'
  detection_file[grep('GEO_1', detection_file$`GPS Names`), 'site.code' ] <- 'J1'
  detection_file[grep('GEO_2', detection_file$`GPS Names`), 'site.code' ] <- 'J1'
  detection_file[grep('GEO_3', detection_file$`GPS Names`), 'site.code' ] <- 'J1'
  detection_file[grep('HC1', detection_file$`GPS Names`), 'site.code' ] <- 'B4'
  detection_file[grep('HC2', detection_file$`GPS Names`), 'site.code' ] <- 'B4'
  detection_file[grep('HC3', detection_file$`GPS Names`), 'site.code' ] <- 'B4'
  detection_file[grep('HC4', detection_file$`GPS Names`), 'site.code' ] <- 'B4'
  detection_file[grep('I80-50_Br2', detection_file$`GPS Names`), 'site.code' ] <- 'K1'
  detection_file[grep('OLD_2', detection_file$`GPS Names`), 'site.code' ] <- 'L1'
  detection_file[grep('OR Q1', detection_file$`GPS Names`), 'site.code' ] <- 'B5'
  detection_file[grep('OR Q2', detection_file$`GPS Names`), 'site.code' ] <- 'B5'
  detection_file[grep('HC4', detection_file$`GPS Names`), 'site.code' ] <- 'B4'
  detection_file[grep('SM1', detection_file$`GPS Names`), 'site.code' ] <- 'B3'
  detection_file[grep('SM2', detection_file$`GPS Names`), 'site.code' ] <- 'B3'
  detection_file[grep('SBG_1', detection_file$`GPS Names`), 'site.code' ] <- 'M1'
  detection_file[grep('SBG_2', detection_file$`GPS Names`), 'site.code' ] <- 'M1'
  detection_file[grep('SBG_3', detection_file$`GPS Names`), 'site.code' ] <- 'M1'
  detection_file[grep('SBG_4', detection_file$`GPS Names`), 'site.code' ] <- 'M1'
  detection_file[grep('SRVD_2', detection_file$`GPS Names`), 'site.code' ] <- 'N1b'
  detection_file[grep('SRVD_3', detection_file$`GPS Names`), 'site.code' ] <- 'N1b'
  detection_file[grep('SRVD_4', detection_file$`GPS Names`), 'site.code' ] <- 'N1b'
  detection_file[grep('SRVU_1', detection_file$`GPS Names`), 'site.code' ] <- 'N1a'
  detection_file[grep('SRVU_3', detection_file$`GPS Names`), 'site.code' ] <- 'N1a'
  detection_file[grep('SRVU_4', detection_file$`GPS Names`), 'site.code' ] <- 'N1a'
  detection_file[grep('SRVU_5', detection_file$`GPS Names`), 'site.code' ] <- 'N1a'
  detection_file[grep('Taylor_Sl', detection_file$`GPS Names`), 'site.code' ] <- 'I1'
  detection_file[grep('TowerBr1', detection_file$`GPS Names`), 'site.code' ] <- 'P1'
  detection_file[grep('TowerBr2', detection_file$`GPS Names`), 'site.code' ] <- 'P1'
  detection_file[grep('TowerBr3', detection_file$`GPS Names`), 'site.code' ] <- 'P1'
  return(detection_file)
}

# Add site codes:
truncated_dets_up<- add_array_codes(truncated_dets_up)
truncated_dets_ds<- add_array_codes(truncated_dets_ds)
truncated_dets_ft<- add_array_codes(truncated_dets_ft)
truncated_dets_all<- add_array_codes(truncated_dets_all)

# Check to make sure every site has an array code:
array_code_NAs_upper<- truncated_dets_up %>% 
  filter(is.na(site.code))
unique(array_code_NAs_upper$`GPS Names`)    #good

array_code_NAs_delta<- truncated_dets_ds %>% 
  filter(is.na(site.code)) 
unique(array_code_NAs_delta$`GPS Names`)    #good

array_code_NAs_frank<- truncated_dets_ft %>% 
  filter(is.na(site.code)) 
unique(array_code_NAs_frank$`GPS Names`)    #good

array_code_NAs_all<- truncated_dets_all %>% 
  filter(is.na(site.code)) 
unique(array_code_NAs_all$`GPS Names`)    #good

#### Make Array Code ------------------------------
# the array code is the code for all the receivers at a location, so it removes the a and b in the dual arrays. 
# gsub function: setting what you want to remove as "[a-z]" allows you to match a pattern and remove all those patterns
# so this means any lowercase letter will be removed, and replaced with nothing (""). ie A11a becomes A11

truncated_dets_up<- truncated_dets_up %>% 
  mutate(array.code = gsub("[a-z]", "", truncated_dets_up$site.code)) # also works with just "[a-b]"

truncated_dets_ds<- truncated_dets_ds %>% 
  mutate(array.code = gsub("[a-z]", "", truncated_dets_ds$site.code)) 

truncated_dets_ft<- truncated_dets_ft %>% 
  mutate(array.code = gsub("[a-z]", "", truncated_dets_ft$site.code))


truncated_dets_all<- truncated_dets_all %>% 
  mutate(array.code = gsub("[a-z]", "", truncated_dets_all$site.code))


# ><)))))*>  ------------- ><)))))*>  ------------- ><)))))*>  ------------- ><)))))*>  ------------- ><)))))*>  
# Simplifying Detection History:

####Arrange by Hex and datetime, combine hex and site code to make unique field---------------------------------
truncated_dets_up <- truncated_dets_up %>% 
  arrange(Hex, dtf) %>% 
  mutate(combo = paste0(Hex, site.code))

truncated_dets_ds <- truncated_dets_ds %>% 
  arrange(Hex, dtf) %>% 
  mutate(combo = paste0(Hex, site.code))

truncated_dets_ft <- truncated_dets_ft %>% 
  arrange(Hex, dtf) %>% 
  mutate(combo = paste0(Hex, site.code))


truncated_dets_all <- truncated_dets_all %>% 
  arrange(TagID_Hex, DetectDateTime) %>% 
  mutate(combo = paste0(TagID_Hex, site.code))

#### Keep first and last dets per visit ------------------------------------------------------------------------
#### arrange by Hex and datetime combine hex and Detect_Location to make a unique field 
upper_visit <- truncated_dets_up %>% 
  arrange(Hex, dtf) %>%
  mutate(combo = (paste0(Hex, site.code)))

delta_visit <- truncated_dets_ds %>% 
  arrange(Hex, dtf) %>%
  mutate(combo = (paste0(Hex, site.code)))

franks_visit <- truncated_dets_ft %>% 
  arrange(Hex, dtf) %>%
  mutate(combo = (paste0(Hex, site.code)))


all_visits <- truncated_dets_all %>% 
  arrange(TagID_Hex, DetectDateTime) %>%
  mutate(combo = (paste0(TagID_Hex, site.code)))


# count dets at each receiver (then, keep all 1's and max in each sequence)
upper_visit$counter <- sequence(rle(upper_visit$combo)$lengths)
# rle() function counts the number of times a specific combo occur and 
# stores that in a list. For example: there are 16 detections of BA5B so for that combo it would be stored as length = 16. wrapping sequence() around
# this then makes a squence of that number. so 16 becomes 1, 2, 3, 4, ...16. These sequences of numbers are then stored in the counter column
upper_visit$first_or_last <- (lead(upper_visit$counter)-upper_visit$counter)==1 
# This line of code says: in the counter vector, subtract the next
# number ( controlled by the lead () function) in the vector from the previous number in the vector. If it equals one, in the row in the column
# first_or_last will say TRUE, meaning it is the next number in the sequence for that combo (or really detection). If it does not equal 1 it will say
# FALSE instead, which means the first number for the next tag at the next receiver (ie the next combo) has been reached, and therefore the rows where
# it said false is the last detection of that tag at that receiver. because it would be X detection number minus the first (n=1) detection for the next tag (X-1 does not equal 1)
# SO IN SUMMARY: TRUE means it is the next detection in that sequence of detections for that specific tag at that receiver. FALSE means it is the last detection
# of that tag at that receicer for that sequence.

delta_visit$counter <- sequence(rle(delta_visit$combo)$lengths)
delta_visit$first_or_last <- (lead(delta_visit$counter)-delta_visit$counter)==1

franks_visit$counter <- sequence(rle(franks_visit$combo)$lengths)
franks_visit$first_or_last <- (lead(franks_visit$counter)-franks_visit$counter)==1


all_visits$counter <- sequence(rle(all_visits$combo)$lengths)
all_visits$first_or_last <- (lead(all_visits$counter)-all_visits$counter)==1


# filter to keep first and last dets ***double check with original df to see if the code worked
UpRel_visits2020_firstlast_nofilter <- upper_visit %>% 
  filter(counter==1|first_or_last==FALSE) # this is keeping just the first in the cou ter column and the last detection (denoted by FALSE in the 
# first_or_last column)
nrow(UpRel_visits2020_firstlast_nofilter)

DeltaRel_visits2020_firstlast_nofilter <- delta_visit %>% 
  filter(counter==1|first_or_last==FALSE)
nrow(DeltaRel_visits2020_firstlast_nofilter)

FranksRel_visits2020_firstlast_nofilter <- franks_visit %>% 
  filter(counter==1|first_or_last==FALSE)
nrow(FranksRel_visits2020_firstlast_nofilter)


ALLRel_visits2020_firstlast_nofilter <- all_visits %>% 
  filter(counter==1|first_or_last==FALSE)
nrow(ALLRel_visits2020_firstlast_nofilter)

# Save:
# These files are before the predator filter was placed on the data and updated the RKMs
#UPPER RELEASE
write_csv(UpRel_visits2020_firstlast_nofilter, "data_output/DetectionFiles/UpRel_visits2020_firstlast_All_02022022_noPredFilter_updatedRKM.csv") # before predator filter


#LOWER/DELTA RELEASE
write_csv(DeltaRel_visits2020_firstlast_nofilter, "data_output/DetectionFiles/DeltaRel_visits2020_firstlast_All_02022022_noPredFilter_updatedRKM.csv") # before predator filter

#FRANKS TRACT RELEASE
write_csv(FranksRel_visits2020_firstlast_nofilter, "data_output/DetectionFiles/FranksRel_visits2020_firstlast_All_02022022_noPredFilter_updatedRKM.csv") # before predator filter

#ALL RELEASES
write_csv(ALLRel_visits2020_firstlast_nofilter, "data_output/DetectionFiles/ALLRel_visits2020_firstlast_All_02022022_noPredFilter_updatedRKM.csv")

# filter to keep first dets

UpRel_visits2020_first <- upper_visit %>% 
  filter(counter==1)
nrow(UpRel_visits2020_first)

DeltaRel_visits2020_first <- delta_visit %>% 
  filter(counter==1)
nrow(DeltaRel_visits2020_first)

FranksRel_visits2020_first <- franks_visit %>% 
  filter(counter==1)
nrow(FranksRel_visits2020_first)

ALLRel_visits2020_first <- all_visits %>% 
  filter(counter==1)
nrow(ALLRel_visits2020_first)

# These files are before the predator filter was placed on the data and updated the RKMs
write_csv(UpRel_visits2020_first, "data_output/DetectionFiles/UpRel_visits2020_firstonly_All_NOAAdata_02022022_NoPredFilter_updatedRKM.csv")

write_csv(DeltaRel_visits2020_first, "data_output/DetectionFiles/DeltaRel_visits2020_firstonly_All_NOAAdata_02022022_NoPredFilter_updatedRKM.csv")

write_csv(FranksRel_visits2020_first, "data_output/DetectionFiles/FranksRel_visits2020_firstonly_All_NOAAdata_02022022_NoPredFilter_updatedRKM.csv") 


write_csv(ALLRel_visits2020_first, "data_output/DetectionFiles/ALLRel_visits2020_firstonly_All_NOAAdata_02022022_NoPredFilter_updatedRKM.csv") 


# ><)))))*>  ------------- ><)))))*>  ------------- ><)))))*>  ------------- ><)))))*>  ------------- ><)))))*>  

# not needed. extra code by Colby for 2019 analysis






################################################ end of code for analysis ######################################################################
################################################################################################################################################
################################################################################################################################################
################################################################################################################################################

# Below is the Code Colby wrote to keep first and last detections. for some reason it keeps 138 less detectopns than gabe's code 
# so use Gabe's code for the actual analyis. This is just so you have the code on hand if you need it:

#Make vector of unique tags:
upper_tags <- unique(truncated_dets_up$Hex)
print(upper_tags)

delta_tags <- unique(truncated_dets_ds$Hex)
print(delta_tags)

#Make vector of unique sites:
#upper_sites <- unique(truncated_dets2018_upper$site.code)
upper_combo <- unique(truncated_dets_up$combo)
print(upper_combo)
delta_combo <- unique(truncated_dets_ds$combo)
print(delta_combo)

#Make empty dataframe
df_first_last_upper2019 <- NULL

# Make df of first and last detections for upper release group:
for(tag in upper_tags) {
  print(tag)
  tag2 <- subset(truncated_dets_up, truncated_dets_up$Hex == tag)
  if(nrow(tag2) > 0) {
    for (site in upper_combo) {
      rec2 <- subset(tag2, tag2$combo == site)
      if (nrow(rec2)>0) {
        first_det <- min(rec2$dtf)
        last_det <- max(rec2$dtf)
        rkm <- rec2$rkm[1]
        Location <- rec2$'GPS Names'[1]
        array_code <- rec2$array.code[1]
        df_first_last_upper2019 <- rbind(df_first_last_upper2019,data.frame(tag, site, array_code, Location, first_det, last_det, rkm))
      }
    }
  }
}

# Make df of first and last detections for delta release group:
df_first_last_delta2019 <- NULL

for(tag in delta_tags) {
  print(tag)
  tag2 <- subset(truncated_dets_ds, truncated_dets_ds$Hex == tag)
  if(nrow(tag2) > 0) {
    for (site in delta_combo) {
      rec2 <- subset(tag2, tag2$combo == site)
      if (nrow(rec2)>0) {
        first_det <- min(rec2$dtf)
        last_det <- max(rec2$dtf)
        rkm <- rec2$rkm[1]
        Location <- rec2$'GPS Names'[1]
        array_code <- rec2$array.code[1]
        df_first_last_delta2019 <- rbind(df_first_last_delta2019,data.frame(tag, site, array_code, Location, first_det, last_det, rkm))
      }
    }
  }
}

#Save to data_output:

write_csv(df_first_last_upper2019, "data_output/UpRel_visits2019_firstlast.csv")
saveRDS(df_first_last_upper2019, "data_output/UpRel_visits2019_firstlast.rds")
write_csv(df_first_last_delta2019, "data_output/DeltaRel_visits2019_firstlast.csv")
saveRDS(df_first_last_delta2019, "data_output/DeltaRel_visits2019_firstlast.rds")


# Below I am comparing Colby and Gabe's code for first and last detection to see if they give the same output:
#GABE'S:
# Keep First and Last Detection per Visit ---------------------------------
# arrange by Hex and datetime combine hex and Detct_Location to make a unique field 

visit <- truncated_dets_up %>% 
  arrange(Hex, dtf) %>%
  mutate(combo = (paste0(Hex, site.code)))

# count dets at each receiver (then, keep all 1's and max in each sequence)
visit$counter <- sequence(rle(visit$combo)$lengths)

visit$first_or_last <- (lead(visit$counter)-visit$counter)==1

# filter to keep first and last dets ***double check with original df to see if the code worked
visits2019_firstlast_GABEs<- visit %>% 
  filter(counter==1|first_or_last==FALSE)

# Compare Colby's to Gabe's Code;
nrow(visits2019_firstlast_GABEs) # 4681 because it has a first detection and last detection on seperate lines
nrow(df_first_last_upper2019) # CODE WAS RUN FURTHER UP IN SCRIPT. 2387 bc has first and last detection on same line 

df_first_last_upper2019 <- read_csv("data_output/UpRel_visits2019_firstlast.csv")
colnames(df_first_last_upper2019)[2] <- "combo"
anti_join(df_first_last_upper2019, visits2019_firstlast_GABEs, by = "combo")
anti_join(visits2019_firstlast_GABEs, df_first_last_upper2019, by = "combo")

# filter to keep first dets
visits2019_firstlast_GABEs_first <- visit %>% 
  filter(counter==1)
nrow(visits2019_firstlast_GABEs_first) # 2525 compared to 2387, so adifference of 138, going to go with Gabes Code....

write_csv(visits2018_firstlast, "data_output/UpRel_visits2019_firstlast_GabesCode.csv")


#############################################################################################################################

##attempt at turning this into a function....not working----------------------------------------------------
#Before running function, make tag vector, site location name vector, and output dataframe (as NULL)
df_first_last_upper_test <- NULL
first_last_dets <- function(df,output_df_name, datetime_column_name, site_column_name, tag_vector, rec_vector) {
  for (tag in tag_vector) {
    print(tag)
    tag2<- subset(df, df$Hex == tag)
    if (nrow(tag2) >0) {
      for (rec in rec_vector) {
        #print(rec)
        rec2 <- subset(tag2, tag2$site_column_name == rec)
        if (nrow(rec2) >0) {
          first_det <- min(rec2$datetime_column_name)
          last_det <- max(rec2$datetime_column_name)
          tag_rkm <- rec2$rkm[1]
          #print(tag_rkm)
          output_df_name <- rbind(output_df_name, data.frame(tag, rec, first_det, last_det, tag_rkm))
        }        
      } 
    }   
  }
  #return(output_df_name)
}

first_last_dets(truncated_dets2018_upper, df_first_last_upper_test, DetectDateTime, Detect_Location, upper_tags, upper_sites)


