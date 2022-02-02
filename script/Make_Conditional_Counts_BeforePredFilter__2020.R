# This is Gabe Singer's Code with Colby's annotation. The first part of this code makes a dataframe that tallys up the number of 
# fish that were detected at each receiver and each subsequent downstream receiver ( in the same way that the model 
# definitions are written for USER)
# The second part of the code does the same thing, but manually for each of the dual arrays (which need to be specified)

# Gabes Original Code is located : C:\Users\chause\Documents\GitHub\SJR_MS\script\Make_Conditional_Counts_w_Aux.R


library(tidyverse)

# Make Detection Counts ---------------------------------------------------

# Load UPPER RELEASE dataframe with just first visits ---------------------------------------------------------------
  data <- read_csv("data_output/DetectionFiles/Files_w_Bridge_Data/FULLmodel_final/Full_Model_Edited/DeltaRel_visits2019_first_All_NOAAdata_011320FINAL_Hwy4sPooled.csv") 

unique(data$`GPS Names`)
# Make a vector of locations that are included in model 

locs <- c('REL', 'A8', 'A9', 'A10', 'B1', 'B2', 'D1', 'E1','E2',
          'A12', 'A13', 'A14', 'A15','F1', 'A16', 'A17', 'A18', 'A19') # A7 is missing because it was originally the DFRelRec which I
#ended up takingout of the model due to poor detection probability, # A11 removed for comparability with null model 
length(locs) #20, #19 with no C1, # 18 with no A11

ids<- unique(data$Hex)
length(ids) #354
# Build a dataframe where rows are the tag Ids and columns are the site array locations. For each tag, a 1 will be placed in the 
# column if it was detected there, a 0 will be placed in the column if there was not detection there
#First, make an empty dataframe where row length is the # of tags and column length is the # of site array locations 
# (not including dual arrays)
enc.hist <- as.data.frame(matrix(rep(NA,(nrow = length(ids)*length(locs))),
                                 nrow = length(ids), ncol = length(locs))) # should have 347 rows ans 28 columns 

# Add column and row names
colnames(enc.hist) <- locs
rownames(enc.hist) <- ids

# Fill in data as explained above (For each tag, a 1 will be placed in the 
# column if it was detected there, a 0 will be placed in the column if there was no detection there)

for(i in 1:length(locs)) {
  subs <- subset(data, data$array.code == locs[i]) # this pulls out all the detections for the i-th place in locs vector that matches the array.code(ie. first REL,then A1, then A2, etc...)
  substag <- unique(subs$Hex) #make a vector of the unique tag IDs in subs df ( the subs df is all the tag that were detected at the array code ID that matches the i-th place)
  enc.hist[ ,i] <- ids %in% substag # Places TRUE in all the rows of the i-th column if a tag in substag matches the row names (ids)
}

#### Convert True to 1 and False to 0

enc.hist[enc.hist == "FALSE"] <- 0
enc.hist[enc.hist == "TRUE" ] <- 1


####Turn tag ID from a rowname to a column 
enc.hist<- rownames_to_column(enc.hist, var = "TagID_Hex") # turn the row names into an official column names "Tag_HexID"

# ><)))))*>  ------------- ><)))))*>  ------------- ><)))))*>  ------------- ><)))))*>  ------------- ><)))))*>  

#### Make input file for branch Model in USER


FirstRec <- c() # make empty vector for values to go into
counter <- 1
for(i in 1:ncol(enc.hist)){ # interate through each column in enc.history
  reclist <- rep(names(enc.hist)[i + 1], length(enc.hist)-counter) # rep means to repeat. First argument is what to repeat:names(enc.hist)[i + 1]. this repeats the names (which are the column names) of enc.hist at the i-th row +1 (bc the first row, which would be the first in the iteration is "TagID_Hex"). Second arg is how many times to repeat it:length(enc.hist)-counter, which is the # of columns (or sites) minus whichever number the counter is on. This creates the list of each receiever, with ech receiver repeated the correct number of times for the model input (so Rel is repeated 28 time, A1 27, A2, 26, etc...) 
  FirstRec <- c(FirstRec, reclist) # assigns listof first receivers to Firstrec
  counter <- counter + 1 # moves counter up so it goes to the number, which decreases the number of times the next site is repeated
}
FirstRec

SecondRec <- c() # make empty vector for values to go into
recnames <- names(enc.hist)[3:length(enc.hist)]# assign site name vector starting from A1 (skipping TagID_Hex and Rel)
recnames <- c(recnames, "x") # add x to end of rec names vector
for(i in 1:length(recnames)){ # now iterating through the new list of rec names, which starts with A1 and ends at x
  reclist <- recnames[i:length(recnames)] # new rec list starting from i to end of recnames ( so list gets shorter by 1 with each iteration)
  SecondRec <- c(SecondRec, reclist) # assign second rec list column to empty vector
}
SecondRec

## cbind results of above loops
receivers <- cbind(FirstRec, SecondRec)  ###It works!!!!!!

## Now fill last column based on the enc.hist df
enc.hist$x <- rep(1, nrow(enc.hist)) # add a row to tally fish detected at a receiver and then never again 
#write_csv(enc.hist, "data_output/enc.hist_delta_011020.csv") #just wanted to see which tags made it to JP

totals <- c() # make empyty totals vector
for(i in 1: nrow(receivers)){ # interating through each row in the receivers df (df that just has first rec and second rec)
  first <- receivers[i, 1] # assigns first as the value in the first column in the i-th row of the receiver df
  second <- receivers[i, 2] # assigns second to be the value in the second column in the i-th row of the receiver df
  first <- which(colnames(enc.hist) == first) # returns the position of where "first" value in reciever df matches in enc.history df. So for first iteration, first would be "REL", which would return a value of 2 bc its the 2nd column in enc.hist df
  second <- which(colnames(enc.hist)== second)# again, returns the position of where "second" value in reciever df matches in enc.history df. So for first iteration, first would be "A1", which would return a value of 3 bc its the 2nd column in enc.hist df
  twos <- ifelse(enc.hist[, first]==1 & enc.hist[, second]==1 & rowSums(enc.hist[, first:second])==2, T, F)# function is saying if the row of the site column assigned in "first" (wich will change as loop iterates) =  1 and the row of site column assigned to "second" value is 1 and those rows add up to 2, then twos object is T, and it calculates this for each row. if not it is F (rules of the ifelse function)
  sums <-  length(which(twos))# sums the numbers of TRUEs, could also be expressed as sums<- length(twos[twos == "TRUE"])
  ifelse(sums>= 1, totals <- c(totals, sums), totals <- c(totals, 0)) # if the sum is greater than or equal to 1, totals column gets filled with that sum, if not true then it is given the value of 0
}
totals
input <- data.frame(cbind(receivers, totals)) # Conditional likelihoods,  auxiliary ones for dual arrays are below

write.csv(input, 'data_output/User_Model_Input/2019_FullModel/HWY4sPooled/A11_Removed/2019_DeltaRelease_counts_FullModel_AllNOAAdata_012820Final_Hwy4sPooled_NoA11.csv') # counts with A11 removed 
write.csv(input, 'data_output/User_Model_Input/2019_FullModel/HWY4sPooled/A11_Removed/2019_DeltaRelease_counts_TEST.csv') # counts with A11 removed 

# ><)))))*>  ------------- ><)))))*>  ------------- ><)))))*>  ------------- ><)))))*>  ------------- ><)))))*>  

# Make auxiliary likelihoods for dual arrays ---------------------------------
# I should really turn this into a function

#  Make auxiliary likelihoods for dual arrays (OR HOR) ------------------------

ORHOR_dual <- data %>% # pull out only the detection data that is at the dual array
  filter(site.code %in% c("B1a", "B1b")) %>% 
  group_by(Hex) %>% 
  count(site.code)

ORHOR_ids <- unique(ORHOR_dual$Hex) # get all the tag ID from the subsetted group
ORHOR_locs <- c("B1a", "B1b")

ORHOR_enc.hist <-  as.data.frame(matrix(rep(NA,(length(ORHOR_ids)*length(ORHOR_locs))), # make enc hist matrix like above
                                        length(ORHOR_ids), length(ORHOR_locs))) 
# name columns and rows
colnames(ORHOR_enc.hist) <-  ORHOR_locs
rownames(ORHOR_enc.hist) <-  ORHOR_ids

## fill in data frame
for (i in 1:length(ORHOR_locs)) {
  subs <- ORHOR_dual[ORHOR_dual$site.code == ORHOR_locs[i],] # first pulls all tag ids that had a detection on the a line, then when loops through again does same for the b line
  substag <- unique(subs$Hex) # make a vector of all these tags and calls it substag
  ORHOR_enc.hist[,i] <- ORHOR_ids %in% substag # fills in the enc.hist df with a TRUE in the tag row for whatever line it was detected on, and FALSE if not detected on that line 
}

## convert TRUE to '1' and FALSE to '0'
ORHOR_enc.hist[ORHOR_enc.hist==TRUE] <- 1
ORHOR_enc.hist[ORHOR_enc.hist==FALSE] <- 0

view(ORHOR_enc.hist)

ORHOR_enc.hist$B1ab <- ifelse(ORHOR_enc.hist$B1a==1 & ORHOR_enc.hist$B1b == 1, 1, 0) # says if fish was detected (==1) at B1a and if fish was detected at B1b (==1) then B1ab  = 1, if those arguments dont hold true not it equals 0
ORHOR_enc.hist$B1a0 <- ifelse(ORHOR_enc.hist$B1a==1 & ORHOR_enc.hist$B1b == 0, 1, 0) # says if fish was detected (==1) at B1a and if fish was not detected at B1b (==0) then B1a0 = 1, if those arguments dont hold true then it equals 0
ORHOR_enc.hist$B10b <- ifelse(ORHOR_enc.hist$B1a==0 & ORHOR_enc.hist$B1b == 1, 1, 0) # says if fish was not detected (==0) at B1a and if fish was detected at B1b (==1) then B10b = 1, if those arguments dont hold true then it equals 0

view(ORHOR_enc.hist)

ORHOR_dual <- ORHOR_enc.hist %>% 
  select(B1ab, B1a0, B10b) %>% 
  summarise_all(funs(sum)) # the funs function says to apply the sum function to all of the columns (i think..)

#  Make auxiliary likelihoods for dual arrays (HWY4s) just to get detection prob ------------------------

HWY4s_dual <- data %>% # pull out only the detection data that is at the dual array
  filter(site.code %in% c("B2a", "B2b")) %>% 
  group_by(Hex) %>% 
  count(site.code)

HWY4s_ids <- unique(HWY4s_dual$Hex) # get all the tag ID from the subsetted group
HWY4s_locs <- c("B2a", "B2b")

HWY4s_enc.hist <-  as.data.frame(matrix(rep(NA,(length(HWY4s_ids)*length(HWY4s_locs))), # make enc hist matrix like above
                                         length(HWY4s_ids), length(HWY4s_locs))) 
# name columns and rows
colnames(HWY4s_enc.hist) <-  HWY4s_locs
rownames(HWY4s_enc.hist) <-  HWY4s_ids

## fill in data frame
for (i in 1:length(HWY4s_locs)) {
  subs <- HWY4s_dual[HWY4s_dual$site.code == HWY4s_locs[i],] # first pulls all tag ids that had a detection on the a line, then when loops through again does same for the b line
  substag <- unique(subs$Hex) # make a vector of all these tags and calls it substag
  HWY4s_enc.hist[,i] <- HWY4s_ids %in% substag # fills in the enc.hist df with a TRUE in the tag row for whatever line it was detected on, and FALSE if not detected on that line 
}

## convert TRUE to '1' and FALSE to '0'
HWY4s_enc.hist[HWY4s_enc.hist==TRUE] <- 1
HWY4s_enc.hist[HWY4s_enc.hist==FALSE] <- 0

view(HWY4s_enc.hist)

HWY4s_enc.hist$B2ab <- ifelse(HWY4s_enc.hist$B2a==1 & HWY4s_enc.hist$B2b == 1, 1, 0) # says if fish was detected (==1) at B2a and if fish was detected at B2b (==1) then B2ab  = 1, if those arguments dont hold true not it equals 0
HWY4s_enc.hist$B2a0 <- ifelse(HWY4s_enc.hist$B2a==1 & HWY4s_enc.hist$B2b == 0, 1, 0) # says if fish was detected (==1) at B2a and if fish was not detected at B2b (==0) then B2a0 = 1, if those arguments dont hold true then it equals 0
HWY4s_enc.hist$B10b <- ifelse(HWY4s_enc.hist$B2a==0 & HWY4s_enc.hist$B2b == 1, 1, 0) # says if fish was not detected (==0) at B2a and if fish was detected at B2b (==1) then B10b = 1, if those arguments dont hold true then it equals 0

view(HWY4s_enc.hist)

HWY4s_dual <- HWY4s_enc.hist %>% 
  select(B2ab, B2a0, B10b) %>% 
  summarise_all(funs(sum)) # the funs function says to apply the sum function to all of the columns (i think..)

HWY4s_dual



#  Make auxiliary likelihoods for dual arrays (MR HWy 4) DO NOT NEED THIS ONE ------------------------

#MRHWY4_dual <- data %>% # pull out only the detection data that is at the dual array
  filter(site.code %in% c("C1a", "C1b")) %>% 
  group_by(Hex) %>% 
  count(site.code)

#MRHWY4_ids <- unique(MRHWY4_dual$Hex) # get all the tag ID from the sunsetted group
#MRHWY4_locs <- c("C1a", "C1b")

#MRHWY4_enc.hist <-  as.data.frame(matrix(rep(NA,(length(MRHWY4_ids)*length(MRHWY4_locs))), # make enc hist matrix like above
                                         length(MRHWY4_ids), length(MRHWY4_locs))) 
# name columns and rows
#colnames(MRHWY4_enc.hist) <-  MRHWY4_locs
#rownames(MRHWY4_enc.hist) <-  MRHWY4_ids

## fill in data frame
#for (i in 1:length(MRHWY4_locs)) {
  subs <- MRHWY4_dual[MRHWY4_dual$site.code == MRHWY4_locs[i],]
  substag <- unique(subs$Hex)
  MRHWY4_enc.hist[,i] <- MRHWY4_ids %in% substag
}

## convert TRUE to '1' and FALSE to '0'
#MRHWY4_enc.hist[MRHWY4_enc.hist==TRUE] <- 1
#MRHWY4_enc.hist[MRHWY4_enc.hist==FALSE] <- 0

#view(MRHWY4_enc.hist)

#MRHWY4_enc.hist$C1ab <- ifelse(MRHWY4_enc.hist$C1a==1 & MRHWY4_enc.hist$C1b == 1, 1, 0) # says if fish was detected (==1) at C1a and if fish was detected at C1b (==1) then C1ab  = 1, if those arguments dont hold true not it equals 0
#MRHWY4_enc.hist$C1a0 <- ifelse(MRHWY4_enc.hist$C1a==1 & MRHWY4_enc.hist$C1b == 0, 1, 0) # says if fish was detected (==1) at C1a and if fish was not detected at C1b (==0) then C1a0 = 1, if those arguments dont hold true then it equals 0
#MRHWY4_enc.hist$C10b <- ifelse(MRHWY4_enc.hist$C1a==0 & MRHWY4_enc.hist$C1b == 1, 1, 0) # says if fish was not detected (==0) at C1a and if fish was detected at C1b (==1) then C10b = 1, if those arguments dont hold true then it equals 0

#view(MRHWY4_enc.hist)

#MRHWY4_dual <- MRHWY4_enc.hist %>% 
  select(C1ab, C1a0, C10b) %>% 
  summarise_all(funs(sum)) # the funs function says to apply the sum function to all of the columns (i think..)

#  Make auxiliary likelihoods for dual arrays (CVP)DO NOT NEED THIS ONE ------------------------

#CVP_dual <- data %>% # pull out only the detection data that is at the dual array
  filter(site.code %in% c("D1a", "D1b")) %>% 
  group_by(Hex) %>% 
  count(site.code)

#CVP_ids <- unique(CVP_dual$Hex) # get all the tag ID from the sunsetted group
#CVP_locs <- c("D1a", "D1b")

#CVP_enc.hist <-  as.data.frame(matrix(rep(NA,(length(CVP_ids)*length(CVP_locs))), # make enc hist matrix like above
                                      length(CVP_ids), length(CVP_locs))) 
# name columns and rows
#colnames(CVP_enc.hist) <-  CVP_locs
#rownames(CVP_enc.hist) <-  CVP_ids

## fill in data frame
#for (i in 1:length(CVP_locs)) {
  subs <- CVP_dual[CVP_dual$site.code == CVP_locs[i],]
  substag <- unique(subs$Hex)
  CVP_enc.hist[,i] <- CVP_ids %in% substag
}

## convert TRUE to '1' and FALSE to '0'
#CVP_enc.hist[CVP_enc.hist==TRUE] <- 1
#CVP_enc.hist[CVP_enc.hist==FALSE] <- 0

#view(CVP_enc.hist)

#CVP_enc.hist$D1ab <- ifelse(CVP_enc.hist$D1a==1 & CVP_enc.hist$D1b == 1, 1, 0) # says if fish was detected (==1) at D1a and if fish was detected at D1b (==1) then D1ab  = 1, if those arguments dont hold true not it equals 0
#CVP_enc.hist$D1a0 <- ifelse(CVP_enc.hist$D1a==1 & CVP_enc.hist$D1b == 0, 1, 0) # says if fish was detected (==1) at D1a and if fish was not detected at D1b (==0) then D1a0 = 1, if those arguments dont hold true then it equals 0
#CVP_enc.hist$D10b <- ifelse(CVP_enc.hist$D1a==0 & CVP_enc.hist$D1b == 1, 1, 0) # says if fish was not detected (==0) at D1a and if fish was detected at D1b (==1) then D10b = 1, if those arguments dont hold true then it equals 0

#view(CVP_enc.hist)

#CVP_dual <- CVP_enc.hist %>% 
  select(D1ab, D1a0, D10b) %>% 
  summarise_all(funs(sum)) # the funs function says to apply the sum function to all of the columns (i think..)


#  Make auxiliary likelihoods for dual arrays (Clifton Court) ------------------------

CC_dual <- data %>% # pull out only the detection data that is at the dual array
  filter(site.code %in% c("E1a", "E1b")) %>% 
  group_by(Hex) %>% 
  count(site.code)

CC_ids <- unique(CC_dual$Hex) # get all the tag ID from the sunsetted group
CC_locs <- c("E1a", "E1b")

CC_enc.hist <-  as.data.frame(matrix(rep(NA,(length(CC_ids)*length(CC_locs))), # make enc hist matrix like above
                                     length(CC_ids), length(CC_locs))) 
# name columns and rows
colnames(CC_enc.hist) <-  CC_locs
rownames(CC_enc.hist) <-  CC_ids

## fill in data frame
for (i in 1:length(CC_locs)) {
  subs <- CC_dual[CC_dual$site.code == CC_locs[i],]
  substag <- unique(subs$Hex)
  CC_enc.hist[,i] <- CC_ids %in% substag
}

## convert TRUE to '1' and FALSE to '0'
CC_enc.hist[CC_enc.hist==TRUE] <- 1
CC_enc.hist[CC_enc.hist==FALSE] <- 0

view(CC_enc.hist)

CC_enc.hist$E1ab <- ifelse(CC_enc.hist$E1a==1 & CC_enc.hist$E1b == 1, 1, 0) # says if fish was detected (==1) at E1a and if fish was detected at E1b (==1) then E1ab  = 1, if those arguments dont hold true not it equals 0
CC_enc.hist$E1a0 <- ifelse(CC_enc.hist$E1a==1 & CC_enc.hist$E1b == 0, 1, 0) # says if fish was detected (==1) at E1a and if fish was not detected at E1b (==0) then E1a0 = 1, if those arguments dont hold true then it equals 0
CC_enc.hist$E10b <- ifelse(CC_enc.hist$E1a==0 & CC_enc.hist$E1b == 1, 1, 0) # says if fish was not detected (==0) at E1a and if fish was detected at E1b (==1) then E10b = 1, if those arguments dont hold true then it equals 0

view(CC_enc.hist)

CC_dual <- CC_enc.hist %>% 
  select(E1ab, E1a0, E10b) %>% 
  summarise_all(funs(sum)) # the funs function says to apply the sum function to all of the columns (i think..)


#  Make auxiliary likelihoods for dual arrays (MAC) ------------------------

MAC_dual <- data %>% # pull out only the detection data that is at the dual array
  filter(site.code %in% c("A15a", "A15b")) %>% 
  group_by(Hex) %>% 
  count(site.code)

MAC_ids <- unique(MAC_dual$Hex) # get all the tag ID from the sunsetted group
MAC_locs <- c("A15a", "A15b")

MAC_enc.hist <-  as.data.frame(matrix(rep(NA,(length(MAC_ids)*length(MAC_locs))), # make enc hist matrix like above
                                      length(MAC_ids), length(MAC_locs))) 
# name columns and rows
colnames(MAC_enc.hist) <-  MAC_locs
rownames(MAC_enc.hist) <-  MAC_ids

## fill in data frame
for (i in 1:length(MAC_locs)) {
  subs <- MAC_dual[MAC_dual$site.code == MAC_locs[i],]
  substag <- unique(subs$Hex)
  MAC_enc.hist[,i] <- MAC_ids %in% substag
}

## convert TRUE to '1' and FALSE to '0'
MAC_enc.hist[MAC_enc.hist==TRUE] <- 1
MAC_enc.hist[MAC_enc.hist==FALSE] <- 0

view(MAC_enc.hist)

MAC_enc.hist$A15ab <- ifelse(MAC_enc.hist$A15a==1 & MAC_enc.hist$A15b == 1, 1, 0) # says if fish was detected (==1) at A15a and if fish was detected at A15b (==1) then A15ab  = 1, if those arguments dont hold true not it equals 0
MAC_enc.hist$A15a0 <- ifelse(MAC_enc.hist$A15a==1 & MAC_enc.hist$A15b == 0, 1, 0) # says if fish was detected (==1) at A15a and if fish was not detected at A15b (==0) then A15a0 = 1, if those arguments dont hold true then it equals 0
MAC_enc.hist$A150b <- ifelse(MAC_enc.hist$A15a==0 & MAC_enc.hist$A15b == 1, 1, 0) # says if fish was not detected (==0) at A15a and if fish was detected at A15b (==1) then A150b = 1, if those arguments dont hold true then it equals 0

view(MAC_enc.hist)

MAC_dual <- MAC_enc.hist %>% 
  select(A15ab, A15a0, A150b) %>% 
  summarise_all(funs(sum)) # the funs function says to apply the sum function to all of the columns (i think..)



#  Make auxiliary likelihoods for dual arrays (JP) ------------------------

JP_dual <- data %>% # pull out only the detection data that is at the dual array
  filter(site.code %in% c("A16a", "A16b")) %>% 
  group_by(Hex) %>% 
  count(site.code)

JP_ids <- unique(JP_dual$Hex) # get all the tag ID from the sunsetted group
JP_locs <- c("A16a", "A16b")

JP_enc.hist <-  as.data.frame(matrix(rep(NA,(length(JP_ids)*length(JP_locs))), # make enc hist matrix like above
                                     length(JP_ids), length(JP_locs))) 
# name columns and rows
colnames(JP_enc.hist) <-  JP_locs
rownames(JP_enc.hist) <-  JP_ids

## fill in data frame
for (i in 1:length(JP_locs)) {
  subs <- JP_dual[JP_dual$site.code == JP_locs[i],]
  substag <- unique(subs$Hex)
  JP_enc.hist[,i] <- JP_ids %in% substag
}

## convert TRUE to '1' and FALSE to '0'
JP_enc.hist[JP_enc.hist==TRUE] <- 1
JP_enc.hist[JP_enc.hist==FALSE] <- 0

view(JP_enc.hist)

JP_enc.hist$A16ab <- ifelse(JP_enc.hist$A16a==1 & JP_enc.hist$A16b == 1, 1, 0) # says if fish was detected (==1) at A16a and if fish was detected at A16b (==1) then A16ab  = 1, if those arguments dont hold true not it equals 0
JP_enc.hist$A16a0 <- ifelse(JP_enc.hist$A16a==1 & JP_enc.hist$A16b == 0, 1, 0) # says if fish was detected (==1) at A16a and if fish was not detected at A16b (==0) then A16a0 = 1, if those arguments dont hold true then it equals 0
JP_enc.hist$A160b <- ifelse(JP_enc.hist$A16a==0 & JP_enc.hist$A16b == 1, 1, 0) # says if fish was not detected (==0) at A16a and if fish was detected at A16b (==1) then A160b = 1, if those arguments dont hold true then it equals 0

view(JP_enc.hist)

JP_dual <- JP_enc.hist %>% 
  select(A16ab, A16a0, A160b) %>% 
  summarise_all(funs(sum)) # the funs function says to apply the sum function to all of the columns (i think..)

#  Make auxiliary likelihoods for dual arrays (Chipps) ------------------------

Chipps_dual <- data %>% # pull out only the detection data that is at the dual array
  filter(site.code %in% c("A17a", "A17b")) %>% 
  group_by(Hex) %>% 
  count(site.code)

Chipps_ids <- unique(Chipps_dual$Hex) # get all the tag ID from the sunsetted group
Chipps_locs <- c("A17a", "A17b")

Chipps_enc.hist <-  as.data.frame(matrix(rep(NA,(length(Chipps_ids)*length(Chipps_locs))), # make enc hist matrix like above
                                         length(Chipps_ids), length(Chipps_locs))) 
# name columns and rows
colnames(Chipps_enc.hist) <-  Chipps_locs
rownames(Chipps_enc.hist) <-  Chipps_ids

## fill in data frame
for (i in 1:length(Chipps_locs)) {
  subs <- Chipps_dual[Chipps_dual$site.code == Chipps_locs[i],]
  substag <- unique(subs$Hex)
  Chipps_enc.hist[,i] <- Chipps_ids %in% substag
}

## convert TRUE to '1' and FALSE to '0'
Chipps_enc.hist[Chipps_enc.hist==TRUE] <- 1
Chipps_enc.hist[Chipps_enc.hist==FALSE] <- 0

view(Chipps_enc.hist)

Chipps_enc.hist$A17ab <- ifelse(Chipps_enc.hist$A17a==1 & Chipps_enc.hist$A17b == 1, 1, 0) # says if fish was detected (==1) at A17a and if fish was detected at A17b (==1) then A17ab  = 1, if those arguments dont hold true not it equals 0
Chipps_enc.hist$A17a0 <- ifelse(Chipps_enc.hist$A17a==1 & Chipps_enc.hist$A17b == 0, 1, 0) # says if fish was detected (==1) at A17a and if fish was not detected at A17b (==0) then A17a0 = 1, if those arguments dont hold true then it equals 0
Chipps_enc.hist$A170b <- ifelse(Chipps_enc.hist$A17a==0 & Chipps_enc.hist$A17b == 1, 1, 0) # says if fish was not detected (==0) at A17a and if fish was detected at A17b (==1) then A170b = 1, if those arguments dont hold true then it equals 0

view(Chipps_enc.hist)

Chipps_dual <- Chipps_enc.hist %>% 
  select(A17ab, A17a0, A170b) %>% 
  summarise_all(funs(sum)) # the funs function says to apply the sum function to all of the columns (i think..)


#  Make auxiliary likelihoods for dual arrays (Benicia) ------------------------

Ben_dual <- data %>% # pull out only the detection data that is at the dual array
  filter(site.code %in% c("A18a", "A18b")) %>% 
  group_by(Hex) %>% 
  count(site.code)

Ben_ids <- unique(Ben_dual$Hex) # get all the tag ID from the sunsetted group
Ben_locs <- c("A18a", "A18b")

Ben_enc.hist <-  as.data.frame(matrix(rep(NA,(length(Ben_ids)*length(Ben_locs))), # make enc hist matrix like above
                                      length(Ben_ids), length(Ben_locs))) 
# name columns and rows
colnames(Ben_enc.hist) <-  Ben_locs
rownames(Ben_enc.hist) <-  Ben_ids

## fill in data frame
for (i in 1:length(Ben_locs)) {
  subs <- Ben_dual[Ben_dual$site.code == Ben_locs[i],]
  substag <- unique(subs$Hex)
  Ben_enc.hist[,i] <- Ben_ids %in% substag
}

## convert TRUE to '1' and FALSE to '0'
Ben_enc.hist[Ben_enc.hist==TRUE] <- 1
Ben_enc.hist[Ben_enc.hist==FALSE] <- 0

view(Ben_enc.hist)

Ben_enc.hist$A18ab <- ifelse(Ben_enc.hist$A18a==1 & Ben_enc.hist$A18b == 1, 1, 0) # says if fish was detected (==1) at A18a and if fish was detected at A18b (==1) then A18ab  = 1, if those arguments dont hold true not it equals 0
Ben_enc.hist$A18a0 <- ifelse(Ben_enc.hist$A18a==1 & Ben_enc.hist$A18b == 0, 1, 0) # says if fish was detected (==1) at A18a and if fish was not detected at A18b (==0) then A18a0 = 1, if those arguments dont hold true then it equals 0
Ben_enc.hist$A180b <- ifelse(Ben_enc.hist$A18a==0 & Ben_enc.hist$A18b == 1, 1, 0) # says if fish was not detected (==0) at A18a and if fish was detected at A18b (==1) then A180b = 1, if those arguments dont hold true then it equals 0

view(Ben_enc.hist)

Ben_dual <- Ben_enc.hist %>% 
  select(A18ab, A18a0, A180b) %>% 
  summarise_all(funs(sum)) # the funs function says to apply the sum function to all of the columns (i think..)


#  Make auxiliary likelihoods for dual arrays (GG) ------------------------

GG_dual <- data %>% # pull out only the detection data that is at the dual array
  filter(site.code %in% c("A19a", "A19b")) %>% 
  group_by(Hex) %>% 
  count(site.code)

GG_ids <- unique(GG_dual$Hex) # get all the tag ID from the sunsetted group
GG_locs <- c("A19a", "A19b")

GG_enc.hist <-  as.data.frame(matrix(rep(NA,(length(GG_ids)*length(GG_locs))), # make enc hist matrix like above
                                     length(GG_ids), length(GG_locs))) 
# name columns and rows
colnames(GG_enc.hist) <-  GG_locs
rownames(GG_enc.hist) <-  GG_ids

## fill in data frame
for (i in 1:length(GG_locs)) {
  subs <- GG_dual[GG_dual$site.code == GG_locs[i],]
  substag <- unique(subs$Hex)
  GG_enc.hist[,i] <- GG_ids %in% substag
}

## convert TRUE to '1' and FALSE to '0'
GG_enc.hist[GG_enc.hist==TRUE] <- 1
GG_enc.hist[GG_enc.hist==FALSE] <- 0

view(GG_enc.hist)

GG_enc.hist$A19ab <- ifelse(GG_enc.hist$A19a==1 & GG_enc.hist$A19b == 1, 1, 0) # says if fish was detected (==1) at A19a and if fish was detected at A19b (==1) then A19ab  = 1, if those arguments dont hold true not it equals 0
GG_enc.hist$A19a0 <- ifelse(GG_enc.hist$A19a==1 & GG_enc.hist$A19b == 0, 1, 0) # says if fish was detected (==1) at A19a and if fish was not detected at A19b (==0) then A19a0 = 1, if those arguments dont hold true then it equals 0
GG_enc.hist$A190b <- ifelse(GG_enc.hist$A19a==0 & GG_enc.hist$A19b == 1, 1, 0) # says if fish was not detected (==0) at A19a and if fish was detected at A19b (==1) then A190b = 1, if those arguments dont hold true then it equals 0

view(GG_enc.hist)

GG_dual <- GG_enc.hist %>% 
  select(A19ab, A19a0, A190b) %>% 
  summarise_all(funs(sum)) # the funs function says to apply the sum function to all of the columns (i think..)

## Bind and write to csv --------------------------------------------------------------------------------------------------

Aux <- rbind(t(ORHOR_dual),t(HWY4s_dual), t(CC_dual),t(MAC_dual), 
             t(JP_dual), t(Chipps_dual), t(GG_dual), t(Ben_dual))
# t() function transposes the data, ie. makes the columns the rows 

#write.csv(Aux, "data_output/User_Model_Input/2019_FullModel/2019_DeltaRel_Cond_Aux_counts_FullModel_AllNOAAdata_FINAL_120319.csv")
#write.csv(Aux, "data_output/User_Model_Input/2019_FullModel/2019_DeltaRel_Cond_Aux_counts_FullModel_AllNOAAdata_FINAL_010320.csv")
#write.csv(Aux, "data_output/User_Model_Input/2019_FullModel/2019_DeltaRel_Cond_Aux_counts_FullModel_AllNOAAdata_010320FINAL.csv")
#write.csv(Aux, "data_output/User_Model_Input/2019_FullModel/2019_DeltaRel_Cond_Aux_counts_FullModel_AllNOAAdata_010320FINAL_2.csv") # use this file if you wnat or and mr hwy4 as seperate routes and cvp as a dual array
#write.csv(Aux, "data_output/User_Model_Input/2019_FullModel/HWY4sPooled/2019_DeltaRel_Cond_Aux_counts_FullModel_AllNOAAdata_011020FINAL_Hwy4sPooled.csv") # or and mr hwy4 pooled, cvp as single array
#write.csv(Aux, "data_output/User_Model_Input/2019_FullModel/HWY4sPooled/2019_DeltaRel_Cond_Aux_counts_FullModel_AllNOAAdata_011320FINAL_Hwy4sPooled.csv") # file with BAE5 det histories removed 
write.csv(Aux, "data_output/User_Model_Input/2019_FullModel/HWY4sPooled/A11_Removed/2019_DeltaRel_Cond_Aux_counts_FullModel_AllNOAAdata_012820FINAL_Hwy4sPooled_NoA11.csv") # should be the same as the file above, A11 removed but it wouldnt effect this 
#write.csv(Aux, "data_output/User_Model_Input/2019_FullModel/HWY4sPooled/A11_Removed/2019_DeltaRel_Cond_Aux_counts_TEST.csv") # should be the same as the file above, A11 removed but it wouldnt effect this 

