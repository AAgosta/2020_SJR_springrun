# Mon Aug 05 15:02:34 2019 ------------------------------
#  The purpose of this code is to verify that your "combined" files (ATScombo, LOTCombo, SUMCombo) that you created
#  (follow instructions from the doument titled "make_combined_data_for_access_import.bash" in Gabe Singers project: 
#  gabrielpsinger/2019_JSATS_filtering ) did not drop any of your accepted files. Since the result is one file
#  per technology with all files combined in it, you will not know if there was a problem with one of the files names which would 
#  result in gitbsh dropping the file, and therefore excluding it from the access database.

# Instructions:
# After you get your combined files, load them into the workspace:
library(tidyverse)
### Technologic:
tekno_merged2020 <- read_csv("data/SUMcombo.csv") #should be 53 receiver IDs
dim(tekno_merged2020) # look at how many rows of data  
###ATS: 
ATS_merged2020 <- read_csv("data/ATScombo.csv")#should be 3 IDs
dim(ATS_merged2020) # look at how many rows of data  
  ATS_merged_GG_Chipps <- read_csv("2019FILTERED/ATS/accepted/combined_files/ATScombo_NOAA_GG_Chipps.csv")
dim(ATS_merged_GG_Chipps)

###Lotek:
dim(lotek_merged2020) # look at how many rows of data  

# Next, determine how many receivers were filtered by counting the number of files in the accepted folder from which you combined the
# files from. NOTE: any file that was accepted but has no data (ie. had good tags in the cleaned file but no data that passed the filter threshold)
# will not be included in the combined file, so make sure to exclude these receiever IDs from your count. 

# Now determine the number of unique reciever IDs in your combined files. Compare this to the number of receiver files that were
# accepted. Note if you are missing any, and figure out why

### Technologic:
length(unique(tekno_merged2020$RecSN)) # determine how many different receiver IDs are in the file
#tekno_ids_merged <- (unique(tekno_merged2020$RecSN)) # save that list of ids as a vector
#print(tekno_ids_merged) # prints all those IDs in the vector
# if you have any discrepencies in the number of receiver in the combo file vs the accepted folder, go back through the file names
# and make sure each are formatted so the code will recognize them as a tekno vs lotek vs ATS. Re-read the new file in as a different object
# here is an example of when I had to do this. The first time I combined the tekno file I was missing 4 receivers from it, all those receivers
# file names were not formated correctly and where therefore not included in the combined file. I fixed the names, re-ran the git bash code to
# combine the files, and then compared the number of receiver names from the new combined file to the old one:
tekno_merged2019_fixed <- read_csv("PreviouslyFiltered/071219/accepted/SUMcombo.csv")
missing_recs <- rec_ids_merged_fixed %in% rec_ids_merged # to see which receivers are in the fixed file and not in the original
x <- which(missing_recs == FALSE)
rec_ids_merged_fixed[x] # to see exactly which files were missing from the original combined file. I compared this to the files in my 
# accepted folder and it all added up 

# following these same steps above but for ATS and Lotek:
### ATS:
length(unique(ATS_merged2020$RecSN)) # determine how many different receiver IDs are in the file
ATS_ids_merged <- (unique(ATS_merged2019$RecSN)) # save that list of ids as a vector
print(ATS_ids_merged)
#[1] 18031    THERE IS ONLY ONE BECASUE THERE WAS NO DATA IN THE OTHER 2 FILES 
#Looking at Chipps and GG merged file:
### ATS:
length(unique(ATS_merged_GG_Chipps$RecSN)) # determine how many different receiver IDs are in the file
#22: this is correct- there were only 22 recievers with data on them 
ATS_ids_merged <- (unique(ATS_merged_GG_Chipps$RecSN)) # save that list of ids as a vector
print(ATS_ids_merged)

### Lotek:
length(unique(lotek_merged2020$RecSN)) # determine how many different receiver IDs are in the file
lotek_ids_merged <- (unique(lotek_merged2019$RecSN)) # save that list of ids as a vector
print(lotek_ids_merged)