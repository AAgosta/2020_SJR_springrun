# there are two functions in this script. 
# 1. fishcheck
# 2. fishcheck_loop

# Fishcheck was written by GS, it plots each indiviudal tag and write a csv with the data for that tag:

fishcheck <- function(ID, Detections, file_location){
  
  # subset detection data by the desired ID
  Dets_Subset <- Detections[Detections$Hex == ID, ] %>%
    arrange(dtf) %>%
    mutate(pred = lag(rkm) - rkm, group = 0, group = ifelse(is.na(pred) | pred <= -16, group + 1, group + 0),  totals = cumsum(group))
  
  # create name for output df of only that fish's detection record
  df_name <<- paste0("t", ID)
  
  # assign the the newly created name to the newly created dataframe 
  assign(df_name, Dets_Subset, envir = parent.frame())
  
  # Make rkm plot, where the color of the plot changes to red if the fish moves 16rkm upstream
  ggplot(Dets_Subset, aes(x = dtf, y = rkm)) + geom_point(aes(color = lead(totals) > 1)) + 
    geom_line(aes(color = lead(totals) > 1)) +  scale_color_manual(values = setNames(c('red','black'),c(T, F))) + 
    labs(x = "", title = paste0("TagID: ", Dets_Subset$Hex[1]), y = "River km") +
    theme_bw() + theme(legend.position = "none") + ggsave(paste0(file_location, Dets_Subset$Hex[1], ".pdf"))
  
}

#test function
fishcheck("BD14", dets, "../fig_output/ind_plots/")

# for 2019:
fishcheck("BAB8", dets_16pf_up, "figure_output/")
unique($`GPS Names`)

length(tBA72$`GPS Names`[grepl(pattern = "MAC", x = tBA72$`GPS Names`)])
unique(dets_up$Hex)


fishcheck("BADB", dat, "../fig_output/")

#####################################################################################################################################################

# Fishcheck Loop: use this loop when you want to loop through a bunch of tag ids, write the plots and csvs. The fishcheck function above is better 
# for just wanting to look at individual tags
# Same function as above but adapted by CH
# MUST RUN THE FUCNTION "fishcheck_loop" AND THEN THE LOOP BELOW IT 

fishcheck_loop <- function(ID, Detections, plot_file_location, csv_file_location){
  
  # subset detection data by the desired ID
  Dets_Subset <- Detections[Detections$Hex == ID, ] %>%
    arrange(dtf) %>%
    mutate(pred = lag(rkm) - rkm, group = 0, group = ifelse(is.na(pred) | pred <= -16, group + 1, group + 0),  totals = cumsum(group))
  write_csv(Dets_Subset, path = paste0(csv_file_location,i,".csv"))
  
  
  # create name for output df of only that fish's detection record
  df_name <<- paste0("t", ID)
  
  # assign the the newly created name to the newly created dataframe 
  assign(df_name, Dets_Subset, envir = parent.frame())
  
  # Make rkm plot, where the color of the plot changes to red if the fish moves 16rkm upstream
  ggplot(Dets_Subset, aes(x = dtf, y = rkm)) + geom_point(aes(color = lead(totals) > 1)) + 
    geom_line(aes(color = lead(totals) > 1)) +  scale_color_manual(values = setNames(c('red','black'),c(T, F))) + 
    labs(x = "", title = paste0("TagID: ", Dets_Subset$Hex[1]), y = "River km") +
    theme_bw() + theme(legend.position = "none") + ggsave(paste0(plot_file_location, Dets_Subset$Hex[1], ".pdf"))
  
}

for (i in tag_list) {
  print(i)
  fishcheck_loop() # FILL THIS IN WITH YOUR DATA, SEE EXAMPLE BELOW
}

# EXAMPLE:----
dets_ds <- read_csv("data_output/DetectionFiles/Files_w_Bridge_Data/FULLmodel_final/Full_Model_Edited/dets_ds_PF16_ModelEdited_relLoc_GG_Ben_chipps_120319.csv")
tag_list <- c("BA75", "BA8E" ,"BA9D", "BAA3", "BAC5" ,"BB75", "BB7A", "BB8A", "BCA5" ,"BCB5" ,"BDA4", "BE96", "C494", "C896", "C95B", "C972", "C9A6", "CA16", "CA57", "CA8C","CA9D")

for (i in tag_list) {
  print(i)
  fishcheck_loop(ID = i, Detections = dets_ds, plot_file_location =  "figure_output/TagPlots_GabesCode/CVPU_tags/DurhamRel/CVP_a/", 
                 csv_file_location =  "data_output/Edited_TagDetHistories_forModel/Fies_w_GG_Chipps_data/Tags_w_Dets_to_be_Removed/B1toJP_included_back_in_for_FINAL_Fullmodel/CVPU_tags/DurhamRel/")
}
