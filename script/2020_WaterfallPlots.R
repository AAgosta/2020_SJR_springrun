# This is the code to make waterfall plots of 2019 data. 
#run code "2020_formatting_detection.csvs" before running this code because it creates the "Dets_raw_....csv" files that you need
#### FIRST: run the function "WaterFall_Plot_Function.Rmd" in the scripts folder of the 

library(lubridate)
library(tidyverse)
library(ggplot2)

# Read in detection files from each release group
dets_up<- read_csv("data_output/2020Dets_raw_UpperRelease.csv")
dets_ds<- read_csv("data_output/2020Dets_raw_LowerRelease.csv")
dets_ft <- read_csv("data_output/2020Dets_raw_FTRelease.csv")



# Run the function "print_tags" in the same Rmd as the function above to look at the waterfall plot for each 
# individual tag:

print_tags(dets_up, "2020UpperDets_Indiv_WFPlots")
print_tags(dets_ds, "2020LowerDets_Indiv_WFPlots")
print_tags(dets_ft, "2020FTDets_Indiv_WFPlots")

###where is the "plot all rkm time jsats" function???
#plot_all_rkm_time_jsats(df =dets_16pf_up,df_tag_list = dets_16pf_up, tag_colname = "Hex", str_start_datetime = "2020-01-01 00:00:00",
#                        str_end_datetime = "2020-09-01 00:0:00", high_rkm = 350, #low_rkm = 0, pdf_name = "UpperRel_WFplot_PredFilter", 
#                        gg_title = "Upper Release,w/ predator Filter", file_location = "figure_output//")

#plot_all_rkm_time_jsats(df =dets_16pf_ds,df_tag_list =  dets_16pf_ds,tag_colname = "Hex", str_start_datetime = "2020-01-01 00:00:00",
#                        str_end_datetime = "2020-09-01 00:0:00", high_rkm = 350, low_rkm = 0, pdf_name = "LowerRel_WFplot_PredFilter", 
#                        gg_title = "Lower Release,w/ predator Filter", file_location = "figure_output//")

#plot_all_rkm_time_jsats(df =dets_16pf_ds,df_tag_list =  dets_16pf_ds,tag_colname = "Hex", str_start_datetime = "2020-01-01 00:00:00",
#                        str_end_datetime = "2020-09-01 00:0:00", high_rkm = 350, low_rkm = 0, pdf_name = "FTRel_WFplot_PredFilter", 
#                        gg_title = "FT Release,q/ predator Filter", file_location = "figure_output//")

# after adjusting for model use this code ---------------------------------

# Looking at the individual plots again after making the edits for the model (from code in the script folder called 
# "Removing_dets_to_fit_model.R")
dets_up_forModel <- read_csv("data_output/DetectionFiles/dets_up_forModel.csv")
dets_ds_forModel <- read_csv("data_output/DetectionFiles/dets_ds_forModel.csv")

print_tags(dets_up_forModel, "2019Upper_IndvPLots_PF16_editedForModel")
print_tags(dets_ds_forModel, "2019Delta_IndvPLots_PF16_editedForModel")

################################################################################################################################
# Running Waterfall Plot Code on data files that now include Chipps and GG:
# # Tue Sep 17 10:59:02 2019 ------------------------------
# Read in detection files from each release group

#the 16 pf means I am using the files that have already gone through the 16 km predator filter
dets_16pf_up<- read_csv("data_output/DetectionFiles/Files_w_Bridge_Data/UpperDets_w_GG_Chipps_pred_filt16.csv")
dets_16pf_ds<- read_csv("data_output/DetectionFiles/Files_w_Bridge_Data/DeltaDets_w_GG_Chipps_pred_filt16.csv")

# run function "plot_all_rkm_time_jsats" which makes the waterfall plots of all the detections.
plot_all_rkm_time_jsats(df =dets_16pf_up,df_tag_list = dets_16pf_up, tag_colname = "Hex", str_start_datetime = "2020-01-01 00:00:00",
                        str_end_datetime = "2020-09-01 00:0:00", high_rkm = 350, low_rkm = 0, pdf_name = "UpperRel_WFplot_PredFilter+w_gg_Chipps", 
                        gg_title = "Upper Release,w/ predator Filter", file_location = "figure_output//")

plot_all_rkm_time_jsats(df =dets_16pf_ds,df_tag_list =  dets_16pf_ds,tag_colname = "Hex", str_start_datetime = "2019-01-01 00:00:00",
                        str_end_datetime = "2019-09-01 00:0:00", high_rkm = 350, low_rkm = 0, pdf_name = "DeltaRel_WFplot_PredFilter_w_GG_Chipps", 
                        gg_title = "Delta Release,q/ predator Filter", file_location = "figure_output//")

# Run the function "print_tags" in the same Rmd as the function above to look at the waterfall plot for each individual tag:
print_tags(dets_16pf_up, "2019UpperDets_Indiv_pf16_WFPlots")
print_tags(dets_16pf_ds, "2019DeltaDets_Indiv_pf16_WFPlots")

# Looking at the individual plots again after making the edits for the model ( from code in the script folder called 
# "Removing_dets_to_fit_model.R")
dets_up_forModel <- read_csv("data_output/DetectionFiles/Files_w_Bridge_Data/dets_up_formodel_w_GG_Chips.csv")
dets_ds_forModel <- read_csv("data_output/DetectionFiles/Files_w_Bridge_Data/dets_ds_formodel_w_GG_Chips.csv")

print_tags(dets_up_forModel, "2019Upper_IndvPLots_PF16_editedForModel")
print_tags(dets_ds_forModel, "2019Delta_IndvPLots_PF16_editedForModel")

##################################### Adding vertical line for Flame Transect Dates###########################################

###### Watefall plots not plotting because too many detections, use the files with first and last detections only:
firstlast_det_up <- read_csv("data_output/UpRel_visits2019_firstlastFLAME_MODEL_w_Chipps_092319.csv")
firstlast_det_ds <- read_csv("data_output/DeltaRel_visits2019_firstlastFLAME_MODEL_w_Chipps_092419.csv")

first_det_up<- read_csv("data_output/Old_Files/UpRel_visits2019_firstFLAME_MODEL_w_Chipps.csv")
first_det_ds <- read_csv("data_output/DeltaRel_visits2019_firstFLAME_MODEL_w_Chipps_092419.csv")

# determine which records have a date that we want:
grep(pattern = "2019-03-28", x = first_det_up$dtf) #41
grep(pattern = "2019-04-19", x = first_det_up$dtf) # none

grep(pattern = "2019-03-28", x = first_det_ds$dtf) #164
grep(pattern = "2019-04-19", x = first_det_ds$dtf) #128

plot_all_rkm_time_jsats_up(df =first_det_up,df_tag_list = first_det_up, tag_colname = "Hex", str_start_datetime = "2019-01-01 00:00:00",
                        str_end_datetime = "2019-09-01 00:0:00", high_rkm = 350, low_rkm = 0, pdf_name = "UpperRel_WFplot_PredFilter_w_Flame_line", 
                        gg_title = "Upper Release,w/ predator Filter", file_location = "figure_output//")

plot_all_rkm_time_jsats_ds(df =first_det_ds,df_tag_list =  first_det_ds,tag_colname = "Hex", str_start_datetime = "2019-01-01 00:00:00",
                        str_end_datetime = "2019-09-01 00:0:00", high_rkm = 350, low_rkm = 0, pdf_name = "DeltaRel_WFplot_PredFilter_w_Flame_line", 
                        gg_title = "Delta Release,q/ predator Filter", file_location = "figure_output//")

plot_all_rkm_time_jsats_up <- function(df, df_tag_list,tag_colname, str_start_datetime, str_end_datetime, high_rkm, low_rkm, pdf_name, gg_title, file_location, initial_rkm = 0) {
  pdf(paste(file_location, pdf_name, ".pdf", sep = "" )) 
  df_edit <- subset(df, rkm > initial_rkm)   #use this line for reguar rkm
  #df_edit <- subset(df, Genrkm > initial_rkm)
  df_edit <- subset(df_edit, dtf > str_start_datetime & dtf < str_end_datetime)
  all_tags <- unique(df_tag_list[[tag_colname]])
  df_edit_time <- df_edit[order(df_edit$dtf), ]
  first_time <- df_edit_time$dtf[1]
  last_time <- df_edit_time$dtf[nrow(df_edit_time)]
  first_det <- df_edit$dtf[1]
  last_det <- df_edit$dtf[nrow(df_edit)]
  
  fish_move <- ggplot(df_edit, aes(x=dtf, y=rkm, color = Hex)) +  #use this line for regular rkm
  #fish_move <- ggplot(df_edit, aes(x=dtf, y=Genrkm, color = tag_colname)) +
    geom_vline(xintercept =as.numeric(first_det_ds$dtf[c(164, 128)])) +
    geom_point(size = .25) + geom_line(size = .25) +
    ggtitle(paste(gg_title)) +
    xlab("Time After Release") +
    #ylab("River Kilometer") +
    theme(plot.title = element_text(size = 10), legend.position = "none") +
    scale_x_datetime(date_breaks = "5 days") +
    scale_y_discrete(name = "River Kilometer", limits = seq(low_rkm, high_rkm, by=10)) +
    coord_cartesian(ylim = c(low_rkm, high_rkm)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  plot(fish_move)
  dev.off()
}


plot_all_rkm_time_jsats_ds <- function(df, df_tag_list,tag_colname, str_start_datetime, str_end_datetime, high_rkm, low_rkm, pdf_name, gg_title, file_location, initial_rkm = 0) {
  pdf(paste(file_location, pdf_name, ".pdf", sep = "" )) 
  df_edit <- subset(df, rkm > initial_rkm)   #use this line for reguar rkm
  #df_edit <- subset(df, Genrkm > initial_rkm)
  df_edit <- subset(df_edit, dtf > str_start_datetime & dtf < str_end_datetime)
  all_tags <- unique(df_tag_list[[tag_colname]])
  df_edit_time <- df_edit[order(df_edit$dtf), ]
  first_time <- df_edit_time$dtf[1]
  last_time <- df_edit_time$dtf[nrow(df_edit_time)]
  first_det <- df_edit$dtf[1]
  last_det <- df_edit$dtf[nrow(df_edit)]
  
  fish_move <- ggplot(df_edit, aes(x=dtf, y=rkm, color = Hex)) +  #use this line for regular rkm
  #fish_move <- ggplot(df_edit, aes(x=dtf, y=Genrkm, color = tag_colname)) +
    geom_vline(xintercept =as.numeric(df_edit$dtf[c(164, 128)])) +
    geom_point(size = .25) + geom_line(size = .25) +
    ggtitle(paste(gg_title)) +
    xlab("Time After Release") +
    #ylab("River Kilometer") +
    theme(plot.title = element_text(size = 10), legend.position = "none") +
    scale_x_datetime(date_breaks = "5 days") +
    scale_y_discrete(name = "River Kilometer", limits = seq(low_rkm, high_rkm, by=10)) +
    coord_cartesian(ylim = c(low_rkm, high_rkm)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  plot(fish_move)
  dev.off()
}


