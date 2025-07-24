# Script to wrangle data from camera trap project 

set.seed(42)
library(data.table) # data.frames better
library(stringr) #dealing with strings
library(stringi) #dealing with strings but faster
library(progress) #for progress bar for lengthy procedures
library(lubridate) #for working with timestamps

#clear workspace
rm(list = ls())

#set path
data_path = file.path("D:/NHMA")

wd = file.path("C:/Users/au784040/Documents_C/WildCam_project")

#read in file containing all paths to images in directory
files = fread(file = file.path(data_path, "file_list.txt"), header = FALSE, col.names = "filepath", sep = "\n")

#read in annotations
annot = fread(file = file.path(wd, "AnnotationsExport.csv"), encoding = "Latin-1")

annot[, `date: start` := as.IDate(`date: start`, format = "%m/%d/%Y %H:%M:%S")]
annot[, `date: stop` := as.IDate(`date: stop`, format = "%m/%d/%Y %H:%M:%S")]
annot[, `time: start` := as.ITime(`time: start`, format = "%m/%d/%Y %H:%M:%S")]
annot[, `time: stop` := as.ITime(`Time: stop`, format = "%m/%d/%Y %H:%M:%S")]
annot[, `time_obs` := as.ITime(`time`, format = "%m/%d/%Y %H:%M:%S")]
annot[, `date_obs` := as.IDate(`date`, format = "%m/%d/%Y %H:%M:%S")]
annot[, c("Time: stop", "date", "time") := NULL]
annot[, year := as.integer(format(date_obs, "%Y"))]

annot[, filename := paste0(`serie name`,"_file_", format(date_obs, "%Y%m%d"),"_" , 
                           sprintf("%02d%02d00", hour(time_obs), minute(time_obs)))]


locations = annot[, unique(`Study area name`)]
series = annot[, unique(`serie name`)]

#extract video and series names from paths
split_paths = files[, tstrsplit(filepath, "\\" , fixed = TRUE)]
files[, video_name := tools::file_path_sans_ext(basename(filepath))]

files[, dir := dirname(filepath)]
files[, series := split_paths[, V5]]

#select subset of available videos from annotations

#subset annotations by which series match and select useful columns
sub_annot = annot[`serie name` %in% files$series,.(`Study area name`,`serie name`, year, date_obs, time_obs,filename, 
                                               query_Annotations_species_species, `N_witin flashrange`,
                                               query_Annotations_type_type, `Scalp/sikkerhed`
)]
setnames(sub_annot, c("Study area name", "serie name", "query_Annotations_species_species", 
                      "query_Annotations_type_type", "Scalp/sikkerhed", "N_witin flashrange",
                      "date_obs", "time_obs"),
                    c("area", "series", "species", "type", "certainty", "count",
                      "date", "time"))




#subset files by which series were annotated
sub_files = files[series %in% sub_annot$series, ]

#library(exifr)
#test = read_exif(test_files[, filepath])
#USE columns: FileModifyDate and CreateDate
#run with external exif tool because exif r is too slow

exiftool_path = file.path("C:/Users/au784040/Documents_C/exiftool-13.32_64/exiftool.exe")

filepaths <- sub_files$filepath
batch_size <- 1000
n <- length(filepaths)
batches <- split(filepaths, ceiling(seq_along(filepaths)/batch_size))


# Progress bar setup
pb <- progress_bar$new(
  format = "[:bar] :current/:total (:percent) eta: :eta",
  total = length(batches),
  width = 60
)

results <- list()

for (i in seq_along(batches)) {
  batch <- batches[[i]]
  tmpfile <- tempfile(fileext = ".txt")
  #write all paths from the batch to txt file
  writeLines(batch, tmpfile)
  
  #create cmd to run exiftool on batch and return csv output
  cmd <- sprintf('"%s" -FileModifyDate -CreateDate -@ "%s" -csv', exiftool_path, tmpfile)
  #csv output is read by data.table and saved
  result <- tryCatch(fread(cmd = cmd), error = function(e) NULL)
  
  if (!is.null(result)) {
    results[[i]] <- result
  }
  pb$tick()
}

# Combine all results
all_results <- rbindlist(results, use.names = TRUE, fill = TRUE)

sub_files[, TimeCharMod := all_results$FileModifyDate]
sub_files[, TimeCharOrig := all_results$CreateDate]

sub_files[, TimeStampUTC := ymd_hms(TimeCharOrig, tz = "UTC")]
sub_files[, unique(substring(sub_files$TimeCharMod, 20))]
sub_files[, TimeMod := ifelse(substring(TimeCharMod, 20) == "+01:00", 1, 2)]

sub_files[, TimeStampCET := with_tz(TimeStampUTC, tzone = "Europe/Copenhagen")]

sub_files[, filename := paste0(series, "_file_", format(TimeStampCET, "%Y%m%d_%H%M00"))]


# Perform full outer join by 'filename'
testDataSet <- merge(
  sub_annot,
  sub_files[, .(filename, filepath, video_name, TimeStampCET)],
  by = "filename",
  all = TRUE  # full outer join
)

match = testDataSet[!(is.na(filepath) | is.na(species)),]
unmatch_annot = testDataSet[is.na(filepath),]
unmatch_file = testDataSet[is.na(species),]

match[, TimeStampCET := format(TimeStampCET, "%Y-%m-%d %H:%M:%S")]

setcolorder(match, c("filename", "filepath", "video_name","area", "series", 
                     "TimeStampCET", "year", "date", "time", 
                     "species", "count", "type", "certainty"))

fwrite(match, "WildCam_testDataSet.csv")

