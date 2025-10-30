# Script to compare deepfaune output to annotations

#clear workspace
rm(list = ls())

set.seed(42)
library(data.table) # data.frames better
library(stringr) #dealing with strings
library(stringi) #dealing with strings but faster
library(progress) #for progress bar for lengthy procedures
library(lubridate) #for working with timestamps


#load files

annot = fread(file.path("C:/Users/au784040/OneDrive - Aarhus universitet/Documents/Projects/WildCam/WildCam_testDataSet.csv"))
deepfaune = fread(file.path("C:/Users/au784040/OneDrive - Aarhus universitet/Documents/Projects/WildCam/deepfauneResults1.csv"))

#extract video and series names from paths
split_paths = deepfaune[, tstrsplit(filename, "\\" , fixed = TRUE)]
deepfaune[, series := split_paths[, V6]]
deepfaune[, video_name := tools::file_path_sans_ext(basename(filename))]

setnames(deepfaune, c("filename", "date", "count"), c("df_filepath", "df_date", "df_count"))

compar = merge(deepfaune[, c("series","video_name", 'prediction', "score", "top1", "df_count" )], 
               annot, by = c("series", "video_name"))

setcolorder(compar, c("filename", "filepath", "video_name","area", "series", 
                     "TimeStampCET", "year", "date", "time", 
                     "species", "prediction", "count", "df_count", "type", "certainty", "score"))

unique(compar$species)

unique(compar$prediction)

#cannot be compared as no category
test = compar[species == "raccoon dog", ]
compar = compar[species != "raccoon dog", ]

compar[prediction == "wild boar", prediction := "boar" ]
compar[prediction == "lagomorph", prediction := "hare" ]
compar[prediction == "cow", prediction := "ungulate sp." ]
compar[species == "mustelid sp.", species := "mustelid" ]
compar[species == "martes foina", species := "mustelid" ]
compar[species == "marten sp.", species := "mustelid" ]
compar[species == "martes martes", species := "mustelid" ]
compar[species == "polecat", species := "mustelid" ]
#compar[species == "raccoon dog", species := "raccoon" ]
compar[species == "unidentified mammal", species := "undefined" ]

categories = unique(c(compar$species, compar$prediction))

compar[,prediction := factor(prediction, levels = categories)]
compar[,species := factor(species, levels = categories)]

table(compar$prediction, compar$species)
table(compar$count, compar$df_count)

#Classification accuracy
mean(compar$prediction == compar$species)

#count accuracy
mean(compar$count == compar$df_count)

#false negatives
compar[prediction ==  'empty',.N]
mean(compar$prediction == 'empty')

#uncertain
compar[prediction ==  'undefined',.N]
mean(compar$prediction == 'undefined')

#check mismatches
mismatch = compar[species != prediction & prediction != "empty",]
test = mismatch[prediction != "undefined"]

fwrite( test,"Mismatch_Deepfaune.csv", sep = ";")
