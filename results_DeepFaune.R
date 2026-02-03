# Script to compare deepfaune output to annotations

#clear workspace
rm(list = ls())

set.seed(42)
library(data.table) # data.frames better
library(stringr) #dealing with strings
library(stringi) #dealing with strings but faster
library(progress) #for progress bar for lengthy procedures
library(lubridate) #for working with timestamps
library(tidyverse)

#load files

#annot = fread(file.path("C:/Users/au761482/Downloads/WildCam_testDataSet.csv"))
#deepfaune = fread(file.path("C:/Users/au761482/OneDrive - Aarhus universitet/Documents/WildCam/testdata_heavy.csv"))

annot = fread(file.path("C:/Users/au784040/OneDrive - Aarhus universitet/Documents/Projects/WildCam/WildCam_testDataSet.csv"))
deepfaune = fread(file.path("C:/Users/au784040/OneDrive - Aarhus universitet/Documents/Projects/WildCam/testdata_deepfaune.csv"))
redwood = fread(file.path("C:/Users/au784040/OneDrive - Aarhus universitet/Documents/Projects/WildCam/testdata_redwood.csv"))

#extract video and series names from paths
#Use \\ for windows paths and / for linux
split_paths = deepfaune[, tstrsplit(filename, "/" , fixed = TRUE)]
deepfaune[, series := split_paths[, V10]]
deepfaune[, video_name := tools::file_path_sans_ext(basename(filename))]

setnames(deepfaune, c("filename", "dates", "count"), c("df_filepath", "df_date", "df_count"))

compar = merge(deepfaune[, c("series","video_name", 'prediction', "score", "Top1", "df_count" )], 
               annot, by = c("series", "video_name"))

setcolorder(compar, c("filename", "filepath", "video_name","area", "series", 
                     "TimeStampCET", "year", "date", "time", 
                     "species", "prediction", "count", "df_count", "type", "certainty", "score"))

unique(compar$species)

unique(compar$prediction)
unique(compar$Top1)

compar[, annot := species]
compar[, df := prediction]

compar = compar[!grep("other",annot),]

#cannot be compared as no category
#test = compar[species == "raccoon dog", ]
# compar = compar[species != "raccoon dog", ]

compar[Top1 == "wild boar", Top1 := "boar" ]
compar[Top1 == "lagomorph", Top1 := "hare" ]
compar[Top1 == "cow", Top1 := "ungulate sp." ]
compar[df == "wild boar", df := "boar" ]
compar[df == "lagomorph", df := "hare" ]
compar[df == "cow", df := "ungulate sp." ]
compar[df == "badger", df := "mustelid" ]
compar[df == "otter", df := "mustelid" ]


compar[annot == "mustelid sp.", annot := "mustelid" ]
compar[annot == "martes foina", annot := "mustelid" ]
compar[annot == "marten sp.", annot := "mustelid" ]
compar[annot == "martes martes", annot := "mustelid" ]
compar[annot == "polecat", annot := "mustelid" ]
compar[annot == "badger", annot := "mustelid" ]
compar[annot == "otter", annot := "mustelid" ]
compar[annot == "unidentified mammal", annot := "undefined" ]

categories = unique(c(compar$annot, compar$df))

compar[,df := factor(df, levels = categories)]
compar[,annot := factor(annot, levels = categories)]

table(compar$df, compar$annot)
table(compar$count, compar$df_count)

#Classification accuracy
mean(compar$annot== compar$df)

#count accuracy
mean(compar$count == compar$df_count)

#false negatives
compar[df ==  'empty',.N]
mean(compar$df == 'empty')

#uncertain
compar[df ==  'undefined',.N]
mean(compar$df == 'undefined')

#incorrect
mean(compar$df != 'undefined' & compar$df != 'empty' & compar$df != compar$annot)

#check mismatches
unqual = compar[annot != df & df != "empty",]
mismatch = unqual[annot != Top1,]

fwrite( mismatch,"Mismatch_Deepfaune.csv", sep = ";")


compar |>
  as_tibble() |>
  add_count(annot) |>
  filter(n > 10) |>
  mutate(disagree = ifelse(annot == df, "Agree",
                           ifelse(df == "empty", "Misses",
                                  "Disagree"))) |>
  ggplot(aes(x = annot, y = score, colour = disagree)) +
  geom_hline(yintercept = 0.8, colour = "grey", linetype = "dashed") +
  geom_boxplot(outlier.shape = NULL) +   # keep boxes black
  geom_jitter(position = position_jitterdodge(dodge.width = 0.5)) +
  
  scale_colour_manual(
    values = c("Agree" = "black", "Disagree" = "orange", "Misses" ="red"),
    name = "Prediction"
  ) +
  labs(x = "annotated species", y ="certainty")+
  theme_bw()

