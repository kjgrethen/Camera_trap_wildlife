# Script wrangle data from camera trap project 

#clear workspace
rm(list = ls())

set.seed(42)
library(data.table) # data.frames better
library(stringr) #dealing with strings

#set path
data_path = file.path("D:/NHMA")

wd = file.path("C:/Users/au784040/Documents_C/wolf_project")

#read in file containing all paths to images in directory
files = fread(file = file.path(data_path, "file_list.txt"), header = FALSE, col.names = "filepath", sep = "\n")

#read in annotations
annot = fread(file = file.path(wd, "AnnotationsExport.csv"), encoding = "Latin-1")

locations = annot[, unique(`Study area name`)]
series = annot[, unique(`serie name`)]

#extract video and series names from paths
split_paths = files[, tstrsplit(filepath, "\\" , fixed = TRUE)]
files[, video_name := tools::file_path_sans_ext(basename(filepath))]

#TODO: find out how to extract cortrect series name
files[, dir := dirname(filepath)]
#files[, series := basename(dir)]


#TODO: find out how to get to the correct file name
#select subset of available videos from annotations
#select <- annot[files, 
#                on = .(`serie name` = series), 
#                nomatch = 0]
