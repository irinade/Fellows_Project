library(soundgen)
library(ProjectTemplate)
library(tuneR)
library(tidyverse)

# Creating a list containing all the paths of the mp3 files we want to analyze

files <- list.files("Desktop/Fellows", pattern=(".mp3"))

#Writing a function for sound analysis

extract_acoustics <- function(files){
  
  # Reading an mp3 file
  rec1 <- readMP3(files)
  path <- files
  name <- strsplit(path, "-") #Extracting species vernacular name
  
  #Analyzing fundamental harmonic of each syllable
  
  ana <- analyze(path)
  anasyl <- filter(ana, voiced == TRUE)
  anasyl <- filter(anasyl, anasyl$dom != "NA")
  
  #Plotting spectrogram and saving it as pdf
  
  pdf(file=paste("Desktop/Fellows/",name[[1]][2],"_plot.pdf",sep=""))
  spectrogram(path)
  dev.off()
  
  #Getting the syllables and bursts
  
  syl <- segment(path)
  length(syl$syllables$start) # Number of syllables
  sylen <- syl$syllables$sylLen #Getting the list of syllable lenghts
  
  pause <- syl$syllables$pauseLen #List of pause between syllables
  pause <- as.array(pause) 
  pause <- pause[1:length(pause)-1] #Extracting last value of the list (always NA)
  
  #Computing syllable rate
  
  rate <- 1/mean(pause)
  
  #Creating data frame with max, min, mean sd syllable lenght, syllable rate, 
  #mean dominant frequency, power of harmonics, harmonics to noise ratio, 
  #frequency of harmonics 1 to 3, entropy and slope
  #mean(sylen, na.rm=TRUE)
  
  df <- data.frame(name[[1]][2], max(sylen, rm.na = TRUE),min(sylen, rm.na = TRUE),mean(sylen, rm.na = TRUE), sd(sylen),rate,mean(anasyl$dom, rm.na = TRUE), max(anasyl$dom, rm.na = TRUE), min(anasyl$dom, rm.na = TRUE), sd(anasyl$dom), mean(anasyl$harmonics, rm.na = TRUE), max(anasyl$harmonics, rm.na = TRUE), min(anasyl$harmonics, rm.na = TRUE), sd(anasyl$harmonics), mean(anasyl$HNR, rm.na = TRUE), max(anasyl$HNR, rm.na = TRUE),min(anasyl$HNR, rm.na = TRUE), sd(anasyl$HNR), mean(anasyl$f1_freq, rm.na = TRUE), max(anasyl$f1_freq, rm.na = TRUE), min(anasyl$f1_freq, rm.na = TRUE), sd(anasyl$f1_freq), mean(anasyl$f2_freq, rm.na = TRUE),max(anasyl$f2_freq, rm.na = TRUE),  min(anasyl$f2_freq, rm.na = TRUE), sd(anasyl$f2_freq), mean(anasyl$f3_freq, rm.na = TRUE), max(anasyl$f3_freq, rm.na = TRUE),  min(anasyl$f3_freq, rm.na = TRUE), sd(anasyl$f3_freq), mean(anasyl$entropy, rm.na = TRUE), max(anasyl$entropy, rm.na = TRUE),  min(anasyl$entropy, rm.na = TRUE), sd(anasyl$entropy), mean(anasyl$specSlope, rm.na = TRUE), max(anasyl$specSlope, rm.na = TRUE), min(anasyl$specSlope, rm.na = TRUE),sd(anasyl$specSlope))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
  #Merging dataframe to full dataframe with all analyzed recordings data
  
  rep <- read.csv("Desktop/Fellows/birds.csv")
  rep <- rbind(rep[2:39], df)
  write.csv(rep, "Desktop/Fellows/birds.csv")}

#Calling the function to analyze all the files contained in the directory

for (i in 1:length(files)){
  extract_acoustics(paste("Desktop/Fellows/",files[i], sep = ""))}
