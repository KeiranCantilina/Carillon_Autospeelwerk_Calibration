## Carillon Autospeelwerk Calibration Error Quantifier

## Note: For the process to work well, please make sure the recording does not contain loud noises other than bells, and that there
## is several seconds of silence before and after the calibration tune.

library(signal)
library(fftw)
library(seewave)
library(tuneR)
library(audio)
library(reshape2)
library(ggplot2)
library(ggpmisc)
library(splus2R)
library(gginnards)


recording_path <- "C:\\Users\\Keiran\\Desktop\\Calibration Tune test.wav"
calibration_tune_path <- "C:\\Users\\Keiran\\Desktop\\Carillon Thesis Composition\\Carillon autospeelwerk project\\Calibration Tune.abc"
autospeelwerk_range_path <- "C:\\Users\\Keiran\\Desktop\\Carillon Thesis Composition\\Carillon autospeelwerk project\\Checklist.abc"
output_path <- "C:\\Users\\Keiran\\Desktop\\Carillon Thesis Composition\\Carillon autospeelwerk project\\abc2xml_218\\results.abc"

## Number of notes in the clock sync header of the calibration tune
number_clocks <- 8

## Number of notes in the total calibration tune
number_notes <- 102

## Number of notes available in the range of teh autospeelwerk (not including repeated notes)
number_autospeelwerk_range <- 40

## Import recording of calibration tune
recording <- readWave(recording_path)

## Import calibration tune ABC file
calibration_tune_file <- read.delim(calibration_tune_path)
calibration_tune_raw <- as.character(calibration_tune_file[9,])
calibration_tune_raw <- gsub(" | ",'\n', calibration_tune_raw, fixed=TRUE)
calibration_tune <- read.table(text=calibration_tune_raw,fill = TRUE, header = FALSE, stringsAsFactors = FALSE)

## Import autospeelwerk note range ABC file
autospeelwerk_range_file <- read.delim(autospeelwerk_range_path, stringsAsFactors = FALSE)
autospeelwerk_range_raw <- as.character(autospeelwerk_range_file[8,])
autospeelwerk_range_raw <- gsub(" | ",'\n', autospeelwerk_range_raw, fixed=TRUE)
autospeelwerk_range <- read.table(text=autospeelwerk_range_raw,fill = TRUE, header = FALSE, stringsAsFactors = FALSE)

## Extract filtetred amplitude envelope (contour)
contour <- acoustat(recording)

## Visualize contour
contour_graph <- contour$time.contour

## Reformat contour as timestamp/amplitude dataframe
contour_dataframe <- as.data.frame(contour_graph)

## Plot contour and compute peaks
peaks_ggplot <- ggplot(contour_dataframe, aes(time,contour))+ geom_line() + stat_peaks(label.fmt = "%0.6f", ignore_threshold= 0.1, colour = "red", span = 50, strict=TRUE, geom="label", hjust = -0.1)
peaks_ggplot

## extract peaks from ggplot object
peaks_ggplot_data <- ggplot_build(peaks_ggplot)
peak_times <- as.numeric(peaks_ggplot_data[["data"]][[2]][["label"]])


## Sort timestamps from first to last (just in case)
peak_times <- sort(peak_times)

## Extract clock sync from data (should be first 8 notes)
clock_times <- peak_times[1:number_clocks]

## Extract reference drum measure duration from clock sync
clock_diffs <- diff(clock_times)
average_measure_time <- mean(clock_diffs)

## Just for fun, find average measure over entire recording (might be more accurate)
total_diffs <- diff(peak_times)
average_total_diffs <- mean(total_diffs)

## Build reference vector (ref_times) of expected peak timestamps based on the drum measure times, using first clock peak as starting time
origin_time <- peak_times[1]
ref_times <- c(origin_time)

for (i in 1:(number_notes-1)){
  ref_times <- append(ref_times, origin_time+(average_total_diffs*i)) ## Try subbing average total vs. average clock diffs
}

## Compare ref (ref_times) to dataset (peak_times) and ouptut diff vector (observed-expected).
## Positive errors are notes playing too late; negative errors are notes playing too early.
error_times <- peak_times-ref_times
plot(error_times)
hist(error_times, breaks=10)

## Pair error with notes in cal tune 
calibration_results <- as.data.frame(calibration_tune[1:number_notes,])
colnames(calibration_results) <- "Notes"
calibration_results$Error <- error_times

## Compute error per note
## use lookup table and grep to pair errors (averaged) with notes
collated_results <- as.data.frame(autospeelwerk_range[1:number_autospeelwerk_range,])
colnames(collated_results) <- "Notes"
collated_results$Error <- NA

for (j in 1:number_autospeelwerk_range){
  grep_indices <- grep(autospeelwerk_range[j,1],calibration_results$Notes)
  grep_mean <- mean(calibration_results$Error[grep_indices[1:length(grep_indices)]])
  collated_results$Error[j] <- grep_mean
}

## Output as ABC file with annotations over notes (^"text")
results_ABC_file <- data.frame(autospeelwerk_range_file$X.1, stringsAsFactors = FALSE)
temp_paste_vector_1 <- c()
for (k in 1:length(collated_results$Notes)){
  temp_paste_vector_1[k] <- paste('@"', sprintf("%0.3f",collated_results$Error[k]),'"', collated_results$Notes[k],sep = "")
}

temp_paste_vector_2 <- paste(temp_paste_vector_1, collapse=" | ")
results_ABC_file[8,] <-temp_paste_vector_2
results_ABC_file[1,] <- "T:Results"
write.table(results_ABC_file, output_path, quote=FALSE, row.names = FALSE, col.names = FALSE, sep="\n")

## Convert ABC File to musicXML file, leave in results folder
command <- "\"C:\\Users\\Keiran\\Desktop\\Carillon Thesis Composition\\Carillon autospeelwerk project\\abc2xml_218\\abc2xml.exe\" \"C:\\Users\\Keiran\\Desktop\\Carillon Thesis Composition\\Carillon autospeelwerk project\\abc2xml_218\\results.abc\" -o \"C:\\Users\\Keiran\\Desktop\\Carillon Thesis Composition\\Carillon autospeelwerk project\\Results\" -z replace"
system(command)

## Convert timing error to length



