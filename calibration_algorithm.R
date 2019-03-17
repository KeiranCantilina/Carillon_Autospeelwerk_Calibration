## R audio pulse syncing function

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


## Number of notes in the clock sync header of the calibration tune
number_clocks <- 8

## Number of notes in the total calibration tune
number_notes <- 102

## Import recording of calibration tune
recording <- readWave("C:\\Users\\Keiran\\Desktop\\Findpeaks test.wav")

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

## Build reference vector (ref_times) of expected peak timestamps based on the drum measure times, using first clock peak as starting time
origin_time <- peak_times[1]
ref_times <- c(origin_time)

for (i in 1:(number_notes-1)){
  ref_times <- append(ref_times, origin_time+(average_measure_time*i))
}

## Compare ref (ref_times) to dataset (peak_times) and ouptut diff vector
error_times <- peak_times-ref_times

## Compute error per note
  ## Gonna either need a MusicXML import here or a lookup table

## Convert timing error to adjustment length required or rotations (need table of thread pitches for this)

## Visualize data in fun ways
