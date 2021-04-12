################################################################################
## Programme name: Nature Aging Paper ADAMANT FIGURES
## Programme language: R (RStudio Version 1.2.5042)
## Initial date: 2020-04-30
## Sponsor | study: AC-AD-003 | ADAMANT
## Author(s): Stanislav Katina (SK)
################################################################################
## Short description: Statistical analyses, figures for Nature Paper
## Requirements: -
## Risk assessment: moderate
################################################################################
## Input:
##   - file "..."
##    - path ""
## Output: 
##   - file "..."
##    - path "..."
## Required libraries/packages:
## - tidyverse, ggplot2, rio, extrafont, Hmisc, scales, openxlsx
## Required functions:
## - 
################################################################################
## Document history:
## Version 	Date 			Author 		Purpose
## V01		  20200305	SK 		    final
## V02		  20200430	SK 		    adding new variables, small corrections
## V03		  20200520	SK 		    adding new variables, small corrections
## V04		  20200630	SK 		    adding new variables, small corrections
## V05		  20200814	SK 		    adding new variables, small corrections
## V06		  20200821	SK 		    adding new variables, small corrections
## V07		  20210409	SK 		    export: data and tables for figures
## V0á		  20210409	SK 		    figures, renumbering, leaving only figures in the paper and supplement
################################################################################

library(tidyverse)
library(ggplot2)
library(rio)
library(extrafont)
library(Hmisc)
library(scales)
library(openxlsx)

# data for figures
dir_svalz <- "~/Documents/SCIENCE/NEURO-STAT/AXON-STAT/ADAMANT/RESULTS/PAPER-ADAMANT/DATA/SVALZ-DATA"
dir_lab <- "~/Documents/SCIENCE/NEURO-STAT/AXON-STAT/ADAMANT/RESULTS/PAPER-ADAMANT/DATA/LAB-DATA"
dir_dti <- "~/Documents/SCIENCE/NEURO-STAT/AXON-STAT/ADAMANT/RESULTS/PAPER-ADAMANT/DATA/DTI-DATA"
## data for supplementary figures
dir_cog <- "~/Documents/SCIENCE/NEURO-STAT/AXON-STAT/ADAMANT/RESULTS/PAPER-ADAMANT/DATA/COG-DATA"
dir_mri <- "~/Documents/SCIENCE/NEURO-STAT/AXON-STAT/ADAMANT/RESULTS/PAPER-ADAMANT/DATA/MRI-DATA"
## figures
dir_figs <- "~/Documents/SCIENCE/NEURO-STAT/AXON-STAT/ADAMANT/RESULTS/PAPER-ADAMANT/FIGURES"
dir_figs_final <- "~/Documents/SCIENCE/NEURO-STAT/AXON-STAT/ADAMANT/RESULTS/PAPER-ADAMANT/SUBMISSION/FIGURES_EXPORT"
dir_data_final <- "~/Documents/SCIENCE/NEURO-STAT/AXON-STAT/ADAMANT/RESULTS/PAPER-ADAMANT/SUBMISSION/DATA_FIGURES_EXPORT"

################################################################################
# IgG Antibody Titre Against Axon Peptide 108 (serum)
################################################################################
# FIGURE 2 a

file_IgGaP108_serum <- "ACAD003_600000207_20200219.csv"
filename <- file_IgGaP108_serum
filedir <- dir_lab

DATA_all_IgGaP108_serum <-  DataWranglingNaturePaper(filename, filedir, k_visits = 18, variable = "titre")
DATA_IgGaP108_serum_long <- DATA_all_IgGaP108_serum$DATA_long
TABS_IgGaP108_serum <- DATA_all_IgGaP108_serum$TABS
range_y <- DATA_all_IgGaP108_serum$range_y
box_colors <- rep("#33CCFF",18)
box_colors <- rep("mediumpurple2",18)
bar_colors <- rep("black",18)


################################################################################
## Export data and tables

setwd(dir_data_final)

DATA_all_IgGaP108_serum_export <- list()
DATA_all_IgGaP108_serum_export[[1]] <- DATA_all_IgGaP108_serum[[1]]
DATA_all_IgGaP108_serum_export[[2]] <- DATA_all_IgGaP108_serum[[2]]
names(DATA_all_IgGaP108_serum_export) <- c("DATA FIG 2a","TABLE FIG 2a")

openxlsx::write.xlsx(DATA_all_IgGaP108_serum_export, file = "stka_NATURE_DATA_Figure_02a_20210409.xlsx")

################################################################################
## Export figures

setwd(dir_figs_final)

quartz(height = 4, width = 6)
visits <- c("0","4","8","12","16","20","24","34","38","48","52","62","66","76","80","90","94","104")
x <- GGboxplotDiagnoses(DATA_IgGaP108_serum_long, xlim = c(1,18), ylim = range_y, 
                        x_labels = visits,
                        variable = "Serum anti-P108 IgG (dilution fold)",
                        box_colors = box_colors, bar_colors = bar_colors, type = "IgG aP108 (serum)",
                        yintercept = 100)
# vaccines weeks c("4","8","12","16","20","34","48","62","76","90")
visits <- factor(visits, levels = visits, labels = visits)
vaccine <- 80
ID_vaccines <- c(vaccine,vaccine,vaccine,vaccine,vaccine,vaccine,NA,vaccine,NA,vaccine,NA,vaccine,NA,vaccine,NA,vaccine,NA,NA)
visits_df <- data.frame(visits,ID_vaccines)
rownames(visits_df) <- NULL
x + geom_point(visits_df,  mapping = aes(x = visits, y = ID_vaccines), shape = rep(17,18), col = box_colors)

pngname <- paste("Figure_2a_boxplot_","IgGaP108_serum_20210409.png", sep = "")
quartz.save(pngname, type = "png", width = 6, height = 4, family = "Helvetica", bg = "white", dpi = 1200)
pdfname <- paste("Figure_2a_boxplot_","IgGaP108_serum_20210409.pdf", sep = "")
quartz.save(pdfname, type = "pdf", width = 6, height = 4, family = "Helvetica", bg = "white", dpi = 1200)

################################################################################
# IgG Antibody Titre Against Axon Peptide 108 quantifiable by qELISA (serum)
################################################################################
# FIGURE 2 b

file_IgGQp108_serum <- "ACAD003_600000804_20200219.csv"
filename <- file_IgGQp108_serum
filedir <- dir_svalz

DATA_all_IgGQp108_serum <-  DataWranglingNaturePaper(filename, filedir, k_visits = 18, variable = "titre")
DATA_IgGQp108_serum_long <- DATA_all_IgGQp108_serum$DATA_long
DATA_IgGQp108_serum_long$result[DATA_IgGQp108_serum_long$visit == 0] <- NA
TABS_IgGQp108_serum <- DATA_all_IgGQp108_serum$TABS
TABS_IgGQp108_serum[[2]][1] <- 0
TABS_IgGQp108_serum[[3]][1] <- NA
TABS_IgGQp108_serum[[4]][1] <- NA
TABS_IgGQp108_serum[[5]][1] <- NA
TABS_IgGQp108_serum[[6]][1] <- NA
range_y <- DATA_all_IgGQp108_serum$range_y
box_colors <- rep("#33CCFF",18)
box_colors <- rep("mediumpurple2",18)
bar_colors <- rep("black",18)

################################################################################
## Export data and tables

setwd(dir_data_final)

DATA_all_IgGQp108_serum_export <- list()
DATA_all_IgGQp108_serum_export[[1]] <- DATA_all_IgGQp108_serum[[1]]
DATA_all_IgGQp108_serum_export[[2]] <- DATA_all_IgGQp108_serum[[2]]
names(DATA_all_IgGQp108_serum_export) <- c("DATA FIG 2b","TABLE FIG 2b")

openxlsx::write.xlsx(DATA_all_IgGQp108_serum_export, file = "stka_NATURE_DATA_Figure_02b_20210409.xlsx")

################################################################################
## Export figures

setwd(dir_figs_final)

quartz(height = 4, width = 6)
visits <- c("0","4","8","12","16","20","24","34","38","48","52","62","66","76","80","90","94","104")
x <- GGboxplotDiagnoses(DATA_IgGQp108_serum_long, xlim = c(1,18), ylim = range_y, 
                        x_labels = visits,
                        variable = "Serum anti-P108 IgG (ng/mL)",
                        box_colors = box_colors, bar_colors = bar_colors, type = "IgG Qp108 (serum)",
                        yintercept = 300)
# vaccines weeks c("4","8","12","16","20","34","48","62","76","90")
visits <- c("0","4","8","12","16","20","24","34","38","48","52","62","66","76","80","90","94","104")
visits <- factor(visits, levels = visits, labels = visits)
vaccine <- 260
ID_vaccines <- c(vaccine,vaccine,vaccine,vaccine,vaccine,vaccine,NA,vaccine,NA,vaccine,NA,vaccine,NA,vaccine,NA,vaccine,NA,NA)
visits_df <- data.frame(visits,ID_vaccines)
rownames(visits_df) <- NULL
x + geom_point(visits_df,  mapping = aes(x = visits, y = ID_vaccines), shape = rep(17,18), col = box_colors)

pngname <- paste("Figure_2b_boxplot_","IgGQp108_serum_20210409.png", sep = "")
quartz.save(pngname, type = "png", width = 6, height = 4, family = "Helvetica", bg = "white", dpi = 1200)
pdfname <- paste("Figure_2b_boxplot_","IgGQp108_serum_20210409.pdf", sep = "")
quartz.save(pdfname, type = "pdf", width = 6, height = 4, family = "Helvetica", bg = "white", dpi = 1200)

################################################################################
## Neurofilament light chain (plasma)
################################################################################
## FIGURE 3 a,b

file_plasma_NfL <- "20190807_ACAD003_SVaLZ_LAB_600000331.axon.csv"
filename <- file_plasma_NfL 
filedir <- dir_svalz

DATA_all_plasma_NfL <-  DataWranglingNaturePaper(filename, filedir, k_visits = 4, variable = "NfL")
DATA_plasma_NfL_long <- DATA_all_plasma_NfL$DATA_long
wDATA_plasma_NfL_long <- DATA_all_plasma_NfL$wDATA_long
wTABS_plasma_NfL <- DATA_all_plasma_NfL$wTABS_diff
wdTABS_plasma_NfL_diff <- DATA_all_plasma_NfL$wdTABS_diff
wDATA_plasma_NfL_long_V02 <- DATA_all_plasma_NfL$wDATA_long_V02
range_y <- DATA_all_plasma_NfL$range_y
#range_y <- range(wDATA_plasma_NfL_long$result, na.rm = TRUE)

DATA_plasma_NfL_long_V02 <- filter(DATA_plasma_NfL_long, visit == "0")

## baseline characteristics of NfL

mean_NfL_V02 <- MeanNA(wDATA_plasma_NfL_long_V02$result)
lower_NfL_V02 <- CiLower(wDATA_plasma_NfL_long_V02$result)
upper_NfL_V02 <- CiUpper(wDATA_plasma_NfL_long_V02$result)

# All together ... 20.76 (19.672,21.858)
# ADDvac1 ... 21.08 (19.468,22.683)
# Placebo ... 20.56 (19.045,22.077)

DATA_plasma_NfL_long_V02 <- filter(DATA_plasma_NfL_long, visit == "0")
wDATA_plasma_NfL_long_V02 <- filter(wDATA_plasma_NfL_long, visit == "0")

TABS_NfL_V02 <- SummariseBasicChars(DATA_plasma_NfL_long_V02) # winsorising
wTABS_NfL_V02 <- SummariseBasicChars(wDATA_plasma_NfL_long_V02) # winsorised

################################################################################
## Export data and tables

setwd(dir_data_final)

wDATA_all_plasma_NfL_export <- list()
wDATA_all_plasma_NfL_export[[1]] <- DATA_plasma_NfL_long_V02
wDATA_all_plasma_NfL_export[[2]] <- wTABS_NfL_V02
names(wDATA_all_plasma_NfL_export) <- c("DATA FIG 3a","TABLE FIG 3a")

openxlsx::write.xlsx(wDATA_all_plasma_NfL_export, file = "stka_NATURE_DATA_Figure_03a_20210409.xlsx")


wDATA_all_plasma_NfL_export <- list()
wDATA_all_plasma_NfL_export[[1]] <- wdDATA_diff_long
wDATA_all_plasma_NfL_export[[2]] <- wdTABS_plasma_NfL_diff
names(wDATA_all_plasma_NfL_export) <- c("DATA FIG 3b","TABLE FIG 3b")

openxlsx::write.xlsx(wDATA_all_plasma_NfL_export, file = "stka_NATURE_DATA_Figure_03b_20210409.xlsx")

################################################################################
## Export figures

setwd(dir_figs_final)

quartz(height = 4, width = 4)
GGboxplotDiagnosis(DATA_plasma_NfL_long_V02,ylim = range_y, variable = "Plasma NfL at baseline (pg/mL)", caption_text = "")
pngname <- paste("Figure_3a_boxplot_","plasma_NfL_20210409.png", sep = "")
quartz.save(pngname, type = "png", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)
pdfname <- paste("Figure_3a_boxplot_","plasma_NfL_20210409.pdf", sep = "")
quartz.save(pdfname, type = "pdf", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)

quartz(height = 4, width = 4)
CiTreatment(wdTABS_plasma_NfL_diff, xlim = c(1,4),  bar_width = 0.2, variable = "Plasma NfL (pg/mL) \n change from baseline")
pngname <- paste("Figure_3b_ci_","plasma_NfL_20210409.png", sep = "")
quartz.save(pngname, type = "png", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)
pdfname <- paste("Figure_3b_ci_","plasma_NfL_20210409.pdf", sep = "")
quartz.save(pdfname, type = "pdf", width = 4, height = 4, family = "Helvetica", bg = "white")

###############################################################################
## Phospho-tau pT181 (CSF)
################################################################################
## FIGURE 4 a,b

file_ptau_T181 <- "20190807_ACAD003_PPDeCRF_LAB_600000105.axon.csv"
filename <- file_ptau_T181 
filedir <- dir_lab

DATA_all_ptau_T181 <-  DataWranglingNaturePaper(filename, filedir, k_visits = 2, variable = "CSF")
wDATA_ptau_T181_long <- DATA_all_ptau_T181$wDATA_long
wTABS_ptau_T181 <- DATA_all_ptau_T181$wTABS_diff
wdTABS_ptau_T181_diff <- DATA_all_ptau_T181$wdTABS_diff
wDATA_ptau_T181_long_V02 <- DATA_all_ptau_T181$wDATA_long_V02
range_y <- DATA_all_ptau_T181$range_y

################################################################################
## Export data and tables

setwd(dir_data_final)

wDATA_all_ptau_T181_export <- list()
wDATA_all_ptau_T181_export[[1]] <- wDATA_ptau_T181_long
wDATA_all_ptau_T181_export[[2]] <- wTABS_ptau_T181
names(wDATA_all_ptau_T181_export) <- c("DATA FIG 4a","TABLE FIG 4a")

openxlsx::write.xlsx(wDATA_all_ptau_T181_export, file = "stka_NATURE_DATA_Figure_04a_20210409.xlsx")


wDATA_all_ptau_T181_export <- list()
wDATA_all_ptau_T181_export[[1]] <- DATA_all_ptau_T181$wdDATA_diff_long
wDATA_all_ptau_T181_export[[2]] <- wdTABS_ptau_T181_diff
names(wDATA_all_ptau_T181_export) <- c("DATA FIG 4b","TABLE FIG 4b")

openxlsx::write.xlsx(wDATA_all_ptau_T181_export, file = "stka_NATURE_DATA_Figure_04b_20210409.xlsx")

################################################################################
## Export figures

setwd(dir_figs_final)

quartz(height = 4, width = 4)
ProfilePlotTreatment(wDATA_ptau_T181_long,wTABS_ptau_T181, ylim = range_y, 
                     variable = "CSF p-tau T181 (pg/mL)")
pngname <- paste("Figure_4a_profile_","ptau_T181_20210409.png", sep = "")
quartz.save(pngname, type = "png", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)
pdfname <- paste("Figure_4a_profile_","ptau_T181_20210409.pdf", sep = "")
quartz.save(pdfname, type = "pdf", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)

quartz(height = 4, width = 4)
CiTreatment(wdTABS_ptau_T181_diff, variable = "CSF p-tau T181 (pg/mL) \n change from baseline")
pngname <- paste("Figure_4b_ci_","ptau_T181_20210409.png", sep = "")
quartz.save(pngname, type = "png", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)
pdfname <- paste("Figure_4b_ci_","ptau_T181_20210409.pdf", sep = "")
quartz.save(pdfname, type = "pdf", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)

##############################################################################
## Phospho-tau pT217 (CSF)
################################################################################
## FIGURE 4 c,d

#file_ptau_T217 <- "ACAD003_600000552_20200219.csv"
file_ptau_T217 <- "20190807_ACAD003_SVaLZ_LAB_600000552.axon.csv"

filename <- file_ptau_T217 
filedir <- dir_svalz

DATA_all_ptau_T217 <-  DataWranglingNaturePaper(filename, filedir, k_visits = 2, variable = "CSF")
wDATA_ptau_T217_long <- DATA_all_ptau_T217$wDATA_long
wTABS_ptau_T217 <- DATA_all_ptau_T217$wTABS_diff
wdTABS_ptau_T217_diff <- DATA_all_ptau_T217$wdTABS_diff
wDATA_ptau_T217_long_V02 <- DATA_all_ptau_T217$wDATA_long_V02
range_y <- DATA_all_ptau_T217$range_y

################################################################################
## Export data and tables

setwd(dir_data_final)

wDATA_all_ptau_T217_export <- list()
wDATA_all_ptau_T217_export[[1]] <- wDATA_ptau_T217_long
wDATA_all_ptau_T217_export[[2]] <- wTABS_ptau_T217
names(wDATA_all_ptau_T217_export) <- c("DATA FIG 4c","TABLE FIG 4c")

openxlsx::write.xlsx(wDATA_all_ptau_T217_export, file = "stka_NATURE_DATA_Figure_04c_20210409.xlsx")


wDATA_all_ptau_T217_export <- list()
wDATA_all_ptau_T217_export[[1]] <- DATA_all_ptau_T217$wdDATA_diff_long
wDATA_all_ptau_T217_export[[2]] <- wdTABS_ptau_T217_diff
names(wDATA_all_ptau_T217_export) <- c("DATA FIG 4d","TABLE FIG 4d")

openxlsx::write.xlsx(wDATA_all_ptau_T217_export, file = "stka_NATURE_DATA_Figure_04d_20210409.xlsx")

################################################################################
## Export figures

setwd(dir_figs_final)

quartz(height = 4, width = 4)
ProfilePlotTreatment(wDATA_ptau_T217_long,wTABS_ptau_T217, ylim = range_y,
                     variable = "CSF p-tau T217 (pg/mL)")
pngname <- paste("Figure_4c_profile_","ptau_T217_20210409.png", sep = "")
quartz.save(pngname, type = "png", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)
pdfname <- paste("Figure_4c_profile_","ptau_T217_20210409.pdf", sep = "")
quartz.save(pdfname, type = "pdf", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)

quartz(height = 4, width = 4)
CiTreatment(wdTABS_ptau_T217_diff, variable = "CSF p-tau T217 (pg/mL) \n change from baseline")
pngname <- paste("Figure_4d_ci_","ptau_T217_20210409.png", sep = "")
quartz.save(pngname, type = "png", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)
pdfname <- paste("Figure_4d_ci_","ptau_T217_20210409.pdf", sep = "")
quartz.save(pdfname, type = "pdf", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)

##############################################################################
## Total Tau (CSF)
################################################################################
## FIGURE 4 e,f

#file_ttau <- "ACAD003_600000025_20200219.csv"
file_ttau <- "20190807_ACAD003_PPDeCRF_LAB_600000025.axon.csv"
filename <- file_ttau 
filedir <- dir_svalz

DATA_all_ttau <-  DataWranglingNaturePaper(filename, filedir, k_visits = 2, variable = "CSF")
wDATA_ttau_long <- DATA_all_ttau$wDATA_long
wTABS_ttau <- DATA_all_ttau$wTABS_diff
wdTABS_ttau_diff <- DATA_all_ttau$wdTABS_diff
wDATA_ttau_long_V02 <- DATA_all_ttau$wDATA_long_V02
range_y <- DATA_all_ttau$range_y

################################################################################
## Export data and tables

setwd(dir_data_final)

wDATA_all_ttau_export <- list()
wDATA_all_ttau_export[[1]] <- wDATA_ttau_long
wDATA_all_ttau_export[[2]] <- wTABS_ttau
names(wDATA_all_ttau_export) <- c("DATA FIG 4e","TABLE FIG 4e")

openxlsx::write.xlsx(wDATA_all_ttau_export, file = "stka_NATURE_DATA_Figure_04e_20210409.xlsx")


wDATA_all_ttau_export <- list()
wDATA_all_ttau_export[[1]] <- DATA_all_ttau$wdDATA_diff_long
wDATA_all_ttau_export[[2]] <- wdTABS_ttau_diff
names(wDATA_all_ttau_export) <- c("DATA FIG 4f","TABLE FIG 4f")

openxlsx::write.xlsx(wDATA_all_ttau_export, file = "stka_NATURE_DATA_Figure_04f_20210409.xlsx")

################################################################################
## Export figures

setwd(dir_figs_final)

quartz(height = 4, width = 4)
ProfilePlotTreatment(wDATA_ttau_long,wTABS_ttau, ylim = range_y,
                     variable = "CSF t-tau (pg/mL)")
pngname <- paste("Figure_4e_profile_","ttau_20210409.png", sep = "")
quartz.save(pngname, type = "png", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)
pdfname <- paste("Figure_4e_profile_","ttau_20210409.pdf", sep = "")
quartz.save(pdfname, type = "pdf", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)

quartz(height = 4, width = 4)
CiTreatment(wdTABS_ttau_diff, variable = "CSF t-tau (pg/mL) \n change from baseline")
pngname <- paste("Figure_4f_ci_","ttau_20210409.png", sep = "")
quartz.save(pngname, type = "png", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)
pdfname <- paste("Figure_4f_ci_","ttau_20210409.pdf", sep = "")
quartz.save(pdfname, type = "pdf", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)

############################################################################
## 600000330	330	CSF Biomarker, Neurogranin
################################################################################
## Supplement FIGURE 8 a,b

filedir <- "~/Documents/SCIENCE/NEURO-STAT/AXON-STAT/ADAMANT/RESULTS/PAPER-ADAMANT/DATA/DATA_ADAMANT/DATA_ORIG"
file_neurogranin <- "20190807_ACAD003_PPDeCRF_LAB_600000330.axon.csv"
filename <- file_neurogranin 

DATA_all_neurogranin <-  DataWranglingNaturePaper(filename, filedir, k_visits = 2, variable = "CSF")

wDATA_neurogranin_long <- DATA_all_neurogranin$wDATA_long
wTABS_neurogranin <- DATA_all_neurogranin$wTABS_diff
wdTABS_neurogranin_diff <- DATA_all_neurogranin$wdTABS_diff
wDATA_neurogranin_long_V02 <- DATA_all_neurogranin$wDATA_long_V02
range_y <- DATA_all_neurogranin$range_y

################################################################################
## Export data and tables

setwd(dir_data_final)

wDATA_all_neurogranin_export <- list()
wDATA_all_neurogranin_export[[1]] <- wDATA_neurogranin_long
wDATA_all_neurogranin_export[[2]] <- wTABS_neurogranin
names(wDATA_all_neurogranin_export) <- c("DATA FIG 8a","TABLE FIG 8a")

openxlsx::write.xlsx(wDATA_all_neurogranin_export, file = "stka_NATURE_DATA_Figure_08a_20210409.xlsx")


wDATA_all_neurogranin_export <- list()
wDATA_all_neurogranin_export[[1]] <- DATA_all_neurogranin$wdDATA_diff_long
wDATA_all_neurogranin_export[[2]] <- wdTABS_neurogranin_diff
names(wDATA_all_neurogranin_export) <- c("DATA FIG 8b","TABLE FIG 8b")

openxlsx::write.xlsx(wDATA_all_neurogranin_export, file = "stka_NATURE_DATA_Figure_08b_20210409.xlsx")

################################################################################
## Export figures

setwd(dir_figs_final)

quartz(height = 4, width = 4)
ProfilePlotTreatment(wDATA_neurogranin_long,wTABS_neurogranin, ylim = range_y, 
                     variable =  bquote(Neurogranin~(pg/mL)))
pngname <- paste("Figure_suppl_8a_profile_","neurogranin_20210409.png", sep = "")
quartz.save(pngname, type = "png", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)
pdfname <- paste("Figure_suppl_8a_profile_","neurogranin_20210409.pdf", sep = "")
quartz.save(pdfname, type = "pdf", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)

quartz(height = 4, width = 4)
CiTreatment(wdTABS_neurogranin_diff, variable = bquote(atop(Neurogranin~(pg/mL),"change from baseline")),
            legend_position = c(0,0), legend_justification = c("left", "bottom"))
pngname <- paste("Figure_suppl_8b_ci_","neurogranin_20210409.png", sep = "")
quartz.save(pngname, type = "png", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)
pdfname <- paste("Figure_suppl_8b_ci_","neurogranin_20210409.pdf", sep = "")
quartz.save(pdfname, type = "pdf", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)

###############################################################################
## 600000329	329	CSF Biomarker, ABeta1-40
################################################################################
## Supplement FIGURE 8 c,d

file_ABeta1_40 <- "20190807_ACAD003_PPDeCRF_LAB_600000329.axon.csv"
filename <- file_ABeta1_40 
filedir <- dir_svalz

DATA_all_ABeta1_40 <-  DataWranglingNaturePaper(filename, filedir, k_visits = 2, variable = "CSF")
wDATA_ABeta1_40_long <- DATA_all_ABeta1_40$wDATA_long
wTABS_ABeta1_40 <- DATA_all_ABeta1_40$wTABS_diff
wdTABS_ABeta1_40_diff <- DATA_all_ABeta1_40$wdTABS_diff
wDATA_ABeta1_40_long_V02 <- DATA_all_ABeta1_40$wDATA_long_V02
range_y <- DATA_all_ABeta1_40$range_y

################################################################################
## Export data and tables

setwd(dir_data_final)

wDATA_all_ABeta1_40_export <- list()
wDATA_all_ABeta1_40_export[[1]] <- wDATA_ABeta1_40_long
wDATA_all_ABeta1_40_export[[2]] <- wTABS_ABeta1_40
names(wDATA_all_ABeta1_40_export) <- c("DATA FIG 8c","TABLE FIG 8c")

openxlsx::write.xlsx(wDATA_all_ABeta1_40_export, file = "stka_NATURE_DATA_Figure_08c_20210409.xlsx")


wDATA_all_ABeta1_40_export <- list()
wDATA_all_ABeta1_40_export[[1]] <- DATA_all_ABeta1_40$wdDATA_diff_long
wDATA_all_ABeta1_40_export[[2]] <- wdTABS_ABeta1_40_diff
names(wDATA_all_ABeta1_40_export) <- c("DATA FIG 8d","TABLE FIG 8d")

openxlsx::write.xlsx(wDATA_all_ABeta1_40_export, file = "stka_NATURE_DATA_Figure_08d_20210409.xlsx")

################################################################################
## Export figures

setwd(dir_figs_final)

quartz(height = 4, width = 4)
ProfilePlotTreatment(wDATA_ABeta1_40_long,wTABS_ABeta1_40, ylim = range_y, 
                     variable =  bquote(CSF~A*beta~40~(pg/mL)))
pngname <- paste("Figure_suppl_8c_profile_","ABeta1_40_20210409.png", sep = "")
quartz.save(pngname, type = "png", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)
pdfname <- paste("Figure_suppl_8c_profile_","ABeta1_40_20210409.pdf", sep = "")
quartz.save(pdfname, type = "pdf", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)

quartz(height = 4, width = 4)
CiTreatment(wdTABS_ABeta1_40_diff, variable = bquote(atop(CSF~A*beta~40~(pg/mL),"change from baseline")),
            legend_position = c(0,0), legend_justification = c("left", "bottom"))
pngname <- paste("Figure_suppl_8d_ci_","ABeta1_40_20210409.png", sep = "")
quartz.save(pngname, type = "png", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)
pdfname <- paste("Figure_suppl_8d_ci_","ABeta1_40_20210409.pdf", sep = "")
quartz.save(pdfname, type = "pdf", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)

###############################################################################
## 600000079	79	CSF Biomarker, ABeta1-42
################################################################################
## Supplement FIGURE 8 e,f

file_ABeta1_42 <- "20190807_ACAD003_PPDeCRF_LAB_600000079.axon.csv"
filename <- file_ABeta1_42 
filedir <- dir_svalz

DATA_all_ABeta1_42 <-  DataWranglingNaturePaper(filename, filedir, k_visits = 2, variable = "CSF")
wDATA_ABeta1_42_long <- DATA_all_ABeta1_42$wDATA_long
wTABS_ABeta1_42 <- DATA_all_ABeta1_42$wTABS_diff
wdTABS_ABeta1_42_diff <- DATA_all_ABeta1_42$wdTABS_diff
wDATA_ABeta1_42_long_V02 <- DATA_all_ABeta1_42$wDATA_long_V02
range_y <- DATA_all_ABeta1_42$range_y

################################################################################
## Export data and tables

setwd(dir_data_final)

wDATA_all_ABeta1_42_export <- list()
wDATA_all_ABeta1_42_export[[1]] <- wDATA_ABeta1_42_long
wDATA_all_ABeta1_42_export[[2]] <- wTABS_ABeta1_42
names(wDATA_all_ABeta1_42_export) <- c("DATA FIG 8e","TABLE FIG 8e")

openxlsx::write.xlsx(wDATA_all_ABeta1_42_export, file = "stka_NATURE_DATA_Figure_08e_20210409.xlsx")


wDATA_all_ABeta1_42_export <- list()
wDATA_all_ABeta1_42_export[[1]] <- DATA_all_ABeta1_42$wdDATA_diff_long
wDATA_all_ABeta1_42_export[[2]] <- wdTABS_ABeta1_42_diff
names(wDATA_all_ABeta1_42_export) <- c("DATA FIG 8f","TABLE FIG 8f")

openxlsx::write.xlsx(wDATA_all_ABeta1_42_export, file = "stka_NATURE_DATA_Figure_08f_20210409.xlsx")

################################################################################
## Export figures

setwd(dir_figs_final)

quartz(height = 4, width = 4)
ProfilePlotTreatment(wDATA_ABeta1_42_long,wTABS_ABeta1_42, ylim = range_y, 
                     variable =  bquote(CSF~A*beta~42~(pg/mL)))
pngname <- paste("Figure_suppl_8e_profile_","ABeta1_42_20210409.png", sep = "")
quartz.save(pngname, type = "png", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)
pdfname <- paste("Figure_suppl_8e_profile_","ABeta1_42_20210409.pdf", sep = "")
quartz.save(pdfname, type = "pdf", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)

quartz(height = 4, width = 4)
CiTreatment(wdTABS_ABeta1_42_diff, variable = bquote(atop(CSF~A*beta~42~(pg/mL),"change from baseline")),
            legend_position = c(0,0), legend_justification = c("left", "bottom"))
pngname <- paste("Figure_suppl_8f_ci_","ABeta1_42_20210409.png", sep = "")
quartz.save(pngname, type = "png", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)
pdfname <- paste("Figure_suppl_8f_ci_","ABeta1_42_20210409.pdf", sep = "")
quartz.save(pdfname, type = "pdf", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)

################################################################################
## DTI Fractional anisotropy of the fornix (FA Fornix)
################################################################################
## Supplement FIGURE 9 a,b

file_FA_Fornix <- "20190827_04_ACAD003_IXICO_MRI_400002046.csv"
filename <- file_FA_Fornix 
filedir <- dir_dti

DATA_all_FA_Fornix <-  DataWranglingNaturePaper(filename, filedir, k_visits = 2, variable = "DTI")
wDATA_FA_Fornix_long <- DATA_all_FA_Fornix$wDATA_long
wTABS_FA_Fornix <- DATA_all_FA_Fornix$wTABS_diff
wdTABS_FA_Fornix_diff <- DATA_all_FA_Fornix$wdTABS_diff
wDATA_FA_Fornix_long_V01 <- DATA_all_FA_Fornix$wDATA_long_V01
range_y <- DATA_all_FA_Fornix$range_y

################################################################################
## Export data and tables

setwd(dir_data_final)

wDATA_all_FA_Fornix_export <- list()
wDATA_all_FA_Fornix_export[[1]] <- wDATA_FA_Fornix_long
wDATA_all_FA_Fornix_export[[2]] <- wTABS_FA_Fornix
names(wDATA_all_FA_Fornix_export) <- c("DATA FIG 9a","TABLE FIG 9a")

openxlsx::write.xlsx(wDATA_all_FA_Fornix_export, file = "stka_NATURE_DATA_Figure_09a_20210409.xlsx")


wDATA_all_FA_Fornix_export <- list()
wDATA_all_FA_Fornix_export[[1]] <- DATA_all_FA_Fornix$wdDATA_diff_long
wDATA_all_FA_Fornix_export[[2]] <- wdTABS_FA_Fornix_diff
names(wDATA_all_FA_Fornix_export) <- c("DATA FIG 9b","TABLE FIG 9b")

openxlsx::write.xlsx(wDATA_all_FA_Fornix_export, file = "stka_NATURE_DATA_Figure_09b_20210409.xlsx")

################################################################################
## Export figures

setwd(dir_figs_final)

quartz(height = 4, width = 4)
ProfilePlotTreatment(wDATA_FA_Fornix_long,wTABS_FA_Fornix, ylim = range_y,
                     variable = "FA fornix")
pngname <- paste("Figure_suppl_9a_profile_","fa_fornix_20200814.png", sep = "")
quartz.save(pngname, type = "png", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)
pdfname <- paste("Figure_suppl_9a_profile_","fa_fornix_20200814.pdf", sep = "")
quartz.save(pdfname, type = "pdf", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)

quartz(height = 4, width = 4)
CiTreatment(wdTABS_FA_Fornix_diff, variable = "FA fornix, change from screening") #  bquote(bold(...))
pngname <- paste("Figure_suppl_9b_ci_","fa_fornix_20200814.png", sep = "")
quartz.save(pngname, type = "png", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)
pdfname <- paste("Figure_suppl_9b_ci_","fa_fornix_20200814.pdf", sep = "")
quartz.save(pdfname, type = "pdf", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)

##############################################################################
## DTI Mean diffusivity of the fornix (MD Fornix)
################################################################################
## Supplement FIGURE 9 c,d

file_MD_Fornix <- "20190827_02_ACAD003_IXICO_MRI_400002036.csv"
filename <- file_MD_Fornix 
filedir <- dir_dti

DATA_all_MD_Fornix <-  DataWranglingNaturePaper(filename, filedir, k_visits = 2, variable = "DTI")
wDATA_MD_Fornix_long <- DATA_all_MD_Fornix$wDATA_long
wTABS_MD_Fornix <- DATA_all_MD_Fornix$wTABS_diff
wdTABS_MD_Fornix_diff <- DATA_all_MD_Fornix$wdTABS_diff
wDATA_MD_Fornix_long_V01 <- DATA_all_MD_Fornix$wDATA_long_V01
range_y <- DATA_all_MD_Fornix$range_y

################################################################################
## Export data and tables

setwd(dir_data_final)

wDATA_all_MD_Fornix_export <- list()
wDATA_all_MD_Fornix_export[[1]] <- wDATA_MD_Fornix_long
wDATA_all_MD_Fornix_export[[2]] <- wTABS_MD_Fornix
names(wDATA_all_MD_Fornix_export) <- c("DATA FIG 9c","TABLE FIG 9c")

openxlsx::write.xlsx(wDATA_all_MD_Fornix_export, file = "stka_NATURE_DATA_Figure_09c_20210409.xlsx")


wDATA_all_MD_Fornix_export <- list()
wDATA_all_MD_Fornix_export[[1]] <- DATA_all_MD_Fornix$wdDATA_diff_long
wDATA_all_MD_Fornix_export[[2]] <- wdTABS_MD_Fornix_diff
names(wDATA_all_MD_Fornix_export) <- c("DATA FIG 9d","TABLE FIG 9d")

openxlsx::write.xlsx(wDATA_all_MD_Fornix_export, file = "stka_NATURE_DATA_Figure_09d_20210409.xlsx")

################################################################################
## Export figures

setwd(dir_figs_final)

quartz(height = 4, width = 4)
ProfilePlotTreatment(wDATA_MD_Fornix_long,wTABS_MD_Fornix, ylim = range_y,
                     variable = bquote(MD~fornix~(10^-3~mm^2/s)))
pngname <- paste("Figure_suppl_9c_profile_","md_fornix_20200814.png", sep = "")
quartz.save(pngname, type = "png", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)
pdfname <- paste("Figure_suppl_9c_profile_","md_fornix_20200814.pdf", sep = "")
quartz.save(pdfname, type = "pdf", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)

quartz(height = 4, width = 4)
CiTreatment(wdTABS_MD_Fornix_diff, variable = bquote(atop(MD~fornix~(10^-3~mm^2/s),"change from screening")))
pngname <- paste("Figure_suppl_9d_ci_","md_fornix_220200814.png", sep = "")
quartz.save(pngname, type = "png", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)
pdfname <- paste("Figure_suppl_9d_ci_","md_fornix_220200814.pdf", sep = "")
quartz.save(pdfname, type = "pdf", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)

##############################################################################
## DTI Fractional anisotropy of the genu of the corpus callosum (FA genu CC)
################################################################################
## Supplement FIGURE 9 e,f

file_FA_genu_CC <- "20190827_16_ACAD003_IXICO_MRI_400002047.csv"
filename <- file_FA_genu_CC 
filedir <- dir_dti

DATA_all_FA_genu_CC <-  DataWranglingNaturePaper(filename, filedir, k_visits = 2, variable = "DTI")
wDATA_FA_genu_CC_long <- DATA_all_FA_genu_CC$wDATA_long
wTABS_FA_genu_CC <- DATA_all_FA_genu_CC$wTABS_diff
wdTABS_FA_genu_CC_diff <- DATA_all_FA_genu_CC$wdTABS_diff
wDATA_FA_genu_CC_long_V01 <- DATA_all_FA_genu_CC$wDATA_long_V01
range_y <- DATA_all_FA_genu_CC$range_y

################################################################################
## Export data and tables

setwd(dir_data_final)

wDATA_all_FA_genu_CC_export <- list()
wDATA_all_FA_genu_CC_export[[1]] <- wDATA_FA_genu_CC_long
wDATA_all_FA_genu_CC_export[[2]] <- wTABS_FA_genu_CC
names(wDATA_all_FA_genu_CC_export) <- c("DATA FIG 9e","TABLE FIG 9e")

openxlsx::write.xlsx(wDATA_all_FA_genu_CC_export, file = "stka_NATURE_DATA_Figure_09e_20210409.xlsx")


wDATA_all_FA_genu_CC_export <- list()
wDATA_all_FA_genu_CC_export[[1]] <- DATA_all_FA_genu_CC$wdDATA_diff_long
wDATA_all_FA_genu_CC_export[[2]] <- wdTABS_FA_genu_CC_diff
names(wDATA_all_FA_genu_CC_export) <- c("DATA FIG 9f","TABLE FIG 9f")

openxlsx::write.xlsx(wDATA_all_FA_genu_CC_export, file = "stka_NATURE_DATA_Figure_09f_20210409.xlsx")

################################################################################
## Export figures

setwd(dir_figs_final)

quartz(height = 4, width = 4)
ProfilePlotTreatment(wDATA_FA_genu_CC_long,wTABS_FA_genu_CC, ylim = range_y,
                     variable = "FA genu CC")
pngname <- paste("Figure_suppl_9e_profile_","fa_genu_cc_20200814.png", sep = "")
quartz.save(pngname, type = "png", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)
pdfname <- paste("Figure_suppl_9e_profile_","fa_genu_cc_20200814.pdf", sep = "")
quartz.save(pdfname, type = "pdf", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)

quartz(height = 4, width = 4)
CiTreatment(wdTABS_FA_genu_CC_diff, variable = "FA genu CC, change from screening")
pngname <- paste("Figure_suppl_9f_ci_","fa_genu_cc_20200814.png", sep = "")
quartz.save(pngname, type = "png", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)
pdfname <- paste("Figure_suppl_9f_ci_","fa_genu_cc_20200814.pdf", sep = "")
quartz.save(pdfname, type = "pdf", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)

##############################################################################
## DTI Mean diffusivity of the genu of the corpus callosum (MD genu CC)
################################################################################
## Supplement FIGURE 9 g,h

file_MD_genu_CC <- "20190827_14_ACAD003_IXICO_MRI_400002010.csv"
filename <- file_MD_genu_CC 
filedir <- dir_dti

DATA_all_MD_genu_CC <-  DataWranglingNaturePaper(filename, filedir, k_visits = 2, variable = "DTI")
wDATA_MD_genu_CC_long <- DATA_all_MD_genu_CC$wDATA_long
wTABS_MD_genu_CC <- DATA_all_MD_genu_CC$wTABS_diff
wdTABS_MD_genu_CC_diff <- DATA_all_MD_genu_CC$wdTABS_diff
wDATA_MD_genu_CC_long_V01 <- DATA_all_MD_genu_CC$wDATA_long_V01
range_y <- DATA_all_MD_genu_CC$range_y

################################################################################
## Export data and tables

setwd(dir_data_final)

wDATA_all_MD_genu_CC_export <- list()
wDATA_all_MD_genu_CC_export[[1]] <- wDATA_MD_genu_CC_long
wDATA_all_MD_genu_CC_export[[2]] <- wTABS_MD_genu_CC
names(wDATA_all_MD_genu_CC_export) <- c("DATA FIG 9g","TABLE FIG 9g")

openxlsx::write.xlsx(wDATA_all_MD_genu_CC_export, file = "stka_NATURE_DATA_Figure_09g_20210409.xlsx")


wDATA_all_MD_genu_CC_export <- list()
wDATA_all_MD_genu_CC_export[[1]] <- DATA_all_MD_genu_CC$wdDATA_diff_long
wDATA_all_MD_genu_CC_export[[2]] <- wdTABS_MD_genu_CC_diff
names(wDATA_all_MD_genu_CC_export) <- c("DATA FIG 9h","TABLE FIG 9h")

openxlsx::write.xlsx(wDATA_all_MD_genu_CC_export, file = "stka_NATURE_DATA_Figure_09h_20210409.xlsx")

################################################################################
## Export figures

setwd(dir_figs_final)

quartz(height = 4, width = 4)
ProfilePlotTreatment(wDATA_MD_genu_CC_long,wTABS_MD_genu_CC, ylim = range_y, 
                     variable = bquote(MD~genu~CC~(10^-3~mm^2/s)))
pngname <- paste("Figure_suppl_9g_profile_","md_genu_cc_20200814.png", sep = "")
quartz.save(pngname, type = "png", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)
pdfname <- paste("Figure_suppl_9g_profile_","md_genu_cc_20200814.pdf", sep = "")
quartz.save(pdfname, type = "pdf", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)

quartz(height = 4, width = 4)
CiTreatment(wdTABS_MD_genu_CC_diff, variable = bquote(atop(MD~genu~CC~(10^-3~mm^2/s),"change from screening")))
pngname <- paste("Figure_suppl_9h_ci_","md_genu_cc_20200814.png", sep = "")
quartz.save(pngname, type = "png", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)
pdfname <- paste("Figure_suppl_9h_ci_","md_genu_cc_20200814.pdf", sep = "")
quartz.save(pdfname, type = "pdf", width = 4, height = 4, family = "Helvetica", bg = "white", dpi = 1200)

################################################################################
################################################################################
## ... Winsorisation TRUE as default !!!

"MeanNA" <- function(x, winsorisation = TRUE) {
  if (winsorisation == TRUE) x <- Winsorisation(x)
  mean(x, na.rm = TRUE)
}

"SdNA" <- function(x, winsorisation = TRUE) {
  if (winsorisation == TRUE) x <- Winsorisation(x)
  sd(x, na.rm = TRUE)
}

"LengthNA" <- function(x) length(na.omit(x))
"CiLower"  <- function(x) MeanNA(x) - qt(0.975,length(na.omit(x)) - 1)*SdNA(x)/sqrt(length(na.omit(x)))
"CiUpper"  <- function(x) MeanNA(x) + qt(0.975,length(na.omit(x)) - 1)*SdNA(x)/sqrt(length(na.omit(x)))

"GeomMean" <- function(x) {
  x <- na.omit(x)
  n <- length(x)
  GM <- exp(mean(log(x)))
  return(GM)
}

"GeomSd" <- function(x) {
  x <- na.omit(x)
  GSD <- exp(sd(log(x)))
  return(GSD)
}

"ConfIntForGeomMeanLB" <- function(x){
  GM <- GeomMean(x)
  GSD <- GeomSd(x)
  x <- na.omit(x) # odstranenie NA
  n <- length(x)
  LB <- GM/GSD^(qt(0.975,n - 1)/sqrt(n))
  UB <- GM*GSD^(qt(0.975,n - 1)/sqrt(n))
  TAB <- c(n,GM,GSD,LB,UB)
  names(TAB) <- c("n","GM","GSD","LB","UB")
  return(LB)
}

"ConfIntForGeomMeanUB" <- function(x){
  GM <- GeomMean(x)
  GSD <- GeomSd(x)
  x <- na.omit(x) # odstranenie NA
  n <- length(x)
  LB <- GM/GSD^(qt(0.975,n - 1)/sqrt(n))
  UB <- GM*GSD^(qt(0.975,n - 1)/sqrt(n))
  TAB <- c(n,GM,GSD,LB,UB)
  names(TAB) <- c("n","GM","GSD","LB","UB")
  return(UB)
}

################################################################################

"GGboxplotDiagnoses" <- function(DATA, xlim = x_range, ylim = range_y, 
                                 x_labels = visits,
                                 variable = "...", caption_text = "", 
                                 family = "Helvetica",  type = "IgG aP108 (serum)",
                                 box_colors = box_colors, bar_colors = bar_colors,
                                 yintercept = 100) {
  if (type == "IgG aP108 (serum)") breaks <- c(100,500,1000,5000,10000,50000,100000,240000)
  if (type == "IgG Qp108 (serum)") breaks <- c(300,500,1000,2000,4000,10000,20000,40000)
  if (type == "DK") breaks <- c(0.01,0.04,0.1,0.2,0.4,1.0,2.0,4.0)
  gg <- ggplot(DATA, aes(visit,result))
  gg +
    theme(plot.margin = margin(0,4,5,4)) + 
    geom_hline(yintercept = yintercept, linetype = "solid", color = "gray70") + 
    geom_boxplot(aes(x = visit, y = result, colour = visit, fill = visit), 
                 alpha = 0.1, outlier.shape = NA, fatten = 1) +
    #scale_y_continuous(trans = 'log', breaks = breaks, labels = number(breaks, digits = 0)) +
    scale_y_continuous(trans = 'log', breaks = breaks, labels = format(breaks, big.mark = ",", scientific = FALSE)) +
    scale_x_discrete(labels = x_labels) +
    # theme_light() +
    labs(title = caption_text, x = "Weeks", y = variable) +
    theme(text = element_text(family = family),
          axis.title = element_text(face = "plain", size = 15),
          axis.text = element_text(size = 11),
          legend.position = "none") + # plot.title = element_text(face = "bold", size = 14, hjust = 0)
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black")) + 
    coord_cartesian(xlim = xlim, ylim = ylim) +
    geom_jitter(aes(x = visit, y = result, colour = visit, fill = visit), 
                position = position_jitter(width = 0.02, height = 0), 
                size = 0.8, alpha = 0.2, color = "gray45") + 
    stat_summary(fun.min = ConfIntForGeomMeanLB, fun.max = ConfIntForGeomMeanUB,
                 aes(x = visit, y = result), geom = "errorbar", col = bar_colors[-1], width = 0.17, size = 0.8) + # width = 0.17,  size = 0.6
    stat_summary(fun = GeomMean, aes(x = visit, y = result), geom = "point", col = bar_colors[-1], size = 1.3) +
    scale_fill_manual(values = box_colors) +
    scale_color_manual(values = box_colors)
}

################################################################################

"GGboxplotDiagnosis" <- function(DATA, ylim = range_y, variable = "...",
                                 caption_text = "", family = "Helvetica") {
  #DATA <- na.omit(DATA)
  gg <- ggplot(DATA, aes(treatment,result, fill = treatment))
  gg +
    theme(plot.margin = margin(0,4,5,4)) + 
    geom_boxplot(aes(x = treatment, y = result, colour = treatment, fill = treatment), 
                 alpha = 0.3, outlier.shape = NA) +
    scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
    # theme_light() +
    labs(title = caption_text, x = "", y = variable) + 
    theme(text = element_text(family = family),
          axis.title = element_text(face = "plain", size = 15),
          axis.text.x = element_text(face = "plain", size = 15),
          axis.text.y = element_text(size = 11),
          legend.position = "none",
          plot.title = element_text(face = "bold", size = 14, hjust = 0)) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black")) + 
    coord_cartesian(ylim = ylim) +
    geom_jitter(aes(x = treatment, y = result), position = position_jitter(width = 0.03, height = 0), size = 0.8, alpha = 0.5, color = "gray45") + 
    stat_summary(fun.min = CiLower, fun.max = CiUpper,
                 aes(x = treatment, y = result), colour = "black", geom = "errorbar", width = 0.07, size = 1) + 
    stat_summary(fun = MeanNA, aes(x = treatment, y = result),  colour =  "black", fill =  "black", geom = "point", size = 2) + #col = c("blue","red"), shape = c(16,16), size = 2.5
    #theme(legend.position = "right", legend.direction = "vertical") +
    scale_fill_manual(values = c("blue","red")) + 
    scale_color_manual(values = c("blue","red"))
}

################################################################################

"ProfilePlotTreatment" <- function(DATA_long,TABLE, xlim = c(1.1,1.5), ylim = range_y, variable = "...",  
                                   caption_text = "", family = "Helvetica") {
  ggplot() + 
    theme(plot.margin = margin(0,4,5,4)) + 
    geom_point(data = DATA_long, aes(x = visit, y = result, color = treatment), size = 0.6) + 
    geom_line(data = DATA_long, aes(x = visit, y = result, group = SUBJID,
                                    color = treatment, linetype = treatment),
              size = 0.2, linetype = "dashed") +
    geom_point(data = TABLE, aes(x = visit, y = mean, color = treatment), size = 1.8) +
    geom_line(data = TABLE, aes(x = visit, y = mean, group = treatment,
                                color = treatment, linetype = treatment),
              size = 1.4, linetype = "solid") +
    scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
    #theme_light() +
    labs(title = "", x = "Weeks", y = variable) +
    theme(text = element_text(family = family),
          axis.title = element_text(face = "plain", size = 15),
          axis.text = element_text(size = 11)) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black")) + 
    coord_cartesian(xlim = xlim, ylim = ylim) +
    theme(legend.direction = "vertical",
          legend.title = element_blank(),
          legend.position = c(0,1),
          legend.justification = c("left", "top"),
          legend.background = element_rect(fill = NA, colour = NA),
          legend.key = element_rect(fill = NA, colour = NA)) +
    scale_fill_manual(values = c( "blue","red")) +
    scale_color_manual(values = c( "blue","red"))
}

################################################################################

 "CiTreatment" <- function(TABLE, xlim = c(1.3,1.6), bar_width = 0.1, variable = "...", family = "Helvetica",
                           legend_position = c(0,1), legend_justification = c("left", "top")) {
  ymin <- min(TABLE$lower)  
  ymax <- max(TABLE$upper)  
  ggplot() + 
    theme(plot.margin = margin(0,4,5,4)) + 
    geom_point(data = TABLE, aes(visit, mean, color = treatment), size = 2,
               position = position_dodge(width = 0.2)) +
    geom_errorbar(data = TABLE, aes(x = visit, ymax = upper, ymin = lower, color = treatment),
                  width = bar_width, size = 0.6, linetype = 1,
                  position = position_dodge(width = 0.2)) +
    geom_line(data = TABLE, aes(x = visit, y = mean, group = treatment, color = treatment),
              size = 0.4, linetype = "dashed",
              position = position_dodge(width = 0.2)) +
    # theme_light() +
    labs(title = "", x = "Weeks", y = variable) +
    theme(text = element_text(family = family),
          axis.title = element_text(face = "plain", size = 15),
          axis.text = element_text(size = 11)) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black")) + 
    coord_cartesian(xlim = xlim, ylim = c(ymin, ymax)) +
    theme(legend.direction = "vertical",
          legend.title = element_blank(),
          legend.position = legend_position,
          legend.justification = legend_justification,
          legend.background = element_rect(fill = NA, colour = NA),
          legend.key = element_rect(fill = NA, colour = NA)) +
    scale_fill_manual(values = c( "blue","red")) +
    scale_color_manual(values = c( "blue","red"))
}

################################################################################

"DataWranglingNaturePaper" <- function(filename, filedir, k_visits = 4, variable = "CSF") {
  
  ## Allocation file
  wd <- "~/Documents/SCIENCE/NEURO-STAT/AXON-STAT/ADAMANT/DATA-RAW/UNBLINDED_DATA/"
  setwd(wd)
  
  SUBJECT_ALLOCATION <- import("AC-AD-003_SUBJECT_ALLOCATION_20190807.xlsx")
  FASa <- SUBJECT_ALLOCATION$FAS
  FAS <- rep(FALSE,208)
  FAS[which(FASa == 1)] <- TRUE
  TRTG <- SUBJECT_ALLOCATION$TRTG
  TRTG <- as.factor(TRTG)
  
  setwd(filedir)
  
  DATA           <- read_delim(filename, delim = "|")
  
  if (variable == "CSF") {
    
    ID_V01 <- !is.na(DATA$V01)
    DATA$V02[ID_V01] <- DATA$V01[ID_V01]
    ID_V14A <- !is.na(DATA$V14A)
    DATA$V11[ID_V14A] <- DATA$V14A[ID_V14A]
    ID_V15 <- !is.na(DATA$V15)
    DATA$V16[ID_V15] <- DATA$V15[ID_V15]
    
  }
  
  if (k_visits == 2 & variable != "DTI") DATA <- select(DATA,V02,V16)
  if (k_visits == 2 & variable == "DTI") DATA <- select(DATA,V01,V16)
  if (k_visits == 4 & variable != "DK") DATA <- select(DATA,V02,V08,V11,V16)
  if (k_visits == 4 & variable == "DK") {
    DATA <- select(DATA,V08,V11,V14,V16)
    DATA <- 1/DATA*10^9
  }
  if (k_visits == 18 & variable == "titre") DATA <- select(DATA,V02:V16)
  DATA           <- add_column(DATA, 
                               SUBJID = SUBJECT_ALLOCATION[,"SUBJID"],
                               TRTG = factor(SUBJECT_ALLOCATION[,"TRTG"]),
                               FAS = FAS,
                               .before = TRUE)
  #if (variable != "titre" & variable != "DK") DATA <- na.omit(DATA)
  if (variable == "CSF" | variable == "DTI") DATA <- na.omit(DATA)
  
  ## original data wide 
  DATA           <- filter(DATA,FAS == TRUE)
  if (variable == "titre" | variable == "DK")  DATA <- filter(DATA, TRTG == "AADvac1")
  
  ## winsorised data wide 
  if (variable != "titre" | variable != "DK") {
    wDATA <- DATA
    if (variable != "DTI") wDATA$V02 <- WinsorisationALL(DATA$V02, DATA$TRTG)
    if (k_visits == 2 & variable == "DTI") wDATA$V01 <- WinsorisationALL(DATA$V01, DATA$TRTG)
    if (k_visits == 4) wDATA$V08 <- WinsorisationALL(DATA$V08, DATA$TRTG)
    if (k_visits == 4) wDATA$V11 <- WinsorisationALL(DATA$V11, DATA$TRTG)
    wDATA$V16 <- WinsorisationALL(DATA$V16, DATA$TRTG)
  }
  
  ## original data long 
  nrow_data      <- nrow(DATA)
  SUBJID_long    <- rep(DATA$SUBJID,k_visits)
  TRTG_long      <- rep(DATA$TRTG,k_visits)
  if (k_visits == 2 & variable != "DTI") result_long <- c(DATA$V02, DATA$V16)
  if (k_visits == 2 & variable == "DTI") result_long <- c(DATA$V01, DATA$V16)
  if (k_visits == 4 & variable != "DK") result_long <- c(DATA$V02, DATA$V08, DATA$V11, DATA$V16)
  if (k_visits == 4 & variable == "DK") result_long <- c(DATA$V08, DATA$V11, DATA$V14, DATA$V16)
  if (k_visits == 18 & variable == "titre")  {
    result_long <- c(DATA$V02,DATA$V03, DATA$V04, DATA$V05, DATA$V06, DATA$V07, DATA$V08, 
                     DATA$V08A, DATA$V09, DATA$V10, DATA$V11, DATA$V11A, DATA$V12,
                     DATA$V13, DATA$V14, DATA$V14A, DATA$V15, DATA$V16)
  }
  if (k_visits == 2 & variable != "DTI") visit_long <- rep(c("0","104"), each = nrow_data)
  if (k_visits == 2 & variable == "DTI") visit_long <- rep(c("-5","104"), each = nrow_data)
  if (k_visits == 4 & variable != "DK") visit_long <- rep(c("0","24","52","104"), each = nrow_data)
  if (k_visits == 4 & variable == "DK") visit_long <- rep(c("24","52","80","104"), each = nrow_data)
  if (k_visits == 18 & variable == "titre") visit_long <- rep(c("0","4","8","12","16","20","24","34","38","48","52","62","66","76","80","90","94","104"), each = nrow_data)
  DATA_long <- cbind.data.frame(SUBJID = SUBJID_long,
                                treatment = TRTG_long,
                                visit = visit_long,
                                result = result_long)
  DATA_long <- as_tibble(DATA_long)
  
  ## winsorised data long 
  if (variable != "titre" & variable != "DK") {
    
    nrow_data      <- nrow(wDATA)
    SUBJID_long    <- rep(wDATA$SUBJID,k_visits)
    TRTG_long      <- rep(wDATA$TRTG,k_visits)
    if (k_visits == 2 & variable != "DTI") result_long <- c(wDATA$V02, wDATA$V16)
    if (k_visits == 2 & variable == "DTI") result_long <- c(wDATA$V01, wDATA$V16)
    if (k_visits == 4) result_long <- c(wDATA$V02, wDATA$V08, wDATA$V11, wDATA$V16)
    if (k_visits == 2 & variable != "DTI") visit_long <- rep(c("0","104"), each = nrow_data)
    if (k_visits == 2 & variable == "DTI") visit_long <- rep(c("-5","104"), each = nrow_data)
    if (k_visits == 4) visit_long <- rep(c("0","24","52","104"), each = nrow_data)
    wDATA_long     <- cbind.data.frame(SUBJID = SUBJID_long,
                                       treatment = TRTG_long,
                                       visit = visit_long,
                                       result = result_long)
    wDATA_long     <- as_tibble(wDATA_long)
    if (variable != "DTI") wDATA_long_V02 <- filter(wDATA_long, visit == "0")
    if (k_visits == 2 & variable == "DTI") wDATA_long_V01 <- filter(wDATA_long, visit == "-5")
    range_y        <- range(wDATA_long$result, na.rm = TRUE)
    
    ## original data difference from baseline/screening wide 
    DATA_diff      <- DATA
    if (variable != "DTI") DATA_diff$V02  <- DATA$V02 - DATA$V02
    if (variable == "DTI") DATA_diff$V01  <- DATA$V01 - DATA$V01
    if (k_visits == 4) DATA_diff$V08 <- DATA$V08 - DATA$V02
    if (k_visits == 4) DATA_diff$V11 <- DATA$V11 - DATA$V02
    if (variable != "DTI") DATA_diff$V16  <- DATA$V16 - DATA$V02
    if (variable == "DTI") DATA_diff$V16  <- DATA$V16 - DATA$V01
    
    nrow_data      <- nrow(DATA_diff)
    SUBJID_long    <- rep(DATA_diff$SUBJID,k_visits)
    TRTG_long      <- rep(DATA_diff$TRTG,k_visits)
    if (k_visits == 2 & variable != "DTI") result_long <- c(DATA_diff$V02, DATA_diff$V16)
    if (k_visits == 2 & variable == "DTI") result_long <- c(DATA_diff$V01, DATA_diff$V16)
    if (k_visits == 4) result_long <- c(DATA_diff$V02, DATA_diff$V08,DATA_diff$V11, DATA_diff$V16)
    if (k_visits == 2 & variable != "DTI") visit_long <- rep(c("0","104"), each = nrow_data)
    if (k_visits == 2 & variable == "DTI") visit_long <- rep(c("-5","104"), each = nrow_data)
    if (k_visits == 4) visit_long <- rep(c("0","24","52","104"), each = nrow_data)
    DATA_diff_long <- cbind.data.frame(SUBJID = SUBJID_long,
                                       treatment = TRTG_long,
                                       visit = visit_long,
                                       result = result_long)
    DATA_diff_long <- as_tibble(DATA_diff_long)
    
    ## winsorised data difference from V02 wide 
    DATA_diff      <- DATA
    if (variable != "DTI") DATA_diff$V02  <- DATA$V02 - DATA$V02
    if (variable == "DTI") DATA_diff$V01  <- DATA$V01 - DATA$V01
    if (k_visits == 4) DATA_diff$V08 <- DATA$V08 - DATA$V02
    if (k_visits == 4) DATA_diff$V11 <- DATA$V11 - DATA$V02
    if (variable != "DTI") DATA_diff$V16  <- DATA$V16 - DATA$V02
    if (variable == "DTI") DATA_diff$V16  <- DATA$V16 - DATA$V01
    
    wdDATA_diff     <- DATA_diff
    if (k_visits == 2 & variable != "DTI") wdDATA_diff$V02 <- WinsorisationALL(DATA_diff$V02, DATA_diff$TRTG)
    if (k_visits == 2 & variable == "DTI") wdDATA_diff$V01 <- WinsorisationALL(DATA_diff$V01, DATA_diff$TRTG)
    if (k_visits == 4) wdDATA_diff$V08 <- WinsorisationALL(DATA_diff$V08, DATA_diff$TRTG)
    if (k_visits == 4) wdDATA_diff$V11 <- WinsorisationALL(DATA_diff$V11, DATA_diff$TRTG)
    wdDATA_diff$V16 <- WinsorisationALL(DATA_diff$V16, DATA_diff$TRTG)
    
    nrow_wDATA    <- nrow(wdDATA_diff)
    SUBJID_long   <- rep(wdDATA_diff$SUBJID,k_visits)
    TRTG_long     <- rep(wdDATA_diff$TRTG,k_visits)
    if (k_visits == 2 & variable != "DTI") result_long <- c(wdDATA_diff$V02, wdDATA_diff$V16)
    if (k_visits == 2 & variable == "DTI") result_long <- c(wdDATA_diff$V01, wdDATA_diff$V16)
    if (k_visits == 4) result_long <- c(wdDATA_diff$V02, wdDATA_diff$V08, wdDATA_diff$V11, wdDATA_diff$V16)
    if (k_visits == 2 & variable != "DTI") visit_long <- rep(c("0","104"), each = nrow_wDATA)
    if (k_visits == 2 & variable == "DTI") visit_long <- rep(c("-5","104"), each = nrow_wDATA)
    if (k_visits == 4) visit_long <- rep(c("0","24","52","104"), each = nrow_wDATA)
    wdDATA_diff_long <- cbind.data.frame(SUBJID = SUBJID_long,
                                         treatment = TRTG_long,
                                         visit = visit_long,
                                         result = result_long)
    wdDATA_diff_long <- as_tibble(wdDATA_diff_long)
    
    if (k_visits == 4) {
      wDATA_long$visit <- factor(wDATA_long$visit, levels = c("0","24","52","104"), labels = c("0","24","52","104"))
      wdDATA_diff_long$visit <- factor(wdDATA_diff_long$visit, levels = c("0","24","52","104"), labels = c("0","24","52","104"))
    }
    if (k_visits == 2 & variable != "DTI") {
      wDATA_long$visit <- factor(wDATA_long$visit, levels = c("0","104"), labels = c("0","104"))
      wdDATA_diff_long$visit <- factor(wdDATA_diff_long$visit, levels = c("0","104"), labels = c("0","104"))
    }
    if (k_visits == 2 & variable == "DTI") {
      wDATA_long$visit <- factor(wDATA_long$visit, levels = c("-5","104"), labels = c("-5","104"))
      wdDATA_diff_long$visit <- factor(wdDATA_diff_long$visit, levels = c("-5","104"), labels = c("-5","104"))
    }
    ## table from winsorised data difference from V02 
    wTABS_diff <- SummariseBasicChars(wDATA_long)
    wdTABS_diff <- SummariseBasicChars(wdDATA_diff_long)
    
  }
  
  if (variable == "titre")  {
    DATA_long$visit <- factor(DATA_long$visit, 
                              levels = c("0","4","8","12","16","20","24","34","38","48","52","62","66","76","80","90","94","104"),
                              labels = c("0","4","8","12","16","20","24","34","38","48","52","62","66","76","80","90","94","104"))
    TABS <- SummariseBasicCharsGeom(DATA_long)
    range_y <- range(DATA_long$result, na.rm = TRUE)
  }
  
  if (variable == "DK")  {
    DATA_long$visit <- factor(DATA_long$visit, 
                              levels = c("24","52","80","104"),
                              labels = c("24","52","80","104"))
    TABS <- SummariseBasicCharsGeom(DATA_long)
    range_y <- range(DATA_long$result, na.rm = TRUE)
  }
  
  RESULTS <- list()
  RESULTS$DATA_long <- DATA_long
  
  if (variable == "titre" | variable == "DK") RESULTS$TABS <- TABS
  if (variable != "titre" &  variable != "DK") {
    RESULTS$wDATA_long <- wDATA_long
    if (variable != "DTI") RESULTS$wDATA_long_V02 <- wDATA_long_V02
    if (k_visits == 2 & variable == "DTI") RESULTS$wDATA_long_V01 <- wDATA_long_V01
    RESULTS$wdDATA_diff_long <- wdDATA_diff_long
    RESULTS$wTABS_diff <- wTABS_diff
    RESULTS$wdTABS_diff <- wdTABS_diff
  }
  
  RESULTS$range_y <- range_y
  return(RESULTS)
  
}

################################################################################

"SummariseBasicChars" <- function(DATA_long) {
  
  TABLE <- DATA_long %>% group_by(treatment, visit) %>% 
    summarise(n = LengthNA(result),
              mean = MeanNA(result),
              sd = SdNA(result),
              lower = CiLower(result),
              upper = CiUpper(result))
  return(TABLE)
  
}

################################################################################

"SummariseBasicCharsGeom" <- function(DATA_long) {
  
  TABLE <- DATA_long %>% group_by(visit) %>% 
    summarise(n = LengthNA(result),
              mean = GeomMean(result),
              sd = GeomSd(result),
              lower = ConfIntForGeomMeanLB(result),
              upper = ConfIntForGeomMeanUB(result))
  return(TABLE)
  
}

################################################################################

"Winsorisation" <- function(x, type = "Tukey"){
  xw.fin <- x
  n <- length(x)
  ID <- (1:n)[!is.na(x)]
  if (length(ID) == 0) return(x) 
  if (length(ID) > 0) {
    x <- na.omit(x) # odstranenie NA
    if (type == "Tukey") {
      q1 <- quantile(x,0.25)
      q3 <- quantile(x,0.75)
      Dq <- q3 - q1
      min.x <- q1 - 1.5*Dq
      max.x <- q3 + 1.5*Dq
    }
    if (type == "quantile") { 
      min.x <- quantile(x,0.025)
      max.x <- quantile(x,0.975)
    }
    xw <- x
    for (i in 1:length(x)) if (x[i] >= max.x) xw[i] <- max.x
    for (i in 1:length(x)) if (x[i] <= min.x) xw[i] <- min.x
    if (length(ID) != n) xw.fin[ID] <- xw
    if (length(ID) == n) xw.fin <- xw
    return(xw.fin)
  }
}

################################################################################

"WinsorisationALL" <- function(effect, groups, type = "Tukey") {
  groups_levels <- levels(groups)
  k_groups      <- length(groups_levels)
  weffect       <- effect
  for (j in 1:k_groups) {
    IDgroup_j   <- which(groups == groups_levels[j])
    effect_ji   <- effect[IDgroup_j]
    k_ji        <- length(effect_ji)
    if (k_ji > 5) {
      weffect[IDgroup_j] <- Winsorisation(effect_ji, type = type)
    }
  }
  return(weffect)
}

################################################################################
################################################################################