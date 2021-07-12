
##############################################################################
##############################################################################
################################## BPF data ##################################

# data points from figure of the 1983 book, extracted with WebPlotDigitizer
BPF_pop <- fread("../data/BongaartsPotter/extracted_table.csv")

BPF_pop[, source:="BP"]

BPF_pop[, residence:=NA_character_]
BPF_pop[(grepl("rural", note)), residence:="rural"]
BPF_pop[(grepl("urban", note)), residence:="urban"]
BPF_pop[(grepl("national", note)), residence:="all"]




