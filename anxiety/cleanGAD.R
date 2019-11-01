### This script identifies bblids that violate the skip logic of the GOASSESS battery
### 
### Ellyn Butler
### March 14, 2019 - present

# load packages 
#library('car')
library('dplyr')

# Load the data 
#df <- read.csv("/home/butellyn/parentchild_psychopathology/data/GOA_itemwise_for_reporter_agreement_noPHI.csv") 
#demo <- read.csv("/home/butellyn/age_prediction/data/n9498_demographics_go1_20161212.csv")

# Check that answers make sense for Middle Proband
#gadcols <- colnames(df)[grepl("GAD", colnames(df))][c(1:8, 10:12, 14:21, 24, 26:28, 13, 9)]
#gad_df <- df[df$INTERVIEW_TYPE == "MP",c("PROBAND_BBLID", gadcols)]
#names(gad_df)[names(gad_df) == 'PROBAND_BBLID'] <- 'bblid'

#rownames(gad_df) <- 1:nrow(gad_df)

	
##### Remove BBLIDs from dataframe that are non-sensical, or modify their answers, and then remove subjects that still have missing values

identifyBadGAD <- function(gad_df) {
	gadcols <- colnames(gad_df)[grepl("GAD", colnames(gad_df))]
	for (gadcol in gadcols) { gad_df[,gadcol] <- as.character(gad_df[,gadcol]) }
	gad_df$Mistake <- 0
	for (i in 1:nrow(gad_df)) {
		# Took the super short version?
		if (gad_df[i, "GAD060"] != ".") { 
			gad_df[i, "Mistake"] <- 1 
			next
		}
		worrytypes <- gad_df[i, grep("GAD003", colnames(gad_df), value=TRUE)]
		firstvec <- gad_df[i, gadcols[9:23]]
		if ((gad_df[i, "GAD001"] != "1" & gad_df[i, "GAD002"] != "1" | length(worrytypes[worrytypes == "1"]) < 2) & ("1" %in% firstvec | "0" %in% firstvec)) { 
			gad_df[i, "Mistake"] <- 1 
			next
		}
		secondvec <- gad_df[i, gadcols[11:23]]
		if (!("1" %in% gad_df[i, c("GAD011", "GAD011A", "GAD011B")]) & ("1" %in% secondvec | "0" %in% secondvec)) {
			gad_df[i, "Mistake"] <- 1 
			next
		}
		thirdvec <- gad_df[i, gadcols[13:18]]
		fourthvec <- gad_df[i, gadcols[19:23]]
		if (!("1" %in% thirdvec) & ("1" %in% fourthvec | "0" %in% fourthvec)) {
			gad_df[i, "Mistake"] <- 1 
			next
		}
		fifthvec <- gad_df[i, gadcols[21:23]]
		if (gad_df[i, "GAD024"] == '.') { gad_df[i, "GAD024"] <- "0" }
		if (gad_df[i, "GAD025"] == '.') { gad_df[i, "GAD025"] <- "0" }
		if ((gad_df[i, "GAD021"] != "1" | as.numeric(gad_df[i, "GAD024"]) < 6) & ("1" %in% fifthvec | "0" %in% fifthvec)) {
			if (as.numeric(gad_df[i, "GAD025"]) < .5) {
				gad_df[i, "Mistake"] <- 1 
			}
			next
		}
		
	}
	return(gad_df)
}

# Remove mistakes between these two

fillInGAD <- function(gad_df) {
	#gad_df$GAD001 <- recode(gad_df$GAD001, "'.'='0';'9'='0'")
	gad_df[gad_df$GAD001 == '.', 'GAD001'] <- '0'
	gad_df[gad_df$GAD001 == '9', 'GAD001'] <- '0'
	#gad_df$GAD002 <- recode(gad_df$GAD002, "'.'='0';'9'='0'")
	gad_df[gad_df$GAD002 == '.', 'GAD002'] <- '0'
	gad_df[gad_df$GAD002 == '9', 'GAD002'] <- '0'
	#gad_df$GAD003A <- recode(gad_df$GAD003A, "'.'='0';'9'='0'")
	gad_df[gad_df$GAD003A == '.', 'GAD003A'] <- '0'
	gad_df[gad_df$GAD003A == '9', 'GAD003A'] <- '0'
	#gad_df$GAD003B <- recode(gad_df$GAD003B, "'.'='0';'9'='0'")
	gad_df[gad_df$GAD003B == '.', 'GAD003B'] <- '0'
	gad_df[gad_df$GAD003B == '9', 'GAD003B'] <- '0'
	#gad_df$GAD003C <- recode(gad_df$GAD003C, "'.'='0';'9'='0'")
	gad_df[gad_df$GAD003C == '.', 'GAD003C'] <- '0'
	gad_df[gad_df$GAD003C == '9', 'GAD003C'] <- '0'
	#gad_df$GAD003D <- recode(gad_df$GAD003D, "'.'='0';'9'='0'")
	gad_df[gad_df$GAD003D == '.', 'GAD003D'] <- '0'
	gad_df[gad_df$GAD003D == '9', 'GAD003D'] <- '0'
	#gad_df$GAD003E <- recode(gad_df$GAD003E, "'.'='0';'9'='0'")
	gad_df[gad_df$GAD003E == '.', 'GAD003E'] <- '0'
	gad_df[gad_df$GAD003E == '9', 'GAD003E'] <- '0'
	#gad_df$GAD003F <- recode(gad_df$GAD003F, "'.'='0';'9'='0'")
	gad_df[gad_df$GAD003F == '.', 'GAD003F'] <- '0'
	gad_df[gad_df$GAD003F == '9', 'GAD003F'] <- '0'
	#gad_df$GAD012 <- recode(gad_df$GAD012, "'.'='0';'9'='0'")
	gad_df[gad_df$GAD012 == '.', 'GAD012'] <- '0'
	gad_df[gad_df$GAD012 == '9', 'GAD012'] <- '0'
	#gad_df$GAD013 <- recode(gad_df$GAD013, "'.'='0';'9'='0'")
	gad_df[gad_df$GAD013 == '.', 'GAD013'] <- '0'
	gad_df[gad_df$GAD013 == '9', 'GAD013'] <- '0'
	#gad_df$GAD015 <- recode(gad_df$GAD015, "'.'='0';'9'='0'")
	gad_df[gad_df$GAD015 == '.', 'GAD015'] <- '0'
	gad_df[gad_df$GAD015 == '9', 'GAD015'] <- '0'
	#gad_df$GAD016 <- recode(gad_df$GAD016, "'.'='0';'9'='0'")
	gad_df[gad_df$GAD016 == '.', 'GAD016'] <- '0'
	gad_df[gad_df$GAD016 == '9', 'GAD016'] <- '0'
	#gad_df$GAD017 <- recode(gad_df$GAD017, "'.'='0';'9'='0'")
	gad_df[gad_df$GAD017 == '.', 'GAD017'] <- '0'
	gad_df[gad_df$GAD017 == '9', 'GAD017'] <- '0'
	#gad_df$GAD018 <- recode(gad_df$GAD018, "'.'='0';'9'='0'")
	gad_df[gad_df$GAD018 == '.', 'GAD018'] <- '0'
	gad_df[gad_df$GAD018 == '9', 'GAD018'] <- '0'
	#gad_df$GAD019 <- recode(gad_df$GAD019, "'.'='0';'9'='0'")
	gad_df[gad_df$GAD019 == '.', 'GAD019'] <- '0'
	gad_df[gad_df$GAD019 == '9', 'GAD019'] <- '0'
	#gad_df$GAD020 <- recode(gad_df$GAD020, "'.'='0';'9'='0'")
	gad_df[gad_df$GAD020 == '.', 'GAD020'] <- '0'
	gad_df[gad_df$GAD020 == '9', 'GAD020'] <- '0'
	#gad_df$GAD021 <- recode(gad_df$GAD021, "'.'='0';'9'='0'")
	gad_df[gad_df$GAD021 == '.', 'GAD021'] <- '0'
	gad_df[gad_df$GAD021 == '9', 'GAD021'] <- '0'
	#gad_df$GAD024 <- recode(gad_df$GAD024, "'.'='0'")
	gad_df[gad_df$GAD024 == '.', 'GAD024'] <- '0'
	#gad_df$GAD039 <- recode(gad_df$GAD039, "'.'='0';'9'='0'")
	gad_df[gad_df$GAD039 == '.', 'GAD039'] <- '0'
	gad_df[gad_df$GAD039 == '9', 'GAD039'] <- '0'
	#gad_df$GAD040 <- recode(gad_df$GAD040, "'.'='0';'9'='0'")
	gad_df[gad_df$GAD040 == '.', 'GAD040'] <- '0'
	gad_df[gad_df$GAD040 == '9', 'GAD040'] <- '0'
	#gad_df$GAD041 <- recode(gad_df$GAD041, "'.'='0';'9'='0'")
	gad_df[gad_df$GAD041 == '.', 'GAD041'] <- '0'
	gad_df[gad_df$GAD041 == '9', 'GAD041'] <- '0'
	return(gad_df)
}

remakeGAD <- function(gad_df, informant="proband") {
	new_df <- data.frame(matrix(NA, nrow=nrow(gad_df), ncol=0))
	#colnames(new_df) <- c("bblid", "informant", "ITEM014", "ITEM015", "ITEM016", "ITEM017", "ITEM018", "ITEM019", "ITEM020", "ITEM021", "ITEM022", "ITEM023", "ITEM024", "ITEM025", "ITEM026", "ITEM27", "ITEM28")
	new_df$bblid <- gad_df$bblid
	new_df$informant <- informant
	# ITEM014 (GAD001)
	new_df$ITEM014 <- gad_df$GAD001
	# ITEM015 (GAD002)
	new_df$ITEM015 <- gad_df$GAD002
	# ITEM016 (GAD003A)
	new_df$ITEM016 <- gad_df$GAD003A
	# ITEM017 (GAD003B)
	new_df$ITEM017 <- gad_df$GAD003B
	# ITEM018 (GAD003C)
	new_df$ITEM018 <- gad_df$GAD003C
	# ITEM019 (GAD003D)
	new_df$ITEM019 <- gad_df$GAD003D
	# ITEM020 (GAD003E)
	new_df$ITEM020 <- gad_df$GAD003E
	# ITEM021 (GAD003F)
	new_df$ITEM021 <- gad_df$GAD003F
	# ITEM022 (GAD012) more complicated
	for (i in 1:nrow(gad_df)) {
		if (gad_df[i, "GAD011"] != 1 & gad_df[i, "GAD011A"] != 1 & gad_df[i, "GAD011B"] != 1 & gad_df[i, "GAD012"] != 1) {
			new_df[i, "ITEM022"] <- 0
		} else if ((gad_df[i, "GAD011"] != 1 | gad_df[i, "GAD011A"] != 1 | gad_df[i, "GAD011B"] != 1) & gad_df[i, "GAD012"] != 1) {
			new_df[i, "ITEM022"] <- 1
		} else {
			new_df[i, "ITEM022"] <- 2
		}
	}
	# ITEM023 (GAD015)
	new_df$ITEM023 <- gad_df$GAD015
	# ITEM024 (GAD016)
	new_df$ITEM024 <- gad_df$GAD016
	# ITEM025 (GAD017)
	new_df$ITEM025 <- gad_df$GAD017
	# ITEM026 (GAD018)
	new_df$ITEM026 <- gad_df$GAD018
	# ITEM027 (GAD019)
	new_df$ITEM027 <- gad_df$GAD019
	# ITEM028 (GAD020)
	new_df$ITEM028 <- gad_df$GAD020


	return(new_df)
}






