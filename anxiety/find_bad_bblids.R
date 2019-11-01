### This script identifies bblids that violate the skip logic of the GOASSESS battery: GAD
### 
### Ellyn Butler
### July 5, 2019 - present

# load packages 


# Load the data 
#df <- read.csv("/home/butellyn/parentchild_psychopathology/data/GOA_itemwise_for_reporter_agreement.csv") 
#demo <- read.csv("/home/butellyn/age_prediction/data/n9498_demographics_go1_20161212.csv")

# Check that answers make sense for Middle Proband
#depcols <- colnames(df)[grepl("DEP", colnames(df))][1:26]
#anx_df <- df[df$INTERVIEW_TYPE == "MP",c("PROBAND_BBLID", depcols)]
#names(anx_df)[names(anx_df) == 'PROBAND_BBLID'] <- 'bblid'
#anx_df <- merge(anx_df, demo, by='bblid')
#anx_df$ageAtClinicalAssess1 <- anx_df$ageAtClinicalAssess1/12

#rownames(anx_df) <- 1:nrow(anx_df)

#df_coord <- read.csv("/home/tymoore/PNC_GO1_GOASSESSDataArchiveGENERAL_DATA_2015-07-14_1210.csv")


##### Find BBLIDs for middle probands who have non-sensical answers
findBadBBLIDs <- function(anx_df) {
	suspicious_bblids <- list("Asked3"=c(), "Asked5"=c(), "Asked7"=c(), "Asked8"=c(), "Asked9"=c(), "Asked10"=c(), "Asked11"=c(), "Asked12"=c(), "Asked13"=c(), "Asked14"=c(), "Asked15"=c(), "Asked16"=c(), "Asked17"=c(), "Asked18"=c(), "Asked19"=c(), "Asked20"=c(), "WrongAge9"=c(), "WrongAge22"=c(), "WrongAge25"=c(), "WrongAge2225"=c(), "Asked3andYes"=c(), "Asked5andYes"=c(), "Asked7andYes"=c(), "Asked8andYes"=c(), "Asked9andYes"=c(), "Asked10andYes"=c(), "Asked11andYes"=c(), "Asked12andYes"=c(), "Asked13andYes"=c(), "Asked14andYes"=c(), "Asked15andYes"=c(), "Asked16andYes"=c(), "Asked17andYes"=c(), "Asked18andYes"=c(), "Asked19andYes"=c(), "Asked20andYes"=c())
	for (i in 1:nrow(anx_df)) {
		###### Asked one of the follow-up questions when inappropriate (i.e., "Was this different from how you usually are?")
		if (anx_df[i,"DEP002"] == 0 & (anx_df[i,"DEP003"] == 0 | anx_df[i,"DEP003"] == 1)) { suspicious_bblids$Asked3 <- c(suspicious_bblids$Asked3, anx_df[i, "bblid"]) }
		if (anx_df[i,"DEP004"] == 0 & (anx_df[i,"DEP005"] == 0 | anx_df[i,"DEP005"] == 1)) { suspicious_bblids$Asked5 <- c(suspicious_bblids$Asked5, anx_df[i, "bblid"]) }
		if (anx_df[i,"DEP006"] == 0 & (anx_df[i,"DEP007"] == 0 | anx_df[i,"DEP007"] == 1)) { suspicious_bblids$Asked7 <- c(suspicious_bblids$Asked7, anx_df[i, "bblid"]) }
		###### Asked one of the follow-up questions when inappropriate, and said yes
		if (anx_df[i,"DEP002"] == 0 & anx_df[i,"DEP003"] == 1) { suspicious_bblids$Asked3andYes <- c(suspicious_bblids$Asked3andYes, anx_df[i, "bblid"]) }
		if (anx_df[i,"DEP004"] == 0 & anx_df[i,"DEP005"] == 1) { suspicious_bblids$Asked5andYes <- c(suspicious_bblids$Asked5andYes, anx_df[i, "bblid"]) }
		if (anx_df[i,"DEP006"] == 0 & anx_df[i,"DEP007"] == 1) { suspicious_bblids$Asked7andYes <- c(suspicious_bblids$Asked7andYes, anx_df[i, "bblid"]) }
		###### Asked DEP008 when did not say yes to DEP004 and DEP006
		dep46 <- c(as.character(anx_df[i,"DEP004"]), as.character(anx_df[i,"DEP006"]))
		if (!("1" %in% dep46)) {
			if (anx_df[i,"DEP008"] == 0 | anx_df[i,"DEP008"] == 1) { suspicious_bblids$Asked8 <- c(suspicious_bblids$Asked8, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP008"] == 1) { suspicious_bblids$Asked8andYes <- c(suspicious_bblids$Asked8andYes, anx_df[i, "bblid"]) }
		}
		dep1246 <- c(as.character(anx_df[i,"DEP001"]), as.character(anx_df[i,"DEP002"]), as.character(anx_df[i,"DEP004"]), as.character(anx_df[i,"DEP006"]))
		if (!("1" %in% dep1246)) { 
			# Asked about their symptoms when they did not report any
			if (anx_df[i,"DEP009"] == 0 | anx_df[i,"DEP009"] == 1) { suspicious_bblids$Asked9 <- c(suspicious_bblids$Asked9, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP010"] == 0 | anx_df[i,"DEP010"] == 1) { suspicious_bblids$Asked10 <- c(suspicious_bblids$Asked10, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP011"] == 0 | anx_df[i,"DEP011"] == 1) { suspicious_bblids$Asked11 <- c(suspicious_bblids$Asked11, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP012"] == 0 | anx_df[i,"DEP012"] == 1) { suspicious_bblids$Asked12 <- c(suspicious_bblids$Asked12, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP013"] == 0 | anx_df[i,"DEP013"] == 1) { suspicious_bblids$Asked13 <- c(suspicious_bblids$Asked13, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP014"] == 0 | anx_df[i,"DEP014"] == 1) { suspicious_bblids$Asked14 <- c(suspicious_bblids$Asked14, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP015"] == 0 | anx_df[i,"DEP015"] == 1) { suspicious_bblids$Asked15 <- c(suspicious_bblids$Asked15, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP016"] == 0 | anx_df[i,"DEP016"] == 1) { suspicious_bblids$Asked16 <- c(suspicious_bblids$Asked16, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP017"] == 0 | anx_df[i,"DEP017"] == 1) { suspicious_bblids$Asked17 <- c(suspicious_bblids$Asked17, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP018"] == 0 | anx_df[i,"DEP018"] == 1) { suspicious_bblids$Asked18 <- c(suspicious_bblids$Asked18, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP019"] == 0 | anx_df[i,"DEP019"] == 1) { suspicious_bblids$Asked19 <- c(suspicious_bblids$Asked19, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP020"] == 0 | anx_df[i,"DEP020"] == 1) { suspicious_bblids$Asked20 <- c(suspicious_bblids$Asked20, anx_df[i, "bblid"]) }
			# Asked about their symptoms when they did not report any, and said yes to the question about their non-existent symptoms
			if (anx_df[i,"DEP009"] == 1) { suspicious_bblids$Asked9andYes <- c(suspicious_bblids$Asked9andYes, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP010"] == 1) { suspicious_bblids$Asked10andYes <- c(suspicious_bblids$Asked10andYes, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP011"] == 1) { suspicious_bblids$Asked11andYes <- c(suspicious_bblids$Asked11andYes, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP012"] == 1) { suspicious_bblids$Asked12andYes <- c(suspicious_bblids$Asked12andYes, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP013"] == 1) { suspicious_bblids$Asked13andYes <- c(suspicious_bblids$Asked13andYes, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP014"] == 1) { suspicious_bblids$Asked14andYes <- c(suspicious_bblids$Asked14andYes, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP015"] == 1) { suspicious_bblids$Asked15andYes <- c(suspicious_bblids$Asked15andYes, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP016"] == 1) { suspicious_bblids$Asked16andYes <- c(suspicious_bblids$Asked16andYes, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP017"] == 1) { suspicious_bblids$Asked17andYes <- c(suspicious_bblids$Asked17andYes, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP018"] == 1) { suspicious_bblids$Asked18andYes <- c(suspicious_bblids$Asked18andYes, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP019"] == 1) { suspicious_bblids$Asked19andYes <- c(suspicious_bblids$Asked19andYes, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP020"] == 1) { suspicious_bblids$Asked20andYes <- c(suspicious_bblids$Asked20andYes, anx_df[i, "bblid"]) }
		}
		if (anx_df[i,"DEP011"] != 1 | anx_df[i,"DEP010"] != 1) {
			# Asked about their symptoms when they did not report having them for long enough
			if (anx_df[i,"DEP012"] == 0 | anx_df[i,"DEP012"] == 1) { suspicious_bblids$Asked12 <- c(suspicious_bblids$Asked12, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP013"] == 0 | anx_df[i,"DEP013"] == 1) { suspicious_bblids$Asked13 <- c(suspicious_bblids$Asked13, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP014"] == 0 | anx_df[i,"DEP014"] == 1) { suspicious_bblids$Asked14 <- c(suspicious_bblids$Asked14, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP015"] == 0 | anx_df[i,"DEP015"] == 1) { suspicious_bblids$Asked15 <- c(suspicious_bblids$Asked15, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP016"] == 0 | anx_df[i,"DEP016"] == 1) { suspicious_bblids$Asked16 <- c(suspicious_bblids$Asked16, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP017"] == 0 | anx_df[i,"DEP017"] == 1) { suspicious_bblids$Asked17 <- c(suspicious_bblids$Asked17, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP018"] == 0 | anx_df[i,"DEP018"] == 1) { suspicious_bblids$Asked18 <- c(suspicious_bblids$Asked18, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP019"] == 0 | anx_df[i,"DEP019"] == 1) { suspicious_bblids$Asked19 <- c(suspicious_bblids$Asked19, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP020"] == 0 | anx_df[i,"DEP020"] == 1) { suspicious_bblids$Asked20 <- c(suspicious_bblids$Asked20, anx_df[i, "bblid"]) }
			# Asked about their symptoms when they did not report having them for long enough, and said yes to the follow-up questions
			if (anx_df[i,"DEP012"] == 1) { suspicious_bblids$Asked12andYes <- c(suspicious_bblids$Asked12andYes, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP013"] == 1) { suspicious_bblids$Asked13andYes <- c(suspicious_bblids$Asked13andYes, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP014"] == 1) { suspicious_bblids$Asked14andYes <- c(suspicious_bblids$Asked14andYes, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP015"] == 1) { suspicious_bblids$Asked15andYes <- c(suspicious_bblids$Asked15andYes, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP016"] == 1) { suspicious_bblids$Asked16andYes <- c(suspicious_bblids$Asked16andYes, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP017"] == 1) { suspicious_bblids$Asked17andYes <- c(suspicious_bblids$Asked17andYes, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP018"] == 1) { suspicious_bblids$Asked18andYes <- c(suspicious_bblids$Asked18andYes, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP019"] == 1) { suspicious_bblids$Asked19andYes <- c(suspicious_bblids$Asked19andYes, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP020"] == 1) { suspicious_bblids$Asked20andYes <- c(suspicious_bblids$Asked20andYes, anx_df[i, "bblid"]) }			
		}
		if (anx_df[i,"DEP012"] != 1 & anx_df[i,"DEP013"] != 1 & anx_df[i,"DEP014"] != 1 & anx_df[i,"DEP015"] != 1 &  anx_df[i,"DEP016"] != 1 & anx_df[i,"DEP017"] != 1) {
			if (anx_df[i,"DEP018"] == 1) { suspicious_bblids$Asked18andYes <- c(suspicious_bblids$Asked18andYes, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP019"] == 1) { suspicious_bblids$Asked19andYes <- c(suspicious_bblids$Asked19andYes, anx_df[i, "bblid"]) }
			if (anx_df[i,"DEP020"] == 1) { suspicious_bblids$Asked20andYes <- c(suspicious_bblids$Asked20andYes, anx_df[i, "bblid"]) }	
		}
		###### Identify age-related variables that give an answer over the person's actual age
		if (anx_df[i,"DEP009"] != ".") { if (as.numeric(as.character(anx_df[i,"DEP009"])) > anx_df[i,"ageAtClinicalAssess1"]) {
			suspicious_bblids$WrongAge9 <- c(suspicious_bblids$WrongAge9, anx_df[i, "bblid"])
		}}
		if (anx_df[i,"DEP022"] != ".") { if (as.numeric(as.character(anx_df[i,"DEP022"])) > anx_df[i,"ageAtClinicalAssess1"]) {
			suspicious_bblids$WrongAge22 <- c(suspicious_bblids$WrongAge22, anx_df[i, "bblid"])
		}}
		if (anx_df[i,"DEP025"] != ".") { if (as.numeric(as.character(anx_df[i,"DEP025"])) > anx_df[i,"ageAtClinicalAssess1"]) {
			suspicious_bblids$WrongAge25 <- c(suspicious_bblids$WrongAge25, anx_df[i, "bblid"])
		}}
		if (anx_df[i,"DEP025"] != ".") { if (anx_df[i,"DEP022"] != ".") { if (as.numeric(as.character(anx_df[i,"DEP022"])) > as.numeric(as.character(anx_df[i,"DEP025"]))) {
			suspicious_bblids$WrongAge2225 <- c(suspicious_bblids$WrongAge2225, anx_df[i, "bblid"])
		}}}
	}
	# Get rid of potential duplicate values in sublists
	suspicious_bblids$Asked3 <- unique(suspicious_bblids$Asked3)
	suspicious_bblids$Asked5 <- unique(suspicious_bblids$Asked5)
	suspicious_bblids$Asked7 <- unique(suspicious_bblids$Asked7)
	suspicious_bblids$Asked8 <- unique(suspicious_bblids$Asked8)
	suspicious_bblids$Asked9 <- unique(suspicious_bblids$Asked9)
	suspicious_bblids$Asked10 <- unique(suspicious_bblids$Asked10)
	suspicious_bblids$Asked11 <- unique(suspicious_bblids$Asked11)
	suspicious_bblids$Asked12 <- unique(suspicious_bblids$Asked12)
	suspicious_bblids$Asked13 <- unique(suspicious_bblids$Asked13)
	suspicious_bblids$Asked14 <- unique(suspicious_bblids$Asked14)
	suspicious_bblids$Asked15 <- unique(suspicious_bblids$Asked15)
	suspicious_bblids$Asked16 <- unique(suspicious_bblids$Asked16)
	suspicious_bblids$Asked17 <- unique(suspicious_bblids$Asked17)
	suspicious_bblids$Asked18 <- unique(suspicious_bblids$Asked18)
	suspicious_bblids$Asked19 <- unique(suspicious_bblids$Asked19)
	suspicious_bblids$Asked20 <- unique(suspicious_bblids$Asked20)
	return(suspicious_bblids)
}	
	

##### Find the number of mistakes per coordinator
identifyCoordinators_MP <- function(df_coord, suspicious_bblids) {
	coordinators <- vector("list", 55)
	names(coordinators) <- levels(df_coord$assigned_coordin)
	for (i in 1:length(suspicious_bblids)) {
		for (j in 1:length(suspicious_bblids[[i]])) {
			bblid <- suspicious_bblids[[i]][[j]]
			bblid_MP <- paste0(bblid, "_MP")
			coord <- as.character(df_coord[df_coord$redcapid == bblid_MP, "assigned_coordin"])
			if (length(coord) != 0) {
				if (coord != "") {
					val <- coordinators[[coord]]	
					if (is.null(val)) { coordinators[[coord]] <- 1 
					} else { coordinators[[coord]] <- coordinators[[coord]] + 1
					}
				}
			}
		}
	}
	return(coordinators)
}

	
##### Remove BBLIDs from dataframe that are non-sensical, or modify their answers, and then remove subjects that still have missing values
# remove options
# "NoAge": remove all subjects with inconsistent age related items
# "NoYes": remove all subjects who answered yes to a question that they shouldn't have been asked (don't put this and "NoAsked" in simultaneously
# "NoAsked": remove all subjects who answered anything to a question that they shouldn't have been asked
# impute options
# 1: imputes values for subjects with only one missing value
# 2: imputes values for subjects with at most two missing values
# ...
# WARNING: DON'T GO ABOVE 3 or 4
# Got rid of question 8
cleanDepression <- function(anx_df, suspicious_bblids, remove=c("NoAsked"), impute=0) {
	# 1) Identify the subjects to be removed based on inconsistent answers
	if (("NoYes" %in% remove) & ("NoAsked" %in% remove)) { remove <- remove[!("NoYes")]}
	bblids_to_remove <- c()
	if ("NoAge" %in% remove) {
		bblids_to_remove <- c(bblids_to_remove, suspicious_bblids$WrongAge9, suspicious_bblids$WrongAge22, suspicious_bblids$WrongAge25, suspicious_bblids$WrongAge2225)
		bblids_to_remove <- unique(bblids_to_remove)
	}
	if ("NoYes" %in% remove) {
		bblids_to_remove <- c(bblids_to_remove, suspicious_bblids$Asked3andYes, suspicious_bblids$Asked5andYes, suspicious_bblids$Asked7andYes, suspicious_bblids$Asked9andYes, suspicious_bblids$Asked10andYes, suspicious_bblids$Asked11andYes, suspicious_bblids$Asked12andYes, suspicious_bblids$Asked13andYes, suspicious_bblids$Asked14andYes, suspicious_bblids$Asked15andYes, suspicious_bblids$Asked16andYes, suspicious_bblids$Asked17andYes, suspicious_bblids$Asked18andYes, suspicious_bblids$Asked19andYes, suspicious_bblids$Asked20andYes) # suspicious_bblids$Asked8andYes
		bblids_to_remove <- unique(bblids_to_remove)
	}
	if ("NoAsked" %in% remove) {
		bblids_to_remove <- c(bblids_to_remove, suspicious_bblids$Asked3, suspicious_bblids$Asked5, suspicious_bblids$Asked7, suspicious_bblids$Asked9, suspicious_bblids$Asked10, suspicious_bblids$Asked11, suspicious_bblids$Asked12, suspicious_bblids$Asked13, suspicious_bblids$Asked14, suspicious_bblids$Asked15, suspicious_bblids$Asked16, suspicious_bblids$Asked17, suspicious_bblids$Asked18, suspicious_bblids$Asked19, suspicious_bblids$Asked20) #suspicious_bblids$Asked8
		bblids_to_remove <- unique(bblids_to_remove)
	}
	anx_df2 <- anx_df[!(anx_df$bblid %in% bblids_to_remove),]
	rownames(anx_df2) <- 1:nrow(anx_df2)
	# 2) Put 0s in place of missing values, where appropriate
	for (i in 1:nrow(anx_df2)) {
		if (anx_df2[i, "DEP002"] == 0) { anx_df2[i, "DEP003"] <- 0 }
		if (anx_df2[i, "DEP004"] == 0) { anx_df2[i, "DEP005"] <- 0 }
		if (anx_df2[i, "DEP006"] == 0) { anx_df2[i, "DEP007"] <- 0 }
		dep46 <- c(as.character(anx_df2[i,"DEP004"]), as.character(anx_df2[i,"DEP006"]))
		if (!(1 %in% dep46) & (0 %in% dep46)) { 
			#anx_df2[i, "DEP008"] <- 0 
		}
		dep1246 <- c(as.character(anx_df2[i,"DEP001"]), as.character(anx_df2[i,"DEP002"]), as.character(anx_df2[i,"DEP004"]), as.character(anx_df2[i,"DEP006"]))
		if (!(1 %in% dep1246) & (0 %in% dep1246)) { 
			anx_df2[i, "DEP009"] <- 0 
			anx_df2[i, "DEP010"] <- 0 
			anx_df2[i, "DEP011"] <- 0 
			anx_df2[i, "DEP012"] <- 0 
			anx_df2[i, "DEP013"] <- 0 
			anx_df2[i, "DEP014"] <- 0 
			anx_df2[i, "DEP015"] <- 0 
			anx_df2[i, "DEP016"] <- 0 
			anx_df2[i, "DEP017"] <- 0 
			anx_df2[i, "DEP018"] <- 0 
			anx_df2[i, "DEP019"] <- 0 
			anx_df2[i, "DEP020"] <- 0 
		}
		if (anx_df2[i, "DEP010"] == 0 | anx_df2[i, "DEP011"] == 0) {
			anx_df2[i, "DEP012"] <- 0 
			anx_df2[i, "DEP013"] <- 0 
			anx_df2[i, "DEP014"] <- 0 
			anx_df2[i, "DEP015"] <- 0 
			anx_df2[i, "DEP016"] <- 0 
			anx_df2[i, "DEP017"] <- 0 
			anx_df2[i, "DEP018"] <- 0 
			anx_df2[i, "DEP019"] <- 0 
			anx_df2[i, "DEP020"] <- 0 
		}
	}
	# 3) For subjects that still have missing values (check the missing values for each variable), add them to the list of subjects to be removed
		#NOTE: Might be able to impute some of these values based on subsequent answers
	bblids_to_remove2 <- c()
	for (i in 1:nrow(anx_df2)) {
		if (anx_df2[i,"DEP001"] == "." | anx_df2[i,"DEP001"] == 9) { bblids_to_remove2 <- c(bblids_to_remove2, anx_df2[i, "bblid"]) }
		if (anx_df2[i,"DEP002"] == "." | anx_df2[i,"DEP002"] == 9) { bblids_to_remove2 <- c(bblids_to_remove2, anx_df2[i, "bblid"]) }
		if (anx_df2[i,"DEP003"] == "." | anx_df2[i,"DEP003"] == 9) { bblids_to_remove2 <- c(bblids_to_remove2, anx_df2[i, "bblid"]) }
		if (anx_df2[i,"DEP004"] == "." | anx_df2[i,"DEP004"] == 9) { bblids_to_remove2 <- c(bblids_to_remove2, anx_df2[i, "bblid"]) }
		if (anx_df2[i,"DEP005"] == "." | anx_df2[i,"DEP005"] == 9) { bblids_to_remove2 <- c(bblids_to_remove2, anx_df2[i, "bblid"]) }
		if (anx_df2[i,"DEP006"] == "." | anx_df2[i,"DEP006"] == 9) { bblids_to_remove2 <- c(bblids_to_remove2, anx_df2[i, "bblid"]) }
		if (anx_df2[i,"DEP007"] == "." | anx_df2[i,"DEP007"] == 9) { bblids_to_remove2 <- c(bblids_to_remove2, anx_df2[i, "bblid"]) }
		#if (anx_df2[i,"DEP008"] == "." | anx_df2[i,"DEP008"] == 9 | anx_df2[i,"DEP008"] == 7) { bblids_to_remove2 <- c(bblids_to_remove2, anx_df2[i, "bblid"]) }
		if (anx_df2[i,"DEP010"] == "." | anx_df2[i,"DEP010"] == 9) { bblids_to_remove2 <- c(bblids_to_remove2, anx_df2[i, "bblid"]) }
		if (anx_df2[i,"DEP011"] == "." | anx_df2[i,"DEP011"] == 9) { bblids_to_remove2 <- c(bblids_to_remove2, anx_df2[i, "bblid"]) }
		if (anx_df2[i,"DEP012"] == "." | anx_df2[i,"DEP012"] == 9) { bblids_to_remove2 <- c(bblids_to_remove2, anx_df2[i, "bblid"]) }
		if (anx_df2[i,"DEP013"] == "." | anx_df2[i,"DEP013"] == 9) { bblids_to_remove2 <- c(bblids_to_remove2, anx_df2[i, "bblid"]) }
		if (anx_df2[i,"DEP014"] == "." | anx_df2[i,"DEP014"] == 9) { bblids_to_remove2 <- c(bblids_to_remove2, anx_df2[i, "bblid"]) }
		if (anx_df2[i,"DEP015"] == "." | anx_df2[i,"DEP015"] == 9) { bblids_to_remove2 <- c(bblids_to_remove2, anx_df2[i, "bblid"]) }	
		if (anx_df2[i,"DEP016"] == "." | anx_df2[i,"DEP016"] == 9) { bblids_to_remove2 <- c(bblids_to_remove2, anx_df2[i, "bblid"]) }
		if (anx_df2[i,"DEP017"] == "." | anx_df2[i,"DEP017"] == 9) { bblids_to_remove2 <- c(bblids_to_remove2, anx_df2[i, "bblid"]) }
		if (anx_df2[i,"DEP018"] == ".") { bblids_to_remove2 <- c(bblids_to_remove2, anx_df2[i, "bblid"]) }
		if (anx_df2[i,"DEP019"] == ".") { bblids_to_remove2 <- c(bblids_to_remove2, anx_df2[i, "bblid"]) }
		if (anx_df2[i,"DEP020"] == "." | anx_df2[i,"DEP020"] == 9) { bblids_to_remove2 <- c(bblids_to_remove2, anx_df2[i, "bblid"]) }
	}
	# 4) Remove duplicate bblids in bblids_to_remove2
	bblids_to_remove2 <- unique(bblids_to_remove2)
	# 5) If a subject has fewer missing values than "impute", remove them from bblids_to_remove2
	anx_df2$number_missing_values <- 0
	for (i in 1:nrow(anx_df2)) {
		if (anx_df2[i,"DEP001"] == "." | anx_df2[i,"DEP001"] == 9) { anx_df2[i, "number_missing_values"] <- anx_df2[i, "number_missing_values"] + 1 }
		if (anx_df2[i,"DEP002"] == "." | anx_df2[i,"DEP002"] == 9) { anx_df2[i, "number_missing_values"] <- anx_df2[i, "number_missing_values"] + 1 }
		if (anx_df2[i,"DEP003"] == "." | anx_df2[i,"DEP003"] == 9) { anx_df2[i, "number_missing_values"] <- anx_df2[i, "number_missing_values"] + 1 }
		if (anx_df2[i,"DEP004"] == "." | anx_df2[i,"DEP004"] == 9) { anx_df2[i, "number_missing_values"] <- anx_df2[i, "number_missing_values"] + 1 }
		if (anx_df2[i,"DEP005"] == "." | anx_df2[i,"DEP005"] == 9) { anx_df2[i, "number_missing_values"] <- anx_df2[i, "number_missing_values"] + 1 }
		if (anx_df2[i,"DEP006"] == "." | anx_df2[i,"DEP006"] == 9) { anx_df2[i, "number_missing_values"] <- anx_df2[i, "number_missing_values"] + 1 }
		if (anx_df2[i,"DEP007"] == "." | anx_df2[i,"DEP007"] == 9) { anx_df2[i, "number_missing_values"] <- anx_df2[i, "number_missing_values"] + 1 }
		#if (anx_df2[i,"DEP008"] == "." | anx_df2[i,"DEP008"] == 9 | anx_df2[i,"DEP008"] == 7) { anx_df2[i, "number_missing_values"] <- anx_df2[i, "number_missing_values"] + 1 }
		if (anx_df2[i,"DEP010"] == "." | anx_df2[i,"DEP010"] == 9) { anx_df2[i, "number_missing_values"] <- anx_df2[i, "number_missing_values"] + 1 }
		if (anx_df2[i,"DEP011"] == "." | anx_df2[i,"DEP011"] == 9) { anx_df2[i, "number_missing_values"] <- anx_df2[i, "number_missing_values"] + 1 }
		if (anx_df2[i,"DEP012"] == "." | anx_df2[i,"DEP012"] == 9) { anx_df2[i, "number_missing_values"] <- anx_df2[i, "number_missing_values"] + 1 }
		if (anx_df2[i,"DEP013"] == "." | anx_df2[i,"DEP013"] == 9) { anx_df2[i, "number_missing_values"] <- anx_df2[i, "number_missing_values"] + 1 }
		if (anx_df2[i,"DEP014"] == "." | anx_df2[i,"DEP014"] == 9) { anx_df2[i, "number_missing_values"] <- anx_df2[i, "number_missing_values"] + 1 }
		if (anx_df2[i,"DEP015"] == "." | anx_df2[i,"DEP015"] == 9) { anx_df2[i, "number_missing_values"] <- anx_df2[i, "number_missing_values"] + 1 }	
		if (anx_df2[i,"DEP016"] == "." | anx_df2[i,"DEP016"] == 9) { anx_df2[i, "number_missing_values"] <- anx_df2[i, "number_missing_values"] + 1 }
		if (anx_df2[i,"DEP017"] == "." | anx_df2[i,"DEP017"] == 9) { anx_df2[i, "number_missing_values"] <- anx_df2[i, "number_missing_values"] + 1 }
		if (anx_df2[i,"DEP018"] == ".") { anx_df2[i, "number_missing_values"] <- anx_df2[i, "number_missing_values"] + 1 }
		if (anx_df2[i,"DEP019"] == ".") { anx_df2[i, "number_missing_values"] <- anx_df2[i, "number_missing_values"] + 1 }
		if (anx_df2[i,"DEP020"] == "." | anx_df2[i,"DEP020"] == 9) { anx_df2[i, "number_missing_values"] <- anx_df2[i, "number_missing_values"] + 1 }

	}
	#okay_bblids <- anx_df2[anx_df2$number_missing_values <= impute & anx_df2$number_missing_values > 0, "bblid"] #March 29, 2019: This should be shorter than bblids_to_remove2....
	#bblids_to_remove2 <- bblids_to_remove2[!(bblids_to_remove2 %in% okay_bblids)]
	# 6) Remove subjects
	anx_df3 <- anx_df2[!(anx_df2$bblid %in% bblids_to_remove2),]
	# 7) Remove DEP009 and DEP021-026
	anx_df3 <- anx_df3[,!(names(anx_df3) %in% c("DEP008", "DEP009", "DEP021", "DEP022", "DEP023", "DEP024", "DEP025", "DEP026"))]
	# 8) Impute missing values #April 1, 2019: Not currently working... intentionally
	#for (i in 1:nrow(anx_df3)) {
		#if (anx_df3[i,"DEP001"] == 9) { anx_df3[i,"DEP001"] <- "." }
		#if (anx_df3[i,"DEP002"] == 9) { anx_df3[i,"DEP002"] <- "." }
		#if (anx_df3[i,"DEP003"] == 9) { anx_df3[i,"DEP003"] <- "." }
		#if (anx_df3[i,"DEP004"] == 9) { anx_df3[i,"DEP004"] <- "." }
		#if (anx_df3[i,"DEP005"] == 9) { anx_df3[i,"DEP005"] <- "."}
		#if (anx_df3[i,"DEP006"] == 9) { anx_df3[i,"DEP006"] <- "." }
		#if (anx_df3[i,"DEP007"] == 9) { anx_df3[i,"DEP007"] <- "." }
		#if (anx_df3[i,"DEP008"] == 9 | anx_df3[i,"DEP008"] == 7) { anx_df3[i,"DEP008"] <- "." }
		#if (anx_df3[i,"DEP010"] == 9) { anx_df3[i,"DEP010"] <- "." }
		#if (anx_df3[i,"DEP011"] == 9) { anx_df3[i,"DEP011"] <- "." }
		#if (anx_df3[i,"DEP012"] == 9) { anx_df3[i,"DEP012"] <- "." }
		#if (anx_df3[i,"DEP013"] == 9) { anx_df3[i,"DEP013"] <- "." }
		#if (anx_df3[i,"DEP014"] == 9) { anx_df3[i,"DEP014"] <- "." }
		#if (anx_df3[i,"DEP015"] == 9) { anx_df3[i,"DEP015"] <- "." }	
		#if (anx_df3[i,"DEP016"] == 9) { anx_df3[i,"DEP016"] <- "." }
		#if (anx_df3[i,"DEP017"] == 9) { anx_df3[i,"DEP017"] <- "." }
		#if (anx_df3[i,"DEP020"] == 9) { anx_df3[i,"DEP020"] <- "." }
	#}

	# 9)
	#tmp <- anx_df3
	anx_df3$DEP001 <- as.numeric(levels(anx_df3$DEP001))[anx_df3$DEP001]
	anx_df3$DEP002 <- as.numeric(levels(anx_df3$DEP002))[anx_df3$DEP002]
	anx_df3$DEP003 <- as.numeric(levels(anx_df3$DEP003))[anx_df3$DEP003]
	anx_df3$DEP004 <- as.numeric(levels(anx_df3$DEP004))[anx_df3$DEP004]
	anx_df3$DEP005 <- as.numeric(levels(anx_df3$DEP005))[anx_df3$DEP005]
	anx_df3$DEP006 <- as.numeric(levels(anx_df3$DEP006))[anx_df3$DEP006]
	anx_df3$DEP007 <- as.numeric(levels(anx_df3$DEP007))[anx_df3$DEP007]
	anx_df3$DEP010 <- as.numeric(levels(anx_df3$DEP010))[anx_df3$DEP010]
	anx_df3$DEP011 <- as.numeric(levels(anx_df3$DEP011))[anx_df3$DEP011]
	anx_df3$DEP012 <- as.numeric(levels(anx_df3$DEP012))[anx_df3$DEP012]
	anx_df3$DEP013 <- as.numeric(levels(anx_df3$DEP013))[anx_df3$DEP013]
	anx_df3$DEP014 <- as.numeric(levels(anx_df3$DEP014))[anx_df3$DEP014]
	anx_df3$DEP015 <- as.numeric(levels(anx_df3$DEP015))[anx_df3$DEP015]
	anx_df3$DEP016 <- as.numeric(levels(anx_df3$DEP016))[anx_df3$DEP016]
	anx_df3$DEP017 <- as.numeric(levels(anx_df3$DEP017))[anx_df3$DEP017]
	anx_df3$DEP018 <- as.numeric(levels(anx_df3$DEP018))[anx_df3$DEP018]
	anx_df3$DEP019 <- as.numeric(levels(anx_df3$DEP019))[anx_df3$DEP019]
	anx_df3$DEP020 <- as.numeric(levels(anx_df3$DEP020))[anx_df3$DEP020]

	# 10) Return clean dataframe
	return(anx_df3)
}











#Notes
#1) Check age related variables aren't over 25... DONE
#2) Check DEP025 > DEP022... DONE
#3) Unclear how DEP023 and DEP024 should relate to each other
#4) DEP026 is a crap question. Not clear what delineates a "time"







