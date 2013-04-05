ptm <- proc.time() ## http://stat.ethz.ch/R-manual/R-patched/library/base/html/proc.time.html
# set dran dir, for people who are into that
options(repos=c("http://cran.at.r-project.org/"))
# load required packages
install.packages("RCurl", dependencies = TRUE)

load(RCurl)
#if(require(RCurl)){
#   print("RCurl is loaded correctly")
# } else {
#   print("trying to install RCurl")
#   install.packages('RCurl')
#  if(require(RCurl)){
#    print("RCurl installed and loaded")
#  } else {
#    stop("could not install RCurl")
#  }
# }
require(RCurl)

####################################################################
##                                                                ##
##     Read data of https://redcap.ucsfopenresearch.org/api/      ##
##                                                                ##
####################################################################

RAW.API <- postForm(REDcap.URL, token=Redcap.token, content="record", type="flat", format="csv", rawOrLabel="Label", .opts=curlOptions(ssl.verifypeer=TRUE, cainfo=REDCap.crt, verbose=FALSE))
####################################################################
##                                                                ##
##      Transform and subset data workable R data frame           ##
##                                                                ##
####################################################################
Rawdata <- read.table(file = textConnection(RAW.API), header = TRUE, sep = ",", na.strings = "", stringsAsFactors = FALSE)

data <- Rawdata
# rm(RAW.API)
# dput(names(data))
# head(data)


####################################################################
##                                                                ##
##         Compute varibes need for Case Summary Report           ##
##                                                                ##
####################################################################
# wdata <- data[,c("unique_id", "redcap_event_name", "therapist_name", "clinician_name", "event_type", grep('_date', names(data), value = T), grep('_time_spent', names(data), value = T))]

wdata <- data

## date varible
df <- wdata[,c("out_date", "ass_date", "con_date", "cpp_date", "grt_date", "cma_date", "oth_tf_cbt_date", "oth_older_date", "oth_carag_date", "oth_crisis_date", "oth_oth_date")]
date <- as.Date(as.vector(apply(df,1,function(x) ifelse(all(is.na(x)),NA,x[!is.na(x)]))), , format = "%F")
wdata <- cbind(date, wdata)

## date varible
wdata$'contact type' <- wdata$event_type

PNeventType <- wdata$event_type
PNeventType <- sub("Outreach", 'OUT', PNeventType)
PNeventType <- sub("Assessment", 'ASS', PNeventType)
PNeventType <- sub("Consultation", 'CON', PNeventType)
PNeventType <- sub("Child Parent Psychotherapy", 'CPP', PNeventType)
PNeventType <- sub("Group Therapy", 'GRT', PNeventType)
PNeventType <- sub("Case Management", 'CMA', PNeventType)
wdata$PNeventType <- sub("Other", 'OTH', PNeventType)

wdata$PNclinician_name <-  gsub("[^A-Z]", "", wdata$clinician_name)

## min contact
wdata$minContact <- rowSums(wdata[,grep('time_spent', names(wdata), value = T)], na.rm = T)

## min Progress Note
wdata$minProgressNote <- rowSums(wdata[,grep('time_prog_note', names(wdata), value = T)], na.rm = T)

## min Process/Narritive Note
wdata$minProcesNote <- rowSums(wdata[,grep('time_narra', names(wdata), value = T)], na.rm = T)

####################################################################
##                                                                ##
##                               Progress Notes                   ##
##                                                                ##
#names(wdata[1:20])

#as.character(apply(data[,grep('_text$', names(data), value = T)],1,function(x) ifelse(all(is.na(x)),NA,x[!is.na(x)])))
#names(#
wdata$ProgressNotes <- as.character(apply(wdata[,c("out_let_note", "out_ot_note", "out_phone_note", "out_phone_ot_note", "ass_show_note", "ass_noshow_note", "con_show_note", "con_noshow_note", "cpp_show_note", "cpp_noshow_note", "grt_cag_show_note", "grt_cag_noshow_note", "grt_cpg_show_note", "grt_cpg_noshow_note", "grt_chg_show_note", "grt_chg_noshow_note", "cma_serv_note", "cma_clo_note", "cma_reo_note", "cma_othe_note", "oth_show_note", "oth_noshow_note")],1,function(x) ifelse(all(is.na(x)),NA,x[!is.na(x)])))

####################################################################
##                                                                ##
##                  supervisors licensing number                  ##
##                                                                ##
# wdata[,c("supervisor", "unique_id", "case_nick", "redcap_event_name")]


SLnumber <- wdata$supervisor
SLnumber[SLnumber=="Case does not have a supervisor"] <- ""
SLnumber[SLnumber=="Alicia Lieberman"] <- "PSY 7881"
SLnumber[SLnumber=="Chandra Ghosh Ippen"] <- "PSY 17759" 
SLnumber[SLnumber=="Griselda Oliver Bucio"] <- "MFC 47063"
SLnumber[SLnumber=="Joyce Dorado"] <- "PSY 15801'#"
SLnumber[SLnumber=="Laura Castro"] <- "PSY 18997"
SLnumber[SLnumber=="Lisa Gutierrez"] <- "PSY 23588"
SLnumber[SLnumber=="Maria Torres"] <- "MFC 42029"
SLnumber[SLnumber=="Miriam Hernandez Dimmler"] <- "PSY 22476"
SLnumber[SLnumber=="Nancy Compton"] <- "PSY 14530"
SLnumber[SLnumber=="Patricia Van Horn"] <- "PSY 15580"
SLnumber[SLnumber=="Vilma Reyes"] <- "PSY 24199"
SLnumber[SLnumber=="Other"] <- NA

wdata$SLnumber <- paste0("-----------------------------------------", " ", wdata$supervisor, ", ",SLnumber)
wdata$SLnumber[wdata$SLnumber=="----------------------------------------- Case does not have a supervisor, "] <- "----------------------------------------- Currently no supervisor"

####################################################################
##                                                                ##
##          Session status, no show, show, cancel, "ass_show\     ##
##                                                                ##
s <- as.character(apply(data[,grep('_show$', names(data), value = T)],1,function(x) ifelse(all(is.na(x)),NA,x[!is.na(x)])))
wdata$showTF <- ifelse(s == 'yes',TRUE,FALSE)
s[s=='yes'] <- 'Show'
s[s=='no'] <- "No show (reason unknown)"
s.2 <- as.character(apply(data[,grep('noshow_reasons$', names(data), value = T)],1,function(x) ifelse(all(is.na(x)),NA,x[!is.na(x)])))
s.2[s.2=='Childcare problem'] <- 'Cancellation, childcare problem'
s.2[s.2=='Conflicting appointment'] <- 'Cancellation, conflicting appointment'
s.2[s.2=='Forgot'] <- 'Cancellation, forgot'
s.2[s.2=='Illness'] <- 'Cancellation, illness'
s.2[s.2=='Transportation'] <- 'Cancellation, transportation'
s.2[s.2=='Weather'] <- 'Cancellation, weather'
s.2[s.2=='Other'] <- 'Cancellation, other'
wdata$SessionStatus <- as.factor(ifelse(is.na(s.2), as.character(s), as.character(s.2)))
wdata$CloOpn[wdata$cma_clo_reo=="I am Re-Opening case"] <- 'Re-Opening'
wdata$CloOpn[wdata$cma_clo_reo=="I am closing this case"] <- 'Closing case'
wdata$SessionStatus <- as.factor(ifelse(is.na(wdata$CloOpn), as.character(wdata$SessionStatus), as.character(wdata$CloOpn)))
wdata$cmaSS[wdata$cma_type=="Meeting with service provider(s)"] <- 'Meeting'
wdata$cmaSS[wdata$cma_type=="Phone call with service provider(s)"] <- "Phone call"
wdata$cmaSS[wdata$cma_type=="Other"] <- "Other"
wdata$SessionStatus <- as.factor(ifelse(is.na(wdata$cmaSS), as.character(wdata$SessionStatus), as.character(wdata$cmaSS)))
wdata$SessionStatus <- as.factor(ifelse(is.na(wdata$out_activity), as.character(wdata$SessionStatus), as.character(wdata$out_activity)))

## append what kind of assesment was done tothe Session Status Vraiabls
temp <- sub("Intake Assessment Summary", 'IAS', wdata$ass_comp)
temp <- sub("CBCL 1.5-5, Child Behavior Checklist", 'CBCL 1.5-5', temp)
temp <- sub("CBCL 6+, Child Behavior Checklist", 'CBCL 6+', temp)
temp <- sub("TRF, The Teacher's Report Form", 'TRF', temp)
temp <- sub("TSCYC, Trauma Symptom Checklist for Young Children", 'TSCYC', temp)
temp <- sub("TESI, Traumatic Events Screening Inventory", 'TESI', temp)
temp <- sub("PSI, Parenting Stress Index", 'PSI', temp)
temp <- sub("LSC-R, Life Stressor Checklist - Revised", 'LSC-R, 2010', temp)
temp <- sub("Davidson Trauma Scale (pre 2012)", 'Davidson', temp, fixed = T)
temp <- sub("PSSI, PTSD Symptom Scale Interview (Edna Foa)", 'PSSI', temp, fixed = T)
temp <- sub("Angels In The Nursery Interview", 'Angels', temp)
temp <- sub("CSQ-8, Client Satisfaction Survey", 'CSQ-8', temp)
temp <- sub("CES-D, Center for Epidemiological Studies Depression Scale", 'CES-D', temp)
temp <- sub("WPPSI, Wechsler Preschool and Primary Scale of Intelligence", 'WPPSI', temp)
temp <- sub("WPPSI, Wechsler Preschool and Primary Scale of Intelligence", 'WPPSI', temp)
wdata$SessionStatus <-paste(wdata$SessionStatus, temp, sep=", ")
rm(temp)

wdata$SessionStatus <-  sub('NA, NA$', NA, wdata$SessionStatus) ## change 'Target child' to 'TC'
wdata$SessionStatus <-  sub(', NA$', '', wdata$SessionStatus) ## change 'Target child' to 'TC'

wdata$SessionStatus <-  sub("Coordinated with agency to meet family", "Coordinated to meet family", wdata$SessionStatus) ## change 'Target child' to 'TC'

# wdata[,c("unique_id", "redcap_event_name","SessionStatus", "minContact")]

####################################################################
##                                                                ##
##          Session status, no show, show, cancel, "ass_show\     ##
##                                                                ##

present_tc <- as.character(apply(data[,grep('present_tc$', names(wdata), value = T)],1,function(x) ifelse(all(is.na(x)),NA,x[!is.na(x)])))
# cbind(present_tc, data[,grep('_present_tc', names(data), value = T)])
present_pc <- as.character(apply(data[,grep('present_pc$', names(wdata), value = T)],1,function(x) ifelse(all(is.na(x)),NA,x[!is.na(x)])))
present_cc <- as.character(apply(data[,grep('present_cc$', names(wdata), value = T)],1,function(x) ifelse(all(is.na(x)),NA,x[!is.na(x)])))
present_oc <- as.character(apply(data[,grep('present_oc$', names(wdata), value = T)],1,function(x) ifelse(all(is.na(x)),NA,x[!is.na(x)])))
present_onc <- as.character(apply(data[,grep('present_onc$', names(wdata), value = T)],1,function(x) ifelse(all(is.na(x)),NA,x[!is.na(x)])))
present_ocn <- as.character(apply(data[,grep('present_ocn$', names(wdata), value = T)],1,function(x) ifelse(all(is.na(x)),NA,x[!is.na(x)])))


present_tc[present_tc=="Present (mark below)"] <- "TC"
present_tc[present_tc=="Not present (mark below)"] <- NA

present_pc[present_pc=="Present (mark below)"] <- "PC"
present_pc[present_pc=="Not present (mark below)"] <- NA

present_cc[present_cc=="Present (mark below)"] <- "SC"
present_cc[present_cc=="Not present (mark below)"] <- NA

present_oc[present_oc=="Present (mark below)"] <- "OC"
present_oc[present_oc=="Not present (mark below)"] <- NA

present_onc[present_onc=="Present (mark below)"] <- "ONC"
present_onc[present_onc=="Not present (mark below)"] <- NA

present_ocn[present_ocn=="Present (mark below)"] <- "OCn"
present_ocn[present_ocn=="Not present (mark below)"] <- NA

wdata$Present <-  paste(present_tc, present_pc, sep=",")
wdata$Present <- sub('NA,NA', NA, wdata$Present)
wdata$Present <- sub('^NA,', '', wdata$Present)
wdata$Present <- sub(',NA$', '', wdata$Present)
wdata$Present <-  paste(wdata$Present, present_cc, sep=",")
wdata$Present <- sub('NA,NA', NA, wdata$Present)
wdata$Present <- sub('^NA,', '', wdata$Present)
wdata$Present <- sub(',NA$', '', wdata$Present)
wdata$Present <-  paste(wdata$Present, present_oc, sep=",")
wdata$Present <- sub('NA,NA', NA, wdata$Present)
wdata$Present <- sub('^NA,', '', wdata$Present)
wdata$Present <- sub(',NA$', '', wdata$Present)
wdata$Present <-  paste(wdata$Present, present_onc, sep=", ")
wdata$Present <- sub('NA, NA', NA, wdata$Present)
wdata$Present <- sub('^NA, ', '', wdata$Present)
wdata$Present <- sub(', NA$', '', wdata$Present)
wdata$Present <-  paste(wdata$Present, present_ocn, sep=",")
wdata$Present <- sub('NA,NA', NA, wdata$Present)
wdata$Present <- sub('^NA,', '', wdata$Present)
wdata$Present <- sub(',NA$', '', wdata$Present)
# wdata$Present

## TC, Target child
## PC, Primary caregiver
## SC, Secondary caregiver 
## OCa, Other caregivers
## OnC, Other non-caregiver 
## OCh, Other child(ren)

####################################################################
##                                                                ##
##        create places variables    ##
##                                                                ##
####################################################################

p <- as.character(apply(data[,grep('_place$', names(data), value = T)],1,function(x) ifelse(all(is.na(x)),NA,x[!is.na(x)])))
p.2 <- as.character(apply(data[,grep('_place_other', names(data), value = T)],1,function(x) ifelse(all(is.na(x)),NA,x[!is.na(x)])))
wdata$where <- as.factor( ifelse(is.na(p.2), as.character(p), as.character(p.2)) )
wdata$where <-  sub('Target child', 'TC', wdata$where) ## change 'Target child' to 'TC'

wdata$index <- wdata$redcap_event_name
wdata$index <- sub('Event[[:space:]]', '', wdata$index) ## Remove 'Event '
wdata$index[wdata$index == "Referral Form"] <- "000"
wdata$index <- as.numeric(wdata$index)

# sweet costum paste function
paste3 <- function(...,sep=", ") {
     L <- list(...)
     L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
     ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
                 gsub(paste0(sep,sep),sep,
                      do.call(paste,c(L,list(sep=sep)))))
     is.na(ret) <- ret==""
     ret
     }

Drop <- grep('cma_clo_reas_drop', names(wdata))
cmaDrop <- paste3(wdata[,Drop[1]], wdata[,Drop[2]])
Drop <- Drop[-c(1,2)]
for (x in Drop){
   if (class(wdata[,x])=="character") {
      cmaDrop <- paste3(cmaDrop, wdata[,x])
    } else {
    }
 }   

# sweet function to identify last item
FUN <- function(x) {
    if (all(is.na(x))) {
        first <- NA
    } else {
        first <- tail(na.omit(x), 1)
    }
    out <- as.character(rep(NA, length(x)))
    out[1] <- as.character(first)
    out
}


wdata$cmaDrop <- cmaDrop
wdata$cmaDrop <- factor(unlist(with(wdata, tapply(cmaDrop, unique_id, FUN=FUN))))

Comp <- grep('cma_clo_reas_comp', names(wdata))
cmaComp <- paste3(wdata[,Comp[1]], wdata[,Comp[2]])
Comp <- Comp[-c(1,2)]
for (x in Comp){
	 if (class(wdata[,x])=="character") {
      cmaComp <- paste3(cmaComp, wdata[,x])
    } else {
    }
 }   

wdata$cmaComp <- cmaComp
wdata$cmaComp <- factor(unlist(with(wdata, tapply(cmaComp, unique_id, FUN=FUN))))


wdata <- subset(wdata, select = c(date, redcap_event_name, event_type, SessionStatus, minContact, minProgressNote, minProcesNote, clinician_name, where, Present, therapist_name, rfdate, unique_id, case_nick, rfprogto, supervisor, PNeventType, PNclinician_name, index, cma_clo_reas, cma_clo_reo_status, CloOpn, cmaDrop, cmaComp, showTF))
