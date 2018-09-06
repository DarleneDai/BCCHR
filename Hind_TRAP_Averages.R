library(xlsx)
library(reshape2)
library(limma)
library(dplyr)
work.dir <-  "C:/Users/hsbihi/Dropbox/NO2_CHILD/microbiome_project/NO2"
setwd(work.dir)

# read in the residential history geocoded addresses
data.resid <- read.xlsx("Hind_Resid_Geocoded.xlsx", 1)
# read in the crosswalk file
data.cw <- read.delim("CrossWalkFinal.csv", header=T, sep=",")
# read in the exposure files during pregnancy and first year of life
data.exp.preg <- read.csv(file="child_adjusted_no2imputed.csv", header=T, sep=",", stringsAsFactors = T)
data.exp.preg[data.exp.preg==-9999]<- NA
exp.preg<- data.exp.preg %>% select(-(X:X.72))
data.exp.yr1 <- read.delim(file="child_adjusted_no2PostPartum.txt",header=T, sep="\t")
data.exp.yr1[data.exp.yr1==-9999] <- NA

# merge data to link MaskedIDs to SubjectIDs
residhx <- merge(data.resid, data.cw, by="MaskID",all=T)
colnames(residhx)[colnames(residhx)=="NA."] <- "Status"


residhx$DOC_d <- as.numeric(strsplit2(residhx$CHILDActualDOC,"/")[,2])
residhx$DOC_m <- as.numeric(strsplit2(residhx$CHILDActualDOC,"/")[,1])
residhx$DOC_y <- as.numeric(strsplit2(residhx$CHILDActualDOC,"/")[,3])

residhx$DOB_d <- as.numeric(strsplit2(residhx$ChildActualDOB,"/")[,2])
residhx$DOB_m <- as.numeric(strsplit2(residhx$ChildActualDOB,"/")[,1])
residhx$DOB_y <- as.numeric(strsplit2(residhx$ChildActualDOB,"/")[,3])

# use mutate to compute trimester averages
require(dplyr)

exp.preg<- exp.preg %>% 
  mutate(no2.tm1 = rowMeans(.[2:91], na.rm=T)) %>% 
  mutate(no2.tm2 = rowMeans(.[92:183], na.rm=T)) %>% 
  mutate(no2.tm3= rowMeans(.[184:292], na.rm=T)) %>%
  mutate (no2.preg=rowMeans(.[2:292], na.rm=T))

exp.yr1<- data.exp.yr1 %>% 
  mutate(no2.y1t1= rowMeans(.[2:91], na.rm=T))%>% 
  mutate(no2.y1t2= rowMeans(.[92:183], na.rm=T))%>%
  mutate(no2.y1t3=rowMeans(.[184:275], na.rm=T))%>%
  mutate(no2.y1t4=rowMeans(.[276:366], na.rm=T))%>%
  mutate(no2.y1= rowMeans(.[2:366], na.rm=T))
