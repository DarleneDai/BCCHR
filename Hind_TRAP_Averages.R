library(xlsx)
library(reshape2)
library(limma)
work.dir <-  "C:/Users/hsbihi/Dropbox/NO2_CHILD/microbiome_project/NO2"
setwd(work.dir)
data.resid <- read.xlsx("Hind_Resid_Geocoded.xlsx", 1)
data.cw <- read.delim("CrossWalkFinal.csv", header=T, sep=",")
data.exp.preg <- read.csv(file="child_adjusted_no2imputed.csv", header=T, sep=",")
data.exp.yr1 <- read.delim(file="child_adjusted_no2PostPartum.txt",header=T, sep="\t")

residhx <- merge(data.resid, data.cw, by="MaskID",all=T)
data.exp.preg[data.exp.preg==-9999]<- NA
data.exp.yr1[data.exp.yr1==-9999] <- NA

residhx$DOC_d <- as.numeric(strsplit2(residhx$CHILDActualDOC,"/")[,2])
residhx$DOC_m <- as.numeric(strsplit2(residhx$CHILDActualDOC,"/")[,1])
residhx$DOC_y <- as.numeric(strsplit2(residhx$CHILDActualDOC,"/")[,3])

residhx$DOB_d <- as.numeric(strsplit2(residhx$ChildActualDOB,"/")[,2])
residhx$DOB_m <- as.numeric(strsplit2(residhx$ChildActualDOB,"/")[,1])
residhx$DOB_y <- as.numeric(strsplit2(residhx$ChildActualDOB,"/")[,3])

# use mutate to compute trimester averages
# Shoud I use rowsums instead of sum...to try 
exp.trim<- mutate(data.exp.preg, no2_tm1= sum(C1:C91, na.rm=T)/91, no2_tm2= sum(C92:C183, na.rm=T)/91,  no2_tm3=sum(c181:C292, na.rm=T)/90)



