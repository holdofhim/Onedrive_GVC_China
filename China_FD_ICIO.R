

# 2016-11-17

rm(list = ls())                                # Remove all
setwd("D:/OneDrive/GVC_China/Code/")           # Working Directory
data <- "D:/OneDrive/GVC_China/data/"          # Data Directory

library(matlab)
library(stringr)
library(RevoUtilsMath)
library(foreign)


# Load file
load(paste0(data,"ICIO_iid3d_matrix.RData"))    # These RData include all necessary data for analysis
CHN_FD <- zeros(SN,17)
cnames <- paste0("FD",c(1995:2011))
dimnames(CHN_FD) <- list(ciid,cnames)
for (i in 1:17) {
    CHN_FD[,i] <- rowSums(as.matrix(FD[i,,c("FD_CHN","FD_CHN1","FD_CHN2","FD_CHN3","FD_CHN4")]))
}
write.dta(as.data.frame(cbind(ciid,CHN_FD)), paste0(data,"CHN_FD_1995-2011.dta"), convert.factors="string")