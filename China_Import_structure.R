

rm(list = ls())                           # Remove all
setwd("C:/KDI/GVC/ICIO/Rcode")            # Working Directory
data  <- "c:/onedrive/GVC_china/data/"    # Saving data Directory
rdata <- "c:/KDI/GVC/ICIO/Rdata/"         # RData Directory

library(foreign)
library(matlab)
library(xlsx)

# year 1995

load(paste0(rdata,"ICIO_iid3d_matrix_1995.RData"))
pos <- sapply(cid.np, function(x) which(substr(ciid,1,3)==x))

# Value of Intermediate and Final Import by country
mim <- sapply(pos, function(x) sum(colSums(M[x,pos[["CHN"]]])))
fim <- sapply(pos, function(x) sum(FD[x,"FD_CHN"]))

# Shares
mimsh1995 <- mim/(mim+fim)*100
fimsh1995 <- fim/sum(fim)*100



# year 2010

load(paste0(rdata,"ICIO_iid3d_matrix_2010.RData"))

# Value of Intermediate and Final Import by country
mim <- sapply(pos, function(x) sum(colSums(M[x,pos[["CHN"]]])))
fim <- sapply(pos, function(x) sum(FD[x,"FD_CHN"]))

# Shares
mimsh2010 <- mim/(mim+fim)*100
fimsh2010 <- fim/sum(fim)*100

write.dta(as.data.frame(cbind(cid.np,mimsh1995,fimsh1995,mimsh2010,fimsh2010)), 
          paste0(data,"China_Import_Structure.dta"), convert.factors="string")

