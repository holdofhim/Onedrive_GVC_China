

rm(list = ls())                         # Remove all
setwd("c:/onedrive/GVC_China/code/")    # Working Directory
rdata <- "c:/KDI/GVC/ICIO/Rdata/"       # Raw Rdata Directory
data  <- "c:/onedrive/GVC_China/data/"  # Saving Directory

library(matlab)
library(openxlsx)
library(foreign)


### Set-up ###

# Assign Sample period, Source Country, Responding Countries, Sector classification, and Variables of interest

period <- c(1995,2000,2005,2008,2009,2010,2011)       # Sample Period
cty.src <- "CHN"                                      # Source country should be a single country
iclass <- "iid3d"                                     # Industry classification to apply
load(paste0(rdata,"ICIO_",iclass,"_meta.RData"))      # load industry classification meta data


for (yr in period) {
      
      load(paste0(rdata,"ICIO_iid3d_matrix_",yr,".RData"))  
      rm(icio)
      chn <- which(substr(ciid, 1, 3) == cty.src)           
      if (yr==period[1]) y.chn <- y[chn] else y.chn <- cbind(y.chn, y[chn])
      
}
y.chn <- cbind(ciid[chn],y.chn)
colnames(y.chn) <- c("ciid", paste0("y",period))
write.dta(as.data.frame(y.chn), paste0(data,"CHN_y_1995-2011.dta"), convert.factors="string")

