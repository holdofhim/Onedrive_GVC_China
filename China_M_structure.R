

rm(list = ls())                           # Remove all
setwd("D:/KDI/GVC/ICIO/Rcode")            # Working Directory
data  <- "D:/onedrive/GVC_china/data/"    # Saving data Directory
rdata <- "D:/KDI/GVC/ICIO/Rdata/"         # RData Directory

library(foreign)
library(matlab)
library(xlsx)

## ICIO includes 62 countries (plus 5 processing ID) with 34 industries over 1995-2011
period <- c(1995,2000,2005,2008,2009,2010,2011)


for (yr in period) {
      
      load(paste0(rdata,"ICIO_iid3d_matrix_",yr,".RData"))
      
      chn <- which(substr(ciid,1,3)=="CHN")
      kor <- which(substr(ciid,1,3)=="KOR")
      jpn <- which(substr(ciid,1,3)=="JPN")
      twn <- which(substr(ciid,1,3)=="TWN")

      M.chn     <- M[1:length(ciid), chn]
      M.chn.chn <- colSums(M.chn[chn,])
      M.chn.kor <- colSums(M.chn[kor,])
      M.chn.jpn <- colSums(M.chn[jpn,])
      M.chn.twn <- colSums(M.chn[twn,])
      M.chn.row <- colSums(M.chn)-M.chn.chn-M.chn.kor-M.chn.jpn-M.chn.twn
      M.chn.agg <- rbind(M.chn.chn,M.chn.jpn,M.chn.kor,M.chn.twn,M.chn.row) / (ones(5,1)%*%colSums(M.chn)) * 100
      M.chn.agg <- cbind(year=yr,cid=c("CHN","JPN","KOR","TWN","ROW"), M.chn.agg)
      if (yr==1995) M.china <- M.chn.agg   else M.china <- rbind(M.china, M.chn.agg)
}

write.xlsx(M.china, paste0(data,"China_M_structure.xlsx"), row.names=FALSE)
