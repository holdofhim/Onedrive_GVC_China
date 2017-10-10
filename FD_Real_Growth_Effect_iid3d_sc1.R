

# 2017-10-10

rm(list = ls())                       # Remove all
setwd("D:/OneDrive/GVC_China/data/")  # Working Directory

library(matlab)
library(plyr)
library(parallel)
library(openxlsx)
library(foreign)


### Set-up ###

# Sample period, Source Country, Responding Countries, Sector classification, and Variables of interest

period <- c(1995:2010)                                # Sample Period should be the base-years to use
cty.src <- "CHN"                                      # Source country should be a single country
iclass <- "iid3d"                                     # Industry classification to apply
iid3d <- c(rep("ndura",5), rep("dura",5), "ucon", rep("svc",6))
sectors <- c("ndura","dura","ucon","svc","all")       # Sectors are classified based on durables vs. non-durables
vars    <- c("y","va","mx","fx","ex")



# Load file

load(paste0("ICIO_",iclass,"_matrix.RData"))          # These RData include all necessary data for analysis


# Row positions of Source Country & Responding Countries in ICIO matrix

scty.row <- which(substr(ciid,1,3) == cty.src)                          # Source country's row position
rcty.row <- lapply(cid.np, function(cty) which(substr(ciid,1,3)==cty))  # Responding countries' row position
names(rcty.row) <- cid.np


# Import Price Index by sector and Exchange Rate from Excel file

d.year <- paste0(period[2]-period[1], "-year")
importfile <- paste0("FD_estimation_",cty.src,".xlsx")
price <- as.list(read.xlsx(importfile, sheet="Price_Index", rowNames=TRUE, startRow=2))
xrate <- as.matrix(read.xlsx(importfile, sheet="EX_rate", colNames=FALSE, rows=3:18, cols=3))
FDhat.name <- as.list(read.xlsx(importfile, sheet=paste0(d.year,"_FD_growth"),
                                colNames=FALSE, startRow=2, rows=1:2, cols=(1+1:length(period))))


# Some Preparations 

FD.src <- t(FD.np[,,paste0("FD_",cty.src)])  # FD of the source country
FD.src[FD.src<1] <- 1   # Replace the value of FD into 1 to avoid zero division
gr.j.yr <- list()       # a List to save calculated growth rates by responding country & year   



### Estimation for Source Country's impact

for (year in period) {

      yr <- year-1994
      
      # Calculate real FD growth rate in China (Price Index is assumed to be same across countries)
      
      FDhat <- (FD.src[,yr+1]/FD.src[,yr]*xrate[yr]/rep(price[[yr]],N)-1)*100

      FD.growth <- as.data.frame(zeros(SN, length(sectors)))
      dimnames(FD.growth) <- list(ciid,sectors)
      FD.growth[,1] <- ifelse(as.integer(rep(iid,N)/10)<=21, FDhat, 0)  # Non-durables
      FD.growth[,2] <- ifelse(as.integer(rep(iid,N)/10)==22, FDhat, 0)  # Durables
      FD.growth[,3] <- ifelse(as.integer(rep(iid,N)/10)==31, FDhat, 0)  # Utility and Construction
      FD.growth[,4] <- ifelse(as.integer(rep(iid,N)/10)==32, FDhat, 0)  # Services
      FD.growth[,5] <- FDhat                                            # All Industries
      
      
      # Obtain Output & Value-added Share Matrix
      
      FDD[yr,,] <- diag(FD.src[,yr])            # FDD = Final Demand Diagonalized Matrix
      Y.alloc <- LeonInv[yr,,] %*% FDD[yr,,]    # Y.alloc = Output Allocation Matrix
      dimnames(Y.alloc) <- list(ciid, ciid)         
      
      yInv <- 1/y.nzero[yr,,]
      names(yInv) <- ciid
      OS <- diag(yInv) %*% Y.alloc              # OS = Output Share Matrix (S matrix in Bems et al. 2010)
      dimnames(OS) <- list(ciid, ciid)
      
      VA.alloc <- diag(r[yr,,]) %*% Y.alloc     # VA.alloc = Value-added Allocation Matrix
      dimnames(VA.alloc) <- list(ciid, ciid)
      
      vaInv <- 1/va.nzero[yr,,]
      names(vaInv) <- ciid
      VAS <- diag(vaInv) %*% VA.alloc           # VAS = Value-added Share Matrix
      dimnames(VAS) <- list(ciid, ciid)
      
      
      # Obtain Export & Import Share
      
      mx <- rowSums(MX[yr,,]) # mx (SNx1) = Intermediate Export by ciid (do not include home trade values)
      mx[mx==0] <- 1          # Set the minimum export equal to 1 to avoid zero division
      fx <- rowSums(FX[yr,,]) # fx = Final Export by ciid
      fx[fx==0] <- 1
      ex  <- mx + fx          # ex = Total Export by ciid
      
      MXS <- MX[yr,,] / mx    # MXS = Intermediate Export Share
      FX.diag <- NULL         # FX.diag = Final Export diagonalized matrix
      for (n in c(1:N)) {
            FX.n    <- diag(FX[yr,,n]) %*% (ones(N,1)%x%eye(S))
            FX.diag <- cbind(FX.diag, FX.n)
      }
      FXS <- FX.diag / fx     # FXS = Final Export Share
      
      
      
      ## Calculating growth rates by Sectors & Countries ###
      
      # Country-by-Sector level Growth Rate

      yhat  <- lapply(FD.growth, function(x) OS %*% x)      # yhat (SN by # of broad sectors) = Output growth by ciid
      vahat <- lapply(FD.growth, function(x) VAS %*% x)     # vahat = VA growth
      mxhat <- lapply(yhat,      function(x) MXS %*% x)     # mxhat = Intermediate Export growth
      fxhat <- lapply(FD.growth, function(x) FXS %*% x)     # fxhat = Final Export growth
      
      yhat  <- as.data.frame(yhat)
      vahat <- as.data.frame(vahat)
      mxhat <- as.data.frame(mxhat)      
      fxhat <- as.data.frame(fxhat)
      exhat <- mx/ex*mxhat + fx/ex*fxhat                    # exhat = Total Export growth

      yhat.j  <- lapply(cid.np, function(j) yhat[rcty.row[[j]],])  # yhat.j = Sector-level Output growth for country j
      vahat.j <- lapply(cid.np, function(j) vahat[rcty.row[[j]],])
      mxhat.j <- lapply(cid.np, function(j) mxhat[rcty.row[[j]],]) 
      fxhat.j <- lapply(cid.np, function(j) fxhat[rcty.row[[j]],]) 
      exhat.j <- lapply(cid.np, function(j) exhat[rcty.row[[j]],]) 
      
      
      # Obtain Aggregate Growth Rates
      
      gr.y  <- list()        # Growth rate of output
      gr.va <- list()        # Growth rate of VA
      gr.mx <- list()        # Growth rate of intermediate export
      gr.fx <- list()        # Growth rate of final export
      gr.ex <- list()        # Growth rate of total export
      gr.j  <- list()        # All growth rates above by Responding country
      
      for (k in 1:N.np) {          # Growth Rates of country k
            
            j <- rcty.row[[k]]     # Country's row position
            
            # Aggregate Growth Rate = weighted avg of sector-level growth rate
      
            yhat.j.agg  <- colSums((y[yr,,][j]/sum(y[yr,,][j]))*yhat.j[[k]])
            vahat.j.agg <- colSums((va[yr,,][j]/sum(va[yr,,][j]))*vahat.j[[k]])  
            mxhat.j.agg <- colSums((mx[j]/sum(mx[j]))*mxhat.j[[k]])
            fxhat.j.agg <- colSums((fx[j]/sum(fx[j]))*fxhat.j[[k]])
            exhat.j.agg <- colSums((ex[j]/sum(ex[j]))*exhat.j[[k]])
            
            
            # Stack sector growth rate and aggregate growth rate under in a row

            gr.y[[k]]  <- rbind(yhat.j[[k]], "AGG.Economy"=yhat.j.agg)
            gr.va[[k]] <- rbind(vahat.j[[k]], "AGG.Economy"=vahat.j.agg)
            gr.mx[[k]] <- rbind(mxhat.j[[k]], "AGG.Economy"=mxhat.j.agg)
            gr.fx[[k]] <- rbind(fxhat.j[[k]], "AGG.Economy"=fxhat.j.agg)
            gr.ex[[k]] <- rbind(exhat.j[[k]], "AGG.Economy"=exhat.j.agg)
            

            # Combine all growth rates by responding country and stack by year

            gr.j[[k]] <- cbind(FDhat.name[[yr]], c(ciid[j],paste0(cid.np[k],"_TOT")), 
                                gr.y[[k]], gr.va[[k]], gr.mx[[k]], gr.fx[[k]], gr.ex[[k]])
            if (year==period[1]) gr.j.yr[[k]] <- gr.j[[k]]   else gr.j.yr[[k]] <- rbind(gr.j.yr[[k]], gr.j[[k]])

      }
}



### Prepare an excel file

note <- c(paste0("(1) This file calculates the real growth of output (y), value added (va), intermediate export (mx), final export (fx), and total export (ex) due to the real FD change in ",cty.src,"."),
      "(2) Sensitivity Check 1: Assumption 3 is relaxed.",
      "(3) All units are in percentage term.", 
      "(4) 34 original industries in the ICIO table are aggregated to 17 industries as below.",
      "(5) Durable = 22x, Non-durable = 10x & 21x, Utility & Construction = 31x, Service = 32x")

wb <- createWorkbook()
addWorksheet(wb, "Note")
writeData(wb, "Note", note)
writeDataTable(wb, "Note", data.frame(iid,iid.eng), startRow=6, withFilter=FALSE)
filename <- paste0("FD_Real_Growth_Effect_",iclass,"_",cty.src,"_sc1.xlsx")



### Convert gr.j.yr to dataframe for each country and save the resutls to the xlsx file

names(gr.j.yr) <- cid.np
gr.colname <- c("year", "ciid", paste("gr",rep(vars, each=length(sectors)),sectors,sep="_"))

for (j in cid.np) {
      result <- as.data.frame(gr.j.yr[[j]])
      colnames(result) <- gr.colname
      addWorksheet(wb, j)
      writeData(wb, j, paste0(j, "'s Real Growth Rate due to Final Demand Change in ", cty.src, " (Unit: %)"))
      writeDataTable(wb, j, result, startRow=2, withFilter=FALSE, tableStyle="TableStyleMedium9")
}

saveWorkbook(wb, filename, overwrite=TRUE)



### Save Data in STATA format

result.all <- c()

for (j in cid.np) {
  result <- as.data.frame(gr.j.yr[[j]])
  colnames(result) <- gr.colname
  result.all <- rbind(result.all, result)
}

write.dta(result.all, paste0("FD_Real_Growth_Effect_",iclass,"_",cty.src,"_sc1.dta"), convert.factors="string")      


### End ###

