

# 2017-10-09

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
sectors <- c("ndura","dura","ucon","svc","all")       # Sectors are classified based on durables vs. non-durables
vars    <- c("y","va","mx","fx","ex")


# Load file

load(paste0("ICIO_",iclass,"_matrix.RData"))          # These RData include all necessary data for analysis
cty.rsp <- cid


# Row positions of Source Country & Responding Countries in ICIO matrix

scty.row <- which(substr(ciid,1,3) == cty.src)                          # Source country's row position
names(cty.rsp) <- cty.rsp
rcty.row <- lapply(cty.rsp, function(cty) which(substr(ciid,1,3)==cty)) # Responding countries' row position


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
      FD.growth <- as.vector(FDhat)
      
      
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

      yhat  <- OS %*% FD.growth             # yhat (SN by # of broad sectors) = Output growth by ciid
      vahat <- VAS %*% FD.growth            # vahat = VA growth
      mxhat <- MXS %*% FD.growth            # mxhat = intermediate export growth
      fxhat <- FXS %*% FD.growth            # fxhat = final export growth
      exhat <- mx/ex*mxhat + fx/ex*fxhat    # exhat = Total Export growth
      
      yhat.j  <- lapply(cty.rsp, function(j) yhat[rcty.row[[j]]])  # yhat.j = Sector-level Output growth for country j
      vahat.j <- lapply(cty.rsp, function(j) vahat[rcty.row[[j]]])
      mxhat.j <- lapply(cty.rsp, function(j) mxhat[rcty.row[[j]]]) 
      fxhat.j <- lapply(cty.rsp, function(j) fxhat[rcty.row[[j]]]) 
      exhat.j <- lapply(cty.rsp, function(j) exhat[rcty.row[[j]]]) 
      
      
      # Obtain Aggregate Growth Rates
      
      gr.y  <- list()        # Growth rate of output
      gr.va <- list()        # Growth rate of VA
      gr.mx <- list()        # Growth rate of intermediate export
      gr.fx <- list()        # Growth rate of final export
      gr.ex <- list()        # Growth rate of total export
      gr.j  <- list()        # All growth rates above by Responding country
      
      for (k in 1:length(cty.rsp)) {          # Growth Rates of country k
            
            j <- rcty.row[[k]]                # Country's row position
            
            # Aggregate Growth Rate = weighted avg of sector-level growth rate
      
            gr.y[[k]]  <- c(yhat.j[[k]], sum((y[yr,,][j]/sum(y[yr,,][j]))*yhat.j[[k]]))
            gr.va[[k]] <- c(vahat.j[[k]], sum((va[yr,,][j]/sum(va[yr,,][j]))*vahat.j[[k]]))  
            gr.mx[[k]] <- c(mxhat.j[[k]], sum((mx[j]/sum(mx[j]))*mxhat.j[[k]]))
            gr.fx[[k]] <- c(fxhat.j[[k]], sum((fx[j]/sum(fx[j]))*fxhat.j[[k]]))
            gr.ex[[k]] <- c(exhat.j[[k]], sum((ex[j]/sum(ex[j]))*exhat.j[[k]]))
            
            
            # Combine all growth rates by responding country and stack by year

            gr.j[[k]] <- cbind(FDhat.name[[yr]], c(ciid[j],paste0(cty.rsp[k],"_TOT")), 
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

names(gr.j.yr) <- cty.rsp
gr.colname <- c("year", "ciid", paste0("gr_",vars,"_all"))

for (j in cty.rsp) {
      result <- as.data.frame(gr.j.yr[[j]])
      colnames(result) <- gr.colname
      addWorksheet(wb, j)
      writeData(wb, j, paste0(j, "'s Real Growth Rate due to Final Demand Change in ", cty.src, " (Unit: %)"))
      writeDataTable(wb, j, result, startRow=2, withFilter=FALSE, tableStyle="TableStyleMedium9")
}

saveWorkbook(wb, filename, overwrite=TRUE)



### Save Data in STATA format

result.all <- c()

for (j in cty.rsp) {
  result <- as.data.frame(gr.j.yr[[j]])
  colnames(result) <- gr.colname
  result.all <- rbind(result.all, result)
}

write.dta(result.all, paste0("FD_Real_Growth_Effect_",iclass,"_",cty.src,"_sc1.dta"), convert.factors="string")      


### End ###

