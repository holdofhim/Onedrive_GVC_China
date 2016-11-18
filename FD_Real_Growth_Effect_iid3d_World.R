

# 2016-10-30

rm(list = ls())                                # Remove all
setwd("C:/OneDrive/GVC_China/Rcode/")          # Working Directory
data <- "C:/OneDrive/GVC_China/data/"          # Data Directory

library(foreign)


### Step 1. 

period <- c(1995:2011)    # Sample Period should cover from the base-years to the years of interest
iclass <- "iid3d"                                  # Industry classification to apply
load(paste0(data,"ICIO_",iclass,"_matrix.RData"))  # These RData include all necessary data for analysis

for (year in period) {
      yr <- year-1994
      if (year==period[1]) FD.allyr <- cbind("year"=year, "ciid"=ciid, FD[yr,,]) 
      else FD.allyr <- rbind(FD.allyr, cbind("year"=year, "ciid"=ciid, FD[yr,,]))
}

write.dta(as.data.frame(FD.allyr), paste0(data,"FD_by_cid.dta"), convert.factors="string")



### Step 2. run the folloiwng do files

# If you need to re-estimate the growth rate of FD in CHN, KOR, JPN or USA, run FD_estimation_XXX.do
# Then, run FDhat_World_PWT.do



### Step 3. 

rm(list = ls())                                # Remove all
setwd("D:/OneDrive/GVC_China/Rcode/")          # Working Directory
data <- "D:/OneDrive/GVC_China/data/"          # Data Directory

library(foreign)
library(openxlsx)
library(matlab)

period <- c(2005,2006,2007,2008,2009,2010)  # Sample Period should be the base-years to use
cty.rsp <- list("KOR","CHN","DEU","JPN","TWN","USA") # For other countries, choose them together in this list

iclass <- "iid3d"                                    # Industry classification to apply
load(paste0(data,"ICIO_",iclass,"_meta.RData"))      # load industry classification meta data
vars   <- c("y","va","mx","fx","ex")



## Import estimate of FD Growth rate

d.year <- paste0(period[2]-period[1], "-year") 
FDhat <- read.dta(paste0(data,"FDhat_World.dta"))
ciid.ord <- FDhat[,1]    # ciid row position
FDhat <- FDhat[,2:(2+length(period)-1)]
rownames(FDhat) <- ciid.ord
FDhat <- FDhat[ciid.np,] # change order of ciid according to ICIO Table


# change some countries' FD growth rate

KOR <- paste0("KOR_",iid)
CHN <- paste0("CHN_",iid)
JPN <- paste0("JPN_",iid)
USA <- paste0("USA_",iid)

FDhat[KOR,] <- read.xlsx(paste0(data,"FD_estimation_KOR.xlsx"), sheet=paste0(d.year,"_FD_growth"), 
                         cols=c(1:(length(period)+1)), rowNames=TRUE, startRow=2)
FDhat[CHN,] <- read.xlsx(paste0(data,"FD_estimation_CHN.xlsx"), sheet=paste0(d.year,"_FD_growth"), 
                         cols=c(1:(length(period)+1)), rowNames=TRUE, startRow=2)
FDhat[JPN,] <- read.xlsx(paste0(data,"FD_estimation_JPN.xlsx"), sheet=paste0(d.year,"_FD_growth"), 
                         cols=c(1:(length(period)+1)), rowNames=TRUE, startRow=2)
FDhat[USA,] <- read.xlsx(paste0(data,"FD_estimation_USA.xlsx"), sheet=paste0(d.year,"_FD_growth"), 
                         cols=c(1:(length(period)+1)), rowNames=TRUE, startRow=2)
FDhat <- as.list(FDhat)

names(FDhat) <- period
FDhat.name  <- list("FDhat2006_05","FDhat2007_06","FDhat2008_07","FDhat2009_08","FDhat2010_09","FDhat2011_10")
names(FDhat.name) <- period



## Prepare an excel file

note <- c(paste0("(1) This file calculates the real growth of output (y), value added (va), intermediate export (mx), final export (fx), and total export (ex) due to the real FD change in the World."),
          "(2) All units are in percentage term.", 
          "(3) 34 original industries in the ICIO table are aggregated to 17 industries as below.",
          "(4) Durable = 22x, Non-durable = 10x & 21x, Utility & Construction = 31x, Service = 32x")

wb <- createWorkbook()
addWorksheet(wb, "Note")
writeData(wb, "Note", note)
writeDataTable(wb, "Note", data.frame(iid,iid.eng), startRow=5, withFilter=FALSE)
filename <- paste0("FD_Real_Growth_Effect_",iclass,"_World_",d.year,".xlsx")


# Prepare a List to save calculated elasticities by responding country & year
eps.j.yr <- list()      



## Estimation for each year

# Load file
load(paste0(data,"ICIO_",iclass,"_matrix.RData"))    # These RData include all necessary data for analysis


# Row positions of Source Country & Responding Countries in ICIO matrix

scty.row <- which(substr(ciid, 1, 3) == cty.src)                        # Source country's row position
names(cty.rsp) <- cty.rsp
rcty.row <- lapply(cty.rsp, function(cty) which(substr(ciid,1,3)==cty)) # Responding countries' row position


for (year in period) {
      
      yr <- year-1994
      
      ## Insert sector-level FD growth rate
      
      FD.growth <- FDhat[[as.character(year)]]
      names(FD.growth) <- ciid

      
      ## Obtain Output & Value-added Share Matrix
      
      Y.alloc <- LeonInv[yr,,] %*% FDD[yr,,]        # Y.alloc = Output Allocation Matrix
      dimnames(Y.alloc) <- list(ciid, ciid)         # FDD = Final Demand Diagonalized Matrix
      
      yInv <- 1/y.nzero[yr,,]
      names(yInv) <- ciid
      OS <- diag(yInv) %*% Y.alloc                  # OS = Output Share Matrix (S matrix in Bems et al. 2010)
      dimnames(OS) <- list(ciid, ciid)
      
      VA.alloc <- diag(r[yr,,]) %*% Y.alloc         # VA.alloc = Value-added Allocation Matrix
      dimnames(VA.alloc) <- list(ciid, ciid)
      
      vaInv <- 1/va.nzero[yr,,]
      names(vaInv) <- ciid
      VAS <- diag(vaInv) %*% VA.alloc               # VAS = Value-added Share Matrix
      dimnames(VAS) <- list(ciid, ciid)
      
      
      # Obtain Export & Import Share
      
      mx <- rowSums(MX[yr,,]) # mx (SNx1) = Intermediate Export by ciid (do not include home trade values)
      mx[mx==0] <- 0.01       # Set the minimum export equal to 0.01 to avoid zero division
      fx <- rowSums(FX[yr,,]) # fx = Final Export by ciid
      fx[fx==0] <- 0.01
      ex  <- mx + fx          # ex = Total Export by ciid
      
      MXS <- MX[yr,,] / mx    # MXS = Intermediate Export Share
      FX.diag <- NULL         # FX.diag = Final Export diagonalized matrix
      for (n in c(1:N)) {
            FX.n    <- diag(FX[yr,,n]) %*% (ones(N,1)%x%eye(S))
            FX.diag <- cbind(FX.diag, FX.n)
      }
      FXS <- FX.diag / fx     # FXS = Final Export Share
      
      
      
      ### Calculating Elasticity by Broad Sectors & Countries ###
      
      # Country-by-Sector level Growth Rate
      yhat  <- lapply(FD.growth, function(x) OS %*% x)      # yhat (SN by # of broad sectors) = Output growth by ciid
      vahat <- lapply(FD.growth, function(x) VAS %*% x)     # vahat = VA growth
      mxhat <- lapply(yhat,      function(x) MXS %*% x)     # mxhat = intermediate export growth
      fxhat <- lapply(FD.growth, function(x) FXS %*% x)     # fxhat = final export growth
      
      yhat  <- as.data.frame(yhat)
      vahat <- as.data.frame(vahat)
      mxhat <- as.data.frame(mxhat)      
      fxhat <- as.data.frame(fxhat)
      exhat <- mx/ex*mxhat + fx/ex*fxhat                    # exhat = Total Export growth
      
      yhat.j  <- lapply(cty.rsp, function(j) yhat[rcty.row[[j]],])  # yhat.j = Sector-level Output growth for country j
      vahat.j <- lapply(cty.rsp, function(j) vahat[rcty.row[[j]],])
      mxhat.j <- lapply(cty.rsp, function(j) mxhat[rcty.row[[j]],]) 
      fxhat.j <- lapply(cty.rsp, function(j) fxhat[rcty.row[[j]],]) 
      exhat.j <- lapply(cty.rsp, function(j) exhat[rcty.row[[j]],]) 
      
      
      # Obtain Aggregate Growth Rates
      
      eps.y  <- list()        # Elasticity of output
      eps.va <- list()        # Elasticity of VA
      eps.mx <- list()        # Elasticity of intermediate export
      eps.fx <- list()        # Elasticity of final export
      eps.ex <- list()        # Elasticity of total export
      eps.j  <- list()        # All Elasticities above by Responding country
      
      for (k in 1:length(cty.rsp)) {
            
            j <- rcty.row[[k]]     # Country's row position
            
            # Aggregate Growth Rate = weighted avg of sector-level growth rate
            yhat.j.agg  <- colSums((y[yr,,][j]/sum(y[yr,,][j]))*yhat.j[[k]])
            vahat.j.agg <- colSums((va[yr,,][j]/sum(va[yr,,][j]))*vahat.j[[k]])  
            mxhat.j.agg <- colSums((mx[j]/sum(mx[j]))*mxhat.j[[k]])
            fxhat.j.agg <- colSums((fx[j]/sum(fx[j]))*fxhat.j[[k]])
            exhat.j.agg <- colSums((ex[j]/sum(ex[j]))*exhat.j[[k]])
            
            # Stack sector growth rate and aggregate growth rate under in a row
            eps.y[[k]]  <- rbind(yhat.j[[k]],  "AGG.Economy"=yhat.j.agg)   # eps.y = Elasticity of Output for country j
            eps.va[[k]] <- rbind(vahat.j[[k]], "AGG.Economy"=vahat.j.agg)  # Elasticity of VA
            eps.mx[[k]] <- rbind(mxhat.j[[k]], "AGG.Economy"=mxhat.j.agg)  # Elasticity of intermediate export
            eps.fx[[k]] <- rbind(fxhat.j[[k]], "AGG.Economy"=fxhat.j.agg)  # Elasticity of final export 
            eps.ex[[k]] <- rbind(exhat.j[[k]], "AGG.Economy"=exhat.j.agg)  # Elasticity of total export
            
            # Combine all elasticities by responding country and stack by year
            eps.j[[k]] <- cbind(year, c(ciid[j],paste0(cty.rsp[k],"_TOT")),
                                eps.y[[k]], eps.va[[k]], eps.mx[[k]], eps.fx[[k]], eps.ex[[k]])
            if (year==period[1]) eps.j.yr[[k]] <- eps.j[[k]]   else eps.j.yr[[k]] <- rbind(eps.j.yr[[k]], eps.j[[k]])
      }
}



## Convert eps.j.yr to dataframe for each country and save the resutls to the xlsx file

names(eps.j.yr) <- cty.rsp
eps.colname <- c("year", "ciid", paste0("gr_",vars,"_all"))

for (j in cty.rsp) {
      
      result <- as.data.frame(eps.j.yr[[j]])
      colnames(result) <- eps.colname
      addWorksheet(wb, j)
      writeData(wb, j, paste0(j, "'s Real Growth Rate due to Final Demand Change in the World (Unit: %)"))
      writeDataTable(wb, j, result, startRow=2, withFilter=FALSE, tableStyle="TableStyleMedium9")
}

saveWorkbook(wb, paste0(data, filename), overwrite=TRUE)



## Write Data in the prepared workbook and save it

result.all <- c()

for (j in cty.rsp) {
      result <- as.data.frame(eps.j.yr[[j]])
      result.all <- rbind(result.all, result)
}
colnames(result.all) <- eps.colname
write.dta(result.all, paste0(data,"FD_elasticity_",iclass,"_",cty.src,".dta"), convert.factors="string")      


### End ###

