
# 2017-09-06
# Calculating Upstream & Downstream Effect


rm(list = ls())                         # Remove all
setwd("D:/onedrive/GVC_China/code/")    # Working Directory
rdata <- "D:/KDI/GVC/ICIO/Rdata/"       # Raw Rdata Directory
data  <- "D:/onedrive/GVC_China/data/"  # Saving Directory

library(foreign)
library(matlab)
library(openxlsx)
library(readxl)

### Set-up ###

# Sample period, Source Country, Responding Countries, Sector classification, and Variables of interest

period <- c(2008,2009,2010)  # Sample Period should be the base-years to use
iclass <- "iid3d"                                     # Industry classification to apply
load(paste0(rdata,"ICIO_",iclass,"_meta.RData"))      # load industry classification meta data
cty.src <- "CHN"                                      # Source country should be a single country
cty.rsp <- cid.np                                     # Responding countries
sectors <- c("ndura","dura","ucon","svc","all")       # Sectors are classified based on durables vs. non-durables
vars    <- c("y","va","mx","fx","ex")



# Prepare an excel output file

note <- c(paste0("(1) This file calculates 10 times the elasticities of output (y), value added (va) to the unit FD change in ",cty.src,"."),
          "(2) All units are in percentage term.", 
          "(3) 34 original industries in the ICIO table are aggregated to industries as below.",
          "(4) Durable = 22x, Non-durable = 10x & 21x, Utility & Construction = 31x, Service = 32x")

wb <- createWorkbook()
addWorksheet(wb, "Note")
writeData(wb, "Note", note)
writeDataTable(wb, "Note", data.frame(iid, iid.eng, iid.kr), startRow=5, withFilter=FALSE)
filename <- paste0("FD_Elasticity_Up&Down_",iclass,"_",cty.src,"_by_year.xlsx")



# Prepare a List to save calculated elasticities by responding country & year
eps.j.yr <- list()      



### Run for Elasticity Calculation

for (yr in period) {
      
      # Load file
      #if (yr>2011) load(paste0(rdata,"ICIO_",iclass,"_matrix_2011.RData"))
      #else load(paste0(rdata,"ICIO_",iclass,"_matrix_",yr,".RData"))    # These RData include all necessary data for analysis
      load(paste0(rdata,"ICIO_",iclass,"_matrix_2011.RData"))
      rm(icio)
      cty.rsp <- cid.np
      
      
      
      # Row positions of Source Country & Responding Countries in ICIO matrix
      scty.row <- which(substr(ciid.np, 1, 3) == cty.src)                     # Source country's row position
      names(cty.rsp) <- cty.rsp
      rcty.row <- lapply(cty.rsp, function(cty) which(substr(ciid,1,3)==cty)) # Responding countries' row position
      
      
      # Sector Classification: nonondurable, durable, utilies & construction, service
      FD.sector <- zeros(S,length(sectors))
      colnames(FD.sector) <- sectors
      FD.sector[,1] <- ifelse(as.integer(iid/10)<=21, 10, 0)      # Non-durable
      FD.sector[,2] <- ifelse(as.integer(iid/10)==22, 10, 0)      # Durable
      FD.sector[,3] <- ifelse(as.integer(iid/10)==31, 10, 0)      # Utilities & Construction
      FD.sector[,4] <- ifelse(as.integer(iid/10)==32, 10, 0)      # Service
      FD.sector[,5] <- 10                                         # Entire Sectors
      
      FD.growth <- as.data.frame(zeros(SN.np, length(sectors)))
      dimnames(FD.growth) <- list(ciid.np, sectors)
      FD.growth[scty.row,] <- FD.sector
      
      
      ## Obtain Output & Value-added Share Matrix
      
      # Hhat = Demand shock matrix
      Ahat <- M / (ones(SN,1)%*%t(y))
      dimnames(Ahat) <- list(ciid, ciid)
      Leonhat <- eye(SN)-Ahat
      Hhat <- solve(Leonhat)
      
      Y.alloc <- Hhat %*% t(FDD)                    # Y.alloc = Output Allocation Matrix
      dimnames(Y.alloc) <- list(ciid.np, ciid)      # FDD = Final Demand Diagonalized Matrix
      
      yInv <- zeros(SN,1)
      names(yInv) <- ciid
      yInv <- 1/y
      OS <- Y.alloc %*% diag(yInv)                  # OS = Output Share Matrix (S matrix in Bems et al. 2010)
      dimnames(OS) <- list(ciid.np, ciid)
      
      VA.alloc <- Y.alloc %*% diag(r)               # VA.alloc = Value-added Allocation Matrix
      dimnames(VA.alloc) <- list(ciid.np, ciid)
      
      vaInv <- zeros(SN,1)
      names(vaInv) <- ciid
      vaInv <- 1/va
      VAS <- VA.alloc %*% diag(vaInv)               # VAS = Value-added Share Matrix
      dimnames(VAS) <- list(ciid.np, ciid)
      
      
      
      ### Calculating Elasticity by Broad Sectors & Countries ###
      
      # Country-by-Sector level Growth Rate
      yhat  <- lapply(FD.growth, function(x) Down %*% x)      # yhat (SN by # of broad sectors) = Output growth by ciid
      vahat <- lapply(FD.growth, function(x) Down %*% x)     # vahat = VA growth

      yhat  <- as.data.frame(yhat)
      vahat <- as.data.frame(vahat)

      yhat.j  <- lapply(cty.rsp, function(j) yhat[rcty.row[[j]],])  # yhat.j = Sector-level Output growth for country j
      vahat.j <- lapply(cty.rsp, function(j) vahat[rcty.row[[j]],])

      
      # Obtain Aggregate Growth Rates
      
      eps.y  <- list()        # Elasticity of output
      eps.va <- list()        # Elasticity of VA
      eps.j  <- list()        # All Elasticities above by Responding country
      
      for (k in 1:length(cty.rsp)) {
            
            j <- rcty.row[[k]]     # Country's row position
            
            # Aggregate Growth Rate = weighted avg of sector-level growth rate
            yhat.j.agg  <- colSums((y[j]/sum(y[j]))*yhat.j[[k]])
            vahat.j.agg <- colSums((va[j]/sum(va[j]))*vahat.j[[k]])  

            # Stack sector growth rate and aggregate growth rate under in a row
            eps.y[[k]]  <- rbind(yhat.j[[k]],  "AGG.Economy"=yhat.j.agg)   # eps.y = Elasticity of Output for country j
            eps.va[[k]] <- rbind(vahat.j[[k]], "AGG.Economy"=vahat.j.agg)  # Elasticity of VA

            # Combine all elasticities by responding country and stack by year
            eps.j[[k]] <- cbind(yr, c(ciid[j],paste0(cty.rsp[k],"_TOT")),
                                eps.y[[k]], eps.va[[k]], eps.mx[[k]], eps.fx[[k]], eps.ex[[k]])
            if (yr==period[1]) eps.j.yr[[k]] <- eps.j[[k]]   else eps.j.yr[[k]] <- rbind(eps.j.yr[[k]], eps.j[[k]])
      }
}


# Convert eps.j.yr to dataframe for each country and save the resutls to the xlsx file

names(eps.j.yr) <- cty.rsp
eps.colname <- c("year", "ciid", paste("e",rep(vars, each=length(sectors)),sectors,sep="_"))

for (j in cty.rsp) {
      
      result <- as.data.frame(eps.j.yr[[j]])
      colnames(result) <- eps.colname
      addWorksheet(wb, j)
      writeData(wb, j, paste0(j, "'s Elasticities to Final Demand Change in ", cty.src, " (Unit: %)"))
      writeDataTable(wb, j, result, startRow=2, withFilter=FALSE, tableStyle="TableStyleMedium9")
}

saveWorkbook(wb, paste0(data,filename), overwrite=TRUE)



### Save Data in STATA format

result.all <- c()

for (j in cty.rsp) {
      result <- as.data.frame(eps.j.yr[[j]])
      colnames(result) <- eps.colname
      result.all <- rbind(result.all, result)
}

write.dta(result.all, paste0(data,"FD_elasticity_Up&Down_",iclass,"_",cty.src,"_by_year.dta"), convert.factors="string")


### End ###


