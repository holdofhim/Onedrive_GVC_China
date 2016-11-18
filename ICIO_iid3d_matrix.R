

## Ver2, 2016-10-30

rm(list = ls())                                # Remove all
setwd("D:/OneDrive/GVC_China/Rcode/")          # Working Directory
Rdata <- "D:/Datasets/OECD-ICIO/Rdata/"        # OECD ICIO Table Rdata Directory
data <- "D:/OneDrive/GVC_China/data/"          # Data Directory

library(openxlsx)
library(matlab)
library(stringr)


### Import data from ICIO Table and assign IDs

load(paste0(Rdata,"DATA.ICIOeconX.Rdata"))              # Output Vector (17x2346) where 2346=69x34
load(paste0(Rdata,"DATA.ICIOeconVA.Rdata"))             # Value-added Vector (17x2346)
load(paste0(Rdata,"DATA.ICIOeconZ.Rdata"))              # M matrix (17x2346x2346)
load(paste0(Rdata,"DATA.ICIOeconFDTTLdisc.Rdata"))      # Final Demand Matrix (17x2346x70)

ciid.old <- colnames(DATA.ICIOeconZ[1,,])
SN.old   <- length(ciid.old)
id.old   <- sapply(strsplit(ciid.old, "[_]"), as.array)

cid    <- unique(id.old[1,])
N      <- length(cid)
cid[(N-6):N] <- c("MEX1","MEX2","MEX3","CHN1","CHN2","CHN3","CHN4")
cid.np <- unique(substr(cid,1,3))
N.np   <- length(cid.np)

iid.old <- unique(str_sub(id.old[2,],-3,-1))
S.old   <- length(iid.old)

ciid.old <- paste0(rep(cid,each=S.old),"_",iid.old)
id.old   <- sapply(strsplit(ciid.old, "[_]"), as.array)



### Import new classification data from ICIO_ciid and apply to ICIO matrix

# Identify original industry positions
irow.old <- id.old[2,]  # irow.old = country-matched industry's original row position
irow.old.list <- sapply(iid.old, function(i) which(irow.old==i)) # list of original 34 industries' row positions


# Import new classification file to apply
iclass  <- "iid3d"
iid.class <- as.matrix(read.xlsx(paste0(data,"ICIO_ciid_classification.xlsx"), iclass))
iid.new <- iid.class[,"post"]                    
iid     <- as.numeric(unique(iid.new))           # the new iid to use
iid.eng <- unique(iid.class[,"ename"])


# Apply new classification to old position                  
irow.new <- irow.old                             # irow.new = re-classified industry's row position
for (i in 1:length(iid.old)) irow.new[irow.old.list[,i]] <- iid.new[i] # replace original iid positions into the new ones 


# Generate new ciid and identify its position in the original data
ciid.new <- paste0(id.old[1,], "_", irow.new)    # ciid.new = new matched country-industry's position
ciid     <- unique(ciid.new)                     # ciid     = the new ciid (order not changed)
SN       <- length(ciid)
ciid.new.list <- sapply(ciid, function(i) which(ciid.new==i)) # list of re-classified ciid's row positions


# construct pre & post matrices for adjustment
post <- zeros(SN.old, SN)                        # post = post adjustment matrix for new ciid
for (j in 1:SN) post[ciid.new.list[[j]],j] <- 1  # assign 1 to the re-classified ciid's row position by column
pre <- t(post)                                   # pre  = pre adjustment matrix for new ciid


# Re-assign NEW Identifiers
S     <- length(iid)
SN.np <- S*N.np
id    <- sapply(strsplit(ciid,"_"), unique)
colnames(id) <- ciid


# Save meta data & remove some noisy data
#save(iclass,cid,ciid,iid,iid.eng,iid.kr, file=paste0(data,"ICIO_",iclass,"_meta.RData")) 
rm(irow.old, irow.old.list, iid.class, irow.new, ciid.new.list)  # remove noisy data


### Function for generating FDD matrix
fdd_i <- function(M, mat) {             # Take each column of M, diagonalize it, and then adjust (multiply) by mat      
      for (k in 1:dim(M)[2]) {          # where mat looks like [1 0 0 ; 0 1 0 ; 0 1 0 ; 0 0 1] (4x3)
            y <- diag(M[,k]) %*% mat    # mat = post (or column) adjust matrix
            if (k==1) z<-y else z<-cbind(z,y)
      }
      return(z)
}



### Compute the values for defined matrices

period <- c(1:17)
year <- paste0("yr",period+1994)

# Create empty variables
y <- zeros(length(period),SN,1)
dimnames(y) <- list(year,ciid,"y")
y.nzero <- zeros(length(period),SN,1)
dimnames(y.nzero) <- list(year,ciid,"y.nzero")
va <- zeros(length(period),SN,1)
dimnames(va) <- list(year,ciid,"va")
va.nzero <- zeros(length(period),SN,1)
dimnames(va.nzero) <- list(year,ciid,"va.nzero")
M <- zeros(length(period),SN,SN)
dimnames(M) <- list(year,ciid,ciid)
A <- zeros(length(period),SN,SN)
dimnames(A) <- list(year,ciid,ciid)
r <- zeros(length(period),SN,1)
dimnames(y) <- list(year,ciid,"r")
LeonInv <- zeros(length(period),SN,SN)
dimnames(LeonInv) <- list(year,ciid,ciid)
FD <- zeros(length(period),SN,N)
dimnames(FD) <- list(year,ciid,paste0("FD_",cid))
FD.np <- zeros(length(period),SN,N.np)
dimnames(FD.np) <- list(year,ciid,paste0("FD_",cid.np))
FDD <- zeros(length(period),SN,SN)
dimnames(FDD) <- list(year,ciid,ciid)
MX <-  zeros(length(period),SN,SN)
dimnames(MX) <- list(year,ciid,ciid)
FX <-  zeros(length(period),SN,N)
dimnames(FX) <- list(year,ciid,cid)


for(yr in period) {

      y.old <- DATA.ICIOeconX[yr,,]                     # y = World Output Vector
      y[yr,,] <- as.vector(pre %*% y.old)
      y.nzero[yr,,] <- y[yr,,]
      y.nzero[yr,y[yr,,]==0,] <- 1 
      
      va.old <- DATA.ICIOeconVA[yr,,]                   # va = World VA Vector
      va[yr,,] <- as.vector(pre %*% va.old)
      va.nzero[yr,,] <- va[yr,,]
      va.nzero[yr,va[yr,,]==0,] <- 1 
      
      M.old <- DATA.ICIOeconZ[yr,,]                     # M = Intermediate IO Matrix
      M[yr,,] <- pre %*% M.old %*% post
      
      A[yr,,] <- M[yr,,] / (ones(SN,1)%*%y.nzero[yr,,]) # A = Input Coefficient Matrix

      r[yr,,] <- va[yr,,] / y.nzero[yr,,]               # r = Ratio of Value-added to Total Output

      Leon <- diag(length(ciid)) - A[yr,,]
      LeonInv[yr,,] <- solve(Leon)                      # LeonInv = Leontief Inverse
      
      FD.old <- DATA.ICIOeconFDTTLdisc[yr,,]            # FD = Final Demand Matrix for N countries
      colnames(FD.old) <- c(cid,"DISC")
      FD.old[,"ROW"] <- FD.old[,"ROW"]+FD.old[,"DISC"]  # Discrepencies in FD (the last column) are merged to ROW
      FD[yr,,] <- pre %*% FD.old[,1:length(cid)]
      
      FD.old[,"CHN"] <- FD.old[,"CHN1"]                 # All China's FDs are included in "CHN1" column
      FD.old[,"MEX"] <- FD.old[,"MEX1"]                 # All Mexico's FDs are included in "MEX1" column
      FD.np[yr,,] <- pre %*% FD.old[,1:length(cid.np)]  # FD = Final Demand Matrix for N.np countries
      
      
      # Generating FDD (Final Demand Diagonalized) matrix
      for (i in 1:N) {                                            # i indicates ith country
            FDD[yr,((i-1)*S+1):(i*S),] <- fdd_i(FD[yr,((i-1)*S+1):(i*S),], eye(S))     
      }

      MX[yr,,] <- M[yr,,]*(1-(eye(N)%x%ones(S,S)))      # MX = Intermediate good Export Matrix
      FX[yr,,] <- FD[yr,,]*(1-(eye(N)%x%ones(S,1)))     # FX = Final good Export Matrix
}


# Save objects
save(S,N,N.np,SN,id,cid,cid.np,iclass,iid,iid.eng,ciid,M,A,y,y.nzero,va,va.nzero,r,LeonInv,FD,FD.np,FDD,MX,FX, 
     file=paste0(data,"ICIO_",iclass,"_matrix.RData"))
      


