data3way<- read.csv("C:/Users/Jason Sjafrudin/Desktop/Stat Files/test_sample 2.csv")

#wMarginal
tblw <- table(data3way$w)
wMarginal <- prop.table(tblw)
wMarginal <- as.vector(wMarginal)

#uMarginal
tblu <- table(data3way$u)
uMarginal <- prop.table(tblu)
uMarginal <- as.vector(uMarginal)


#vMarginal
tblv <- table(data3way$v)
vMarginal <- prop.table(tblv)
vMarginal <- as.vector(vMarginal)


#cond.v.w.given.u1
a <- select(data3way[(data3way$u == 1),], w, v)
tblwv.given.u1<- table(a)
cond.v.w.given.u1 <- prop.table(tblwv.given.u1)

#cond.v.given.u1
a <- select(data3way[(data3way$u == 1),], v)
tblv.given.u1 <- table(a)
cond.v.given.u1_matrix <- prop.table(tblv.given.u1)
cond.v.given.u1 <- as.vector(cond.v.given.u1_matrix)


#cond.w.given.u1.v2
a <- select(data3way[(data3way$u == 1) & (data3way$v == 2),], w)
tblw.given.u1.v2 <- table(a)
cond.w.given.u1.v2_matrix <- prop.table(tblw.given.u1.v2)
cond.w.given.u1.v2 <- as.vector(cond.w.given.u1.v2_matrix)




res<- list(vMarginal = vMarginal,
           uMarginal = uMarginal,
           wMarginal = wMarginal,
           cond1 = cond.v.w.given.u1, 
           cond2 = cond.v.given.u1,
           cond3 = cond.w.given.u1.v2)
saveRDS(res, file = paste("C:/Users/Jason Sjafrudin/Desktop/Stat Files",'resultS2.rds',sep = '/'))
















a <- select(data3way[(data3way$u == 0),], w, v)
mat.u0 <- table(a$w, a$v) / nrow(data3way)



b <- select(data3way[(data3way$u == 1),], w, v)
mat.u1 <- table(b$w, b$v) / nrow(data3way)



data3way.array<-array(rep(NA,18),dim=c(3,3,2),dimnames=list(paste("w",1:3,sep=""),
                                                            paste("v",1:3,sep=""),
                                                            paste("u",0:1,sep="")))



data3way.array[,,1]<-mat.u0
data3way.array[,,2]<-mat.u1

uMarginal <- c(u0 = sum(data3way.array[,,"u0"]), u1 = sum(data3way.array[,,"u1"]))
vMarginal <- apply(data3way.array, 2, sum)
wMarginal <- apply(data3way.array, 1, sum)

cond.v.w.given.u1 <- data3way.array[,,"u1"]/uMarginal["u1"]
cond.v.given.u1 <- apply(data3way.array[,,"u1"], 2, sum)/uMarginal["u1"]
cond.w.given.u1.v2 <- data3way.array[,'v2','u1']/cond.v.given.u1['v2']/uMarginal['u1']
print(rbind(uMarginal["u1"]*cond.v.given.u1["v2"]*cond.w.given.u1.v2,data3way.array[,"v2","u1"]))






res<- list(vMarginal = vMarginal,
           uMarginal = uMarginal,
           wMarginal = wMarginal,
           cond1 = cond.v.w.given.u1, 
           cond2 = cond.v.given.u1,
           cond3 = cond.w.given.u1.v2)

saveRDS(res, file = paste('Downloads/','result.rds',sep = '/'))
