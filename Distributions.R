dataPath <- "C:/Users/Jason Sjafrudin/Desktop/Stat Files"
data <- read.table(paste(dataPath,'Week1_Test_Sample.csv',sep = '/'), header=TRUE)

data$u # sample u;
data$v # sample v.




tbl_uv <- table(data$u,data$v)
joint_distribution <- prop.table(tbl_uv)



tbl_u <-table(data$u)
u_Marginal <- prop.table(tbl_u)



tbl_v <-table(data$v)
v_Marginal <- prop.table(tbl_v)



data_v4 <- data[ (data$v == 4), ]
table_u_v4 <- table(data_v4$u, data_v4$v)
u_Conditional_v <- prop.table(table_u_v4)
#u_Conditional_v <- as.vector(as.matrix(u_Conditional_v_tbl[1:3]))




data_u3 <- data[ (data$u == 3), ]
table_u3_v <- table(data_u3$u, data_u3$v)
v_Conditional_u <- prop.table(table_u3_v)














res <-list(Joint_distribution=joint_distribution,
           u_Marginal = u_Marginal,
           v_Marginal = v_Marginal,
           u_Conditional_v = u_Conditional_v,
           v_Conditional_u = v_Conditional_u          )

saveRDS(res, file = paste("C:/Users/Jason Sjafrudin/Desktop/Stat Files",'Stats I1.rds',sep = '/'))
