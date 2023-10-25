library(readxl)
library(dplyr)

Group_Assignment_1_Stats <- read_excel("C:/Users/Jason Sjafrudin/Desktop/Group Assignment 1 Stats.csv")

Group_Assignment_1_Stats
f

# P(B>A,B>C|B=5)
d1 <- count(Group_Assignment_1_Stats[(Group_Assignment_1_Stats$B == 5),])
d2 <- count(Group_Assignment_1_Stats[(Group_Assignment_1_Stats$B > Group_Assignment_1_Stats$A) 
                                     & (Group_Assignment_1_Stats$B > Group_Assignment_1_Stats$C) 
                                     & (Group_Assignment_1_Stats$B == 5), ])

d2/d1


# P(C>A,C>B|C=4)
d1 <- count(Group_Assignment_1_Stats[(Group_Assignment_1_Stats$C == 4),])
d2 <- count(Group_Assignment_1_Stats[(Group_Assignment_1_Stats$C > Group_Assignment_1_Stats$A) & (Group_Assignment_1_Stats$C > Group_Assignment_1_Stats$B) & (Group_Assignment_1_Stats$C == 4), ])

d2/d1


#P(A>B,A>C)
d1 <- count(Group_Assignment_1_Stats)
d2 <- count( Group_Assignment_1_Stats[(Group_Assignment_1_Stats$A > Group_Assignment_1_Stats$B) & (Group_Assignment_1_Stats$A > Group_Assignment_1_Stats$C),] )

d2/d1


# P(B>A,B>C)
d1 <- count(Group_Assignment_1_Stats)
d2 <- count(Group_Assignment_1_Stats[(Group_Assignment_1_Stats$B > Group_Assignment_1_Stats$A) & (Group_Assignment_1_Stats$B > Group_Assignment_1_Stats$C), ])

d2/d1


# P(C>A,C>B)
d1 <- count(Group_Assignment_1_Stats)
d2 <- count(Group_Assignment_1_Stats[(Group_Assignment_1_Stats$C > Group_Assignment_1_Stats$A) & (Group_Assignment_1_Stats$C > Group_Assignment_1_Stats$B), ])

d2/d1



#P(A>B)

d1 <- count(Group_Assignment_1_Stats)
d2 <- count(Group_Assignment_1_Stats[(Group_Assignment_1_Stats$A > Group_Assignment_1_Stats$B), ])

d2/d1
