flint = read.csv("Flint-Sample-FINAL.csv",header=TRUE)
colnames=c("ID","Zip","Ward","B1","B2","B3","Notes")
flinc$avg = (B1+B2+B3)/3


