#P1
#====
# install.packages('xlsx')
# library(xlsx)
install.packages('readxl')
library(readxl)

Data_exm = read_excel('221453.xlsx')
Data_exm$Y
Data_exm$X1
Data_exm$X2
#----------------------------------------
#P2
#===

Ys = Data_exm$Y^2
X1s = Data_exm$X1^2
X2s = Data_exm$X2^2


E_Data_1 = matrix(data=c(Ys, X1s, X2s), nrow=10,  ncol = 3)
colnames(E_Data_1)=c("Ys","X1s","X2s")

install.packages('writexl')
library(writexl)

write_xlsx(as.data.frame(E_Data_1), "Lab1 file1 export.xlsx")

#------------------------------------------

#P3
#===

Y = Data_exm$Y
X = as.matrix(cbind(1,Data_exm[,-1]))

(b1.hat = solve(crossprod(X))%*%t(X)%*%Y)
#-------------------------------------------

#P4
#=====
lm(Y~Data_exm$X1+Data_exm$X2)
#===============================================