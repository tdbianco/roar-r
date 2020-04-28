# With a matrix
a1 <- c("a1",9,8,rep(NA,5),7,8) 
a2 <- c(3,NA,NA,NA,4,3,4,6,0) 
a3 <- c(11,6,NA,NA,6,7,5,5,3) 
M <- rbind(a1,a2,a3) 
NAindex <- which(is.na(a1))
firstNA <- min(NAindex)
lastNA <- max(NAindex)
lastNonNa <- firstNA-1
firstNonNa <- lastNA+1
a1[lastNonNa]-a1[firstNonNa]

# With a dataframe
M <- data.frame(id=c(rep("a1", 9), rep("a2", 9)), Pup=c(0.9,0.2,rep(NA,5),0.8,0.3,0.3,rep(NA,3),0.4,0.3,0.4,0.6,0))
M

library(dplyr)
M2 <- M %>% group_by(id) %>% 
  summarise(firstNA=min(which(is.na(Pup))),
            lastNA=max(which(is.na(Pup))),
            lastNonNa=firstNA-1,
            firstNonNa=lastNA+1) %>%
  select(id,lastNonNa,firstNonNa) %>%
  left_join(M)

# In base R
M2[M2$firstNonNa[1],"Pup"] - M2[M2$lastNonNa[1],"Pup"]

# With custom function and for cycle
myf <- function(x, y) {
  x[first(x$firstNonNa),y] - x[first(x$lastNonNa),y]
}

subj <- unique(M2$id)
nsubj <- length(subj)

diffP <- data.frame()
for (i in 1:nsubj) {
  ds <- subset(M2, id==subj[i])
  diff <- myf(ds, "Pup")
  colnames(diff) <- NULL
  diffP <- rbind(diffP, cbind(id=subj[i], D=diff))
}

M3 <- left_join(M2, diffP)
M3

# Test if it works
library(zoo)
M3 %>% group_by(id) %>% mutate(int_Pup=ifelse(D<0.2, na.approx(Pup, maxgap=3, na.rm=FALSE), Pup))
