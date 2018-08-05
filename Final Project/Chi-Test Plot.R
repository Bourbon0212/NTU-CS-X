# Make a table manually
party <- matrix(c(762, 327, 468, 484, 239, 477), ncol = 3, byrow = T)
colnames(party) <- c('Democrat', 'Independent', 'Republican')
rownames(party) <- c('F', 'M')
party <- as.table(party)
# Obsrvation
party

# Bike
sbi <- read.csv('D:/GitHub/NTU-CS-X/Final Project/Youbike/data/Youbike_sbi.csv')
sbi_g <- gather(sbi, time, quan, 6:ncol(sbi))

sbi_tab <- table(sbi_g$time, sbi_g$sarea)
sbi_tab

# Table
with(sbi_g, tapply(quan, list(sarea, time), FUN=sum))