DPS <- read.csv(file="DPS.csv")
xtabs(~SEX + DEATHPEN, data = DPS)