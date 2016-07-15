setwd("~/Database")
library(dplyr)
source("functions.R")

db <- src_postgres(dbname = "antom", host = "biodb.uef.fi", user = "antom", password = "d0189244be")

associations <- tbl(db,"associations",dataset_id == 40) %>% data.frame

df_qq <-associations %>% select(effect)

make_qqplot(df_qq,"OR")

qqnorm(log2(df_qq$effect))


str <- "anton,mikko,sane"

splitted <- unlist(strsplit(str,split=","))
