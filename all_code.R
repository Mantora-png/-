#----------------------------------
##安装相应的程序包并加载
#----------------------------------
install.packages("lightgbm")
install.packages("randomFrost")
install.packages("xgboost")
library(lightgbm)
library(randomForest)
library(xgboost)
library(Matrix)
library(catboost)

data_all <- read.csv()  #写入训练的全部数据
test_all <- read.csv()  #写入要预测的数据
#----------------------
#训练模型train_data
#----------------------
data <- data_all
data1 <- subset(data, is.na(data$cfvfdpvnumvm0001) == FALSE)
data2 <- subset(data, is.na(data$cfvfdpvnumvm0001) == TRUE)  # flag == 0

#对数据集进行划分，寻找缺失变量的特征;
#根据sex基本变量的NA划分数据集和非NA数据；
data1.1 <- subset(data1,is.na(data1$sex) == TRUE| data1$sex == -1) #划分为NA
data1.2 <- subset(data1, is.na(data1$sex) == FALSE & data1$sex != -1) #划分为非NA
#根据education基本变量的NA进行划分数据集;
data1.2.1 <- subset(data1.2, is.na(data1.2$educate) == TRUE) #划分为NA
data1.2.2 <- subset(data1.2, is.na(data1.2$educate) == FALSE) #划分为非NA

# 去掉：data[,49:54]、data[,55:58]、data[,179:230]、data[,231:282]、data[,283:314]、data[,315:322]、data[,323:330]、data[,336:337]
# 考虑是否去掉、不去掉的话是否可以划分为NA和非NA:data[,59:64]、data[,65:70]、data[,103:158]、data[,167:172]、data[,173:178]
data <- data1.2.2
data1.2.2.1 <- data[,c(-336:-337)];data1.2.2.1 <- data1.2.2.1[,c(-179:-330)];data1.2.2.1<-data1.2.2.1[c(-49:-58)]
#data1.2.2.1 <- data[,c(-179:-330)];data1.2.2.1<-data1.2.2.1[c(-49:-58)]

data <- data1.2.1
data1.2.1.1 <- data[,c(-336:-337)];data1.2.1.1 <- data1.2.1.1[,c(-179:-330)];data1.2.1.1<-data1.2.1.1[c(-49:-58)]


data <- data1.2.2
data1.2.2.2 <- data[,c(-336:-337)];data1.2.2.2 <- data1.2.2.2[,c(-173:-330)];data1.2.2.2 <-data1.2.2.2[,c(-167:-172)];data1.2.2.2<-data1.2.2.2[,c(-103:-158)];data1.2.2.2 <- data1.2.2.2[,c(-49:-70)]

data <- data1.2.1
data1.2.1.2 <- data[,c(-336:-337)];data1.2.1.2 <- data1.2.1.2[,c(-173:-330)];data1.2.1.2 <-data1.2.1.2[,c(-167:-172)];data1.2.1.2<-data1.2.1.2[,c(-103:-158)];data1.2.1.2 <- data1.2.1.2[,c(-49:-70)];


#--------------------------------------------------------------------
##对data1.2.2.1分析
#--------------------------------------------------------------------
data <- data1.2.2.1
data1.2.2.1.1 <- subset(data,is.na(data$cbvchadrevnumvm0012) == FALSE)
data1.2.2.1.2 <- subset(data,is.na(data$cbvchadrevnumvm0012) == TRUE)

#data1.2.2.1.2有哪些是NA呢
#data1.2.2.1.2[49:60]; data1.2.2.1.2[93:148]; data1.2.2.1.2[157:168]
data1.2.2.1.2 <- data1.2.2.1.2[,c(-157:-168)];data1.2.2.1.2 <- data1.2.2.1.2[,c(-93:-148)];data1.2.2.1.2 <- data1.2.2.1.2[,c(-49:-60)]

#对基本信息进行分析
data <- data1.2.2.1.2

# marriage: -1(6577/147188)、1(12711/168412)、2(26629/76210)、3(15:55)、4(4:9)、6(8/72)、7(40/436)、8(36195/290658)、NA(1821/16960)
# devided: -1 1 2 8; 3 4 6 7 NA: 未知、丧偶、离婚、未婚、已婚
data$marriage_satatus <- ifelse((data$marriage_satatus == 3 | data$marriage_satatus == 4 |data$marriage_satatus == 6 |data$marriage_satatus == 7 |data$marriage_satatus == 5),5,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus == 8),4,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus ==2),3,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus == 1),2,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus == -1),1,data$marriage_satatus)
data$marriage_satatus <- factor(data$marriage_satatus, levels = c(1,2,3,4,5))
# occupation: occupation为NA,sex和marriage都为NA时可以单独考虑，另外的空值全为0; NA = 19368
# -1(1997/143481)、1 (827/10201)、2345组合(5103/16537)、6789 10 11 12 13 16 23(8341/104002)、14 15(2269/15179)、
# 17、18和>23(3454/5533)、19 20 (547/6139)、21(7096/52851)、22(5803/6101)
data$occupation <-ifelse((data$occupation == -1| is.na(data$occupation) == TRUE),0,data$occupation)
data$occupation <-ifelse((data$occupation == 1),1,data$occupation)
data$occupation <-ifelse((data$occupation == 2 |data$occupation == 3|data$occupation == 4|data$occupation == 5),2,data$occupation)
data$occupation <-ifelse((data$occupation == 6 |data$occupation == 7|data$occupation == 8|data$occupation == 9|data$occupation == 10|data$occupation == 11|data$occupation == 12|data$occupation == 13|data$occupation == 16|data$occupation == 23 ),3,data$occupation)
data$occupation <-ifelse((data$occupation == 14|data$occupation == 15),4,data$occupation)
data$occupation <-ifelse((data$occupation >23|data$occupation == 17|data$occupation == 18),5,data$occupation)
data$occupation <-ifelse((data$occupation == 19|data$occupation == 20),6,data$occupation)
data$occupation <-ifelse((data$occupation == 21),7,data$occupation)
data$occupation <-ifelse((data$occupation == 22),8,data$occupation)
data$occupation <- factor(data$occupation, levels = c(0,1,2,3,4,5,6,7,8))

# educate: NA部分单独分析、-1 9(3195/13572)、1 3 4 5 8(3195/17545)、2(840/1256)、6 7(453/1521)
data$educate <- ifelse((data$educate == 1 | data$educate == 3 |data$educate == 2),2,data$educate)
data$educate <- ifelse((data$educate == -1),1,data$educate)
data$educate <- ifelse((data$educate == 4),3,data$educate)
data$educate <- ifelse((data$educate == 5|data$educate == 6 | data$educate == 7| data$educate == 8),4,data$educate)
data$educate <- ifelse((data$educate == 9),5,data$educate)
data$educate <- factor(data$educate, levels = c(1,2,3,4,5))
summary(data$educate)

data1.2.2.1.2 <- data
#对数据data1.2.2.2.2分析
# settime的分析
data1.2.2.1.2$settime <- ifelse((data1.2.2.1.2$settime == 202006),1,data1.2.2.1.2$settime)
data1.2.2.1.2$settime <- ifelse((data1.2.2.1.2$settime == 202007),2,data1.2.2.1.2$settime)
data1.2.2.1.2$settime <- ifelse((data1.2.2.1.2$settime == 202008),3,data1.2.2.1.2$settime)
data1.2.2.1.2$settime <- ifelse((data1.2.2.1.2$settime == 202009),4,data1.2.2.1.2$settime)
data1.2.2.1.2$settime <- ifelse((data1.2.2.1.2$settime == 202010),5,data1.2.2.1.2$settime)
data1.2.2.1.2$settime <- ifelse((data1.2.2.1.2$settime == 202011),6,data1.2.2.1.2$settime)
data1.2.2.1.2$settime <- ifelse((data1.2.2.1.2$settime == 202012),7,data1.2.2.1.2$settime)
data1.2.2.1.2$settime <- ifelse((data1.2.2.1.2$settime == 202101),8,data1.2.2.1.2$settime)
data1.2.2.1.2$settime <- ifelse((data1.2.2.1.2$settime == 202102),9,data1.2.2.1.2$settime)
data1.2.2.1.2$settime <- ifelse((data1.2.2.1.2$settime == 202103),10,data1.2.2.1.2$settime)
data1.2.2.1.2$settime <- ifelse((data1.2.2.1.2$settime == 202104),11,data1.2.2.1.2$settime)
data1.2.2.1.2$settime <- factor(data1.2.2.1.2$settime, levels = c(1,2,3,4,5,6,7,8,9,10,11))

#最后整理的数据
data <- data1.2.2.1.2

#对数据进行标准化
data$flag <- factor(data$flag)
data$sex <- factor(data$sex)
data$online_loans_flag <- factor(data$online_loans_flag)
data$credit_card_flag <- factor(data$credit_card_flag)
data$pre_list_glag <-factor(data$pre_list_glag)
data$merchant_falg <- factor(data$merchant_falg)
data$settime <- factor(data$settime)
data$cfvfdpvnumvm0024 <- data$cfvfdpvnumvm0012
data$cfvdctrdamovm0002 <- (as.integer(data$cfvdctrdamovm0001) + as.integer(data$cfvdctrdamovm0003))/2
data$cfvdctrdamovm0024 <- data$cfvdctrdamovm0012
data$cfvcctrdamovm0002 <- (data$cfvcctrdamovm0001 + data$cfvcctrdamovm0003)/2
data$cfvcctrdamovm0024 <- data$cfvcctrdamovm0012
data$cfvcctrdvnumvm0002 <- (data$cfvcctrdvnumvm0001+data$cfvcctrdvnumvm0003)/2
data$cfvcctrdvnumvm0024 <- data$cfvcctrdvnumvm0012
data$covassvhypvm0024 <-data$covassvhypvm0012 - (data$covassvhypvm0006 - data$covassvhypvm0012)
data$covassbalvmaxvm0024 <- data$covassbalvmaxvm0012
data1.2.2.1.2 <-data

#对data1.2.2.1.1分析
data <- data1.2.2.1.1
# marriage: -1(6577/147188)、1(12711/168412)、2(26629/76210)、3(15:55)、4(4:9)、6(8/72)、7(40/436)、8(36195/290658)、NA(1821/16960)
# devided: -1 1 2 8; 3 4 6 7 NA: 未知、丧偶、离婚、未婚、已婚
data$marriage_satatus <- ifelse((data$marriage_satatus == 3 | data$marriage_satatus == 4 |data$marriage_satatus == 6 |data$marriage_satatus == 7 |data$marriage_satatus == 5),5,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus == 8),4,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus ==2),3,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus == 1),2,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus == -1),1,data$marriage_satatus)
data$marriage_satatus <- factor(data$marriage_satatus, levels = c(1,2,3,4,5))
# occupation: occupation为NA,sex和marriage都为NA时可以单独考虑，另外的空值全为0; NA = 19368
# -1(1997/143481)、1 (827/10201)、2345组合(5103/16537)、6789 10 11 12 13 16 23(8341/104002)、14 15(2269/15179)、
# 17、18和>23(3454/5533)、19 20 (547/6139)、21(7096/52851)、22(5803/6101)
data$occupation <-ifelse((data$occupation == -1 | is.na(data$occupation) == TRUE),0,data$occupation)
data$occupation <-ifelse((data$occupation == 1),1,data$occupation)
data$occupation <-ifelse((data$occupation == 2 |data$occupation == 3|data$occupation == 4|data$occupation == 5),2,data$occupation)
data$occupation <-ifelse((data$occupation == 6 |data$occupation == 7|data$occupation == 8|data$occupation == 9|data$occupation == 10|data$occupation == 11|data$occupation == 12|data$occupation == 13|data$occupation == 16|data$occupation == 23 ),3,data$occupation)
data$occupation <-ifelse((data$occupation == 14|data$occupation == 15),4,data$occupation)
data$occupation <-ifelse((data$occupation >23|data$occupation == 17|data$occupation == 18),5,data$occupation)
data$occupation <-ifelse((data$occupation == 19|data$occupation == 20),6,data$occupation)
data$occupation <-ifelse((data$occupation == 21),7,data$occupation)
data$occupation <-ifelse((data$occupation == 22),8,data$occupation)
data$occupation <- factor(data$occupation, levels = c(0,1,2,3,4,5,6,7,8))

# educate: NA部分单独分析、-1 9(3195/13572)、1 3 4 5 8(3195/17545)、2(840/1256)、6 7(453/1521)
data$educate <- ifelse((data$educate == 1 | data$educate == 3 |data$educate == 2),2,data$educate)
data$educate <- ifelse((data$educate == -1),1,data$educate)
data$educate <- ifelse((data$educate == 4),3,data$educate)
data$educate <- ifelse((data$educate == 6 | data$educate == 7| data$educate == 8),4,data$educate)
data$educate <- ifelse((data$educate == 9),5,data$educate)
data$educate <- factor(data$educate, levels = c(1,2,3,4,5))
summary(data$educate)

data1.2.2.1.1 <- data

# settime的分析
data1.2.2.1.1$settime <- ifelse((data1.2.2.1.1$settime == 202006),1,data1.2.2.1.1$settime)
data1.2.2.1.1$settime <- ifelse((data1.2.2.1.1$settime == 202007),2,data1.2.2.1.1$settime)
data1.2.2.1.1$settime <- ifelse((data1.2.2.1.1$settime == 202008),3,data1.2.2.1.1$settime)
data1.2.2.1.1$settime <- ifelse((data1.2.2.1.1$settime == 202009),4,data1.2.2.1.1$settime)
data1.2.2.1.1$settime <- ifelse((data1.2.2.1.1$settime == 202010),5,data1.2.2.1.1$settime)
data1.2.2.1.1$settime <- ifelse((data1.2.2.1.1$settime == 202011),6,data1.2.2.1.1$settime)
data1.2.2.1.1$settime <- ifelse((data1.2.2.1.1$settime == 202012),7,data1.2.2.1.1$settime)
data1.2.2.1.1$settime <- ifelse((data1.2.2.1.1$settime == 202101),8,data1.2.2.1.1$settime)
data1.2.2.1.1$settime <- ifelse((data1.2.2.1.1$settime == 202102),9,data1.2.2.1.1$settime)
data1.2.2.1.1$settime <- ifelse((data1.2.2.1.1$settime == 202103),10,data1.2.2.1.1$settime)
data1.2.2.1.1$settime <- ifelse((data1.2.2.1.1$settime == 202104),11,data1.2.2.1.1$settime)
data1.2.2.1.1$settime <- factor(data1.2.2.1.1$settime, levels = c(1,2,3,4,5,6,7,8,9,10,11))

#最后整理的数据
data <- data1.2.2.1.1

#对数据进行标准化
data$flag <- factor(data$flag)
data$sex <- factor(data$sex)
data$online_loans_flag <- factor(data$online_loans_flag)
data$credit_card_flag <- factor(data$credit_card_flag)
data$pre_list_glag <-factor(data$pre_list_glag)
data$merchant_falg <- factor(data$merchant_falg)
data$settime <- factor(data$settime)
data$cfvfdpvnumvm0024 <- data$cfvfdpvnumvm0012
data$cfvdctrdamovm0002 <- (as.integer(data$cfvdctrdamovm0001) + as.integer(data$cfvdctrdamovm0003))/2
data$cfvdctrdamovm0024 <- data$cfvdctrdamovm0012
data$cfvcctrdamovm0002 <- (data$cfvcctrdamovm0001 + data$cfvcctrdamovm0003)/2
data$cfvcctrdamovm0024 <- data$cfvcctrdamovm0012
data$cfvcctrdvnumvm0002 <- (data$cfvcctrdvnumvm0001+data$cfvcctrdvnumvm0003)/2
data$cfvcctrdvnumvm0024 <- data$cfvcctrdvnumvm0012
data$covassvhypvm0024 <-data$covassvhypvm0012 - (data$covassvhypvm0006 - data$covassvhypvm0012)
data$covassbalvmaxvm0024 <- data$covassbalvmaxvm0012
data1.2.2.1.1 <-data

#-------------------------------------
#对data1.2.1.1分析
#-------------------------------------
data <- data1.2.1.1
data1.2.1.1.1 <- subset(data,is.na(data$cbvchadrevnumvm0012) == FALSE)
data1.2.1.1.2 <- subset(data,is.na(data$cbvchadrevnumvm0012) == TRUE)

#data1.2.1.1.2有哪些是NA呢
#data1.2.2.1.2[49:60]; data1.2.2.1.2[93:148]; data1.2.2.1.2[157:168]
data1.2.1.1.2 <- data1.2.1.1.2[,c(-157:-168)];data1.2.1.1.2 <- data1.2.1.1.2[,c(-93:-148)];data1.2.1.1.2 <- data1.2.1.1.2[,c(-49:-60)]

#对基本信息进行分析
data <- data1.2.1.1.2

# marriage: -1(6577/147188)、1(12711/168412)、2(26629/76210)、3(15:55)、4(4:9)、6(8/72)、7(40/436)、8(36195/290658)、NA(1821/16960)
# devided: -1 1 2 8; 3 4 6 7 NA: 未知、丧偶、离婚、未婚、已婚
data$marriage_satatus <- ifelse((data$marriage_satatus == 3 | data$marriage_satatus == 4 |data$marriage_satatus == 6 |data$marriage_satatus == 7 |data$marriage_satatus == 5),5,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus == 8),4,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus ==2),3,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus == 1),2,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus == -1),1,data$marriage_satatus)
data$marriage_satatus <- factor(data$marriage_satatus, levels = c(1,2,3,4,5))
# occupation: occupation为NA,sex和marriage都为NA时可以单独考虑，另外的空值全为0; NA = 19368
# -1(1997/143481)、1 (827/10201)、2345组合(5103/16537)、6789 10 11 12 13 16 23(8341/104002)、14 15(2269/15179)、
# 17、18和>23(3454/5533)、19 20 (547/6139)、21(7096/52851)、22(5803/6101)
data$occupation <-ifelse((data$occupation == -1|is.na(data$occupation) == TRUE),0,data$occupation)
data$occupation <-ifelse((data$occupation == 1),1,data$occupation)
data$occupation <-ifelse((data$occupation == 2 |data$occupation == 3|data$occupation == 4|data$occupation == 5),2,data$occupation)
data$occupation <-ifelse((data$occupation == 6 |data$occupation == 7|data$occupation == 8|data$occupation == 9|data$occupation == 10|data$occupation == 11|data$occupation == 12|data$occupation == 13|data$occupation == 16|data$occupation == 23 ),3,data$occupation)
data$occupation <-ifelse((data$occupation == 14|data$occupation == 15),4,data$occupation)
data$occupation <-ifelse((data$occupation >23|data$occupation == 17|data$occupation == 18),5,data$occupation)
data$occupation <-ifelse((data$occupation == 19|data$occupation == 20),6,data$occupation)
data$occupation <-ifelse((data$occupation == 21),7,data$occupation)
data$occupation <-ifelse((data$occupation == 22),8,data$occupation)
data$occupation <- factor(data$occupation, levels = c(0,1,2,3,4,5,6,7,8))

data1.2.1.1.2 <- data
#对数据data1.2.2.2.2分析
# settime的分析
data1.2.1.1.2$settime <- ifelse((data1.2.1.1.2$settime == 202006),1,data1.2.1.1.2$settime)
data1.2.1.1.2$settime <- ifelse((data1.2.1.1.2$settime == 202007),2,data1.2.1.1.2$settime)
data1.2.1.1.2$settime <- ifelse((data1.2.1.1.2$settime == 202008),3,data1.2.1.1.2$settime)
data1.2.1.1.2$settime <- ifelse((data1.2.1.1.2$settime == 202009),4,data1.2.1.1.2$settime)
data1.2.1.1.2$settime <- ifelse((data1.2.1.1.2$settime == 202010),5,data1.2.1.1.2$settime)
data1.2.1.1.2$settime <- ifelse((data1.2.1.1.2$settime == 202011),6,data1.2.1.1.2$settime)
data1.2.1.1.2$settime <- ifelse((data1.2.1.1.2$settime == 202012),7,data1.2.1.1.2$settime)
data1.2.1.1.2$settime <- ifelse((data1.2.1.1.2$settime == 202101),8,data1.2.1.1.2$settime)
data1.2.1.1.2$settime <- ifelse((data1.2.1.1.2$settime == 202102),9,data1.2.1.1.2$settime)
data1.2.1.1.2$settime <- ifelse((data1.2.1.1.2$settime == 202103),10,data1.2.1.1.2$settime)
data1.2.1.1.2$settime <- ifelse((data1.2.1.1.2$settime == 202104),11,data1.2.1.1.2$settime)
data1.2.1.1.2$settime <- factor(data1.2.1.1.2$settime, levels = c(1,2,3,4,5,6,7,8,9,10,11))

#最后整理的数据
data <- data1.2.1.1.2

#对数据进行标准化
data$flag <- factor(data$flag)
data$sex <- factor(data$sex)
data$online_loans_flag <- factor(data$online_loans_flag)
data$credit_card_flag <- factor(data$credit_card_flag)
data$pre_list_glag <-factor(data$pre_list_glag)
data$merchant_falg <- factor(data$merchant_falg)
data$settime <- factor(data$settime)
data$cfvfdpvnumvm0024 <- data$cfvfdpvnumvm0012
data$cfvdctrdamovm0002 <- (as.integer(data$cfvdctrdamovm0001) + as.integer(data$cfvdctrdamovm0003))/2
data$cfvdctrdamovm0024 <- data$cfvdctrdamovm0012
data$cfvcctrdamovm0002 <- (data$cfvcctrdamovm0001 + data$cfvcctrdamovm0003)/2
data$cfvcctrdamovm0024 <- data$cfvcctrdamovm0012
data$cfvcctrdvnumvm0002 <- (data$cfvcctrdvnumvm0001+data$cfvcctrdvnumvm0003)/2
data$cfvcctrdvnumvm0024 <- data$cfvcctrdvnumvm0012
data$covassvhypvm0024 <-data$covassvhypvm0012 - (data$covassvhypvm0006 - data$covassvhypvm0012)
data$covassbalvmaxvm0024 <- data$covassbalvmaxvm0012
data1.2.1.1.2 <-data



#对data1.2.1.1.1分析
data <- data1.2.1.1.1
# marriage: -1(6577/147188)、1(12711/168412)、2(26629/76210)、3(15:55)、4(4:9)、6(8/72)、7(40/436)、8(36195/290658)、NA(1821/16960)
# devided: -1 1 2 8; 3 4 6 7 NA: 未知、丧偶、离婚、未婚、已婚
data$marriage_satatus <- ifelse((data$marriage_satatus == 3 | data$marriage_satatus == 4 |data$marriage_satatus == 6 |data$marriage_satatus == 7 |data$marriage_satatus == 5),5,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus == 8),4,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus ==2),3,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus == 1),2,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus == -1),1,data$marriage_satatus)
data$marriage_satatus <- factor(data$marriage_satatus, levels = c(1,2,3,4,5))
# occupation: occupation为NA,sex和marriage都为NA时可以单独考虑，另外的空值全为0; NA = 19368
# -1(1997/143481)、1 (827/10201)、2345组合(5103/16537)、6789 10 11 12 13 16 23(8341/104002)、14 15(2269/15179)、
# 17、18和>23(3454/5533)、19 20 (547/6139)、21(7096/52851)、22(5803/6101)
data$occupation <-ifelse((data$occupation == -1 | is.na(data$occupation) == TRUE),0,data$occupation)
data$occupation <-ifelse((data$occupation == 1),1,data$occupation)
data$occupation <-ifelse((data$occupation == 2 |data$occupation == 3|data$occupation == 4|data$occupation == 5),2,data$occupation)
data$occupation <-ifelse((data$occupation == 6 |data$occupation == 7|data$occupation == 8|data$occupation == 9|data$occupation == 10|data$occupation == 11|data$occupation == 12|data$occupation == 13|data$occupation == 16|data$occupation == 23 ),3,data$occupation)
data$occupation <-ifelse((data$occupation == 14|data$occupation == 15),4,data$occupation)
data$occupation <-ifelse((data$occupation >23|data$occupation == 17|data$occupation == 18),5,data$occupation)
data$occupation <-ifelse((data$occupation == 19|data$occupation == 20),6,data$occupation)
data$occupation <-ifelse((data$occupation == 21),7,data$occupation)
data$occupation <-ifelse((data$occupation == 22),8,data$occupation)
data$occupation <- factor(data$occupation, levels = c(0,1,2,3,4,5,6,7,8))

data1.2.1.1.1 <- data

# settime的分析
data1.2.1.1.1$settime <- ifelse((data1.2.1.1.1$settime == 202006),1,data1.2.1.1.1$settime)
data1.2.1.1.1$settime <- ifelse((data1.2.1.1.1$settime == 202007),2,data1.2.1.1.1$settime)
data1.2.1.1.1$settime <- ifelse((data1.2.1.1.1$settime == 202008),3,data1.2.1.1.1$settime)
data1.2.1.1.1$settime <- ifelse((data1.2.1.1.1$settime == 202009),4,data1.2.1.1.1$settime)
data1.2.1.1.1$settime <- ifelse((data1.2.1.1.1$settime == 202010),5,data1.2.1.1.1$settime)
data1.2.1.1.1$settime <- ifelse((data1.2.1.1.1$settime == 202011),6,data1.2.1.1.1$settime)
data1.2.1.1.1$settime <- ifelse((data1.2.1.1.1$settime == 202012),7,data1.2.1.1.1$settime)
data1.2.1.1.1$settime <- ifelse((data1.2.1.1.1$settime == 202101),8,data1.2.1.1.1$settime)
data1.2.1.1.1$settime <- ifelse((data1.2.1.1.1$settime == 202102),9,data1.2.1.1.1$settime)
data1.2.1.1.1$settime <- ifelse((data1.2.1.1.1$settime == 202103),10,data1.2.1.1.1$settime)
data1.2.1.1.1$settime <- ifelse((data1.2.1.1.1$settime == 202104),11,data1.2.1.1.1$settime)
data1.2.1.1.1$settime <- factor(data1.2.1.1.1$settime, levels = c(1,2,3,4,5,6,7,8,9,10,11))

#最后整理的数据
data <- data1.2.1.1.1

#对数据进行标准化
data$flag <- factor(data$flag)
data$sex <- factor(data$sex)
data$online_loans_flag <- factor(data$online_loans_flag)
data$credit_card_flag <- factor(data$credit_card_flag)
data$pre_list_glag <-factor(data$pre_list_glag)
data$merchant_falg <- factor(data$merchant_falg)
data$settime <- factor(data$settime)
data$cfvfdpvnumvm0024 <- data$cfvfdpvnumvm0012
data$cfvdctrdamovm0002 <- (as.integer(data$cfvdctrdamovm0001) + as.integer(data$cfvdctrdamovm0003))/2
data$cfvdctrdamovm0024 <- data$cfvdctrdamovm0012
data$cfvcctrdamovm0002 <- (data$cfvcctrdamovm0001 + data$cfvcctrdamovm0003)/2
data$cfvcctrdamovm0024 <- data$cfvcctrdamovm0012
data$cfvcctrdvnumvm0002 <- (data$cfvcctrdvnumvm0001+data$cfvcctrdvnumvm0003)/2
data$cfvcctrdvnumvm0024 <- data$cfvcctrdvnumvm0012
data$covassvhypvm0024 <-data$covassvhypvm0012 - (data$covassvhypvm0006 - data$covassvhypvm0012)
data$covassbalvmaxvm0024 <- data$covassbalvmaxvm0012
data1.2.1.1.1 <-data
#---------------------------------------
#对data2进行分析
data2.1 <- subset(data2,data2$flag == 1)
data2.2 <- subset(data2,data2$flag == 0)
data <- data2.2
data2.2 <- data[,c(-336:-337)];data2.2 <- data2.2[,c(-179:-330)];data2.2<-data2.2[c(-49:-58)]
data2.2 <- subset(data2.2,is.na(data2.2$educate) == FALSE)
data2.2 <- subset(data2.2,is.na(data2.2$cfvdctrdamovm0001) == FALSE)
data2.2 <- subset(data2.2,is.na(data2.2$covechavnumvm0012) == FALSE)
data2.2 <- subset(data2.2, data2.2$sex == 1 | data2.2$sex == 2)
data2.2.1 <- subset(data2.2,is.na(data2.2$cbvnetblogvnumvm0024) == FALSE)
data <- data2.2.1

# marriage: -1(6577/147188)、1(12711/168412)、2(26629/76210)、3(15:55)、4(4:9)、6(8/72)、7(40/436)、8(36195/290658)、NA(1821/16960)
# devided: -1 1 2 8; 3 4 6 7 NA: 未知、丧偶、离婚、未婚、已婚
data$marriage_satatus <- ifelse((data$marriage_satatus == 3 | data$marriage_satatus == 4 |data$marriage_satatus == 6 |data$marriage_satatus == 7 |data$marriage_satatus == 5),5,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus == 8),4,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus ==2),3,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus == 1),2,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus == -1),1,data$marriage_satatus)
data$marriage_satatus <- factor(data$marriage_satatus, levels = c(1,2,3,4,5))
# occupation: occupation为NA,sex和marriage都为NA时可以单独考虑，另外的空值全为0; NA = 19368
# -1(1997/143481)、1 (827/10201)、2345组合(5103/16537)、6789 10 11 12 13 16 23(8341/104002)、14 15(2269/15179)、
# 17、18和>23(3454/5533)、19 20 (547/6139)、21(7096/52851)、22(5803/6101)
data$occupation <-ifelse((data$occupation == -1 | is.na(data$occupation) == TRUE),0,data$occupation)
data$occupation <-ifelse((data$occupation == 1),1,data$occupation)
data$occupation <-ifelse((data$occupation == 2 |data$occupation == 3|data$occupation == 4|data$occupation == 5),2,data$occupation)
data$occupation <-ifelse((data$occupation == 6 |data$occupation == 7|data$occupation == 8|data$occupation == 9|data$occupation == 10|data$occupation == 11|data$occupation == 12|data$occupation == 13|data$occupation == 16|data$occupation == 23 ),3,data$occupation)
data$occupation <-ifelse((data$occupation == 14|data$occupation == 15),4,data$occupation)
data$occupation <-ifelse((data$occupation >23|data$occupation == 17|data$occupation == 18),5,data$occupation)
data$occupation <-ifelse((data$occupation == 19|data$occupation == 20),6,data$occupation)
data$occupation <-ifelse((data$occupation == 21),7,data$occupation)
data$occupation <-ifelse((data$occupation == 22),8,data$occupation)
data$occupation <- factor(data$occupation, levels = c(0,1,2,3,4,5,6,7,8))

# settime的分析
data$settime <- ifelse((data$settime == 202006),1,data$settime)
data$settime <- ifelse((data$settime == 202007),2,data$settime)
data$settime <- ifelse((data$settime == 202008),3,data$settime)
data$settime <- ifelse((data$settime == 202009),4,data$settime)
data$settime <- ifelse((data$settime == 202010),5,data$settime)
data$settime <- ifelse((data$settime == 202011),6,data$settime)
data$settime <- ifelse((data$settime == 202012),7,data$settime)
data$settime <- ifelse((data$settime == 202101),8,data$settime)
data$settime <- ifelse((data$settime == 202102),9,data$settime)
data$settime <- ifelse((data$settime == 202103),10,data$settime)
data$settime <- ifelse((data$settime == 202104),11,data$settime)
data$settime <- factor(data$settime, levels = c(1,2,3,4,5,6,7,8,9,10,11))

#对数据进行标准化
data$flag <- factor(data$flag)
data$sex <- factor(data$sex)
data$online_loans_flag <- factor(data$online_loans_flag)
data$credit_card_flag <- factor(data$credit_card_flag)
data$pre_list_glag <-factor(data$pre_list_glag)
data$merchant_falg <- factor(data$merchant_falg)
data$settime <- factor(data$settime)
# educate: NA部分单独分析、-1 9(3195/13572)、1 3 4 5 8(3195/17545)、2(840/1256)、6 7(453/1521)
data$educate <- ifelse((data$educate == 1 | data$educate == 3 |data$educate == 2),2,data$educate)
data$educate <- ifelse((data$educate == -1),1,data$educate)
data$educate <- ifelse((data$educate == 4),3,data$educate)
data$educate <- ifelse((data$educate == 6 | data$educate == 7| data$educate == 8),4,data$educate)
data$educate <- ifelse((data$educate == 9),5,data$educate)
data$educate <- factor(data$educate, levels = c(1,2,3,4,5))
summary(data$educate)
data$cfvfdpvnumvm0024 <- data$cfvfdpvnumvm0012
data$cfvdctrdamovm0002 <- (as.integer(data$cfvdctrdamovm0001) + as.integer(data$cfvdctrdamovm0003))/2
data$cfvdctrdamovm0024 <- data$cfvdctrdamovm0012
data$cfvcctrdamovm0002 <- (data$cfvcctrdamovm0001 + data$cfvcctrdamovm0003)/2
data$cfvcctrdamovm0024 <- data$cfvcctrdamovm0012
data$cfvcctrdvnumvm0002 <- (data$cfvcctrdvnumvm0001+data$cfvcctrdvnumvm0003)/2
data$cfvcctrdvnumvm0024 <- data$cfvcctrdvnumvm0012
data$covassvhypvm0024 <-data$covassvhypvm0012 - (data$covassvhypvm0006 - data$covassvhypvm0012)
data$covassbalvmaxvm0024 <- data$covassbalvmaxvm0012
data2.2.1 <-data 
#--------------------------------------
#分别对data1.2.2.1.1和data1.2.2.1.2进行randomFrost、LightGBM和XGBoost分析
#--------------------------------------
#对data1.2.2.1.1分析
data <- data1.2.2.1.1
data$flag <- factor(data$flag)

data <- data[,c(-124:-155,-162:-174)]
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]
#randomFrost分析
formula2.1.1 <- Formula::as.Formula(data[2:139])
random2.1.1 <- randomForest(formula2.1.1,data = data, mtry = 15,ntree=500 ,importance = FALSE,na.action = na.omit)
pred1 <- predict(random2.1.1,test)
a<-table(pred1,test$flag);a;(a[2,2]+a[1,1])/sum(a) #0.8120668
score1 <- (a[2,2]+a[1,1])/sum(a)

data <- data1.2.2.1.1
data$flag <- factor(data$flag)
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

formula2.1.2 <- Formula::as.Formula(data[2:184])
random2.1.2 <- randomForest(formula2.1.2,data = data, mtry = 15,ntree=500 ,importance = FALSE,na.action = na.omit)
pred2 <- predict(random2.1.2,test)
a<-table(pred2,test$flag);a;(a[2,2]+a[1,1])/sum(a) 
score2 <- (a[2,2]+a[1,1])/sum(a)

#LightGBM分析
data <- data1.2.2.1.1
data$flag <- factor(data$flag)
data <- data[,c(-124:-155,-162:-174)]
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

bla1 <- data.matrix(data[3:139])
bla2 <- data$flag
dtrain <- lgb.Dataset(data = bla1, label = bla2)
bla3 <- data.matrix(test[3:139])
bla4 <- test$flag
dtest <- lgb.Dataset.create.valid(dataset = dtrain, data = bla3, label = bla4)
valids <- list(test = dtest)
bla5 <- data.matrix(data[3:139])
params <- list( learning_rate = .08, 
                num_leaves = 200,
                max_bin = 600,
                min_data_in_bin = 45,
                feature_fraction = .54,
                min_sum_hessian = 0.0066, 
                lambda_l1 = .0009,
                lambda_l2 = .0004,
                drop_rate = .4, 
                max_drop = 14)
lgb2.1.1 <- lightgbm(params = params,data = dtrain,nrounds = 600,early_stopping_rounds = 8,
                     num_threads = 2,objective = "regression")
prob <- predict(lgb2.1.1,bla3)
pred3 <- ifelse((prob > 1.5),1,0);summary(factor(pred3));a<-table(pred3,test$flag);a;(a[2,2]+a[1,1])/sum(a);(2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1]))  
score3 <- (a[2,2]+a[1,1])/sum(a)

data <- data1.2.2.1.1
data$flag <- factor(data$flag)
data <- data[,c(-175)]
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

bla1 <- data.matrix(data[3:183])
bla2 <- data$flag
dtrain <- lgb.Dataset(data = bla1, label = bla2)
bla3 <- data.matrix(test[3:183])
bla4 <- test$flag
dtest <- lgb.Dataset.create.valid(dataset = dtrain, data = bla3, label = bla4)
valids <- list(test = dtest)
bla5 <- data.matrix(data[3:183])
params <- list( learning_rate = .08, 
                num_leaves = 200,
                max_bin = 600,
                min_data_in_bin = 45,
                feature_fraction = .54,
                min_sum_hessian = 0.0066, 
                lambda_l1 = .0009,
                lambda_l2 = .0004,
                drop_rate = .4, 
                max_drop = 14)
lgb2.1.2 <- lightgbm(params = params,data = dtrain,nrounds = 600,early_stopping_rounds = 8,
                     num_threads = 2,objective = "regression")
prob <- predict(lgb2.1.2,bla3)
pred4 <- ifelse((prob > 1.48),1,0);summary(factor(pred4))
a<-table(pred4,test$flag);a;(a[2,2]+a[1,1])/sum(a)  # 0.8238804
score4 <- (a[2,2]+a[1,1])/sum(a)

#XGBoost分析
data <- data1.2.2.1.1
data$flag <- factor(data$flag)
data <- data[,c(-124:-155,-162:-174)]
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]
train_matrix <- sparse.model.matrix(flag~.-1, data = data[,c(-1)])
train_label <- as.numeric(data$flag)-1
train_fin <- list(data = train_matrix, label = train_label)
dtrain <- xgb.DMatrix(data =train_fin$data, label = train_fin$label)

test_matrix <- sparse.model.matrix(flag~.-1, data = test[,c(-1)])
test_label <- as.numeric(test$flag)-1
test_fin <- list(data = test_matrix, label = test_label)
dtest <- xgb.DMatrix(data =test_fin$data, label = test_fin$label)
params <- list(
  booster= 'gbtree',
  objective= 'binary:logistic',
  eta = 0.32,
  max_depth = 8,
  subsample = 1.0,
  min_child_weight = 3,
  colsample_bytree = 0.2,
  scale_pos_weight = 0.2,
  eval_metric = 'auc',
  gamma = 0.2,           
  lambda = 200
)
xgb2.1.1 <- xgboost(params = params,data = dtrain,nrounds = 300,early_stopping_rounds = 11,num_threads = 2)
prob <- predict(xgb2.1.1, dtest)
pred5 <- ifelse((prob > 0.25),1,0);a <- table(pred5,test$flag);a;(a[2,2]+a[1,1])/sum(a);2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1])
score5 <- (a[2,2]+a[1,1])/sum(a)


data <- data1.2.2.1.1
data$flag <- factor(data$flag)
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]
train_matrix <- sparse.model.matrix(flag~.-1, data = data[,c(-1)])
train_label <- as.numeric(data$flag)-1
train_fin <- list(data = train_matrix, label = train_label)
dtrain <- xgb.DMatrix(data =train_fin$data, label = train_fin$label)

test_matrix <- sparse.model.matrix(flag~.-1, data = test[,c(-1)])
test_label <- as.numeric(test$flag)-1
test_fin <- list(data = test_matrix, label = test_label)
dtest <- xgb.DMatrix(data =test_fin$data, label = test_fin$label)
params <- list(
  booster= 'gbtree',
  objective= 'binary:logistic',
  eta = 0.32,
  max_depth = 8,
  subsample = 1.0,
  min_child_weight = 3,
  colsample_bytree = 0.2,
  scale_pos_weight = 0.2,
  eval_metric = 'auc',
  gamma = 0.2,           
  lambda = 200
)
xgb2.1.2 <- xgboost(params = params,data = dtrain,nrounds = 300,early_stopping_rounds = 11,num_threads = 2)
prob <- predict(xgb2.1.2, dtest)
pred6 <- ifelse((prob > 0.19),1,0)
a <- table(pred6,test$flag);a;(a[2,2]+a[1,1])/sum(a)  
score6 <- (a[2,2]+a[1,1])/sum(a)

#catboost分析
data <- data1.2.2.1.1
data$flag <- factor(data$flag)
data <- data[,c(-124:-155,-162:-174)]
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

train_feactures <- data[,-1:-2]
train_label <- as.numeric(data$flag)-1
train_pool <- catboost.load_pool(data = train_feactures,label = train_label)
test_pool <- catboost.load_pool(test[,-1:-2])
#params = list(loss_function = 'Logloss',
#              iterations = 1000,
#              metric_period=10,
#              border_count = 32,
#              depth = 5,
#              learning_rate = 0.03,
#              use_best_model = TRUE,
#              l2_leaf_reg = 3.5
#            )
cat2.1.1 <- catboost.train(train_pool, NULL,
                           params = list(loss_function = 'Logloss',
                                         iterations = 500, metric_period=30))
prob <- catboost.predict(cat2.1.1,test_pool)
pred7 <- ifelse((prob > 0.161),1,0);a <- table(pred7,test$flag);a;(a[2,2]+a[1,1])/sum(a);(2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1]))
score7 <- (a[2,2]+a[1,1])/sum(a)


data <- data1.2.2.1.1
data$flag <- factor(data$flag)
data <- data[,c(-175)]
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

train_feactures <- data[,-1:-2]
train_label <- as.numeric(data$flag)-1
train_pool <- catboost.load_pool(data = train_feactures,label = train_label)
test_pool <- catboost.load_pool(test[,-1:-2])
#params = list(loss_function = 'Logloss',
#              iterations = 1000,
#              metric_period=10,
#              border_count = 32,
#              depth = 5,
#              learning_rate = 0.03,
#              use_best_model = TRUE,
#              l2_leaf_reg = 3.5
#            )
cat2.1.2 <- catboost.train(train_pool, NULL,
                           params = list(loss_function = 'Logloss',
                                         iterations = 550, metric_period=30))
prob <- catboost.predict(cat2.1.2,test_pool)
pred8 <- ifelse((prob > 0.165),1,0);a <- table(pred8,test$flag);a;(a[2,2]+a[1,1])/sum(a);(2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1]))
score8 <- (a[2,2]+a[1,1])/sum(a)

prob <- (as.integer(pred1))*score1/(score1 + score2 + score3 + score4 + score5 + score6) + 
  (as.integer(pred2))*score2/(score1 + score2 + score3 + score4 + score5 + score6) + 
  (as.integer(pred3))*score3/(score1 + score2 + score3 + score4 + score5 + score6) +
  (as.integer(pred4))*score4/(score1 + score2 + score3 + score4 + score5 + score6) +
  (as.integer(pred5))*score5/(score1 + score2 + score3 + score4 + score5 + score6) +
  (as.integer(pred6))*score5/(score1 + score2 + score3 + score4 + score5 + score6);summary(factor(prob))
pred <- ifelse((prob > 0.8318),1,0);a <- table(pred,test$flag);a;(a[2,2]+a[1,1])/sum(a)
#-------------------------------------------------------------------------------------
#对data1.2.2.1.2分析
data <- data1.2.2.1.2
data$flag <- factor(data$flag)
data <- data[,c(-81:-94)]
#data <- data[,c(-65:-69,-73:-80)]
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]
#randomFrost分析
formula2.2.1 <- Formula::as.Formula(data[2:90])
random2.2.1 <- randomForest(formula2.2.1,data = data, mtry = 15,ntree=500 ,importance = FALSE,na.action = na.omit)
pred1 <- predict(random2.2.1,test)
a<-table(pred1,test$flag);a;(a[2,2]+a[1,1])/sum(a)
score7 <- (a[2,2]+a[1,1])/sum(a)

data <- data1.2.2.1.2
data$flag <- factor(data$flag)
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]
#randomFrost分析
formula2.2.2 <- Formula::as.Formula(data[2:104])
random2.2.2 <- randomForest(formula2.2.2,data = data, mtry = 15,ntree=500 ,importance = FALSE,na.action = na.omit)
pred2 <- predict(random2.2.2,test)
a<-table(pred2,test$flag);a;(a[2,2]+a[1,1])/sum(a)
score8 <- (a[2,2]+a[1,1])/sum(a)

#LightGBM分析
data <- data1.2.2.1.2
data$flag <- factor(data$flag)
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

bla1 <- data.matrix(data[3:104])
bla2 <- data$flag
dtrain <- lgb.Dataset(data = bla1, label = bla2)
bla3 <- data.matrix(test[3:104])
bla4 <- test$flag
dtest <- lgb.Dataset.create.valid(dataset = dtrain, data = bla3, label = bla4)
valids <- list(test = dtest)
bla5 <- data.matrix(data[3:104])
params <- list(learning_rate = .05, 
               num_leaves = 200, 
               max_bin = 600,
               min_data_in_bin = 45,
               feature_fraction = .54,
               min_sum_hessian = 0.0065,
               lambda_l1 = .0004,
               lambda_l2 = .0005,
               drop_rate = .4,    
               max_drop = 14)
lgb2.2.1 <- lightgbm(params = params,data = dtrain, nrounds = 500,early_stopping_rounds = 10, num_threads = 2,objective = "regression")
prob <- predict(lgb2.2.1,bla3)
pred3 <- ifelse((prob > 1.52),1,0);summary(factor(pred3))
a<-table(pred3,test$flag);a;(a[2,2]+a[1,1])/sum(a)
score9 <- (a[2,2]+a[1,1])/sum(a)

data <- data1.2.2.1.2
data$flag <- factor(data$flag)
data <- data[,c(-81:-94)]
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

bla1 <- data.matrix(data[3:90])
bla2 <- data$flag
dtrain <- lgb.Dataset(data = bla1, label = bla2)
bla3 <- data.matrix(test[3:90])
bla4 <- test$flag
dtest <- lgb.Dataset.create.valid(dataset = dtrain, data = bla3, label = bla4)
valids <- list(test = dtest)
bla5 <- data.matrix(data[3:90])
params <- list(learning_rate = .05, 
               num_leaves = 200, 
               max_bin = 600,
               min_data_in_bin = 45,
               feature_fraction = .54,
               min_sum_hessian = 0.0065,
               lambda_l1 = .0004,
               lambda_l2 = .0005,
               drop_rate = .4,    
               max_drop = 14)
lgb2.2.2 <- lightgbm(params = params,data = dtrain, nrounds = 500,early_stopping_rounds = 10, num_threads = 2,objective = "regression")
prob <- predict(lgb2.2.2,bla3)
pred4 <- ifelse((prob > 1.5),1,0);summary(factor(pred4))
a<-table(pred4,test$flag);a;(a[2,2]+a[1,1])/sum(a)
score10 <- (a[2,2]+a[1,1])/sum(a)

#XGBoost分析
data <- data1.2.2.1.2
data$flag <- factor(data$flag)
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

train_matrix <- sparse.model.matrix(flag~.-1, data = data[,c(-1)])
train_label <- as.numeric(data$flag)-1
train_fin <- list(data = train_matrix, label = train_label)
dtrain <- xgb.DMatrix(data =train_fin$data, label = train_fin$label)

test_matrix <- sparse.model.matrix(flag~.-1, data = test[,c(-1)])
test_label <- as.numeric(test$flag)-1
test_fin <- list(data = test_matrix, label = test_label)
dtest <- xgb.DMatrix(data =test_fin$data, label = test_fin$label)
params <- list(
  booster= 'gbtree',
  objective= 'binary:logistic',
  eta = 0.29,
  max_depth = 8,
  subsample = 1.0,
  min_child_weight = 3,
  colsample_bytree = 0.2,
  scale_pos_weight = 0.2,
  eval_metric = 'auc',
  gamma = 0.2,           
  lambda = 200
)
xgb2.2.1 <- xgboost(params = params,data = dtrain,nrounds = 500,early_stopping_rounds = 11,num_threads = 3)
prob <- predict(xgb2.2.1, dtest)
pred5 <- ifelse((prob > 0.22),1,0);a <- table(pred5,test$flag);a;(a[2,2]+a[1,1])/sum(a)
score11 <- (a[2,2]+a[1,1])/sum(a)


data <- data1.2.2.1.2
data$flag <- factor(data$flag)
data <- data[,c(-81:-94)]
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

train_matrix <- sparse.model.matrix(flag~.-1, data = data[,c(-1)])
train_label <- as.numeric(data$flag)-1
train_fin <- list(data = train_matrix, label = train_label)
dtrain <- xgb.DMatrix(data =train_fin$data, label = train_fin$label)

test_matrix <- sparse.model.matrix(flag~.-1, data = test[,c(-1)])
test_label <- as.numeric(test$flag)-1
test_fin <- list(data = test_matrix, label = test_label)
dtest <- xgb.DMatrix(data =test_fin$data, label = test_fin$label)
params <- list(
  booster= 'gbtree',
  objective= 'binary:logistic',
  eta = 0.29,
  max_depth = 8,
  subsample = 1.0,
  min_child_weight = 3,
  colsample_bytree = 0.2,
  scale_pos_weight = 0.2,
  eval_metric = 'auc',
  gamma = 0.2,           
  lambda = 200
)
xgb2.2.2 <- xgboost(params = params,data = dtrain,nrounds = 500,early_stopping_rounds = 11,num_threads = 3)
prob <- predict(xgb2.2.2, dtest)
pred6 <- ifelse((prob > 0.2),1,0);a <- table(pred6,test$flag);a;(a[2,2]+a[1,1])/sum(a)
score12 <- (a[2,2]+a[1,1])/sum(a)

#catboost
data <- data1.2.2.1.2
data$flag <- factor(data$flag)
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

train_feactures <- data[,-1:-2]
train_label <- as.numeric(data$flag)-1
train_pool <- catboost.load_pool(data = train_feactures,label = train_label)
test_pool <- catboost.load_pool(test[,-1:-2])
#params = list(loss_function = 'Logloss',
#              iterations = 1000,
#              metric_period=10,
#              border_count = 32,
#              depth = 5,
#              learning_rate = 0.03,
#              use_best_model = TRUE,
#              l2_leaf_reg = 3.5
#            )
cat2.2.1 <- catboost.train(train_pool, NULL,
                           params = list(loss_function = 'Logloss',
                                         iterations = 500, metric_period=30))
prob <- catboost.predict(cat2.2.1,test_pool)
pred8 <- ifelse((prob > 0.16),1,0);a <- table(pred8,test$flag);a;(a[2,2]+a[1,1])/sum(a);(2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1]))
score8 <- (a[2,2]+a[1,1])/sum(a)


data <- data1.2.2.1.2
data$flag <- factor(data$flag)
data <- data[,c(-81:-94)]
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

train_feactures <- data[,-1:-2]
train_label <- as.numeric(data$flag)-1
train_pool <- catboost.load_pool(data = train_feactures,label = train_label)
test_pool <- catboost.load_pool(test[,-1:-2])
#params = list(loss_function = 'Logloss',
#              iterations = 1000,
#              metric_period=10,
#              border_count = 32,
#              depth = 5,
#              learning_rate = 0.03,
#              use_best_model = TRUE,
#              l2_leaf_reg = 3.5
#            )
cat2.2.2 <- catboost.train(train_pool, NULL,
                           params = list(loss_function = 'Logloss',
                                         iterations = 550, metric_period=30))
prob <- catboost.predict(cat2.2.2,test_pool)
pred8 <- ifelse((prob > 0.161),1,0);a <- table(pred8,test$flag);a;(a[2,2]+a[1,1])/sum(a);(2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1]))
score8 <- (a[2,2]+a[1,1])/sum(a)

prob <- (as.integer(pred1))*score7/(score7 + score8 + score9 + score10 + score11 + score12) + 
  (as.integer(pred2))*score8/(score7 + score8 + score9 + score10 + score11 + score12) + 
  (as.integer(pred3))*score9/(score7 + score8 + score9 + score10 + score11 + score12) +
  (as.integer(pred4))*score10/(score7 + score8 + score9 + score10 + score11 + score12) +
  (as.integer(pred5))*score11/(score7 + score8 + score9 + score10 + score11 + score12) +
  (as.integer(pred6))*score12/(score7 + score8 + score9 + score10 + score11 + score12);summary(factor(prob))
pred <- ifelse((prob > 0.83227),1,0);a <- table(pred,test$flag);a;(a[2,2]+a[1,1])/sum(a)
#-----------------------------------------------------------------------
#分别对data1.2.1.1.1和data1.2.1.1.2进行randomFrost、LightGBM和XGBoost分析
#-----------------------------------------------------------------------
#对data1.2.1.1.1分析
data <- data1.2.1.1.1
data$flag <- factor(data$flag)
data <- data[,c(-7)]
data <- data[,c(-123:-154,-161:-173)]
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]
#randomFrost分析
formula1.1.1 <- Formula::as.Formula(data[2:138])
random1.1.1 <- randomForest(formula1.1.1,data = data, mtry = 16,ntree=400 ,importance = FALSE,na.action = na.omit)
pred1 <- predict(random1.1.1,test);a <- table(pred1,test$flag);a;(a[2,2]+a[1,1])/sum(a)
score13 <- (a[2,2]+a[1,1])/sum(a)

data <- data1.2.1.1.1
data$flag <- factor(data$flag)
data <- data[,c(-7)]
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

formula1.1.2 <- Formula::as.Formula(data[2:183])
random1.1.2 <- randomForest(formula1.1.2,data = data, mtry = 16,ntree=400 ,importance = FALSE,na.action = na.omit)
pred2 <- predict(random1.1.2,test);a <- table(pred2,test$flag);a;(a[2,2]+a[1,1])/sum(a)
score14 <- (a[2,2]+a[1,1])/sum(a)
#LightGBM分析
data <- data1.2.1.1.1
data$flag <- factor(data$flag)
data <- data[,c(-7)]
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

bla1 <- data.matrix(data[3:183])
bla2 <- data$flag
dtrain <- lgb.Dataset(data = bla1, label = bla2)
bla3 <- data.matrix(test[3:183])
bla4 <- test$flag
dtest <- lgb.Dataset.create.valid(dataset = dtrain, data = bla3, label = bla4)
valids <- list(test = dtest)
bla5 <- data.matrix(data[3:183])
params <- list(learning_rate = .03,
               num_leaves = 200,
               max_bin = 600,
               min_data_in_bin = 45,
               feature_fraction = .54,
               min_sum_hessian = 0.0065,
               lambda_l1 = .0002,
               lambda_l2 = .0004,
               drop_rate = .4,
               max_drop = 14)
lgb1.1.1 <- lightgbm(params = params,data = dtrain,nrounds = 100,early_stopping_rounds = 8,num_threads = 2,objective = "regression")
prob <- predict(lgb1.1.1,bla3)
pred3 <- ifelse((prob > 1.5),1,0);summary(factor(pred3));a<-table(pred3,test$flag);a;(a[2,2]+a[1,1])/sum(a)
score15 <- (a[2,2]+a[1,1])/sum(a)


data <- data1.2.1.1.1
data$flag <- factor(data$flag)
data <- data[,c(-7)]
data <- data[,c(-123:-154,-161:-173)]
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

bla1 <- data.matrix(data[3:138])
bla2 <- data$flag
dtrain <- lgb.Dataset(data = bla1, label = bla2)
bla3 <- data.matrix(test[3:138])
bla4 <- test$flag
dtest <- lgb.Dataset.create.valid(dataset = dtrain, data = bla3, label = bla4)
valids <- list(test = dtest)
bla5 <- data.matrix(data[3:138])
params <- list(learning_rate = .03,
               num_leaves = 200,
               max_bin = 600,
               min_data_in_bin = 45,
               feature_fraction = .54,
               min_sum_hessian = 0.0065,
               lambda_l1 = .0002,
               lambda_l2 = .0004,
               drop_rate = .4,
               max_drop = 14)
lgb1.1.2 <- lightgbm(params = params,data = dtrain,nrounds = 100,early_stopping_rounds = 8,num_threads = 2,objective = "regression")
prob <- predict(lgb1.1.2,bla3)
pred4 <- ifelse((prob > 1.5),1,0);summary(factor(pred4));a<-table(pred4,test$flag);a;(a[2,2]+a[1,1])/sum(a)
score16 <- (a[2,2]+a[1,1])/sum(a)

#XGBoost分析
data <- data1.2.1.1.1
data$flag <- factor(data$flag)
data <- data[,c(-7)]
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

train_matrix <- sparse.model.matrix(flag~.-1, data = data[,c(-1)])
train_label <- as.numeric(data$flag)-1
train_fin <- list(data = train_matrix, label = train_label)
dtrain <- xgb.DMatrix(data =train_fin$data, label = train_fin$label)

test_matrix <- sparse.model.matrix(flag~.-1, data = test[,c(-1)])
test_label <- as.numeric(test$flag)-1
test_fin <- list(data = test_matrix, label = test_label)
dtest <- xgb.DMatrix(data =test_fin$data, label = test_fin$label)
params <- list(
  booster= 'gbtree',
  objective= 'binary:logistic',
  eta = 0.32,
  max_depth = 8,
  subsample = 1.0,
  min_child_weight = 3,
  colsample_bytree = 0.2,
  scale_pos_weight = 0.2,
  eval_metric = 'auc',
  gamma = 0.2,           
  lambda = 200
)
xgb1.1.1 <- xgboost(params = params,data = dtrain,nrounds = 300,early_stopping_rounds = 11,num_threads = 2)
prob <- predict(xgb1.1.1, dtest)
pred5 <- ifelse((prob > 0.25),1,0);a <- table(pred5,test$flag);a;(a[2,2]+a[1,1])/sum(a)
score17 <- (a[2,2]+a[1,1])/sum(a)


data <- data1.2.1.1.1
data$flag <- factor(data$flag)
data <- data[,c(-7)]
data <- data[,c(-123:-154,-161:-173)]
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

train_matrix <- sparse.model.matrix(flag~.-1, data = data[,c(-1)])
train_label <- as.numeric(data$flag)-1
train_fin <- list(data = train_matrix, label = train_label)
dtrain <- xgb.DMatrix(data =train_fin$data, label = train_fin$label)

test_matrix <- sparse.model.matrix(flag~.-1, data = test[,c(-1)])
test_label <- as.numeric(test$flag)-1
test_fin <- list(data = test_matrix, label = test_label)
dtest <- xgb.DMatrix(data =test_fin$data, label = test_fin$label)
params <- list(
  booster= 'gbtree',
  objective= 'binary:logistic',
  eta = 0.32,
  max_depth = 8,
  subsample = 1.0,
  min_child_weight = 3,
  colsample_bytree = 0.2,
  scale_pos_weight = 0.2,
  eval_metric = 'auc',
  gamma = 0.2,           
  lambda = 200
)
xgb1.1.2 <- xgboost(params = params,data = dtrain,nrounds = 300,early_stopping_rounds = 11,num_threads = 2)
prob <- predict(xgb1.1.2, dtest)
pred6 <- ifelse((prob > 0.24),1,0);a <- table(pred6,test$flag);a;(a[2,2]+a[1,1])/sum(a)
score18 <- (a[2,2]+a[1,1])/sum(a)

#catboost
data <- data1.2.1.1.1
data$flag <- factor(data$flag)
data <- data[,c(-7)]
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

train_feactures <- data[,-1:-2]
train_label <- as.numeric(data$flag)-1
train_pool <- catboost.load_pool(data = train_feactures,label = train_label)
test_pool <- catboost.load_pool(test[,-1:-2])
#params = list(loss_function = 'Logloss',
#              iterations = 1000,
#              metric_period=10,
#              border_count = 32,
#              depth = 5,
#              learning_rate = 0.03,
#              use_best_model = TRUE,
#              l2_leaf_reg = 3.5
#            )
cat1.1.1 <- catboost.train(train_pool, NULL,
                           params = list(loss_function = 'Logloss',
                                         iterations = 500, metric_period=30))
prob <- catboost.predict(cat1.1.1,test_pool)
pred8 <- ifelse((prob > 0.16),1,0);a <- table(pred8,test$flag);a;(a[2,2]+a[1,1])/sum(a);(2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1]))
score8 <- (a[2,2]+a[1,1])/sum(a)


data <- data1.2.1.1.1
data$flag <- factor(data$flag)
data <- data[,c(-7)]
data <- data[,c(-123:-154,-161:-173)]
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

train_feactures <- data[,-1:-2]
train_label <- as.numeric(data$flag)-1
train_pool <- catboost.load_pool(data = train_feactures,label = train_label)
test_pool <- catboost.load_pool(test[,-1:-2])
#params = list(loss_function = 'Logloss',
#              iterations = 1000,
#              metric_period=10,
#              border_count = 32,
#              depth = 5,
#              learning_rate = 0.03,
#              use_best_model = TRUE,
#              l2_leaf_reg = 3.5
#            )
cat1.1.2 <- catboost.train(train_pool, NULL,
                           params = list(loss_function = 'Logloss',
                                         iterations = 500, metric_period=30))
prob <- catboost.predict(cat1.1.2,test_pool)
pred8 <- ifelse((prob > 0.14),1,0);a <- table(pred8,test$flag);a;(a[2,2]+a[1,1])/sum(a);(2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1]))
score8 <- (a[2,2]+a[1,1])/sum(a)

prob <- (as.integer(pred1))*score13/(score13 + score14 + score15 + score16 + score17 + score18) + 
  (as.integer(pred2))*score14/(score13 + score14 + score15 + score16 + score17 + score18) + 
  (as.integer(pred3))*score15/(score13 + score14 + score15 + score16 + score17 + score18) +
  (as.integer(pred4))*score16/(score13 + score14 + score15 + score16 + score17 + score18) +
  (as.integer(pred5))*score17/(score13 + score14 + score15 + score16 + score17 + score18) +
  (as.integer(pred6))*score18/(score13 + score14 + score15 + score16 + score17 + score18);summary(factor(prob))
pred <- ifelse((prob > 0.9988),1,0);a <- table(pred,test$flag);a;(a[2,2]+a[1,1])/sum(a)
#---------------------------------------------------------------------------------------
#对data1.2.1.1.2分析
data <- data1.2.1.1.2
data$flag <- factor(data$flag)
data <- data[,c(-7)]
data <- data[,c(-48:-50,-65:-68,-73:-94)]
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]
#randomFrost分析
formula1.2.1 <- Formula::as.Formula(data[2:74])
random1.2.1 <- randomForest(formula1.2.1,data = data, mtry = 16,ntree=400 ,importance = FALSE,na.action = na.omit)
pred1 <- predict(random1.2.1,test);a <- table(pred1,test$flag);a;(a[2,2]+a[1,1])/sum(a)
score19 <- (a[2,2]+a[1,1])/sum(a)

data <- data1.2.1.1.2
data$flag <- factor(data$flag)
data <- data[,c(-7)]
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]
#randomFrost分析
formula1.2.2 <- Formula::as.Formula(data[2:103])
random1.2.2 <- randomForest(formula1.2.2,data = data, mtry = 16,ntree=400 ,importance = FALSE,na.action = na.omit)
pred2 <- predict(random1.2.2,test);a <- table(pred2,test$flag);a;(a[2,2]+a[1,1])/sum(a)
score20 <- (a[2,2]+a[1,1])/sum(a)

#LightGBM分析
data <- data1.2.1.1.2
data$flag <- factor(data$flag)
data <- data[,c(-7)]
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

bla1 <- data.matrix(data[3:103])
bla2 <- data$flag
dtrain <- lgb.Dataset(data = bla1, label = bla2)
bla3 <- data.matrix(test[3:103])
bla4 <- test$flag
dtest <- lgb.Dataset.create.valid(dataset = dtrain, data = bla3, label = bla4)
valids <- list(test = dtest)
bla5 <- data.matrix(data[3:103])
params <- list(learning_rate = .05,
               num_leaves = 200,
               max_bin = 600,
               min_data_in_bin = 45,
               feature_fraction = .54,
               min_sum_hessian = 0.0065,
               lambda_l1 = .0005,
               lambda_l2 = .0005,
               drop_rate = .4,
               max_drop = 14)
lgb1.2.1 <- lightgbm(params = params,data = dtrain,nrounds = 150,early_stopping_rounds = 10,num_threads = 2,objective = "regression")
prob <- predict(lgb1.2.1,bla3)
pred3 <- ifelse((prob > 1.39),1,0);summary(factor(pred3));a <- table(pred3,test$flag);a;(a[2,2]+a[1,1])/sum(a)
score21 <- (a[2,2]+a[1,1])/sum(a)

data <- data1.2.1.1.2
data$flag <- factor(data$flag)
data <- data[,c(-7)]
data <- data[,c(-48:-50,-65:-68,-73:-94)]
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

bla1 <- data.matrix(data[3:74])
bla2 <- data$flag
dtrain <- lgb.Dataset(data = bla1, label = bla2)
bla3 <- data.matrix(test[3:74])
bla4 <- test$flag
dtest <- lgb.Dataset.create.valid(dataset = dtrain, data = bla3, label = bla4)
valids <- list(test = dtest)
bla5 <- data.matrix(data[3:74])
params <- list(learning_rate = .05,
               num_leaves = 200,
               max_bin = 600,
               min_data_in_bin = 45,
               feature_fraction = .54,
               min_sum_hessian = 0.0065,
               lambda_l1 = .0005,
               lambda_l2 = .0005,
               drop_rate = .4,
               max_drop = 14)
lgb1.2.2 <- lightgbm(params = params,data = dtrain,nrounds = 150,early_stopping_rounds = 10,num_threads = 2,objective = "regression")
prob <- predict(lgb1.2.2,bla3)
pred4 <- ifelse((prob > 1.46),1,0);summary(factor(pred4));a <- table(pred4,test$flag);a;(a[2,2]+a[1,1])/sum(a)
score22 <- (a[2,2]+a[1,1])/sum(a)

#XGBoost分析
data <- data1.2.1.1.2
data$flag <- factor(data$flag)
data <- data[,c(-7)]
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

train_matrix <- sparse.model.matrix(flag~.-1, data = data[,c(-1)])
train_label <- as.numeric(data$flag)-1
train_fin <- list(data = train_matrix, label = train_label)
dtrain <- xgb.DMatrix(data =train_fin$data, label = train_fin$label)

test_matrix <- sparse.model.matrix(flag~.-1, data = test[,c(-1)])
test_label <- as.numeric(test$flag)-1
test_fin <- list(data = test_matrix, label = test_label)
dtest <- xgb.DMatrix(data =test_fin$data, label = test_fin$label)
params <- list(
  booster= 'gbtree',
  objective= 'binary:logistic',
  eta = 0.32,
  max_depth = 8,
  subsample = 1.0,
  min_child_weight = 3,
  colsample_bytree = 0.2,
  scale_pos_weight = 0.2,
  eval_metric = 'auc',
  gamma = 0.2,           
  lambda = 200
)
xgb1.2.1 <- xgboost(params = params,data = dtrain,nrounds = 400,early_stopping_rounds = 10,num_threads = 2)
prob <- predict(xgb1.2.1, dtest)
pred5 <- ifelse((prob > 0.18),1,0);a <- table(pred5,test$flag);a;(a[1,1]+a[2,2])/sum(a)
score23 <- (a[2,2]+a[1,1])/sum(a)

data <- data1.2.1.1.2
data$flag <- factor(data$flag)
data <- data[,c(-7)]
data <- data[,c(-48:-50,-65:-68,-73:-94)]
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

train_matrix <- sparse.model.matrix(flag~.-1, data = data[,c(-1)])
train_label <- as.numeric(data$flag)-1
train_fin <- list(data = train_matrix, label = train_label)
dtrain <- xgb.DMatrix(data =train_fin$data, label = train_fin$label)

test_matrix <- sparse.model.matrix(flag~.-1, data = test[,c(-1)])
test_label <- as.numeric(test$flag)-1
test_fin <- list(data = test_matrix, label = test_label)
dtest <- xgb.DMatrix(data =test_fin$data, label = test_fin$label)
params <- list(
  booster= 'gbtree',
  objective= 'binary:logistic',
  eta = 0.32,
  max_depth = 8,
  subsample = 1.0,
  min_child_weight = 3,
  colsample_bytree = 0.2,
  scale_pos_weight = 0.2,
  eval_metric = 'auc',
  gamma = 0.2,           
  lambda = 200
)
xgb1.2.2 <- xgboost(params = params,data = dtrain,nrounds = 400,early_stopping_rounds = 10,num_threads = 2)
prob <- predict(xgb1.2.2, dtest)
pred6 <- ifelse((prob > 0.22),1,0);a <- table(pred6,test$flag);a;(a[1,1]+a[2,2])/sum(a)
score24 <- (a[2,2]+a[1,1])/sum(a)


#catboost
data <- data1.2.1.1.2
data$flag <- factor(data$flag)
data <- data[,c(-7)]
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

train_feactures <- data[,-1:-2]
train_label <- as.numeric(data$flag)-1
train_pool <- catboost.load_pool(data = train_feactures,label = train_label)
test_pool <- catboost.load_pool(test[,-1:-2])
#params = list(loss_function = 'Logloss',
#              iterations = 1000,
#              metric_period=10,
#              border_count = 32,
#              depth = 5,
#              learning_rate = 0.03,
#              use_best_model = TRUE,
#              l2_leaf_reg = 3.5
#            )
cat1.2.1 <- catboost.train(train_pool, NULL,
                           params = list(loss_function = 'Logloss',
                                         iterations = 500, metric_period=30))
prob <- catboost.predict(cat1.2.1,test_pool)
pred8 <- ifelse((prob > 0.13),1,0);a <- table(pred8,test$flag);a;(a[2,2]+a[1,1])/sum(a);(2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1]))
score8 <- (a[2,2]+a[1,1])/sum(a)


data <- data1.2.1.1.2
data$flag <- factor(data$flag)
data <- data[,c(-7)]
data <- data[,c(-48:-50,-65:-68,-73:-94)]
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

train_feactures <- data[,-1:-2]
train_label <- as.numeric(data$flag)-1
train_pool <- catboost.load_pool(data = train_feactures,label = train_label)
test_pool <- catboost.load_pool(test[,-1:-2])
#params = list(loss_function = 'Logloss',
#              iterations = 1000,
#              metric_period=10,
#              border_count = 32,
#              depth = 5,
#              learning_rate = 0.03,
#              use_best_model = TRUE,
#              l2_leaf_reg = 3.5
#            )
cat1.2.2 <- catboost.train(train_pool, NULL,
                           params = list(loss_function = 'Logloss',
                                         iterations = 500, metric_period=30))
prob <- catboost.predict(cat1.2.2,test_pool)
pred8 <- ifelse((prob > 0.14),1,0);a <- table(pred8,test$flag);a;(a[2,2]+a[1,1])/sum(a);(2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1]))
score8 <- (a[2,2]+a[1,1])/sum(a)

prob <- (as.integer(pred1))*score19/(score19 + score20 + score21 + score22 + score23 + score24) + 
  (as.integer(pred2))*score20/(score19 + score20 + score21 + score22 + score23 + score24) + 
  (as.integer(pred3))*score21/(score19 + score20 + score21 + score22 + score23 + score24) +
  (as.integer(pred4))*score22/(score19 + score20 + score21 + score22 + score23 + score24) +
  (as.integer(pred5))*score23/(score19 + score20 + score21 + score22 + score23 + score24) +
  (as.integer(pred6))*score24/(score19 + score20 + score21 + score22 + score23 + score24);summary(factor(prob))
pred <- ifelse((prob > 0.8325),1,0);a <- table(pred,test$flag);a;(a[2,2]+a[1,1])/sum(a)
#--------------------------------------
#分别对data1.1进行Catboost分析
#--------------------------------------
data <- data1.1
data <- data[,c(-3,-4,-6,-7,-50:-70,-103:-158,-167:-338)]
data <- data[,c(-45)]
data$flag <- factor(data$flag)
data$settime <- factor(data$settime)
data$cfvfdpvnumvm0024 <- data$cfvfdpvnumvm0012
data$cfvdctrdamovm0002 <- (as.integer(data$cfvdctrdamovm0001) + as.integer(data$cfvdctrdamovm0003))/2
data$cfvdctrdamovm0024 <- data$cfvdctrdamovm0012
data$cfvcctrdamovm0002 <- (data$cfvcctrdamovm0001 + data$cfvcctrdamovm0003)/2
data$cfvcctrdamovm0024 <- data$cfvcctrdamovm0012
data$cfvcctrdvnumvm0002 <- (data$cfvcctrdvnumvm0001+data$cfvcctrdvnumvm0003)/2
data$cfvcctrdvnumvm0024 <- data$cfvcctrdvnumvm0012
data$covassvhypvm0024 <-data$covassvhypvm0012 - (data$covassvhypvm0006 - data$covassvhypvm0012)
data$covassbalvmaxvm0024 <- data$covassbalvmaxvm0012

train_feactures <- data[,-1:-2]
train_label <- as.numeric(data$flag)-1
train_pool <- catboost.load_pool(data = train_feactures,label = train_label)
test_pool <- catboost.load_pool(data[,-1:-2])

cat <- catboost.train(train_pool, NULL,
                      params = list(loss_function = 'Logloss',
                                    iterations = 500, metric_period=30))
prob <- catboost.predict(cat,test_pool)
pred8 <- ifelse((prob > 0),1,0);a <- table(pred8,data$flag);a;(a[2,2]+a[1,1])/sum(a);(2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1]))

#------------------------------------------------------------
#预测模型test_data:虚构一个flag列,放在和训练集的flag同样的地方或者将数据中的label_flag放进test数据中，位置与train训练数据中的flag相同
#------------------------------------------------------------
data <- test_all

#创建一个虚假的flag变量，也可以直接在测试数据test_all.csv文件中直接加，也可以采用以下方法
#data$flag <- rep(0,times = 340043)
#data <- cbind(data[,c(1,339)],data[,c(2:338)])

data1 <- subset(data, is.na(data$cfvfdpvnumvm0001) == FALSE)
data2 <- subset(data, is.na(data$cfvfdpvnumvm0001) == TRUE)  # flag == 0

#对数据集进行划分，寻找缺失变量的特征;
#根据sex基本变量的NA划分数据集和非NA数据；
data1.1 <- subset(data1,is.na(data1$sex) == TRUE| data1$sex == -1) #划分为NA
data1.2 <- subset(data1, is.na(data1$sex) == FALSE & data1$sex != -1) #划分为非NA
#根据education基本变量的NA进行划分数据集;
data1.2.1 <- subset(data1.2, is.na(data1.2$educate) == TRUE) #划分为NA
data1.2.2 <- subset(data1.2, is.na(data1.2$educate) == FALSE) #划分为非NA

# 去掉：data[,49:54]、data[,55:58]、data[,179:230]、data[,231:282]、data[,283:314]、data[,315:322]、data[,323:330]、data[,336:337]
# 考虑是否去掉、不去掉的话是否可以划分为NA和非NA:data[,59:64]、data[,65:70]、data[,103:158]、data[,167:172]、data[,173:178]
data <- data1.2.2
data1.2.2.1 <- data[,c(-336:-337)];data1.2.2.1 <- data1.2.2.1[,c(-179:-330)];data1.2.2.1<-data1.2.2.1[c(-49:-58)]
#data1.2.2.1 <- data[,c(-179:-330)];data1.2.2.1<-data1.2.2.1[c(-49:-58)]

data <- data1.2.1
data1.2.1.1 <- data[,c(-336:-337)];data1.2.1.1 <- data1.2.1.1[,c(-179:-330)];data1.2.1.1<-data1.2.1.1[c(-49:-58)]


data <- data1.2.2
data1.2.2.2 <- data[,c(-336:-337)];data1.2.2.2 <- data1.2.2.2[,c(-173:-330)];data1.2.2.2 <-data1.2.2.2[,c(-167:-172)];data1.2.2.2<-data1.2.2.2[,c(-103:-158)];data1.2.2.2 <- data1.2.2.2[,c(-49:-70)]

data <- data1.2.1
data1.2.1.2 <- data[,c(-336:-337)];data1.2.1.2 <- data1.2.1.2[,c(-173:-330)];data1.2.1.2 <-data1.2.1.2[,c(-167:-172)];data1.2.1.2<-data1.2.1.2[,c(-103:-158)];data1.2.1.2 <- data1.2.1.2[,c(-49:-70)];


#--------------------------------------------------------------------
##对data1.2.2.1分析
#--------------------------------------------------------------------
data <- data1.2.2.1
data1.2.2.1.1 <- subset(data,is.na(data$cbvchadrevnumvm0012) == FALSE)
data1.2.2.1.2 <- subset(data,is.na(data$cbvchadrevnumvm0012) == TRUE)

#data1.2.2.1.2有哪些是NA呢
#data1.2.2.1.2[49:60]; data1.2.2.1.2[93:148]; data1.2.2.1.2[157:168]
data1.2.2.1.2 <- data1.2.2.1.2[,c(-157:-168)];data1.2.2.1.2 <- data1.2.2.1.2[,c(-93:-148)];data1.2.2.1.2 <- data1.2.2.1.2[,c(-49:-60)]

#对基本信息进行分析
data <- data1.2.2.1.2

# marriage: -1(6577/147188)、1(12711/168412)、2(26629/76210)、3(15:55)、4(4:9)、6(8/72)、7(40/436)、8(36195/290658)、NA(1821/16960)
# devided: -1 1 2 8; 3 4 6 7 NA: 未知、丧偶、离婚、未婚、已婚
data$marriage_satatus <- ifelse((data$marriage_satatus == 3 | data$marriage_satatus == 4 |data$marriage_satatus == 6 |data$marriage_satatus == 7 |data$marriage_satatus == 5),5,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus == 8),4,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus ==2),3,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus == 1),2,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus == -1),1,data$marriage_satatus)
data$marriage_satatus <- factor(data$marriage_satatus, levels = c(1,2,3,4,5))
# occupation: occupation为NA,sex和marriage都为NA时可以单独考虑，另外的空值全为0; NA = 19368
# -1(1997/143481)、1 (827/10201)、2345组合(5103/16537)、6789 10 11 12 13 16 23(8341/104002)、14 15(2269/15179)、
# 17、18和>23(3454/5533)、19 20 (547/6139)、21(7096/52851)、22(5803/6101)
data$occupation <-ifelse((data$occupation == -1| is.na(data$occupation) == TRUE),0,data$occupation)
data$occupation <-ifelse((data$occupation == 1),1,data$occupation)
data$occupation <-ifelse((data$occupation == 2 |data$occupation == 3|data$occupation == 4|data$occupation == 5),2,data$occupation)
data$occupation <-ifelse((data$occupation == 6 |data$occupation == 7|data$occupation == 8|data$occupation == 9|data$occupation == 10|data$occupation == 11|data$occupation == 12|data$occupation == 13|data$occupation == 16|data$occupation == 23 ),3,data$occupation)
data$occupation <-ifelse((data$occupation == 14|data$occupation == 15),4,data$occupation)
data$occupation <-ifelse((data$occupation >23|data$occupation == 17|data$occupation == 18),5,data$occupation)
data$occupation <-ifelse((data$occupation == 19|data$occupation == 20),6,data$occupation)
data$occupation <-ifelse((data$occupation == 21),7,data$occupation)
data$occupation <-ifelse((data$occupation == 22),8,data$occupation)
data$occupation <- factor(data$occupation, levels = c(0,1,2,3,4,5,6,7,8))

# educate: NA部分单独分析、-1 9(3195/13572)、1 3 4 5 8(3195/17545)、2(840/1256)、6 7(453/1521)
data$educate <- ifelse((data$educate == 1 | data$educate == 3 |data$educate == 2),2,data$educate)
data$educate <- ifelse((data$educate == -1),1,data$educate)
data$educate <- ifelse((data$educate == 4),3,data$educate)
data$educate <- ifelse((data$educate == 6 | data$educate == 7| data$educate == 8),4,data$educate)
data$educate <- ifelse((data$educate == 9),5,data$educate)
data$educate <- factor(data$educate, levels = c(1,2,3,4,5))
summary(data$educate)

data1.2.2.1.2 <- data
#对数据data1.2.2.2.2分析
# settime的分析
data1.2.2.1.2$settime <- ifelse((data1.2.2.1.2$settime == 202006),1,data1.2.2.1.2$settime)
data1.2.2.1.2$settime <- ifelse((data1.2.2.1.2$settime == 202007),2,data1.2.2.1.2$settime)
data1.2.2.1.2$settime <- ifelse((data1.2.2.1.2$settime == 202008),3,data1.2.2.1.2$settime)
data1.2.2.1.2$settime <- ifelse((data1.2.2.1.2$settime == 202009),4,data1.2.2.1.2$settime)
data1.2.2.1.2$settime <- ifelse((data1.2.2.1.2$settime == 202010),5,data1.2.2.1.2$settime)
data1.2.2.1.2$settime <- ifelse((data1.2.2.1.2$settime == 202011),6,data1.2.2.1.2$settime)
data1.2.2.1.2$settime <- ifelse((data1.2.2.1.2$settime == 202012),7,data1.2.2.1.2$settime)
data1.2.2.1.2$settime <- ifelse((data1.2.2.1.2$settime == 202101),8,data1.2.2.1.2$settime)
data1.2.2.1.2$settime <- ifelse((data1.2.2.1.2$settime == 202102),9,data1.2.2.1.2$settime)
data1.2.2.1.2$settime <- ifelse((data1.2.2.1.2$settime == 202103),10,data1.2.2.1.2$settime)
data1.2.2.1.2$settime <- ifelse((data1.2.2.1.2$settime == 202104),11,data1.2.2.1.2$settime)
data1.2.2.1.2$settime <- factor(data1.2.2.1.2$settime, levels = c(1,2,3,4,5,6,7,8,9,10,11))

#最后整理的数据
data <- data1.2.2.1.2

#对数据进行标准化
data$flag <- factor(data$flag)
data$sex <- factor(data$sex)
data$online_loans_flag <- factor(data$online_loans_flag)
data$credit_card_flag <- factor(data$credit_card_flag)
data$pre_list_glag <-factor(data$pre_list_glag)
data$merchant_falg <- factor(data$merchant_falg)
data$settime <- factor(data$settime)
data$cfvfdpvnumvm0024 <- data$cfvfdpvnumvm0012
data$cfvdctrdamovm0002 <- (as.integer(data$cfvdctrdamovm0001) + as.integer(data$cfvdctrdamovm0003))/2
data$cfvdctrdamovm0024 <- data$cfvdctrdamovm0012
data$cfvcctrdamovm0002 <- (data$cfvcctrdamovm0001 + data$cfvcctrdamovm0003)/2
data$cfvcctrdamovm0024 <- data$cfvcctrdamovm0012
data$cfvcctrdvnumvm0002 <- (data$cfvcctrdvnumvm0001+data$cfvcctrdvnumvm0003)/2
data$cfvcctrdvnumvm0024 <- data$cfvcctrdvnumvm0012
data$covassvhypvm0024 <-data$covassvhypvm0012 - (data$covassvhypvm0006 - data$covassvhypvm0012)
data$covassbalvmaxvm0024 <- data$covassbalvmaxvm0012
data1.2.2.1.2 <-data

#对data1.2.2.1.1分析
data <- data1.2.2.1.1
# marriage: -1(6577/147188)、1(12711/168412)、2(26629/76210)、3(15:55)、4(4:9)、6(8/72)、7(40/436)、8(36195/290658)、NA(1821/16960)
# devided: -1 1 2 8; 3 4 6 7 NA: 未知、丧偶、离婚、未婚、已婚
data$marriage_satatus <- ifelse((data$marriage_satatus == 3 | data$marriage_satatus == 4 |data$marriage_satatus == 6 |data$marriage_satatus == 7 |data$marriage_satatus == 5),5,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus == 8),4,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus ==2),3,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus == 1),2,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus == -1),1,data$marriage_satatus)
data$marriage_satatus <- factor(data$marriage_satatus, levels = c(1,2,3,4,5))
# occupation: occupation为NA,sex和marriage都为NA时可以单独考虑，另外的空值全为0; NA = 19368
# -1(1997/143481)、1 (827/10201)、2345组合(5103/16537)、6789 10 11 12 13 16 23(8341/104002)、14 15(2269/15179)、
# 17、18和>23(3454/5533)、19 20 (547/6139)、21(7096/52851)、22(5803/6101)
data$occupation <-ifelse((data$occupation == -1 | is.na(data$occupation) == TRUE),0,data$occupation)
data$occupation <-ifelse((data$occupation == 1),1,data$occupation)
data$occupation <-ifelse((data$occupation == 2 |data$occupation == 3|data$occupation == 4|data$occupation == 5),2,data$occupation)
data$occupation <-ifelse((data$occupation == 6 |data$occupation == 7|data$occupation == 8|data$occupation == 9|data$occupation == 10|data$occupation == 11|data$occupation == 12|data$occupation == 13|data$occupation == 16|data$occupation == 23 ),3,data$occupation)
data$occupation <-ifelse((data$occupation == 14|data$occupation == 15),4,data$occupation)
data$occupation <-ifelse((data$occupation >23|data$occupation == 17|data$occupation == 18),5,data$occupation)
data$occupation <-ifelse((data$occupation == 19|data$occupation == 20),6,data$occupation)
data$occupation <-ifelse((data$occupation == 21),7,data$occupation)
data$occupation <-ifelse((data$occupation == 22),8,data$occupation)
data$occupation <- factor(data$occupation, levels = c(0,1,2,3,4,5,6,7,8))

# educate: NA部分单独分析、-1 9(3195/13572)、1 3 4 5 8(3195/17545)、2(840/1256)、6 7(453/1521)
data$educate <- ifelse((data$educate == 1 | data$educate == 3 |data$educate == 2),2,data$educate)
data$educate <- ifelse((data$educate == -1),1,data$educate)
data$educate <- ifelse((data$educate == 4),3,data$educate)
data$educate <- ifelse((data$educate == 6 | data$educate == 7| data$educate == 8),4,data$educate)
data$educate <- ifelse((data$educate == 9),5,data$educate)
data$educate <- factor(data$educate, levels = c(1,2,3,4,5))
summary(data$educate)

data1.2.2.1.1 <- data

# settime的分析
data1.2.2.1.1$settime <- ifelse((data1.2.2.1.1$settime == 202006),1,data1.2.2.1.1$settime)
data1.2.2.1.1$settime <- ifelse((data1.2.2.1.1$settime == 202007),2,data1.2.2.1.1$settime)
data1.2.2.1.1$settime <- ifelse((data1.2.2.1.1$settime == 202008),3,data1.2.2.1.1$settime)
data1.2.2.1.1$settime <- ifelse((data1.2.2.1.1$settime == 202009),4,data1.2.2.1.1$settime)
data1.2.2.1.1$settime <- ifelse((data1.2.2.1.1$settime == 202010),5,data1.2.2.1.1$settime)
data1.2.2.1.1$settime <- ifelse((data1.2.2.1.1$settime == 202011),6,data1.2.2.1.1$settime)
data1.2.2.1.1$settime <- ifelse((data1.2.2.1.1$settime == 202012),7,data1.2.2.1.1$settime)
data1.2.2.1.1$settime <- ifelse((data1.2.2.1.1$settime == 202101),8,data1.2.2.1.1$settime)
data1.2.2.1.1$settime <- ifelse((data1.2.2.1.1$settime == 202102),9,data1.2.2.1.1$settime)
data1.2.2.1.1$settime <- ifelse((data1.2.2.1.1$settime == 202103),10,data1.2.2.1.1$settime)
data1.2.2.1.1$settime <- ifelse((data1.2.2.1.1$settime == 202104),11,data1.2.2.1.1$settime)
data1.2.2.1.1$settime <- factor(data1.2.2.1.1$settime, levels = c(1,2,3,4,5,6,7,8,9,10,11))

#最后整理的数据
data <- data1.2.2.1.1

#对数据进行标准化
data$flag <- factor(data$flag)
data$sex <- factor(data$sex)
data$online_loans_flag <- factor(data$online_loans_flag)
data$credit_card_flag <- factor(data$credit_card_flag)
data$pre_list_glag <-factor(data$pre_list_glag)
data$merchant_falg <- factor(data$merchant_falg)
data$settime <- factor(data$settime)
data$cfvfdpvnumvm0024 <- data$cfvfdpvnumvm0012
data$cfvdctrdamovm0002 <- (as.integer(data$cfvdctrdamovm0001) + as.integer(data$cfvdctrdamovm0003))/2
data$cfvdctrdamovm0024 <- data$cfvdctrdamovm0012
data$cfvcctrdamovm0002 <- (data$cfvcctrdamovm0001 + data$cfvcctrdamovm0003)/2
data$cfvcctrdamovm0024 <- data$cfvcctrdamovm0012
data$cfvcctrdvnumvm0002 <- (data$cfvcctrdvnumvm0001+data$cfvcctrdvnumvm0003)/2
data$cfvcctrdvnumvm0024 <- data$cfvcctrdvnumvm0012
data$covassvhypvm0024 <-data$covassvhypvm0012 - (data$covassvhypvm0006 - data$covassvhypvm0012)
data$covassbalvmaxvm0024 <- data$covassbalvmaxvm0012
data1.2.2.1.1 <-data

#-------------------------------------
#对data1.2.1.1分析
#-------------------------------------
data <- data1.2.1.1
data1.2.1.1.1 <- subset(data,is.na(data$cbvchadrevnumvm0012) == FALSE)
data1.2.1.1.2 <- subset(data,is.na(data$cbvchadrevnumvm0012) == TRUE)

#data1.2.1.1.2有哪些是NA呢
#data1.2.2.1.2[49:60]; data1.2.2.1.2[93:148]; data1.2.2.1.2[157:168]
data1.2.1.1.2 <- data1.2.1.1.2[,c(-157:-168)];data1.2.1.1.2 <- data1.2.1.1.2[,c(-93:-148)];data1.2.1.1.2 <- data1.2.1.1.2[,c(-49:-60)]

#对基本信息进行分析
data <- data1.2.1.1.2

# marriage: -1(6577/147188)、1(12711/168412)、2(26629/76210)、3(15:55)、4(4:9)、6(8/72)、7(40/436)、8(36195/290658)、NA(1821/16960)
# devided: -1 1 2 8; 3 4 6 7 NA: 未知、丧偶、离婚、未婚、已婚
data$marriage_satatus <- ifelse((data$marriage_satatus == 3 | data$marriage_satatus == 4 |data$marriage_satatus == 6 |data$marriage_satatus == 7 |data$marriage_satatus == 5),5,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus == 8),4,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus ==2),3,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus == 1),2,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus == -1),1,data$marriage_satatus)
data$marriage_satatus <- factor(data$marriage_satatus, levels = c(1,2,3,4,5))
# occupation: occupation为NA,sex和marriage都为NA时可以单独考虑，另外的空值全为0; NA = 19368
# -1(1997/143481)、1 (827/10201)、2345组合(5103/16537)、6789 10 11 12 13 16 23(8341/104002)、14 15(2269/15179)、
# 17、18和>23(3454/5533)、19 20 (547/6139)、21(7096/52851)、22(5803/6101)
data$occupation <-ifelse((data$occupation == -1|is.na(data$occupation) == TRUE),0,data$occupation)
data$occupation <-ifelse((data$occupation == 1),1,data$occupation)
data$occupation <-ifelse((data$occupation == 2 |data$occupation == 3|data$occupation == 4|data$occupation == 5),2,data$occupation)
data$occupation <-ifelse((data$occupation == 6 |data$occupation == 7|data$occupation == 8|data$occupation == 9|data$occupation == 10|data$occupation == 11|data$occupation == 12|data$occupation == 13|data$occupation == 16|data$occupation == 23 ),3,data$occupation)
data$occupation <-ifelse((data$occupation == 14|data$occupation == 15),4,data$occupation)
data$occupation <-ifelse((data$occupation >23|data$occupation == 17|data$occupation == 18),5,data$occupation)
data$occupation <-ifelse((data$occupation == 19|data$occupation == 20),6,data$occupation)
data$occupation <-ifelse((data$occupation == 21),7,data$occupation)
data$occupation <-ifelse((data$occupation == 22),8,data$occupation)
data$occupation <- factor(data$occupation, levels = c(0,1,2,3,4,5,6,7,8))

data1.2.1.1.2 <- data
#对数据data1.2.2.2.2分析
# settime的分析
data1.2.1.1.2$settime <- ifelse((data1.2.1.1.2$settime == 202006),1,data1.2.1.1.2$settime)
data1.2.1.1.2$settime <- ifelse((data1.2.1.1.2$settime == 202007),2,data1.2.1.1.2$settime)
data1.2.1.1.2$settime <- ifelse((data1.2.1.1.2$settime == 202008),3,data1.2.1.1.2$settime)
data1.2.1.1.2$settime <- ifelse((data1.2.1.1.2$settime == 202009),4,data1.2.1.1.2$settime)
data1.2.1.1.2$settime <- ifelse((data1.2.1.1.2$settime == 202010),5,data1.2.1.1.2$settime)
data1.2.1.1.2$settime <- ifelse((data1.2.1.1.2$settime == 202011),6,data1.2.1.1.2$settime)
data1.2.1.1.2$settime <- ifelse((data1.2.1.1.2$settime == 202012),7,data1.2.1.1.2$settime)
data1.2.1.1.2$settime <- ifelse((data1.2.1.1.2$settime == 202101),8,data1.2.1.1.2$settime)
data1.2.1.1.2$settime <- ifelse((data1.2.1.1.2$settime == 202102),9,data1.2.1.1.2$settime)
data1.2.1.1.2$settime <- ifelse((data1.2.1.1.2$settime == 202103),10,data1.2.1.1.2$settime)
data1.2.1.1.2$settime <- ifelse((data1.2.1.1.2$settime == 202104),11,data1.2.1.1.2$settime)
data1.2.1.1.2$settime <- factor(data1.2.1.1.2$settime, levels = c(1,2,3,4,5,6,7,8,9,10,11))

#最后整理的数据
data <- data1.2.1.1.2

#对数据进行标准化
data$flag <- factor(data$flag)
data$sex <- factor(data$sex)
data$online_loans_flag <- factor(data$online_loans_flag)
data$credit_card_flag <- factor(data$credit_card_flag)
data$pre_list_glag <-factor(data$pre_list_glag)
data$merchant_falg <- factor(data$merchant_falg)
data$settime <- factor(data$settime)
data$cfvfdpvnumvm0024 <- data$cfvfdpvnumvm0012
data$cfvdctrdamovm0002 <- (as.integer(data$cfvdctrdamovm0001) + as.integer(data$cfvdctrdamovm0003))/2
data$cfvdctrdamovm0024 <- data$cfvdctrdamovm0012
data$cfvcctrdamovm0002 <- (data$cfvcctrdamovm0001 + data$cfvcctrdamovm0003)/2
data$cfvcctrdamovm0024 <- data$cfvcctrdamovm0012
data$cfvcctrdvnumvm0002 <- (data$cfvcctrdvnumvm0001+data$cfvcctrdvnumvm0003)/2
data$cfvcctrdvnumvm0024 <- data$cfvcctrdvnumvm0012
data$covassvhypvm0024 <-data$covassvhypvm0012 - (data$covassvhypvm0006 - data$covassvhypvm0012)
data$covassbalvmaxvm0024 <- data$covassbalvmaxvm0012
data1.2.1.1.2 <-data

#对data1.2.1.1.1分析
data <- data1.2.1.1.1
# marriage: -1(6577/147188)、1(12711/168412)、2(26629/76210)、3(15:55)、4(4:9)、6(8/72)、7(40/436)、8(36195/290658)、NA(1821/16960)
# devided: -1 1 2 8; 3 4 6 7 NA: 未知、丧偶、离婚、未婚、已婚
data$marriage_satatus <- ifelse((data$marriage_satatus == 3 | data$marriage_satatus == 4 |data$marriage_satatus == 6 |data$marriage_satatus == 7 |data$marriage_satatus == 5),5,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus == 8),4,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus ==2),3,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus == 1),2,data$marriage_satatus)
data$marriage_satatus <- ifelse((data$marriage_satatus == -1),1,data$marriage_satatus)
data$marriage_satatus <- factor(data$marriage_satatus, levels = c(1,2,3,4,5))
# occupation: occupation为NA,sex和marriage都为NA时可以单独考虑，另外的空值全为0; NA = 19368
# -1(1997/143481)、1 (827/10201)、2345组合(5103/16537)、6789 10 11 12 13 16 23(8341/104002)、14 15(2269/15179)、
# 17、18和>23(3454/5533)、19 20 (547/6139)、21(7096/52851)、22(5803/6101)
data$occupation <-ifelse((data$occupation == -1 | is.na(data$occupation) == TRUE),0,data$occupation)
data$occupation <-ifelse((data$occupation == 1),1,data$occupation)
data$occupation <-ifelse((data$occupation == 2 |data$occupation == 3|data$occupation == 4|data$occupation == 5),2,data$occupation)
data$occupation <-ifelse((data$occupation == 6 |data$occupation == 7|data$occupation == 8|data$occupation == 9|data$occupation == 10|data$occupation == 11|data$occupation == 12|data$occupation == 13|data$occupation == 16|data$occupation == 23 ),3,data$occupation)
data$occupation <-ifelse((data$occupation == 14|data$occupation == 15),4,data$occupation)
data$occupation <-ifelse((data$occupation >23|data$occupation == 17|data$occupation == 18),5,data$occupation)
data$occupation <-ifelse((data$occupation == 19|data$occupation == 20),6,data$occupation)
data$occupation <-ifelse((data$occupation == 21),7,data$occupation)
data$occupation <-ifelse((data$occupation == 22),8,data$occupation)
data$occupation <- factor(data$occupation, levels = c(0,1,2,3,4,5,6,7,8))

data1.2.1.1.1 <- data

# settime的分析
data1.2.1.1.1$settime <- ifelse((data1.2.1.1.1$settime == 202006),1,data1.2.1.1.1$settime)
data1.2.1.1.1$settime <- ifelse((data1.2.1.1.1$settime == 202007),2,data1.2.1.1.1$settime)
data1.2.1.1.1$settime <- ifelse((data1.2.1.1.1$settime == 202008),3,data1.2.1.1.1$settime)
data1.2.1.1.1$settime <- ifelse((data1.2.1.1.1$settime == 202009),4,data1.2.1.1.1$settime)
data1.2.1.1.1$settime <- ifelse((data1.2.1.1.1$settime == 202010),5,data1.2.1.1.1$settime)
data1.2.1.1.1$settime <- ifelse((data1.2.1.1.1$settime == 202011),6,data1.2.1.1.1$settime)
data1.2.1.1.1$settime <- ifelse((data1.2.1.1.1$settime == 202012),7,data1.2.1.1.1$settime)
data1.2.1.1.1$settime <- ifelse((data1.2.1.1.1$settime == 202101),8,data1.2.1.1.1$settime)
data1.2.1.1.1$settime <- ifelse((data1.2.1.1.1$settime == 202102),9,data1.2.1.1.1$settime)
data1.2.1.1.1$settime <- ifelse((data1.2.1.1.1$settime == 202103),10,data1.2.1.1.1$settime)
data1.2.1.1.1$settime <- ifelse((data1.2.1.1.1$settime == 202104),11,data1.2.1.1.1$settime)
data1.2.1.1.1$settime <- factor(data1.2.1.1.1$settime, levels = c(1,2,3,4,5,6,7,8,9,10,11))

#最后整理的数据
data <- data1.2.1.1.1

#对数据进行标准化
data$flag <- factor(data$flag)
data$sex <- factor(data$sex)
data$online_loans_flag <- factor(data$online_loans_flag)
data$credit_card_flag <- factor(data$credit_card_flag)
data$pre_list_glag <-factor(data$pre_list_glag)
data$merchant_falg <- factor(data$merchant_falg)
data$settime <- factor(data$settime)
data$cfvfdpvnumvm0024 <- data$cfvfdpvnumvm0012
data$cfvdctrdamovm0002 <- (as.integer(data$cfvdctrdamovm0001) + as.integer(data$cfvdctrdamovm0003))/2
data$cfvdctrdamovm0024 <- data$cfvdctrdamovm0012
data$cfvcctrdamovm0002 <- (data$cfvcctrdamovm0001 + data$cfvcctrdamovm0003)/2
data$cfvcctrdamovm0024 <- data$cfvcctrdamovm0012
data$cfvcctrdvnumvm0002 <- (data$cfvcctrdvnumvm0001+data$cfvcctrdvnumvm0003)/2
data$cfvcctrdvnumvm0024 <- data$cfvcctrdvnumvm0012
data$covassvhypvm0024 <-data$covassvhypvm0012 - (data$covassvhypvm0006 - data$covassvhypvm0012)
data$covassbalvmaxvm0024 <- data$covassbalvmaxvm0012
data1.2.1.1.1 <-data

#-------------------------------------------------------------------------------------------------------------
#分别对data1.2.2.1.1/data1.2.2.1.2/data1.2.1.1.1/data1.2.1.1.2进行randomFrost、LightGBM、XGBoost和CatBoost分析
#-------------------------------------------------------------------------------------------------------------
#预测1
data <- data1.2.2.1.1
data <- data[,c(-124:-155,-162:-174)]
pred1 <- as.integer(predict(random2.1.1,data))-1
data <- data1.2.2.1.1
pred2 <- as.integer(predict(random2.1.2,data))-1

data <- data1.2.2.1.1
data <- data[,c(-124:-155,-162:-174)]
bla1 <- data.matrix(data[3:139])
prob <- predict(lgb2.1.1,bla1)
pred3 <- ifelse((prob > 1.542),1,0)
data <- data1.2.2.1.1
bla1 <- data.matrix(data[3:184])
prob <- predict(lgb2.1.2,bla1)
pred4 <- ifelse((prob > 1.51),1,0)

data <- data1.2.2.1.1
data <- data[,c(-124:-155,-162:-174)]
train_matrix <- sparse.model.matrix(flag~.-1, data = data[,c(-1)])
train_label <- as.numeric(data$flag)-1
train_fin <- list(data = train_matrix, label = train_label)
dtrain <- xgb.DMatrix(data =train_fin$data, label = train_fin$label)
prob <- predict(xgb2.1.1, dtrain)
pred5 <- ifelse((prob > 0.221),1,0)
data <- data1.2.2.1.1
train_matrix <- sparse.model.matrix(flag~.-1, data = data[,c(-1)])
train_label <- as.numeric(data$flag)-1
train_fin <- list(data = train_matrix, label = train_label)
dtrain <- xgb.DMatrix(data =train_fin$data, label = train_fin$label)
prob <- predict(xgb2.1.2, dtrain)
pred6 <- ifelse((prob > 0.202),1,0)

data <- data1.2.2.1.1
data <- data[,c(-124:-155,-162:-174)]
test_pool <- catboost.load_pool(data[,-1:-2])
prob <- catboost.predict(cat2.1.1,test_pool)
pred7 <- ifelse((prob > 0.095),1,0)
data <- data1.2.2.1.1
test_pool <- catboost.load_pool(data[,-1:-2])
prob <- catboost.predict(cat2.1.2,test_pool)
pred8 <- ifelse((prob > 0.095),1,0)

prob <- (as.integer(pred1))*(1/8) + (as.integer(pred2))*(1/8) + (as.integer(pred3))*(1/8) +
  (as.integer(pred4))*(1/8) + (as.integer(pred5))*(1/8) + (as.integer(pred6))*(1/8) +
  (as.integer(pred7))*(1/8)+(as.integer(pred8))*(1/8);summary(factor(prob))
pred <- ifelse((prob >= 0.625),1,0)
data1.2.2.1.1$class <- pred
#-------------------------------------------------------------------------------------------------------
data <- data1.2.2.1.2
data <- data[,c(-81:-94)]
pred1 <- as.integer(predict(random2.2.1,data))-1
data <- data1.2.2.1.2
pred2 <- as.integer(predict(random2.2.2,data))-1

data <- data1.2.2.1.2
bla1 <- data.matrix(data[3:104])
prob <- predict(lgb2.2.1,bla1)
pred3 <- ifelse((prob > 1.48),1,0)
data <- data1.2.2.1.2
data <- data[,c(-81:-94)]
bla1 <- data.matrix(data[3:90])
prob <- predict(lgb2.2.2,bla1)
pred4 <- ifelse((prob > 1.495),1,0)

data <- data1.2.2.1.2
train_matrix <- sparse.model.matrix(flag~.-1, data = data[,c(-1)])
train_label <- as.numeric(data$flag)-1
train_fin <- list(data = train_matrix, label = train_label)
dtrain <- xgb.DMatrix(data =train_fin$data, label = train_fin$label)
prob <- predict(xgb2.2.1, dtrain)
pred5 <- ifelse((prob > 0.22),1,0)
data <- data1.2.2.1.2
data <- data[,c(-81:-94)]
train_matrix <- sparse.model.matrix(flag~.-1, data = data[,c(-1)])
train_label <- as.numeric(data$flag)-1
train_fin <- list(data = train_matrix, label = train_label)
dtrain <- xgb.DMatrix(data =train_fin$data, label = train_fin$label)
prob <- predict(xgb2.2.2, dtrain)
pred6 <- ifelse((prob > 0.191),1,0)

data <- data1.2.2.1.2
test_pool <- catboost.load_pool(data[,-1:-2])
prob <- catboost.predict(cat2.2.1,test_pool)
pred7 <- ifelse((prob >0),1,0)
data <- data1.2.2.1.2
data <- data[,c(-81:-94)]
test_pool <- catboost.load_pool(data[,-1:-2])
prob <- catboost.predict(cat2.2.2,test_pool)
pred8 <- ifelse((prob >0.01),1,0)

prob <- (as.integer(pred1))*(1/8) + (as.integer(pred2))*(1/8) + (as.integer(pred3))*(1/8) +
  (as.integer(pred4))*(1/8) + (as.integer(pred5))*(1/8) + (as.integer(pred6))*(1/8) +
  (as.integer(pred7))*(1/8)+(as.integer(pred8))*(1/8);summary(factor(prob))
pred <- ifelse((prob >= 0.625),1,0)
data1.2.2.1.2$class <- pred
#----------------------------------------------------------------------------------------------
data <- data1.2.1.1.1
data <- data[,c(-7)]
data <- data[,c(-123:-154,-161:-173)]
pred1 <- as.integer(predict(random1.1.1,data))-1
data <- data1.2.1.1.1
data <- data[,c(-7)]
pred2 <- as.integer(predict(random1.1.2,data))-1

data <- data1.2.1.1.1
data <- data[,c(-7)]
bla1 <- data.matrix(data[3:183])
prob <- predict(lgb1.1.1,bla1)
pred3 <- ifelse((prob > 1.48),1,0)
data <- data1.2.1.1.1
data <- data[,c(-7)]
data <- data[,c(-123:-154,-161:-173)]
bla1 <- data.matrix(data[3:138])
prob <- predict(lgb1.1.2,bla1)
pred4 <- ifelse((prob > 1.55),1,0)

data <- data1.2.1.1.1
data <- data[,c(-7)]
train_matrix <- sparse.model.matrix(flag~.-1, data = data[,c(-1)])
train_label <- as.numeric(data$flag)-1
train_fin <- list(data = train_matrix, label = train_label)
dtrain <- xgb.DMatrix(data =train_fin$data, label = train_fin$label)
prob <- predict(xgb1.1.1, dtrain)
pred5 <- ifelse((prob > 0.18),1,0)
data <- data1.2.1.1.1
data <- data[,c(-7)]
data <- data[,c(-123:-154,-161:-173)]
train_matrix <- sparse.model.matrix(flag~.-1, data = data[,c(-1)])
train_label <- as.numeric(data$flag)-1
train_fin <- list(data = train_matrix, label = train_label)
dtrain <- xgb.DMatrix(data =train_fin$data, label = train_fin$label)
prob <- predict(xgb1.1.2, dtrain)
pred6 <- ifelse((prob > 0.2),1,0)

data <- data1.2.1.1.1
data <- data[,c(-7)]
test_pool <- catboost.load_pool(data[,-1:-2])
prob <- catboost.predict(cat1.1.1,test_pool)
pred7 <- ifelse((prob >0.02),1,0)
data <- data1.2.1.1.1
data <- data[,c(-7)]
data <- data[,c(-123:-154,-161:-173)]
test_pool <- catboost.load_pool(data[,-1:-2])
prob <- catboost.predict(cat1.1.2,test_pool)
pred8 <- ifelse((prob >-0.1),1,0)

prob <- (as.integer(pred1))*(1/8) + (as.integer(pred2))*(1/8) + (as.integer(pred3))*(1/8) +
  (as.integer(pred4))*(1/8) + (as.integer(pred5))*(1/8) + (as.integer(pred6))*(1/8) +
  (as.integer(pred7))*(1/8)+(as.integer(pred8))*(1/8);summary(factor(prob))
pred <- ifelse((prob >= 0.725),1,0)
data1.2.1.1.1$class <- pred
#-------------------------------------------------------------------------------------
data <- data1.2.1.1.2
data <- data[,c(-7)]
data <- data[,c(-48:-50,-65:-68,-73:-94)]
pred1 <- as.integer(predict(random1.2.1,data))-1
data <- data1.2.1.1.2
data <- data[,c(-7)]
pred2 <- as.integer(predict(random1.2.2,data))-1

data <- data1.2.1.1.2
data <- data[,c(-7)]
bla1 <- data.matrix(data[3:103])
prob <- predict(lgb1.2.1,bla1)
pred3 <- ifelse((prob > 1.43),1,0)
data <- data1.2.1.1.2
data <- data[,c(-7)]
data <- data[,c(-48:-50,-65:-68,-73:-94)]
bla1 <- data.matrix(data[3:74])
prob <- predict(lgb1.2.2,bla1)
pred4 <- ifelse((prob > 1.43),1,0)

data <- data1.2.1.1.2
data <- data[,c(-7)]
train_matrix <- sparse.model.matrix(flag~.-1, data = data[,c(-1)])
train_label <- as.numeric(data$flag)-1
train_fin <- list(data = train_matrix, label = train_label)
dtrain <- xgb.DMatrix(data =train_fin$data, label = train_fin$label)
prob <- predict(xgb1.2.1, dtrain)
pred5 <- ifelse((prob > 0.14),1,0)
data <- data1.2.1.1.2
data <- data[,c(-7)]
data <- data[,c(-48:-50,-65:-68,-73:-94)]
train_matrix <- sparse.model.matrix(flag~.-1, data = data[,c(-1)])
train_label <- as.numeric(data$flag)-1
train_fin <- list(data = train_matrix, label = train_label)
dtrain <- xgb.DMatrix(data =train_fin$data, label = train_fin$label)
prob <- predict(xgb1.2.2, dtrain)
pred6 <- ifelse((prob > 0.11),1,0)

data <- data1.2.1.1.2
data <- data[,c(-7)]
test_pool <- catboost.load_pool(data[,-1:-2])
prob <- catboost.predict(cat1.2.1,test_pool)
pred7 <- ifelse((prob >-0.3),1,0)
data <- data1.2.1.1.2
data <- data[,c(-7)]
data <- data[,c(-48:-50,-65:-68,-73:-94)]
test_pool <- catboost.load_pool(data[,-1:-2])
prob <- catboost.predict(cat1.2.2,test_pool)
pred8 <- ifelse((prob >-0.3),1,0)

prob <- (as.integer(pred1))*(1/8) + (as.integer(pred2))*(1/8) + (as.integer(pred3))*(1/8) +
  (as.integer(pred4))*(1/8) + (as.integer(pred5))*(1/8) + (as.integer(pred6))*(1/8) +
  (as.integer(pred7))*(1/8)+(as.integer(pred8))*(1/8);summary(factor(prob))
pred <- ifelse((prob >= 0.625),1,0)
data1.2.1.1.2$class <- pred
#-------------------------------------------------------------------------------------
data <- data1.1
data <- data[,c(-3,-4,-6,-7,-50:-70,-103:-158,-167:-338)]
data <- data[,c(-45)]
data$flag <- factor(data$flag)
data$settime <- factor(data$settime)
data$cfvfdpvnumvm0024 <- data$cfvfdpvnumvm0012
data$cfvdctrdamovm0002 <- (as.integer(data$cfvdctrdamovm0001) + as.integer(data$cfvdctrdamovm0003))/2
data$cfvdctrdamovm0024 <- data$cfvdctrdamovm0012
data$cfvcctrdamovm0002 <- (data$cfvcctrdamovm0001 + data$cfvcctrdamovm0003)/2
data$cfvcctrdamovm0024 <- data$cfvcctrdamovm0012
data$cfvcctrdvnumvm0002 <- (data$cfvcctrdvnumvm0001+data$cfvcctrdvnumvm0003)/2
data$cfvcctrdvnumvm0024 <- data$cfvcctrdvnumvm0012
data$covassvhypvm0024 <-data$covassvhypvm0012 - (data$covassvhypvm0006 - data$covassvhypvm0012)
data$covassbalvmaxvm0024 <- data$covassbalvmaxvm0012

test_pool <- catboost.load_pool(data[,-1:-2])
prob <- catboost.predict(cat,test_pool)
pred <- ifelse((prob >0),1,0);a <- table(pred, data$flag);a;2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1]);(a[1,1]+a[2,2])/sum(a)
data1.1$class <- pred

a <- rbind(data1.2.1.1.1[,c(1,185)],data1.2.1.1.2[,c(1,105)],data1.2.2.1.1[,c(1,185)],data1.2.2.1.2[,c(1,105)],data1.1[,c(1,340)])
data2$class <- rep(0,times = 274924)
b <- rbind(a[,c(1,2)],data2[,c(1,340)]);b<- b[order(b$cust_id),]
data1.2.1.1.1<- data1.2.1.1.1[,c(-185)];data1.2.1.1.2 <- data1.2.1.1.2[,c(-105)];data1.2.2.1.1 <- data1.2.2.1.1[,c(-185)];data1.2.2.1.2 <- data1.2.2.1.2[,c(-105)]

#---------------------------------------------------------------------------------------------------
#预测2
data <- data1.2.2.1.1
data <- data[,c(-124:-155,-162:-174)]
pred <- predict(random2.1.1,data)
data1.2.2.1.1$class <- pred

data <- data1.2.2.1.2
data <- data[,c(-81:-94)]
pred <- predict(random2.2.1,data)
data1.2.2.1.2$class <- pred

data <- data1.2.1.1.1
data <- data[,c(-7)]
data <- data[,c(-123:-154,-161:-173)]
pred <- predict(random1.1.1,data)
data1.2.1.1.1$class <- pred

data <- data1.2.1.1.2
data <- data[,c(-7)]
data <- data[,c(-48:-50,-65:-68,-73:-94)]
pred <- predict(random1.2.1,data)
data1.2.1.1.2$class <- pred

data <- data1.1
data <- data[,c(-3,-4,-6,-7,-50:-70,-103:-158,-167:-338)]
data <- data[,c(-45)]
data$flag <- factor(data$flag)
data$settime <- factor(data$settime)
data$cfvfdpvnumvm0024 <- data$cfvfdpvnumvm0012
data$cfvdctrdamovm0002 <- (as.integer(data$cfvdctrdamovm0001) + as.integer(data$cfvdctrdamovm0003))/2
data$cfvdctrdamovm0024 <- data$cfvdctrdamovm0012
data$cfvcctrdamovm0002 <- (data$cfvcctrdamovm0001 + data$cfvcctrdamovm0003)/2
data$cfvcctrdamovm0024 <- data$cfvcctrdamovm0012
data$cfvcctrdvnumvm0002 <- (data$cfvcctrdvnumvm0001+data$cfvcctrdvnumvm0003)/2
data$cfvcctrdvnumvm0024 <- data$cfvcctrdvnumvm0012
data$covassvhypvm0024 <-data$covassvhypvm0012 - (data$covassvhypvm0006 - data$covassvhypvm0012)
data$covassbalvmaxvm0024 <- data$covassbalvmaxvm0012
test_pool <- catboost.load_pool(data[,-1:-2])
prob <- catboost.predict(cat,test_pool)
pred <- ifelse((prob >0),1,0);a <- table(pred, data$flag);a;2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1]);(a[1,1]+a[2,2])/sum(a)
data1.1$class <- pred

a <- rbind(data1.2.1.1.1[,c(1,185)],data1.2.1.1.2[,c(1,105)],data1.2.2.1.1[,c(1,185)],data1.2.2.1.2[,c(1,105)],data1.1[,c(1,340)])
data2$class <- rep(0,times = 274924)
c <- rbind(a[,c(1,2)],data2[,c(1,340)]);c<- c[order(c$cust_id),]
b$class1 <- c$class
data1.2.1.1.1<- data1.2.1.1.1[,c(-185)];data1.2.1.1.2 <- data1.2.1.1.2[,c(-105)];data1.2.2.1.1 <- data1.2.2.1.1[,c(-185)];data1.2.2.1.2 <- data1.2.2.1.2[,c(-105)]

#-----------------------------------------------------------------------------
#预测3
data <- data1.2.2.1.1
pred <- predict(random2.1.2,data)
data1.2.2.1.1$class <- pred

data <- data1.2.2.1.2
pred <- predict(random2.2.2,data)
data1.2.2.1.2$class <- pred

data <- data1.2.1.1.1
data <- data[,c(-7)]
pred <- predict(random1.1.2,data)
data1.2.1.1.1$class <- pred

data <- data1.2.1.1.2
data <- data[,c(-7)]
pred <- predict(random1.2.2,data)
data1.2.1.1.2$class <- pred

data <- data1.1
data <- data[,c(-3,-4,-6,-7,-50:-70,-103:-158,-167:-338)]
data <- data[,c(-45)]
data$flag <- factor(data$flag)
data$settime <- factor(data$settime)
data$cfvfdpvnumvm0024 <- data$cfvfdpvnumvm0012
data$cfvdctrdamovm0002 <- (as.integer(data$cfvdctrdamovm0001) + as.integer(data$cfvdctrdamovm0003))/2
data$cfvdctrdamovm0024 <- data$cfvdctrdamovm0012
data$cfvcctrdamovm0002 <- (data$cfvcctrdamovm0001 + data$cfvcctrdamovm0003)/2
data$cfvcctrdamovm0024 <- data$cfvcctrdamovm0012
data$cfvcctrdvnumvm0002 <- (data$cfvcctrdvnumvm0001+data$cfvcctrdvnumvm0003)/2
data$cfvcctrdvnumvm0024 <- data$cfvcctrdvnumvm0012
data$covassvhypvm0024 <-data$covassvhypvm0012 - (data$covassvhypvm0006 - data$covassvhypvm0012)
data$covassbalvmaxvm0024 <- data$covassbalvmaxvm0012
test_pool <- catboost.load_pool(data[,-1:-2])
prob <- catboost.predict(cat,test_pool)
pred <- ifelse((prob >0),1,0);a <- table(pred, data$flag);a;2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1]);(a[1,1]+a[2,2])/sum(a)
data1.1$class <- pred

a <- rbind(data1.2.1.1.1[,c(1,185)],data1.2.1.1.2[,c(1,105)],data1.2.2.1.1[,c(1,185)],data1.2.2.1.2[,c(1,105)],data1.1[,c(1,340)])
data2$class <- rep(0,times = 274924)
c <- rbind(a[,c(1,2)],data2[,c(1,340)]);c<- c[order(c$cust_id),]
b$class2 <- c$class
data1.2.1.1.1<- data1.2.1.1.1[,c(-185)];data1.2.1.1.2 <- data1.2.1.1.2[,c(-105)];data1.2.2.1.1 <- data1.2.2.1.1[,c(-185)];data1.2.2.1.2 <- data1.2.2.1.2[,c(-105)]
#---------------------------------------------------------------------------------------------------------------
#预测四
data <- data1.2.2.1.1
data <- data[,c(-124:-155,-162:-174)]
bla1 <- data.matrix(data[3:139])
prob <- predict(lgb2.1.1,bla1)
pred <- ifelse((prob > 1.542),1,0)
data1.2.2.1.1$class <- pred

data <- data1.2.2.1.2
data <- data[,c(-81:-94)]
bla1 <- data.matrix(data[3:90])
prob <- predict(lgb2.2.2,bla1)
pred <- ifelse((prob > 1.495),1,0)
data1.2.2.1.2$class <- pred

data <- data1.2.1.1.1
data <- data[,c(-7)]
data <- data[,c(-123:-154,-161:-173)]
bla1 <- data.matrix(data[3:138])
prob <- predict(lgb1.1.2,bla1)
pred <- ifelse((prob > 1.55),1,0)
data1.2.1.1.1$class <- pred

data <- data1.2.1.1.2
data <- data[,c(-7)]
data <- data[,c(-48:-50,-65:-68,-73:-94)]
bla1 <- data.matrix(data[3:74])
prob <- predict(lgb1.2.2,bla1)
pred <- ifelse((prob > 1.43),1,0)
data1.2.1.1.2$class <- pred

data <- data1.1
data <- data[,c(-3,-4,-6,-7,-50:-70,-103:-158,-167:-338)]
data <- data[,c(-45)]
data$flag <- factor(data$flag)
data$settime <- factor(data$settime)
data$cfvfdpvnumvm0024 <- data$cfvfdpvnumvm0012
data$cfvdctrdamovm0002 <- (as.integer(data$cfvdctrdamovm0001) + as.integer(data$cfvdctrdamovm0003))/2
data$cfvdctrdamovm0024 <- data$cfvdctrdamovm0012
data$cfvcctrdamovm0002 <- (data$cfvcctrdamovm0001 + data$cfvcctrdamovm0003)/2
data$cfvcctrdamovm0024 <- data$cfvcctrdamovm0012
data$cfvcctrdvnumvm0002 <- (data$cfvcctrdvnumvm0001+data$cfvcctrdvnumvm0003)/2
data$cfvcctrdvnumvm0024 <- data$cfvcctrdvnumvm0012
data$covassvhypvm0024 <-data$covassvhypvm0012 - (data$covassvhypvm0006 - data$covassvhypvm0012)
data$covassbalvmaxvm0024 <- data$covassbalvmaxvm0012
test_pool <- catboost.load_pool(data[,-1:-2])
prob <- catboost.predict(cat,test_pool)
pred <- ifelse((prob >0),1,0);a <- table(pred, data$flag);a;2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1]);(a[1,1]+a[2,2])/sum(a)
data1.1$class <- pred

a <- rbind(data1.2.1.1.1[,c(1,185)],data1.2.1.1.2[,c(1,105)],data1.2.2.1.1[,c(1,185)],data1.2.2.1.2[,c(1,105)],data1.1[,c(1,340)])
data2$class <- rep(0,times = 274924)
c <- rbind(a[,c(1,2)],data2[,c(1,340)]);c<- c[order(c$cust_id),]
b$class3 <- c$class
data1.2.1.1.1<- data1.2.1.1.1[,c(-185)];data1.2.1.1.2 <- data1.2.1.1.2[,c(-105)];data1.2.2.1.1 <- data1.2.2.1.1[,c(-185)];data1.2.2.1.2 <- data1.2.2.1.2[,c(-105)]

#---------------------------------------------------------------------------------------------------
#预测五
data <- data1.2.2.1.1
bla1 <- data.matrix(data[3:184])
prob <- predict(lgb2.1.2,bla1)
pred <- ifelse((prob > 1.51),1,0)
data1.2.2.1.1$class <- pred

data <- data1.2.2.1.2
bla1 <- data.matrix(data[3:104])
prob <- predict(lgb2.2.1,bla1)
pred <- ifelse((prob > 1.48),1,0)
data1.2.2.1.2$class <- pred

data <- data1.2.1.1.1
data <- data[,c(-7)]
bla1 <- data.matrix(data[3:183])
prob <- predict(lgb1.1.1,bla1)
pred <- ifelse((prob > 1.48),1,0)
data1.2.1.1.1$class <- pred

data <- data1.2.1.1.2
data <- data[,c(-7)]
bla1 <- data.matrix(data[3:103])
prob <- predict(lgb1.2.1,bla1)
pred <- ifelse((prob > 1.43),1,0)
data1.2.1.1.2$class <- pred

data <- data1.1
data <- data[,c(-3,-4,-6,-7,-50:-70,-103:-158,-167:-338)]
data <- data[,c(-45)]
data$flag <- factor(data$flag)
data$settime <- factor(data$settime)
data$cfvfdpvnumvm0024 <- data$cfvfdpvnumvm0012
data$cfvdctrdamovm0002 <- (as.integer(data$cfvdctrdamovm0001) + as.integer(data$cfvdctrdamovm0003))/2
data$cfvdctrdamovm0024 <- data$cfvdctrdamovm0012
data$cfvcctrdamovm0002 <- (data$cfvcctrdamovm0001 + data$cfvcctrdamovm0003)/2
data$cfvcctrdamovm0024 <- data$cfvcctrdamovm0012
data$cfvcctrdvnumvm0002 <- (data$cfvcctrdvnumvm0001+data$cfvcctrdvnumvm0003)/2
data$cfvcctrdvnumvm0024 <- data$cfvcctrdvnumvm0012
data$covassvhypvm0024 <-data$covassvhypvm0012 - (data$covassvhypvm0006 - data$covassvhypvm0012)
data$covassbalvmaxvm0024 <- data$covassbalvmaxvm0012
test_pool <- catboost.load_pool(data[,-1:-2])
prob <- catboost.predict(cat,test_pool)
pred <- ifelse((prob >0),1,0);a <- table(pred, data$flag);a;2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1]);(a[1,1]+a[2,2])/sum(a)
data1.1$class <- pred

a <- rbind(data1.2.1.1.1[,c(1,185)],data1.2.1.1.2[,c(1,105)],data1.2.2.1.1[,c(1,185)],data1.2.2.1.2[,c(1,105)],data1.1[,c(1,340)])
data2$class <- rep(0,times = 274924)
c <- rbind(a[,c(1,2)],data2[,c(1,340)]);c<- c[order(c$cust_id),]
b$class4 <- c$class
data1.2.1.1.1<- data1.2.1.1.1[,c(-185)];data1.2.1.1.2 <- data1.2.1.1.2[,c(-105)];data1.2.2.1.1 <- data1.2.2.1.1[,c(-185)];data1.2.2.1.2 <- data1.2.2.1.2[,c(-105)]
#---------------------------------------------------------------------------------------------------
#预测六
data <- data1.2.2.1.1
data <- data[,c(-124:-155,-162:-174)]
train_matrix <- sparse.model.matrix(flag~.-1, data = data[,c(-1)])
train_label <- as.numeric(data$flag)-1
train_fin <- list(data = train_matrix, label = train_label)
dtrain <- xgb.DMatrix(data =train_fin$data, label = train_fin$label)
prob <- predict(xgb2.1.1, dtrain)
pred <- ifelse((prob > 0.221),1,0)
data1.2.2.1.1$class <- pred

data <- data1.2.2.1.2
data <- data[,c(-81:-94)]
train_matrix <- sparse.model.matrix(flag~.-1, data = data[,c(-1)])
train_label <- as.numeric(data$flag)-1
train_fin <- list(data = train_matrix, label = train_label)
dtrain <- xgb.DMatrix(data =train_fin$data, label = train_fin$label)
prob <- predict(xgb2.2.2, dtrain)
pred <- ifelse((prob > 0.191),1,0)
data1.2.2.1.2$class <- pred

data <- data1.2.1.1.1
data <- data[,c(-7)]
data <- data[,c(-123:-154,-161:-173)]
train_matrix <- sparse.model.matrix(flag~.-1, data = data[,c(-1)])
train_label <- as.numeric(data$flag)-1
train_fin <- list(data = train_matrix, label = train_label)
dtrain <- xgb.DMatrix(data =train_fin$data, label = train_fin$label)
prob <- predict(xgb1.1.2, dtrain)
pred <- ifelse((prob > 0.2),1,0)
data1.2.1.1.1$class <- pred

data <- data1.2.1.1.2
data <- data[,c(-7)]
data <- data[,c(-48:-50,-65:-68,-73:-94)]
train_matrix <- sparse.model.matrix(flag~.-1, data = data[,c(-1)])
train_label <- as.numeric(data$flag)-1
train_fin <- list(data = train_matrix, label = train_label)
dtrain <- xgb.DMatrix(data =train_fin$data, label = train_fin$label)
prob <- predict(xgb1.2.2, dtrain)
pred <- ifelse((prob > 0.11),1,0)
data1.2.1.1.2$class <- pred

data <- data1.1
data <- data[,c(-3,-4,-6,-7,-50:-70,-103:-158,-167:-338)]
data <- data[,c(-45)]
data$flag <- factor(data$flag)
data$settime <- factor(data$settime)
data$cfvfdpvnumvm0024 <- data$cfvfdpvnumvm0012
data$cfvdctrdamovm0002 <- (as.integer(data$cfvdctrdamovm0001) + as.integer(data$cfvdctrdamovm0003))/2
data$cfvdctrdamovm0024 <- data$cfvdctrdamovm0012
data$cfvcctrdamovm0002 <- (data$cfvcctrdamovm0001 + data$cfvcctrdamovm0003)/2
data$cfvcctrdamovm0024 <- data$cfvcctrdamovm0012
data$cfvcctrdvnumvm0002 <- (data$cfvcctrdvnumvm0001+data$cfvcctrdvnumvm0003)/2
data$cfvcctrdvnumvm0024 <- data$cfvcctrdvnumvm0012
data$covassvhypvm0024 <-data$covassvhypvm0012 - (data$covassvhypvm0006 - data$covassvhypvm0012)
data$covassbalvmaxvm0024 <- data$covassbalvmaxvm0012
test_pool <- catboost.load_pool(data[,-1:-2])
prob <- catboost.predict(cat,test_pool)
pred <- ifelse((prob >0),1,0);a <- table(pred, data$flag);a;2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1]);(a[1,1]+a[2,2])/sum(a)
data1.1$class <- pred

a <- rbind(data1.2.1.1.1[,c(1,185)],data1.2.1.1.2[,c(1,105)],data1.2.2.1.1[,c(1,185)],data1.2.2.1.2[,c(1,105)],data1.1[,c(1,340)])
data2$class <- rep(0,times = 274924)
c <- rbind(a[,c(1,2)],data2[,c(1,340)]);c<- c[order(c$cust_id),]
b$class5 <- c$class
data1.2.1.1.1<- data1.2.1.1.1[,c(-185)];data1.2.1.1.2 <- data1.2.1.1.2[,c(-105)];data1.2.2.1.1 <- data1.2.2.1.1[,c(-185)];data1.2.2.1.2 <- data1.2.2.1.2[,c(-105)]
#---------------------------------------------------------------------------------------------------
#预测七
data <- data1.2.2.1.1
train_matrix <- sparse.model.matrix(flag~.-1, data = data[,c(-1)])
train_label <- as.numeric(data$flag)-1
train_fin <- list(data = train_matrix, label = train_label)
dtrain <- xgb.DMatrix(data =train_fin$data, label = train_fin$label)
prob <- predict(xgb2.1.2, dtrain)
pred <- ifelse((prob > 0.202),1,0)
data1.2.2.1.1$class <- pred

data <- data1.2.2.1.2
train_matrix <- sparse.model.matrix(flag~.-1, data = data[,c(-1)])
train_label <- as.numeric(data$flag)-1
train_fin <- list(data = train_matrix, label = train_label)
dtrain <- xgb.DMatrix(data =train_fin$data, label = train_fin$label)
prob <- predict(xgb2.2.1, dtrain)
pred <- ifelse((prob > 0.22),1,0)
data1.2.2.1.2$class <- pred

data <- data1.2.1.1.1
data <- data[,c(-7)]
train_matrix <- sparse.model.matrix(flag~.-1, data = data[,c(-1)])
train_label <- as.numeric(data$flag)-1
train_fin <- list(data = train_matrix, label = train_label)
dtrain <- xgb.DMatrix(data =train_fin$data, label = train_fin$label)
prob <- predict(xgb1.1.1, dtrain)
pred <- ifelse((prob > 0.18),1,0)
data1.2.1.1.1$class <- pred

data <- data1.2.1.1.2
data <- data[,c(-7)]
train_matrix <- sparse.model.matrix(flag~.-1, data = data[,c(-1)])
train_label <- as.numeric(data$flag)-1
train_fin <- list(data = train_matrix, label = train_label)
dtrain <- xgb.DMatrix(data =train_fin$data, label = train_fin$label)
prob <- predict(xgb1.2.1, dtrain)
pred <- ifelse((prob > 0.14),1,0)
data1.2.1.1.2$class <- pred

data <- data1.1
data <- data[,c(-3,-4,-6,-7,-50:-70,-103:-158,-167:-338)]
data <- data[,c(-45)]
data$flag <- factor(data$flag)
data$settime <- factor(data$settime)
data$cfvfdpvnumvm0024 <- data$cfvfdpvnumvm0012
data$cfvdctrdamovm0002 <- (as.integer(data$cfvdctrdamovm0001) + as.integer(data$cfvdctrdamovm0003))/2
data$cfvdctrdamovm0024 <- data$cfvdctrdamovm0012
data$cfvcctrdamovm0002 <- (data$cfvcctrdamovm0001 + data$cfvcctrdamovm0003)/2
data$cfvcctrdamovm0024 <- data$cfvcctrdamovm0012
data$cfvcctrdvnumvm0002 <- (data$cfvcctrdvnumvm0001+data$cfvcctrdvnumvm0003)/2
data$cfvcctrdvnumvm0024 <- data$cfvcctrdvnumvm0012
data$covassvhypvm0024 <-data$covassvhypvm0012 - (data$covassvhypvm0006 - data$covassvhypvm0012)
data$covassbalvmaxvm0024 <- data$covassbalvmaxvm0012
test_pool <- catboost.load_pool(data[,-1:-2])
prob <- catboost.predict(cat,test_pool)
pred <- ifelse((prob >0),1,0);a <- table(pred, data$flag);a;2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1]);(a[1,1]+a[2,2])/sum(a)
data1.1$class <- pred

a <- rbind(data1.2.1.1.1[,c(1,185)],data1.2.1.1.2[,c(1,105)],data1.2.2.1.1[,c(1,185)],data1.2.2.1.2[,c(1,105)],data1.1[,c(1,340)])
data2$class <- rep(0,times = 274924)
c <- rbind(a[,c(1,2)],data2[,c(1,340)]);c<- c[order(c$cust_id),]
b$class6 <- c$class
data1.2.1.1.1<- data1.2.1.1.1[,c(-185)];data1.2.1.1.2 <- data1.2.1.1.2[,c(-105)];data1.2.2.1.1 <- data1.2.2.1.1[,c(-185)];data1.2.2.1.2 <- data1.2.2.1.2[,c(-105)]
#------------------------------------------------------------------------------------
#预测八
data <- data1.2.2.1.1
test_pool <- catboost.load_pool(data[,-1:-2])
prob <- catboost.predict(cat2.1.2,test_pool)
pred <- ifelse((prob > 0.095),1,0)
data1.2.2.1.1$class <- pred

data <- data1.2.2.1.2
test_pool <- catboost.load_pool(data[,-1:-2])
prob <- catboost.predict(cat2.2.1,test_pool)
pred <- ifelse((prob > 0),1,0)
data1.2.2.1.2$class <- pred

data <- data1.2.1.1.1
test_pool <- catboost.load_pool(data[,-1:-2])
prob <- catboost.predict(cat1.1.1,test_pool)
pred <- ifelse((prob > 0.02),1,0)
data1.2.1.1.1$class <- pred

data <- data1.2.1.1.2
test_pool <- catboost.load_pool(data[,-1:-2])
prob <- catboost.predict(cat1.2.1,test_pool)
pred <- ifelse((prob > 0.12),1,0)
data1.2.1.1.2$class <- pred

data <- data1.1
data <- data[,c(-3,-4,-6,-7,-50:-70,-103:-158,-167:-338)]
data <- data[,c(-45)]
data$flag <- factor(data$flag)
data$settime <- factor(data$settime)
data$cfvfdpvnumvm0024 <- data$cfvfdpvnumvm0012
data$cfvdctrdamovm0002 <- (as.integer(data$cfvdctrdamovm0001) + as.integer(data$cfvdctrdamovm0003))/2
data$cfvdctrdamovm0024 <- data$cfvdctrdamovm0012
data$cfvcctrdamovm0002 <- (data$cfvcctrdamovm0001 + data$cfvcctrdamovm0003)/2
data$cfvcctrdamovm0024 <- data$cfvcctrdamovm0012
data$cfvcctrdvnumvm0002 <- (data$cfvcctrdvnumvm0001+data$cfvcctrdvnumvm0003)/2
data$cfvcctrdvnumvm0024 <- data$cfvcctrdvnumvm0012
data$covassvhypvm0024 <-data$covassvhypvm0012 - (data$covassvhypvm0006 - data$covassvhypvm0012)
data$covassbalvmaxvm0024 <- data$covassbalvmaxvm0012
test_pool <- catboost.load_pool(data[,-1:-2])
prob <- catboost.predict(cat,test_pool)
pred <- ifelse((prob >0),1,0);a <- table(pred, data$flag);a;2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1]);(a[1,1]+a[2,2])/sum(a)
data1.1$class <- pred

a <- rbind(data1.2.1.1.1[,c(1,185)],data1.2.1.1.2[,c(1,105)],data1.2.2.1.1[,c(1,185)],data1.2.2.1.2[,c(1,105)],data1.1[,c(1,340)])
data2$class <- rep(0,times = 274924)
c <- rbind(a[,c(1,2)],data2[,c(1,340)]);c<- c[order(c$cust_id),]
b$class7 <- c$class
data1.2.1.1.1<- data1.2.1.1.1[,c(-185)];data1.2.1.1.2 <- data1.2.1.1.2[,c(-105)];data1.2.2.1.1 <- data1.2.2.1.1[,c(-185)];data1.2.2.1.2 <- data1.2.2.1.2[,c(-105)]
#------------------------------------------------------------------------------------
#预测九
data <- data1.2.2.1.1
data <- data[,c(-124:-155,-162:-174)]
test_pool <- catboost.load_pool(data[,-1:-2])
prob <- catboost.predict(cat2.1.1,test_pool)
pred <- ifelse((prob > 0.095),1,0)
data1.2.2.1.1$class <- pred

data <- data1.2.2.1.2
data <- data[,c(-81:-94)]
test_pool <- catboost.load_pool(data[,-1:-2])
prob <- catboost.predict(cat2.2.2,test_pool)
pred <- ifelse((prob > 0.01),1,0)
data1.2.2.1.2$class <- pred

data <- data1.2.1.1.1
data <- data[,c(-7)]
data <- data[,c(-123:-154,-161:-173)]
test_pool <- catboost.load_pool(data[,-1:-2])
prob <- catboost.predict(cat1.1.2,test_pool)
pred <- ifelse((prob > -0.1),1,0)
data1.2.1.1.1$class <- pred

data <- data1.2.1.1.2
data <- data[,c(-7)]
data <- data[,c(-48:-50,-65:-68,-73:-94)]
test_pool <- catboost.load_pool(data[,-1:-2])
prob <- catboost.predict(cat1.2.2,test_pool)
pred <- ifelse((prob > 0.11),1,0)
data1.2.1.1.2$class <- pred

data <- data1.1
data <- data[,c(-3,-4,-6,-7,-50:-70,-103:-158,-167:-338)]
data <- data[,c(-45)]
data$flag <- factor(data$flag)
data$settime <- factor(data$settime)
data$cfvfdpvnumvm0024 <- data$cfvfdpvnumvm0012
data$cfvdctrdamovm0002 <- (as.integer(data$cfvdctrdamovm0001) + as.integer(data$cfvdctrdamovm0003))/2
data$cfvdctrdamovm0024 <- data$cfvdctrdamovm0012
data$cfvcctrdamovm0002 <- (data$cfvcctrdamovm0001 + data$cfvcctrdamovm0003)/2
data$cfvcctrdamovm0024 <- data$cfvcctrdamovm0012
data$cfvcctrdvnumvm0002 <- (data$cfvcctrdvnumvm0001+data$cfvcctrdvnumvm0003)/2
data$cfvcctrdvnumvm0024 <- data$cfvcctrdvnumvm0012
data$covassvhypvm0024 <-data$covassvhypvm0012 - (data$covassvhypvm0006 - data$covassvhypvm0012)
data$covassbalvmaxvm0024 <- data$covassbalvmaxvm0012
test_pool <- catboost.load_pool(data[,-1:-2])
prob <- catboost.predict(cat,test_pool)
pred <- ifelse((prob >0),1,0);a <- table(pred, data$flag);a;2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1]);(a[1,1]+a[2,2])/sum(a)
data1.1$class <- pred

a <- rbind(data1.2.1.1.1[,c(1,185)],data1.2.1.1.2[,c(1,105)],data1.2.2.1.1[,c(1,185)],data1.2.2.1.2[,c(1,105)],data1.1[,c(1,340)])
data2$class <- rep(0,times = 274924)
c <- rbind(a[,c(1,2)],data2[,c(1,340)]);c<- c[order(c$cust_id),]
b$class8 <- c$class
data1.2.1.1.1<- data1.2.1.1.1[,c(-185)];data1.2.1.1.2 <- data1.2.1.1.2[,c(-105)];data1.2.2.1.1 <- data1.2.2.1.1[,c(-185)];data1.2.2.1.2 <- data1.2.2.1.2[,c(-105)]

#------------------------------------------------------------------------------------
#结果融合
b$class9 <- ifelse((b$class3 == 0 &b$class4 == 0&b$class5 ==0|
                      b$class3 == 0&b$class4 == 0&b$class6 ==0|
                      b$class3 == 0&b$class4 == 0&b$class7 ==0|
                      b$class3 == 0&b$class4 == 0&b$class8 ==0|
                      b$class4 == 0&b$class5 == 0&b$class6 ==0|
                      b$class4 == 0&b$class5 == 0&b$class7 ==0|
                      b$class4 == 0&b$class5 == 0&b$class8 ==0|
                      b$class5 == 0&b$class6 == 0&b$class7 ==0|
                      b$class5 == 0&b$class6 == 0&b$class8 ==0|
                      b$class6 == 0&b$class7 == 0&b$class8 ==0),0,1);summary(factor(b$class9))

b$class10 <- ifelse((b$class9 == 0& b$class == 0),0,1);summary(factor(b$class10))
#-------------------------------------------
#将b的结果写入csv文件;class列是预测的flag
#-------------------------------------------
write.csv(b[,c(1,10)],file = "文件位置")

a <- table(b$class9,pred_data$flag);a;(a[1,1]+a[2,2])/sum(a);2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1])
a <- table(b$class9,pred_data$flag_label);a;(a[1,1]+a[2,2])/sum(a);2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1])
a <- table(pred_data$classALL,pred_data$flag);a;(a[1,1]+a[2,2])/sum(a);2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1])
a <- table(pred_data$classALL,pred_data$flag_label);a;(a[1,1]+a[2,2])/sum(a);2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1])
pred_data$classALL <- ifelse((pred_data$flag == 1& pred_data$flag_label == 1|
                                b$class3 == 1& b$class4 ==1& b$class5 ==1 & 
                                b$class6 ==1& b$class7 ==1& b$class8 ==1& b$class9==1 &pred_data$flag_label ==0),1,0)
summary(factor(pred_data$classALL))
importance<- data.frame(importance(random), check.names = FALSE)
#作图展示 top30 重要的 OTUs
varImpPlot(random, n.var = min(30, nrow(random$importance)),
           main = 'Top 30 - variable importance')




a <- (86*2 + 86*4 + 85*4 + 86*4 + 89*2 + 88*2 + 91*2 + 93*3 + 90*1 + 94*2 + 76*3 + 95*2 + 87*2 + 91*2 + 92*2 + 99*2)*40/(39*100)



par(las = 1,mar = c(4,10,5,3));barplot(height = import$value, names.arg = import$feacture, horiz = TRUE)
#--------------------------------------------------------------------------
# 画ROC曲线和计算AUC值
library(pROC)
rocl <- roc(b$class6, pred_data$flag_label)
rocl
plot(rocl)
a <- table(factor(pred_data$class_random),factor(pred_data$flag_label));a;a[2,2]/(a[2,2]+a[1,2]);a[2,2]/(a[2,2]+a[2,1]);2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1])
a <- table(factor(b$class10),factor(pred_data$flag_label));a;a[2,2]/(a[2,2]+a[1,2]);a[2,2]/(a[2,2]+a[2,1]);2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1]);rocl <- roc(b$class10, pred_data$flag_label);rocl
