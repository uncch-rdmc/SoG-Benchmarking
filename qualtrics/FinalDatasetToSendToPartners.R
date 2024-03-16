# Add in packages
library(readr)
# install.packages('xlsx')
library(xlsx)

data <- read_csv("data.csv", col_types = cols (.default = "n", Finished = "c", 
                                               a_year = "n", a_municipality = "c", a_service = "c"))
Pop <- read_csv("Populations.csv", col_types = cols (.default = "n", Municipality = "c"))

# I don't think we're actually using this...) 
# VarGen <- read.csv("VariableGeneration.csv")

MetricNames <- read.csv("MetricNames.csv")

# Reading in just the first row for labels
DataLabels <- read.csv(file="data.csv", nrows = 1)

# Delete some unnecessary clutter
data <- data[-1:-2,]

# Remove unnecessary questions
unnecessary <- names(data[,grepl("approval", names(data))])
temp <- names(data[,grepl("comments", names(data))])
unnecessary <- append(unnecessary, temp)
temp <- names(data[,grepl("app", names(data))])
unnecessary <- append(unnecessary, temp)
temp <- names(data[,grepl("comm", names(data))])
unnecessary <- append(unnecessary, temp)
temp <- names(data[,c(1:19, 23, 567:578)])
unnecessary <- append(unnecessary, temp)

# New dataset with only necessary questions
necessary <- (data[,!names(data) %in% unnecessary])
necessary[necessary=="Yard Waste/Leaf Collection"] <- "Yard Waste and Leaf Collection"
data <- necessary

# Load in the populations
data$Population <- Pop$Population[match(data$a_municipality, Pop$Municipality)]

# Fix qbi05 issue (needs to be filled with 0 to account for those who don't have other)
data$qbi05[is.na(data$qbi05)] <- 0

#save for testing
safe <- data

# Manual generations for all the variables that don't fit the mold
data$qam12_1 <- data$qam12 - ((data$qam13)/260) - ((data$qam14)/2080)
data$qam17_1<- (data$qam16) + (data$qam17)
data$qbi05_1 <- data$qbi01 + data$qbi02 + data$qbi03 + data$qbi04 + data$qbi05
data$qbi14_1 <- data$qbi11 + data$qbi12 + data$qbi13 + data$qbi14
data$qbi19_1 <- data$qbi11 + data$qbi12 + data$qbi13 + data$qbi14 + data$qbi15 + data$qbi16 + data$qbi17 + data$qbi18 + data$qbi19
data$qbi39_2 <- data$qbi38 + data$qbi39
data$qbi46_1 <- data$qbi46 - ((data$qbi47)/260) - ((data$qbi48)/2080)
data$qbi52_1<- (data$qbi51) + (data$qbi52)
data$qfm68_2 <- data$qfm68 - ((data$qfm69)/260) - ((data$qfm70)/2080)
data$qfm72_1 <- (data$qfm71) + (data$qfm72)
data$qhr16_2 <- data$qhr16 - ((data$qhr17)/260) - ((data$qhr18)/2080)
data$qhr19_1 <- data$qhr19 - ((data$qhr20)/260) - ((data$qhr21)/2080)
data$qhr24_1<- (data$qhr23) + (data$qhr24)
data$qec28_2<- data$qec28 - ((data$qec29)/260) - ((data$qec30)/2080)
data$qec33_1 <- (data$qec32) + (data$qec33)
data$qfs12_2<- data$qfs12 - ((data$qfs13)/260) - ((data$qfs14)/2080)
data$qfs37_1 <- (data$qfs36) + (data$qfs37)
data$qpr09_1 <- (data$qpr08) + (data$qpr09)
data$qpr23_1 <- data$qpr23- ((data$qpr24)/260) - ((data$qpr25)/2080)
data$qpr27_1 <- (data$qpr26) + (data$qpr27)
data$qps04_2 <- data$qps01 + data$qps02 + data$qps03 + data$qps04
data$qps08_2 <- data$qps05 + data$qps06 + data$qps07 + data$qps08
data$qps10_1 <- (data$qps09) + (data$qps10)
data$qps27_1 <- (data$qps26) + (data$qps27)
data$qps22_1 <- data$qps22 - ((data$qps23)/260) - ((data$qps24)/2080)
data$qre10_1 <- data$qre10 - ((data$qre11)/260) - ((data$qre12)/2080)
data$qre14_1 <- (data$qre13) + (data$qre14)
data$qrr10_1 <- data$qrr10- ((data$qrr11)/260) - ((data$qrr12)/2080)
data$qrr14_1 <- (data$qrr13) + (data$qrr14)
data$qww31_1<- data$qww31 - ((data$qww32)/260) - ((data$qww33)/2080)
data$qww22_1<- (data$qww21) + (data$qww22)
data$qww35_1<- (data$qww34) + (data$qww35)
data$qws37_1 <- data$qws37- ((data$qws38)/260) - ((data$qws39)/2080)
data$qws09_1 <- (data$qws08) + (data$qws09)
data$qws41_1 <- (data$qws40) + (data$qws41)
data$qyw09_1 <- data$qyw09 - ((data$qyw10)/260) - ((data$qyw11)/2080)
data$qyw13_1 <- (data$qyw12) + (data$qyw13)

# Main ones
data$qam01_1 <- (data$qam01/data$Population)*1000
data$qam02_1 <- (data$qam02/data$Population)*1000
data$qam02_2 <- (data$qam02/data$qam12)*1
data$qam05_1 <- (data$qam05/data$qam02)*1
data$qam06_1 <- (data$qam06/data$qam05)*100
data$qam07_1 <- (data$qam07/data$qam05)*100
data$qam08_1 <- (data$qam08/data$qam02)*1
data$qam12_2 <- (data$Population/data$qam12)*1
data$qam12_3 <- (data$Population/data$qam12_1)*1
data$qam14_1 <- (data$qam14/data$qam12_1)*1
data$qam13_1 <- (data$qam13/data$qam12)*1
data$qam17_2 <- (data$qam16/data$qam17_1)*100
data$qam17_3 <- (data$qam17_1/data$qam02)*1
data$qam17_4 <- (data$qam17_1/data$Population)*1
data$qbi01_1 <- (data$qbi01/data$qbi05_1)*100
data$qbi02_1 <- (data$qbi02/data$qbi05_1)*100
data$qbi03_1 <- (data$qbi03/data$qbi05_1)*100
data$qbi04_1 <- (data$qbi04/data$qbi05_1)*100
data$qbi05_2 <- (data$qbi05/data$qbi05_1)*100
data$qbi05_3 <- (data$qbi05_1/data$Population)*1
data$qbi06_1 <- (data$qbi06/data$qbi05_1)*100
data$qbi07_1 <- (data$qbi07/data$qbi05_1)*100
data$qbi08_1 <- (data$qbi08/data$qbi05_1)*100
data$qbi10_1 <- (data$qbi10/data$qbi05_1)*100
data$qbi11_1 <- (data$qbi11/data$Population)*1000
data$qbi12_1 <- (data$qbi12/data$Population)*1000
data$qbi13_1 <- (data$qbi13/data$Population)*1000
data$qbi14_1 <- data$qbi11 + data$qbi12 + data$qbi13 + data$qbi14
data$qbi14_2 <- (data$qbi14/data$Population)*1000
data$qbi15_1 <- (data$qbi15/data$Population)*1000
data$qbi16_1 <- (data$qbi16/data$Population)*1000
data$qbi17_1 <- (data$qbi17/data$Population)*1000
data$qbi18_1 <- (data$qbi18/data$Population)*1000
data$qbi19_1 <- data$qbi11 + data$qbi12 + data$qbi13 + data$qbi14 + data$qbi15 + data$qbi16 + data$qbi17 + data$qbi18 + data$qbi19
data$qbi19_2 <- (data$qbi14_1/data$qbi19_1)*100
data$qbi19_3 <- (data$qbi19/data$Population)*1000
data$qbi20_1 <- (data$qbi20/data$qbi05_1)*100
data$qbi21_1 <- (data$qbi21/data$qbi05_1)*100
data$qbi22_1 <- (data$qbi22/data$qbi05_1)*100
data$qbi23_1 <- (data$qbi23/data$qbi05_1)*100
data$qbi24_1 <- (data$qbi24/data$qbi05_1)*100
data$qbi25_1 <- (data$qbi25/data$qbi05_1)*100
data$qbi26_1 <- (data$qbi26/data$qbi05_1)*100
data$qbi27_1 <- (data$qbi27/data$qbi05_1)*100
data$qbi28_1 <- (data$qbi28/data$qbi05_1)*100
data$qbi29_1 <- (data$qbi29/data$qbi19_1)*100
data$qbi30_1 <- (data$qbi30/data$qbi19_1)*100
data$qbi31_1 <- (data$qbi31/data$qbi19_1)*100
data$qbi32_1 <- (data$qbi32/data$qbi19_1)*100
data$qbi33_1 <- (data$qbi33/data$qbi19_1)*100
data$qbi38_1 <- (data$qbi38/data$Population)*1000
data$qbi39_1 <- (data$qbi39/data$Population)*1000
data$qbi39_3 <- (data$qbi38/data$qbi39_2)*100
data$qbi42_1 <- (data$qbi42/data$qbi46)*100
data$qbi43_1 <- (data$qbi43/data$qbi46)*100
data$qbi44_1 <- (data$qbi44/data$qbi46)*100
data$qbi45_1 <- (data$qbi45/data$qbi46)*1
data$qbi46_2 <- (data$Population/data$qbi46)*1
data$qbi46_3 <- (data$Population/data$qbi46_1)*1
data$qbi46_4 <- (data$qbi05_1/data$qbi46)*1
data$qbi46_5 <- (data$qbi05_1/data$qbi46_1)*1
data$qbi47_1 <- (data$qbi47/data$qbi46)*1
data$qbi48_1 <- (data$qbi48/data$qbi46_1)*1
data$qbi50_1 <- (data$qbi50/data$qbi46)*1
data$qbi51_1 <- (data$qbi51/data$qbi52_1)*100
data$qhr04_1 <- (data$qhr04/data$qhr16)*100
data$qhr05_1 <- (data$qhr05/data$qhr16)*100
data$qhr06_1 <- (data$qhr06/data$qhr16)*100
data$qhr07_1 <- (data$qhr07/data$qhr04)*100
data$qhr08_1 <- (data$qhr08/data$qhr05)*100
data$qhr09_1 <- (data$qhr09/data$qhr16)*100
data$qhr10_1 <- (data$qhr10/data$qhr16)*100
data$qhr11_1 <- (data$qhr11/data$qhr16)*100
data$qhr12_1 <- (data$qhr12/data$qhr16)*100
data$qhr13_1 <- (data$qhr13/data$qhr16)*100
data$qhr14_1 <- (data$qhr14/data$qhr11)*100
data$qhr15_1 <- (data$qhr15/data$qhr16)*100
data$qhr16_1 <- (data$qhr16/data$qhr19)*1
data$qhr16_3 <- (data$Population/data$qhr16)*1
data$qhr16_4 <- (data$Population/data$qhr16_2)*1
data$qhr17_1 <- (data$qhr17/data$qhr16)*1
data$qhr18_1 <- (data$qhr18/data$qhr16_2)*1
data$qhr20_1 <- (data$qhr20/data$qhr19)*1
data$qhr21_2 <- (data$qhr21/data$qhr19_1)*1
data$qhr22_1 <- (data$qhr22/data$qhr16)*1
data$qhr23_1 <- (data$qhr23/data$qhr24_1)*100
data$qhr24_2 <- (data$qhr24_1/data$qhr16)*1
data$qhr24_3 <- (data$qhr24_1/data$Population)*1
data$qec01_1 <- (data$qec01/data$Population)*1
data$qec01_2 <- (data$qec01/data$qec28)*1
data$qec02_1 <- (data$qec02/data$qec01)*1
data$qec03_1 <- (data$qec03/data$qec01)*1
data$qec04_1 <- (data$qec04/data$qec01)*1
data$qec05_1 <- (data$qec05/data$qec01)*1
data$qec13_1 <- (data$qec13/data$Population)*1
data$qec14_1 <- (data$qec14/data$Population)*1
data$qec15_1 <- (data$qec15/data$Population)*1
data$qec16_1 <- (data$qec16/data$Population)*1
data$qec21_1 <- (data$qec21/data$Population)*1000
data$qec22_1 <- (data$qec22/data$qec28)*1
data$qec23_1 <- (data$qec23/data$qec28)*1
data$qec24_1 <- (data$qec24/data$qec28)*1
data$qec26_1 <- (data$qec26/data$qec28)*1
data$qec27_1 <- (data$qec27/data$qec28)*1
data$qec28_1 <- (data$Population/data$qec28)*1
data$qec28_3 <- (data$Population/data$qec28_2)*1
data$qec28_4 <- (data$qec01/data$qec28_2)*1
data$qec29_1 <- (data$qec29/data$qec28)*1
data$qec30_1 <- (data$qec30/data$qec28_2)*1
data$qec31_1 <- (data$qec31/data$qec33_1)*100
data$qec32_1 <- (data$qec32/data$qec33_1)*100
data$qec33_2 <- (data$qec33_1/data$Population)*1
data$qec33_3 <- (data$qec33_1/data$qec01)*1
data$qec28_3 <- (data$qec01/data$qec28_2)*1
data$qfs01_1 <- (data$qfs01/data$Population)*1
data$qfs01_2 <- (data$qfs01/data$qfs12)*1
data$qfs02_1 <- (data$qfs02/data$Population)*1000
data$qfs02_2 <- (data$qfs02/data$qfs01)*100
data$qfs03_1 <- (data$qfs03/data$Population)*1000
data$qfs04_1 <- (data$qfs04/data$Population)*1000
data$qfs05_1 <- (data$qfs05/data$qfs02)*100
data$qfs12_1 <- (data$Population/data$qfs12)*1
data$qfs12_3 <- (data$Population/data$qfs12_2)*1
data$qfs13_1 <- (data$qfs13/data$qfs12)*1
data$qfs14_1 <- (data$qfs14/data$qfs12_2)*1
data$qfs15_1 <- (data$qfs15/data$qfs02)*1
data$qfs16_1 <- (data$qfs16/data$qfs02)*1
data$qfs17_1 <- (data$qfs17/data$qfs02)*1
data$qfs18_1 <- (data$qfs18/data$qfs02)*1
data$qfs21_1 <- (data$qfs21/data$Population)*1
data$qfs22_1 <- (data$qfs22/data$qfs21)*100
data$qfs23_1 <- (data$qfs23/data$Population)*1000
data$qfs24_1 <- (data$qfs24/data$qfs23)*100
data$qfs25_1 <- (data$qfs25/data$Population)*1000
data$qfs26_1 <- (data$qfs26/data$qfs25)*100
data$qfs27_1 <- (data$qfs27/data$Population)*1000
data$qfs28_1 <- (data$qfs28/data$qfs27)*100
data$qfs29_1 <- (data$qfs29/data$Population)*1
data$qfs32_1 <- (data$qfs32/data$qfs37_1)*100
data$qfs33_1 <- (data$qfs33/data$Population)*1000
data$qfs34_1 <- (data$qfs34/data$qfs33)*100
data$qfs35_1 <- (data$qfs35/data$qfs12)*1
data$qfs36_1 <- (data$qfs36/data$qfs37_1)*100
data$qfs37_2 <- (data$qfs37_1/data$Population)*1
data$qfs37_3 <- (data$qfs37_1/data$qfs01)*1
data$qfm01_1 <- (data$Population/data$qfm01)*1
data$qfm02_1 <- (data$Population/data$qfm02)*1
data$qfm03_1 <- (data$Population/data$qfm03)*1
data$qfm04_1 <- (data$Population/data$qfm04)*1
data$qfm05_1 <- (data$Population/data$qfm05)*1
data$qfm06_1 <- (data$Population/data$qfm06)*1
data$qfm07_1 <- (data$Population/data$qfm07)*1
data$qfm08_1 <- (data$Population/data$qfm08)*1
data$qfm09_1 <- (data$Population/data$qfm09)*1
data$qfm10_1 <- (data$Population/data$qfm10)*1
data$qfm11_1 <- (data$qfm11/data$qfm01)*1
data$qfm12_1 <- (data$qfm12/data$qfm02)*1
data$qfm13_1 <- (data$qfm13/data$qfm03)*1
data$qfm14_1 <- (data$qfm14/data$qfm04)*1
data$qfm15_1 <- (data$qfm15/data$qfm05)*1
data$qfm16_1 <- (data$qfm16/data$qfm06)*1
data$qfm17_1 <- (data$qfm17/data$qfm07)*1
data$qfm18_1 <- (data$qfm18/data$qfm08)*1
data$qfm19_1 <- (data$qfm19/data$qfm09)*1
data$qfm20_1 <- (data$qfm20/data$qfm10)*1
data$qfm11_2 <- (data$qfm11_1/data$qfm13_1)*1
data$qfm12_2 <- (data$qfm12_1/data$qfm13_1)*1
data$qfm13_2 <- (data$qfm13_1/data$qfm13_1)*1
data$qfm14_2 <- (data$qfm14_1/data$qfm13_1)*1
data$qfm15_2 <- (data$qfm15_1/data$qfm13_1)*1
data$qfm16_2 <- (data$qfm16_1/data$qfm13_1)*1
data$qfm17_2 <- (data$qfm17_1/data$qfm13_1)*1
data$qfm18_2 <- (data$qfm18_1/data$qfm13_1)*1
data$qfm19_2 <- (data$qfm19_1/data$qfm13_1)*1
data$qfm20_2 <- (data$qfm20_1/data$qfm13_1)*1
data$qfm31_1 <- (data$qfm31/data$qfm01)*1
data$qfm32_1 <- (data$qfm32/data$qfm02)*1
data$qfm33_1 <- (data$qfm33/data$qfm03)*1
data$qfm34_1 <- (data$qfm34/data$qfm04)*1
data$qfm35_1 <- (data$qfm35/data$qfm05)*1
data$qfm36_1 <- (data$qfm36/data$qfm06)*1
data$qfm37_1 <- (data$qfm37/data$qfm07)*1
data$qfm38_1 <- (data$qfm38/data$qfm08)*1
data$qfm39_1 <- (data$qfm39/data$qfm09)*1
data$qfm40_1 <- (data$qfm40/data$qfm10)*1
data$qfm41_1 <- (data$qfm41/data$qfm02)*1
data$qfm42_1 <- (data$qfm42/data$qfm03)*1
data$qfm43_1 <- (data$qfm43/data$qfm04)*1
data$qfm44_1 <- (data$qfm44/data$qfm05)*1
data$qfm45_1 <- (data$qfm45/data$qfm06)*1
data$qfm46_1 <- (data$qfm46/data$qfm07)*1
data$qfm47_1 <- (data$qfm47/data$qfm08)*1
data$qfm48_1 <- (data$qfm48/data$qfm09)*1
data$qfm49_1 <- (data$qfm49/data$qfm10)*1
data$qfm50_1 <- (data$qfm50/data$qfm01)*1
data$qfm50_2 <- (data$qfm50_1/365)*1
data$qfm51_1 <- (data$qfm51/data$qfm02)*1
data$qfm51_2 <- (data$qfm51_1/365)*1
data$qfm52_1 <- (data$qfm52/data$qfm03)*1
data$qfm52_2 <- (data$qfm52_1/365)*1
data$qfm53_1 <- (data$qfm53/data$qfm04)*1
data$qfm53_2 <- (data$qfm53_1/365)*1
data$qfm54_1 <- (data$qfm54/data$qfm05)*1
data$qfm54_2 <- (data$qfm54_1/365)*1
data$qfm55_1 <- (data$qfm55/data$qfm06)*1
data$qfm55_2 <- (data$qfm55_1/365)*1
data$qfm56_1 <- (data$qfm56/data$qfm07)*1
data$qfm56_2 <- (data$qfm56_1/365)*1
data$qfm57_1 <- (data$qfm57/data$qfm08)*1
data$qfm57_2 <- (data$qfm57_1/365)*1
data$qfm58_1 <- (data$qfm58/data$qfm09)*1
data$qfm58_2 <- (data$qfm58_1/365)*1
data$qfm59_1 <- (data$qfm59/data$qfm10)*1
data$qfm59_2 <- (data$qfm59_1/365)*1
data$qfm60_1 <- (data$qfm60/data$Population)*1
data$qfm61_1 <- (data$qfm61/data$qfm60)*100
data$qfm62_1 <- (data$qfm62/data$Population)*1
data$qfm63_1 <- (data$qfm63/data$qfm62)*100
data$qfm64_1 <- (data$qfm64/data$qfm62)*100
data$qfm65_1 <- (data$qfm65/data$qfm62)*100
data$qfm66_1 <- (data$qfm66/data$Population)*1000
data$qfm67_1 <- (data$qfm67/data$qfm66)*100
data$qfm68_1 <- (data$Population/data$qfm68)*1
data$qfm68_3 <- (data$Population/data$qfm68_2)*1
data$qfm69_1 <- (data$qfm69/data$qfm68)*1
data$qfm70_1 <- (data$qfm70/data$qfm68_2)*1
data$qfm71_1 <- (data$qfm71/data$qfm72_1)*100
data$qfm72_2 <- (data$qfm72_1/data$Population)*1
data$qfm74_1 <- 10*(data$qfm50_2 + data$qfm51_2 + data$qfm52_2 + data$qfm53_2 + data$qfm54_2 + data$qfm55_2 + data$qfm56_2 + data$qfm57_2 + data$qfm58_2 + data$qfm59_2)
data$qpr06_1 <- (data$qpr06/data$qpr23)*1
data$qpr07_1 <- (data$qpr07/data$qpr09_1)*1
data$qpr08_1 <- (data$qpr08/data$qpr09_1)*1
data$qpr09_2 <- (data$qpr09_1/data$Population)*1
data$qpr10_1 <- (data$qpr10/data$qpr08)*100
data$qpr11_1 <- (data$qpr11/data$qpr09)*100
data$qpr12_1 <- (data$qpr12/data$Population)*1000
data$qpr13_1 <- (data$qpr13/data$Population)*1000
data$qpr14_1 <- (data$qpr14/data$Population)*1000
data$qpr15_1 <- (data$qpr15/data$Population)*1000
data$qpr16_1 <- (data$qpr16/data$Population)*1000
data$qpr17_1 <- (data$qpr17/data$Population)*1000
data$qpr18_1 <- (data$qpr18/data$Population)*1000
data$qpr19_1 <- (data$qpr19/data$Population)*1000
data$qpr20_1 <- (data$qpr20/data$Population)*1000
data$qpr21_1 <- (data$qpr21/data$Population)*1000
data$qpr22_1 <- (data$qpr22/data$qpr27_1)*100
data$qpr23_2 <- (data$Population/data$qpr23)*1
data$qpr23_3 <- (data$Population/data$qpr23_1)*1
data$qpr24_1 <- (data$qpr24/data$qpr23)*1
data$qpr25_1 <- (data$qpr25/data$qpr23_1)*1
data$qpr26_1 <- (data$qpr26/data$qpr27_1)*100
data$qpr27_2 <- (data$qpr27_1/data$Population)*1
data$qps01_1 <- (data$qps01/data$Population)*10000
data$qps02_1 <- (data$qps02/data$Population)*10000
data$qps03_1 <- (data$qps03/data$Population)*10000
data$qps04_1 <- (data$qps04/data$Population)*10000
data$qps05_1 <- (data$qps05/data$Population)*10000
data$qps06_1 <- (data$qps06/data$Population)*10000
data$qps07_1 <- (data$qps07/data$Population)*10000
data$qps08_1 <- (data$qps08/data$Population)*10000
data$qps09_1 <- (data$qps09/data$Population)*1
data$qps10_2 <- (data$qps10/data$qps10_1)*100
data$qps10_3 <- (data$qps10_1/data$Population)*1
data$qps11_1 <- (data$qps11/data$qps10_1)*100
data$qps13_1 <- (data$qps13/data$qps11)*100
data$qps14_1 <- (data$qps14/data$Population)*1000
data$qps15_1 <- (data$qps15/data$Population)*1000
data$qps16_1 <- (data$qps16/data$Population)*1000
data$qps17_1 <- (data$qps17/data$Population)*1000
data$qps18_1 <- (data$qps18/data$Population)*1000
data$qps19_1 <- (data$qps19/data$qps22_1)*1
data$qps20_1 <- (data$qps20/data$qps22_1)*1
data$qps21_1 <- (data$qps21/data$qps22_1)*1
data$qps22_2 <- (data$Population/data$qps22)*1
data$qps22_3 <- (data$Population/data$qps22_1)*1
data$qps10_3 <- (data$qps10_1/data$qps22)*1
data$qps10_4 <- (data$qps10_1/data$qps22)*1
data$qps10_5 <- (data$qps10_1/data$qps22_1)*1
data$qps11_1 <- (data$qps11/data$qps22)*1
data$qps11_2 <- (data$qps11/data$qps22)*1
data$qps11_3 <- (data$qps11/data$qps22_1)*1
data$qps08_3 <- (data$qps08_2/data$qps22)*1
data$qps08_4 <- (data$qps08_2/data$qps22_1)*1
data$qps23_1 <- (data$qps23/data$qps22)*1
data$qps24_1 <- (data$qps24/data$qps22_1)*1
data$qps25_1 <- (data$qps25/data$qps22)*1
data$qps26_1 <- (data$qps26/data$qps27_1)*100
data$qps27_2 <- (data$qps27_1/data$Population)*1
data$qre01_1 <- (data$qre01/data$Population)*1
data$qre02_1 <- (data$qre02/data$Population)*1
data$qre05_1 <- (data$qre05/data$qre01)*1
data$qre06_1 <- (data$qre06/data$qre02)*1
data$qre07_1 <- (data$qre07/data$qre02)*1
data$qre08_1 <- (data$qre08/data$qre07)*100
data$qre09_1 <- (data$qre09/data$qre10_1)*1
data$qre09_2 <- (data$qre09/data$qre01)*1
data$qre10_2 <- (data$Population/data$qre10)*1
data$qre10_3 <- (data$Population/data$qre10_1)*1
data$qre01_2 <- (data$qre01/data$qre10)*1
data$qre01_3 <- (data$qre01/data$qre10_1)*1
data$qre11_1 <- (data$qre11/data$qre10)*1
data$qre12_1 <- (data$qre12/data$qre10_1)*1
data$qre13_1 <- (data$qre13/data$qre14_1)*100
data$qre14_2 <- (data$qre14_1/data$Population)*1
data$qre14_3 <- (data$qre14_1/data$qre01)*1
data$qrr01_1 <- (data$qrr01/data$Population)*1
data$qrr02_1 <- (data$qrr02/data$Population)*1
data$qrr04_1 <- (data$qrr04/data$qrr01)*1
data$qrr05_1 <- (data$qrr05/data$qrr02)*1
data$qrr07_1 <- (data$qrr07/data$qrr02)*1
data$qrr08_1 <- (data$qrr08/data$qrr07)*100
data$qrr09_1 <- (data$qrr09/data$qrr10_1)*1
data$qrr09_2 <- (data$qrr09/data$qrr01)*1
data$qrr10_2 <- (data$Population/data$qrr10)*1
data$qrr10_3 <- (data$Population/data$qrr10_1)*1
data$qrr11_1 <- (data$qrr11/data$qrr10)*1
data$qrr12_1 <- (data$qrr12/data$qrr10_1)*1
data$qrr01_2 <- (data$qrr01/data$qrr10)*1
data$qrr01_3 <- (data$qrr01/data$qrr10_1)*1
data$qrr13_1 <- (data$qrr13/data$qrr14_1)*100
data$qrr14_2 <- (data$qrr14_1/data$Population)*1
data$qrr14_3 <- (data$qrr14_1/data$qrr01)*1
data$qww01_1 <- (data$qww01/data$Population)*1000
data$qww02_1 <- (data$qww02/data$qww01)*100
data$qww03_1 <- (data$qww03/data$qww02)*100
data$qww04_1 <- (data$qww04/data$qww02)*100
data$qww05_1 <- (data$qww01/data$qww05)*1
data$qww06_1 <- (data$qww01/data$qww06)*1
data$qww07_1 <- (data$qww07/data$Population)*1
data$qww09_1 <- (data$qww09/data$qww02)*1
data$qww10_1 <- (data$qww10/data$qww02)*100
data$qww11_1 <- (data$qww11/data$qww02)*100
data$qww14_1 <- (data$qww14/data$qww02)*1
data$qww15_1 <- (data$qww15/data$qww02)*1
data$qww16_1 <- (data$qww16/data$qww02)*1
data$qww18_1 <- (data$qww18/data$qww02)*1
data$qww20_1 <- (data$qww20/data$qww07)*1
data$qww21_1 <- (data$qww21/data$qww22_1)*1
data$qww23_1 <- (data$qww23/data$qww02)*1
data$qww24_1 <- (data$qww24/data$qww23)*100
data$qww27_1 <- (data$qww27/data$qww02)*1
data$qww28_1 <- (data$qww28/data$qww02)*1
data$qww29_1 <- (data$qww29/data$qww02)*1
data$qww30_1 <- (data$qww30/data$qww02)*1
data$qww31_2 <- (data$Population/data$qww31)*1
data$qww31_3 <- (data$Population/data$qww31_1)*1
data$qww02_2 <- (data$qww02/data$qww31)*1
data$qww02_3 <- (data$qww02/data$qww31_1)*1
data$qww32_1 <- (data$qww32/data$qww31)*1
data$qww33_1 <- (data$qww33/data$qww31_1)*1
data$qww34_1 <- (data$qww34/data$qww35_1)*1
data$qww35_2 <- (data$qww35_1/data$Population)*1
data$qww35_3 <- (data$qww35_1/data$qww02)*1
data$qws01_1 <- (data$qws01/data$qws08)*100
data$qws02_1 <- (data$qws02/data$qws08)*100
data$qws03_1 <- (data$qws03/data$qws08)*100
data$qws04_1 <- (data$qws04/data$qws08)*100
data$qws05_1 <- (data$qws05/data$qws08)*100
data$qws06_1 <- (data$qws06/data$qws08)*100
data$qws07_1 <- (data$qws07/data$qws08)*100
data$qws09_2 <- (data$qws09/data$qws09_1)*100
data$qws09_3 <- (data$qws09_1/data$Population)*1000
data$qws10_1 <- (data$qws10/data$qws09_1)*100
data$qws11_1 <- (data$qws11/data$qws09_1)*100
data$qws12_1 <- (data$qws12/data$qws09_1)*100
data$qws13_1 <- (data$qws13/data$qws09_1)*100
data$qws08_1 <- (data$qws08/data$qws14)*100
data$qws15_1 <- (data$qws15/data$qws08)*1
data$qws16_1 <- (data$qws16/data$qws08)*1
data$qws17_1 <- (data$qws17/data$qws08)*1
data$qws18_1 <- (data$qws08/data$qws18)*1
data$qws19_1 <- (data$qws08/data$qws19)*1
data$qws20_1 <- (data$qws08/data$qws20)*1
data$qws21_1 <- (data$qws21/data$qws22)*1
data$qws35_1 <- (data$qws35/data$qws09_1)*1
data$qws36_1 <- (data$qws36/data$qws09_1)*1
data$qws37_2 <- (data$Population/data$qws37)*1
data$qws37_3 <- (data$Population/data$qws37_1)*1
data$qws09_4 <- (data$qws09_1/data$qws37)*1
data$qws09_5 <- (data$qws09_1/data$qws37_1)*1
data$qws38_1 <- (data$qws38/data$qws37)*1
data$qws39_1 <- (data$qws39/data$qws37_1)*1
data$qws40_1 <- (data$qws40/data$qws41_1)*100
data$qws41_2 <- (data$qws41_1/data$Population)*1
data$qws41_3 <- (data$qws41_1/data$qws09_1)*1
data$qyw01_1 <- (data$qyw01/data$Population)*1
data$qyw02_1 <- (data$qyw02/data$Population)*1
data$qyw04_1 <- (data$qyw04/data$Population)*1
data$qyw05_1 <- (data$qyw05/data$qyw02)*1
data$qyw06_1 <- (data$qyw06/data$qyw02)*1
data$qyw07_1 <- (data$qyw07/data$qyw06)*1
data$qyw08_1 <- (data$qyw08/data$qyw09_1)*1
data$qyw08_2 <- (data$qyw08/data$qyw01)*1
data$qyw09_2 <- (data$Population/data$qyw09)*1
data$qyw09_3 <- (data$Population/data$qyw09_1)*1
data$qyw10_1 <- (data$qyw10/data$qyw09)*1
data$qyw11_1 <- (data$qyw11/data$qyw09_1)*1
data$qyw01_2 <- (data$qyw01/data$qyw09)*1
data$qyw01_3 <- (data$qyw01/data$qyw09_1)*1
data$qyw12_1 <- (data$qyw12/data$qyw13_1)*100
data$qyw13_2 <- (data$qyw13_1/data$Population)*1
data$qyw13_3 <- (data$qyw13_1/data$qyw01)*1

# Final touches
data$qbi17_3 <- (data$qbi52_1/data$qbi05_1)*1
data$qbi17_4 <- (data$qbi52_1/data$Population)*1
data$qfm73_1 <- data$qfm11_2 + data$qfm12_2 + data$qfm13_2 + data$qfm14_2 + data$qfm15_2 + data$qfm16_2 + data$qfm17_2 + data$qfm18_2 + data$qfm19_2 +data$qfm20_2
data$qfm72_3 <- (data$qfm72_1/data$qfm73_1)*1
data$qfm73_2 <- (data$qfm73_1/data$qfm68)*1
data$qfm73_3 <- (data$qfm73_1/data$qfm68_2)*1

# Fill certain empty metrics with 0s
data$qre13[is.na(data$qre13)] <- 0
data$qrr13[is.na(data$qrr13)] <- 0
data$qyw12[is.na(data$qyw12)] <- 0
data$qre10[is.na(data$qre10)] <- 0
data$qrr10[is.na(data$qrr10)] <- 0
data$qyw09[is.na(data$qyw09)] <- 0

data <- rbind(names(data), data)

# These metrics are being worked on and need to indefinitely be removed from the dataset
removelist <- c("qam13", "qam14", "qam12_1", "qbi47", "qbi48", "qbi46_1", "qhr17", "qhr18",
                "qhr20", "qhr21", "qhr16_2", "qhr19_1", "qec29", "qec30", "qec28_2", "qfs13", 
                "qfs14", "qfs12_2", "qfm69", "qfm70", "qfm68_2", "qpr24", "qpr25", "qpr23_1", 
                "qps23", "qps24", "qps22_1", "qre11", "qre12", "qre10_1", "qrr11", "qrr12", 
                "qrr10_1", "qww32", "qww33", "qww31_1", "qws38", "qws39", "qws37_1", "qyw10", 
                "qyw11", "qyw09_1", "qam12_3", "qam13_1", "qam14_1", "qbi46_3", "qbi46_5", 
                "qbi47_1", "qbi48_1", "qhr16_4", "qhr17_1", "qhr18_1", "qhr19_1", "qhr20_1",
                "qhr21_2", "qec28_3", "qec28_4", "qec29_1", "qec30_1", "qfs12_3", "qfs13_1", 
                "qfs14_1", "qfm68_3", "qfm69_1", "qfm70_1", "qfm73_3", "qpr23_3", "qpr24_1",
                "qpr25_1", "qps08_4", "qps10_5", "qps11_3", "qps19_1", "qps21_1", "qps22_1",
                "qps22_3", "qps23_1", "qps24_1", "qre01_3", "qre09_1", "qre10_1", "qre10_3", 
                "qre11_1", "qre12_1", "qrr01_3", "qrr09_1", "qrr10_1", "qrr10_3", "qrr11_1", 
                "qrr12_1", "qww02_3", "qww31_1", "qww31_3", "qww32_1", "qww33_1", "qws09_5", 
                "qws37_3", "qws38_1", "qws39_1", "qyw01_3", "qyw08_1", "qyw09_1", "qyw09_3", 
                "qyw10_1", "qyw11_1")

# Needs to be tweaked
data <- data[,!(names(data) %in% removelist)]

# Separate out the questions
qrr <- as.character(data[1,grepl("qrr", names(data))])
qre <- as.character(data[1,grepl("qre", names(data))])
qyw <- as.character(data[1,grepl("qyw", names(data))])
qps <- as.character(data[1,grepl("qps", names(data))])
qec <- as.character(data[1,grepl("qec", names(data))])
qam <- as.character(data[1,grepl("qam", names(data))])
qfs <- as.character(data[1,grepl("qfs", names(data))])
qbi <- as.character(data[1,grepl("qbi", names(data))])
qfm <- as.character(data[1,grepl("qfm", names(data))])
qhr <- as.character(data[1,grepl("qhr", names(data))])
qws <- as.character(data[1,grepl("qws", names(data))])
qww <- as.character(data[1,grepl("qww", names(data))])
qpr <- as.character(data[1,grepl("qpr", names(data))])
qbf <- as.character(data[1,grepl("qbf", names(data))])

# Replace with NAs
data <- data[-1,]
data[, c(4:length(data))] <- sapply(data[, c(4:length(data))], as.numeric)

# Find aggregations (to remove duplicates and to convert to NaNs)
aggregations <- aggregate(data[,4:length(data)], list(data$a_year, data$a_municipality, data$a_service), FUN=mean, na.rm=TRUE, nan.rm=TRUE)
colnames(aggregations)[1:3] <- c("Year", "Municipality", "Service")

# Remove wrong or unneeded data
aggregations <- aggregations[aggregations$Municipality != "SOG Test",]
aggregations <- aggregations[aggregations$Municipality != "Municipality",]
aggregations <- aggregations[aggregations$Service != "Service",]
aggregations <- aggregations[aggregations$Service != "",]
aggregations <- aggregations[aggregations$Service != "Budget and Finance",]

# List of municipalities
municipalities <- unique(aggregations$Municipality)

# List of services
services <- unique(aggregations$Service)

# Remove all infinite values
aggregations[aggregations == "Inf"] <- NaN

# A loop that creates a different workbook for each municipality, and a different sheet for each service
# Create an Excel file to write to
write.xlsx("CompleteAudited", file="CompleteAudited.xlsx", sheetName="CompleteAudited", 
           row.names=FALSE, col.names=FALSE, showNA = FALSE)

# A loop that creates a different workbook for each municipality, and a different sheet for each service
for (service in services)
{
  # Select which set of questions to use based on the municipality
  var <- switch(service,
                "Asphalt Maintenance and Repair"= qam,
                "Budget and Finance" = qbf,  
                "Building Inspection" = qbi,
                "Central Human Resources" = qhr,
                "Emergency Communications" = qec,
                "Fire Service" = qfs,
                "Fleet Maintenance" = qfm,
                "Household Recycling" = qre,
                "Parks and Recreation" = qpr,
                "Police Service" = qps,
                "Residential Refuse Collection" = qrr,
                "Wastewater Service" = qww,
                "Water Service" = qws,
                "Yard Waste and Leaf Collection" = qyw)
  
  # Create an empty dataset with just the questions
  temp <- aggregations[aggregations$Service==service & aggregations$Municipality=="None",]
  # Create an empty list
  list <- list()
  
  # Cycle through municipalities
  for (municipality in municipalities)
  {
    # If there exists any rows for that municipality/service combo, add it to that list
    if(nrow(aggregations[aggregations$Service==service & aggregations$Municipality==municipality,])>0)
    {
      list <- append(list, municipality)
    }
  }
  
  for (municipality in municipalities)
  {
    # If it's in the list, merge them together
    if (municipality %in% unlist(list))
    {
      temp2 <- aggregations[aggregations$Service==service & aggregations$Municipality==municipality,]
      temp2 <- temp2[,names(temp2) %in% c("Year", "Municipality", var)]
      # Remove empty rows (rows that have more NAs than length of the row, accounting for Year + Municipality)
      temp2 <- temp2[rowSums(is.na(temp2)) < (length(temp2)-2),]
      temp <- rbind(temp, temp2) 
    }
  }
  
  print(service)
  temp <- rbind(MetricNames$Name[match(names(temp), MetricNames$Code)], temp)
  temp[temp=="NaN"] <-""
  # Write the merged dataset for that service onto the sheet created
  write.xlsx(temp, file="CompleteAudited.xlsx", sheetName=service, append=TRUE, row.names=FALSE, col.names=TRUE, showNA= FALSE)
  
}