##############DATA FOR 1998 ##############
data1998 <- read.csv("zbp1998detail.txt", header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")
colnames(data1998) <- tolower(colnames(data1998))
data1998$naics <- as.numeric(as.character(data1998$naics))
data1998 <- subset(data1998, naics > 0)
data1998$n1_4_1998 <- data1998$n1_4 * 3
data1998$n5_9_1998 <- data1998$n5_9 * 7
data1998$n10_19_1998 <- data1998$n10_19 * 15
data1998$n20_49_1998 <- data1998$n20_49 * 35
data1998$n50_99_1998 <- data1998$n50_99 * 75
data1998$n100_249_1998 <- data1998$n100_249 * 175
data1998$n250_499_1998 <- data1998$n250_499 * 375
data1998$n500_999_1998 <- data1998$n500_999 * 750
data1998$n1000_1998 <- data1998$n1000 * 2000
data1998$Jobs_1998 <- data1998$n1_4_1998 + data1998$n5_9_1998 + data1998$n10_19_1998 + data1998$n20_49_1998 + data1998$n50_99_1998 + data1998$n100_249_1998 + data1998$n250_499_1998 + data1998$n500_999_1998 + data1998$n1000_1998
data1998_JobsT <- aggregate(data1998$Jobs_1998, by=list(data1998$zip), sum)
names(data1998_JobsT) <- c("zip","JobsT_1998")
data1998_NARROW <- subset(data1998, naics=="334612" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512230" | naics=="512290" | naics=="513111" | naics=="513112" | naics=="514110" | naics=="514120" | naics=="541310" | naics=="541340" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712190" | naics=="713210" | naics=="713290" | naics=="713940" | naics=="713990")
data1998_JobsN <- aggregate(data1998_NARROW$Jobs_1998, by=list(data1998_NARROW$zip), sum)
names(data1998_JobsN) <- c("zip", "ArtsN_1998")
data1998_WIDE <- subset(data1998, naics=="323117" | naics=="334612" | naics=="339992" | naics=="443130" | naics=="451140" | naics=="451211" | naics=="451220" | naics=="453920" | naics=="487110" | naics=="487210" | naics=="487990" | naics=="511110" | naics=="511120" | naics=="511130" | naics=="512110" | naics=="512120" | naics=="512131" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512220" | naics=="512230" | naics=="512240" | naics=="512290" | naics=="513111" | naics=="513112" | naics=="513120" | naics=="513210" | naics=="514110" | naics=="514120" | naics=="532292" | naics=="532490" | naics=="541310" | naics=="541340" | naics=="541370" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="541830" | naics=="541840" | naics=="541850" | naics=="541870" | naics=="541890" | naics=="541921" | naics=="541922" | naics=="561591" | naics=="561920" | naics=="561990" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711211" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712120" | naics=="712130" | naics=="712190" | naics=="713110" | naics=="713120" | naics=="713210" | naics=="713290" | naics=="713920" | naics=="713940" | naics=="713990" | naics=="722110" | naics=="722211" | naics=="722212" | naics=="722213" | naics=="722310" | naics=="722320")
data1998_JobsW <- aggregate(data1998_WIDE$Jobs_1998, by=list(data1998_WIDE$zip), sum)
names(data1998_JobsW) <- c("zip", "ArtsW_1998")
data1998_Total <- merge(data1998_JobsT, data1998_JobsN, by="zip")
data1998 <- merge(data1998_Total, data1998_JobsW, by="zip")
data1998$ArtsN_1998[data1998$JobsT_1998 > 0 & is.na(data1998$ArtsN_1998)] <- 0 
data1998$ArtsW_1998[data1998$JobsT_1998 > 0 & is.na(data1998$ArtsW_1998)] <- 0 
data1998$JobsN_1998 <- data1998$JobsT_1998 - data1998$ArtsN_1998
data1998$JobsW_1998 <- data1998$JobsT_1998 - data1998$ArtsW_1998
data1998$ArtSupport_1998 <- data1998$ArtsW_1998 - data1998$ArtsN_1998
data <- data1998
keep(data, sure=TRUE)
##############DATA FOR 1999 ##############
data1999 <- read.csv("zbp1999detail.txt", header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")
colnames(data1999) <- tolower(colnames(data1999))
data1999$naics <- as.numeric(as.character(data1999$naics))
data1999 <- subset(data1999, naics > 0)
data1999$n1_4_1999 <- data1999$n1_4 * 3
data1999$n5_9_1999 <- data1999$n5_9 * 7
data1999$n10_19_1999 <- data1999$n10_19 * 15
data1999$n20_49_1999 <- data1999$n20_49 * 35
data1999$n50_99_1999 <- data1999$n50_99 * 75
data1999$n100_249_1999 <- data1999$n100_249 * 175
data1999$n250_499_1999 <- data1999$n250_499 * 375
data1999$n500_999_1999 <- data1999$n500_999 * 750
data1999$n1000_1999 <- data1999$n1000 * 2000
data1999$Jobs_1999 <- data1999$n1_4_1999 + data1999$n5_9_1999 + data1999$n10_19_1999 + data1999$n20_49_1999 + data1999$n50_99_1999 + data1999$n100_249_1999 + data1999$n250_499_1999 + data1999$n500_999_1999 + data1999$n1000_1999
data1999_JobsT <- aggregate(data1999$Jobs_1999, by=list(data1999$zip), sum)
names(data1999_JobsT) <- c("zip","JobsT_1999")
data1999_NARROW <- subset(data1999, naics=="334612" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512230" | naics=="512290" | naics=="513111" | naics=="513112" | naics=="514110" | naics=="514120" | naics=="541310" | naics=="541340" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712190" | naics=="713210" | naics=="713290" | naics=="713940" | naics=="713990")
data1999_JobsN <- aggregate(data1999_NARROW$Jobs_1999, by=list(data1999_NARROW$zip), sum)
names(data1999_JobsN) <- c("zip", "ArtsN_1999")
data1999_WIDE <- subset(data1999, naics=="323117" | naics=="334612" | naics=="339992" | naics=="443130" | naics=="451140" | naics=="451211" | naics=="451220" | naics=="453920" | naics=="487110" | naics=="487210" | naics=="487990" | naics=="511110" | naics=="511120" | naics=="511130" | naics=="512110" | naics=="512120" | naics=="512131" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512220" | naics=="512230" | naics=="512240" | naics=="512290" | naics=="513111" | naics=="513112" | naics=="513120" | naics=="513210" | naics=="514110" | naics=="514120" | naics=="532292" | naics=="532490" | naics=="541310" | naics=="541340" | naics=="541370" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="541830" | naics=="541840" | naics=="541850" | naics=="541870" | naics=="541890" | naics=="541921" | naics=="541922" | naics=="561591" | naics=="561920" | naics=="561990" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711211" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712120" | naics=="712130" | naics=="712190" | naics=="713110" | naics=="713120" | naics=="713210" | naics=="713290" | naics=="713920" | naics=="713940" | naics=="713990" | naics=="722110" | naics=="722211" | naics=="722212" | naics=="722213" | naics=="722310" | naics=="722320")
data1999_JobsW <- aggregate(data1999_WIDE$Jobs_1999, by=list(data1999_WIDE$zip), sum)
names(data1999_JobsW) <- c("zip", "ArtsW_1999")
data1999_Total <- merge(data1999_JobsT, data1999_JobsN, by="zip")
data1999 <- merge(data1999_Total, data1999_JobsW, by="zip")
data1999$ArtsN_1999[data1999$JobsT_1999 > 0 & is.na(data1999$ArtsN_1999)] <- 0 
data1999$ArtsW_1999[data1999$JobsT_1999 > 0 & is.na(data1999$ArtsW_1999)] <- 0 
data1999$JobsN_1999 <- data1999$JobsT_1999 - data1999$ArtsN_1999
data1999$JobsW_1999 <- data1999$JobsT_1999 - data1999$ArtsW_1999
data1999$ArtSupport_1999 <- data1999$ArtsW_1999 - data1999$ArtsN_1999
data <- merge(data, data1999, by="zip", all=TRUE)
keep(data, sure=TRUE)
##############DATA FOR 2000 ##############
data2000 <- read.csv("zbp2000detail.txt", header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")
colnames(data2000) <- tolower(colnames(data2000))
data2000$naics <- as.numeric(as.character(data2000$naics))
data2000 <- subset(data2000, naics > 0)
data2000$n1_4_2000 <- data2000$n1_4 * 3
data2000$n5_9_2000 <- data2000$n5_9 * 7
data2000$n10_19_2000 <- data2000$n10_19 * 15
data2000$n20_49_2000 <- data2000$n20_49 * 35
data2000$n50_99_2000 <- data2000$n50_99 * 75
data2000$n100_249_2000 <- data2000$n100_249 * 175
data2000$n250_499_2000 <- data2000$n250_499 * 375
data2000$n500_999_2000 <- data2000$n500_999 * 750
data2000$n1000_2000 <- data2000$n1000 * 2000
data2000$Jobs_2000 <- data2000$n1_4_2000 + data2000$n5_9_2000 + data2000$n10_19_2000 + data2000$n20_49_2000 + data2000$n50_99_2000 + data2000$n100_249_2000 + data2000$n250_499_2000 + data2000$n500_999_2000 + data2000$n1000_2000
data2000_JobsT <- aggregate(data2000$Jobs_2000, by=list(data2000$zip), sum)
names(data2000_JobsT) <- c("zip","JobsT_2000")
data2000_NARROW <- subset(data2000, naics=="334612" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512230" | naics=="512290" | naics=="513111" | naics=="513112" | naics=="514110" | naics=="514120" | naics=="541310" | naics=="541340" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712190" | naics=="713210" | naics=="713290" | naics=="713940" | naics=="713990")
data2000_JobsN <- aggregate(data2000_NARROW$Jobs_2000, by=list(data2000_NARROW$zip), sum)
names(data2000_JobsN) <- c("zip", "ArtsN_2000")
data2000_WIDE <- subset(data2000, naics=="323117" | naics=="334612" | naics=="339992" | naics=="443130" | naics=="451140" | naics=="451211" | naics=="451220" | naics=="453920" | naics=="487110" | naics=="487210" | naics=="487990" | naics=="511110" | naics=="511120" | naics=="511130" | naics=="512110" | naics=="512120" | naics=="512131" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512220" | naics=="512230" | naics=="512240" | naics=="512290" | naics=="513111" | naics=="513112" | naics=="513120" | naics=="513210" | naics=="514110" | naics=="514120" | naics=="532292" | naics=="532490" | naics=="541310" | naics=="541340" | naics=="541370" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="541830" | naics=="541840" | naics=="541850" | naics=="541870" | naics=="541890" | naics=="541921" | naics=="541922" | naics=="561591" | naics=="561920" | naics=="561990" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711211" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712120" | naics=="712130" | naics=="712190" | naics=="713110" | naics=="713120" | naics=="713210" | naics=="713290" | naics=="713920" | naics=="713940" | naics=="713990" | naics=="722110" | naics=="722211" | naics=="722212" | naics=="722213" | naics=="722310" | naics=="722320")
data2000_JobsW <- aggregate(data2000_WIDE$Jobs_2000, by=list(data2000_WIDE$zip), sum)
names(data2000_JobsW) <- c("zip", "ArtsW_2000")
data2000_Total <- merge(data2000_JobsT, data2000_JobsN, by="zip")
data2000 <- merge(data2000_Total, data2000_JobsW, by="zip")
data2000$ArtsN_2000[data2000$JobsT_2000 > 0 & is.na(data2000$ArtsN_2000)] <- 0 
data2000$ArtsW_2000[data2000$JobsT_2000 > 0 & is.na(data2000$ArtsW_2000)] <- 0 
data2000$JobsN_2000 <- data2000$JobsT_2000 - data2000$ArtsN_2000
data2000$JobsW_2000 <- data2000$JobsT_2000 - data2000$ArtsW_2000
data2000$ArtSupport_2000 <- data2000$ArtsW_2000 - data2000$ArtsN_2000
data <- merge(data, data2000, by="zip", all=TRUE)
keep(data, sure=TRUE)
##############DATA FOR 2001 ##############
data2001 <- read.csv("zbp2001detail.txt", header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")
colnames(data2001) <- tolower(colnames(data2001))
data2001$naics <- as.numeric(as.character(data2001$naics))
data2001 <- subset(data2001, naics > 0)
data2001$n1_4_2001 <- data2001$n1_4 * 3
data2001$n5_9_2001 <- data2001$n5_9 * 7
data2001$n10_19_2001 <- data2001$n10_19 * 15
data2001$n20_49_2001 <- data2001$n20_49 * 35
data2001$n50_99_2001 <- data2001$n50_99 * 75
data2001$n100_249_2001 <- data2001$n100_249 * 175
data2001$n250_499_2001 <- data2001$n250_499 * 375
data2001$n500_999_2001 <- data2001$n500_999 * 750
data2001$n1000_2001 <- data2001$n1000 * 2000
data2001$Jobs_2001 <- data2001$n1_4_2001 + data2001$n5_9_2001 + data2001$n10_19_2001 + data2001$n20_49_2001 + data2001$n50_99_2001 + data2001$n100_249_2001 + data2001$n250_499_2001 + data2001$n500_999_2001 + data2001$n1000_2001
data2001_JobsT <- aggregate(data2001$Jobs_2001, by=list(data2001$zip), sum)
names(data2001_JobsT) <- c("zip","JobsT_2001")
data2001_NARROW <- subset(data2001, naics=="334612" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512230" | naics=="512290" | naics=="513111" | naics=="513112" | naics=="514110" | naics=="514120" | naics=="541310" | naics=="541340" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712190" | naics=="713210" | naics=="713290" | naics=="713940" | naics=="713990")
data2001_JobsN <- aggregate(data2001_NARROW$Jobs_2001, by=list(data2001_NARROW$zip), sum)
names(data2001_JobsN) <- c("zip", "ArtsN_2001")
data2001_WIDE <- subset(data2001, naics=="323117" | naics=="334612" | naics=="339992" | naics=="443130" | naics=="451140" | naics=="451211" | naics=="451220" | naics=="453920" | naics=="487110" | naics=="487210" | naics=="487990" | naics=="511110" | naics=="511120" | naics=="511130" | naics=="512110" | naics=="512120" | naics=="512131" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512220" | naics=="512230" | naics=="512240" | naics=="512290" | naics=="513111" | naics=="513112" | naics=="513120" | naics=="513210" | naics=="514110" | naics=="514120" | naics=="532292" | naics=="532490" | naics=="541310" | naics=="541340" | naics=="541370" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="541830" | naics=="541840" | naics=="541850" | naics=="541870" | naics=="541890" | naics=="541921" | naics=="541922" | naics=="561591" | naics=="561920" | naics=="561990" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711211" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712120" | naics=="712130" | naics=="712190" | naics=="713110" | naics=="713120" | naics=="713210" | naics=="713290" | naics=="713920" | naics=="713940" | naics=="713990" | naics=="722110" | naics=="722211" | naics=="722212" | naics=="722213" | naics=="722310" | naics=="722320")
data2001_JobsW <- aggregate(data2001_WIDE$Jobs_2001, by=list(data2001_WIDE$zip), sum)
names(data2001_JobsW) <- c("zip", "ArtsW_2001")
data2001_Total <- merge(data2001_JobsT, data2001_JobsN, by="zip")
data2001 <- merge(data2001_Total, data2001_JobsW, by="zip")
data2001$ArtsN_2001[data2001$JobsT_2001 > 0 & is.na(data2001$ArtsN_2001)] <- 0 
data2001$ArtsW_2001[data2001$JobsT_2001 > 0 & is.na(data2001$ArtsW_2001)] <- 0 
data2001$JobsN_2001 <- data2001$JobsT_2001 - data2001$ArtsN_2001
data2001$JobsW_2001 <- data2001$JobsT_2001 - data2001$ArtsW_2001
data2001$ArtSupport_2001 <- data2001$ArtsW_2001 - data2001$ArtsN_2001
data <- merge(data, data2001, by="zip", all=TRUE)
keep(data, sure=TRUE)
##############DATA FOR 2002 ##############
data2002 <- read.csv("zbp2002detail.txt", header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")
colnames(data2002) <- tolower(colnames(data2002))
data2002$naics <- as.numeric(as.character(data2002$naics))
data2002 <- subset(data2002, naics > 0)
data2002$n1_4_2002 <- data2002$n1_4 * 3
data2002$n5_9_2002 <- data2002$n5_9 * 7
data2002$n10_19_2002 <- data2002$n10_19 * 15
data2002$n20_49_2002 <- data2002$n20_49 * 35
data2002$n50_99_2002 <- data2002$n50_99 * 75
data2002$n100_249_2002 <- data2002$n100_249 * 175
data2002$n250_499_2002 <- data2002$n250_499 * 375
data2002$n500_999_2002 <- data2002$n500_999 * 750
data2002$n1000_2002 <- data2002$n1000 * 2000
data2002$Jobs_2002 <- data2002$n1_4_2002 + data2002$n5_9_2002 + data2002$n10_19_2002 + data2002$n20_49_2002 + data2002$n50_99_2002 + data2002$n100_249_2002 + data2002$n250_499_2002 + data2002$n500_999_2002 + data2002$n1000_2002
data2002_JobsT <- aggregate(data2002$Jobs_2002, by=list(data2002$zip), sum)
names(data2002_JobsT) <- c("zip","JobsT_2002")
data2002_NARROW <- subset(data2002, naics=="334612" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512230" | naics=="512290" | naics=="515111" | naics=="515112" | naics=="519110" | naics=="519120" | naics=="541310" | naics=="541340" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712190" | naics=="713210" | naics=="713290" | naics=="713940" | naics=="713990")
data2002_JobsN <- aggregate(data2002_NARROW$Jobs_2002, by=list(data2002_NARROW$zip), sum)
names(data2002_JobsN) <- c("zip", "ArtsN_2002")
data2002_WIDE <- subset(data2002, naics=="323117" | naics=="334612" | naics=="339992" | naics=="443130" | naics=="451140" | naics=="451211" | naics=="451220" | naics=="453920" | naics=="487110" | naics=="487210" | naics=="487990" | naics=="511110" | naics=="511120" | naics=="511130" | naics=="512110" | naics=="512120" | naics=="512131" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512220" | naics=="512230" | naics=="512240" | naics=="512290" | naics=="515111" | naics=="515112" | naics=="515120" | naics=="515210" | naics=="519110" | naics=="519120" | naics=="532292" | naics=="532490" | naics=="541310" | naics=="541340" | naics=="541370" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="541830" | naics=="541840" | naics=="541850" | naics=="541870" | naics=="541890" | naics=="541921" | naics=="541922" | naics=="561591" | naics=="561920" | naics=="561990" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711211" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712120" | naics=="712130" | naics=="712190" | naics=="713110" | naics=="713120" | naics=="713210" | naics=="713290" | naics=="713920" | naics=="713940" | naics=="713990" | naics=="722110" | naics=="722211" | naics=="722212" | naics=="722213" | naics=="722310" | naics=="722320")
data2002_JobsW <- aggregate(data2002_WIDE$Jobs_2002, by=list(data2002_WIDE$zip), sum)
names(data2002_JobsW) <- c("zip", "ArtsW_2002")
data2002_Total <- merge(data2002_JobsT, data2002_JobsN, by="zip")
data2002 <- merge(data2002_Total, data2002_JobsW, by="zip")
data2002$ArtsN_2002[data2002$JobsT_2002 > 0 & is.na(data2002$ArtsN_2002)] <- 0 
data2002$ArtsW_2002[data2002$JobsT_2002 > 0 & is.na(data2002$ArtsW_2002)] <- 0 
data2002$JobsN_2002 <- data2002$JobsT_2002 - data2002$ArtsN_2002
data2002$JobsW_2002 <- data2002$JobsT_2002 - data2002$ArtsW_2002
data2002$ArtSupport_2002 <- data2002$ArtsW_2002 - data2002$ArtsN_2002
data <- merge(data, data2002, by="zip", all=TRUE)
keep(data, sure=TRUE)
##############DATA FOR 2003 ##############
data2003 <- read.csv("zbp2003detail.txt", header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")
colnames(data2003) <- tolower(colnames(data2003))
data2003$naics <- as.numeric(as.character(data2003$naics))
data2003 <- subset(data2003, naics > 0)
data2003$n1_4_2003 <- data2003$n1_4 * 3
data2003$n5_9_2003 <- data2003$n5_9 * 7
data2003$n10_19_2003 <- data2003$n10_19 * 15
data2003$n20_49_2003 <- data2003$n20_49 * 35
data2003$n50_99_2003 <- data2003$n50_99 * 75
data2003$n100_249_2003 <- data2003$n100_249 * 175
data2003$n250_499_2003 <- data2003$n250_499 * 375
data2003$n500_999_2003 <- data2003$n500_999 * 750
data2003$n1000_2003 <- data2003$n1000 * 2000
data2003$Jobs_2003 <- data2003$n1_4_2003 + data2003$n5_9_2003 + data2003$n10_19_2003 + data2003$n20_49_2003 + data2003$n50_99_2003 + data2003$n100_249_2003 + data2003$n250_499_2003 + data2003$n500_999_2003 + data2003$n1000_2003
data2003_JobsT <- aggregate(data2003$Jobs_2003, by=list(data2003$zip), sum)
names(data2003_JobsT) <- c("zip","JobsT_2003")
data2003_NARROW <- subset(data2003, naics=="334612" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512230" | naics=="512290" | naics=="515111" | naics=="515112" | naics=="519110" | naics=="519120" | naics=="541310" | naics=="541340" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712190" | naics=="713210" | naics=="713290" | naics=="713940" | naics=="713990")
data2003_JobsN <- aggregate(data2003_NARROW$Jobs_2003, by=list(data2003_NARROW$zip), sum)
names(data2003_JobsN) <- c("zip", "ArtsN_2003")
data2003_WIDE <- subset(data2003, naics=="323117" | naics=="334612" | naics=="339992" | naics=="443130" | naics=="451140" | naics=="451211" | naics=="451220" | naics=="453920" | naics=="487110" | naics=="487210" | naics=="487990" | naics=="511110" | naics=="511120" | naics=="511130" | naics=="512110" | naics=="512120" | naics=="512131" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512220" | naics=="512230" | naics=="512240" | naics=="512290" | naics=="515111" | naics=="515112" | naics=="515120" | naics=="515210" | naics=="519110" | naics=="519120" | naics=="532292" | naics=="532490" | naics=="541310" | naics=="541340" | naics=="541370" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="541830" | naics=="541840" | naics=="541850" | naics=="541870" | naics=="541890" | naics=="541921" | naics=="541922" | naics=="561591" | naics=="561920" | naics=="561990" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711211" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712120" | naics=="712130" | naics=="712190" | naics=="713110" | naics=="713120" | naics=="713210" | naics=="713290" | naics=="713920" | naics=="713940" | naics=="713990" | naics=="722110" | naics=="722211" | naics=="722212" | naics=="722213" | naics=="722310" | naics=="722320")
data2003_JobsW <- aggregate(data2003_WIDE$Jobs_2003, by=list(data2003_WIDE$zip), sum)
names(data2003_JobsW) <- c("zip", "ArtsW_2003")
data2003_Total <- merge(data2003_JobsT, data2003_JobsN, by="zip")
data2003 <- merge(data2003_Total, data2003_JobsW, by="zip")
data2003$ArtsN_2003[data2003$JobsT_2003 > 0 & is.na(data2003$ArtsN_2003)] <- 0 
data2003$ArtsW_2003[data2003$JobsT_2003 > 0 & is.na(data2003$ArtsW_2003)] <- 0 
data2003$JobsN_2003 <- data2003$JobsT_2003 - data2003$ArtsN_2003
data2003$JobsW_2003 <- data2003$JobsT_2003 - data2003$ArtsW_2003
data2003$ArtSupport_2003 <- data2003$ArtsW_2003 - data2003$ArtsN_2003
data <- merge(data, data2003, by="zip", all=TRUE)
keep(data, sure=TRUE)
##############DATA FOR 2004 ##############
data2004 <- read.csv("zbp2004detail.txt", header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")
colnames(data2004) <- tolower(colnames(data2004))
data2004$naics <- as.numeric(as.character(data2004$naics))
data2004 <- subset(data2004, naics > 0)
data2004$n1_4_2004 <- data2004$n1_4 * 3
data2004$n5_9_2004 <- data2004$n5_9 * 7
data2004$n10_19_2004 <- data2004$n10_19 * 15
data2004$n20_49_2004 <- data2004$n20_49 * 35
data2004$n50_99_2004 <- data2004$n50_99 * 75
data2004$n100_249_2004 <- data2004$n100_249 * 175
data2004$n250_499_2004 <- data2004$n250_499 * 375
data2004$n500_999_2004 <- data2004$n500_999 * 750
data2004$n1000_2004 <- data2004$n1000 * 2000
data2004$Jobs_2004 <- data2004$n1_4_2004 + data2004$n5_9_2004 + data2004$n10_19_2004 + data2004$n20_49_2004 + data2004$n50_99_2004 + data2004$n100_249_2004 + data2004$n250_499_2004 + data2004$n500_999_2004 + data2004$n1000_2004
data2004_JobsT <- aggregate(data2004$Jobs_2004, by=list(data2004$zip), sum)
names(data2004_JobsT) <- c("zip","JobsT_2004")
data2004_NARROW <- subset(data2004, naics=="334612" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512230" | naics=="512290" | naics=="515111" | naics=="515112" | naics=="519110" | naics=="519120" | naics=="541310" | naics=="541340" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712190" | naics=="713210" | naics=="713290" | naics=="713940" | naics=="713990")
data2004_JobsN <- aggregate(data2004_NARROW$Jobs_2004, by=list(data2004_NARROW$zip), sum)
names(data2004_JobsN) <- c("zip", "ArtsN_2004")
data2004_WIDE <- subset(data2004, naics=="323117" | naics=="334612" | naics=="339992" | naics=="443130" | naics=="451140" | naics=="451211" | naics=="451220" | naics=="453920" | naics=="487110" | naics=="487210" | naics=="487990" | naics=="511110" | naics=="511120" | naics=="511130" | naics=="512110" | naics=="512120" | naics=="512131" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512220" | naics=="512230" | naics=="512240" | naics=="512290" | naics=="515111" | naics=="515112" | naics=="515120" | naics=="515210" | naics=="519110" | naics=="519120" | naics=="532292" | naics=="532490" | naics=="541310" | naics=="541340" | naics=="541370" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="541830" | naics=="541840" | naics=="541850" | naics=="541870" | naics=="541890" | naics=="541921" | naics=="541922" | naics=="561591" | naics=="561920" | naics=="561990" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711211" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712120" | naics=="712130" | naics=="712190" | naics=="713110" | naics=="713120" | naics=="713210" | naics=="713290" | naics=="713920" | naics=="713940" | naics=="713990" | naics=="722110" | naics=="722211" | naics=="722212" | naics=="722213" | naics=="722310" | naics=="722320")
data2004_JobsW <- aggregate(data2004_WIDE$Jobs_2004, by=list(data2004_WIDE$zip), sum)
names(data2004_JobsW) <- c("zip", "ArtsW_2004")
data2004_Total <- merge(data2004_JobsT, data2004_JobsN, by="zip")
data2004 <- merge(data2004_Total, data2004_JobsW, by="zip")
data2004$ArtsN_2004[data2004$JobsT_2004 > 0 & is.na(data2004$ArtsN_2004)] <- 0 
data2004$ArtsW_2004[data2004$JobsT_2004 > 0 & is.na(data2004$ArtsW_2004)] <- 0 
data2004$JobsN_2004 <- data2004$JobsT_2004 - data2004$ArtsN_2004
data2004$JobsW_2004 <- data2004$JobsT_2004 - data2004$ArtsW_2004
data2004$ArtSupport_2004 <- data2004$ArtsW_2004 - data2004$ArtsN_2004
data <- merge(data, data2004, by="zip", all=TRUE)
keep(data, sure=TRUE)
##############DATA FOR 2005 ##############
data2005 <- read.csv("zbp2005detail.txt", header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")
colnames(data2005) <- tolower(colnames(data2005))
data2005$naics <- as.numeric(as.character(data2005$naics))
data2005 <- subset(data2005, naics > 0)
data2005$n1_4_2005 <- data2005$n1_4 * 3
data2005$n5_9_2005 <- data2005$n5_9 * 7
data2005$n10_19_2005 <- data2005$n10_19 * 15
data2005$n20_49_2005 <- data2005$n20_49 * 35
data2005$n50_99_2005 <- data2005$n50_99 * 75
data2005$n100_249_2005 <- data2005$n100_249 * 175
data2005$n250_499_2005 <- data2005$n250_499 * 375
data2005$n500_999_2005 <- data2005$n500_999 * 750
data2005$n1000_2005 <- data2005$n1000 * 2000
data2005$Jobs_2005 <- data2005$n1_4_2005 + data2005$n5_9_2005 + data2005$n10_19_2005 + data2005$n20_49_2005 + data2005$n50_99_2005 + data2005$n100_249_2005 + data2005$n250_499_2005 + data2005$n500_999_2005 + data2005$n1000_2005
data2005_JobsT <- aggregate(data2005$Jobs_2005, by=list(data2005$zip), sum)
names(data2005_JobsT) <- c("zip","JobsT_2005")
data2005_NARROW <- subset(data2005, naics=="334612" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512230" | naics=="512290" | naics=="515111" | naics=="515112" | naics=="519110" | naics=="519120" | naics=="541310" | naics=="541340" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712190" | naics=="713210" | naics=="713290" | naics=="713940" | naics=="713990")
data2005_JobsN <- aggregate(data2005_NARROW$Jobs_2005, by=list(data2005_NARROW$zip), sum)
names(data2005_JobsN) <- c("zip", "ArtsN_2005")
data2005_WIDE <- subset(data2005, naics=="323117" | naics=="334612" | naics=="339992" | naics=="443130" | naics=="451140" | naics=="451211" | naics=="451220" | naics=="453920" | naics=="487110" | naics=="487210" | naics=="487990" | naics=="511110" | naics=="511120" | naics=="511130" | naics=="512110" | naics=="512120" | naics=="512131" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512220" | naics=="512230" | naics=="512240" | naics=="512290" | naics=="515111" | naics=="515112" | naics=="515120" | naics=="515210" | naics=="519110" | naics=="519120" | naics=="532292" | naics=="532490" | naics=="541310" | naics=="541340" | naics=="541370" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="541830" | naics=="541840" | naics=="541850" | naics=="541870" | naics=="541890" | naics=="541921" | naics=="541922" | naics=="561591" | naics=="561920" | naics=="561990" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711211" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712120" | naics=="712130" | naics=="712190" | naics=="713110" | naics=="713120" | naics=="713210" | naics=="713290" | naics=="713920" | naics=="713940" | naics=="713990" | naics=="722110" | naics=="722211" | naics=="722212" | naics=="722213" | naics=="722310" | naics=="722320")
data2005_JobsW <- aggregate(data2005_WIDE$Jobs_2005, by=list(data2005_WIDE$zip), sum)
names(data2005_JobsW) <- c("zip", "ArtsW_2005")
data2005_Total <- merge(data2005_JobsT, data2005_JobsN, by="zip")
data2005 <- merge(data2005_Total, data2005_JobsW, by="zip")
data2005$ArtsN_2005[data2005$JobsT_2005 > 0 & is.na(data2005$ArtsN_2005)] <- 0 
data2005$ArtsW_2005[data2005$JobsT_2005 > 0 & is.na(data2005$ArtsW_2005)] <- 0 
data2005$JobsN_2005 <- data2005$JobsT_2005 - data2005$ArtsN_2005
data2005$JobsW_2005 <- data2005$JobsT_2005 - data2005$ArtsW_2005
data2005$ArtSupport_2005 <- data2005$ArtsW_2005 - data2005$ArtsN_2005
data <- merge(data, data2005, by="zip", all=TRUE)
keep(data, sure=TRUE)
##############DATA FOR 2006 ##############
data2006 <- read.csv("zbp2006detail.txt", header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")
colnames(data2006) <- tolower(colnames(data2006))
data2006$naics <- as.numeric(as.character(data2006$naics))
data2006 <- subset(data2006, naics > 0)
data2006$n1_4_2006 <- data2006$n1_4 * 3
data2006$n5_9_2006 <- data2006$n5_9 * 7
data2006$n10_19_2006 <- data2006$n10_19 * 15
data2006$n20_49_2006 <- data2006$n20_49 * 35
data2006$n50_99_2006 <- data2006$n50_99 * 75
data2006$n100_249_2006 <- data2006$n100_249 * 175
data2006$n250_499_2006 <- data2006$n250_499 * 375
data2006$n500_999_2006 <- data2006$n500_999 * 750
data2006$n1000_2006 <- data2006$n1000 * 2000
data2006$Jobs_2006 <- data2006$n1_4_2006 + data2006$n5_9_2006 + data2006$n10_19_2006 + data2006$n20_49_2006 + data2006$n50_99_2006 + data2006$n100_249_2006 + data2006$n250_499_2006 + data2006$n500_999_2006 + data2006$n1000_2006
data2006_JobsT <- aggregate(data2006$Jobs_2006, by=list(data2006$zip), sum)
names(data2006_JobsT) <- c("zip","JobsT_2006")
data2006_NARROW <- subset(data2006, naics=="334612" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512230" | naics=="512290" | naics=="515111" | naics=="515112" | naics=="519110" | naics=="519120" | naics=="541310" | naics=="541340" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712190" | naics=="713210" | naics=="713290" | naics=="713940" | naics=="713990")
data2006_JobsN <- aggregate(data2006_NARROW$Jobs_2006, by=list(data2006_NARROW$zip), sum)
names(data2006_JobsN) <- c("zip", "ArtsN_2006")
data2006_WIDE <- subset(data2006, naics=="323117" | naics=="334612" | naics=="339992" | naics=="443130" | naics=="451140" | naics=="451211" | naics=="451220" | naics=="453920" | naics=="487110" | naics=="487210" | naics=="487990" | naics=="511110" | naics=="511120" | naics=="511130" | naics=="512110" | naics=="512120" | naics=="512131" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512220" | naics=="512230" | naics=="512240" | naics=="512290" | naics=="515111" | naics=="515112" | naics=="515120" | naics=="515210" | naics=="519110" | naics=="519120" | naics=="532292" | naics=="532490" | naics=="541310" | naics=="541340" | naics=="541370" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="541830" | naics=="541840" | naics=="541850" | naics=="541870" | naics=="541890" | naics=="541921" | naics=="541922" | naics=="561591" | naics=="561920" | naics=="561990" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711211" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712120" | naics=="712130" | naics=="712190" | naics=="713110" | naics=="713120" | naics=="713210" | naics=="713290" | naics=="713920" | naics=="713940" | naics=="713990" | naics=="722110" | naics=="722211" | naics=="722212" | naics=="722213" | naics=="722310" | naics=="722320")
data2006_JobsW <- aggregate(data2006_WIDE$Jobs_2006, by=list(data2006_WIDE$zip), sum)
names(data2006_JobsW) <- c("zip", "ArtsW_2006")
data2006_Total <- merge(data2006_JobsT, data2006_JobsN, by="zip")
data2006 <- merge(data2006_Total, data2006_JobsW, by="zip")
data2006$ArtsN_2006[data2006$JobsT_2006 > 0 & is.na(data2006$ArtsN_2006)] <- 0 
data2006$ArtsW_2006[data2006$JobsT_2006 > 0 & is.na(data2006$ArtsW_2006)] <- 0 
data2006$JobsN_2006 <- data2006$JobsT_2006 - data2006$ArtsN_2006
data2006$JobsW_2006 <- data2006$JobsT_2006 - data2006$ArtsW_2006
data2006$ArtSupport_2006 <- data2006$ArtsW_2006 - data2006$ArtsN_2006
data <- merge(data, data2006, by="zip", all=TRUE)
keep(data, sure=TRUE)
##############DATA FOR 2007 ##############
data2007 <- read.csv("zbp2007detail.txt", header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")
colnames(data2007) <- tolower(colnames(data2007))
data2007$naics <- as.numeric(as.character(data2007$naics))
data2007 <- subset(data2007, naics > 0)
data2007$n1_4_2007 <- data2007$n1_4 * 3
data2007$n5_9_2007 <- data2007$n5_9 * 7
data2007$n10_19_2007 <- data2007$n10_19 * 15
data2007$n20_49_2007 <- data2007$n20_49 * 35
data2007$n50_99_2007 <- data2007$n50_99 * 75
data2007$n100_249_2007 <- data2007$n100_249 * 175
data2007$n250_499_2007 <- data2007$n250_499 * 375
data2007$n500_999_2007 <- data2007$n500_999 * 750
data2007$n1000_2007 <- data2007$n1000 * 2000
data2007$Jobs_2007 <- data2007$n1_4_2007 + data2007$n5_9_2007 + data2007$n10_19_2007 + data2007$n20_49_2007 + data2007$n50_99_2007 + data2007$n100_249_2007 + data2007$n250_499_2007 + data2007$n500_999_2007 + data2007$n1000_2007
data2007_JobsT <- aggregate(data2007$Jobs_2007, by=list(data2007$zip), sum)
names(data2007_JobsT) <- c("zip","JobsT_2007")
data2007_NARROW <- subset(data2007, naics=="334612" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512230" | naics=="512290" | naics=="515111" | naics=="515112" | naics=="519110" | naics=="519120" | naics=="541310" | naics=="541340" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712190" | naics=="713210" | naics=="713290" | naics=="713940" | naics=="713990")
data2007_JobsN <- aggregate(data2007_NARROW$Jobs_2007, by=list(data2007_NARROW$zip), sum)
names(data2007_JobsN) <- c("zip", "ArtsN_2007")
data2007_WIDE <- subset(data2007, naics=="323117" | naics=="334612" | naics=="339992" | naics=="443130" | naics=="451140" | naics=="451211" | naics=="451220" | naics=="453920" | naics=="487110" | naics=="487210" | naics=="487990" | naics=="511110" | naics=="511120" | naics=="511130" | naics=="512110" | naics=="512120" | naics=="512131" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512220" | naics=="512230" | naics=="512240" | naics=="512290" | naics=="515111" | naics=="515112" | naics=="515120" | naics=="515210" | naics=="519110" | naics=="519120" | naics=="532292" | naics=="532490" | naics=="541310" | naics=="541340" | naics=="541370" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="541830" | naics=="541840" | naics=="541850" | naics=="541870" | naics=="541890" | naics=="541921" | naics=="541922" | naics=="561591" | naics=="561920" | naics=="561990" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711211" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712120" | naics=="712130" | naics=="712190" | naics=="713110" | naics=="713120" | naics=="713210" | naics=="713290" | naics=="713920" | naics=="713940" | naics=="713990" | naics=="722110" | naics=="722211" | naics=="722212" | naics=="722213" | naics=="722310" | naics=="722320")
data2007_JobsW <- aggregate(data2007_WIDE$Jobs_2007, by=list(data2007_WIDE$zip), sum)
names(data2007_JobsW) <- c("zip", "ArtsW_2007")
data2007_Total <- merge(data2007_JobsT, data2007_JobsN, by="zip")
data2007 <- merge(data2007_Total, data2007_JobsW, by="zip")
data2007$ArtsN_2007[data2007$JobsT_2007 > 0 & is.na(data2007$ArtsN_2007)] <- 0 
data2007$ArtsW_2007[data2007$JobsT_2007 > 0 & is.na(data2007$ArtsW_2007)] <- 0 
data2007$JobsN_2007 <- data2007$JobsT_2007 - data2007$ArtsN_2007
data2007$JobsW_2007 <- data2007$JobsT_2007 - data2007$ArtsW_2007
data2007$ArtSupport_2007 <- data2007$ArtsW_2007 - data2007$ArtsN_2007
data <- merge(data, data2007, by="zip", all=TRUE)
keep(data, sure=TRUE)
##############DATA FOR 2008 ##############
data2008 <- read.csv("zbp2008detail.txt", header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")
colnames(data2008) <- tolower(colnames(data2008))
data2008$naics <- as.numeric(as.character(data2008$naics))
data2008 <- subset(data2008, naics > 0)
data2008$n1_4_2008 <- data2008$n1_4 * 3
data2008$n5_9_2008 <- data2008$n5_9 * 7
data2008$n10_19_2008 <- data2008$n10_19 * 15
data2008$n20_49_2008 <- data2008$n20_49 * 35
data2008$n50_99_2008 <- data2008$n50_99 * 75
data2008$n100_249_2008 <- data2008$n100_249 * 175
data2008$n250_499_2008 <- data2008$n250_499 * 375
data2008$n500_999_2008 <- data2008$n500_999 * 750
data2008$n1000_2008 <- data2008$n1000 * 2000
data2008$Jobs_2008 <- data2008$n1_4_2008 + data2008$n5_9_2008 + data2008$n10_19_2008 + data2008$n20_49_2008 + data2008$n50_99_2008 + data2008$n100_249_2008 + data2008$n250_499_2008 + data2008$n500_999_2008 + data2008$n1000_2008
data2008_JobsT <- aggregate(data2008$Jobs_2008, by=list(data2008$zip), sum)
names(data2008_JobsT) <- c("zip","JobsT_2008")
data2008_NARROW <- subset(data2008, naics=="334612" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512230" | naics=="512290" | naics=="515111" | naics=="515112" | naics=="519110" | naics=="519120" | naics=="541310" | naics=="541340" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712190" | naics=="713210" | naics=="713290" | naics=="713940" | naics=="713990")
data2008_JobsN <- aggregate(data2008_NARROW$Jobs_2008, by=list(data2008_NARROW$zip), sum)
names(data2008_JobsN) <- c("zip", "ArtsN_2008")
data2008_WIDE <- subset(data2008, naics=="323117" | naics=="334612" | naics=="339992" | naics=="443130" | naics=="451140" | naics=="451211" | naics=="451220" | naics=="453920" | naics=="487110" | naics=="487210" | naics=="487990" | naics=="511110" | naics=="511120" | naics=="511130" | naics=="512110" | naics=="512120" | naics=="512131" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512220" | naics=="512230" | naics=="512240" | naics=="512290" | naics=="515111" | naics=="515112" | naics=="515120" | naics=="515210" | naics=="519110" | naics=="519120" | naics=="532292" | naics=="532490" | naics=="541310" | naics=="541340" | naics=="541370" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="541830" | naics=="541840" | naics=="541850" | naics=="541870" | naics=="541890" | naics=="541921" | naics=="541922" | naics=="561591" | naics=="561920" | naics=="561990" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711211" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712120" | naics=="712130" | naics=="712190" | naics=="713110" | naics=="713120" | naics=="713210" | naics=="713290" | naics=="713920" | naics=="713940" | naics=="713990" | naics=="722110" | naics=="722211" | naics=="722212" | naics=="722213" | naics=="722310" | naics=="722320")
data2008_JobsW <- aggregate(data2008_WIDE$Jobs_2008, by=list(data2008_WIDE$zip), sum)
names(data2008_JobsW) <- c("zip", "ArtsW_2008")
data2008_Total <- merge(data2008_JobsT, data2008_JobsN, by="zip")
data2008 <- merge(data2008_Total, data2008_JobsW, by="zip")
data2008$ArtsN_2008[data2008$JobsT_2008 > 0 & is.na(data2008$ArtsN_2008)] <- 0 
data2008$ArtsW_2008[data2008$JobsT_2008 > 0 & is.na(data2008$ArtsW_2008)] <- 0 
data2008$JobsN_2008 <- data2008$JobsT_2008 - data2008$ArtsN_2008
data2008$JobsW_2008 <- data2008$JobsT_2008 - data2008$ArtsW_2008
data2008$ArtSupport_2008 <- data2008$ArtsW_2008 - data2008$ArtsN_2008
data <- merge(data, data2008, by="zip", all=TRUE)
keep(data, sure=TRUE)
##############DATA FOR 2009 ##############
data2009 <- read.csv("zbp2009detail.txt", header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")
colnames(data2009) <- tolower(colnames(data2009))
data2009$naics <- as.numeric(as.character(data2009$naics))
data2009 <- subset(data2009, naics > 0)
data2009$n1_4_2009 <- data2009$n1_4 * 3
data2009$n5_9_2009 <- data2009$n5_9 * 7
data2009$n10_19_2009 <- data2009$n10_19 * 15
data2009$n20_49_2009 <- data2009$n20_49 * 35
data2009$n50_99_2009 <- data2009$n50_99 * 75
data2009$n100_249_2009 <- data2009$n100_249 * 175
data2009$n250_499_2009 <- data2009$n250_499 * 375
data2009$n500_999_2009 <- data2009$n500_999 * 750
data2009$n1000_2009 <- data2009$n1000 * 2000
data2009$Jobs_2009 <- data2009$n1_4_2009 + data2009$n5_9_2009 + data2009$n10_19_2009 + data2009$n20_49_2009 + data2009$n50_99_2009 + data2009$n100_249_2009 + data2009$n250_499_2009 + data2009$n500_999_2009 + data2009$n1000_2009
data2009_JobsT <- aggregate(data2009$Jobs_2009, by=list(data2009$zip), sum)
names(data2009_JobsT) <- c("zip","JobsT_2009")
data2009_NARROW <- subset(data2009, naics=="334612" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512230" | naics=="512290" | naics=="515111" | naics=="515112" | naics=="519110" | naics=="519120" | naics=="541310" | naics=="541340" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712190" | naics=="713210" | naics=="713290" | naics=="713940" | naics=="713990")
data2009_JobsN <- aggregate(data2009_NARROW$Jobs_2009, by=list(data2009_NARROW$zip), sum)
names(data2009_JobsN) <- c("zip", "ArtsN_2009")
data2009_WIDE <- subset(data2009, naics=="323117" | naics=="334612" | naics=="339992" | naics=="443130" | naics=="451140" | naics=="451211" | naics=="451220" | naics=="453920" | naics=="487110" | naics=="487210" | naics=="487990" | naics=="511110" | naics=="511120" | naics=="511130" | naics=="512110" | naics=="512120" | naics=="512131" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512220" | naics=="512230" | naics=="512240" | naics=="512290" | naics=="515111" | naics=="515112" | naics=="515120" | naics=="515210" | naics=="519110" | naics=="519120" | naics=="532292" | naics=="532490" | naics=="541310" | naics=="541340" | naics=="541370" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="541830" | naics=="541840" | naics=="541850" | naics=="541870" | naics=="541890" | naics=="541921" | naics=="541922" | naics=="561591" | naics=="561920" | naics=="561990" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711211" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712120" | naics=="712130" | naics=="712190" | naics=="713110" | naics=="713120" | naics=="713210" | naics=="713290" | naics=="713920" | naics=="713940" | naics=="713990" | naics=="722110" | naics=="722211" | naics=="722212" | naics=="722213" | naics=="722310" | naics=="722320")
data2009_JobsW <- aggregate(data2009_WIDE$Jobs_2009, by=list(data2009_WIDE$zip), sum)
names(data2009_JobsW) <- c("zip", "ArtsW_2009")
data2009_Total <- merge(data2009_JobsT, data2009_JobsN, by="zip")
data2009 <- merge(data2009_Total, data2009_JobsW, by="zip")
data2009$ArtsN_2009[data2009$JobsT_2009 > 0 & is.na(data2009$ArtsN_2009)] <- 0 
data2009$ArtsW_2009[data2009$JobsT_2009 > 0 & is.na(data2009$ArtsW_2009)] <- 0 
data2009$JobsN_2009 <- data2009$JobsT_2009 - data2009$ArtsN_2009
data2009$JobsW_2009 <- data2009$JobsT_2009 - data2009$ArtsW_2009
data2009$ArtSupport_2009 <- data2009$ArtsW_2009 - data2009$ArtsN_2009
data <- merge(data, data2009, by="zip", all=TRUE)
keep(data, sure=TRUE)
##############DATA FOR 2010 ##############
data2010 <- read.csv("zbp2010detail.txt", header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")
colnames(data2010) <- tolower(colnames(data2010))
data2010$naics <- as.numeric(as.character(data2010$naics))
data2010 <- subset(data2010, naics > 0)
data2010$n1_4_2010 <- data2010$n1_4 * 3
data2010$n5_9_2010 <- data2010$n5_9 * 7
data2010$n10_19_2010 <- data2010$n10_19 * 15
data2010$n20_49_2010 <- data2010$n20_49 * 35
data2010$n50_99_2010 <- data2010$n50_99 * 75
data2010$n100_249_2010 <- data2010$n100_249 * 175
data2010$n250_499_2010 <- data2010$n250_499 * 375
data2010$n500_999_2010 <- data2010$n500_999 * 750
data2010$n1000_2010 <- data2010$n1000 * 2000
data2010$Jobs_2010 <- data2010$n1_4_2010 + data2010$n5_9_2010 + data2010$n10_19_2010 + data2010$n20_49_2010 + data2010$n50_99_2010 + data2010$n100_249_2010 + data2010$n250_499_2010 + data2010$n500_999_2010 + data2010$n1000_2010
data2010_JobsT <- aggregate(data2010$Jobs_2010, by=list(data2010$zip), sum)
names(data2010_JobsT) <- c("zip","JobsT_2010")
data2010_NARROW <- subset(data2010, naics=="334612" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512230" | naics=="512290" | naics=="515111" | naics=="515112" | naics=="519110" | naics=="519120" | naics=="541310" | naics=="541340" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712190" | naics=="713210" | naics=="713290" | naics=="713940" | naics=="713990")
data2010_JobsN <- aggregate(data2010_NARROW$Jobs_2010, by=list(data2010_NARROW$zip), sum)
names(data2010_JobsN) <- c("zip", "ArtsN_2010")
data2010_WIDE <- subset(data2010, naics=="323117" | naics=="334612" | naics=="339992" | naics=="443130" | naics=="451140" | naics=="451211" | naics=="451220" | naics=="453920" | naics=="487110" | naics=="487210" | naics=="487990" | naics=="511110" | naics=="511120" | naics=="511130" | naics=="512110" | naics=="512120" | naics=="512131" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512220" | naics=="512230" | naics=="512240" | naics=="512290" | naics=="515111" | naics=="515112" | naics=="515120" | naics=="515210" | naics=="519110" | naics=="519120" | naics=="532292" | naics=="532490" | naics=="541310" | naics=="541340" | naics=="541370" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="541830" | naics=="541840" | naics=="541850" | naics=="541870" | naics=="541890" | naics=="541921" | naics=="541922" | naics=="561591" | naics=="561920" | naics=="561990" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711211" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712120" | naics=="712130" | naics=="712190" | naics=="713110" | naics=="713120" | naics=="713210" | naics=="713290" | naics=="713920" | naics=="713940" | naics=="713990" | naics=="722110" | naics=="722211" | naics=="722212" | naics=="722213" | naics=="722310" | naics=="722320")
data2010_JobsW <- aggregate(data2010_WIDE$Jobs_2010, by=list(data2010_WIDE$zip), sum)
names(data2010_JobsW) <- c("zip", "ArtsW_2010")
data2010_Total <- merge(data2010_JobsT, data2010_JobsN, by="zip")
data2010 <- merge(data2010_Total, data2010_JobsW, by="zip")
data2010$ArtsN_2010[data2010$JobsT_2010 > 0 & is.na(data2010$ArtsN_2010)] <- 0 
data2010$ArtsW_2010[data2010$JobsT_2010 > 0 & is.na(data2010$ArtsW_2010)] <- 0 
data2010$JobsN_2010 <- data2010$JobsT_2010 - data2010$ArtsN_2010
data2010$JobsW_2010 <- data2010$JobsT_2010 - data2010$ArtsW_2010
data2010$ArtSupport_2010 <- data2010$ArtsW_2010 - data2010$ArtsN_2010
data <- merge(data, data2010, by="zip", all=TRUE)
keep(data, sure=TRUE)
##############DATA FOR 2011 ##############
data2011 <- read.csv("zbp2011detail.txt", header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")
colnames(data2011) <- tolower(colnames(data2011))
data2011$naics <- as.numeric(as.character(data2011$naics))
data2011 <- subset(data2011, naics > 0)
data2011$n1_4_2011 <- data2011$n1_4 * 3
data2011$n5_9_2011 <- data2011$n5_9 * 7
data2011$n10_19_2011 <- data2011$n10_19 * 15
data2011$n20_49_2011 <- data2011$n20_49 * 35
data2011$n50_99_2011 <- data2011$n50_99 * 75
data2011$n100_249_2011 <- data2011$n100_249 * 175
data2011$n250_499_2011 <- data2011$n250_499 * 375
data2011$n500_999_2011 <- data2011$n500_999 * 750
data2011$n1000_2011 <- data2011$n1000 * 2000
data2011$Jobs_2011 <- data2011$n1_4_2011 + data2011$n5_9_2011 + data2011$n10_19_2011 + data2011$n20_49_2011 + data2011$n50_99_2011 + data2011$n100_249_2011 + data2011$n250_499_2011 + data2011$n500_999_2011 + data2011$n1000_2011
data2011_JobsT <- aggregate(data2011$Jobs_2011, by=list(data2011$zip), sum)
names(data2011_JobsT) <- c("zip","JobsT_2011")
data2011_NARROW <- subset(data2011, naics=="334612" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512230" | naics=="512290" | naics=="515111" | naics=="515112" | naics=="519110" | naics=="519120" | naics=="541310" | naics=="541340" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712190" | naics=="713210" | naics=="713290" | naics=="713940" | naics=="713990")
data2011_JobsN <- aggregate(data2011_NARROW$Jobs_2011, by=list(data2011_NARROW$zip), sum)
names(data2011_JobsN) <- c("zip", "ArtsN_2011")
data2011_WIDE <- subset(data2011, naics=="323117" | naics=="334612" | naics=="339992" | naics=="443130" | naics=="451140" | naics=="451211" | naics=="451220" | naics=="453920" | naics=="487110" | naics=="487210" | naics=="487990" | naics=="511110" | naics=="511120" | naics=="511130" | naics=="512110" | naics=="512120" | naics=="512131" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512220" | naics=="512230" | naics=="512240" | naics=="512290" | naics=="515111" | naics=="515112" | naics=="515120" | naics=="515210" | naics=="519110" | naics=="519120" | naics=="532292" | naics=="532490" | naics=="541310" | naics=="541340" | naics=="541370" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="541830" | naics=="541840" | naics=="541850" | naics=="541870" | naics=="541890" | naics=="541921" | naics=="541922" | naics=="561591" | naics=="561920" | naics=="561990" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711211" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712120" | naics=="712130" | naics=="712190" | naics=="713110" | naics=="713120" | naics=="713210" | naics=="713290" | naics=="713920" | naics=="713940" | naics=="713990" | naics=="722110" | naics=="722211" | naics=="722212" | naics=="722213" | naics=="722310" | naics=="722320")
data2011_JobsW <- aggregate(data2011_WIDE$Jobs_2011, by=list(data2011_WIDE$zip), sum)
names(data2011_JobsW) <- c("zip", "ArtsW_2011")
data2011_Total <- merge(data2011_JobsT, data2011_JobsN, by="zip")
data2011 <- merge(data2011_Total, data2011_JobsW, by="zip")
data2011$ArtsN_2011[data2011$JobsT_2011 > 0 & is.na(data2011$ArtsN_2011)] <- 0 
data2011$ArtsW_2011[data2011$JobsT_2011 > 0 & is.na(data2011$ArtsW_2011)] <- 0 
data2011$JobsN_2011 <- data2011$JobsT_2011 - data2011$ArtsN_2011
data2011$JobsW_2011 <- data2011$JobsT_2011 - data2011$ArtsW_2011
data2011$ArtSupport_2011 <- data2011$ArtsW_2011 - data2011$ArtsN_2011
data <- merge(data, data2011, by="zip", all=TRUE)
keep(data, sure=TRUE)
##############DATA FOR 2012 ##############
data2012 <- read.csv("zbp2012detail.txt", header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")
colnames(data2012) <- tolower(colnames(data2012))
data2012$naics <- as.numeric(as.character(data2012$naics))
data2012 <- subset(data2012, naics > 0)
data2012$n1_4_2012 <- data2012$n1_4 * 3
data2012$n5_9_2012 <- data2012$n5_9 * 7
data2012$n10_19_2012 <- data2012$n10_19 * 15
data2012$n20_49_2012 <- data2012$n20_49 * 35
data2012$n50_99_2012 <- data2012$n50_99 * 75
data2012$n100_249_2012 <- data2012$n100_249 * 175
data2012$n250_499_2012 <- data2012$n250_499 * 375
data2012$n500_999_2012 <- data2012$n500_999 * 750
data2012$n1000_2012 <- data2012$n1000 * 2000
data2012$Jobs_2012 <- data2012$n1_4_2012 + data2012$n5_9_2012 + data2012$n10_19_2012 + data2012$n20_49_2012 + data2012$n50_99_2012 + data2012$n100_249_2012 + data2012$n250_499_2012 + data2012$n500_999_2012 + data2012$n1000_2012
data2012_JobsT <- aggregate(data2012$Jobs_2012, by=list(data2012$zip), sum)
names(data2012_JobsT) <- c("zip","JobsT_2012")
data2012_NARROW <- subset(data2012, naics=="334614" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512230" | naics=="512290" | naics=="515111" | naics=="515112" | naics=="519110" | naics=="519120" | naics=="541310" | naics=="541340" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712190" | naics=="713210" | naics=="713290" | naics=="713940" | naics=="713990")
data2012_JobsN <- aggregate(data2012_NARROW$Jobs_2012, by=list(data2012_NARROW$zip), sum)
names(data2012_JobsN) <- c("zip", "ArtsN_2012")
data2012_WIDE <- subset(data2012, naics=="323117" | naics=="334614" | naics=="339992" | naics=="443142" | naics=="451140" | naics=="451211" | naics=="443142" | naics=="453920" | naics=="487110" | naics=="487210" | naics=="487990" | naics=="511110" | naics=="511120" | naics=="511130" | naics=="512110" | naics=="512120" | naics=="512131" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512220" | naics=="512230" | naics=="512240" | naics=="512290" | naics=="515111" | naics=="515112" | naics=="515120" | naics=="515210" | naics=="519110" | naics=="519120" | naics=="532292" | naics=="532490" | naics=="541310" | naics=="541340" | naics=="541370" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="541830" | naics=="541840" | naics=="541850" | naics=="541870" | naics=="541890" | naics=="541921" | naics=="541922" | naics=="561591" | naics=="561920" | naics=="561990" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711211" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712120" | naics=="712130" | naics=="712190" | naics=="713110" | naics=="713120" | naics=="713210" | naics=="713290" | naics=="713920" | naics=="713940" | naics=="713990" | naics=="722511" | naics=="722513" | naics=="722514" | naics=="722515" | naics=="722310" | naics=="722320")
data2012_JobsW <- aggregate(data2012_WIDE$Jobs_2012, by=list(data2012_WIDE$zip), sum)
names(data2012_JobsW) <- c("zip", "ArtsW_2012")
data2012_Total <- merge(data2012_JobsT, data2012_JobsN, by="zip")
data2012 <- merge(data2012_Total, data2012_JobsW, by="zip")
data2012$ArtsN_2012[data2012$JobsT_2012 > 0 & is.na(data2012$ArtsN_2012)] <- 0 
data2012$ArtsW_2012[data2012$JobsT_2012 > 0 & is.na(data2012$ArtsW_2012)] <- 0 
data2012$JobsN_2012 <- data2012$JobsT_2012 - data2012$ArtsN_2012
data2012$JobsW_2012 <- data2012$JobsT_2012 - data2012$ArtsW_2012
data2012$ArtSupport_2012 <- data2012$ArtsW_2012 - data2012$ArtsN_2012
data <- merge(data, data2012, by="zip", all=TRUE)
keep(data, sure=TRUE)
##############DATA FOR 2013 ##############
data2013 <- read.csv("zbp2013detail.txt", header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")
colnames(data2013) <- tolower(colnames(data2013))
data2013$naics <- as.numeric(as.character(data2013$naics))
data2013 <- subset(data2013, naics > 0)
data2013$n1_4_2013 <- data2013$n1_4 * 3
data2013$n5_9_2013 <- data2013$n5_9 * 7
data2013$n10_19_2013 <- data2013$n10_19 * 15
data2013$n20_49_2013 <- data2013$n20_49 * 35
data2013$n50_99_2013 <- data2013$n50_99 * 75
data2013$n100_249_2013 <- data2013$n100_249 * 175
data2013$n250_499_2013 <- data2013$n250_499 * 375
data2013$n500_999_2013 <- data2013$n500_999 * 750
data2013$n1000_2013 <- data2013$n1000 * 2000
data2013$Jobs_2013 <- data2013$n1_4_2013 + data2013$n5_9_2013 + data2013$n10_19_2013 + data2013$n20_49_2013 + data2013$n50_99_2013 + data2013$n100_249_2013 + data2013$n250_499_2013 + data2013$n500_999_2013 + data2013$n1000_2013
data2013_JobsT <- aggregate(data2013$Jobs_2013, by=list(data2013$zip), sum)
names(data2013_JobsT) <- c("zip","JobsT_2013")
data2013_NARROW <- subset(data2013, naics=="334614" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512230" | naics=="512290" | naics=="515111" | naics=="515112" | naics=="519110" | naics=="519120" | naics=="541310" | naics=="541340" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712190" | naics=="713210" | naics=="713290" | naics=="713940" | naics=="713990")
data2013_JobsN <- aggregate(data2013_NARROW$Jobs_2013, by=list(data2013_NARROW$zip), sum)
names(data2013_JobsN) <- c("zip", "ArtsN_2013")
data2013_WIDE <- subset(data2013, naics=="323117" | naics=="334614" | naics=="339992" | naics=="443142" | naics=="451140" | naics=="451211" | naics=="443142" | naics=="453920" | naics=="487110" | naics=="487210" | naics=="487990" | naics=="511110" | naics=="511120" | naics=="511130" | naics=="512110" | naics=="512120" | naics=="512131" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512220" | naics=="512230" | naics=="512240" | naics=="512290" | naics=="515111" | naics=="515112" | naics=="515120" | naics=="515210" | naics=="519110" | naics=="519120" | naics=="532292" | naics=="532490" | naics=="541310" | naics=="541340" | naics=="541370" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="541830" | naics=="541840" | naics=="541850" | naics=="541870" | naics=="541890" | naics=="541921" | naics=="541922" | naics=="561591" | naics=="561920" | naics=="561990" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711211" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712120" | naics=="712130" | naics=="712190" | naics=="713110" | naics=="713120" | naics=="713210" | naics=="713290" | naics=="713920" | naics=="713940" | naics=="713990" | naics=="722511" | naics=="722513" | naics=="722514" | naics=="722515" | naics=="722310" | naics=="722320")
data2013_JobsW <- aggregate(data2013_WIDE$Jobs_2013, by=list(data2013_WIDE$zip), sum)
names(data2013_JobsW) <- c("zip", "ArtsW_2013")
data2013_Total <- merge(data2013_JobsT, data2013_JobsN, by="zip")
data2013 <- merge(data2013_Total, data2013_JobsW, by="zip")
data2013$ArtsN_2013[data2013$JobsT_2013 > 0 & is.na(data2013$ArtsN_2013)] <- 0 
data2013$ArtsW_2013[data2013$JobsT_2013 > 0 & is.na(data2013$ArtsW_2013)] <- 0 
data2013$JobsN_2013 <- data2013$JobsT_2013 - data2013$ArtsN_2013
data2013$JobsW_2013 <- data2013$JobsT_2013 - data2013$ArtsW_2013
data2013$ArtSupport_2013 <- data2013$ArtsW_2013 - data2013$ArtsN_2013
data <- merge(data, data2013, by="zip", all=TRUE)
keep(data, sure=TRUE)
##############DATA FOR 2014 ##############
data2014 <- read.csv("zbp2014detail.txt", header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")
colnames(data2014) <- tolower(colnames(data2014))
data2014$naics <- as.numeric(as.character(data2014$naics))
data2014 <- subset(data2014, naics > 0)
data2014$n1_4_2014 <- data2014$n1_4 * 3
data2014$n5_9_2014 <- data2014$n5_9 * 7
data2014$n10_19_2014 <- data2014$n10_19 * 15
data2014$n20_49_2014 <- data2014$n20_49 * 35
data2014$n50_99_2014 <- data2014$n50_99 * 75
data2014$n100_249_2014 <- data2014$n100_249 * 175
data2014$n250_499_2014 <- data2014$n250_499 * 375
data2014$n500_999_2014 <- data2014$n500_999 * 750
data2014$n1000_2014 <- data2014$n1000 * 2000
data2014$Jobs_2014 <- data2014$n1_4_2014 + data2014$n5_9_2014 + data2014$n10_19_2014 + data2014$n20_49_2014 + data2014$n50_99_2014 + data2014$n100_249_2014 + data2014$n250_499_2014 + data2014$n500_999_2014 + data2014$n1000_2014
data2014_JobsT <- aggregate(data2014$Jobs_2014, by=list(data2014$zip), sum)
names(data2014_JobsT) <- c("zip","JobsT_2014")
data2014_NARROW <- subset(data2014, naics=="334614" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512230" | naics=="512290" | naics=="515111" | naics=="515112" | naics=="519110" | naics=="519120" | naics=="541310" | naics=="541340" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712190" | naics=="713210" | naics=="713290" | naics=="713940" | naics=="713990")
data2014_JobsN <- aggregate(data2014_NARROW$Jobs_2014, by=list(data2014_NARROW$zip), sum)
names(data2014_JobsN) <- c("zip", "ArtsN_2014")
data2014_WIDE <- subset(data2014, naics=="323117" | naics=="334614" | naics=="339992" | naics=="443142" | naics=="451140" | naics=="451211" | naics=="443142" | naics=="453920" | naics=="487110" | naics=="487210" | naics=="487990" | naics=="511110" | naics=="511120" | naics=="511130" | naics=="512110" | naics=="512120" | naics=="512131" | naics=="512191" | naics=="512199" | naics=="512210" | naics=="512220" | naics=="512230" | naics=="512240" | naics=="512290" | naics=="515111" | naics=="515112" | naics=="515120" | naics=="515210" | naics=="519110" | naics=="519120" | naics=="532292" | naics=="532490" | naics=="541310" | naics=="541340" | naics=="541370" | naics=="541410" | naics=="541420" | naics=="541430" | naics=="541490" | naics=="541830" | naics=="541840" | naics=="541850" | naics=="541870" | naics=="541890" | naics=="541921" | naics=="541922" | naics=="561591" | naics=="561920" | naics=="561990" | naics=="611610" | naics=="611620" | naics=="711110" | naics=="711120" | naics=="711130" | naics=="711190" | naics=="711211" | naics=="711219" | naics=="711310" | naics=="711320" | naics=="711410" | naics=="711510" | naics=="712110" | naics=="712120" | naics=="712130" | naics=="712190" | naics=="713110" | naics=="713120" | naics=="713210" | naics=="713290" | naics=="713920" | naics=="713940" | naics=="713990" | naics=="722511" | naics=="722513" | naics=="722514" | naics=="722515" | naics=="722310" | naics=="722320")
data2014_JobsW <- aggregate(data2014_WIDE$Jobs_2014, by=list(data2014_WIDE$zip), sum)
names(data2014_JobsW) <- c("zip", "ArtsW_2014")
data2014_Total <- merge(data2014_JobsT, data2014_JobsN, by="zip")
data2014 <- merge(data2014_Total, data2014_JobsW, by="zip")
data2014$ArtsN_2014[data2014$JobsT_2014 > 0 & is.na(data2014$ArtsN_2014)] <- 0 
data2014$ArtsW_2014[data2014$JobsT_2014 > 0 & is.na(data2014$ArtsW_2014)] <- 0 
data2014$JobsN_2014 <- data2014$JobsT_2014 - data2014$ArtsN_2014
data2014$JobsW_2014 <- data2014$JobsT_2014 - data2014$ArtsW_2014
data2014$ArtSupport_2014 <- data2014$ArtsW_2014 - data2014$ArtsN_2014
data <- merge(data, data2014, by="zip", all=TRUE)
keep(data, sure=TRUE)
