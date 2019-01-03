library(textreadr)
library(stringi)
library(stringr)
library(magrittr)

#Disables scientific notation, 0 to turn it ON
options(scipen = 999)

#/////////////////
#/// Inventory ///
#/////////////////
useItemsID <- {c()}
useItemsName <- {c()}
setupItemsID <- {c()}
setupItemsName <- {c()}
etcItemsID <- {c()}
etcItemsName <- {c()}
cashItemsID <- {c()}
cashItemsName <- {c()}
pathToInputINV <- "D://v83_GM_Handbook/Inventory"
pathToOutputINV <- "D:/v83_GM_Handbook/Inventory/Out"

InventoryData <- textreadr::read_dir(pathToInputINV)
InventoryDataContent <- InventoryData$content
InventoryDataText <- as.vector(t(InventoryDataContent))
InventoryItemsID <- stri_match_first_regex(InventoryDataText, "[0-9]+") %>% as.numeric()
InventoryItemsNoID <- sub("[[:digit:]]+","",InventoryDataText)
InventoryItemsNoIDClean <- sub("-","",InventoryItemsNoID) %>% trimws()
InventoryItemsName <- stri_match_first_regex(InventoryItemsNoIDClean,".*? \\-")
InventoryItemsName <- sub(" \\-",'',InventoryItemsName)
InventoryItemsDF <- data.frame(InventoryItemsID, InventoryItemsName, stringsAsFactors = FALSE)
InventoryItemsDF <- InventoryItemsDF[order(InventoryItemsDF$InventoryItemsID),]

i <- 1
while (i < length(InventoryItemsDF$InventoryItemsID)) {
  if (is.na(InventoryItemsDF$InventoryItemsID[i]))   print(paste0("NA: ", InventoryItemsDF$InventoryItemsID[i]))
  if (is.na(InventoryItemsDF$InventoryItemsName[i])) print(paste0("NA: ",InventoryItemsDF$InventoryItemsName[i]))

  if (InventoryItemsDF$InventoryItemsID[i] >= 2000000 && InventoryItemsDF$InventoryItemsID[i] <= 2450000) {
    useItemsID[i]  <- InventoryItemsDF$InventoryItemsID[i]
    useItemsName[i] <- InventoryItemsDF$InventoryItemsName[i]
  } else if (InventoryItemsDF$InventoryItemsID[i] >= 3010000 && InventoryItemsDF$InventoryItemsID[i] <= 3995000) {
    setupItemsID[i]  <- InventoryItemsDF$InventoryItemsID[i]
    setupItemsName[i] <- InventoryItemsDF$InventoryItemsName[i]
  } else if (InventoryItemsDF$InventoryItemsID[i] >= 4000000 && InventoryItemsDF$InventoryItemsID[i] <= 4310000) {
    etcItemsID[i]  <- InventoryItemsDF$InventoryItemsID[i]
    etcItemsName[i] <- InventoryItemsDF$InventoryItemsName[i]
  } else if (InventoryItemsDF$InventoryItemsID[i] >= 5010000 && InventoryItemsDF$InventoryItemsID[i] <= 5990000) {
    cashItemsID[i]  <- InventoryItemsDF$InventoryItemsID[i]
    cashItemsName[i] <- InventoryItemsDF$InventoryItemsName[i]
  } else
    print(paste0("Ommited: ", InventoryItemsDF$InventoryItemsID[i]))
  i = i + 1
}

ifelse(!dir.exists(pathToOutputINV), dir.create(pathToOutputINV), FALSE)
setwd(pathToOutputINV)

useItemsID <- na.omit(useItemsID) %>% as.numeric()
useItemsName <- na.omit(useItemsName) %>% dQuote()
useItemsName <- paste("{", useItemsName)
useItemsDF <- data.frame(useItemsID, useItemsName, stringsAsFactors = FALSE)
useItemPair <- paste(useItemsDF$useItemsName, useItemsDF$useItemsID, sep = ", ") %>% noquote()
useItemPair <- paste(useItemPair,"},")
fileConn <- file("UseItems.txt", open = "w+")
header <- c('std::unordered_map<std::string, int> useItems({')
footer <- c('});')
cat(header, file = fileConn, append = TRUE, sep = "\n")
cat(useItemPair, file = fileConn, append = TRUE, sep = "\n")
cat(footer, file = fileConn, append = TRUE, sep = "\n")
close(fileConn)

setupItemsID <- na.omit(setupItemsID) %>% as.numeric()
setupItemsName <- na.omit(setupItemsName) %>% dQuote()
setupItemsName <- paste("{", setupItemsName)
setupItemsDF <- data.frame(setupItemsID, setupItemsName, stringsAsFactors = FALSE)
setupItemPair <- paste(setupItemsDF$setupItemsName, setupItemsDF$setupItemsID, sep = ", ") %>% noquote()
setupItemPair <- paste(setupItemPair,"},")
fileConn <- file("SetUpItems.txt", open = "w+")
header <- c('std::unordered_map<std::string, int> setupItems({')
footer <- c('});')
cat(header, file = fileConn, append = TRUE, sep = "\n")
cat(setupItemPair, file = fileConn, append = TRUE, sep = "\n")
cat(footer, file = fileConn, append = TRUE, sep = "\n")
close(fileConn)

etcItemsID <- na.omit(etcItemsID) %>% as.numeric()
etcItemsName <- na.omit(etcItemsName) %>% dQuote()
etcItemsName <- paste("{", etcItemsName)
etcItemsDF <- data.frame(etcItemsID, etcItemsName, stringsAsFactors = FALSE)
etcItemPair <- paste(etcItemsDF$etcItemsName, etcItemsDF$etcItemsID, sep = ", ") %>% noquote()
etcItemPair <- paste(etcItemPair,"},")
fileConn <- file("EtcItems.txt", open = "w+")
header <- c('std::unordered_map<std::string, int> etcItems({')
footer <- c('});')
cat(header, file = fileConn, append = TRUE, sep = "\n")
cat(etcItemPair, file = fileConn, append = TRUE, sep = "\n")
cat(footer, file = fileConn, append = TRUE, sep = "\n")
close(fileConn)

cashItemsID <- na.omit(cashItemsID) %>% as.numeric()
cashItemsName <- na.omit(cashItemsName) %>% dQuote()
cashItemsName <- paste("{", cashItemsName)
cashItemsDF <- data.frame(cashItemsID, cashItemsName, stringsAsFactors = FALSE)
cashItemPair <- paste(cashItemsDF$cashItemsName, cashItemsDF$cashItemsID, sep = ", ") %>% noquote()
cashItemPair <- paste(cashItemPair,"},")
fileConn <- file("CashItems.txt", open = "w+")
header <- c('std::unordered_map<std::string, int> cashItems({')
footer <- c('});')
cat(header, file = fileConn, append = TRUE, sep = "\n")
cat(cashItemPair, file = fileConn, append = TRUE, sep = "\n")
cat(footer, file = fileConn, append = TRUE, sep = "\n")
close(fileConn)

#/////////////
#/// Equip ///
#/////////////
faceOrLookID <- {c()}
faceOrLookName <- {c()}
hairsID <- {c()}
hairsName <- {c()}
hatID <- {c()}
hatName <- {c()}
faceAccID <- {c()}
faceAccName <- {c()}
eyeAccID <- {c()}
eyeAccName <- {c()}
earringID <- {c()}
earringName <- {c()}
topID <- {c()}
topName <- {c()}
bottomID <- {c()}
bottomName <- {c()}
shoesID <- {c()}
shoesName <- {c()}
glovesID <- {c()}
glovesName <- {c()}
shieldID <- {c()}
shieldName <- {c()}
capeID <- {c()}
capeName <- {c()}
ringID <- {c()}
ringName <- {c()}
necklaceID <- {c()}
necklaceName <- {c()}
beltID <- {c()}
beltName <- {c()}
medalID <- {c()}
medalName <- {c()}
weaponID <- {c()}
weaponName <- {c()}
skillEffectID <- {c()}
skillEffectName <- {c()}
cashWeaponID <- {c()}
cashWeaponName <- {c()}
petEquipID <- {c()}
petEquipName <- {c()}
mountID <- {c()}
mountName <- {c()}
pathToInputEQP <- "D://v83_GM_Handbook/Equip"
pathToOutputEQP <- "D://v83_GM_Handbook/Equip/Out"

EquipData <- textreadr::read_dir(pathToInputEQP)
EquipDataContent <- EquipData$content
EquipDataText <- as.vector(t(EquipDataContent))
EquipItemsID <- stri_match_first_regex(EquipDataText, "[0-9]+") %>% as.numeric()
LinesminusItemsID <- sub("[[:digit:]]+","",EquipDataText)
LinesminusItemsIDminus <- sub("-","",LinesminusItemsID) %>% trimws()
EquipItemsName <- stri_match_first_regex(LinesminusItemsIDminus,".*? \\-")
EquipItemsName <- sub(" \\-",'',EquipItemsName)
EquipItemsDF <- data.frame(EquipItemsID, EquipItemsName, stringsAsFactors = FALSE)
EquipItemsDF <- EquipItemsDF[order(EquipItemsDF$EquipItemsID),]

i <- 1
while (i < length(EquipItemsDF$EquipItemsID)) {
  if (is.na(EquipItemsDF$EquipItemsID[i]))   print(paste0("NA: ", EquipItemsDF$EquipItemsID[i]))
  if (is.na(EquipItemsDF$EquipItemsName[i])) print(paste0("NA: ",EquipItemsDF$EquipItemsName[i]))

  if (EquipItemsDF$EquipItemsID[i] >= 20000  && EquipItemsDF$EquipItemsID[i] <= 21826) {
    faceOrLookID[i]  <- EquipItemsDF$EquipItemsID[i]
    faceOrLookName[i] <- EquipItemsDF$EquipItemsName[i]
  } else if (EquipItemsDF$EquipItemsID[i] >= 30000 && EquipItemsDF$EquipItemsID[i] <= 34117) {
    hairsID[i]  <- EquipItemsDF$EquipItemsID[i]
    hairsName[i] <- EquipItemsDF$EquipItemsName[i]
  } else if (EquipItemsDF$EquipItemsID[i] >= 1000000 && EquipItemsDF$EquipItemsID[i] <= 1003073) {
    hatID[i]  <- EquipItemsDF$EquipItemsID[i]
    hatName[i] <- EquipItemsDF$EquipItemsName[i]
  } else if (EquipItemsDF$EquipItemsID[i] >= 1010000 && EquipItemsDF$EquipItemsID[i] <= 1012186) {
    faceAccID[i]  <- EquipItemsDF$EquipItemsID[i]
    faceAccName[i] <- EquipItemsDF$EquipItemsName[i]
  } else if (EquipItemsDF$EquipItemsID[i] >= 1020000 && EquipItemsDF$EquipItemsID[i] <= 1022104) {
    eyeAccID[i]  <- EquipItemsDF$EquipItemsID[i]
    eyeAccName[i] <- EquipItemsDF$EquipItemsName[i]
  } else if (EquipItemsDF$EquipItemsID[i] >= 1032000 && EquipItemsDF$EquipItemsID[i] <= 1032075) {
    earringID[i]  <- EquipItemsDF$EquipItemsID[i]
    earringName[i] <- EquipItemsDF$EquipItemsName[i]
  } else if (EquipItemsDF$EquipItemsID[i] >= 1040000 && EquipItemsDF$EquipItemsID[i] <= 1052234) {
    topID[i]  <- EquipItemsDF$EquipItemsID[i]
    topName[i] <- EquipItemsDF$EquipItemsName[i]
  } else if (EquipItemsDF$EquipItemsID[i] >= 1060000 && EquipItemsDF$EquipItemsID[i] <= 1062119) {
    bottomID[i]  <- EquipItemsDF$EquipItemsID[i]
    bottomName[i] <- EquipItemsDF$EquipItemsName[i]
  } else if (EquipItemsDF$EquipItemsID[i] >= 1070000 && EquipItemsDF$EquipItemsID[i] <= 1072437) {
    shoesID[i]  <- EquipItemsDF$EquipItemsID[i]
    shoesName[i] <- EquipItemsDF$EquipItemsName[i]
  } else if (EquipItemsDF$EquipItemsID[i] >= 1080000 && EquipItemsDF$EquipItemsID[i] <= 1082261) {
    glovesID[i]  <- EquipItemsDF$EquipItemsID[i]
    glovesName[i] <- EquipItemsDF$EquipItemsName[i]
  } else if (EquipItemsDF$EquipItemsID[i] >= 1092000 && EquipItemsDF$EquipItemsID[i] <= 1092062) {
    shieldID[i]  <- EquipItemsDF$EquipItemsID[i]
    shieldName[i] <- EquipItemsDF$EquipItemsName[i]
  } else if (EquipItemsDF$EquipItemsID[i] >= 1102000 && EquipItemsDF$EquipItemsID[i] <= 1102236) {
    capeID[i]  <- EquipItemsDF$EquipItemsID[i]
    capeName[i] <- EquipItemsDF$EquipItemsName[i]
  } else if (EquipItemsDF$EquipItemsID[i] >= 1112000 && EquipItemsDF$EquipItemsID[i] <= 1112916) {
    ringID[i]  <- EquipItemsDF$EquipItemsID[i]
    ringName[i] <- EquipItemsDF$EquipItemsName[i]
  } else if (EquipItemsDF$EquipItemsID[i] >= 1122000 && EquipItemsDF$EquipItemsID[i] <= 1122059) {
    necklaceID[i]  <- EquipItemsDF$EquipItemsID[i]
    necklaceName[i] <- EquipItemsDF$EquipItemsName[i]
  } else if (EquipItemsDF$EquipItemsID[i] >= 1132000 && EquipItemsDF$EquipItemsID[i] <= 1132016) {
    beltID[i]  <- EquipItemsDF$EquipItemsID[i]
    beltName[i] <- EquipItemsDF$EquipItemsName[i]
  } else if (EquipItemsDF$EquipItemsID[i] >= 1140000 && EquipItemsDF$EquipItemsID[i] <= 1142155) {
    medalID[i]  <- EquipItemsDF$EquipItemsID[i]
    medalName[i] <- EquipItemsDF$EquipItemsName[i]
  } else if (EquipItemsDF$EquipItemsID[i] >= 1302000 && EquipItemsDF$EquipItemsID[i] <= 1492048) {
    weaponID[i]  <- EquipItemsDF$EquipItemsID[i]
    weaponName[i] <- EquipItemsDF$EquipItemsName[i]
  } else if (EquipItemsDF$EquipItemsID[i] >= 1602000 && EquipItemsDF$EquipItemsID[i] <= 1602007) {
    skillEffectID[i]  <- EquipItemsDF$EquipItemsID[i]
    skillEffectName[i] <- EquipItemsDF$EquipItemsName[i]
  } else if (EquipItemsDF$EquipItemsID[i] >= 1702000 && EquipItemsDF$EquipItemsID[i] <= 1702251) {
    cashWeaponID[i]  <- EquipItemsDF$EquipItemsID[i]
    cashWeaponName[i] <- EquipItemsDF$EquipItemsName[i]
  } else if (EquipItemsDF$EquipItemsID[i] >= 1802000 && EquipItemsDF$EquipItemsID[i] <= 1832000) {
    petEquipID[i]  <- EquipItemsDF$EquipItemsID[i]
    petEquipName[i] <- EquipItemsDF$EquipItemsName[i]
    #1802100 - 1812007 pet eq main
    #1822000 - 1832000 pet eq rings
  } else if (EquipItemsDF$EquipItemsID[i] >= 1902000 && EquipItemsDF$EquipItemsID[i] <= 1912032) {
    mountID[i]  <- EquipItemsDF$EquipItemsID[i]
    mountName[i] <- EquipItemsDF$EquipItemsName[i]
  } else {
    print(paste0("Ommited: ", EquipItemsDF$EquipItemsID[i]))
  }
  i = i + 1
}

ifelse(!dir.exists(pathToOutputEQP), dir.create(pathToOutputEQP), FALSE)
setwd(pathToOutputEQP)

faceOrLookID <- na.omit(faceOrLookID) %>% as.numeric()
faceOrLookName <- na.omit(faceOrLookName) %>% dQuote()
faceOrLookName <- paste("{", faceOrLookName)
faceOrLookDF <- data.frame(faceOrLookID, faceOrLookName, stringsAsFactors = FALSE)
faceOrLookPair <- paste(faceOrLookDF$faceOrLookName, faceOrLookDF$faceOrLookID, sep = ", ") %>% noquote()
faceOrLookPair <- paste(faceOrLookPair,"},")
fileConn <- file("FaceOrLooks.txt", open = "w+")
header <- c('std::unordered_map<std::string, int> faceOrLookItems({')
footer <- c('});')
cat(header, file = fileConn, append = TRUE, sep = "\n")
cat(faceOrLookPair, file = fileConn, append = TRUE, sep = "\n")
cat(footer, file = fileConn, append = TRUE, sep = "\n")
close(fileConn)

hatID <- na.omit(hatID) %>% as.numeric()
hatName <- na.omit(hatName) %>% dQuote()
hatName <- paste("{", hatName)
hatDF <- data.frame(hatID, hatName, stringsAsFactors = FALSE)
hatPair <- paste(hatDF$hatName, hatDF$hatID, sep = ", ") %>% noquote()
hatPair <- paste(hatPair,"},")
fileConn <- file("Hats.txt", open = "w+")
header <- c('std::unordered_map<std::string, int> Hats({')
footer <- c('});')
cat(header, file = fileConn, append = TRUE, sep = "\n")
cat(hatPair, file = fileConn, append = TRUE, sep = "\n")
cat(footer, file = fileConn, append = TRUE, sep = "\n")
close(fileConn)

faceAccID <- na.omit(faceAccID) %>% as.numeric()
faceAccName <- na.omit(faceAccName) %>% dQuote()
faceAccName <- paste("{", faceAccName)
faceAccDF <- data.frame(faceAccID, faceAccName, stringsAsFactors = FALSE)
faceAccPair <- paste(faceAccDF$faceAccName, faceAccDF$faceAccID, sep = ", ") %>% noquote()
faceAccPair <- paste(faceAccPair,"},")
fileConn <- file("FaceAccs.txt", open = "w+")
header <- c('std::unordered_map<std::string, int> FaceAccessory({')
footer <- c('});')
cat(header, file = fileConn, append = TRUE, sep = "\n")
cat(faceAccPair, file = fileConn, append = TRUE, sep = "\n")
cat(footer, file = fileConn, append = TRUE, sep = "\n")
close(fileConn)

eyeAccID <- na.omit(eyeAccID) %>% as.numeric()
eyeAccName <- na.omit(eyeAccName) %>% dQuote()
eyeAccName <- paste("{", eyeAccName)
eyeAccDF <- data.frame(eyeAccID, eyeAccName, stringsAsFactors = FALSE)
eyeAccPair <- paste(eyeAccDF$eyeAccName, eyeAccDF$eyeAccID, sep = ", ") %>% noquote()
eyeAccPair <- paste(eyeAccPair,"},")
fileConn <- file("EyeAccs.txt", open = "w+")
header <- c('std::unordered_map<std::string, int> EyeAccessory({')
footer <- c('});')
cat(header, file = fileConn, append = TRUE, sep = "\n")
cat(eyeAccPair, file = fileConn, append = TRUE, sep = "\n")
cat(footer, file = fileConn, append = TRUE, sep = "\n")
close(fileConn)

earringID <- na.omit(earringID) %>% as.numeric()
earringName <- na.omit(earringName) %>% dQuote()
earringName <- paste("{", earringName)
earringDF <- data.frame(earringID, earringName, stringsAsFactors = FALSE)
earringPair <- paste(earringDF$earringName, earringDF$earringID, sep = ", ") %>% noquote()
earringPair <- paste(earringPair,"},")
fileConn <- file("Earrings.txt", open = "w+")
header <- c('std::unordered_map<std::string, int> Earrings({')
footer <- c('});')
cat(header, file = fileConn, append = TRUE, sep = "\n")
cat(earringPair, file = fileConn, append = TRUE, sep = "\n")
cat(footer, file = fileConn, append = TRUE, sep = "\n")
close(fileConn)

topID <- na.omit(topID) %>% as.numeric()
topName <- na.omit(topName) %>% dQuote()
topName <- paste("{", topName)
topDF <- data.frame(topID, topName, stringsAsFactors = FALSE)
topPair <- paste(topDF$topName, topDF$topID, sep = ", ") %>% noquote()
topPair <- paste(topPair,"},")
fileConn <- file("Tops.txt", open = "w+")
header <- c('std::unordered_map<std::string, int> Tops({')
footer <- c('});')
cat(header, file = fileConn, append = TRUE, sep = "\n")
cat(topPair, file = fileConn, append = TRUE, sep = "\n")
cat(footer, file = fileConn, append = TRUE, sep = "\n")
close(fileConn)

bottomID <- na.omit(bottomID) %>% as.numeric()
bottomName <- na.omit(bottomName) %>% dQuote()
bottomName <- paste("{", bottomName)
bottomDF <- data.frame(bottomID, bottomName, stringsAsFactors = FALSE)
bottomPair <- paste(bottomDF$bottomName, bottomDF$bottomID, sep = ", ") %>% noquote()
bottomPair <- paste(bottomPair,"},")
fileConn <- file("Bottoms.txt", open = "w+")
header <- c('std::unordered_map<std::string, int> Bottoms({')
footer <- c('});')
cat(header, file = fileConn, append = TRUE, sep = "\n")
cat(bottomPair, file = fileConn, append = TRUE, sep = "\n")
cat(footer, file = fileConn, append = TRUE, sep = "\n")
close(fileConn)

shoesID <- na.omit(shoesID) %>% as.numeric()
shoesName <- na.omit(shoesName) %>% dQuote()
shoesName <- paste("{", shoesName)
shoesDF <- data.frame(shoesID, shoesName, stringsAsFactors = FALSE)
shoesPair <- paste(shoesDF$shoesName, shoesDF$shoesID, sep = ", ") %>% noquote()
shoesPair <- paste(shoesPair,"},")
fileConn <- file("Shoes.txt", open = "w+")
header <- c('std::unordered_map<std::string, int> Shoes({')
footer <- c('});')
cat(header, file = fileConn, append = TRUE, sep = "\n")
cat(shoesPair, file = fileConn, append = TRUE, sep = "\n")
cat(footer, file = fileConn, append = TRUE, sep = "\n")
close(fileConn)

glovesID <- na.omit(glovesID) %>% as.numeric()
glovesName <- na.omit(glovesName) %>% dQuote()
glovesName <- paste("{", glovesName)
glovesDF <- data.frame(glovesID, glovesName, stringsAsFactors = FALSE)
glovesPair <- paste(glovesDF$glovesName, glovesDF$glovesID, sep = ", ") %>% noquote()
glovesPair <- paste(glovesPair,"},")
fileConn <- file("Gloves.txt", open = "w+")
header <- c('std::unordered_map<std::string, int> Gloves({')
footer <- c('});')
cat(header, file = fileConn, append = TRUE, sep = "\n")
cat(glovesPair, file = fileConn, append = TRUE, sep = "\n")
cat(footer, file = fileConn, append = TRUE, sep = "\n")
close(fileConn)

capeID <- na.omit(capeID) %>% as.numeric()
capeName <- na.omit(capeName) %>% dQuote()
capeName <- paste("{", capeName)
capeDF <- data.frame(capeID, capeName, stringsAsFactors = FALSE)
capePair <- paste(capeDF$capeName, capeDF$capeID, sep = ", ") %>% noquote()
capePair <- paste(capePair,"},")
fileConn <- file("Capes.txt", open = "w+")
header <- c('std::unordered_map<std::string, int> Capes({')
footer <- c('});')
cat(header, file = fileConn, append = TRUE, sep = "\n")
cat(capePair, file = fileConn, append = TRUE, sep = "\n")
cat(footer, file = fileConn, append = TRUE, sep = "\n")
close(fileConn)

shieldID <- na.omit(shieldID) %>% as.numeric()
shieldName <- na.omit(shieldName) %>% dQuote()
shieldName <- paste("{", shieldName)
shieldDF <- data.frame(shieldID, shieldName, stringsAsFactors = FALSE)
shieldPair <- paste(shieldDF$shieldName, shieldDF$shieldID, sep = ", ") %>% noquote()
shieldPair <- paste(shieldPair,"},")
fileConn <- file("Shields.txt", open = "w+")
header <- c('std::unordered_map<std::string, int> Shields({')
footer <- c('});')
cat(header, file = fileConn, append = TRUE, sep = "\n")
cat(shieldPair, file = fileConn, append = TRUE, sep = "\n")
cat(footer, file = fileConn, append = TRUE, sep = "\n")
close(fileConn)

weaponID <- na.omit(weaponID) %>% as.numeric()
weaponName <- na.omit(weaponName) %>% dQuote()
weaponName <- paste("{", weaponName)
weaponDF <- data.frame(weaponID, weaponName, stringsAsFactors = FALSE)
weaponPair <- paste(weaponDF$weaponName, weaponDF$weaponID, sep = ", ") %>% noquote()
weaponPair <- paste(weaponPair,"},")
fileConn <- file("Weapons.txt", open = "w+")
header <- c('std::unordered_map<std::string, int> Weapons({')
footer <- c('});')
cat(header, file = fileConn, append = TRUE, sep = "\n")
cat(weaponPair, file = fileConn, append = TRUE, sep = "\n")
cat(footer, file = fileConn, append = TRUE, sep = "\n")
close(fileConn)

ringID <- na.omit(ringID) %>% as.numeric()
ringName <- na.omit(ringName) %>% dQuote()
ringName <- paste("{", ringName)
ringDF <- data.frame(ringID, ringName, stringsAsFactors = FALSE)
ringPair <- paste(ringDF$ringName, ringDF$ringID, sep = ", ") %>% noquote()
ringPair <- paste(ringPair,"},")
fileConn <- file("Rings.txt", open = "w+")
header <- c('std::unordered_map<std::string, int> Rings({')
footer <- c('});')
cat(header, file = fileConn, append = TRUE, sep = "\n")
cat(ringPair, file = fileConn, append = TRUE, sep = "\n")
cat(footer, file = fileConn, append = TRUE, sep = "\n")
close(fileConn)

necklaceID <- na.omit(necklaceID) %>% as.numeric()
necklaceName <- na.omit(necklaceName) %>% dQuote()
necklaceName <- paste("{", necklaceName)
necklaceDF <- data.frame(necklaceID, necklaceName, stringsAsFactors = FALSE)
necklacePair <- paste(necklaceDF$necklaceName, necklaceDF$necklaceID, sep = ", ") %>% noquote()
necklacePair <- paste(necklacePair,"},")
fileConn <- file("Necklaces.txt", open = "w+")
header <- c('std::unordered_map<std::string, int> Necklaces({')
footer <- c('});')
cat(header, file = fileConn, append = TRUE, sep = "\n")
cat(necklacePair, file = fileConn, append = TRUE, sep = "\n")
cat(footer, file = fileConn, append = TRUE, sep = "\n")
close(fileConn)

beltID <- na.omit(beltID) %>% as.numeric()
beltName <- na.omit(beltName) %>% dQuote()
beltName <- paste("{", beltName)
beltDF <- data.frame(beltID, beltName, stringsAsFactors = FALSE)
beltPair <- paste(beltDF$beltName, beltDF$beltID, sep = ", ") %>% noquote()
beltPair <- paste(beltPair,"},")
fileConn <- file("Belts.txt", open = "w+")
header <- c('std::unordered_map<std::string, int> Belts({')
footer <- c('});')
cat(header, file = fileConn, append = TRUE, sep = "\n")
cat(beltPair, file = fileConn, append = TRUE, sep = "\n")
cat(footer, file = fileConn, append = TRUE, sep = "\n")
close(fileConn)

petEquipID <- na.omit(petEquipID) %>% as.numeric()
petEquipName <- na.omit(petEquipName) %>% dQuote()
petEquipName <- paste("{", petEquipName)
petEquipDF <- data.frame(petEquipID, petEquipName, stringsAsFactors = FALSE)
petEquipPair <- paste(petEquipDF$petEquipName, petEquipDF$petEquipID, sep = ", ") %>% noquote()
petEquipPair <- paste(petEquipPair,"},")
fileConn <- file("PetEquips.txt", open = "w+")
header <- c('std::unordered_map<std::string, int> PetEquips({')
footer <- c('});')
cat(header, file = fileConn, append = TRUE, sep = "\n")
cat(petEquipPair, file = fileConn, append = TRUE, sep = "\n")
cat(footer, file = fileConn, append = TRUE, sep = "\n")
close(fileConn)

mountID <- na.omit(mountID) %>% as.numeric()
mountName <- na.omit(mountName) %>% dQuote()
mountName <- paste("{", mountName)
mountsDF <- data.frame(mountID, mountName, stringsAsFactors = FALSE)
mountPair <- paste(mountsDF$mountName, mountsDF$mountID, sep = ", ") %>% noquote()
mountPair <- paste(mountPair,"},")
fileConn <- file("Mounts.txt", open = "w+")
header <- c('std::unordered_map<std::string, int> Mounts({')
footer <- c('});')
cat(header, file = fileConn, append = TRUE, sep = "\n")
cat(mountPair, file = fileConn, append = TRUE, sep = "\n")
cat(footer, file = fileConn, append = TRUE, sep = "\n")
close(fileConn)
