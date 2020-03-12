library(RODBC)
library(dplyr)
library(soiltexture)

setwd("T:/WP_T3_Case_studies_Biomass/substrat_deck/PET_RDA_rechnung") #set directory
mpa_xrd <- odbcConnect("mpa_xrd_results.accdb")

eingabeXRD <- sqlFetch(mpa_xrd, "eingabeXRD")#input values X-RAY POWDER DIFFRACTOMETRY
eingabeMPA <- sqlFetch(mpa_xrd, "eingabeMPA")#input values MANUAL PETROGRAPHIC ANALYSIS
gesteine <- sqlFetch(mpa_xrd, "gesteine")#mineral components of rocks (%)
odbcClose(mpa_xrd)

#calculate from percentage of sand, silt and clay the percentage of fine fraction
eingXRD_siebung <- eingabeXRD

eingXRD_siebung$feinanteil <- ifelse(is.na(eingXRD_siebung$feinanteil),eingXRD_siebung$sand_fein + eingXRD_siebung$silt_fein + eingXRD_siebung$ton_fein,eingXRD_siebung$feinanteil)

eingXRD_siebung[is.na(eingXRD_siebung)]<-0

#dreieck XRD ohne TM Aufwertung
eingXRD_siebung$dreieckohneTM <- ifelse((eingXRD_siebung$calcit + eingXRD_siebung$dolomit)>10,"D-2","D-3")
eingXRD_siebung$dreieckohneTM <- ifelse((eingXRD_siebung$calcit + eingXRD_siebung$dolomit)>75,"D-1",eingXRD_siebung$dreieckohneTM)

#calculation of illit
eingXRD_siebung$illit <- eingXRD_siebung$muskovit_illit/2
eingXRD_siebung$hell_glimmer <- eingXRD_siebung$muskovit_illit - eingXRD_siebung$illit

#CM weighted with fine fraction (Feinanteil)
eingXRD_siebung$TMdurchFA <- ((1+(eingXRD_siebung$feinanteil)/100))*(eingXRD_siebung$vermiculit_smectit_calc + eingXRD_siebung$illit + eingXRD_siebung$chlorit)
#summe
eingXRD_siebung$summe <- eingXRD_siebung$quarz + eingXRD_siebung$alkali_feldspat + eingXRD_siebung$albit + eingXRD_siebung$calcit + eingXRD_siebung$dolomit + eingXRD_siebung$muskovit_illit + eingXRD_siebung$parag_werte + eingXRD_siebung$amphibol_calc_biotit + eingXRD_siebung$vermiculit_smectit_calc + eingXRD_siebung$illit + eingXRD_siebung$chlorit + eingXRD_siebung$kaolinit + eingXRD_siebung$hydrobiotit
#Sum after weighing the FA 
eingXRD_siebung$summe_percTMFA <- eingXRD_siebung$TMdurchFA - (eingXRD_siebung$vermiculit_smectit_calc + eingXRD_siebung$illit + eingXRD_siebung$chlorit) + eingXRD_siebung$summe + eingXRD_siebung$chlorit
#K%
eingXRD_siebung$K_perc <- (eingXRD_siebung$calcit/eingXRD_siebung$summe_percTMFA)*100
#D%
eingXRD_siebung$D_perc <- (eingXRD_siebung$dolomit/eingXRD_siebung$summe_percTMFA)*100
#triangle XRD with weighted CM
eingXRD_siebung$dreieckmitTM <- ifelse((eingXRD_siebung$K_perc + eingXRD_siebung$D_perc)>10,"D-2","D-3")
eingXRD_siebung$dreieckmitTM <- ifelse((eingXRD_siebung$K_perc + eingXRD_siebung$D_perc)>75,"D-1",eingXRD_siebung$dreieckmitTM)
#FM_perc
eingXRD_siebung$FM_perc <- ((eingXRD_siebung$quarz + eingXRD_siebung$alkali_feldspat + eingXRD_siebung$albit + eingXRD_siebung$muskovit_illit + eingXRD_siebung$parag_werte + eingXRD_siebung$kaolinit)/eingXRD_siebung$summe_percTMFA)*100
#MM_perc
eingXRD_siebung$MM_perc <- ((eingXRD_siebung$amphibol_calc_biotit + eingXRD_siebung$chlorit + eingXRD_siebung$hydrobiotit)/eingXRD_siebung$summe_percTMFA)*100
#CM_perc
eingXRD_siebung$CM_perc <- (eingXRD_siebung$TMdurchFA/eingXRD_siebung$summe_percTMFA)*100
#kontrolle_proz
eingXRD_siebung$kontrolle <- eingXRD_siebung$FM_perc + eingXRD_siebung$MM_perc + eingXRD_siebung$CM_perc + eingXRD_siebung$K_perc + eingXRD_siebung$D_perc

#save the table eingRDA in the database
#mpa_xrd <- odbcConnect("mpa_xrd_github.accdb")

#eingabeXRD_complete <- sqlSave(channel= ..., dat= eingXRD_siebung, tablename= "calc_XRD")

#odbcClose(mpa_xrd)

#creation data table mpa+xrd-3eck

MPA_XRD_3eck <- data.frame(eingXRD_siebung$probeNR, eingXRD_siebung$dreieckmitTM, eingXRD_siebung$gruppe)
names(MPA_XRD_3eck) <- c("ProbeNR","XRD","gruppe")
MPA_XRD_3eck$XRD <- as.character(MPA_XRD_3eck$XRD)
MPA_XRD_3eck$gruppe <- as.character(MPA_XRD_3eck$gruppe)

ProbeNR <- MPA_XRD_3eck$ProbeNR

eingabeMPA[is.na(eingabeMPA)]<-0

for (i in ProbeNR){
  MPA_XRD_3eck[MPA_XRD_3eck$ProbeNR %in% i,'K_MPA'] <- crossprod(as.numeric(eingabeMPA[eingabeMPA$ProbeNR %in% c("K"),5:96]),as.numeric(eingabeMPA[eingabeMPA$ProbeNR %in% i,5:96]))}
#pet_d
for (i in ProbeNR){
  MPA_XRD_3eck[MPA_XRD_3eck$ProbeNR %in% i,'D_MPA'] <- crossprod(as.numeric(eingabeMPA[eingabeMPA$ProbeNR %in% c("D"),5:96]),as.numeric(eingabeMPA[eingabeMPA$ProbeNR %in% i,5:96]))}
#pet_SS
for (i in ProbeNR){
  MPA_XRD_3eck[MPA_XRD_3eck$ProbeNR %in% i,'FM_MPA'] <- crossprod(as.numeric(eingabeMPA[eingabeMPA$ProbeNR %in% c("FM"),5:96]),as.numeric(eingabeMPA[eingabeMPA$ProbeNR %in% i,5:96]))}
#pet_BS
for (i in ProbeNR){
  MPA_XRD_3eck[MPA_XRD_3eck$ProbeNR %in% i,'MM_MPA'] <- crossprod(as.numeric(eingabeMPA[eingabeMPA$ProbeNR %in% c("MM"),5:96]),as.numeric(eingabeMPA[eingabeMPA$ProbeNR %in% i,5:96]))}
#pet_TM
for (i in ProbeNR){
  MPA_XRD_3eck[MPA_XRD_3eck$ProbeNR %in% i,'CM_MPA'] <- crossprod(as.numeric(eingabeMPA[eingabeMPA$ProbeNR %in% c("CM"),5:96]),as.numeric(eingabeMPA[eingabeMPA$ProbeNR %in% i,5:96]))}


#summe der Teile
MPA_XRD_3eck$summeTeile <- rowSums(MPA_XRD_3eck[,4:8])
#when multiple rows and rowSums(pet_rda_3eck[,3:7])=0, summeTeile=1 default
MPA_XRD_3eck$summeTeile <- ifelse(MPA_XRD_3eck$summeTeile == 0, summeTeile <- 1, MPA_XRD_3eck$summeTeile)

#MPA triangle
MPA_XRD_3eck$MPA <- ifelse((MPA_XRD_3eck$K_MPA + MPA_XRD_3eck$D_MPA)/MPA_XRD_3eck$summeTeile> 0.1, "D-2", "D-3" )
MPA_XRD_3eck$MPA <- ifelse((MPA_XRD_3eck$K_MPA + MPA_XRD_3eck$D_MPA)/MPA_XRD_3eck$summeTeile> 0.75, "D-1", MPA_XRD_3eck$MPA)

MPA_XRD_3eck <- merge(x=MPA_XRD_3eck, y=eingXRD_siebung[,c("probeNR","D_perc","K_perc","FM_perc","MM_perc","CM_perc")], by.x="ProbeNR", by.y="probeNR")

#coordinates all triangles XRD
#XRD_dreieck1_K
MPA_XRD_3eck$K_dr1_XRD <- ifelse(MPA_XRD_3eck$XRD =="D-1",MPA_XRD_3eck$K_perc, "0")
MPA_XRD_3eck$K_dr1_XRD <- as.numeric(as.character(MPA_XRD_3eck$K_dr1_XRD))
#XRD_dreieck1_D
MPA_XRD_3eck$D_dr1_XRD <- ifelse(MPA_XRD_3eck$XRD=="D-1",MPA_XRD_3eck$D_perc, "0")
MPA_XRD_3eck$D_dr1_XRD <- as.numeric(as.character(MPA_XRD_3eck$D_dr1_XRD))
#XRD_dreieck1_FM_MM_CM
MPA_XRD_3eck$FM_MM_CM_dr1_XRD <- ifelse(MPA_XRD_3eck$XRD=="D-1",MPA_XRD_3eck$FM_perc+MPA_XRD_3eck$MM_perc+MPA_XRD_3eck$CM_perc, "0")
MPA_XRD_3eck$FM_MM_CM_dr1_XRD <- as.numeric(as.character(MPA_XRD_3eck$FM_MM_CM_dr1_XRD))

#XRD_dreieck2_FM
MPA_XRD_3eck$FM_dr2_XRD <- ifelse(MPA_XRD_3eck$XRD=="D-2", MPA_XRD_3eck$FM_perc, "0")
MPA_XRD_3eck$FM_dr2_XRD <- as.numeric(as.character(MPA_XRD_3eck$FM_dr2_XRD))
#XRD_dreieck2_K+D
MPA_XRD_3eck$K_D_dr2_XRD <- ifelse(MPA_XRD_3eck$XRD=="D-2", MPA_XRD_3eck$K_perc + MPA_XRD_3eck$D_perc, "0")
MPA_XRD_3eck$K_D_dr2_XRD <- as.numeric(as.character(MPA_XRD_3eck$K_D_dr2_XRD))
#XRD_dreieck2_CM+MM
MPA_XRD_3eck$CM_MM_dr2_XRD <- as.numeric(ifelse(MPA_XRD_3eck$XRD=="D-2", MPA_XRD_3eck$MM_perc + MPA_XRD_3eck$CM_perc, "0"))
MPA_XRD_3eck$CM_MM_dr2_XRD <- as.numeric(as.character(MPA_XRD_3eck$CM_MM_dr2_XRD))

#XRD_dreieck3_MM
MPA_XRD_3eck$MM_dr3_XRD <- ifelse(MPA_XRD_3eck$XRD=="D-3", MPA_XRD_3eck$MM_perc+MPA_XRD_3eck$K_perc+MPA_XRD_3eck$D_perc, 0)
MPA_XRD_3eck$MM_dr3_XRD <- as.numeric(as.character(MPA_XRD_3eck$MM_dr3_XRD))
#XRD_dreieck3_FM
MPA_XRD_3eck$FM_dr3_XRD <- ifelse(MPA_XRD_3eck$XRD=="D-3", MPA_XRD_3eck$FM_perc, "0")
MPA_XRD_3eck$FM_dr3_XRD <- as.numeric(as.character(MPA_XRD_3eck$FM_dr3_XRD))
#XRD_dreieck3_CM
MPA_XRD_3eck$CM_dr3_XRD <- ifelse(MPA_XRD_3eck$XRD=="D-3", MPA_XRD_3eck$CM_perc, "0")
MPA_XRD_3eck$CM_dr3_XRD <- as.numeric(as.character(MPA_XRD_3eck$CM_dr3_XRD))

#FA_fein_proz
for (i in ProbeNR){
  MPA_XRD_3eck[MPA_XRD_3eck$ProbeNR %in% i,"FA_fein_proz"] <- eingXRD_siebung[eingXRD_siebung$probeNR %in% i, "feinanteil"]}
#CMdurchFA
MPA_XRD_3eck$CMdurchFA <- MPA_XRD_3eck$CM_MPA*(1+(MPA_XRD_3eck$FA_fein_proz/100))
#Summe aller Teile und CM durch FA aufgewertet
MPA_XRD_3eck$sumalleTeile_CM <- MPA_XRD_3eck$CMdurchFA + MPA_XRD_3eck$FM_MPA + MPA_XRD_3eck$MM_MPA + MPA_XRD_3eck$K_MPA + MPA_XRD_3eck$D_MPA

#coordinates 3 triangles MPA
#MPA_dreieck1_K
MPA_XRD_3eck$K_dr1_MPA <- ifelse(MPA_XRD_3eck$MPA=="D-1",MPA_XRD_3eck$K_MPA/MPA_XRD_3eck$sumalleTeile_CM*100, "0")
MPA_XRD_3eck$K_dr1_MPA <- as.numeric(as.character(MPA_XRD_3eck$K_dr1_MPA))
#MPA_dreieck1_D
MPA_XRD_3eck$D_dr1_MPA <- ifelse(MPA_XRD_3eck$MPA=="D-1",MPA_XRD_3eck$D_MPA/MPA_XRD_3eck$sumalleTeile_CM*100, "0")
MPA_XRD_3eck$D_dr1_MPA <- as.numeric(as.character(MPA_XRD_3eck$D_dr1_MPA))
#MPA_dreieck1_FM_CM_MM
MPA_XRD_3eck$FM_MM_CM_dr1_MPA <- ifelse(MPA_XRD_3eck$MPA=="D-1",(MPA_XRD_3eck$MM_MPA+MPA_XRD_3eck$CMdurchFA+MPA_XRD_3eck$FM_MPA)/MPA_XRD_3eck$sumalleTeile_CM*100, "0")
MPA_XRD_3eck$FM_MM_CM_dr1_MPA <- as.numeric(as.character(MPA_XRD_3eck$FM_MM_CM_dr1_MPA))


#MPA_dreieck3_MM
MPA_XRD_3eck$MM_dr3_MPA <- ifelse(MPA_XRD_3eck$MPA=="D-3", ((MPA_XRD_3eck$MM_MPA+MPA_XRD_3eck$K_MPA+MPA_XRD_3eck$D_MPA)/MPA_XRD_3eck$sumalleTeile_CM)*100, "0")
MPA_XRD_3eck$MM_dr3_MPA <- as.numeric(as.character(MPA_XRD_3eck$MM_dr3_MPA))
#MPA_dreieck3_FM
MPA_XRD_3eck$FM_dr3_MPA <- ifelse(MPA_XRD_3eck$MPA=="D-3", (MPA_XRD_3eck$FM_MPA/MPA_XRD_3eck$sumalleTeile_CM)*100, "0")
MPA_XRD_3eck$FM_dr3_MPA <- as.numeric(as.character(MPA_XRD_3eck$FM_dr3_MPA))
#MPA_dreieck3_CM
MPA_XRD_3eck$CM_dr3_MPA <- ifelse(MPA_XRD_3eck$MPA=="D-3", (MPA_XRD_3eck$CMdurchFA/MPA_XRD_3eck$sumalleTeile_CM)*100, "0")
MPA_XRD_3eck$CM_dr3_MPA <- as.numeric(as.character(MPA_XRD_3eck$CM_dr3_MPA))

#MPA_dreieck2_FM
MPA_XRD_3eck$FM_dr2_MPA <- ifelse(MPA_XRD_3eck$MPA=="D-2", (MPA_XRD_3eck$FM_MPA/MPA_XRD_3eck$sumalleTeile_CM)*100, "0")
MPA_XRD_3eck$FM_dr2_MPA <- as.numeric(as.character(MPA_XRD_3eck$FM_dr2_MPA))
#MPA_dreieck2_K_D
MPA_XRD_3eck$K_D_dr2_MPA <- ifelse(MPA_XRD_3eck$MPA=="D-2", ((MPA_XRD_3eck$D_MPA+MPA_XRD_3eck$K_MPA)/MPA_XRD_3eck$sumalleTeile_CM)*100, "0")
MPA_XRD_3eck$K_D_dr2_MPA <- as.numeric(as.character(MPA_XRD_3eck$K_D_dr2_MPA))
#MPA_dreieck2_CM_MM
MPA_XRD_3eck$CM_MM_dr2_MPA <- ifelse(MPA_XRD_3eck$MPA=="D-2", ((MPA_XRD_3eck$MM_MPA+MPA_XRD_3eck$CMdurchFA)/MPA_XRD_3eck$sumalleTeile_CM)*100, "0")
MPA_XRD_3eck$CM_MM_dr2_MPA <- as.numeric(as.character(MPA_XRD_3eck$CM_MM_dr2_MPA))

MPA_XRD_3eck$MM_proz_MPA <- (MPA_XRD_3eck$MM_MPA/MPA_XRD_3eck$sumalleTeile_CM)*100
MPA_XRD_3eck$FM_proz_MPA <- (MPA_XRD_3eck$FM_MPA/MPA_XRD_3eck$sumalleTeile_CM)*100
MPA_XRD_3eck$CM_proz_MPA <- (MPA_XRD_3eck$CMdurchFA/MPA_XRD_3eck$sumalleTeile_CM)*100
MPA_XRD_3eck$K_proz_MPA <- (MPA_XRD_3eck$K_MPA/MPA_XRD_3eck$sumalleTeile_CM)*100
MPA_XRD_3eck$D_proz_MPA <- (MPA_XRD_3eck$D_MPA/MPA_XRD_3eck$sumalleTeile_CM)*100


#weighing factors for particle size distribution
eingabeXRD$kies_w <- eingabeXRD$kies_fein*1
eingabeXRD$sand_w <- eingabeXRD$sand_fein*1
eingabeXRD$Gsilt_w <- eingabeXRD$Gsilt*1
eingabeXRD$Msilt_w <- eingabeXRD$Msilt*1.5
eingabeXRD$Fsilt_w <- eingabeXRD$Fsilt*2.5
eingabeXRD$ton_w <- eingabeXRD$ton*5

eingabeXRD$sum_w <- eingabeXRD$kies_w + eingabeXRD$sand_w + eingabeXRD$Gsilt_w + eingabeXRD$Msilt_w + eingabeXRD$Fsilt_w + eingabeXRD$ton_w
eingabeXRD$ff_w <- (eingabeXRD$sand_w/eingabeXRD$sum_w)+(eingabeXRD$Gsilt_w/eingabeXRD$sum_w)+(eingabeXRD$Msilt_w/eingabeXRD$sum_w) +(eingabeXRD$Fsilt_w/eingabeXRD$sum_w)+(eingabeXRD$ton_w/eingabeXRD$sum_w)

MPA_XRD_3eck <- merge(x=MPA_XRD_3eck, y=eingabeXRD[,c("probeNR","ff_w")], by.x="ProbeNR", by.y="probeNR")

#save the table MPA_XRD_3eck in the database
#mpa_xrd <- odbcConnect("mpa_xrd_results.accdb")

#eingabeXRD_complete <- sqlSave(channel= mpa_xrd, dat= MPA_XRD_3eck, tablename= "mpa_xrd_3eck")

#odbcClose(mpa_xrd)

#for samples which have both XRD and MPA plotted in the same triangle
D1 <- MPA_XRD_3eck[which(MPA_XRD_3eck$XRD == "D-1" & MPA_XRD_3eck$MPA == "D-1"),]
D2 <- MPA_XRD_3eck[which(MPA_XRD_3eck$XRD == "D-2" & MPA_XRD_3eck$MPA == "D-2"),]
D3 <- MPA_XRD_3eck[which(MPA_XRD_3eck$XRD == "D-3" & MPA_XRD_3eck$MPA == "D-3"),]

#for samples which have both XRD and MPA plotted in triangle 1
D1$K_fact <- (abs(D1$K_dr1_XRD - D1$K_dr1_MPA))*D1$ff_w
D1$K_dr1 <- ifelse(D1$K_dr1_XRD > D1$K_dr1_MPA, D1$K_fact + D1$K_dr1_MPA, D1$K_dr1_MPA - D1$K_fact)

D1$D_fact <- (abs(D1$D_dr1_XRD - D1$D_dr1_MPA))*D1$ff_w
D1$D_dr1 <- ifelse(D1$D_dr1_XRD > D1$D_dr1_MPA, D1$D_fact + D1$D_dr1_MPA, D1$D_dr1_MPA - D1$D_fact)

D1$FM_MM_CM_fact <- (abs(D1$FM_MM_CM_dr1_XRD - D1$FM_MM_CM_dr1_MPA))*D1$ff_w
D1$FM_MM_CM_dr1 <- ifelse(D1$FM_MM_CM_dr1_XRD > D1$FM_MM_CM_dr1_MPA, D1$FM_MM_CM_fact + D1$FM_MM_CM_dr1_MPA, D1$FM_MM_CM_dr1_MPA - D1$FM_MM_CM_fact)

#for samples which have both XRD and MPA plotted in triangle 2
D2$K_D_fact <- (abs(D2$K_D_dr2_XRD - D2$K_D_dr2_MPA))*D2$ff_w
D2$K_D_dr2 <- ifelse(D2$K_D_dr2_XRD > D2$K_D_dr2_MPA, D2$K_D_fact + D2$K_D_dr2_MPA, D2$K_D_dr2_MPA - D2$K_D_fact)

D2$CM_MM_fact <- (abs(D2$CM_MM_dr2_XRD - D2$CM_MM_dr2_MPA))*D2$ff_w
D2$CM_MM_dr2 <- ifelse(D2$CM_MM_dr2_XRD > D2$CM_MM_dr2_MPA, D2$CM_MM_fact + D2$CM_MM_dr2_MPA, D2$CM_MM_dr2_MPA - D2$CM_MM_fact)

D2$FM_fact <- (abs(D2$FM_dr2_XRD - D2$FM_dr2_MPA))*D2$ff_w
D2$FM_dr2 <- ifelse(D2$FM_dr2_XRD > D2$FM_dr2_MPA, D2$FM_fact + D2$FM_dr2_MPA, D2$FM_dr2_MPA - D2$FM_fact)

#for samples which have both XRD and MPA plotted in triangle 3
D3$MM_fact <- (abs(D3$MM_dr3_XRD - D3$MM_dr3_MPA))*D3$ff_w
D3$MM_dr3 <- ifelse(D3$MM_dr3_XRD > D3$MM_dr3_MPA, D3$MM_fact + D3$MM_dr3_MPA, D3$MM_dr3_MPA - D3$MM_fact)

D3$FM_fact <- (abs(D3$FM_dr3_XRD - D3$FM_dr3_MPA))*D3$ff_w
D3$FM_dr3 <- ifelse(D3$FM_dr3_XRD > D3$FM_dr3_MPA, D3$FM_fact + D3$FM_dr3_MPA, D3$FM_dr3_MPA - D3$FM_fact)

D3$CM_fact <- (abs(D3$CM_dr3_XRD - D3$CM_dr3_MPA))*D3$ff_w
D3$CM_dr3 <- ifelse(D3$CM_dr3_XRD > D3$CM_dr3_MPA, D3$CM_fact + D3$CM_dr3_MPA, D3$CM_dr3_MPA - D3$CM_fact)

#triangle1
for (i in D1$ProbeNR){
load(file="dreieck1.rda")#add directory path to file
TT.set(reset=TRUE)
TT.add("tas.TT"= tas)

mix <- data.frame(
  "z"=2,
  "Dolomit" = D1[D1$ProbeNR %in% i, "D_dr1"],
  "TM_Silikate" = D1[D1$ProbeNR %in% i, "FM_MM_CM_dr1"],
  "Kalk" = D1[D1$ProbeNR %in% i, "K_dr1"])
XRD <- data.frame(
  "z"=1,
  "Dolomit" = D1[D1$ProbeNR %in% i, "D_dr1_XRD"],
  "TM_Silikate" = D1[D1$ProbeNR %in% i, "FM_MM_CM_dr1_XRD"],
  "Kalk" = D1[D1$ProbeNR %in% i, "K_dr1_XRD"])
MPA <- data.frame(
  "z"=1.5,
  "Dolomit" = D1[D1$ProbeNR %in% i, "D_dr1_MPA"],
  "TM_Silikate" = D1[D1$ProbeNR %in% i, "FM_MM_CM_dr1_MPA"],
  "Kalk" = D1[D1$ProbeNR %in% i, "K_dr1_MPA"])

alle <- rbind(mix,XRD,MPA)

setwd("")#add directory path to folder
png(file=paste0(i,".png"), width=1500,height=1500, res=140, pointsize=8)
D1T <- TT.plot( class.sys = "tas.TT",
                css.names=c("Dolomit","TM_Silikate","Kalk"),
                css.lab = c("D","FM+MM+CM","K"),
                main="",
                cex.axis=3.5,
                cex.lab=3.5,
                cex=3.5,
                lwd.axis=0.5,
                lwd.lab = 0.8,
                class.p.bg.col=c("gray61","gray68","gray75","deepskyblue","deepskyblue1","deepskyblue2","cyan1","cyan2","cyan3","aquamarine1","aquamarine2","aquamarine3","seagreen1","seagreen2","mistyrose1","mistyrose2")
)
TT.points(tri.data = alle, geo= D1T, css.names=c("Dolomit","TM_Silikate","Kalk"), #z.name="z",
          cex= c(6),
          pch=c(15,17,19))

legend("topright", c("XRD","MPA","resulting"),
       pt.cex = c(4,4,4),
       xjust=4, yjust=1,
       cex=4,
       pch = c(17,19,15), col=c("black"), bg="white")
dev.off()}

#triangle3
for (i in D3$ProbeNR){
load(file="dreieck3.rda")#add directory path to file
TT.set(reset=TRUE)
TT.add("tap.TT"= tap)

mix <- data.frame(
  "z"=2,
  "QF_Mu" = D3[D3$ProbeNR %in% i, "FM_dr3"],
  "TM" = D3[D3$ProbeNR %in% i, "CM_dr3"],
  "BS" = D3[D3$ProbeNR %in% i, "MM_dr3"])
XRD <- data.frame(
  "z"=1,
  "QF_Mu" = D3[D3$ProbeNR %in% i, "FM_dr3_XRD"],
  "TM" = D3[D3$ProbeNR %in% i, "CM_dr3_XRD"],
  "BS" = D3[D3$ProbeNR %in% i, "MM_dr3_XRD"])
MPA <- data.frame(
  "z"=1.5,
  "QF_Mu" = D3[D3$ProbeNR %in% i, "FM_dr3_MPA"],
  "TM" = D3[D3$ProbeNR %in% i, "CM_dr3_MPA"],
  "BS" = D3[D3$ProbeNR %in% i, "MM_dr3_MPA"])

alle <- rbind(mix,XRD,MPA)

setwd("")#add directory path to folder
png(file=paste0(i,".png"), width=1500,height=1500, res=140, pointsize=8)
D1T <- TT.plot( class.sys = "tap.TT",
                css.names=c("QF_Mu","TM","BS"),
                css.lab = c("FM","CM","D+K+MM"),
                main="",
                cex.axis=3.5,
                cex.lab=3.5,
                cex=3.5,
                lwd.lab = 0.8,
                lwd.axis=0.5,
                class.p.bg.col=c("coral1","coral2","mistyrose1","mistyrose2","olivedrab1","olivedrab2","olivedrab3","mistyrose3","magenta")
)
TT.points(tri.data = alle, geo= D1T, css.names=c("QF_Mu","TM","BS"), #z.name="z",
          cex= c(6),
          pch=c(15,17,19))

legend("topright", c("XRD","MPA","resulting"),
       pt.cex = c(4,4,4),
       xjust=4, yjust=1,
       cex=4,
       pch = c(17,19,15), col=c("black"), bg="white")
dev.off()}

#triangle 2
for (i in D2$ProbeNR){
load(file="dreieck2.rda")#add directory path to file
TT.set(reset=TRUE)
TT.add("tat.TT"= tat)

mix <- data.frame(
  "z"=2,
  "Karbonat" = D2[D2$ProbeNR %in% i, "K_D_dr2"],
  "TM_BS" = D2[D2$ProbeNR %in% i, "CM_MM_dr2"],
  "SS" = D2[D2$ProbeNR %in% i, "FM_dr2"])
XRD <- data.frame(
  "z"=1,
  "Karbonat" = D2[D2$ProbeNR %in% i, "K_D_dr2_XRD"],
  "TM_BS" = D2[D2$ProbeNR %in% i, "CM_MM_dr2_XRD"],
  "SS" = D2[D2$ProbeNR %in% i, "FM_dr2_XRD"])
MPA <- data.frame(
  "z"=1.5,
  "Karbonat" = D2[D2$ProbeNR %in% i, "K_D_dr2_MPA"],
  "TM_BS" = D2[D2$ProbeNR %in% i, "CM_MM_dr2_MPA"],
  "SS" = D2[D2$ProbeNR %in% i, "FM_dr2_MPA"])

alle <- rbind(mix,XRD,MPA)

setwd("")#add directory path
png(file=paste0(i,".png"), width=1500,height=1500, res=140, pointsize=8)
D1T <- TT.plot( class.sys = "tat.TT",
                css.names=c("Karbonat","TM_BS","SS"),
                css.lab = c("D+K","MM+CM","FM"),
                main="",
                cex.axis=3.5,
                cex.lab=3.5,
                cex=3.5,
                lwd.lab = 0.8,
                lwd.axis=0.5,
                class.p.bg.col=c("lightblue","deepskyblue1","deepskyblue3","cyan1","cyan2","cyan3","aquamarine1","aquamarine2", "aquamarine3", "coral1","mistyrose1","mistyrose2" )
)
TT.points(tri.data = alle, geo= D1T, css.names=c("Karbonat","TM_BS","SS"), #z.name="z",
          cex= c(6),
          pch=c(15,17,19))

legend("topright", c("XRD","MPA","resulting"),
       pt.cex = c(4,4,4),
       xjust=4, yjust=1,
       cex=4,
       pch = c(17,19,15), col=c("black"), bg="white")
dev.off()}
############################################################################################################################################
############################################################################################################################################
#start calculations for points plotting in two different triangles: 
#choose one triangle, the one where XRD or MPA have lower carbonate content and plot both points there
#these points will be plotted either in triangle 2 or 3
DW <- MPA_XRD_3eck[!MPA_XRD_3eck$ProbeNR %in% D1$ProbeNR,]
DW <- DW[!DW$ProbeNR %in% D2$ProbeNR,]
DW <- DW[!DW$ProbeNR %in% D3$ProbeNR,]

DMpoints <- split(DW, list(DW$XRD, DW$MPA))
#plot all points in triangle 2
D12 <- as.vector(DMpoints$`D-1.D-2`$ProbeNR)
D21 <- as.vector(DMpoints$`D-2.D-1`$ProbeNR)
ProbeTR2 <- c(D12, D21)

DW$K_D_XRD <- DW$K_perc + DW$D_perc
DW$K_D_MPA <- DW$K_proz_MPA + DW$D_proz_MPA
DW$K_D_fact <- (abs(DW$K_D_XRD - DW$K_D_MPA))*DW$ff_w
DW$K_D_dr2 <- ifelse(DW$K_D_XRD > DW$K_D_MPA, DW$K_D_fact + DW$K_D_MPA, DW$K_D_MPA - DW$K_D_fact)

DW$CM_MM_XRD <- DW$CM_perc + DW$MM_perc
DW$CM_MM_MPA <- DW$CM_proz_MPA + DW$MM_proz_MPA
DW$CM_MM_fact <- (abs(DW$CM_MM_XRD - DW$CM_MM_MPA))*DW$ff_w
DW$CM_MM_dr2 <- ifelse(DW$CM_MM_XRD > DW$CM_MM_MPA, DW$CM_MM_fact + DW$CM_MM_MPA, DW$CM_MM_MPA - DW$CM_MM_fact)

DW$FM_fact <- (abs(DW$FM_perc - DW$FM_proz_MPA))*DW$ff_w
DW$FM_dr2 <- ifelse(DW$FM_perc > DW$FM_proz_MPA, DW$FM_fact + DW$FM_proz_MPA, DW$FM_proz_MPA - DW$FM_fact)


for (i in ProbeTR2){
  load(file="dreieck2.rda")#add directory path to file
  TT.set(reset=TRUE)
  TT.add("tat.TT"= tat)
  
  mix <- data.frame(
    "z"=2,
    "Karbonat" = DW[DW$ProbeNR %in% i, "K_D_dr2"],
    "TM_BS" = DW[DW$ProbeNR %in% i, "CM_MM_dr2"],
    "SS" = DW[DW$ProbeNR %in% i, "FM_dr2"])
  XRD <- data.frame(
    "z"=1,
    "Karbonat" = DW[DW$ProbeNR %in% i, "K_D_XRD"],
    "TM_BS" = DW[DW$ProbeNR %in% i, "CM_MM_XRD"],
    "SS" = DW[DW$ProbeNR %in% i, "FM_perc"])
  MPA <- data.frame(
    "z"=1.5,
    "Karbonat" = DW[DW$ProbeNR %in% i, "K_D_MPA"],
    "TM_BS" = DW[DW$ProbeNR %in% i, "CM_MM_MPA"],
    "SS" = DW[DW$ProbeNR %in% i, "FM_proz_MPA"])
  
  alle <- rbind(mix,XRD,MPA)
  
  setwd("")#add directory path to folder
  png(file=paste0(i,"einz_dr2.png"), width=1500,height=1500, res=140, pointsize=8)
  D1T <- TT.plot( class.sys = "tat.TT",
                  css.names=c("Karbonat","TM_BS","SS"),
                  css.lab = c("D+K","MM+CM","FM"),
                  main="",
                  cex.axis=3.5,
                  cex.lab=3.5,
                  cex=3.5,
                  lwd.lab = 0.8,
                  lwd.axis=0.5,
                  class.p.bg.col=c("lightblue","deepskyblue1","deepskyblue3","cyan1","cyan2","cyan3","aquamarine1","aquamarine2", "aquamarine3", "coral1","mistyrose1","mistyrose2" )
  )
  TT.points(tri.data = alle, geo= D1T, css.names=c("Karbonat","TM_BS","SS"), #z.name="z",
            cex= c(6),
            pch=c(15,17,19))
  
  legend("topright", c("XRD","MPA","resulting"),
         pt.cex = c(4,4,4),
         xjust=4, yjust=1,
         cex=4,
         pch = c(17,19,15), col=c("black"), bg="white")
  dev.off()}

#triangle 3
D13 <- as.vector(DMpoints$`D-1.D-3`$ProbeNR)
D23 <- as.vector(DMpoints$`D-2.D-3`$ProbeNR)
D31 <- as.vector(DMpoints$`D-3.D-1`$ProbeNR)
D32 <- as.vector(DMpoints$`D-3.D-2`$ProbeNR)
ProbeTR3 <- c(D13, D23, D31, D32)

DW$MM_K_D_XRD <- DW$K_perc + DW$D_perc + DW$MM_perc
DW$MM_K_D_MPA <- DW$K_proz_MPA + DW$D_proz_MPA + DW$MM_proz_MPA
DW$MM_K_D_fact <- (abs(DW$MM_K_D_XRD - DW$MM_K_D_MPA))*DW$ff_w
DW$MM_K_D_dr3 <- ifelse(DW$MM_K_D_XRD > DW$MM_K_D_MPA, DW$MM_K_D_fact + DW$MM_K_D_MPA, DW$MM_K_D_MPA - DW$MM_K_D_fact)

DW$FM_fact <- (abs(DW$FM_perc - DW$FM_proz_MPA))*DW$ff_w
DW$FM_dr3 <- ifelse(DW$FM_perc > DW$FM_proz_MPA, DW$FM_fact + DW$FM_proz_MPA, DW$FM_proz_MPA - DW$FM_fact)

DW$CM_fact <- (abs(DW$CM_perc - DW$CM_proz_MPA))*DW$ff_w
DW$CM_dr3 <- ifelse(DW$CM_perc > DW$CM_proz_MPA, DW$CM_fact + DW$CM_proz_MPA, DW$CM_proz_MPA - DW$CM_fact)

for (i in ProbeTR3){
  load(file="dreieck3.rda")#add directory path to file
  TT.set(reset=TRUE)
  TT.add("tap.TT"= tap)
  
  mix <- data.frame(
    "z"=2,
    "QF_Mu" = DW[DW$ProbeNR %in% i, "FM_dr3"],
    "TM" = DW[DW$ProbeNR %in% i, "CM_dr3"],
    "BS" = DW[DW$ProbeNR %in% i, "MM_K_D_dr3"])
  XRD <- data.frame(
    "z"=1,
    "QF_Mu" = DW[DW$ProbeNR %in% i, "FM_perc"],
    "TM" = DW[DW$ProbeNR %in% i, "CM_perc"],
    "BS" = DW[DW$ProbeNR %in% i, "MM_perc"]+DW[DW$ProbeNR %in% i, "K_perc"]+DW[DW$ProbeNR %in% i, "D_perc"])
  MPA <- data.frame(
    "z"=1.5,
    "QF_Mu" = DW[DW$ProbeNR %in% i, "FM_proz_MPA"],
    "TM" = DW[DW$ProbeNR %in% i, "CM_proz_MPA"],
    "BS" = DW[DW$ProbeNR %in% i, "MM_proz_MPA"]+ DW[DW$ProbeNR %in% i, "K_proz_MPA"] + DW[DW$ProbeNR %in% i, "D_proz_MPA"])
  
  alle <- rbind(mix,XRD,MPA)
  
  setwd("")#add directory path to folder
  png(file=paste0(i,"einz_dr3.png"), width=1500,height=1500, res=140, pointsize=8)
  D1T <- TT.plot( class.sys = "tap.TT",
                  css.names=c("QF_Mu","TM","BS"),
                  css.lab = c("FM","CM","D+K+MM"),
                  main="",
                  cex.axis=3.5,
                  cex.lab=3.5,
                  cex=3.5,
                  lwd.lab = 0.8,
                  lwd.axis=0.5,
                  class.p.bg.col=c("coral1","coral2","mistyrose1","mistyrose2","olivedrab1","olivedrab2","olivedrab3","mistyrose3","magenta")
  )
  TT.points(tri.data = alle, geo= D1T, css.names=c("QF_Mu","TM","BS"), #z.name="z",
            cex= c(6),
            pch=c(15,17,19))
  
  legend("topright", c("XRD","MPA","resulting"),
         pt.cex = c(4,4,4),
         xjust=4, yjust=1,
         cex=4,
         pch = c(17,19,15), col=c("black"), bg="white")
  dev.off()}
#######################################################################################################################################

#####################################################################################################################################
#Plot percentage of mineral composition of rocks with single point in triangles 1,2,3
gesteine$karb <- gesteine$Kalk + gesteine$Dolomit
gesteine_dr1 <- gesteine[gesteine$karb >= 75,]
gesteine_dr2 <- gesteine[gesteine$karb < 75,]
gesteine_dr2 <- gesteine_dr2[gesteine_dr2$karb > 10,]
gesteine_dr3 <- gesteine[gesteine$karb <= 10,]

#triangle1
for (i in gesteine_dr1$bez){
  load(file="dreieck1.rda")#add directory path to file
  TT.set(reset=TRUE)
  TT.add("tas.TT"= tas)

  MPA <- data.frame(
    "z"=1.5,
    "Dolomit" = gesteine_dr1[gesteine_dr1$bez %in% i, "Dolomit"],
    "TM_Silikate" = gesteine_dr1[gesteine_dr1$bez %in% i, "SaureSilikate"]+gesteine_dr1[gesteine_dr1$bez %in% i, "BasischeSilikate"]+gesteine_dr1[gesteine_dr1$bez %in% i, "Tonminerale"],
    "Kalk" = gesteine_dr1[gesteine_dr1$bez %in% i, "Kalk"])
  
  
  setwd("")#add directory path to folder
  png(file=paste0(i,".png"), width=1500,height=1500, res=140, pointsize=8)
  D1T <- TT.plot( class.sys = "tas.TT",
                  css.names=c("Dolomit","TM_Silikate","Kalk"),
                  css.lab = c("D","FM+MM+CM","K"),
                  main="",
                  cex.axis=3.5,
                  cex.lab=3.5,
                  cex=3.5,
                  lwd.axis=0.5,
                  lwd.lab = 0.8,
                  class.p.bg.col=c("gray61","gray68","gray75","deepskyblue","deepskyblue1","deepskyblue2","cyan1","cyan2","cyan3","aquamarine1","aquamarine2","aquamarine3","seagreen1","seagreen2","mistyrose1","mistyrose2")
  )
  TT.points(tri.data = MPA, geo= D1T, css.names=c("Dolomit","TM_Silikate","Kalk"), #z.name="z",
            cex= c(6),
            pch=c(19))
  
  legend("topright", c("MPA"),
         pt.cex = c(4),
         xjust=4, yjust=1,
         cex=4,
         pch = c(19), col=c("black"), bg="white")
  dev.off()}


#triangle 2
for (i in gesteine_dr2$bez){
  load(file="dreieck2.rda")#add directory path to file
  TT.set(reset=TRUE)
  TT.add("tat.TT"= tat)
  
  MPA <- data.frame(
    "z"=1.5,
    "Karbonat" = gesteine_dr2[gesteine_dr2$bez %in% i, "Dolomit"]+gesteine_dr2[gesteine_dr2$bez %in% i, "Kalk"],
    "TM_BS" = gesteine_dr2[gesteine_dr2$bez %in% i, "Tonminerale"]+gesteine_dr2[gesteine_dr2$bez %in% i, "BasischeSilikate"],
    "SS" = gesteine_dr2[gesteine_dr2$bez %in% i, "SaureSilikate"])
  
  setwd("")#add directory path to folder
  png(file=paste0(i,".png"), width=1500,height=1500, res=140, pointsize=8)
  D1T <- TT.plot( class.sys = "tat.TT",
                  css.names=c("Karbonat","TM_BS","SS"),
                  css.lab = c("D+K","MM+CM","FM"),
                  main="",
                  cex.axis=3.5,
                  cex.lab=3.5,
                  cex=3.5,
                  lwd.lab = 0.8,
                  lwd.axis=0.5,
                  class.p.bg.col=c("lightblue","deepskyblue1","deepskyblue3","cyan1","cyan2","cyan3","aquamarine1","aquamarine2", "aquamarine3", "coral1","mistyrose1","mistyrose2" )
  )
  TT.points(tri.data = MPA, geo= D1T, css.names=c("Karbonat","TM_BS","SS"), #z.name="z",
            cex= c(6),
            pch=c(19))
  
  legend("topright", c("MPA"),
         pt.cex = c(4),
         xjust=4, yjust=1,
         cex=4,
         pch = c(19), col=c("black"), bg="white")
  dev.off()}

#triangle3
for (i in gesteine_dr3$bez){
  load(file="dreieck3.rda")#add directory path to file
  TT.set(reset=TRUE)
  TT.add("tap.TT"= tap)
  
  MPA <- data.frame(
    "z"=1.5,
    "QF_Mu" = gesteine_dr3[gesteine_dr3$bez %in% i, "SaureSilikate"],
    "TM" = gesteine_dr3[gesteine_dr3$bez %in% i, "Tonminerale"],
    "BS" = gesteine_dr3[gesteine_dr3$bez %in% i, "BasischeSilikate"]+gesteine_dr3[gesteine_dr3$bez %in% i, "Kalk"]+gesteine_dr3[gesteine_dr3$bez %in% i, "Dolomit"])
  
  
  setwd("")#add directory path to folder
  png(file=paste0(i,".png"), width=1500,height=1500, res=140, pointsize=8)
  D1T <- TT.plot( class.sys = "tap.TT",
                  css.names=c("QF_Mu","TM","BS"),
                  css.lab = c("FM","CM","D+K+MM"),
                  main="",
                  cex.axis=3.5,
                  cex.lab=3.5,
                  cex=3.5,
                  lwd.lab = 0.8,
                  lwd.axis=0.5,
                  class.p.bg.col=c("coral1","coral2","mistyrose1","mistyrose2","olivedrab1","olivedrab2","olivedrab3","mistyrose3","magenta")
  )
  TT.points(tri.data = MPA, geo= D1T, css.names=c("QF_Mu","TM","BS"), #z.name="z",
            cex= c(6),
            pch=c(19))
  
  legend("topright", c("MPA"),
         pt.cex = c(4),
         xjust=4, yjust=1,
         cex=4,
         pch = c(19), col=c("black"), bg="white")
  dev.off()}
