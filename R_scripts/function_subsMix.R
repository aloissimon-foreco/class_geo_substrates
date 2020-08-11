
##change "load" directory of .rda files
#creates the function TT.subst, based on the chemical-physical entity, average combinations of percentages of dolomite, 
#calcite, felsic, mafic and clay minerals are assigned, for both the underlying solid bedrock (R_subst) and the overburdens (R_SubstD). 
#For the resulting mixed values, the overburden thickness is taken into account. Given the bedrock and 
#overburden chemical entities and the overburden thickness (1- 5)(DeckM) as input, the function calculates the resulting 
#chemical-physical entity and it shows it in the triangle. 

TT.subst <- function(R_Subst,R_SubstD,DeckM){
  
  library("soiltexture")
  if (R_Subst == "-"){
    R_Subst <- R_SubstD}
  if (R_Subst == "B0"){
    Dolomit <- 4
    Kalk<- 4
    Karbonat <- 8
    CM <- 12.5
    MM <- 52   # + 8 (Karbonat)when triangle 2a is used
    FM <- 27.5
  } else if (R_Subst == "B-"){
    Dolomit <- 1.25 
    Kalk <- 1.25
    Karbonat <- 2.5
    CM <- 2.5
    MM <- 42.5 # + 2.5 (Karbonat)when triangle 2a is used
    FM <- 52.5
  } else if (R_Subst == "B+"){
    Dolomit <- 3
    Kalk <- 3
    Karbonat <-6
    CM <- 40 
    MM <- 39   #+ 6 (Karbonat) when triangle 2a is used
    FM <- 15 
  } else if (R_Subst == "I0"){
    Dolomit <- 2
    Kalk <- 2
    Karbonat <- 4
    CM <- 17.5  
    MM <- 11 #+ 4 (Karbonat) when triangle 2a is used
    FM <- 67.5
  } else if (R_Subst == "I-"){
    Dolomit <- 1.25
    Kalk <- 1.25
    Karbonat <- 2.5 
    CM <- 5
    MM <- 17.5  #+ 2.5 (Karbonat) when triangle 2a is used
    FM <- 75
  } else if (R_Subst == "I+"){
    Dolomit <- 1
    Kalk <- 1
    Karbonat <- 2
    CM <- 55
    MM <- 13 #+ 2 (Karbonat) when triangle 2a is used
    FM <- 30
  } else if (R_Subst == "U0"){
    Dolomit <- 2.5
    Kalk <- 2.5
    Karbonat <- 5
    CM <- 2.5
    MM <- 90
    FM <- 2.5
  } else if (R_Subst == "K0"){
    Dolomit <- 25
    Kalk <- 65
    Karbonat <- 90
    CM <- 2.5
    MM <- 2.5
    FM <- 5
  } else if (R_Subst == "K-"){
    Dolomit <- 25
    Kalk <- 72.5
    Karbonat <- 97.5
    CM <- 0.7
    MM <- 1
    FM <- 0.8
  } else if (R_Subst == "K+"){
    Dolomit <- 25
    Kalk <- 55
    Karbonat <- 80 
    CM <- 16
    MM <- 2
    FM <- 2
  } else if (R_Subst == "C0"){
    Dolomit <- 30
    Kalk <- 25
    Karbonat <- 55 
    CM <- 10
    MM <- 10
    FM <- 25
  } else if (R_Subst == "C-"){
    Dolomit <- 50
    Kalk <- 5
    Karbonat <- 55 
    CM <- 2.5
    MM <- 2.5
    FM <- 40
  } else if (R_Subst == "C+"){
    Dolomit <- 15
    Kalk <- 40
    Karbonat <- 55 
    CM <- 20
    MM <- 15
    FM <- 10
  } else if (R_Subst == "D0"){
    Dolomit <- 70
    Kalk <- 20
    Karbonat <- 90 
    CM <- 3
    MM <- 3
    FM <- 4
  } else if (R_Subst == "D-"){
    Dolomit <- 72.5
    Kalk <- 22.5
    Karbonat <- 95
    CM <- 2.5
    MM <- 0
    FM <- 2.5
  } else if (R_Subst == "D+"){
    Dolomit <- 65
    Kalk <- 15
    Karbonat <- 80 
    CM <- 16
    MM <- 2
    FM <- 2
  } else if (R_Subst == "M0"){
    Dolomit <- 12.5
    Kalk <- 10
    Karbonat <- 22.5
    CM <- 12.5
    MM <- 10
    FM <- 55
  } else if (R_Subst == "M-"){
    Dolomit <- 17.5
    Kalk <- 5
    Karbonat <- 22.5
    CM <- 2.5
    MM <- 2.5
    FM <- 72.5
  } else if (R_Subst == "M+"){
    Dolomit <- 2.5#7.5
    Kalk <- 20#7.5
    Karbonat <- 22.5
    CM <- 32.5
    MM <- 25
    FM <- 20
  } else if (R_Subst == "S0"){
    Dolomit <- 0.5
    Kalk <- 0.5
    Karbonat <- 1 
    CM <- 8
    MM <- 1 #+ 1 (Karbonat) when triangle 2a is used
    FM <- 90
  } else if (R_Subst == "S-"){
    Dolomit <- 0.5
    Kalk <- 0.5
    Karbonat <- 1
    CM <- 2
    MM <- 2 #+ 1 (Karbonat) when triangle 2a is used
    FM <- 95
  }
  
  
  
  # zuweisung fÃ¼r Deckschicht
  
  if (R_SubstD == "B0"){
    Dolomit_D <- 4
    Kalk_D <- 4
    Karbonat_D <- 8
    CM_D <- 12.5
    MM_D <- 52   
    FM_D <- 27.5
  } else if (R_SubstD == "B-"){
    Dolomit_D <- 1.25 
    Kalk_D <- 1.25 
    Karbonat_D <- 2.5
    CM_D <- 2.5
    MM_D <- 42.5 
    FM_D <- 52.5
  } else if (R_SubstD == "B+"){
    Dolomit_D <- 3
    Kalk_D <- 3
    Karbonat_D <- 6 
    CM_D <- 40 
    MM_D <- 39   
    FM_D <- 15 
  } else if (R_SubstD == "I0"){
    Dolomit_D <- 2
    Kalk_D <- 2
    Karbonat_D <- 4
    CM_D <- 17.5  
    MM_D <- 11 
    FM_D <- 67.5
  } else if (R_SubstD == "I-"){
    Dolomit_D <- 1.25
    Kalk_D <- 1.25
    Karbonat_D <- 2.5 
    CM_D <- 5
    MM_D <- 17.5 
    FM_D <- 75
  } else if (R_SubstD == "I+"){
    Dolomit_D <- 1 
    Kalk_D <- 1
    Karbonat_D <- 2 
    CM_D <- 55
    MM_D <- 13  
    FM_D <- 30
  } else if (R_SubstD == "U0"){
    Dolomit_D <- 2.5 
    Kalk_D <- 2.5
    Karbonat_D <- 5 
    CM_D <- 2.5
    MM_D <- 90  
    FM_D <- 2.5
  } else if (R_SubstD == "K0"){
    Dolomit_D <- 25
    Kalk_D <- 65
    Karbonat_D <- 90
    CM_D <- 2.5
    MM_D <- 2.5
    FM_D <- 5
  } else if (R_SubstD == "K-"){
    Dolomit_D <- 25
    Kalk_D <- 72.5
    Karbonat_D <- 97.5
    CM_D <- 0.7
    MM_D <- 1
    FM_D <- 0.8
  } else if (R_SubstD == "K+"){
    Dolomit_D <- 25
    Kalk_D <- 55
    Karbonat_D <- 80 
    CM_D <- 16
    MM_D <- 2
    FM_D <- 2
  } else if (R_SubstD == "C0"){
    Dolomit_D <- 30
    Kalk_D <- 25
    Karbonat_D <- 55 
    CM_D <- 10
    MM_D <- 10
    FM_D <- 25
  } else if (R_SubstD == "C-"){
    Dolomit_D <- 50
    Kalk_D <- 5
    Karbonat_D <- 55 
    CM_D <- 2.5
    MM_D <- 2.5
    FM_D <- 40
  } else if (R_SubstD == "C+"){
    Dolomit_D <- 15
    Kalk_D <- 40
    Karbonat_D <- 55 
    CM_D <- 20
    MM_D <- 15
    FM_D <- 10
  } else if (R_SubstD == "D0"){
    Dolomit_D <- 70
    Kalk_D <- 20
    Karbonat_D <- 90 
    CM_D <- 3
    MM_D <- 3
    FM_D <- 4
  } else if (R_SubstD == "D-"){
    Dolomit_D <- 72.5
    Kalk_D <- 22.5
    Karbonat_D <- 95
    CM_D <- 2.5
    MM_D <- 0
    FM_D <- 2.5
  } else if (R_SubstD == "D+"){
    Dolomit_D <- 65
    Kalk_D <- 15
    Karbonat_D <- 80 
    CM_D <- 16
    MM_D <- 2
    FM_D <- 2
  } else if (R_SubstD == "M0"){
    Dolomit_D <- 12.5
    Kalk_D <- 10
    Karbonat_D <- 22.5
    CM_D <- 12.5
    MM_D <- 10
    FM_D <- 55
  } else if (R_SubstD == "M-"){
    Dolomit_D <- 17.5
    Kalk_D <- 5
    Karbonat_D <- 22.5
    CM_D <- 2.5
    MM_D <- 2.5
    FM_D <- 72.5
  } else if (R_SubstD == "M+"){
    Dolomit_D <- 2.5#7.5
    Kalk_D <- 20#7.5
    Karbonat_D <- 22.5
    CM_D <- 32.5
    MM_D <- 20
    FM_D <- 25
  } else if (R_SubstD == "S0"){
    Dolomit_D <- 0.5
    Kalk_D <- 0.5
    Karbonat_D <- 1 
    CM_D <- 8
    MM_D <- 1 
    FM_D <- 90
  } else if (R_SubstD == "S-"){
    Dolomit_D <- 0.5
    Kalk_D <- 0.5
    Karbonat_D<- 1
    CM_D <- 2
    MM_D <- 2 
    FM_D <- 95
  }
  
  
##################general factors###############################################  
  
  #R_TM_Mix
  
  if (DeckM == 1){
    R_CM_Mix <- (0.9*CM)+(0.1*CM_D)
  } else if (DeckM == 2){
    R_CM_Mix <- (0.5*CM)+(0.5*CM_D) 
  } else if (DeckM == 3){
    R_CM_Mix <- (0.3*CM)+(0.7*CM_D)
  } else if (DeckM == 5){
    R_CM_Mix <- (0*CM)+(CM_D)
  } else if (DeckM == 4){
    R_CM_Mix <- (0.1*CM)+(0.9*CM_D)
  }
  
  #R_MM_Mix
  if (DeckM == 1){
    R_MM_Mix <- (0.9*MM)+(0.1*MM_D)
  } else if (DeckM == 2){
    R_MM_Mix <- (0.5*MM)+(0.5*MM_D)
  } else if (DeckM == 3){
    R_MM_Mix <- (0.3*MM)+(0.7*MM_D)
  } else if (DeckM == 5){
    R_MM_Mix <- (0*MM)+(MM_D)
  } else if (DeckM == 4){
    R_MM_Mix <- (0.1*MM)+(0.9*MM_D)
  }
  
  #R_FM_Mix
  if (DeckM == 1){
    R_FM_Mix <- (0.9*FM)+(0.1*FM_D)
  } else if (DeckM == 2){
    R_FM_Mix <- (0.5*FM)+(0.5*FM_D)
  } else if (DeckM == 3){
    R_FM_Mix <- (0.3*FM)+(0.7*FM_D)
  } else if (DeckM == 5){
    R_FM_Mix <- (0*FM)+(FM_D)
  } else if (DeckM == 4){
    R_FM_Mix <- (0.1*FM)+(0.9*FM_D)
  }
  
  #R_Kabonat_Mix
  if (DeckM == 1){
    R_Karbonat_Mix <- (0.9*Karbonat)+(0.1*Karbonat_D)
  } else if (DeckM == 2){
    R_Karbonat_Mix <- (0.5*Karbonat)+(0.5*Karbonat_D)
  } else if (DeckM == 3){
    R_Karbonat_Mix <- (0.3*Karbonat)+(0.7*Karbonat_D)
  } else if (DeckM == 5){
    R_Karbonat_Mix <- (0*Karbonat)+(Karbonat_D)
  } else if (DeckM == 4){
    R_Karbonat_Mix <- (0.1*Karbonat)+(0.9*Karbonat_D)
  }
  
  #R_Dolomit_Mix
  if (DeckM == 1){
    R_Dolomit_Mix <- (0.9*Dolomit)+(0.1*Dolomit_D)
  } else if (DeckM == 2){
    R_Dolomit_Mix <- (0.5*Dolomit)+(0.5*Dolomit_D)
  } else if (DeckM == 3){
    R_Dolomit_Mix <- (0.3*Dolomit)+(0.7*Dolomit_D)
  } else if (DeckM == 5){
    R_Dolomit_Mix <- (0*Dolomit)+(Dolomit_D)
  } else if (DeckM == 4){
    R_Dolomit_Mix <- (0.1*Dolomit)+(0.9*Dolomit_D)
  }
  
  #R_Kalk_Mix
  if (DeckM == 1){
    R_Kalk_Mix <- (0.9*Kalk)+(0.1*Kalk_D)
  } else if (DeckM == 2){
    R_Kalk_Mix <- (0.5*Kalk)+(0.5*Kalk_D)
  } else if (DeckM == 3){
    R_Kalk_Mix <- (0.3*Kalk)+(0.7*Kalk_D)
  } else if (DeckM == 5){
    R_Kalk_Mix <- (0*Kalk)+(Kalk_D)
  } else if (DeckM == 4){
    R_Kalk_Mix <- (0.1*Kalk)+(0.9*Kalk_D)
  }
  
#############################################################################
  if(R_Karbonat_Mix<=10){
    load(file="J:/Waldtypisierung/waldtypenbeschreibung/substrat_deck/dreieck3.rda")
    
    TT.set(reset=TRUE)
    TT.add("tap.TT"= tap)
    
    newMM_Mix = R_MM_Mix + R_Karbonat_Mix
    punkte10 <- data.frame(
      "z"=2,
      "FM" = R_FM_Mix,
      "CM" = R_CM_Mix,
      "MM" = newMM_Mix)
    
    substrat <- TT.points.in.classes(
      tri.data=punkte10,
      css.names=c("FM","CM","MM"),
      class.sys="tap.TT",
      PiC.type="t")
    
    newMM = MM + Karbonat
    newMM_D = MM_D + Karbonat_D
    CM_MM = CM + MM
    CM_MM_D = CM_D + MM_D
    
    punkte10S <- data.frame(
      "z"=1,
      "FM" = FM,
      "CM" = CM,
      "MM" = newMM)
    
    punkte10D <- data.frame(
      "z"= 1.5,
      "FM" = FM_D,
      "CM" = CM_D,
      "MM" = newMM_D)
############################subs and deck have carbonate <10%########################################    
    if(Karbonat<=10 & Karbonat_D<=10){
      
      punkte10_alle <- rbind(punkte10, punkte10S, punkte10D)
      
      geo <- TT.plot( class.sys = "tap.TT",
               css.names=c("FM","CM","MM"),
               css.lab = c("FM","CM","D+K+MM"),
               main="",
               cex.axis=0.7,
               cex.lab=0.9,
               cex=1,
               lwd.axis= 0.5,
               lwd.lab = 0.8,
               class.p.bg.col=c("coral1","coral2","mistyrose1","mistyrose2","chartreuse1","chartreuse2","chartreuse3","mistyrose3")
      )
      cib <- data.frame(FM=punkte10_alle$FM*0.01,CM=punkte10_alle$CM*0.01,MM=punkte10_alle$MM*0.01)
      
      mac <- structure(list("Substratgesellschaftsdreieck 1",structure(list((cib$FM),
                                                                            (cib$CM),
                                                                            (cib$MM)),
                                                                       .Names = c("CLAY","SILT","SAND"),class="data.frame",row.names=c(NA,-14L)),
                            structure(list(structure(list("Dolomitisch rückstandsarm", c(2,3)), .Names = c("name", "points"))),
                                      .Names = c("D-")),
                            blr.tx=c("SAND","CLAY","SILT"),
                            blr.clock=c(TRUE,TRUE,TRUE),
                            tlr.an=c(60,60,60),
                            c(0, 2, 50, 2000), 
                            c(0, 2, 50, 2000), 
                            quote(bold(mu) * bold("m")), quote(bold("%")), 100),
                       .Names = c("main", "tt.points", "tt.polygons","blr.tx","blr.clock","tlr.an","base.css.ps.lim", "tri.css.ps.lim", "unit.ps", 
                                  "unit.tx", "text.sum"))
      
      
      
      TT.add("mac.TT"= mac)
      TT.classes( geo=geo,
                  class.sys = "mac.TT",
                  css.transf=TRUE,
                  lwd.axis=1,
                  class.line.col= "black",
                  class.p.bg.col=c("lightblue","deepskyblue1","deepskyblue3","cyan1","cyan2","cyan3","aquamarine1","aquamarine2", "aquamarine3", "coral1","mistyrose1","mistyrose2" )
      )
      
      TT.points(tri.data = punkte10_alle, geo= geo, css.names=c("FM","CM","MM"), #z.name="z",
                cex= c(2),
                pch=c(15,17,19))
      
      legend(title=paste0("OVt=",DeckM),
        "topright", c("ULsc","OVsc","resulting"),
        pt.cex = c(2,2,2),  
        xjust=4, yjust=1,
        pch = c(17,19,15), col=c("black"), bg="white")
      
################################subs has carbonate <=10% and deck has >10%####################################
    }else if(Karbonat <=10 & Karbonat_D > 10){
    
      punkte10_mix_subs <- rbind(punkte10, punkte10S)
      
    TT.plot( class.sys = "tap.TT",
             tri.data= punkte10_mix_subs,
             css.names=c("FM","CM","MM"),
             css.lab = c("FM","CM","D+K+MM"),
             main="",
             cex.axis=0.7,
             cex.lab=0.9,
             cex=1,
             pch=c(15,17),
             lwd.lab=0.8,
             lwd.axis= 0.5,
             class.p.bg.col=c("coral1","coral2","mistyrose1","mistyrose2","chartreuse1","chartreuse2","chartreuse3","mistyrose3")
    )
      legend(title=paste0("OVt=",DeckM),
        "topright", c("OVsc","resulting"),
        pt.cex = c(2),         
        xjust=4, yjust=1,
        pch = c(17,15), col=c("black"), bg="white")
      
      punkte10D <- data.frame(
        "Karbonat" = Karbonat_D,
        "CM_MM" = CM_MM_D,
        "FM" = FM_D)
      load(file="T:/WP_T3_Case_studies_Biomass/substrat_deck/dreieck2.rda")
      TT.set(reset=TRUE)
      TT.add("tat.TT"= tat)
      TT.plot( class.sys = "tat.TT",
               tri.data= punkte10D,
               css.names=c("Karbonat","CM_MM","FM"),
               css.lab = c("D+K","MM+CM","FM"),
               main="",
               cex.axis=0.7,
               cex.lab=0.9,
               cex=1,
               pch=c(19),
               lwd.lab=0.8,
               lwd.axis=0.5,
               class.p.bg.col=c("lightblue","deepskyblue1","deepskyblue3","cyan1","cyan2","cyan3","aquamarine1","aquamarine2", "aquamarine3", "coral1","mistyrose1","mistyrose2" )
      )
      legend(title=paste0("OVt=",DeckM),
             "topright", c("OVsc"),
             pt.cex = c(2),
             xjust=4, yjust=1,
             pch = c(19), col=c("black"), bg="white")}
    
##############################subs has carbonate >10% and deck has <10%####################################
    else if (Karbonat >10 & Karbonat_D <=10){
      punkte10_mix_deck <- rbind(punkte10, punkte10D)
    
    TT.plot( class.sys = "tap.TT",
             tri.data= punkte10_mix_deck,
             css.names=c("FM","CM","MM"),
             css.lab = c("FM","CM","D+K+MM"),
             main="",
             cex.axis=0.7,
             cex.lab=0.9,
             cex=1,
             pch=c(15,19),
             lwd.lab = 0.8,
             lwd.axis= 0.5,
             class.p.bg.col=c("coral1","coral2","mistyrose1","mistyrose2","olivedrab1","olivedrab2","olivedrab3","mistyrose3")
    )
    legend(title=paste0("OVt=",DeckM),
           "topright", c("OVsc","resulting"),
           pt.cex = c(2,2),         
           xjust=4, yjust=1,
           pch = c(19,15), col=c("black"), bg="white")
    
    punkte10S <- data.frame(
      "Karbonat" = Karbonat,
      "CM_MM" = CM_MM,
      "FM" = FM)
    load(file="J:/Waldtypisierung/waldtypenbeschreibung/substrat_deck/dreieck2.rda")
    TT.set(reset=TRUE)
    TT.add("tat.TT"= tat)
    TT.plot( class.sys = "tat.TT",
             tri.data= punkte10S,
             css.names=c("Karbonat","CM_MM","FM"),
             css.lab = c("D+K","MM+CM","FM"),
             main="",
             cex.axis=0.7,
             cex.lab=0.9,
             cex=1,
             lwd.axis= 0.5,
             lwd.lab = 0.8,
             class.p.bg.col=c("lightblue","deepskyblue1","deepskyblue3","cyan1","cyan2","cyan3","aquamarine1","aquamarine2", "aquamarine3", "coral1","mistyrose1","mistyrose2" )
    )
    legend(title=paste0("OVt=",DeckM),
           "topright", c("ULsc"),
           pt.cex = c(2),
           #cex=4,
           xjust=4, yjust=1,
           pch = c(17), col=c("black"), bg="white")
      
    }
  }else if(R_Karbonat_Mix>10 & R_Karbonat_Mix<75){
    load(file="J:/Waldtypisierung/waldtypenbeschreibung/substrat_deck/dreieck2.rda")
    TT.set(reset=TRUE)
    TT.add("tat.TT"= tat)
    CM_MM_Mix = R_MM_Mix + R_CM_Mix
    CM_MM = CM + MM
    CM_MM_D = CM_D + MM_D
    
    punkte10_75 <- data.frame(
      "z"=2,
      "Karbonat" = R_Karbonat_Mix,
      "CM_MM" = CM_MM_Mix,
      "FM" = R_FM_Mix)
    punkte10_75S <- data.frame(
      "z"=1,
      "Karbonat" = Karbonat,
      "CM_MM" = CM_MM,
      "FM" = FM)
    punkte10_75D <- data.frame(
      "z"=1.5,
      "Karbonat" = Karbonat_D,
      "CM_MM" = CM_MM_D,
      "FM" = FM_D)
    punkte10_75_alle <- rbind(punkte10_75, punkte10_75S, punkte10_75D)
    
    substrat <- TT.points.in.classes(
      tri.data=punkte10_75,
      css.names=c("Karbonat","CM_MM","FM"),
      class.sys="tat.TT",
      PiC.type="t")
    
    geo <- TT.plot( class.sys = "tat.TT",
             css.names=c("Karbonat","CM_MM","FM"),
             css.lab = c("D+K","MM+CM","FM"),
             main="",
             cex.axis=0.7,
             cex.lab=0.9,
             cex=1,
             lwd.axis= 0.5,
             lwd.lab = 0.8,
             class.p.bg.col=c("lightblue","deepskyblue1","deepskyblue3","cyan1","cyan2","cyan3","aquamarine1","aquamarine2", "aquamarine3", "coral1","mistyrose1","mistyrose2" )
    )
    
    cib <- data.frame(Karbonat=punkte10_75_alle$Karbonat*0.01,CM_MM=punkte10_75_alle$CM_MM*0.01,FM=punkte10_75_alle$FM*0.01)
    
    mac <- structure(list("Substratgesellschaftsdreieck 1",structure(list((cib$Karbonat),
                                                                          (cib$CM_MM),
                                                                          (cib$FM)),
                                                                     .Names = c("CLAY","SILT","SAND"),class="data.frame",row.names=c(NA,-14L)),
                          structure(list(structure(list("Dolomitisch rückstandsarm", c(2,3)), .Names = c("name", "points"))),
                                    .Names = c("D-")),
                          blr.tx=c("SAND","CLAY","SILT"),
                          blr.clock=c(TRUE,TRUE,TRUE),
                          tlr.an=c(60,60,60),
                          c(0, 2, 50, 2000), 
                          c(0, 2, 50, 2000), 
                          quote(bold(mu) * bold("m")), quote(bold("%")), 100),
                     .Names = c("main", "tt.points", "tt.polygons","blr.tx","blr.clock","tlr.an","base.css.ps.lim", "tri.css.ps.lim", "unit.ps", 
                                "unit.tx", "text.sum"))
    
    
    
    TT.add("mac.TT"= mac)
    TT.classes( geo=geo,
                class.sys = "mac.TT",
                css.transf=TRUE,
                lwd.axis=1,
                class.line.col= "black",
                class.p.bg.col=c("lightblue","deepskyblue1","deepskyblue3","cyan1","cyan2","cyan3","aquamarine1","aquamarine2", "aquamarine3", "coral1","mistyrose1","mistyrose2" )
    )
   
     TT.points(tri.data = punkte10_75_alle, geo= geo, css.names=c("Karbonat","CM_MM","FM"),
             cex= c(2),#c(1.5),
              pch=c(15,17,19))
   
     legend(title=paste0("OVt=",DeckM),
            "topright", c("ULsc","OVsc","resulting"),
            pt.cex = c(2,2,2),#c(1.5,1.5,1.5),         
            xjust=0, yjust=1,
            pch = c(17,19,15), col=c("black"), bg="white")
    
  }else if(R_Karbonat_Mix>=75){
    load(file="J:/Waldtypisierung/waldtypenbeschreibung/substrat_deck/dreieck1.rda")
    TT.set(reset=TRUE)
    TT.add("tas.TT"= tas)
    CM_Si_Mix = R_CM_Mix + R_MM_Mix + R_FM_Mix
    CM_Si = CM + MM + FM
    CM_Si_D = CM_D + MM_D + FM_D
    
    
    punkte75 <- data.frame(
      "z"=2,
      "Dolomit" = R_Dolomit_Mix,
      "CM_Silikate" = CM_Si_Mix,
      "Kalk" = R_Kalk_Mix)
    punkte75S <- data.frame(
      "z"=1,
      "Dolomit" = Dolomit,
      "CM_Silikate" = CM_Si,
      "Kalk" = Kalk)
    punkte75D <- data.frame(
      "z"=1.5,
      "Dolomit" = Dolomit_D,
      "CM_Silikate" = CM_Si_D,
      "Kalk" = Kalk_D)
    punkte75_alle <- rbind(punkte75,punkte75S,punkte75D)
    
    substrat <- TT.points.in.classes(
      tri.data=punkte75,
      css.names=c("Dolomit","CM_Silikate","Kalk"),
      class.sys="tas.TT",
      PiC.type="t")
    
    geo <- TT.plot( class.sys = "tas.TT",
             css.names=c("Dolomit","CM_Silikate","Kalk"),
             css.lab = c("D","FM+MM+CM","K"),
             main="",
             cex.axis=0.7,
             cex.lab=0.9,
             cex=1,
             col="black",
             lwd.axis= 0.5,
             lwd.lab = 0.8,
             class.p.bg.col=c("gray61","gray68","gray75","deepskyblue","deepskyblue1","deepskyblue2","cyan1","cyan2","cyan3","aquamarine1","aquamarine2","aquamarine3","seagreen1","seagreen2","mistyrose1","mistyrose2")
    )
    
    cib <- data.frame(Dolomit=punkte75_alle$Dolomit*0.01,CM_Si=punkte75_alle$CM_Si*0.01,Kalk=punkte75_alle$Kalk*0.01)
    
    mac <- structure(list("Substratgesellschaftsdreieck 1",structure(list((cib$Dolomit),
                                                                          (cib$CM_Si),
                                                                          (cib$Kalk)),
                                                                     .Names = c("CLAY","SILT","SAND"),class="data.frame",row.names=c(NA,-14L)),
                          structure(list(structure(list("Dolomitisch rückstandsarm", c(2,3)), .Names = c("name", "points"))),
                                    .Names = c("D-")),
                          blr.tx=c("SAND","CLAY","SILT"),
                          blr.clock=c(TRUE,TRUE,TRUE),
                          tlr.an=c(60,60,60),
                          c(0, 2, 50, 2000), 
                          c(0, 2, 50, 2000), 
                          quote(bold(mu) * bold("m")), quote(bold("%")), 100),
                     .Names = c("main", "tt.points", "tt.polygons","blr.tx","blr.clock","tlr.an","base.css.ps.lim", "tri.css.ps.lim", "unit.ps", 
                                "unit.tx", "text.sum"))
    
    

    TT.add("mac.TT"= mac)
    TT.classes( geo=geo,
                class.sys = "mac.TT",
                css.transf=TRUE,
                lwd.axis=1,
                class.line.col= "black",
                class.p.bg.col=c("lightblue","deepskyblue1","deepskyblue3","cyan1","cyan2","cyan3","aquamarine1","aquamarine2", "aquamarine3", "coral1","mistyrose1","mistyrose2" )
    )
    
    TT.points(tri.data = punkte75_alle, geo= geo, css.names=c("Dolomit","CM_Silikate","Kalk"), #z.name="z",
              cex= c(2),
              pch=c(15,17,19))
    
    legend(title=paste0("OVt=",DeckM),
      "topright", c("ULsc","OVsc","resulting"),
      pt.cex = c(2,2,2), 
      xjust=4, yjust=1,
      pch = c(17,19,15), col=c("black"), bg="white")
  }

   return(substrat)
}

#examples
#TT.subst("K0","M0","3")#B0
#TT.subst("B0","B0","2")#B0   
#TT.subst("B0","B-","2")#B
#TT.subst("B0","I0","2")#I0
# TT.subst("B0","I0","3")#I0
# TT.subst("B0","I0","1")#I0
# TT.subst("B0","M0","3")#M - M+
# TT.subst("B0","S0","1")#I0
# 
# TT.subst("B-","B0","3")#B
# TT.subst("B-","I0","3")#I
# TT.subst("B-","B0","2")#B-   NO  -> I/B
# TT.subst("B-","I0","2")#I-
# 
# TT.subst("B+","B0","3")#B
# 
# TT.subst("C0","I0","3")#B0 - M0
# TT.subst("C0","M0","3")#M
# TT.subst("C0","S0","2")#M
# TT.subst("C0","K-","1")#C
# TT.subst("C0","D0","3")#C
# 
# TT.subst("C-","M0","2")#C
# 
# #silikatisch karbonatreich parent material is prevalent when deck=1,2
#TT.subst("C+","M0","1")#C+
# TT.subst("C+","M+","1")#C+
# TT.subst("C+","K0","1")#C+
# TT.subst("C+","C0","1")#C+
# TT.subst("C+","K-","1")#C+
# TT.subst("C+","C0","2")#C+
# TT.subst("C+","K0","2")#C+
# TT.subst("C+","K+","2")#C+
# TT.subst("C+","M0","2")#C+
# TT.subst("C+","K-","2")#C+
# TT.subst("C+","I0","2")#M+
# TT.subst("C+","M0","3")#M
# 
# #dolomitisch is prevalent with deck=1 - with deck=3 prevails the cover
# TT.subst("D0","K+","1")#D
# TT.subst("D0","I0","1")#D
# TT.subst("D0","K-","1")#D
# TT.subst("D0","M0","1")#D
# TT.subst("D0","M0","2")#C
#TT.subst("D0","C0","3")#C
# TT.subst("D0","K0","3")#K
# 
# TT.subst("D-","I0","1")#D
# TT.subst("D-","M0","1")#D
# TT.subst("D-","C0","1")#D
# TT.subst("D-","K-","1")#D-
# TT.subst("D-","K0","1")#D NO -> D-
# TT.subst("D-","I0","2")#C
# TT.subst("D-","M0","2")#C
# TT.subst("D-","K0","2")#K  NO -> D
# TT.subst("D-","K-","2")#D-
# TT.subst("D-","D0","2")#D
# TT.subst("D-","C0","2")#D  NO -> D+
# TT.subst("D-","C+","2")#D+/K+
# TT.subst("D-","K-","3")#K-
# TT.subst("D-","M0","3")#C
# TT.subst("D-","K0","3")#K
# TT.subst("D-","C0","3")#C
# TT.subst("D-","S0","3")#M/M-
# TT.subst("D-","I0","3")#M
# 
# TT.subst("D+","K0","1")#D
# TT.subst("D+","K0","2")#D/K
# 
# TT.subst("I0","S0","1")#I
# TT.subst("I0","S-","1")#I
# TT.subst("I0","M0","1")#I
# TT.subst("I0","M0","2")#I
# TT.subst("I0","I-","2")#I
# TT.subst("I0","I+","2")#I+
# TT.subst("I0","S0","2")#I
# TT.subst("I0","C0","2")#M
# TT.subst("I0","C0","3")#M
# TT.subst("I0","S0","3")#S  NO -> I
# TT.subst("I0","B0","3")#B
# 
# TT.subst("I-","I0","1")#I-
# TT.subst("I-","S0","2")#I-
# TT.subst("I-","I0","2")#I
# TT.subst("I-","D-","3")#C-
# 
# TT.subst("I+","K0","1")#I+
# TT.subst("I+","I0","1")#I+
# TT.subst("I+","M0","1")#I+
# TT.subst("I+","I0","2")#I+
# TT.subst("I+","M0","2")#I+
# TT.subst("I+","B0","2")#I+
# 
# TT.subst("K0","K+","1")#K
# TT.subst("K0","M0","1")#K
# TT.subst("K0","B0","2")#C  No ->c+
# TT.subst("K0","C0","2")#C
# TT.subst("K0","C+","2")#K-/K
# TT.subst("K0","M0","2")#C
# TT.subst("K0","S0","2")#C/M NO -> C-
# TT.subst("K0","K-","2")#K
# TT.subst("K0","K+","2")#K+ NO -> K
# TT.subst("K0","I0","3")#M
# TT.subst("K0","M0","3")#M
# 
# TT.subst("K-","C0","1")#K
# TT.subst("K-","M0","1")#K
# TT.subst("K-","K0","2")#K
# TT.subst("K-","M0","2")#K  NO -> C
# TT.subst("K-","K+","2")#K
# TT.subst("K-","C0","2")#C
# TT.subst("K-","K0","3")#K
# TT.subst("K-","I0","3")#M
# TT.subst("K-","M0","3")#M
# 
# TT.subst("K+","K0","1")#K+
# TT.subst("K+","M0","1")#K+
# TT.subst("K+","K-","1")#K+
# TT.subst("K+","K-","2")#K
# TT.subst("K+","C0","2")#C
# TT.subst("K+","M0","3")#C
# TT.subst("K+","C0","3")#C
# 
# TT.subst("M0","K-","1")#M
# TT.subst("M0","B0","1")#M
# TT.subst("M0","K0","1")#M
# TT.subst("M0","I0","2")#M
# TT.subst("M0","B-","2")#M
# TT.subst("M0","C0","2")#M
# TT.subst("M0","B0","2")#M
# TT.subst("M0","S0","3")#I
# TT.subst("M0","I0","3")#M
# TT.subst("M0","K-","3")#C  NO  -> C-
# TT.subst("M0","B0","3")#M/B  NO  -> M+
# 
# TT.subst("M-","K0","1")#M-
# TT.subst("M-","S0","3")#S
# TT.subst("M-","I0","3")#I
# 
# TT.subst("M+","M0","1")#M+
# TT.subst("M+","M0","2")#M+
# TT.subst("M+","K+","1")#M+
# TT.subst("M+","C+","1")#M+
# TT.subst("M+","K0","1")#M+
# 
# TT.subst("S0","I0","1")#S
# TT.subst("S0","M0","1")#S
# TT.subst("S0","I0","2")#I
# TT.subst("S0","S-","2")#S
# TT.subst("S0","S-","3")#S-
# TT.subst("S0","I0","3")#I
# 
# TT.subst("S-","I0","2")#S
# TT.subst("S-","S0","3")#S
# # 
# # 

