library("soiltexture")
setwd("T:/WP_T3_Case_studies_Biomass/substrat_deck")
###################################dreieck 2####################################

TT.set(reset=TRUE)
tat <- structure(list("Substratgesellschaftsdreieck 2",structure(list(c(1.00, 0.94, 0.94, 0.85, 0.75, 0.85, 0.75, 0.75, 0.50, 0.50, 0.35, 0.35, 0.35, 0.35, 0.10, 0.10, 0.10, 0.10, 0.00, 0.00, 0.00, 0.00),
                                                                       c(0.00, 0.00, 0.06, 0.15, 0.00, 0.00, 0.10, 0.25, 0.25, 0.30, 0.00, 0.10, 0.30, 0.65, 0.00, 0.10, 0.30, 0.90, 0.00, 0.10, 0.30, 1.00),
                                                                       c(0.00, 0.06, 0.00, 0.00, 0.25, 0.15, 0.15, 0.00, 0.25, 0.20, 0.65, 0.55, 0.35, 0.00, 0.90, 0.80, 0.60, 0.00, 1.00, 0.90, 0.70, 0.00)),
                                                                  .Names = c("CLAY","SILT","SAND"),class="data.frame",row.names=c(NA,-23L)),
                      structure(list(structure(list("Karbonatisch/Dolomitisch (rückstandsarm)", c(1,2,3)), .Names = c("name", "points")),
                                     structure(list("Karbonatisch/Dolomitisch", c(2,3,4,6)), .Names = c("name", "points")),
                                     structure(list("Karbonatisch (rückstandsreich)", c(6,4,8,7,5)), .Names = c("name", "points")),
                                     structure(list("Silikatisch-karbonatreich (rückstandsarm)", c(5,7,12,11)), .Names = c("name", "points")),
                                     structure(list("Silikatisch-karbonatreich",c(7,8,9,10,13,12)), .Names = c("name", "points")),
                                     structure(list("Silikatisch-karbonatreich (rückstandsreich)", c(8,14,13,10,9)), .Names = c("name", "points")),
                                     structure(list("Silikatisch-karbonatarm (rückstandsarm)", c(11,12,16,15)), .Names = c("name", "points")),
                                     structure(list("Silikatisch-karbonatarm", c(12,13,17,16)), .Names = c("name", "points")),
                                     structure(list("Silikatisch-karbonatarm (rückstandsreich)", c(13,14,18,17)), .Names = c("name", "points")),
                                     structure(list("Silikatisch-quarzreich/rückstandsarm", c(15,16,20,19)), .Names = c("name", "points")),
                                     structure(list("Silikatisch-intermediär", c(16,17,21,20)), .Names = c("name", "points")),
                                     structure(list("Silikatisch-intermediär (rückstandsreich)", c(17,18,22,21)), .Names = c("name", "points"))),
                                .Names = c("D-;K-", "D0;K0", "D+;K+", "C-", "C0", "C+", "M-", "M0", "M+", "S-/S0", "I-/I0", "I+;B*")),
                      blr.tx=c("SAND","CLAY","SILT"),
                      blr.clock=c(TRUE,TRUE,TRUE),
                      tlr.an=c(60,60,60),
                      c(0, 2, 50, 2000), 
                      c(0, 2, 50, 2000), 
                      quote(bold(mu) * bold("m")), quote(bold("%")), 100),
                 .Names = c("main", "tt.points", "tt.polygons","blr.tx","blr.clock","tlr.an","base.css.ps.lim", "tri.css.ps.lim", "unit.ps", 
                            "unit.tx", "text.sum"))

save(tat, file="T:/WP_T3_Case_studies_Biomass/substrat_deck/dreieck2.rda")


################dreieck3###############################################################
library("soiltexture")
tap <- structure(list("Substratgesellschaftsdreieck 3",structure(list(c(1.00, 0.95, 0.80, 0.85, 0.90, 0.70, 0.60, 0.75, 0.35, 0.35, 0.45, 0.10, 0.00, 0.00, 0.00, 0.00, 0.00),
                                                                       c(0.00, 0.05, 0.00, 0.05, 0.10, 0.00, 0.10, 0.25, 0.00, 0.15, 0.25, 0.00, 0.00, 0.10, 0.25, 0.70, 1.00),
                                                                       c(0.00, 0.00, 0.20, 0.10, 0.00, 0.30, 0.30, 0.00, 0.65, 0.50, 0.30, 0.90, 1.00, 0.90, 0.75, 0.30, 0.00)),
                                                                  .Names = c("CLAY","SILT","SAND"),class="data.frame",row.names=c(NA,-17L)),
                      structure(list(structure(list("Silikatisch-quarzreich/rückstandsarm", c(1,3,4,2)), .Names = c("name", "points")),
                                     structure(list("Silikatisch-quarzreich", c(2,4,5)), .Names = c("name", "points")),
                                     structure(list("Silikatisch-intermediär (rückstandsarm)", c(3,6,7,5,4)), .Names = c("name", "points")),
                                     structure(list("Silikatisch-intermediär", c(5,7,9,10,11,8,5)), .Names = c("name", "points")),
                                     structure(list("Silikatisch-basenreich (rückstandsarm)", c(6,9,7)), .Names = c("name", "points")),
                                     structure(list("Silikatisch-basenreich",c(9,12,14,15,11,10,9)), .Names = c("name", "points")),
                                     structure(list("Silikatisch-basenreich (rückstandsreich)", c(11,15,16)), .Names = c("name", "points")),
                                     structure(list("Silikatisch-intermediär (rückstandsreich)", c(8,11,16,17,8)), .Names = c("name", "points")),
                                     structure(list("U", c(12,13,14)), .Names = c("name", "points"))),
                                .Names = c("S-", "S0", "I-", "I0", "B-", "B0", "B+", "I+","U")),
                      blr.tx=c("SAND","CLAY","SILT"),
                      blr.clock=c(TRUE,TRUE,TRUE),
                      tlr.an=c(60,60,60),
                      c(0, 2, 50, 2000), 
                      c(0, 2, 50, 2000),
                      quote(bold(mu) * bold("m")), quote(bold("%")), 100),
                 .Names = c("main", "tt.points", "tt.polygons","blr.tx","blr.clock","tlr.an","base.css.ps.lim", "tri.css.ps.lim", 
                            "unit.ps", "unit.tx", "text.sum"))
                            


save(tap, file="T:/WP_T3_Case_studies_Biomass/substrat_deck/dreieck3.rda")
################dreieck1#############################################################

library("soiltexture")
tas <- structure(list("Substratgesellschaftsdreieck 1",structure(list(c(1.00, 0.94, 0.50, 0.50, 0.75, 0.50, 0.35, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.10, 0.5, 0.85, 0.00, 0.6, 0.45, 0.20, 0.20, 0.25, 0.15, 0.05, 0.85, 0.50),
                                                                      c(0.00, 0.06, 0.00, 0.06, 0.25, 0.25, 0.65, 0.00, 0.06, 0.25, 0.65, 0.90, 1.00, 0.90, 0.15, 0.15, 0.15, 0.25, 0.25, 0.50, 0.65, 0.65, 0.65, 0.90, 0.15, 0.15),
                                                                      c(0.00, 0.00, 0.50, 0.44, 0.00, 0.25, 0.00, 1.00, 0.94, 0.75, 0.35, 0.10, 0.00, 0.00, 0.35, 0.00, 0.85, 0.15, 0.30, 0.30, 0.15, 0.10, 0.20, 0.05, 0.00, 0.35)),
                                                                 .Names = c("CLAY","SILT","SAND"),class="data.frame",row.names=c(NA,-14L)),
                      structure(list(structure(list("Dolomitisch rückstandsarm", c(1,3,4,2)), .Names = c("name", "points")),
                                     structure(list("Dolomitisch intermediär", c(2,4,15,25)), .Names = c("name", "points")),
                                     structure(list("Dolomitisch rückstandsreich", c(25,15,6,5)), .Names = c("name", "points")),
                                     structure(list("Karbonatisch rückstandsarm", c(3,8,9,4)), .Names = c("name", "points")),
                                     structure(list("Karbonatisch intermediär", c(4,9,17,26)), .Names = c("name", "points")),
                                     structure(list("Karbonatisch rückstandsreich", c(17,10,6,26)), .Names = c("name", "points")),
                                     structure(list("Silikatisch-karbonatreich (rückstandsarm)", c(5,7,22,21,18)), .Names = c("name", "points")),
                                     structure(list("Silikatisch-karbonatreich (intermediär)",c(18,19,20,21,18)), .Names = c("name", "points")),
                                     structure(list("Silikatisch-karbonatreich (rückstandsreich)", c(19,10,11,21,20,19)), .Names = c("name", "points")),
                                     structure(list("Silikatisch-karbonatarm (rückstandsarm)", c(7,14,24,22)), .Names = c("name", "points")),
                                     structure(list("Silikatisch-karbonatarm (intermediär)",c(22,23,12,24)), .Names = c("name", "points")),
                                     structure(list("Silikatisch-karbonatarm (rückstandsreich)", c(23,11,12)), .Names = c("name", "points")),
                                     structure(list("Silikatisch-basenreich (intermediär)", c(14,12,13)), .Names = c("name", "points"))),
                                     #structure(list("Silikatisch-basenreich (rückstandsreich)",c(22,23,25,24)), .Names = c("name", "points")),
                                     #structure(list("Silikatisch-intermediär (intermediär)", c(24,25,27,26)), .Names = c("name", "points")),
                                     #structure(list("Silikatisch-intermediär (rückstandsreich)", c(26,27,13)), .Names = c("name", "points"))),
                                .Names = c("D-","D0","D+", "K-", "K0", "K+", "C-","C0","C+", "M-","M0","M+","B*;I*")),
                      blr.tx=c("SAND","CLAY","SILT"),
                      blr.clock=c(TRUE,TRUE,TRUE),
                      tlr.an=c(60,60,60),
                      c(0, 2, 50, 2000), 
                      c(0, 2, 50, 2000), 
                      quote(bold(mu) * bold("m")), quote(bold("%")), 100),
                 .Names = c("main", "tt.points", "tt.polygons","blr.tx","blr.clock","tlr.an","base.css.ps.lim", "tri.css.ps.lim", "unit.ps", 
                            "unit.tx", "text.sum"))




save(tas, file="T:/WP_T3_Case_studies_Biomass/substrat_deck/dreieck1.rda")



