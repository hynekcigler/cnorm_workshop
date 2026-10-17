## This is working script extracting data for the workshop. 

## Script BACH_PA_vXX.R must be run first!
## Delete the working space before the live run, or run all the tests in once!!!

load("BACH/all/agregovane.rData")
load("BACH/all/agregovane_indexy.rData")

## Now prepare the dataset and save it

## THEN CLEAN WORKSPACE AND RESTART R!

dat4$group



dat4$vek_kategorie


x <- dat4[dat4$std, c("vek_roky", "group", "vek_kategorie", 
            "T01_M", "T01_IQ", "M06_W", "M06_IQ", 
            "C07_M", "C07_W", "C07_IQ_spravnost", "C07_IQ_cas")]
## T01 - Grafomotorické tempo
## M06 - Slovní úlohy
## CO7 - Diktát se skórem rychlosti (M) a správnosti (W) 


write.csv2(x, file = "c:/Users/175803/Documents/GitHub/cnorm_workshop/data.csv", row.names = FALSE)
