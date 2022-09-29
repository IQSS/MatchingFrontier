## code to prepare `finkel2012` dataset goes here

wd <- getwd()
setwd("~/Downloads")

#archive_psnot is a folder downloaded from https://doi.org/10.7910/DVN/A9LZNV
load("archive_psnot/5.3_Finkel_et_al_example/saved_objects/finkelWorkspace_16jan2019.RData")
# dat is final dataset used

#dat2 is original dataset used in Finkel et al (2012)
dat2 <- haven::read_dta("archive_psnot/5.3_Finkel_et_al_example/FinkelJOP2012/Finkel-Horowitz-Rojo-Mendoza.Replication Materials/Finkel-Horowitz-Rojo.JoP.2012.Replication.Data.dta")

#Add religion variable to dat
dat$relig_w <- haven::as_factor(dat2[rownames(dat),]$relig_w)
rel <- as.numeric(dat$relig_w)
rel[!rel %in% 1:3] <- 4
dat$relig_w <- factor(rel, levels = 1:4, labels = c("protestant", "catholic", "muslim", "other"))

finkel2012 <- dat

# names(finkel2012)
# [1] "uraiadich_k" "age_w"       "churchgo_w"  "groups2_w"   "incomek"
# [6] "leader_w"    "sex"         "educ_w"      "media_k"     "interest_w"
# [11] "polpart1_w"  "group7_w"    "group10_w"   "treat_k"     "know2aa_w"
#      "relig_w"
names(finkel2012) <- c("uraiamedia", "age", "churchgo", "groupactive", "income",
                       "groupleader", "male", "educ", "media", "polinterest",
                       "poldiscuss", "civicgroup", "polparty", "treat", "polknow",
                       "religion")
finkel2012$income <- round(finkel2012$income)
attr(finkel2012, "na.action") <- NULL

for (i in c("uraiamedia", "age", "churchgo", "income",
            "groupleader", "male", "educ", "poldiscuss",
            "civicgroup", "polparty", "treat")) {
  finkel2012[[i]] <- as.integer(finkel2012[[i]])
}

#Reorder
finkel2012 <- finkel2012[c("treat", "uraiamedia", "age", "churchgo", "groupactive", "income",
                           "groupleader", "male", "educ", "religion", "media", "polinterest",
                           "poldiscuss", "civicgroup", "polparty", "polknow")]
setwd(wd)

usethis::use_data(finkel2012, overwrite = TRUE)
