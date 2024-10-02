load(file='36347-0001-Data.rda')
OG_YH <-da36347.0001
#View(OG_YH)

library(prettyR)
library(tidyverse)

OG_YH_test <-OG_YH

levels(OG_YH_test$SERVICES)

#reverse level this order
levels(OG_YH_test$SERVICES)
#OG_YH_test$SERVICES
OG_YH_test$SERVICES <- factor(OG_YH_test$SERVICES, levels=rev(levels(OG_YH_test$SERVICES)))

#adding no answer level
levels(OG_YH_test$SERVICES) <- c(levels(OG_YH_test$SERVICES), '(0) no answer')
OG_YH_test$SERVICES[is.na(OG_YH_test$SERVICES)] <- '(0) no answer'

#Renaming levels so 0 is never thru 5 is morethan once a week
levels(OG_YH_test$SERVICES)[levels(OG_YH_test$SERVICES)=='(5) never'] <- '(1) never'
levels(OG_YH_test$SERVICES)[levels(OG_YH_test$SERVICES)=='(4) rarely'] <- '(2) rarely'
levels(OG_YH_test$SERVICES)[levels(OG_YH_test$SERVICES)=='(3) only on holidays'] <- '(3) only on holidays'
levels(OG_YH_test$SERVICES)[levels(OG_YH_test$SERVICES)=='(2) monthly'] <- '(4) monthly'
levels(OG_YH_test$SERVICES)[levels(OG_YH_test$SERVICES)=='(1) weekly'] <- '(5) weekly'
levels(OG_YH_test$SERVICES)[levels(OG_YH_test$SERVICES)=='(0) more than once a week'] <- '(6) more than once a week'
OG_YH_test$SERVICES <- relevel(OG_YH_test$SERVICES, "(0) no answer")


#adding another level for SMOKING for people who did not answer 1909
levels(OG_YH$SMOKING)
#OG_YH_test$SMOKING
levels(OG_YH_test$SMOKING) <- c(levels(OG_YH_test$SMOKING), '(0) no answer')
OG_YH_test$SMOKING[is.na(OG_YH_test$SMOKING)] <- '(0) no answer'

  #no longer need to shift this up
levels(OG_YH_test$SMOKING)[levels(OG_YH_test$SMOKING)=='(2) often'] <- '(3) often'
levels(OG_YH_test$SMOKING)[levels(OG_YH_test$SMOKING)=='(1) occasionally'] <- '(2) occasionally'
levels(OG_YH_test$SMOKING)[levels(OG_YH_test$SMOKING)=='(0) no'] <- '(1) no'
levels(OG_YH_test$SMOKING)[levels(OG_YH_test$SMOKING)=='(0) no answer'] <- '(0) no answer'

OG_YH_test$SMOKING <- factor(OG_YH_test$SMOKING, levels=c('(0) no answer', '(1) no', '(2) occasionally', '(3) often'))
#making the NA level the bottom level
OG_YH_test$SMOKING <- relevel(OG_YH_test$SMOKING, "(0) no answer")


#adding another level for DRINKING for people who did not answer 1909
levels(OG_YH$DRINKING)
#OG_YH_test$DRINKING
levels(OG_YH_test$DRINKING) <- c(levels(OG_YH_test$DRINKING), '(0) no answer')
OG_YH_test$DRINKING[is.na(OG_YH_test$DRINKING)] <- '(0) no answer'

levels(OG_YH_test$DRINKING)[levels(OG_YH_test$DRINKING)=='(2) often'] <- '(3) often'
levels(OG_YH_test$DRINKING)[levels(OG_YH_test$DRINKING)=='(1) occasionally'] <- '(2) occasionally'
levels(OG_YH_test$DRINKING)[levels(OG_YH_test$DRINKING)=='(0) no'] <- '(1) no'
levels(OG_YH_test$DRINKING)[levels(OG_YH_test$DRINKING)=='(0) no answer'] <- '(0) no answer'
#making the NA level the bottom level
OG_YH_test$DRINKING <- relevel(OG_YH_test$DRINKING, "(0) no answer")

#OG_YH_test$SMOKING <- factor(OG_YH_test$SMOKING, levels=c('(0) no answer', '(1) no', '(2) occasionally', '(3) often'))



#factorCols <- colnames(OG_YH_forfact %>% 
               #          select_if(sapply(., class) %in% c("factor"))) 

characterCols <- colnames(OG_YH_test %>% 
                     select_if(sapply(., class) %in% c("character"))) 


####NEXT STEP - Remove educ rows that are NA

#colSums(is.na(OG_YH_test)) #checking to see how many NAs I have in each column.
#education has 184. Remove those rows
#5894 rows left

#removing rows where EDUCATION of the profile is NA
OG_YH_test <- subset(OG_YH_test, !is.na(OG_YH_test$EDUCATION))


##### NEXT STEP - make a new column for "yes, college graduate" and "no college graduate"
#effects cols 21 and 120 
#unique(OG_YH_test$EDUCATION)

OG_YH_test <- OG_YH_test %>%  mutate(CGRAD = dplyr::case_when(EDUCATION == "(3) college graduate" | EDUCATION == "(4) post-graduate"  ~ 1,
                                                              TRUE ~ 0))

OG_YH_test <- OG_YH_test %>%  mutate(D_CGRAD = dplyr::case_when(D_EDUC == "(3) college graduate" | D_EDUC == "(4) post-graduate"  ~ 1,
                                                                TRUE ~ 0))

#move new columns next to education variables - NOTE how I might have to take into
  #account pre-analysis column placement
OG_YH_test <- OG_YH_test %>% relocate(CGRAD, .after = EDUCATION)
OG_YH_test <- OG_YH_test %>% relocate(D_CGRAD, .after = D_EDUC)

#sapply(OG_YH_test, class) #getting the class of all the columns


#D_MARSTAT_ANY
#create list of binary columns
MarName <- c("D_MARSTAT_SINGLE", "D_MARSTAT_DIV", "D_MARSTAT_WID", 
             "D_MARSTAT_SEP")

#no inherent value so unordered cat is fine
levels(OG_YH_test$"D_MARSTAT_SINGLE") <- c(levels(OG_YH_test[,"D_MARSTAT_SINGLE"]), '(2) no answer')
OG_YH_test$"D_MARSTAT_SINGLE"[is.na(OG_YH_test$"D_MARSTAT_SINGLE") & OG_YH_test[,'D_MARSTAT_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_MARSTAT_SINGLE)) #checking the number of NAs
unique(OG_YH_test$D_MARSTAT_SINGLE)

levels(OG_YH_test$"D_MARSTAT_DIV") <- c(levels(OG_YH_test[,"D_MARSTAT_DIV"]), '(2) no answer')
OG_YH_test$"D_MARSTAT_DIV"[is.na(OG_YH_test$"D_MARSTAT_DIV") & OG_YH_test[,'D_MARSTAT_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_MARSTAT_DIV)) #checking the number of NAs
unique(OG_YH_test$D_MARSTAT_DIV)

levels(OG_YH_test$"D_MARSTAT_WID") <- c(levels(OG_YH_test[,"D_MARSTAT_WID"]), '(2) no answer')
OG_YH_test$"D_MARSTAT_WID"[is.na(OG_YH_test$"D_MARSTAT_WID") & OG_YH_test[,'D_MARSTAT_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_MARSTAT_WID)) #checking the number of NAs
unique(OG_YH_test$D_MARSTAT_WID)

levels(OG_YH_test$"D_MARSTAT_SEP") <- c(levels(OG_YH_test[,"D_MARSTAT_SEP"]), '(2) no answer')
OG_YH_test$"D_MARSTAT_SEP"[is.na(OG_YH_test$"D_MARSTAT_SEP") & OG_YH_test[,'D_MARSTAT_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_MARSTAT_SEP)) #checking the number of NAs
unique(OG_YH_test$D_MARSTAT_SEP)


#D_BODY_ANY
BodyName <- c("D_BODY_SLIM", "D_BODY_SLENDER", "D_BODY_AVERAGE", 
              "D_BODY_ATH", "D_BODY_FIT", "D_BODY_THICK", "D_BODY_EXTRA", 
              "D_BODY_LARGE", "D_BODY_VOLUP", "D_BODY_CURVY")


levels(OG_YH_test$"D_BODY_SLIM") <- c(levels(OG_YH_test[,"D_BODY_SLIM"]), '(2) no answer')
OG_YH_test$"D_BODY_SLIM"[is.na(OG_YH_test$"D_BODY_SLIM") & OG_YH_test[,'D_BODY_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_BODY_SLIM)) #checking the number of NAs
unique(OG_YH_test$D_BODY_SLIM)

levels(OG_YH_test$"D_BODY_SLENDER") <- c(levels(OG_YH_test[,"D_BODY_SLENDER"]), '(2) no answer')
OG_YH_test$"D_BODY_SLENDER"[is.na(OG_YH_test$"D_BODY_SLENDER") & OG_YH_test[,'D_BODY_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_BODY_SLENDER)) #checking the number of NAs
unique(OG_YH_test$D_BODY_SLENDER)

levels(OG_YH_test$"D_BODY_AVERAGE") <- c(levels(OG_YH_test[,"D_BODY_AVERAGE"]), '(2) no answer')
OG_YH_test$"D_BODY_AVERAGE"[is.na(OG_YH_test$"D_BODY_AVERAGE") & OG_YH_test[,'D_BODY_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_BODY_AVERAGE)) #checking the number of NAs
unique(OG_YH_test$D_BODY_AVERAGE)

levels(OG_YH_test$"D_BODY_ATH") <- c(levels(OG_YH_test[,"D_BODY_ATH"]), '(2) no answer')
OG_YH_test$"D_BODY_ATH"[is.na(OG_YH_test$"D_BODY_ATH") & OG_YH_test[,'D_BODY_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_BODY_ATH)) #checking the number of NAs
unique(OG_YH_test$D_BODY_ATH)

levels(OG_YH_test$"D_BODY_FIT") <- c(levels(OG_YH_test[,"D_BODY_FIT"]), '(2) no answer')
OG_YH_test$"D_BODY_FIT"[is.na(OG_YH_test$"D_BODY_FIT") & OG_YH_test[,'D_BODY_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_BODY_FIT)) #checking the number of NAs
unique(OG_YH_test$D_BODY_FIT)

levels(OG_YH_test$"D_BODY_THICK") <- c(levels(OG_YH_test[,"D_BODY_THICK"]), '(2) no answer')
OG_YH_test$"D_BODY_THICK"[is.na(OG_YH_test$"D_BODY_THICK") & OG_YH_test[,'D_BODY_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_BODY_THICK)) #checking the number of NAs
unique(OG_YH_test$D_BODY_THICK)

levels(OG_YH_test$"D_BODY_EXTRA") <- c(levels(OG_YH_test[,"D_BODY_EXTRA"]), '(2) no answer')
OG_YH_test$"D_BODY_EXTRA"[is.na(OG_YH_test$"D_BODY_EXTRA") & OG_YH_test[,'D_BODY_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_BODY_EXTRA)) #checking the number of NAs
unique(OG_YH_test$D_BODY_EXTRA)

levels(OG_YH_test$"D_BODY_LARGE") <- c(levels(OG_YH_test[,"D_BODY_LARGE"]), '(2) no answer')
OG_YH_test$"D_BODY_LARGE"[is.na(OG_YH_test$"D_BODY_LARGE") & OG_YH_test[,'D_BODY_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_BODY_LARGE)) #checking the number of NAs
unique(OG_YH_test$D_BODY_LARGE)

levels(OG_YH_test$"D_BODY_VOLUP") <- c(levels(OG_YH_test[,"D_BODY_VOLUP"]), '(2) no answer')
OG_YH_test$"D_BODY_VOLUP"[is.na(OG_YH_test$"D_BODY_VOLUP") & OG_YH_test[,'D_BODY_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_BODY_VOLUP)) #checking the number of NAs
unique(OG_YH_test$D_BODY_VOLUP)

levels(OG_YH_test$"D_BODY_CURVY") <- c(levels(OG_YH_test[,"D_BODY_CURVY"]), '(2) no answer')
OG_YH_test$"D_BODY_CURVY"[is.na(OG_YH_test$"D_BODY_CURVY") & OG_YH_test[,'D_BODY_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_BODY_CURVY)) #checking the number of NAs
unique(OG_YH_test$D_BODY_CURVY)


#D_EYES_ANY
EyeName <- c("D_EYES_BLACK", "D_EYES_BLUE", "D_EYES_BROWN", 
             "D_EYES_GRAY", "D_EYES_GREEN", "D_EYES_HAZEL")

levels(OG_YH_test$"D_EYES_BLACK") <- c(levels(OG_YH_test[,"D_EYES_BLACK"]), '(2) no answer')
OG_YH_test$"D_EYES_BLACK"[is.na(OG_YH_test$"D_EYES_BLACK") & OG_YH_test[,'D_EYES_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_EYES_BLACK)) #checking the number of NAs
unique(OG_YH_test$D_EYES_BLACK)

levels(OG_YH_test$"D_EYES_BLUE") <- c(levels(OG_YH_test[,"D_EYES_BLUE"]), '(2) no answer')
OG_YH_test$"D_EYES_BLUE"[is.na(OG_YH_test$"D_EYES_BLUE") & OG_YH_test[,'D_EYES_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_EYES_BLUE)) #checking the number of NAs
unique(OG_YH_test$D_EYES_BLUE)

levels(OG_YH_test$"D_EYES_BROWN") <- c(levels(OG_YH_test[,"D_EYES_BROWN"]), '(2) no answer')
OG_YH_test$"D_EYES_BROWN"[is.na(OG_YH_test$"D_EYES_BROWN") & OG_YH_test[,'D_EYES_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_EYES_BROWN)) #checking the number of NAs
unique(OG_YH_test$D_EYES_BROWN)

levels(OG_YH_test$"D_EYES_GRAY") <- c(levels(OG_YH_test[,"D_EYES_GRAY"]), '(2) no answer')
OG_YH_test$"D_EYES_GRAY"[is.na(OG_YH_test$"D_EYES_GRAY") & OG_YH_test[,'D_EYES_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_EYES_GRAY)) #checking the number of NAs
unique(OG_YH_test$D_EYES_GRAY)

levels(OG_YH_test$"D_EYES_GREEN") <- c(levels(OG_YH_test[,"D_EYES_GREEN"]), '(2) no answer')
OG_YH_test$"D_EYES_GREEN"[is.na(OG_YH_test$"D_EYES_GREEN") & OG_YH_test[,'D_EYES_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_EYES_GREEN)) #checking the number of NAs
unique(OG_YH_test$D_EYES_GREEN)

levels(OG_YH_test$"D_EYES_HAZEL") <- c(levels(OG_YH_test[,"D_EYES_HAZEL"]), '(2) no answer')
OG_YH_test$"D_EYES_HAZEL"[is.na(OG_YH_test$"D_EYES_HAZEL") & OG_YH_test[,'D_EYES_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_EYES_HAZEL)) #checking the number of NAs
unique(OG_YH_test$D_EYES_HAZEL)

#D_HAIR_ANY
HairName <- c("D_HAIR_AUBURN", "D_HAIR_BLACK", "D_HAIR_BLONDE", 
              "D_HAIR_DARKBLONDE", "D_HAIR_LIGHTBROWN", "D_HAIR_DARKBROWN", "D_HAIR_RED",
              "D_HAIR_WHITE", "D_HAIR_BALD", "D_HAIR_LITGRAY") #10

levels(OG_YH_test$"D_HAIR_AUBURN") <- c(levels(OG_YH_test[,"D_HAIR_AUBURN"]), '(2) no answer')
OG_YH_test$"D_HAIR_AUBURN"[is.na(OG_YH_test$"D_HAIR_AUBURN") & OG_YH_test[,'D_HAIR_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_HAIR_AUBURN)) #checking the number of NAs
unique(OG_YH_test$D_HAIR_AUBURN)

levels(OG_YH_test$"D_HAIR_BLACK") <- c(levels(OG_YH_test[,"D_HAIR_BLACK"]), '(2) no answer')
OG_YH_test$"D_HAIR_BLACK"[is.na(OG_YH_test$"D_HAIR_BLACK") & OG_YH_test[,'D_HAIR_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_HAIR_BLACK)) #checking the number of NAs
unique(OG_YH_test$D_HAIR_BLACK)

levels(OG_YH_test$"D_HAIR_BLONDE") <- c(levels(OG_YH_test[,"D_HAIR_BLONDE"]), '(2) no answer')
OG_YH_test$"D_HAIR_BLONDE"[is.na(OG_YH_test$"D_HAIR_BLONDE") & OG_YH_test[,'D_HAIR_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_HAIR_BLONDE)) #checking the number of NAs
unique(OG_YH_test$D_HAIR_BLONDE)

levels(OG_YH_test$"D_HAIR_DARKBLONDE") <- c(levels(OG_YH_test[,"D_HAIR_DARKBLONDE"]), '(2) no answer')
OG_YH_test$"D_HAIR_DARKBLONDE"[is.na(OG_YH_test$"D_HAIR_DARKBLONDE") & OG_YH_test[,'D_HAIR_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_HAIR_DARKBLONDE)) #checking the number of NAs
unique(OG_YH_test$D_HAIR_DARKBLONDE)

levels(OG_YH_test$"D_HAIR_LIGHTBROWN") <- c(levels(OG_YH_test[,"D_HAIR_LIGHTBROWN"]), '(2) no answer')
OG_YH_test$"D_HAIR_LIGHTBROWN"[is.na(OG_YH_test$"D_HAIR_LIGHTBROWN") & OG_YH_test[,'D_HAIR_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_HAIR_LIGHTBROWN)) #checking the number of NAs
unique(OG_YH_test$D_HAIR_LIGHTBROWN)

levels(OG_YH_test$"D_HAIR_DARKBROWN") <- c(levels(OG_YH_test[,"D_HAIR_DARKBROWN"]), '(2) no answer')
OG_YH_test$"D_HAIR_DARKBROWN"[is.na(OG_YH_test$"D_HAIR_DARKBROWN") & OG_YH_test[,'D_HAIR_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_HAIR_DARKBROWN)) #checking the number of NAs
unique(OG_YH_test$D_HAIR_DARKBROWN)

levels(OG_YH_test$"D_HAIR_RED") <- c(levels(OG_YH_test[,"D_HAIR_RED"]), '(2) no answer')
OG_YH_test$"D_HAIR_RED"[is.na(OG_YH_test$"D_HAIR_RED") & OG_YH_test[,'D_HAIR_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_HAIR_RED)) #checking the number of NAs
unique(OG_YH_test$D_HAIR_RED)

levels(OG_YH_test$"D_HAIR_WHITE") <- c(levels(OG_YH_test[,"D_HAIR_WHITE"]), '(2) no answer')
OG_YH_test$"D_HAIR_WHITE"[is.na(OG_YH_test$"D_HAIR_WHITE") & OG_YH_test[,'D_HAIR_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_HAIR_WHITE)) #checking the number of NAs
unique(OG_YH_test$D_HAIR_WHITE)

levels(OG_YH_test$"D_HAIR_BALD") <- c(levels(OG_YH_test[,"D_HAIR_BALD"]), '(2) no answer')
OG_YH_test$"D_HAIR_BALD"[is.na(OG_YH_test$"D_HAIR_BALD") & OG_YH_test[,'D_HAIR_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_HAIR_BALD)) #checking the number of NAs
unique(OG_YH_test$D_HAIR_BALD)

levels(OG_YH_test$"D_HAIR_LITGRAY") <- c(levels(OG_YH_test[,"D_HAIR_LITGRAY"]), '(2) no answer')
OG_YH_test$"D_HAIR_LITGRAY"[is.na(OG_YH_test$"D_HAIR_LITGRAY") & OG_YH_test[,'D_HAIR_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_HAIR_LITGRAY)) #checking the number of NAs
unique(OG_YH_test$D_HAIR_LITGRAY)


#D_ETHNIC_ANY
EthName <- c("D_ETHNIC_BLACK", "D_ETHNIC_ASIAN", "D_ETHNIC_WHITE", 
             "D_ETHNIC_EINDIAN", "D_ETHNIC_LATINO", "D_ETHNIC_MIDEAST", "D_ETHNIC_NATIVE",
             "D_ETHNIC_PACIFIC", "D_ETHNIC_INTER", "D_ETHNIC_OTHER") #10

levels(OG_YH_test$"D_ETHNIC_BLACK") <- c(levels(OG_YH_test[,"D_ETHNIC_BLACK"]), '(2) no answer')
OG_YH_test$"D_ETHNIC_BLACK"[is.na(OG_YH_test$"D_ETHNIC_BLACK") & OG_YH_test[,'D_ETHNIC_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_ETHNIC_BLACK)) #checking the number of NAs
unique(OG_YH_test$D_ETHNIC_BLACK)

levels(OG_YH_test$"D_ETHNIC_ASIAN") <- c(levels(OG_YH_test[,"D_ETHNIC_ASIAN"]), '(2) no answer')
OG_YH_test$"D_ETHNIC_ASIAN"[is.na(OG_YH_test$"D_ETHNIC_ASIAN") & OG_YH_test[,'D_ETHNIC_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_ETHNIC_ASIAN)) #checking the number of NAs
unique(OG_YH_test$D_ETHNIC_ASIAN)

levels(OG_YH_test$"D_ETHNIC_WHITE") <- c(levels(OG_YH_test[,"D_ETHNIC_WHITE"]), '(2) no answer')
OG_YH_test$"D_ETHNIC_WHITE"[is.na(OG_YH_test$"D_ETHNIC_WHITE") & OG_YH_test[,'D_ETHNIC_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_ETHNIC_WHITE)) #checking the number of NAs
unique(OG_YH_test$D_ETHNIC_WHITE)

levels(OG_YH_test$"D_ETHNIC_EINDIAN") <- c(levels(OG_YH_test[,"D_ETHNIC_EINDIAN"]), '(2) no answer')
OG_YH_test$"D_ETHNIC_EINDIAN"[is.na(OG_YH_test$"D_ETHNIC_EINDIAN") & OG_YH_test[,'D_ETHNIC_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_ETHNIC_EINDIAN)) #checking the number of NAs
unique(OG_YH_test$D_ETHNIC_EINDIAN)

levels(OG_YH_test$"D_ETHNIC_LATINO") <- c(levels(OG_YH_test[,"D_ETHNIC_LATINO"]), '(2) no answer')
OG_YH_test$"D_ETHNIC_LATINO"[is.na(OG_YH_test$"D_ETHNIC_LATINO") & OG_YH_test[,'D_ETHNIC_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_ETHNIC_LATINO)) #checking the number of NAs
unique(OG_YH_test$D_ETHNIC_LATINO)

levels(OG_YH_test$"D_ETHNIC_MIDEAST") <- c(levels(OG_YH_test[,"D_ETHNIC_MIDEAST"]), '(2) no answer')
OG_YH_test$"D_ETHNIC_MIDEAST"[is.na(OG_YH_test$"D_ETHNIC_MIDEAST") & OG_YH_test[,'D_ETHNIC_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_ETHNIC_MIDEAST)) #checking the number of NAs
unique(OG_YH_test$D_ETHNIC_MIDEAST)

levels(OG_YH_test$"D_ETHNIC_NATIVE") <- c(levels(OG_YH_test[,"D_ETHNIC_NATIVE"]), '(2) no answer')
OG_YH_test$"D_ETHNIC_NATIVE"[is.na(OG_YH_test$"D_ETHNIC_NATIVE") & OG_YH_test[,'D_ETHNIC_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_ETHNIC_NATIVE)) #checking the number of NAs
unique(OG_YH_test$D_ETHNIC_NATIVE)

levels(OG_YH_test$"D_ETHNIC_PACIFIC") <- c(levels(OG_YH_test[,"D_ETHNIC_PACIFIC"]), '(2) no answer')
OG_YH_test$"D_ETHNIC_PACIFIC"[is.na(OG_YH_test$"D_ETHNIC_PACIFIC") & OG_YH_test[,'D_ETHNIC_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_ETHNIC_PACIFIC)) #checking the number of NAs
unique(OG_YH_test$D_ETHNIC_PACIFIC)

levels(OG_YH_test$"D_ETHNIC_INTER") <- c(levels(OG_YH_test[,"D_ETHNIC_INTER"]), '(2) no answer')
OG_YH_test$"D_ETHNIC_INTER"[is.na(OG_YH_test$"D_ETHNIC_INTER") & OG_YH_test[,'D_ETHNIC_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_ETHNIC_INTER)) #checking the number of NAs
unique(OG_YH_test$D_ETHNIC_INTER)

levels(OG_YH_test$"D_ETHNIC_OTHER") <- c(levels(OG_YH_test[,"D_ETHNIC_OTHER"]), '(2) no answer')
OG_YH_test$"D_ETHNIC_OTHER"[is.na(OG_YH_test$"D_ETHNIC_OTHER") & OG_YH_test[,'D_ETHNIC_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_ETHNIC_OTHER)) #checking the number of NAs
unique(OG_YH_test$D_ETHNIC_OTHER)

#D_SMOKE_ANY
SmkName <- c("D_SMOKE_NO", "D_SMOKE_OCC", "D_SMOKE_OFTEN")

levels(OG_YH_test$"D_SMOKE_NO") <- c(levels(OG_YH_test[,"D_SMOKE_NO"]), '(2) no answer')
OG_YH_test$"D_SMOKE_NO"[is.na(OG_YH_test$"D_SMOKE_NO") & OG_YH_test[,'D_SMOKE_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_SMOKE_NO)) #checking the number of NAs
unique(OG_YH_test$D_SMOKE_NO)

levels(OG_YH_test$"D_SMOKE_OCC") <- c(levels(OG_YH_test[,"D_SMOKE_OCC"]), '(2) no answer')
OG_YH_test$"D_SMOKE_OCC"[is.na(OG_YH_test$"D_SMOKE_OCC") & OG_YH_test[,'D_SMOKE_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_SMOKE_OCC)) #checking the number of NAs
unique(OG_YH_test$D_SMOKE_OCC)

levels(OG_YH_test$"D_SMOKE_OFTEN") <- c(levels(OG_YH_test[,"D_SMOKE_OFTEN"]), '(2) no answer')
OG_YH_test$"D_SMOKE_OFTEN"[is.na(OG_YH_test$"D_SMOKE_OFTEN") & OG_YH_test[,'D_SMOKE_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_SMOKE_OFTEN)) #checking the number of NAs
unique(OG_YH_test$D_SMOKE_OFTEN)

#D_DRINK_ANY
DrnkName <- c("D_DRINK_NO", "D_DRINK_SOCIAL", "D_DRINK_REG")

levels(OG_YH_test$"D_DRINK_NO") <- c(levels(OG_YH_test[,"D_DRINK_NO"]), '(2) no answer')
OG_YH_test$"D_DRINK_NO"[is.na(OG_YH_test$"D_DRINK_NO") & OG_YH_test[,'D_DRINK_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_DRINK_NO)) #checking the number of NAs
unique(OG_YH_test$D_DRINK_NO)

levels(OG_YH_test$"D_DRINK_SOCIAL") <- c(levels(OG_YH_test[,"D_DRINK_SOCIAL"]), '(2) no answer')
OG_YH_test$"D_DRINK_SOCIAL"[is.na(OG_YH_test$"D_DRINK_SOCIAL") & OG_YH_test[,'D_DRINK_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_DRINK_SOCIAL)) #checking the number of NAs
unique(OG_YH_test$D_DRINK_SOCIAL)

levels(OG_YH_test$"D_DRINK_REG") <- c(levels(OG_YH_test[,"D_DRINK_REG"]), '(2) no answer')
OG_YH_test$"D_DRINK_REG"[is.na(OG_YH_test$"D_DRINK_REG") & OG_YH_test[,'D_DRINK_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_DRINK_REG)) #checking the number of NAs
unique(OG_YH_test$D_DRINK_REG)


#D_LIVING_ANY
DlivName <- c("D_LIVING_ALONE", "D_LIVING_KIDS", "D_LIVING_PARENTS", "D_LIVING_PETS",
              "D_LIVING_ROOMMATE", "D_LIVING_FAMILY", "D_LIVING_PARTY")

levels(OG_YH_test$"D_LIVING_ALONE") <- c(levels(OG_YH_test[,"D_LIVING_ALONE"]), '(2) no answer')
OG_YH_test$"D_LIVING_ALONE"[is.na(OG_YH_test$"D_LIVING_ALONE") & OG_YH_test[,'D_LIVING_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_LIVING_ALONE)) #checking the number of NAs
unique(OG_YH_test$D_LIVING_ALONE)

levels(OG_YH_test$"D_LIVING_KIDS") <- c(levels(OG_YH_test[,"D_LIVING_KIDS"]), '(2) no answer')
OG_YH_test$"D_LIVING_KIDS"[is.na(OG_YH_test$"D_LIVING_KIDS") & OG_YH_test[,'D_LIVING_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_LIVING_KIDS)) #checking the number of NAs
unique(OG_YH_test$D_LIVING_KIDS)

levels(OG_YH_test$"D_LIVING_PARENTS") <- c(levels(OG_YH_test[,"D_LIVING_PARENTS"]), '(2) no answer')
OG_YH_test$"D_LIVING_PARENTS"[is.na(OG_YH_test$"D_LIVING_PARENTS") & OG_YH_test[,'D_LIVING_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_LIVING_PARENTS)) #checking the number of NAs
unique(OG_YH_test$D_LIVING_PARENTS)

levels(OG_YH_test$"D_LIVING_PETS") <- c(levels(OG_YH_test[,"D_LIVING_PETS"]), '(2) no answer')
OG_YH_test$"D_LIVING_PETS"[is.na(OG_YH_test$"D_LIVING_PETS") & OG_YH_test[,'D_LIVING_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_LIVING_PETS)) #checking the number of NAs
unique(OG_YH_test$D_LIVING_PETS)

levels(OG_YH_test$"D_LIVING_ROOMMATE") <- c(levels(OG_YH_test[,"D_LIVING_ROOMMATE"]), '(2) no answer')
OG_YH_test$"D_LIVING_ROOMMATE"[is.na(OG_YH_test$"D_LIVING_ROOMMATE") & OG_YH_test[,'D_LIVING_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_LIVING_ROOMMATE)) #checking the number of NAs
unique(OG_YH_test$D_LIVING_ROOMMATE)

levels(OG_YH_test$"D_LIVING_FAMILY") <- c(levels(OG_YH_test[,"D_LIVING_FAMILY"]), '(2) no answer')
OG_YH_test$"D_LIVING_FAMILY"[is.na(OG_YH_test$"D_LIVING_FAMILY") & OG_YH_test[,'D_LIVING_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_LIVING_FAMILY)) #checking the number of NAs
unique(OG_YH_test$D_LIVING_FAMILY)

levels(OG_YH_test$"D_LIVING_PARTY") <- c(levels(OG_YH_test[,"D_LIVING_PARTY"]), '(2) no answer')
OG_YH_test$"D_LIVING_PARTY"[is.na(OG_YH_test$"D_LIVING_PARTY") & OG_YH_test[,'D_LIVING_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_LIVING_PARTY)) #checking the number of NAs
unique(OG_YH_test$D_LIVING_PARTY)

colSums(is.na(OG_YH_test))

#D_KIDS_ANY - Do seperately - G0 BACK TO. I think it's okay making this one numeric
#OG_YH_test$D_KIDS_NO <- ifelse(OG_YH_test$D_KIDS_ANY == 1, 0, OG_YH_test$D_KIDS_NO) #if you're okay with any kids, they you would say "no, I don't want to get rid of people how do not have kids"
        #or it's like if you select "any", then you don't have the explicit preference to date some without kids
#OG_YH_test$D_KIDS_HOMEFT <- ifelse(OG_YH_test$D_KIDS_ANY == 1, 1, OG_YH_test$D_KIDS_HOMEFT)
#OG_YH_test$D_KIDS_HOMEPT <- ifelse(OG_YH_test$D_KIDS_ANY == 1, 1, OG_YH_test$D_KIDS_HOMEPT)
#OG_YH_test$D_KIDS_NOTHOME <- ifelse(OG_YH_test$D_KIDS_ANY == 1, 1, OG_YH_test$D_KIDS_NOTHOME)

levels(OG_YH_test$"D_KIDS_NO") <- c(levels(OG_YH_test[,"D_KIDS_NO"]), '(2) no answer')
OG_YH_test$"D_KIDS_NO"[is.na(OG_YH_test$"D_KIDS_NO") & OG_YH_test[,'D_KIDS_ANY'] == "(1) yes"] <- '(2) no answer'

levels(OG_YH_test$"D_KIDS_HOMEFT") <- c(levels(OG_YH_test[,"D_KIDS_HOMEFT"]), '(2) no answer')
OG_YH_test$"D_KIDS_HOMEFT"[is.na(OG_YH_test$"D_KIDS_HOMEFT") & OG_YH_test[,'D_KIDS_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_LIVING_PARTY)) #checking the number of NAs
unique(OG_YH_test$D_LIVING_PARTY)

levels(OG_YH_test$"D_KIDS_HOMEPT") <- c(levels(OG_YH_test[,"D_KIDS_HOMEPT"]), '(2) no answer')
OG_YH_test$"D_KIDS_HOMEPT"[is.na(OG_YH_test$"D_KIDS_HOMEPT") & OG_YH_test[,'D_KIDS_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_LIVING_PARTY)) #checking the number of NAs
unique(OG_YH_test$D_LIVING_PARTY)

levels(OG_YH_test$"D_KIDS_NOTHOME") <- c(levels(OG_YH_test[,"D_KIDS_NOTHOME"]), '(2) no answer')
OG_YH_test$"D_KIDS_NOTHOME"[is.na(OG_YH_test$"D_KIDS_NOTHOME") & OG_YH_test[,'D_KIDS_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_LIVING_PARTY)) #checking the number of NAs
unique(OG_YH_test$D_LIVING_PARTY)
  #basically, anyone with a "no answer" doesn't have that explicit preference. They don't care. If they didn't select "Any", they
      #could be okay with anything. Hypothetically, they all could be "yes"
    


#D_WANTKIDS_ANY
DwantkdName <- c("D_WANTKIDS_NO", "D_WANTKIDS_YES", "D_WANTKIDS_NOTSURE")

levels(OG_YH_test$"D_WANTKIDS_NO") <- c(levels(OG_YH_test[,"D_WANTKIDS_NO"]), '(2) no answer')
OG_YH_test$"D_WANTKIDS_NO"[is.na(OG_YH_test$"D_WANTKIDS_NO") & OG_YH_test[,'D_WANTKIDS_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_WANTKIDS_NO)) #checking the number of NAs
unique(OG_YH_test$D_WANTKIDS_NO)

levels(OG_YH_test$"D_WANTKIDS_YES") <- c(levels(OG_YH_test[,"D_WANTKIDS_YES"]), '(2) no answer')
OG_YH_test$"D_WANTKIDS_YES"[is.na(OG_YH_test$"D_WANTKIDS_YES") & OG_YH_test[,'D_WANTKIDS_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_WANTKIDS_YES)) #checking the number of NAs
unique(OG_YH_test$D_WANTKIDS_YES)

levels(OG_YH_test$"D_WANTKIDS_NOTSURE") <- c(levels(OG_YH_test[,"D_WANTKIDS_NOTSURE"]), '(2) no answer')
OG_YH_test$"D_WANTKIDS_NOTSURE"[is.na(OG_YH_test$"D_WANTKIDS_NOTSURE") & OG_YH_test[,'D_WANTKIDS_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_WANTKIDS_NOTSURE)) #checking the number of NAs
unique(OG_YH_test$D_WANTKIDS_NOTSURE)

#D_EMPSTAT_ANY
DEmpStName <- c("D_EMPSTAT_FT", "D_EMPSTAT_PT", "D_EMPSTAT_HOME", "D_EMPSTAT_RETIRED",
                "D_EMPSTAT_SELF", "D_EMPSTAT_STUDENT", "D_EMPSTAT_UNEMP",
                "D_EMPSTAT_ATHOME") #8


levels(OG_YH_test$"D_EMPSTAT_FT") <- c(levels(OG_YH_test[,"D_EMPSTAT_FT"]), '(2) no answer')
OG_YH_test$"D_EMPSTAT_FT"[is.na(OG_YH_test$"D_EMPSTAT_FT") & OG_YH_test[,'D_EMPSTAT_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_EMPSTAT_FT)) #checking the number of NAs
unique(OG_YH_test$D_EMPSTAT_FT)

levels(OG_YH_test$"D_EMPSTAT_PT") <- c(levels(OG_YH_test[,"D_EMPSTAT_PT"]), '(2) no answer')
OG_YH_test$"D_EMPSTAT_PT"[is.na(OG_YH_test$"D_EMPSTAT_PT") & OG_YH_test[,'D_EMPSTAT_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_EMPSTAT_PT)) #checking the number of NAs
unique(OG_YH_test$D_EMPSTAT_PT)

levels(OG_YH_test$"D_EMPSTAT_HOME") <- c(levels(OG_YH_test[,"D_EMPSTAT_HOME"]), '(2) no answer')
OG_YH_test$"D_EMPSTAT_HOME"[is.na(OG_YH_test$"D_EMPSTAT_HOME") & OG_YH_test[,'D_EMPSTAT_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_EMPSTAT_HOME)) #checking the number of NAs
unique(OG_YH_test$D_EMPSTAT_HOME)

levels(OG_YH_test$"D_EMPSTAT_RETIRED") <- c(levels(OG_YH_test[,"D_EMPSTAT_RETIRED"]), '(2) no answer')
OG_YH_test$"D_EMPSTAT_RETIRED"[is.na(OG_YH_test$"D_EMPSTAT_RETIRED") & OG_YH_test[,'D_EMPSTAT_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_EMPSTAT_RETIRED)) #checking the number of NAs
unique(OG_YH_test$D_EMPSTAT_RETIRED)

levels(OG_YH_test$"D_EMPSTAT_SELF") <- c(levels(OG_YH_test[,"D_EMPSTAT_SELF"]), '(2) no answer')
OG_YH_test$"D_EMPSTAT_SELF"[is.na(OG_YH_test$"D_EMPSTAT_SELF") & OG_YH_test[,'D_EMPSTAT_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_EMPSTAT_SELF)) #checking the number of NAs
unique(OG_YH_test$D_EMPSTAT_SELF)

levels(OG_YH_test$"D_EMPSTAT_STUDENT") <- c(levels(OG_YH_test[,"D_EMPSTAT_STUDENT"]), '(2) no answer')
OG_YH_test$"D_EMPSTAT_STUDENT"[is.na(OG_YH_test$"D_EMPSTAT_STUDENT") & OG_YH_test[,'D_EMPSTAT_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_EMPSTAT_STUDENT)) #checking the number of NAs
unique(OG_YH_test$D_EMPSTAT_STUDENT)

levels(OG_YH_test$"D_EMPSTAT_UNEMP") <- c(levels(OG_YH_test[,"D_EMPSTAT_UNEMP"]), '(2) no answer')
OG_YH_test$"D_EMPSTAT_UNEMP"[is.na(OG_YH_test$"D_EMPSTAT_UNEMP") & OG_YH_test[,'D_EMPSTAT_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_EMPSTAT_UNEMP)) #checking the number of NAs
unique(OG_YH_test$D_EMPSTAT_UNEMP)

levels(OG_YH_test$"D_EMPSTAT_ATHOME") <- c(levels(OG_YH_test[,"D_EMPSTAT_ATHOME"]), '(2) no answer')
OG_YH_test$"D_EMPSTAT_ATHOME"[is.na(OG_YH_test$"D_EMPSTAT_ATHOME") & OG_YH_test[,'D_EMPSTAT_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_EMPSTAT_ATHOME)) #checking the number of NAs
unique(OG_YH_test$D_EMPSTAT_ATHOME)


#D_OCC_ANY
DOccName <- c("D_OCC_ART", "D_OCC_BANK", "D_OCC_CLER", "D_OCC_COMP", "D_OCC_CONSTR",
              "D_OCC_EDUC", "D_OCC_MEDIA", "D_OCC_EXEC", "D_OCC_TRAVEL", "D_OCC_LEGAL",
              "D_OCC_MANU", "D_OCC_MED", "D_OCC_POLITICS", "D_OCC_SALES", "D_OCC_TECH",
              "D_OCC_TRANSPORT", "D_OCC_FOOD", "D_OCC_OTHER")


levels(OG_YH_test$"D_OCC_ART") <- c(levels(OG_YH_test[,"D_OCC_ART"]), '(2) no answer')
OG_YH_test$"D_OCC_ART"[is.na(OG_YH_test$"D_OCC_ART") & OG_YH_test[,'D_OCC_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_OCC_ART)) #checking the number of NAs
unique(OG_YH_test$D_OCC_ART)

levels(OG_YH_test$"D_OCC_BANK") <- c(levels(OG_YH_test[,"D_OCC_BANK"]), '(2) no answer')
OG_YH_test$"D_OCC_BANK"[is.na(OG_YH_test$"D_OCC_BANK") & OG_YH_test[,'D_OCC_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_OCC_BANK)) #checking the number of NAs
unique(OG_YH_test$D_OCC_BANK)

levels(OG_YH_test$"D_OCC_CLER") <- c(levels(OG_YH_test[,"D_OCC_CLER"]), '(2) no answer')
OG_YH_test$"D_OCC_CLER"[is.na(OG_YH_test$"D_OCC_CLER") & OG_YH_test[,'D_OCC_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_OCC_CLER)) #checking the number of NAs
unique(OG_YH_test$D_OCC_CLER)

levels(OG_YH_test$"D_OCC_COMP") <- c(levels(OG_YH_test[,"D_OCC_COMP"]), '(2) no answer')
OG_YH_test$"D_OCC_COMP"[is.na(OG_YH_test$"D_OCC_COMP") & OG_YH_test[,'D_OCC_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_OCC_COMP)) #checking the number of NAs
unique(OG_YH_test$D_OCC_COMP)

levels(OG_YH_test$"D_OCC_CONSTR") <- c(levels(OG_YH_test[,"D_OCC_CONSTR"]), '(2) no answer')
OG_YH_test$"D_OCC_CONSTR"[is.na(OG_YH_test$"D_OCC_CONSTR") & OG_YH_test[,'D_OCC_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_OCC_CONSTR)) #checking the number of NAs
unique(OG_YH_test$D_OCC_CONSTR)

levels(OG_YH_test$"D_OCC_EDUC") <- c(levels(OG_YH_test[,"D_OCC_EDUC"]), '(2) no answer')
OG_YH_test$"D_OCC_EDUC"[is.na(OG_YH_test$"D_OCC_EDUC") & OG_YH_test[,'D_OCC_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_OCC_EDUC)) #checking the number of NAs
unique(OG_YH_test$D_OCC_EDUC)

levels(OG_YH_test$"D_OCC_MEDIA") <- c(levels(OG_YH_test[,"D_OCC_MEDIA"]), '(2) no answer')
OG_YH_test$"D_OCC_MEDIA"[is.na(OG_YH_test$"D_OCC_MEDIA") & OG_YH_test[,'D_OCC_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_OCC_MEDIA)) #checking the number of NAs
unique(OG_YH_test$D_OCC_MEDIA)

levels(OG_YH_test$"D_OCC_EXEC") <- c(levels(OG_YH_test[,"D_OCC_EXEC"]), '(2) no answer')
OG_YH_test$"D_OCC_EXEC"[is.na(OG_YH_test$"D_OCC_EXEC") & OG_YH_test[,'D_OCC_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_OCC_EXEC)) #checking the number of NAs
unique(OG_YH_test$D_OCC_EXEC)

levels(OG_YH_test$"D_OCC_TRAVEL") <- c(levels(OG_YH_test[,"D_OCC_TRAVEL"]), '(2) no answer')
OG_YH_test$"D_OCC_TRAVEL"[is.na(OG_YH_test$"D_OCC_TRAVEL") & OG_YH_test[,'D_OCC_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_OCC_TRAVEL)) #checking the number of NAs
unique(OG_YH_test$D_OCC_TRAVEL)

levels(OG_YH_test$"D_OCC_LEGAL") <- c(levels(OG_YH_test[,"D_OCC_LEGAL"]), '(2) no answer')
OG_YH_test$"D_OCC_LEGAL"[is.na(OG_YH_test$"D_OCC_LEGAL") & OG_YH_test[,'D_OCC_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_OCC_LEGAL)) #checking the number of NAs
unique(OG_YH_test$D_OCC_LEGAL)

levels(OG_YH_test$"D_OCC_MANU") <- c(levels(OG_YH_test[,"D_OCC_MANU"]), '(2) no answer')
OG_YH_test$"D_OCC_MANU"[is.na(OG_YH_test$"D_OCC_MANU") & OG_YH_test[,'D_OCC_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_OCC_MANU)) #checking the number of NAs
unique(OG_YH_test$D_OCC_MANU)

levels(OG_YH_test$"D_OCC_MED") <- c(levels(OG_YH_test[,"D_OCC_MED"]), '(2) no answer')
OG_YH_test$"D_OCC_MED"[is.na(OG_YH_test$"D_OCC_MED") & OG_YH_test[,'D_OCC_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_OCC_MED)) #checking the number of NAs
unique(OG_YH_test$D_OCC_MED)

levels(OG_YH_test$"D_OCC_POLITICS") <- c(levels(OG_YH_test[,"D_OCC_POLITICS"]), '(2) no answer')
OG_YH_test$"D_OCC_POLITICS"[is.na(OG_YH_test$"D_OCC_POLITICS") & OG_YH_test[,'D_OCC_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_OCC_POLITICS)) #checking the number of NAs
unique(OG_YH_test$D_OCC_POLITICS)

levels(OG_YH_test$"D_OCC_SALES") <- c(levels(OG_YH_test[,"D_OCC_SALES"]), '(2) no answer')
OG_YH_test$"D_OCC_SALES"[is.na(OG_YH_test$"D_OCC_SALES") & OG_YH_test[,'D_OCC_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_OCC_SALES)) #checking the number of NAs
unique(OG_YH_test$D_OCC_SALES)

levels(OG_YH_test$"D_OCC_TECH") <- c(levels(OG_YH_test[,"D_OCC_TECH"]), '(2) no answer')
OG_YH_test$"D_OCC_TECH"[is.na(OG_YH_test$"D_OCC_TECH") & OG_YH_test[,'D_OCC_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_OCC_TECH)) #checking the number of NAs
unique(OG_YH_test$D_OCC_TECH)

levels(OG_YH_test$"D_OCC_TRANSPORT") <- c(levels(OG_YH_test[,"D_OCC_TRANSPORT"]), '(2) no answer')
OG_YH_test$"D_OCC_TRANSPORT"[is.na(OG_YH_test$"D_OCC_TRANSPORT") & OG_YH_test[,'D_OCC_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_OCC_TRANSPORT)) #checking the number of NAs
unique(OG_YH_test$D_OCC_TRANSPORT)

levels(OG_YH_test$"D_OCC_FOOD") <- c(levels(OG_YH_test[,"D_OCC_FOOD"]), '(2) no answer')
OG_YH_test$"D_OCC_FOOD"[is.na(OG_YH_test$"D_OCC_FOOD") & OG_YH_test[,'D_OCC_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_OCC_FOOD)) #checking the number of NAs
unique(OG_YH_test$D_OCC_FOOD)

levels(OG_YH_test$"D_OCC_OTHER") <- c(levels(OG_YH_test[,"D_OCC_OTHER"]), '(2) no answer')
OG_YH_test$"D_OCC_OTHER"[is.na(OG_YH_test$"D_OCC_OTHER") & OG_YH_test[,'D_OCC_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_OCC_OTHER)) #checking the number of NAs
unique(OG_YH_test$D_OCC_OTHER)


#D_FAITH_ANY
DFaitName <- c("D_FAITH_BUDDHIST", "D_FAITH_CHRISTIAN", "D_FAITH_HINDU", 
               "D_FAITH_JEWISH", "D_FAITH_ISLAM", "D_FAITH_AGNOSTIC", "D_FAITH_PROTESTANT",
               "D_FAITH_CATHOLIC", "D_FAITH_ATHEIST", "D_FAITH_LDS", 
               "D_FAITH_CHRISTIAN_OTHER", "D_FAITH_NOTRELIG", "D_FAITH_SPIRITUAL",
               "D_FAITH_SCIENT", "D_FAITH_OTHER") #15

levels(OG_YH_test$"D_FAITH_BUDDHIST") <- c(levels(OG_YH_test[,"D_FAITH_BUDDHIST"]), '(2) no answer')
OG_YH_test$"D_FAITH_BUDDHIST"[is.na(OG_YH_test$"D_FAITH_BUDDHIST") & OG_YH_test[,'D_FAITH_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_FAITH_BUDDHIST)) #checking the number of NAs
unique(OG_YH_test$D_FAITH_BUDDHIST)

levels(OG_YH_test$"D_FAITH_CHRISTIAN") <- c(levels(OG_YH_test[,"D_FAITH_CHRISTIAN"]), '(2) no answer')
OG_YH_test$"D_FAITH_CHRISTIAN"[is.na(OG_YH_test$"D_FAITH_CHRISTIAN") & OG_YH_test[,'D_FAITH_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_FAITH_CHRISTIAN)) #checking the number of NAs
unique(OG_YH_test$D_FAITH_CHRISTIAN)

levels(OG_YH_test$"D_FAITH_HINDU") <- c(levels(OG_YH_test[,"D_FAITH_HINDU"]), '(2) no answer')
OG_YH_test$"D_FAITH_HINDU"[is.na(OG_YH_test$"D_FAITH_HINDU") & OG_YH_test[,'D_FAITH_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_FAITH_HINDU)) #checking the number of NAs
unique(OG_YH_test$D_FAITH_HINDU)

levels(OG_YH_test$"D_FAITH_JEWISH") <- c(levels(OG_YH_test[,"D_FAITH_JEWISH"]), '(2) no answer')
OG_YH_test$"D_FAITH_JEWISH"[is.na(OG_YH_test$"D_FAITH_JEWISH") & OG_YH_test[,'D_FAITH_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_FAITH_JEWISH)) #checking the number of NAs
unique(OG_YH_test$D_FAITH_JEWISH)

levels(OG_YH_test$"D_FAITH_ISLAM") <- c(levels(OG_YH_test[,"D_FAITH_ISLAM"]), '(2) no answer')
OG_YH_test$"D_FAITH_ISLAM"[is.na(OG_YH_test$"D_FAITH_ISLAM") & OG_YH_test[,'D_FAITH_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_FAITH_ISLAM)) #checking the number of NAs
unique(OG_YH_test$D_FAITH_ISLAM)

levels(OG_YH_test$"D_FAITH_AGNOSTIC") <- c(levels(OG_YH_test[,"D_FAITH_AGNOSTIC"]), '(2) no answer')
OG_YH_test$"D_FAITH_AGNOSTIC"[is.na(OG_YH_test$"D_FAITH_AGNOSTIC") & OG_YH_test[,'D_FAITH_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_FAITH_AGNOSTIC)) #checking the number of NAs
unique(OG_YH_test$D_FAITH_AGNOSTIC)

levels(OG_YH_test$"D_FAITH_PROTESTANT") <- c(levels(OG_YH_test[,"D_FAITH_PROTESTANT"]), '(2) no answer')
OG_YH_test$"D_FAITH_PROTESTANT"[is.na(OG_YH_test$"D_FAITH_PROTESTANT") & OG_YH_test[,'D_FAITH_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_FAITH_PROTESTANT)) #checking the number of NAs
unique(OG_YH_test$D_FAITH_PROTESTANT)

levels(OG_YH_test$"D_FAITH_CATHOLIC") <- c(levels(OG_YH_test[,"D_FAITH_CATHOLIC"]), '(2) no answer')
OG_YH_test$"D_FAITH_CATHOLIC"[is.na(OG_YH_test$"D_FAITH_CATHOLIC") & OG_YH_test[,'D_FAITH_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_FAITH_CATHOLIC)) #checking the number of NAs
unique(OG_YH_test$D_FAITH_CATHOLIC)

levels(OG_YH_test$"D_FAITH_ATHEIST") <- c(levels(OG_YH_test[,"D_FAITH_ATHEIST"]), '(2) no answer')
OG_YH_test$"D_FAITH_ATHEIST"[is.na(OG_YH_test$"D_FAITH_ATHEIST") & OG_YH_test[,'D_FAITH_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_FAITH_ATHEIST)) #checking the number of NAs
unique(OG_YH_test$D_FAITH_ATHEIST)

levels(OG_YH_test$"D_FAITH_LDS") <- c(levels(OG_YH_test[,"D_FAITH_LDS"]), '(2) no answer')
OG_YH_test$"D_FAITH_LDS"[is.na(OG_YH_test$"D_FAITH_LDS") & OG_YH_test[,'D_FAITH_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_FAITH_LDS)) #checking the number of NAs
unique(OG_YH_test$D_FAITH_LDS)

levels(OG_YH_test$"D_FAITH_CHRISTIAN_OTHER") <- c(levels(OG_YH_test[,"D_FAITH_CHRISTIAN_OTHER"]), '(2) no answer')
OG_YH_test$"D_FAITH_CHRISTIAN_OTHER"[is.na(OG_YH_test$"D_FAITH_CHRISTIAN_OTHER") & OG_YH_test[,'D_FAITH_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_FAITH_CHRISTIAN_OTHER)) #checking the number of NAs
unique(OG_YH_test$D_FAITH_CHRISTIAN_OTHER)

levels(OG_YH_test$"D_FAITH_NOTRELIG") <- c(levels(OG_YH_test[,"D_FAITH_NOTRELIG"]), '(2) no answer')
OG_YH_test$"D_FAITH_NOTRELIG"[is.na(OG_YH_test$"D_FAITH_NOTRELIG") & OG_YH_test[,'D_FAITH_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_FAITH_NOTRELIG)) #checking the number of NAs
unique(OG_YH_test$D_FAITH_NOTRELIG)

levels(OG_YH_test$"D_FAITH_SPIRITUAL") <- c(levels(OG_YH_test[,"D_FAITH_SPIRITUAL"]), '(2) no answer')
OG_YH_test$"D_FAITH_SPIRITUAL"[is.na(OG_YH_test$"D_FAITH_SPIRITUAL") & OG_YH_test[,'D_FAITH_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_FAITH_SPIRITUAL)) #checking the number of NAs
unique(OG_YH_test$D_FAITH_SPIRITUAL)

levels(OG_YH_test$"D_FAITH_SCIENT") <- c(levels(OG_YH_test[,"D_FAITH_SCIENT"]), '(2) no answer')
OG_YH_test$"D_FAITH_SCIENT"[is.na(OG_YH_test$"D_FAITH_SCIENT") & OG_YH_test[,'D_FAITH_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_FAITH_SCIENT)) #checking the number of NAs
unique(OG_YH_test$D_FAITH_SCIENT)

levels(OG_YH_test$"D_FAITH_OTHER") <- c(levels(OG_YH_test[,"D_FAITH_OTHER"]), '(2) no answer')
OG_YH_test$"D_FAITH_OTHER"[is.na(OG_YH_test$"D_FAITH_OTHER") & OG_YH_test[,'D_FAITH_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_FAITH_OTHER)) #checking the number of NAs
unique(OG_YH_test$D_FAITH_OTHER)



#D_SERVICE_ANY
DServiceName <- c("D_SERVICE_MORE", "D_SERVICE_WEEKLY", "D_SERVICE_MONTHLY", 
                  "D_SERVICE_HOLIDAYS", "D_SERVICE_RARELY", "D_SERVICE_NEVER")

levels(OG_YH_test$"D_SERVICE_MORE") <- c(levels(OG_YH_test[,"D_SERVICE_MORE"]), '(2) no answer')
OG_YH_test$"D_SERVICE_MORE"[is.na(OG_YH_test$"D_SERVICE_MORE") & OG_YH_test[,'D_SERVICE_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_SERVICE_MORE)) #checking the number of NAs
unique(OG_YH_test$D_SERVICE_MORE)

levels(OG_YH_test$"D_SERVICE_WEEKLY") <- c(levels(OG_YH_test[,"D_SERVICE_WEEKLY"]), '(2) no answer')
OG_YH_test$"D_SERVICE_WEEKLY"[is.na(OG_YH_test$"D_SERVICE_WEEKLY") & OG_YH_test[,'D_SERVICE_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_SERVICE_WEEKLY)) #checking the number of NAs
unique(OG_YH_test$D_SERVICE_WEEKLY)

levels(OG_YH_test$"D_SERVICE_MONTHLY") <- c(levels(OG_YH_test[,"D_SERVICE_MONTHLY"]), '(2) no answer')
OG_YH_test$"D_SERVICE_MONTHLY"[is.na(OG_YH_test$"D_SERVICE_MONTHLY") & OG_YH_test[,'D_SERVICE_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_SERVICE_MONTHLY)) #checking the number of NAs
unique(OG_YH_test$D_SERVICE_MONTHLY)

levels(OG_YH_test$"D_SERVICE_HOLIDAYS") <- c(levels(OG_YH_test[,"D_SERVICE_HOLIDAYS"]), '(2) no answer')
OG_YH_test$"D_SERVICE_HOLIDAYS"[is.na(OG_YH_test$"D_SERVICE_HOLIDAYS") & OG_YH_test[,'D_SERVICE_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_SERVICE_HOLIDAYS)) #checking the number of NAs
unique(OG_YH_test$D_SERVICE_HOLIDAYS)

levels(OG_YH_test$"D_SERVICE_RARELY") <- c(levels(OG_YH_test[,"D_SERVICE_RARELY"]), '(2) no answer')
OG_YH_test$"D_SERVICE_RARELY"[is.na(OG_YH_test$"D_SERVICE_RARELY") & OG_YH_test[,'D_SERVICE_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_SERVICE_RARELY)) #checking the number of NAs
unique(OG_YH_test$D_SERVICE_RARELY)

levels(OG_YH_test$"D_SERVICE_NEVER") <- c(levels(OG_YH_test[,"D_SERVICE_NEVER"]), '(2) no answer')
OG_YH_test$"D_SERVICE_NEVER"[is.na(OG_YH_test$"D_SERVICE_NEVER") & OG_YH_test[,'D_SERVICE_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_SERVICE_NEVER)) #checking the number of NAs
unique(OG_YH_test$D_SERVICE_NEVER)

#D_POLITICS_ANY
DPolName <- c("D_POLITICS_VCONS", "D_POLITICS_CONS", "D_POLITICS_MIDDLE", 
              "D_POLITICS_LIB", "D_POLITICS_VLIB", "D_POLITICS_NOTPOL") 

levels(OG_YH_test$"D_POLITICS_VCONS") <- c(levels(OG_YH_test[,"D_POLITICS_VCONS"]), '(2) no answer')
OG_YH_test$"D_POLITICS_VCONS"[is.na(OG_YH_test$"D_POLITICS_VCONS") & OG_YH_test[,'D_POLITICS_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_POLITICS_VCONS)) #checking the number of NAs
unique(OG_YH_test$D_POLITICS_VCONS)

levels(OG_YH_test$"D_POLITICS_CONS") <- c(levels(OG_YH_test[,"D_POLITICS_CONS"]), '(2) no answer')
OG_YH_test$"D_POLITICS_CONS"[is.na(OG_YH_test$"D_POLITICS_CONS") & OG_YH_test[,'D_POLITICS_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_POLITICS_CONS)) #checking the number of NAs
unique(OG_YH_test$D_POLITICS_CONS)

levels(OG_YH_test$"D_POLITICS_MIDDLE") <- c(levels(OG_YH_test[,"D_POLITICS_MIDDLE"]), '(2) no answer')
OG_YH_test$"D_POLITICS_MIDDLE"[is.na(OG_YH_test$"D_POLITICS_MIDDLE") & OG_YH_test[,'D_POLITICS_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_POLITICS_MIDDLE)) #checking the number of NAs
unique(OG_YH_test$D_POLITICS_MIDDLE)

levels(OG_YH_test$"D_POLITICS_LIB") <- c(levels(OG_YH_test[,"D_POLITICS_LIB"]), '(2) no answer')
OG_YH_test$"D_POLITICS_LIB"[is.na(OG_YH_test$"D_POLITICS_LIB") & OG_YH_test[,'D_POLITICS_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_POLITICS_LIB)) #checking the number of NAs
unique(OG_YH_test$D_POLITICS_LIB)

levels(OG_YH_test$"D_POLITICS_VLIB") <- c(levels(OG_YH_test[,"D_POLITICS_VLIB"]), '(2) no answer')
OG_YH_test$"D_POLITICS_VLIB"[is.na(OG_YH_test$"D_POLITICS_VLIB") & OG_YH_test[,'D_POLITICS_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_POLITICS_VLIB)) #checking the number of NAs
unique(OG_YH_test$D_POLITICS_VLIB)

levels(OG_YH_test$"D_POLITICS_NOTPOL") <- c(levels(OG_YH_test[,"D_POLITICS_NOTPOL"]), '(2) no answer')
OG_YH_test$"D_POLITICS_NOTPOL"[is.na(OG_YH_test$"D_POLITICS_NOTPOL") & OG_YH_test[,'D_POLITICS_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_POLITICS_NOTPOL)) #checking the number of NAs
unique(OG_YH_test$D_POLITICS_NOTPOL)



#D_LANG_ANY
DLangName <- c("D_LANG_ENGLISH", "D_LANG_FRENCH", "D_LANG_GERMAN", "D_LANG_ITALIAN", 
               "D_LANG_SPANISH", "D_LANG_PORTUGUESE", "D_LANG_DUTCH", 
               "D_LANG_CHINESE", "D_LANG_JAPANESE", "D_LANG_ARABIC", "D_LANG_RUSSIAN",
               "D_LANG_HEBREW", "D_LANG_HINDI", "D_LANG_TAGALOG", "D_LANG_URDU") #15

levels(OG_YH_test$"D_LANG_ENGLISH") <- c(levels(OG_YH_test[,"D_LANG_ENGLISH"]), '(2) no answer')
OG_YH_test$"D_LANG_ENGLISH"[is.na(OG_YH_test$"D_LANG_ENGLISH") & OG_YH_test[,'D_LANG_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_LANG_ENGLISH)) #checking the number of NAs
unique(OG_YH_test$D_LANG_ENGLISH)

levels(OG_YH_test$"D_LANG_FRENCH") <- c(levels(OG_YH_test[,"D_LANG_FRENCH"]), '(2) no answer')
OG_YH_test$"D_LANG_FRENCH"[is.na(OG_YH_test$"D_LANG_FRENCH") & OG_YH_test[,'D_LANG_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_LANG_FRENCH)) #checking the number of NAs
unique(OG_YH_test$D_LANG_FRENCH)

levels(OG_YH_test$"D_LANG_GERMAN") <- c(levels(OG_YH_test[,"D_LANG_GERMAN"]), '(2) no answer')
OG_YH_test$"D_LANG_GERMAN"[is.na(OG_YH_test$"D_LANG_GERMAN") & OG_YH_test[,'D_LANG_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_LANG_GERMAN)) #checking the number of NAs
unique(OG_YH_test$D_LANG_GERMAN)

levels(OG_YH_test$"D_LANG_ITALIAN") <- c(levels(OG_YH_test[,"D_LANG_ITALIAN"]), '(2) no answer')
OG_YH_test$"D_LANG_ITALIAN"[is.na(OG_YH_test$"D_LANG_ITALIAN") & OG_YH_test[,'D_LANG_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_LANG_ITALIAN)) #checking the number of NAs
unique(OG_YH_test$D_LANG_ITALIAN)

levels(OG_YH_test$"D_LANG_SPANISH") <- c(levels(OG_YH_test[,"D_LANG_SPANISH"]), '(2) no answer')
OG_YH_test$"D_LANG_SPANISH"[is.na(OG_YH_test$"D_LANG_SPANISH") & OG_YH_test[,'D_LANG_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_LANG_SPANISH)) #checking the number of NAs
unique(OG_YH_test$D_LANG_SPANISH)

levels(OG_YH_test$"D_LANG_PORTUGUESE") <- c(levels(OG_YH_test[,"D_LANG_PORTUGUESE"]), '(2) no answer')
OG_YH_test$"D_LANG_PORTUGUESE"[is.na(OG_YH_test$"D_LANG_PORTUGUESE") & OG_YH_test[,'D_LANG_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_LANG_PORTUGUESE)) #checking the number of NAs
unique(OG_YH_test$D_LANG_PORTUGUESE)

levels(OG_YH_test$"D_LANG_DUTCH") <- c(levels(OG_YH_test[,"D_LANG_DUTCH"]), '(2) no answer')
OG_YH_test$"D_LANG_DUTCH"[is.na(OG_YH_test$"D_LANG_DUTCH") & OG_YH_test[,'D_LANG_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_LANG_DUTCH)) #checking the number of NAs
unique(OG_YH_test$D_LANG_DUTCH)

levels(OG_YH_test$"D_LANG_CHINESE") <- c(levels(OG_YH_test[,"D_LANG_CHINESE"]), '(2) no answer')
OG_YH_test$"D_LANG_CHINESE"[is.na(OG_YH_test$"D_LANG_CHINESE") & OG_YH_test[,'D_LANG_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_LANG_CHINESE)) #checking the number of NAs
unique(OG_YH_test$D_LANG_CHINESE)

levels(OG_YH_test$"D_LANG_JAPANESE") <- c(levels(OG_YH_test[,"D_LANG_JAPANESE"]), '(2) no answer')
OG_YH_test$"D_LANG_JAPANESE"[is.na(OG_YH_test$"D_LANG_JAPANESE") & OG_YH_test[,'D_LANG_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_LANG_JAPANESE)) #checking the number of NAs
unique(OG_YH_test$D_LANG_JAPANESE)

levels(OG_YH_test$"D_LANG_ARABIC") <- c(levels(OG_YH_test[,"D_LANG_ARABIC"]), '(2) no answer')
OG_YH_test$"D_LANG_ARABIC"[is.na(OG_YH_test$"D_LANG_ARABIC") & OG_YH_test[,'D_LANG_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_LANG_ARABIC)) #checking the number of NAs
unique(OG_YH_test$D_LANG_ARABIC)

levels(OG_YH_test$"D_LANG_RUSSIAN") <- c(levels(OG_YH_test[,"D_LANG_RUSSIAN"]), '(2) no answer')
OG_YH_test$"D_LANG_RUSSIAN"[is.na(OG_YH_test$"D_LANG_RUSSIAN") & OG_YH_test[,'D_LANG_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_LANG_RUSSIAN)) #checking the number of NAs
unique(OG_YH_test$D_LANG_RUSSIAN)

levels(OG_YH_test$"D_LANG_HEBREW") <- c(levels(OG_YH_test[,"D_LANG_HEBREW"]), '(2) no answer')
OG_YH_test$"D_LANG_HEBREW"[is.na(OG_YH_test$"D_LANG_HEBREW") & OG_YH_test[,'D_LANG_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_LANG_HEBREW)) #checking the number of NAs
unique(OG_YH_test$D_LANG_HEBREW)

levels(OG_YH_test$"D_LANG_HINDI") <- c(levels(OG_YH_test[,"D_LANG_HINDI"]), '(2) no answer')
OG_YH_test$"D_LANG_HINDI"[is.na(OG_YH_test$"D_LANG_HINDI") & OG_YH_test[,'D_LANG_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_LANG_HINDI)) #checking the number of NAs
unique(OG_YH_test$D_LANG_HINDI)

levels(OG_YH_test$"D_LANG_TAGALOG") <- c(levels(OG_YH_test[,"D_LANG_TAGALOG"]), '(2) no answer')
OG_YH_test$"D_LANG_TAGALOG"[is.na(OG_YH_test$"D_LANG_TAGALOG") & OG_YH_test[,'D_LANG_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_LANG_TAGALOG)) #checking the number of NAs
unique(OG_YH_test$D_LANG_TAGALOG)

levels(OG_YH_test$"D_LANG_URDU") <- c(levels(OG_YH_test[,"D_LANG_URDU"]), '(2) no answer')
OG_YH_test$"D_LANG_URDU"[is.na(OG_YH_test$"D_LANG_URDU") & OG_YH_test[,'D_LANG_ANY'] == "(1) yes"] <- '(2) no answer'
sum(is.na(OG_YH_test$D_LANG_URDU)) #checking the number of NAs
unique(OG_YH_test$D_LANG_URDU)

sum(is.na(OG_YH_test$D_LANG_CHINESE))

sum(is.na(OG_YH_test$D_EDUC)) #2514

#variable is numeric. Where should I put no preferecnce? Should I shift all the 
#others up and make no preference the bottom?
#People could only pick one minimum threshold.

#So maybe I should add them to the lowest threshold. If they didn't care enough to select 
#a minimum, they can be at the lowest threshold.
#OG_YH_test$D_EDUC <- ifelse(is.na(OG_YH_test$D_EDUC), 0, OG_YH_test$D_EDUC)
levels(OG_YH_test$"D_EDUC") <- c(levels(OG_YH_test[,"D_EDUC"]), '(0) no answer')
OG_YH_test$"D_EDUC"[is.na(OG_YH_test$"D_EDUC")] <- '(0) no answer'

levels(OG_YH_test$D_EDUC)[levels(OG_YH_test$D_EDUC)=='(0) some high school'] <- '(1) some high school'
levels(OG_YH_test$D_EDUC)[levels(OG_YH_test$D_EDUC)=='(1) high school graduate'] <- '(2) high school graduate'
levels(OG_YH_test$D_EDUC)[levels(OG_YH_test$D_EDUC)=='(2) some college'] <- '(3) some college'
levels(OG_YH_test$D_EDUC)[levels(OG_YH_test$D_EDUC)=='(0) no answer'] <- '(0) no answer'
#making the NA level the bottom level
OG_YH_test$D_EDUC <- relevel(OG_YH_test$D_EDUC, "(0) no answer")


#Now, cleaning the profile's data
colSums(is.na(OG_YH_test))
#changing NA's to 0 for profile Languages if not specified

levels(OG_YH_test$"LANG_FRENCH") <- c(levels(OG_YH_test[,"LANG_FRENCH"]), '(2) no answer')
OG_YH_test$"LANG_FRENCH"[is.na(OG_YH_test$"LANG_FRENCH")] <- '(2) no answer'
sum(is.na(OG_YH_test$LANG_FRENCH)) #checking the number of NAs
unique(OG_YH_test$LANG_FRENCH)

levels(OG_YH_test$"LANG_GERMAN") <- c(levels(OG_YH_test[,"LANG_GERMAN"]), '(2) no answer')
OG_YH_test$"LANG_GERMAN"[is.na(OG_YH_test$"LANG_GERMAN")] <- '(2) no answer'
sum(is.na(OG_YH_test$LANG_GERMAN)) #checking the number of NAs
unique(OG_YH_test$LANG_GERMAN)

levels(OG_YH_test$"LANG_ITALIAN") <- c(levels(OG_YH_test[,"LANG_ITALIAN"]), '(2) no answer')
OG_YH_test$"LANG_ITALIAN"[is.na(OG_YH_test$"LANG_ITALIAN")] <- '(2) no answer'
sum(is.na(OG_YH_test$LANG_ITALIAN)) #checking the number of NAs
unique(OG_YH_test$LANG_ITALIAN)

levels(OG_YH_test$"LANG_SPANISH") <- c(levels(OG_YH_test[,"LANG_SPANISH"]), '(2) no answer')
OG_YH_test$"LANG_SPANISH"[is.na(OG_YH_test$"LANG_SPANISH")] <- '(2) no answer'
sum(is.na(OG_YH_test$LANG_SPANISH)) #checking the number of NAs
unique(OG_YH_test$LANG_SPANISH)

levels(OG_YH_test$"LANG_PORTUGUESE") <- c(levels(OG_YH_test[,"LANG_PORTUGUESE"]), '(2) no answer')
OG_YH_test$"LANG_PORTUGUESE"[is.na(OG_YH_test$"LANG_PORTUGUESE")] <- '(2) no answer'
sum(is.na(OG_YH_test$LANG_PORTUGUESE)) #checking the number of NAs
unique(OG_YH_test$LANG_PORTUGUESE)

levels(OG_YH_test$"LANG_DUTCH") <- c(levels(OG_YH_test[,"LANG_DUTCH"]), '(2) no answer')
OG_YH_test$"LANG_DUTCH"[is.na(OG_YH_test$"LANG_DUTCH")] <- '(2) no answer'
sum(is.na(OG_YH_test$LANG_DUTCH)) #checking the number of NAs
unique(OG_YH_test$LANG_DUTCH)

levels(OG_YH_test$"LANG_CHINESE") <- c(levels(OG_YH_test[,"LANG_CHINESE"]), '(2) no answer')
OG_YH_test$"LANG_CHINESE"[is.na(OG_YH_test$"LANG_CHINESE")] <- '(2) no answer'
sum(is.na(OG_YH_test$LANG_CHINESE)) #checking the number of NAs
unique(OG_YH_test$LANG_CHINESE)

levels(OG_YH_test$"LANG_JAPANESE") <- c(levels(OG_YH_test[,"LANG_JAPANESE"]), '(2) no answer')
OG_YH_test$"LANG_JAPANESE"[is.na(OG_YH_test$"LANG_JAPANESE")] <- '(2) no answer'
sum(is.na(OG_YH_test$LANG_JAPANESE)) #checking the number of NAs
unique(OG_YH_test$LANG_JAPANESE)

levels(OG_YH_test$"LANG_ARABIC") <- c(levels(OG_YH_test[,"LANG_ARABIC"]), '(2) no answer')
OG_YH_test$"LANG_ARABIC"[is.na(OG_YH_test$"LANG_ARABIC")] <- '(2) no answer'
sum(is.na(OG_YH_test$LANG_ARABIC)) #checking the number of NAs
unique(OG_YH_test$LANG_ARABIC)

levels(OG_YH_test$"LANG_RUSSIAN") <- c(levels(OG_YH_test[,"LANG_RUSSIAN"]), '(2) no answer')
OG_YH_test$"LANG_RUSSIAN"[is.na(OG_YH_test$"LANG_RUSSIAN")] <- '(2) no answer'
sum(is.na(OG_YH_test$LANG_RUSSIAN)) #checking the number of NAs
unique(OG_YH_test$LANG_RUSSIAN)

levels(OG_YH_test$"LANG_HEBREW") <- c(levels(OG_YH_test[,"LANG_HEBREW"]), '(2) no answer')
OG_YH_test$"LANG_HEBREW"[is.na(OG_YH_test$"LANG_HEBREW")] <- '(2) no answer'
sum(is.na(OG_YH_test$LANG_HEBREW)) #checking the number of NAs
unique(OG_YH_test$LANG_HEBREW)

levels(OG_YH_test$"LANG_HINDI") <- c(levels(OG_YH_test[,"LANG_HINDI"]), '(2) no answer')
OG_YH_test$"LANG_HINDI"[is.na(OG_YH_test$"LANG_HINDI")] <- '(2) no answer'
sum(is.na(OG_YH_test$LANG_HINDI)) #checking the number of NAs
unique(OG_YH_test$LANG_HINDI)

levels(OG_YH_test$"LANG_TAGALOG") <- c(levels(OG_YH_test[,"LANG_TAGALOG"]), '(2) no answer')
OG_YH_test$"LANG_TAGALOG"[is.na(OG_YH_test$"LANG_TAGALOG")] <- '(2) no answer'
sum(is.na(OG_YH_test$LANG_TAGALOG)) #checking the number of NAs
unique(OG_YH_test$LANG_TAGALOG)

levels(OG_YH_test$"LANG_URDU") <- c(levels(OG_YH_test[,"LANG_URDU"]), '(2) no answer')
OG_YH_test$"LANG_URDU"[is.na(OG_YH_test$"LANG_URDU")] <- '(2) no answer'
sum(is.na(OG_YH_test$LANG_URDU)) #checking the number of NAs
unique(OG_YH_test$LANG_URDU)

####LANG_ENGLISH - 1722 missing. maybe make this as English as the default?
#view(filter(OG_YH_test, is.na(OG_YH_test$LANG_ENGLISH)))
#adding 1s to English - I am assuming that by default, people spoke English.
#OG_YH_test$LANG_ENGLISH <- ifelse(is.na(OG_YH_test$LANG_ENGLISH), 1, OG_YH_test$LANG_ENGLISH)
OG_YH_test$"LANG_ENGLISH"[is.na(OG_YH_test$"LANG_ENGLISH")] <- '(1) yes'


### MARSTAT
#making the MARSTAT NAs as a factor
class(OG_YH_test$MARSTAT)
levels(OG_YH_test$MARSTAT) <- c(levels(OG_YH_test$MARSTAT), '(4) no answer')
OG_YH_test$MARSTAT[is.na(OG_YH_test$MARSTAT)] <- '(4) no answer'
#colSums(is.na(OG_YH_test))


### BODY
colnames(OG_YH_test %>% select_if(sapply(., class) %in% c("factor")))
class(OG_YH_test$BODY)
levels(OG_YH_test$BODY) <- c(levels(OG_YH_test$BODY), '(9) no answer')
OG_YH_test$BODY[is.na(OG_YH_test$BODY)] <- '(9) no answer'


## LIVE_ALONE
class(OG_YH_test$LIVE_ALONE)
levels(OG_YH_test$LIVE_ALONE) <- c(levels(OG_YH_test$LIVE_ALONE), '(2) no answer')
OG_YH_test$LIVE_ALONE[is.na(OG_YH_test$LIVE_ALONE)] <- '(2) no answer'

#LIVE_KIDS
class(OG_YH_test$LIVE_KIDS)
levels(OG_YH_test$LIVE_KIDS) <- c(levels(OG_YH_test$LIVE_KIDS), '(2) no answer')
OG_YH_test$LIVE_KIDS[is.na(OG_YH_test$LIVE_KIDS)] <- '(2) no answer'

#LIVE_PAR
class(OG_YH_test$LIVE_PAR)
levels(OG_YH_test$LIVE_PAR) <- c(levels(OG_YH_test$LIVE_PAR), '(2) no answer')
OG_YH_test$LIVE_PAR[is.na(OG_YH_test$LIVE_PAR)] <- '(2) no answer'

#LIVE_PETS
class(OG_YH_test$LIVE_PETS)
levels(OG_YH_test$LIVE_PETS) <- c(levels(OG_YH_test$LIVE_PETS), '(2) no answer')
OG_YH_test$LIVE_PETS[is.na(OG_YH_test$LIVE_PETS)] <- '(2) no answer'

#LIVE_ROOM
class(OG_YH_test$LIVE_ROOM)
levels(OG_YH_test$LIVE_ROOM) <- c(levels(OG_YH_test$LIVE_ROOM), '(2) no answer')
OG_YH_test$LIVE_ROOM[is.na(OG_YH_test$LIVE_ROOM)] <- '(2) no answer'


#LIVE_FAM
class(OG_YH_test$LIVE_FAM)
levels(OG_YH_test$LIVE_FAM) <- c(levels(OG_YH_test$LIVE_FAM), '(2) no answer')
OG_YH_test$LIVE_FAM[is.na(OG_YH_test$LIVE_FAM)] <- '(2) no answer'


#LIVE_PARTY
class(OG_YH_test$LIVE_PARTY)
levels(OG_YH_test$LIVE_PARTY) <- c(levels(OG_YH_test$LIVE_PARTY), '(2) no answer')
OG_YH_test$LIVE_PARTY[is.na(OG_YH_test$LIVE_PARTY)] <- '(2) no answer'


## HAVEKIDS
class(OG_YH_test$HAVEKIDS)
levels(OG_YH_test$HAVEKIDS) <- c(levels(OG_YH_test$HAVEKIDS), '(4) no answer')
OG_YH_test$HAVEKIDS[is.na(OG_YH_test$HAVEKIDS)] <- '(4) no answer'


## WANTKIDS
class(OG_YH_test$WANTKIDS)
levels(OG_YH_test$WANTKIDS) <- c(levels(OG_YH_test$WANTKIDS), '(3) no answer')
OG_YH_test$WANTKIDS[is.na(OG_YH_test$WANTKIDS)] <- '(3) no answer'


## EMPSTAT
class(OG_YH_test$EMPSTAT)
levels(OG_YH_test$EMPSTAT) <- c(levels(OG_YH_test$EMPSTAT), '(8) no answer')
OG_YH_test$EMPSTAT[is.na(OG_YH_test$EMPSTAT)] <- '(8) no answer'


## OCCUP
class(OG_YH_test$OCCUP)
levels(OG_YH_test$OCCUP) <- c(levels(OG_YH_test$OCCUP), '(18) no answer')
OG_YH_test$OCCUP[is.na(OG_YH_test$OCCUP)] <- '(18) no answer'


## INCOME - ordered categorical
#- numeric, not factor. Should I make it factor again?
#at the moment, reshuffling amounts where if they did not respond, their INCOME is 0. Good idea?
#make income factor
class(OG_YH_test$INCOME)
#OG_YH_test <- OG_YH_test %>% mutate(INCOME = dplyr::case_when(INCOME == 0 ~ 1,
 #                                                             INCOME == 1 ~ 2,
  #                                                            INCOME == 2 ~ 3,
   #                                                           INCOME == 3 ~ 4,
    #                                                          INCOME == 4 ~ 5,
     #                                                         INCOME == 5 ~ 6,
      #                                                        INCOME == 6 ~ 7,
       #                                                       TRUE ~ 0))

levels(OG_YH_test$INCOME) <- c(levels(OG_YH_test$INCOME), '(0) no answer')
OG_YH_test$INCOME[is.na(OG_YH_test$INCOME)] <- '(0) no answer'

#no longer need to shift this up
levels(OG_YH_test$INCOME)[levels(OG_YH_test$INCOME)=='(00) less than $24,999'] <- '(1) less than $24,999'
levels(OG_YH_test$INCOME)[levels(OG_YH_test$INCOME)=='(01) $25,000 to $34,999'] <- '(2) $25,000 to $34,999y'
levels(OG_YH_test$INCOME)[levels(OG_YH_test$INCOME)=='(02) $35,000 to $49,999'] <- '(3) $35,000 to $49,999'
levels(OG_YH_test$INCOME)[levels(OG_YH_test$INCOME)=='(03) $50,000 to $74,999'] <- '(4) $50,000 to $74,999'
levels(OG_YH_test$INCOME)[levels(OG_YH_test$INCOME)=='(04) $75,000 to $99,999'] <- '(5) $75,000 to $99,999'
levels(OG_YH_test$INCOME)[levels(OG_YH_test$INCOME)=='(05) $100,000 to $149,999'] <- '(6) $100,000 to $149,999'
levels(OG_YH_test$INCOME)[levels(OG_YH_test$INCOME)=='(06) $150,000+'] <- '(7) $150,000+'
levels(OG_YH_test$INCOME)[levels(OG_YH_test$INCOME)=='(98) UNDOCUMENTED CODE'] <- "(0) no answer"


OG_YH_test$INCOME <- relevel(OG_YH_test$INCOME, "(0) no answer")

## RELIGION
class(OG_YH_test$RELIGION)
levels(OG_YH_test$RELIGION) <- c(levels(OG_YH_test$RELIGION), '(15) no answer')
OG_YH_test$RELIGION[is.na(OG_YH_test$RELIGION)] <- '(15) no answer'

##DON'T NEED -FIXED ABOVE

## POLITICS - kind of unordered
class(OG_YH_test$POLITICS)
levels(OG_YH_test$POLITICS) <- c(levels(OG_YH_test$POLITICS), '(6) no answer')
OG_YH_test$POLITICS[is.na(OG_YH_test$POLITICS)] <- '(6) no answer'

# Percent ethnicities
#view(filter(OG_YH_test, is.na(OG_YH_test$PERMEX)))
#Still, all the other latin American variables are missing 642.
  #vs the other races which is 108. 
#Maybe just use Black and white percentages? We'd only delete 108 rows


colSums(is.na(OG_YH_test))

#one NA left for D_BODY_SLENDER
#view(filter(OG_YH_test, is.na(D_BODY_SLENDER))) #ID IS 5925
#view(filter(OG_YH, OG_YH$ID == 5925))
#likes person just skipped it. D_ANY was not selected
OG_YH_test$D_BODY_SLENDER[is.na(OG_YH_test$D_BODY_SLENDER)] <- '(2) no answer'

#D_INCOME  - ordered categorical
#no answer is by far the highest group: 4384 NA, which is 72% of the data

levels(OG_YH_test$D_INCOME) <- c(levels(OG_YH_test$D_INCOME), '(0) no answer')
OG_YH_test$D_INCOME[is.na(OG_YH_test$D_INCOME)] <- '(0) no answer'

#no longer need to shift this up
levels(OG_YH_test$D_INCOME)[levels(OG_YH_test$D_INCOME)=='(0) less than $24,999'] <- '(1) less than $24,999'
levels(OG_YH_test$D_INCOME)[levels(OG_YH_test$D_INCOME)=='(1) $25,000 to $34,999'] <- '(2) $25,000 to $34,999y'
levels(OG_YH_test$D_INCOME)[levels(OG_YH_test$D_INCOME)=='(2) $35,000 to $49,999'] <- '(3) $35,000 to $49,999'
levels(OG_YH_test$D_INCOME)[levels(OG_YH_test$D_INCOME)=='(3) $50,000 to $74,999'] <- '(4) $50,000 to $74,999'
levels(OG_YH_test$D_INCOME)[levels(OG_YH_test$D_INCOME)=='(4) $75,000 to $99,999'] <- '(5) $75,000 to $99,999'
levels(OG_YH_test$D_INCOME)[levels(OG_YH_test$D_INCOME)=='(5) $100,000 to $149,999'] <- '(6) $100,000 to $149,999'
levels(OG_YH_test$D_INCOME)[levels(OG_YH_test$D_INCOME)=='(6) $150,000+'] <- '(7) $150,000+'

OG_YH_test$D_INCOME <- relevel(OG_YH_test$D_INCOME, "(0) no answer")

#D_YOUNGEST(26 missing) and D_OLDEST (#80 NA)
#if they say NA for youngest, then they can date anyone 18 and older. So give them a baseline
#of 18.
OG_YH_test$D_YOUNGEST <- ifelse(is.na(OG_YH_test$D_YOUNGEST), 18, OG_YH_test$D_YOUNGEST) #cause legally...
#OG_YH_test$D_YOUNGEST <- as.factor(OG_YH_test$D_YOUNGEST)

#D_OLDEST - looks like 95 is actually the highest.
sort(unique(OG_YH_test$D_OLDEST))
#maybe make it numeric because it just looks like some values were left out (like people just didn't type in 68, but 69). So it's not categorical
#going to make 96 the highest
OG_YH_test$D_OLDEST <- ifelse(is.na(OG_YH_test$D_OLDEST), 96, OG_YH_test$D_OLDEST)

#factor- backup just incase
#OG_YH_test$D_OLDEST <- as.factor(OG_YH_test$D_OLDEST)
#OG_YH_test$D_OLDEST<- is.na(OG_YH_test$D_OLDEST)<- '(99) no answer'


#heights
#looks like height in feet and inches was seperated (in codebook. Interesting)
#do the same thing where people get the mininums and maximums
#D_MINHEIGHT_FT
#D_MINHEIGHT_IN
#D_MAXHEIGHT_FT
#D_MAXHEIGHT_IN

OG_YH_test$D_MINHEIGHT_FT<- ifelse(is.na(OG_YH_test$D_MINHEIGHT_FT), 3, OG_YH_test$D_MINHEIGHT_FT)
OG_YH_test$D_MINHEIGHT_IN<- ifelse(is.na(OG_YH_test$D_MINHEIGHT_IN), 0, OG_YH_test$D_MINHEIGHT_IN)
OG_YH_test$D_MAXHEIGHT_FT<- ifelse(is.na(OG_YH_test$D_MAXHEIGHT_FT), 7, OG_YH_test$D_MAXHEIGHT_FT)
OG_YH_test$D_MAXHEIGHT_IN<- ifelse(is.na(OG_YH_test$D_MAXHEIGHT_IN), 11, OG_YH_test$D_MAXHEIGHT_IN)
  #it's only 22 people who didn't put min and max heights, so I think giving them the min and max threshold is okay.
    #so they are okay with the full spectrum of people the site offers

#the profiles own height? - If I have to get balance on them, it might be hard to get balanc on that
#HEIGHT_FT - 24 NAs, change to 0 or just delete.
#I might actually just delete b/c you can't be 0 height. I can't make that 0, or any value less or 
#view(subset(OG_YH_test, is.na(OG_YH_test$HEIGHT_FT))) - checking that 24 that are missing FT are also the
#ones missing IN.
OG_YH_test <- subset(OG_YH_test, !is.na(OG_YH_test$HEIGHT_FT))


#D_MILES
#max is 250. Should that be 250+? Maybe, I am assuming
#OG_YH_test$D_MILES<- ifelse(is.na(OG_YH_test$D_MILES), 250, OG_YH_test$D_MILES)
  #going to make this factor because there is no scheme on what I should increase 250 to mean greater than 250 miles. LIke shoudl it be 251 or 500?
OG_YH_test$D_MILES <- as.factor(OG_YH_test$D_MILES)
levels(OG_YH_test$D_MILES) <- c(levels(OG_YH_test$D_MILES), '(0) no answer')
OG_YH_test$D_MILES[is.na(OG_YH_test$D_MILES)] <- '(0) no answer'


#no longer need to shift this up
levels(OG_YH_test$D_MILES)[levels(OG_YH_test$D_MILES)=='0'] <- '(1) 0'
levels(OG_YH_test$D_MILES)[levels(OG_YH_test$D_MILES)=='5'] <- '(2) 5'
levels(OG_YH_test$D_MILES)[levels(OG_YH_test$D_MILES)=='10'] <- '(3) 10'
levels(OG_YH_test$D_MILES)[levels(OG_YH_test$D_MILES)=='15'] <- '(4) 15'
levels(OG_YH_test$D_MILES)[levels(OG_YH_test$D_MILES)=='20'] <- '(5) 20'
levels(OG_YH_test$D_MILES)[levels(OG_YH_test$D_MILES)=='25'] <- '(6) 25'
levels(OG_YH_test$D_MILES)[levels(OG_YH_test$D_MILES)=='50'] <- '(7) 50'
levels(OG_YH_test$D_MILES)[levels(OG_YH_test$D_MILES)=='100'] <- '(8) 100'
levels(OG_YH_test$D_MILES)[levels(OG_YH_test$D_MILES)=='250'] <- '(9) 250'
OG_YH_test$D_MILES <- relevel(OG_YH_test$D_MILES, "(0) no answer")

#is overlap only important for the covariates that matter, the ones that we include in our analysis?

colSums(is.na(OG_YH_test))

write_csv(OG_YH_test, file = "YHdata_clean_NAs.csv")

#0 NAs except for in percentages

