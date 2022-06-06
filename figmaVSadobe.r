og_DATA <- read.csv2("FA_DATA.csv")
DATA <- read.csv2("FA_DATA.csv")

toBeReversed <- c(FALSE, FALSE, TRUE, TRUE, TRUE,
                  FALSE, FALSE, FALSE, TRUE, TRUE,
                  FALSE, TRUE, FALSE, FALSE, FALSE,
                  FALSE, TRUE, TRUE, TRUE, FALSE,
                  TRUE, FALSE, TRUE, TRUE, TRUE, FALSE)


############################### UEQ questionnaire analysis ##################################### # nolint
# clean up the data
# convert 1-7 Likert values to range (-3, 3)

for (i in c(1:26)){
    if (toBeReversed[i] == TRUE) {
        # Figma items
        DATA[, i + 10] <- 4 - DATA[, i + 10];
        # Adobe XD items
        DATA[, i + 36] <- 4 - DATA[, i + 36];
    } else{
        # Figma items
        DATA[, i + 10] <- DATA[, i + 10] - 4;
        # Adobe XD items
        DATA[, i + 36] <- DATA[, i + 36] - 4;
    }
}


# column numbers for every scale
attractiveness_col <- c(1, 12, 14, 16, 24, 25)
perspicuity_col <- c(2, 4, 13, 21)
efficiency_col <- c(9, 20, 22, 23)
dependability_col <- c(8, 11, 17, 19)
stimulation_col <- c(5, 6, 7, 18)
novelty_col <- c(3, 10, 15, 25)


# Figma means for every user
f_attractiveness_users <- apply(DATA[, attractiveness_col+10], 1, mean, na.rm= TRUE)
f_perspicuity_users <- apply(DATA[, perspicuity_col+10], 1, mean, na.rm= TRUE)
f_efficiency_users <- apply(DATA[, efficiency_col+10], 1, mean, na.rm= TRUE)
f_dependability_users <- apply(DATA[, dependability_col+10], 1, mean, na.rm= TRUE)
f_stimulation_users <- apply(DATA[, stimulation_col+10], 1, mean, na.rm= TRUE)
f_novelty_users <- apply(DATA[, novelty_col+10], 1, mean, na.rm= TRUE)

# general Figma means for every scale
f_attractiveness_mean = mean(f_attractiveness_users, na.rm= TRUE)
f_perspicuity_mean = mean(f_perspicuity_users, na.rm= TRUE)
f_efficiency_mean = mean(f_efficiency_users, na.rm= TRUE)
f_dependability_mean = mean(f_dependability_users, na.rm= TRUE)
f_stimulation_mean = mean(f_stimulation_users, na.rm= TRUE)
f_novelty_mean = mean(f_novelty_users, na.rm= TRUE)



# Adobe means for every user
a_attractiveness_users <- apply(DATA[, attractiveness_col+36], 1, mean, na.rm= TRUE)
a_perspicuity_users <- apply(DATA[, perspicuity_col+36], 1, mean, na.rm= TRUE)
a_efficiency_users <- apply(DATA[, efficiency_col+36], 1, mean, na.rm= TRUE)
a_dependability_users <- apply(DATA[, dependability_col+36], 1, mean, na.rm= TRUE)
a_stimulation_users <- apply(DATA[, stimulation_col+36], 1, mean, na.rm= TRUE)
a_novelty_users <- apply(DATA[, novelty_col+36], 1, mean, na.rm= TRUE)

# general Adobe means for every scale
a_attractiveness_mean = mean(a_attractiveness_users, na.rm= TRUE)
a_perspicuity_mean = mean(a_perspicuity_users, na.rm= TRUE)
a_efficiency_mean = mean(a_efficiency_users, na.rm= TRUE)
a_dependability_mean = mean(a_dependability_users, na.rm= TRUE)
a_stimulation_mean = mean(a_stimulation_users, na.rm= TRUE)
a_novelty_mean = mean(a_novelty_users, na.rm= TRUE)





###################################### UMUX Lite ##########################################

# cleaning the data
# subtract 1 from every item

# cycling through the items
for (i in c(1:6)){
    #Figma items
    DATA[, i + 62] <- DATA[, i + 62] - 1;
    #Adobe items
    DATA[, i + 68] <- DATA[, i + 68] - 1;
}

# column numbers for every feature
coll_col <- c(1,2)
plug_col <- c(3,4)
shar_col <- c(5,6)

# Figma sum for every user
f_collaboration_users <- apply(DATA[, coll_col + 62], 1, sum) / 12
f_plugin_users <- apply(DATA[, plug_col + 62], 1, sum) / 12
f_sharing_users <- apply(DATA[, shar_col + 62], 1, sum) / 12

# Adobe sum for every user
a_collaboration_users <- apply(DATA[, coll_col + 68], 1, sum) / 12
a_plugin_users <- apply(DATA[, plug_col + 68], 1, sum) / 12
a_sharing_users <- apply(DATA[, shar_col + 68], 1, sum) / 12





#################################### Demographics #########################################

# convert into nominal variable -> as.factor(DATA$Gender)
# print gender demographics
gender_percentage= round(100*table(as.factor(DATA$Gender))/sum(table(as.factor(DATA$Gender))), digits = 1)

gender_label = paste(as.factor(DATA$Gender)," (", gender_percentage,"%)", sep = "")

pie(table(as.factor(DATA$Gender)), labels=gender_label)
