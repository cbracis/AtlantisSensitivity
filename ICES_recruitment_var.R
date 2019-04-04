rm(list = ls())
gc()
library(ggplot2)
library(dplyr)

SR_ICES <- read.table("files/ICES_SR_variability_2018advices.csv", sep = ",", dec =".", header = T)
SR_ICES$ICES_Blim <- as.numeric(SR_ICES$ICES_Blim)
SR_ICES$ICES_recruit_mean <- as.numeric(SR_ICES$ICES_recruit_mean)

# remove data for SSB below Blim
# build table with SSB corresponding to recruit
SSB_ICES <- SR_ICES[,c("species", "year", "recruit_age", "ICES_SSB_mean", "ICES_SSB_low", "ICES_SSB_high")]
SSB_ICES$year <-  SSB_ICES$year + SSB_ICES$recruit_age
names(SSB_ICES) <- c("species", "year", "recruit_age", "ICES_SSBatrecruit_mean", "ICES_SSBatrecruit_low", "ICES_SSBatrecruit_high")
SSB_ICES <- SSB_ICES[,c("species", "year", "ICES_SSBatrecruit_mean", "ICES_SSBatrecruit_low", "ICES_SSBatrecruit_high")]

SR_ICES <- merge(SR_ICES, SSB_ICES, all = T)
SR_ICES$ICES_SSBatrecruit_mean[SR_ICES$species == "Squalus acanthias"] <- 0 
SR_ICES <- SR_ICES[!is.na(SR_ICES$ICES_SSBatrecruit_mean) & !is.na(SR_ICES$ICES_recruit_mean),]
SR_ICES <- SR_ICES[SR_ICES$ICES_SSB_mean >= SR_ICES$ICES_Blim | is.na(SR_ICES$ICES_Blim),]

SR_ICES_mean <- aggregate(SR_ICES$ICES_recruit_mean, by = list(species = SR_ICES$species), mean)
SR_ICES_sd <- aggregate(SR_ICES$ICES_recruit_mean, by = list(species = SR_ICES$species), sd)

SR_ICES <- merge(SR_ICES, SR_ICES_mean, all = T)
names(SR_ICES)[length(names(SR_ICES))] <- "mean"
SR_ICES <- merge(SR_ICES, SR_ICES_sd, all = T)
names(SR_ICES)[length(names(SR_ICES))] <- "sd"
SR_ICES$ICES_recruit_error <- (SR_ICES$ICES_recruit_mean - SR_ICES$mean) / SR_ICES$mean
SR_ICES$ICES_recruit_norm_error <-  (SR_ICES$ICES_recruit_mean - SR_ICES$mean) / SR_ICES$sd

pdf("plots/ICES_Recruit_var.pdf", onefile = T)
ggplot(data = SR_ICES, aes(x = species, y = ICES_recruit_error, fill = species)) + geom_boxplot() + theme(axis.title.x=element_blank(),
                                                                                                               axis.text.x=element_blank(),
                                                                                                               axis.ticks.x=element_blank())

ggplot(data = SR_ICES, aes(x = species, y = ICES_recruit_norm_error, fill = species)) + geom_boxplot() + theme(axis.title.x=element_blank(),
                                                                                                               axis.text.x=element_blank(),
                                                                                                               axis.ticks.x=element_blank())
# delete species that don't have Atlantis group equivalents
SR_ICES <- SR_ICES[!is.na(SR_ICES_AEEC$AEEC_group) | SR_ICES$AEEC_group != "",]

# pool all species to make overall ALL estimate to use for unassesed species
SR_ICES$AEEC_group <- as.character(SR_ICES$AEEC_group)
SR_ICES_AEEC <- SR_ICES
SR_ICES_AEEC$AEEC_group <- "ALL"
SR_ICES_AEEC <- rbind(SR_ICES, SR_ICES_AEEC)

ggplot(data = SR_ICES_AEEC, aes(x = AEEC_group, y = ICES_recruit_error, fill = AEEC_group)) + geom_boxplot() + theme(axis.title.x=element_blank(),
                                                                                                          axis.text.x=element_blank(),
                                                                                                          axis.ticks.x=element_blank())

ggplot(data = SR_ICES_AEEC, aes(x = AEEC_group, y = ICES_recruit_norm_error, fill = AEEC_group)) + geom_boxplot() + theme(axis.title.x=element_blank(),
                                                                                                               axis.text.x=element_blank(),
                                                                                                               axis.ticks.x=element_blank())
dev.off()


BHalpha_mult_factors = SR_ICES_AEEC %>% group_by(AEEC_group) %>% 
  dplyr::summarise( level1 = 1 + quantile(ICES_recruit_error, probs = 0.05),
                    level2 = 1 + quantile(ICES_recruit_error, probs = 0.25),
                    level3 = 1 + quantile(ICES_recruit_error, probs = 0.75),
                    level4 = 1 + quantile(ICES_recruit_error, probs = 0.95) )
names(BHalpha_mult_factors)[1] = "Code"

write.csv(BHalpha_mult_factors, "files/BHalpha_mult_factors.csv", quote = FALSE, row.names = FALSE)
