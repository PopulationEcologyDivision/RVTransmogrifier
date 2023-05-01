# #<!agelen derived from dataLF>
# #binSizes and sexes may have been changed, so aggregate
# 
# 
# #was working in stratify_calcLenths, but is age-related

# 
# ##### 
# alw_pre<-  stats::aggregate(FWT~MISSION+SPEC+AGE+FLEN+FSEX,data=dataDETSFixed,FUN=mean)
# a2 = expand.grid(MISSION = theseMissions, SPEC = thisSpec, AGE = alk_ages, FLEN = -1, FWT = -1, FSEX = alk_sexes)
# 
# alw_pre=rbind(alw_pre,a2)
# alw_pre<-alw_pre[order(alw_pre$FSEX,alw_pre$AGE,alw_pre$FLEN),]
# 
# alw <- alw_pre %>%
#   filter(FLEN>0) %>% 
#   tidyr::pivot_wider(id_cols = c(MISSION, SPEC, FLEN), names_from = c(FSEX,AGE), values_from = FWT) %>% 
#   mutate_at(vars(-c(MISSION, SPEC, FLEN)), function(x) round(x/1000,6)) %>% 
#   as.data.frame()
# 
# colnames(alw) <- sub("0_", "UNKN_AGE_", colnames(alw))
# colnames(alw) <- sub("1_", "MALE_AGE_", colnames(alw))
# colnames(alw) <- sub("2_", "FEMALE_AGE_", colnames(alw))
# colnames(alw) <- sub("9_", "AGE_", colnames(alw))
# 
# message("think we can add average weight by adding all of the values for a length , and then 
#           dividing by count of animals of each group found in dataDETSFixed (then the same again for each age)")
# # dataDETSFixed[which(dataDETSFixed$FLEN==4.5),]
# 
# # 