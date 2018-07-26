## LOAD REQUIRED PACKAGES & FUNCTIONS

library(lme4)
source('contr.helmert.unweighted.R')
source('contr.helmert.weighted.R')
source('summaryCI.R')
source('logit.R')

## READ DATA

meta.FullData <- read.csv('Fraundorf_Hourihan_Peters_Benjamin.csv')


## SET CONTRASTS & CENTER

meta.FullData$YA_Discrimination.c <- meta.FullData$YA_Discrimination - mean(meta.FullData$YA_Discrimination)
meta.FullData$YA_Criterion.c <- meta.FullData$YA_Criterion - mean(meta.FullData$YA_Criterion)

meta.FullData$YA_HR_Logit <- logit(meta.FullData$YA_HR) 
meta.FullData$OA_HR_Logit <- logit(meta.FullData$OA_HR) 
meta.FullData$YA_FAR_Logit <- logit(meta.FullData$YA_FAR) 
meta.FullData$OA_FAR_Logit <- logit(meta.FullData$OA_FAR) 

meta.FullData$YA_HR.c <- meta.FullData$YA_HR_Logit - mean(meta.FullData$YA_HR_Logit)
meta.FullData$YA_FAR.c <- meta.FullData$YA_FAR_Logit - mean(meta.FullData$YA_FAR_Logit)

meta.FullData$AgeDiff_Discrimination <- meta.FullData$YA_Discrimination - meta.FullData$OA_Discrimination
meta.FullData$AgeDiff_Criterion <- meta.FullData$YA_Criterion - meta.FullData$OA_Criterion
meta.FullData$AgeDiff_Educ <- meta.FullData$OA_YearsEduc - meta.FullData$YA_YearsEduc

meta.FullData$AgeDiff_HR <- meta.FullData$OA_HR_Logit - meta.FullData$YA_HR_Logit
meta.FullData$AgeDiff_FAR <- meta.FullData$OA_FAR_Logit - meta.FullData$YA_FAR_Logit

meta.FullData$OA_Age.c <- meta.FullData$OA_Age - mean(meta.FullData$OA_Age)
meta.FullData$AgeDiff_Educ.cen <- meta.FullData$AgeDiff_Educ - mean(meta.FullData$AgeDiff_Educ, na.rm=TRUE)

meta.FullData$Total_N <- meta.FullData$YA_N + meta.FullData$OA_N

contrasts(meta.FullData$Faces) <- contr.helmert.weighted(meta.FullData$Faces)
contrasts(meta.FullData$Pictures) <- contr.helmert.weighted(meta.FullData$Pictures)
contrasts(meta.FullData$Text) <- contr.helmert.weighted(meta.FullData$Text)

contrasts(meta.FullData$AuditoryOnly) <- contr.helmert.weighted(meta.FullData$AuditoryOnly)
contrasts(meta.FullData$Audiovisual) <- contr.helmert.weighted(meta.FullData$Audiovisual)

contrasts(meta.FullData$GenerationTask) <- contr.helmert.weighted(meta.FullData$GenerationTask)
contrasts(meta.FullData$DividedAttention) <- contr.helmert.weighted(meta.FullData$DividedAttention)
contrasts(meta.FullData$DeepOrientingTask) <- contr.helmert.weighted(meta.FullData$DeepOrientingTask)
contrasts(meta.FullData$ShallowOrientingTask) <- contr.helmert.weighted(meta.FullData$ShallowOrientingTask)

meta.FullData$StudyTimePerItem.Sec.cen <- meta.FullData$StudyTimePerItem.Sec - mean(meta.FullData$StudyTimePerItem.Sec)

contrasts(meta.FullData$Production) <- contr.helmert.weighted(meta.FullData$Production)

contrasts(meta.FullData$ConjunctionLures) <- contr.helmert.weighted(meta.FullData$ConjunctionLures)
contrasts(meta.FullData$SemanticLures) <- contr.helmert.weighted(meta.FullData$SemanticLures)
contrasts(meta.FullData$FeaturalLures) <- contr.helmert.weighted(meta.FullData$FeaturalLures)
contrasts(meta.FullData$ComponentLures) <- contr.helmert.weighted(meta.FullData$ComponentLures)

contrasts(meta.FullData$PositiveValence) <- contr.helmert.weighted(meta.FullData$PositiveValence)
contrasts(meta.FullData$NegativeValence) <- contr.helmert.weighted(meta.FullData$NegativeValence)

meta.FullData$NumMemoranda.cen <- meta.FullData$NumMemoranda - mean(meta.FullData$NumMemoranda)

meta.FullData$NumTestProbes.cen <- meta.FullData$NumTestProbes - mean(meta.FullData$NumTestProbes)

meta.FullData$LogRetentionInterval.cen <- log(meta.FullData$RetentionInterval) - mean(log(meta.FullData$RetentionInterval))
meta.FullData$RetentionInterval <- meta.FullData$LogRetentionInterval.cen

contrasts(meta.FullData$MultipleStudyOpportunities) <- contr.helmert.weighted(meta.FullData$MultipleStudyOpportunities)

contrasts(meta.FullData$SelfPacedStudy) <- c(0,1)
contrasts(meta.FullData$IntentionalEncoding) <- contr.helmert.weighted(meta.FullData$IntentionalEncoding)

contrasts(meta.FullData$ContinuousRecog) <- contr.helmert.weighted(meta.FullData$ContinuousRecog)

contrasts(meta.FullData$InterveningCuedRecall) <- contr.helmert.weighted(meta.FullData$InterveningCuedRecall)
contrasts(meta.FullData$InterveningFreeRecall) <- contr.helmert.weighted(meta.FullData$InterveningFreeRecall)


## Create variables for the interactions so we can do LR test

# self-pacing x encoding type
meta.FullData$SelfPacedStudy.Num <- ifelse(meta.FullData$SelfPacedStudy=='TRUE', contrasts(meta.FullData$SelfPacedStudy)[2], contrasts(meta.FullData$SelfPacedStudy)[1])
meta.FullData$IntentionalEncoding.Num <- ifelse(meta.FullData$IntentionalEncoding=='TRUE', contrasts(meta.FullData$IntentionalEncoding)[2], contrasts(meta.FullData$IntentionalEncoding)[1])
meta.FullData$IntentionalSelfPacedStudy <- meta.FullData$SelfPacedStudy.Num * meta.FullData$IntentionalEncoding.Num

meta.FullData$ContinuousRecog.Num <- ifelse(meta.FullData$ContinuousRecog=='TRUE', contrasts(meta.FullData$ContinuousRecog)[2], contrasts(meta.FullData$ContinuousRecog)[1])
meta.FullData$ContinuousRI <- meta.FullData$ContinuousRecog.Num * meta.FullData$RetentionInterval


## COMPARE LEVEL OF EDUCATION

# Get just the subset of studies that report education:
meta.Educ <- meta.FullData[is.na(meta.FullData$AgeDiff_Educ) == FALSE, ]

summary(meta.Educ$YA_YearsEduc)
summary(meta.Educ$OA_YearsEduc)

t.test(meta.Educ$YA_YearsEduc, meta.Educ$OA_YearsEduc, paired=TRUE)


## DISCRIMINATION MODEL

model.Dprime.weighted <- lmer(AgeDiff_Discrimination ~ 1 + YA_Discrimination.c + OA_Age.c + Faces + Pictures + Text + PositiveValence + NegativeValence + AuditoryOnly + Audiovisual + IntentionalEncoding + ShallowOrientingTask + DeepOrientingTask + GenerationTask + DividedAttention + Production + NumMemoranda.cen + MultipleStudyOpportunities + SelfPacedStudy + IntentionalSelfPacedStudy + StudyTimePerItem.Sec.cen + InterveningCuedRecall + InterveningFreeRecall + RetentionInterval + ContinuousRecog + ContinuousRI + ConjunctionLures + SemanticLures + FeaturalLures + ComponentLures + NumTestProbes.cen + ProportionLures.cen + (1|Paper) + (1|Experiment) + (1|SubjectGroup) + (1|Lab), data=meta.FullData, weights=1/Total_N)
summaryCI(model.Dprime.weighted,digits=3)
drop1(model.Dprime.weighted, test='Chisq') # likelihood-ratio test

# Test for heterogeneity
model.Dprime.w.ExperimentOnly <- lmer(AgeDiff_Discrimination ~ 1 + (1|Experiment), data=meta.FullData, weights=1/Total_N)
model.Dprime.w.InterceptOnly <- lm(AgeDiff_Discrimination ~ 1, data=meta.FullData, weights=1/Total_N)
anova(model.Dprime.w.ExperimentOnly, model.Dprime.w.InterceptOnly)

# Test other random effects
model.Dprime.w.PaperOnly <- lmer(AgeDiff_Discrimination ~ 1 + (1|Paper), data=meta.FullData, weights=1/Total_N)
anova(model.Dprime.w.PaperOnly, model.Dprime.w.InterceptOnly)
model.Dprime.w.SubjectGroupOnly <- lmer(AgeDiff_Discrimination ~ 1 + (1|SubjectGroup), data=meta.FullData, weights=1/Total_N)
anova(model.Dprime.w.SubjectGroupOnly, model.Dprime.w.InterceptOnly)
model.Dprime.w.LabOnly <- lmer(AgeDiff_Discrimination ~ 1 + (1|Lab), data=meta.FullData, weights=1/Total_N)
anova(model.Dprime.w.LabOnly, model.Dprime.w.InterceptOnly)

# Peters' test for publication bias
meta.FullData$Total_N_Inverse <- 1/meta.FullData$Total_N
model.Dprime.weighted.bias <- lmer(AgeDiff_Discrimination ~ 1 + Total_N_Inverse + YA_Discrimination.c + OA_Age.c + Faces + Pictures + Text + PositiveValence + NegativeValence + AuditoryOnly + Audiovisual + IntentionalEncoding + ShallowOrientingTask + DeepOrientingTask + GenerationTask + DividedAttention + Production + NumMemoranda.cen + MultipleStudyOpportunities + SelfPacedStudy + IntentionalSelfPacedStudy + StudyTimePerItem.Sec.cen + InterveningCuedRecall + InterveningFreeRecall + RetentionInterval + ContinuousRecog + ContinuousRI + ConjunctionLures + SemanticLures + FeaturalLures + ComponentLures + NumTestProbes.cen + ProportionLures.cen + (1|Paper) + (1|Experiment) + (1|SubjectGroup) + (1|Lab), data=meta.FullData, weights=1/Total_N)
summaryCI(model.Dprime.weighted.bias, digits=3)
drop1(model.Dprime.weighted.bias, test='Chisq') # likelihood-ratio test

# Test interactions with face stimuli
model.Dprime.weighted.Face <- lmer(AgeDiff_Discrimination ~ 1 + AuditoryOnly + Audiovisual + GenerationTask + DividedAttention + InterveningCuedRecall + InterveningFreeRecall + Pictures + Text + Faces * (YA_Discrimination.c + OA_Age.c + PositiveValence + NegativeValence + IntentionalEncoding + ShallowOrientingTask + DeepOrientingTask + Production + NumMemoranda.cen + MultipleStudyOpportunities + SelfPacedStudy + IntentionalSelfPacedStudy + StudyTimePerItem.Sec.cen + RetentionInterval + ContinuousRecog + ContinuousRI + ConjunctionLures + SemanticLures + FeaturalLures + ComponentLures + NumTestProbes.cen + ProportionLures.cen) + (1|Paper) + (1|Experiment) + (1|SubjectGroup) + (1|Lab), data=meta.FullData, weights=1/Total_N)
summaryCI(model.Dprime.weighted.Face,digits=3)


## CRITERION MODEL

model.C.weighted <- lmer(AgeDiff_Criterion ~ 1 + YA_Criterion.c + OA_Age.c + Faces + Pictures + Text + PositiveValence + NegativeValence + AuditoryOnly + Audiovisual + IntentionalEncoding + ShallowOrientingTask + DeepOrientingTask + GenerationTask + DividedAttention + Production + NumMemoranda.cen + MultipleStudyOpportunities + SelfPacedStudy + IntentionalSelfPacedStudy + StudyTimePerItem.Sec.cen + InterveningCuedRecall + InterveningFreeRecall + RetentionInterval + ContinuousRecog + ContinuousRI + ConjunctionLures + SemanticLures + FeaturalLures + ComponentLures + NumTestProbes.cen + ProportionLures.cen + (1|Paper) + (1|Experiment) + (1|SubjectGroup) + (1|Lab), data=meta.FullData, weights=1/Total_N)
summaryCI(model.C.weighted,digits=3)
drop1(model.C.weighted, test='Chisq') # likelihood-ratio test

# Test for heterogeneity
model.C.w.InterceptOnly <- lm(AgeDiff_Criterion ~ 1, data=meta.FullData, weights=1/Total_N)
model.C.w.ExperimentOnly <- lmer(AgeDiff_Criterion ~ 1 + (1|Experiment), data=meta.FullData, weights=1/Total_N)
anova(model.C.w.ExperimentOnly, model.C.w.InterceptOnly)

# Test other random effects
model.C.w.PaperOnly <- lmer(AgeDiff_Criterion ~ 1 + (1|Paper), data=meta.FullData, weights=1/Total_N)
anova(model.C.w.PaperOnly, model.C.w.InterceptOnly)
model.C.w.SubjectGroupOnly <- lmer(AgeDiff_Criterion ~ 1 + (1|SubjectGroup), data=meta.FullData, weights=1/Total_N)
anova(model.C.w.SubjectGroupOnly, model.C.w.InterceptOnly)
model.C.w.LabOnly <- lmer(AgeDiff_Criterion ~ 1 + (1|Lab), data=meta.FullData, weights=1/Total_N)
anova(model.C.w.LabOnly, model.C.w.InterceptOnly)

# Test for criteron differences even controlling for d'
model.C.weighted.d <- lmer(AgeDiff_Criterion ~ 1 + AgeDiff_Discrimination + YA_Criterion.c + OA_Age.c + Faces + Pictures + Text + PositiveValence + NegativeValence + AuditoryOnly + Audiovisual + IntentionalEncoding + ShallowOrientingTask + DeepOrientingTask + GenerationTask + DividedAttention + Production + NumMemoranda.cen + MultipleStudyOpportunities + SelfPacedStudy + IntentionalSelfPacedStudy + StudyTimePerItem.Sec.cen + InterveningCuedRecall + InterveningFreeRecall + RetentionInterval + ContinuousRecog + ContinuousRI + ConjunctionLures + SemanticLures + FeaturalLures + ComponentLures + NumTestProbes.cen + ProportionLures.cen + (1|Paper) + (1|Experiment) + (1|SubjectGroup) + (1|Lab), data=meta.FullData, weights=1/Total_N)
summaryCI(model.C.weighted.d, digits=3)
drop1(model.C.weighted.d, test='Chisq') # likelihood-ratio test

# Peters' test for publication bias
meta.FullData$Total_N_Inverse <- 1/meta.FullData$Total_N
model.C.weighted.bias <- lmer(AgeDiff_Criterion ~ 1 + Total_N_Inverse + YA_Criterion.c + OA_Age.c + Faces + Pictures + Text + PositiveValence + NegativeValence + AuditoryOnly + Audiovisual + IntentionalEncoding + ShallowOrientingTask + DeepOrientingTask + GenerationTask + DividedAttention + Production + NumMemoranda.cen + MultipleStudyOpportunities + SelfPacedStudy + IntentionalSelfPacedStudy + StudyTimePerItem.Sec.cen + InterveningCuedRecall + InterveningFreeRecall + RetentionInterval + ContinuousRecog + ContinuousRI + ConjunctionLures + SemanticLures + FeaturalLures + ComponentLures + NumTestProbes.cen + ProportionLures.cen + (1|Paper) + (1|Experiment) + (1|SubjectGroup) + (1|Lab), data=meta.FullData, weights=1/Total_N)
summaryCI(model.C.weighted.bias, digits=3)
drop1(model.C.weighted.bias, test='Chisq') # likelihood-ratio test

# Test interactions with face stimuli
model.C.weighted.Face <- lmer(AgeDiff_Criterion ~ 1 + AuditoryOnly + Audiovisual + GenerationTask + DividedAttention + InterveningCuedRecall + InterveningFreeRecall + Pictures + Text + Faces * (YA_Criterion.c + OA_Age.c + PositiveValence + NegativeValence + IntentionalEncoding + ShallowOrientingTask + DeepOrientingTask + Production + NumMemoranda.cen + MultipleStudyOpportunities + SelfPacedStudy + IntentionalSelfPacedStudy + StudyTimePerItem.Sec.cen + RetentionInterval + ContinuousRecog + ContinuousRI + ConjunctionLures + SemanticLures + FeaturalLures + ComponentLures + NumTestProbes.cen + ProportionLures.cen) + (1|Paper) + (1|Experiment) + (1|SubjectGroup) + (1|Lab), data=meta.FullData, weights=1/Total_N)
summaryCI(model.C.weighted.Face,digits=3)


## TABLES

# Table 1: dependent measures
DVTable <- cbind(c(mean(meta.FullData$YA_Discrimination), mean(meta.FullData$OA_Discrimination), mean(meta.FullData$AgeDiff_Discrimination), mean(meta.FullData$YA_Criterion), mean(meta.FullData$OA_Criterion), mean(meta.FullData$AgeDiff_Criterion)), 
                c(sd(meta.FullData$YA_Discrimination), sd(meta.FullData$OA_Discrimination), sd(meta.FullData$AgeDiff_Discrimination), sd(meta.FullData$YA_Criterion), sd(meta.FullData$OA_Criterion), sd(meta.FullData$AgeDiff_Criterion)))
colnames(DVTable) = c('Mean', 'SD')
rownames(DVTable) = c("d' - YA", "d' - OA", "d'- Diff", "c - YA", "c - OA", "c - Diff")
DVTable

# Table 2: independent measures
IVTable <- cbind(c(mean(meta.FullData$OA_Age),
                   mean(ifelse(meta.FullData$Words==TRUE, 100, 0)),
                   mean(ifelse(meta.FullData$Pictures==TRUE, 100, 0)),
                   mean(ifelse(meta.FullData$Faces==TRUE, 100, 0)),
                   mean(ifelse(meta.FullData$Text==TRUE, 100, 0)),
                   mean(ifelse(meta.FullData$PositiveValence==TRUE, 100, 0)),
                   mean(ifelse(meta.FullData$NeutralValence==TRUE, 100, 0)),
                   mean(ifelse(meta.FullData$NegativeValence==TRUE, 100, 0)),
                   mean(ifelse(meta.FullData$VisualOnly==TRUE, 100, 0)),
                   mean(ifelse(meta.FullData$AuditoryOnly==TRUE, 100, 0)),
                   mean(ifelse(meta.FullData$Audiovisual==TRUE, 100, 0)),                   
                   mean(ifelse(meta.FullData$DeepOrientingTask==TRUE, 100, 0)),                                      
                   mean(ifelse(meta.FullData$ShallowOrientingTask==TRUE, 100, 0)),
                   mean(ifelse(meta.FullData$GenerationTask==TRUE, 100, 0)),                                                         
                   mean(ifelse(meta.FullData$DividedAttention==TRUE, 100, 0)),                                      
                   mean(ifelse(meta.FullData$Production=='Production', 100, 0)),                                      
                   mean(ifelse(meta.FullData$SelfPacedStudy=='Self-paced', 100, 0)),                                      
                   mean(meta.FullData[meta.FullData$StudyTimePerItem.Sec!=0, ]$StudyTimePerItem.Sec),
                   mean(ifelse(meta.FullData$MultipleStudyOpportunities=='Multiple', 100, 0)),                                      
                   mean(ifelse(meta.FullData$IntentionalEncoding=='Intentional', 100, 0)),                                      
                   mean(meta.FullData$NumMemoranda),
                   mean(ifelse(meta.FullData$InterveningCuedRecall==TRUE, 100, 0)),                                                         
                   mean(ifelse(meta.FullData$InterveningFreeRecall==TRUE, 100, 0)),                                                                            
                   mean(meta.FullData$RetentionInterval),
                   exp(mean(log(meta.FullData$RetentionInterval))),                   
                   mean(ifelse(meta.FullData$ContinuousRecog==TRUE, 100, 0)),                                                                            
                   mean(meta.FullData$NumTestProbes),
                   mean(ifelse(meta.FullData$ConjunctionLures==TRUE, 100, 0)),
                   mean(ifelse(meta.FullData$SemanticLures==TRUE, 100, 0)),
                   mean(ifelse(meta.FullData$FeaturalLures==TRUE, 100, 0)),
                   mean(ifelse(meta.FullData$ComponentLures==TRUE, 100, 0)),
                   mean(ifelse(meta.FullData$UnrelatedLures==TRUE, 100, 0)),
                   mean(meta.FullData$ProportionLures)),
                 c(sd(meta.FullData$OA_Age),
                   NA, NA, NA, NA,
                   NA, NA, NA,
                   NA, NA, NA,
                   NA, NA, NA, NA,
                   NA,
                   NA,
                   sd(meta.FullData[meta.FullData$StudyTimePerItem.Sec!=0, ]$StudyTimePerItem.Sec),
                   NA,
                   NA,
                   sd(meta.FullData$NumMemoranda),
                   NA, NA, 
                   sd(meta.FullData$RetentionInterval),
                   exp(sd(log(meta.FullData$RetentionInterval))),
                   NA,
                   sd(meta.FullData$NumTestProbes),
                   NA, NA, NA, NA, NA,
                   sd(meta.FullData$ProportionLures)),
				c(NA,
                   sum(meta.FullData$Words==TRUE),
                   sum(meta.FullData$Pictures==TRUE),
                   sum(meta.FullData$Faces==TRUE),
                   sum(meta.FullData$Text==TRUE),
                   sum(meta.FullData$PositiveValence==TRUE),
                   sum(meta.FullData$NeutralValence==TRUE),
                   sum(meta.FullData$NegativeValence==TRUE),
                   sum(meta.FullData$VisualOnly==TRUE),
                   sum(meta.FullData$AuditoryOnly==TRUE),
                   sum(meta.FullData$Audiovisual==TRUE),
                   sum(meta.FullData$DeepOrientingTask==TRUE),
                   sum(meta.FullData$ShallowOrientingTask==TRUE),
                   sum(meta.FullData$GenerationTask==TRUE),
                   sum(meta.FullData$DividedAttenion==TRUE),
                   sum(meta.FullData$Production==TRUE),
                   sum(meta.FullData$SelfPacedStudy==TRUE),
                   NA,
                   sum(meta.FullData$MultipleStudyOpportuniities==TRUE),
                   sum(meta.FullData$IntentionalEncoding==TRUE),
                   NA,
                   sum(meta.FullData$InterveningCuedRecall==TRUE),
                   sum(meta.FullData$InterveningFreeRecall==TRUE),
                   NA,
                   NA,
                   sum(meta.FullData$ContinuousRecog==TRUE),
                   NA,
                   sum(meta.FullData$ConjunctionLures==TRUE),
                   sum(meta.FullData$SemanticLures==TRUE),
                   sum(meta.FullData$FeaturalLures==TRUE),
                   sum(meta.FullData$ComponentLures==TRUE),
                   sum(meta.FullData$UnrelatedLures==TRUE),
                   NA))                        
colnames(IVTable) = c('Mean', 'SD', 'Count')
rownames(IVTable) = c('OA Age', 'Words', 'Pictures','Faces','Texts','Positive valence', 'Neutral valence', 'Negative valence', 'Visual', 'Auditory', 'Multimodal', 'Deep', 'Shallow', 'Generation', 'Divided attn', 'Production', 'Self-paced', 'Study time', 'Multiple study', 'Intentionality', 'Num memoranda', 'Intervening cued recall', 'Intervening free recall', 'Retention interval', 'Log retention interval', 'Continuous recog', 'Test list length', 'Conjunction lures', 'Component lures', 'Semantic lures', 'Featural lures', 'Unrelated lures', 'Proportion lures')
round(IVTable, digits=1)


## FIGURES

# Plot all d' points
pdf('Figure2-OA_Dprime_vs_YA_Dprime.pdf')
plot(meta.FullData$YA_Discrimination, meta.FullData$OA_Discrimination, xlab="Young adult d'", ylab="Older adult d'", xlim=c(0,5), ylim=c(0,5), bty='l')
abline(a=0, b=1)
dev.off()

# Plot all c points
pdf('Figure3-OA_c_vs_YA_c.pdf')
plot(meta.FullData$YA_Criterion, meta.FullData$OA_Criterion, xlab="Young adult c", ylab="Older adult c", xlim=c(-1.5,1.5), ylim=c(-1.5,1.5), bty='l')
abline(h=0, lty=2)
abline(v=0, lty=2)
abline(a=0, b=1)
dev.off()


## ALTERNATE MODELS WITH POTENTIAL OUTLIERS REMOVED

# Discrimination model
# Trim based on residuals
meta.D.Trimmed <- meta.FullData[abs(scale(resid(model.Dprime.weighted))) <= 3, ]
1-(nrow(meta.D.Trimmed)/nrow(meta.FullData))
# and refit model:
model.Dprime.Trimmed <- lmer(AgeDiff_Discrimination ~ 1 + YA_Discrimination.c + OA_Age.c + Faces + Pictures + Text + PositiveValence + NegativeValence + AuditoryOnly + Audiovisual + IntentionalEncoding + ShallowOrientingTask + DeepOrientingTask + GenerationTask + DividedAttention + Production + NumMemoranda.cen + MultipleStudyOpportunities + SelfPacedStudy + IntentionalSelfPacedStudy + StudyTimePerItem.Sec.cen + InterveningCuedRecall + InterveningFreeRecall + RetentionInterval + ContinuousRecog + ContinuousRI + ConjunctionLures + SemanticLures + FeaturalLures + ComponentLures + NumTestProbes.cen + ProportionLures.cen + (1|Paper) + (1|Experiment) + (1|SubjectGroup) + (1|Lab), data=meta.D.Trimmed, weights=1/Total_N)
summaryCI(model.Dprime.Trimmed)
drop1(model.Dprime.Trimmed, test='Chisq')

# Criterion model
# Trim based on residuals
meta.C.Trimmed <- meta.FullData[abs(scale(resid(model.C.weighted))) <= 3, ]
1-(nrow(meta.C.Trimmed)/nrow(meta.FullData))
# and refit model:
model.C.Trimmed <- lmer(AgeDiff_Criterion ~ 1 + YA_Criterion.c + OA_Age.c + Faces + Pictures + Text + PositiveValence + NegativeValence + AuditoryOnly + Audiovisual + IntentionalEncoding + ShallowOrientingTask + DeepOrientingTask + GenerationTask + DividedAttention + Production + NumMemoranda.cen + MultipleStudyOpportunities + SelfPacedStudy + IntentionalSelfPacedStudy + StudyTimePerItem.Sec.cen + InterveningCuedRecall + InterveningFreeRecall + RetentionInterval + ContinuousRecog + ContinuousRI + ConjunctionLures + SemanticLures + FeaturalLures + ComponentLures + NumTestProbes.cen + ProportionLures.cen + (1|Paper) + (1|Experiment) + (1|SubjectGroup) + (1|Lab), data=meta.C.Trimmed, weights=1/Total_N)
summaryCI(model.C.Trimmed)
drop1(model.C.Trimmed, test='Chisq')


## APPENDIX A: MODELS OF HITS AND FALSE ALARMS

# Hit rates
model.HR.weighted <- lmer(AgeDiff_HR ~ 1 + YA_HR.c + OA_Age.c + Faces + Pictures + Text + PositiveValence + NegativeValence + AuditoryOnly + Audiovisual + IntentionalEncoding + ShallowOrientingTask + DeepOrientingTask + GenerationTask + DividedAttention + Production + NumMemoranda.cen + MultipleStudyOpportunities + SelfPacedStudy + IntentionalSelfPacedStudy + StudyTimePerItem.Sec.cen + InterveningCuedRecall + InterveningFreeRecall + RetentionInterval + ContinuousRecog + ContinuousRI + ConjunctionLures + SemanticLures + FeaturalLures + ComponentLures + NumTestProbes.cen + ProportionLures.cen + (1|Paper) + (1|Experiment) + (1|SubjectGroup) + (1|Lab), data=meta.FullData, weights=1/Total_N)
summaryCI(model.HR.weighted, digits=3)
drop1(model.HR.weighted, test='Chisq')

# False alarm rates
model.FAR.weighted <- lmer(AgeDiff_FAR ~ 1 + YA_FAR.c + OA_Age.c + Faces + Pictures + Text + PositiveValence + NegativeValence + AuditoryOnly + Audiovisual + IntentionalEncoding + ShallowOrientingTask + DeepOrientingTask + GenerationTask + DividedAttention + Production + NumMemoranda.cen + MultipleStudyOpportunities + SelfPacedStudy + IntentionalSelfPacedStudy + StudyTimePerItem.Sec.cen + InterveningCuedRecall + InterveningFreeRecall + RetentionInterval + ContinuousRecog + ContinuousRI + ConjunctionLures + SemanticLures + FeaturalLures + ComponentLures + NumTestProbes.cen + ProportionLures.cen + (1|Paper) + (1|Experiment) + (1|SubjectGroup) + (1|Lab), data=meta.FullData, weights=1/Total_N)
summaryCI(model.FAR.weighted, digits=3)
drop1(model.FAR.weighted, test='Chisq')


## SUPPLEMENTARY ANALYSES

# Interactions with age w/in older sample - discrimination
model.Dprime.Age <- lmer(AgeDiff_Discrimination ~ 1 + OA_Age.c * (YA_Discrimination.c + Faces + Pictures + Text + PositiveValence + NegativeValence + AuditoryOnly + Audiovisual + IntentionalEncoding + ShallowOrientingTask + DeepOrientingTask + GenerationTask + DividedAttention + Production + NumMemoranda.cen + MultipleStudyOpportunities + SelfPacedStudy + IntentionalSelfPacedStudy + StudyTimePerItem.Sec.cen + InterveningCuedRecall + InterveningFreeRecall + RetentionInterval + ContinuousRecog + ContinuousRI + ConjunctionLures + SemanticLures + FeaturalLures + ComponentLures + NumTestProbes.cen + ProportionLures.cen) + (1|Paper) + (1|Experiment) + (1|SubjectGroup) + (1|Lab), data=meta.FullData, weights=1/Total_N)
summaryCI(model.Dprime.Age,digits=3)

# Interactions with age w/in older sample - criterion
model.C.Age <- lmer(AgeDiff_Discrimination ~ 1 + OA_Age.c * (YA_Criterion.c + Faces + Pictures + Text + PositiveValence + NegativeValence + AuditoryOnly + Audiovisual + IntentionalEncoding + ShallowOrientingTask + DeepOrientingTask + GenerationTask + DividedAttention + Production + NumMemoranda.cen + MultipleStudyOpportunities + SelfPacedStudy + IntentionalSelfPacedStudy + StudyTimePerItem.Sec.cen + InterveningCuedRecall + InterveningFreeRecall + RetentionInterval + ContinuousRecog + ContinuousRI + ConjunctionLures + SemanticLures + FeaturalLures + ComponentLures + NumTestProbes.cen + ProportionLures.cen) + (1|Paper) + (1|Experiment) + (1|SubjectGroup) + (1|Lab), data=meta.FullData, weights=1/Total_N)
summaryCI(model.C.Age,digits=3)

# Effects of education (SMALLER SAMPLE) - discrimination
model.Dprime.Educ <- lmer(AgeDiff_Discrimination ~ 1 + YA_Discrimination.c + OA_Age.c + Faces + Pictures + Text + PositiveValence + NegativeValence + AuditoryOnly + Audiovisual + IntentionalEncoding + ShallowOrientingTask + DeepOrientingTask + GenerationTask + DividedAttention + Production + NumMemoranda.cen + MultipleStudyOpportunities + SelfPacedStudy + IntentionalSelfPacedStudy + StudyTimePerItem.Sec.cen + InterveningCuedRecall + InterveningFreeRecall + RetentionInterval + ContinuousRecog + ContinuousRI + ConjunctionLures + SemanticLures + FeaturalLures + ComponentLures + NumTestProbes.cen + ProportionLures.cen + AgeDiff_Educ.cen + (1|Paper) + (1|Experiment) + (1|SubjectGroup) + (1|Lab), data=meta.Educ, weights=1/Total_N)
summaryCI(model.Dprime.Educ,digits=3)
drop1(model.Dprime.Educ, test='Chisq') # likelihood-ratio test

# Effects of education (SMALLER SAMPLE) - criterion
model.C.Educ <- lmer(AgeDiff_Criterion ~ 1 + YA_Criterion.c + OA_Age.c + Faces + Pictures + Text + PositiveValence + NegativeValence + AuditoryOnly + Audiovisual + IntentionalEncoding + ShallowOrientingTask + DeepOrientingTask + GenerationTask + DividedAttention + Production + NumMemoranda.cen + MultipleStudyOpportunities + SelfPacedStudy + IntentionalSelfPacedStudy + StudyTimePerItem.Sec.cen + InterveningCuedRecall + InterveningFreeRecall + RetentionInterval + ContinuousRecog + ContinuousRI + ConjunctionLures + SemanticLures + FeaturalLures + ComponentLures + NumTestProbes.cen + ProportionLures.cen + AgeDiff_Educ.cen + (1|Paper) + (1|Experiment) + (1|SubjectGroup) + (1|Lab), data=meta.Educ, weights=1/Total_N)
summaryCI(model.C.Educ,digits=3)
drop1(model.C.Educ, test='Chisq') # likelihood-ratio test