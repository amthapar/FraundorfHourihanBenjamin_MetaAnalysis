## LOAD REQUIRED PACKAGES & FUNCTIONS

library(lme4)
source('contr.helmert.unweighted.R')
source('contr.helmert.weighted.R')
source('summaryCI.R')

## READ DATA

meta.FullData <- read.csv('Fraundorf_Hourihan_Benjamin.csv')


## SET CONTRASTS & CENTER

meta.FullData$YA_Discrimination.c <- meta.FullData$YA_Discrimination - mean(meta.FullData$YA_Discrimination)
meta.FullData$YA_Criterion.c <- meta.FullData$YA_Criterion - mean(meta.FullData$YA_Criterion)

meta.FullData$AgeDiff_Discrimination <- meta.FullData$YA_Discrimination - meta.FullData$OA_Discrimination
meta.FullData$AgeDiff_Criterion <- meta.FullData$YA_Criterion - meta.FullData$OA_Criterion

meta.FullData$OA_Age.c <- meta.FullData$OA_Age - mean(meta.FullData$OA_Age)

meta.FullData$Total_N <- meta.FullData$YA_N + meta.FullData$OA_N

contrasts(meta.FullData$Faces) <- contr.helmert.weighted(meta.FullData$Faces)
contrasts(meta.FullData$Pictures) <- contr.helmert.weighted(meta.FullData$Pictures)
contrasts(meta.FullData$Text) <- contr.helmert.weighted(meta.FullData$Text)

contrasts(meta.FullData$AuditoryOnly) <- contr.helmert.weighted(meta.FullData$AuditoryOnly)
contrasts(meta.FullData$Audiovisual) <- contr.helmert.weighted(meta.FullData$Audiovisual)
contrasts(meta.FullData$ModalityMatch) <- contr.helmert.weighted(meta.FullData$ModalityMatch)

contrasts(meta.FullData$GenerationStudy) <- contr.helmert.weighted(meta.FullData$GenerationStudy)
contrasts(meta.FullData$DividedStudy) <- contr.helmert.weighted(meta.FullData$DividedStudy)
contrasts(meta.FullData$DeepStudy) <- contr.helmert.weighted(meta.FullData$DeepStudy)
contrasts(meta.FullData$ShallowStudy) <- contr.helmert.weighted(meta.FullData$ShallowStudy)

meta.FullData$StudyTimePer.Num.c <- meta.FullData$StudyTimePer.Num - mean(meta.FullData$StudyTimePer.Num)

contrasts(meta.FullData$Production) <- contr.helmert.weighted(meta.FullData$Production)

contrasts(meta.FullData$ConjunctionLures) <- contr.helmert.weighted(meta.FullData$ConjunctionLures)
contrasts(meta.FullData$SemanticLures) <- contr.helmert.weighted(meta.FullData$SemanticLures)
contrasts(meta.FullData$FeaturalLures) <- contr.helmert.weighted(meta.FullData$FeaturalLures)
contrasts(meta.FullData$ComponentLures) <- contr.helmert.weighted(meta.FullData$ComponentLures)

contrasts(meta.FullData$PositiveValence) <- contr.helmert.weighted(meta.FullData$PositiveValence)
contrasts(meta.FullData$NegativeValence) <- contr.helmert.weighted(meta.FullData$NegativeValence)

meta.FullData$NumTBRItems.c <- meta.FullData$NumTBRItems - mean(meta.FullData$NumTBRItems)

meta.FullData$TestListLength.Num.c <- meta.FullData$TestListLength.Num - mean(meta.FullData$TestListLength.Num)

meta.FullData$LogRetentionInterval.Num.c <- log(meta.FullData$RetentionInterval.Num) - mean(log(meta.FullData$RetentionInterval.Num))
meta.FullData$RetentionInterval <- meta.FullData$LogRetentionInterval.Num.c

contrasts(meta.FullData$MultipleStudy) <- contr.helmert.weighted(meta.FullData$MultipleStudy)

contrasts(meta.FullData$SelfPaced) <- c(0,1)
contrasts(meta.FullData$EncodingType) <- contr.helmert.weighted(meta.FullData$EncodingType)

contrasts(meta.FullData$ContinuousRecog) <- contr.helmert.weighted(meta.FullData$ContinuousRecog)

contrasts(meta.FullData$InterveningCuedRecall) <- contr.helmert.weighted(meta.FullData$InterveningCuedRecall)
contrasts(meta.FullData$InterveningFreeRecall) <- contr.helmert.weighted(meta.FullData$InterveningFreeRecall)

## Create variables for the interactions so we can do LR test

# self-pacing x encoding type
meta.FullData$SelfPaced.Num <- ifelse(meta.FullData$SelfPaced=='Self-paced', contrasts(meta.FullData$SelfPaced)[2], contrasts(meta.FullData$SelfPaced)[1])
meta.FullData$EncodingType.Num <- ifelse(meta.FullData$EncodingType=='Intentional', contrasts(meta.FullData$EncodingType)[2], contrasts(meta.FullData$EncodingType)[1])
meta.FullData$IntentionalSelfPaced <- meta.FullData$SelfPaced.Num * meta.FullData$EncodingType.Num

meta.FullData$ContinuousRecog.Num <- ifelse(meta.FullData$ContinuousRecog==TRUE, contrasts(meta.FullData$ContinuousRecog)[2], contrasts(meta.FullData$ContinuousRecog)[1])
meta.FullData$ContinuousRI <- meta.FullData$ContinuousRecog.Num * meta.FullData$RetentionInterval


## DISCRIMINATION MODEL

model.Dprime.weighted <- lmer(AgeDiff_Discrimination ~ 1 + YA_Discrimination.c + OA_Age.c + Faces + Pictures + Text + PositiveValence + NegativeValence + AuditoryOnly + Audiovisual + EncodingType + ShallowStudy + DeepStudy + GenerationStudy + DividedStudy + Production + NumTBRItems.c + MultipleStudy + SelfPaced + IntentionalSelfPaced + StudyTimePer.Num + InterveningCuedRecall + InterveningFreeRecall + RetentionInterval + ContinuousRecog + ContinuousRI + ConjunctionLures + SemanticLures + FeaturalLures + ComponentLures + TestListLength.Num.c + propLures.c + (1|PaperID) + (1|StudyID) + (1|SubjGroup), data=meta.FullData, weights=1/Total_N)
summaryCI(model.Dprime.weighted)
drop1(model.Dprime.weighted, test='Chisq') # likelihood-ratio test

# Peters' test for publication bias
meta.FullData$Total_N_Inverse <- 1/meta.FullData$Total_N
model.Dprime.weighted.bias <- lmer(AgeDiff_Discrimination ~ 1 + Total_N_Inverse + YA_Discrimination.c + OA_Age.c + Faces + Pictures + Text + PositiveValence + NegativeValence + AuditoryOnly + Audiovisual + EncodingType + ShallowStudy + DeepStudy + GenerationStudy + DividedStudy + Production + NumTBRItems.c + MultipleStudy + SelfPaced + IntentionalSelfPaced + StudyTimePer.Num + InterveningCuedRecall + InterveningFreeRecall + RetentionInterval + ContinuousRecog + ContinuousRI + ConjunctionLures + SemanticLures + FeaturalLures + ComponentLures + TestListLength.Num.c + propLures.c + (1|PaperID) + (1|StudyID) + (1|SubjGroup), data=meta.FullData, weights=1/Total_N)
summaryCI(model.Dprime.weighted.bias)
drop1(model.Dprime.weighted.bias, test='Chisq') # likelihood-ratio test


## CRITERION MODEL

model.C.weighted <- lmer(AgeDiff_Criterion ~ 1 + YA_Criterion.c + OA_Age.c + Faces + Pictures + Text + PositiveValence + NegativeValence + AuditoryOnly + Audiovisual + EncodingType + ShallowStudy + DeepStudy + GenerationStudy + DividedStudy + Production + NumTBRItems.c + MultipleStudy + SelfPaced + IntentionalSelfPaced + StudyTimePer.Num.c + InterveningCuedRecall + InterveningFreeRecall + RetentionInterval + ContinuousRecog + ContinuousRI + ConjunctionLures + SemanticLures + FeaturalLures + ComponentLures + TestListLength.Num.c + propLures.c + (1|PaperID) + (1|StudyID) + (1|SubjGroup), data=meta.FullData, weights=1/Total_N)
summaryCI(model.C.weighted)
drop1(model.C.weighted, test='Chisq') # likelihood-ratio test

# Test for criteron differences even controlling for d'
model.C.weighted.d <- lmer(AgeDiff_Criterion ~ 1 + AgeDiff_Discrimination + YA_Criterion.c + OA_Age.c + Faces + Pictures + Text + PositiveValence + NegativeValence + AuditoryOnly + Audiovisual + EncodingType + ShallowStudy + DeepStudy + GenerationStudy + DividedStudy + Production + NumTBRItems.c + MultipleStudy + SelfPaced + IntentionalSelfPaced + StudyTimePer.Num.c + InterveningCuedRecall + InterveningFreeRecall + RetentionInterval + ContinuousRecog + ContinuousRI + ConjunctionLures + SemanticLures + FeaturalLures + ComponentLures + TestListLength.Num.c + propLures.c + (1|PaperID) + (1|StudyID) + (1|SubjGroup), data=meta.FullData, weights=1/Total_N)
summaryCI(model.C.weighted.d)
drop1(model.C.weighted.d, test='Chisq') # likelihood-ratio test

# Peters' test for publication bias
meta.FullData$Total_N_Inverse <- 1/meta.FullData$Total_N
model.C.weighted.bias <- lmer(AgeDiff_Criterion ~ 1 + Total_N_Inverse + YA_Criterion.c + OA_Age.c + Faces + Pictures + Text + PositiveValence + NegativeValence + AuditoryOnly + Audiovisual + EncodingType + ShallowStudy + DeepStudy + GenerationStudy + DividedStudy + Production + NumTBRItems.c + MultipleStudy + SelfPaced + IntentionalSelfPaced + StudyTimePer.Num.c + InterveningCuedRecall + InterveningFreeRecall + RetentionInterval + ContinuousRecog + ContinuousRI + ConjunctionLures + SemanticLures + FeaturalLures + ComponentLures + TestListLength.Num.c + propLures.c + (1|PaperID) + (1|StudyID) + (1|SubjGroup), data=meta.FullData, weights=1/Total_N)
summaryCI(model.C.weighted.bias)
drop1(model.C.weighted.bias, test='Chisq') # likelihood-ratio test

## TABLES

# dependent measures
DVTable <- cbind(c(mean(meta.FullData$YA_Discrimination), mean(meta.FullData$OA_Discrimination), mean(meta.FullData$AgeDiff_Discrimination), mean(meta.FullData$YA_Criterion), mean(meta.FullData$OA_Criterion), mean(meta.FullData$AgeDiff_Criterion)), 
                c(sd(meta.FullData$YA_Discrimination), sd(meta.FullData$OA_Discrimination), sd(meta.FullData$AgeDiff_Discrimination), sd(meta.FullData$YA_Criterion), sd(meta.FullData$OA_Criterion), sd(meta.FullData$AgeDiff_Criterion)))
colnames(DVTable) = c('Mean', 'SD')
rownames(DVTable) = c("d' - YA", "d' - OA", "d'- Diff", "c - YA", "c - OA", "c - Diff")
DVTable

# independent measures
# All IVs
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
                   mean(ifelse(meta.FullData$DeepStudy==TRUE, 100, 0)),                                      
                   mean(ifelse(meta.FullData$ShallowStudy==TRUE, 100, 0)),
                   mean(ifelse(meta.FullData$GenerationStudy==TRUE, 100, 0)),                                                         
                   mean(ifelse(meta.FullData$DividedStudy==TRUE, 100, 0)),                                      
                   mean(ifelse(meta.FullData$Production=='Production', 100, 0)),                                      
                   mean(ifelse(meta.FullData$SelfPaced=='Self-paced', 100, 0)),                                      
                   mean(meta.FullData[meta.FullData$StudyTimePer.Num!=0, ]$StudyTimePer.Num),
                   mean(ifelse(meta.FullData$MultipleStudy=='Multiple', 100, 0)),                                      
                   mean(ifelse(meta.FullData$EncodingType=='Intentional', 100, 0)),                                      
                   mean(meta.FullData$NumTBRItems),
                   mean(ifelse(meta.FullData$InterveningCuedRecall==TRUE, 100, 0)),                                                         
                   mean(ifelse(meta.FullData$InterveningFreeRecall==TRUE, 100, 0)),                                                                            
                   mean(meta.FullData$RetentionInterval.Num),
                   exp(mean(log(meta.FullData$RetentionInterval.Num))),                   
                   mean(ifelse(meta.FullData$ContinuousRecog==TRUE, 100, 0)),                                                                            
                   mean(meta.FullData$TestListLength.Num),
                   mean(ifelse(meta.FullData$ConjunctionLures==TRUE, 100, 0)),
                   mean(ifelse(meta.FullData$SemanticLures==TRUE, 100, 0)),
                   mean(ifelse(meta.FullData$FeaturalLures==TRUE, 100, 0)),
                   mean(ifelse(meta.FullData$ComponentLures==TRUE, 100, 0)),
                   mean(ifelse(meta.FullData$UnrelatedLures==TRUE, 100, 0)),
                   mean(meta.FullData$propLures)),
                 c(sd(meta.FullData$OA_Age),
                   NA, NA, NA, NA,
                   NA, NA, NA,
                   NA, NA, NA,
                   NA, NA, NA, NA,
                   NA,
                   NA,
                   sd(meta.FullData[meta.FullData$StudyTimePer.Num!=0, ]$StudyTimePer.Num),
                   NA,
                   NA,
                   sd(meta.FullData$NumTBRItems),
                   NA, NA, 
                   sd(meta.FullData$RetentionInterval.Num),
                   exp(sd(log(meta.FullData$RetentionInterval.Num))),
                   NA,
                   sd(meta.FullData$TestListLength.Num),
                   NA, NA, NA, NA, NA,
                   sd(meta.FullData$propLures)))
colnames(IVTable) = c('Mean', 'SD')
rownames(IVTable) = c('OA Age', 'Words', 'Pictures','Faces','Texts','Positive valence', 'Neutral valence', 'Negative valence', 'Visual', 'Auditory', 'Multimodal', 'Deep', 'Shallow', 'Generation', 'Divided attn', 'Production', 'Self-paced', 'Study time', 'Multiple study', 'Intentionality', 'Num memoranda', 'Intervening cued recall', 'Intervening free recall', 'Retention interval', 'Log retention interval', 'Continuous recog', 'Test list length', 'Conjunction lures', 'Component lures', 'Semantic lures', 'Featural lures', 'Unrelated lures', 'Proportion lures')
IVTable


## FIGURES

# Plot all d' points
pdf('Figure1-OA_Dprime_vs_YA_Dprime.pdf')
plot(meta.FullData$YA_Discrimination, meta.FullData$OA_Discrimination, xlab="Young adult d'", ylab="Older adult d'", xlim=c(0,5), ylim=c(0,5), bty='l')
abline(a=0, b=1)
dslope = summary(model.Dprime)$coeff['YA_Discrimination.c','Estimate']
dxcenter = mean(meta.FullData$YA_Discrimination)
dintercept = summary(model.Dprime)$coeff['(Intercept)','Estimate']
drealintercept = (dslope * (-1 * dxcenter)) + dintercept
dev.off()

# Plot all c points
pdf('Figure2-OA_c_vs_YA_c.pdf')
plot(meta.FullData$YA_Criterion, meta.FullData$OA_Criterion, xlab="Young adult c", ylab="Older adult c", xlim=c(-1.5,1.5), ylim=c(-1.5,1.5), bty='l')
abline(h=0, lty=2)
abline(v=0, lty=2)
abline(a=0, b=1)
dev.off()



## ALTERNATE MODELS WITH POTENTIAL OUTLIERS REMOVED

# Discrimination model
# Trim based on residuals
meta.Reg.D.Trimmed <- meta.FullData[abs(scale(resid(model.Dprime.weighted))) <= 3, ]
1-(nrow(meta.Reg.D.Trimmed)/nrow(meta.FullData))
# and refit model:
model.Dprime.Trimmed <- lmer(AgeDiff_Discrimination ~ 1 + YA_Discrimination.c + OA_Age.c + Faces + Pictures + Text + PositiveValence + NegativeValence + AuditoryOnly + Audiovisual + EncodingType + ShallowStudy + DeepStudy + GenerationStudy + DividedStudy + Production + NumTBRItems.c + MultipleStudy + SelfPaced + IntentionalSelfPaced + StudyTimePer.Num.c + InterveningCuedRecall + InterveningFreeRecall + RetentionInterval + ContinuousRecog + ContinuousRI + ConjunctionLures + SemanticLures + FeaturalLures + ComponentLures + TestListLength.Num.c + propLures.c + (1|PaperID) + (1|StudyID) + (1|SubjGroup), data=meta.Reg.D.Trimmed, weights=1/Total_N)
summaryCI(model.Dprime.Trimmed)
drop1(model.Dprime.Trimmed, test='Chisq')

# Criterion model
# Trim based on residuals
meta.Reg.C.Trimmed <- meta.FullData[abs(scale(resid(model.C.weighted))) <= 3, ]
1-(nrow(meta.Reg.C.Trimmed)/nrow(meta.FullData))
# and refit model:
model.C.Trimmed <- lmer(AgeDiff_Criterion ~ 1 + YA_Criterion.c + OA_Age.c + Faces + Pictures + Text + PositiveValence + NegativeValence + AuditoryOnly + Audiovisual + EncodingType + ShallowStudy + DeepStudy + GenerationStudy + DividedStudy + Production + NumTBRItems.c + MultipleStudy + SelfPaced + IntentionalSelfPaced + StudyTimePer.Num.c + InterveningCuedRecall + InterveningFreeRecall + RetentionInterval + ContinuousRecog + ContinuousRI + ConjunctionLures + SemanticLures + FeaturalLures + ComponentLures + TestListLength.Num.c + propLures.c + (1|PaperID) + (1|StudyID) + (1|SubjGroup), data=meta.Reg.C.Trimmed, weights=1/Total_N)
summaryCI(model.C.Trimmed)
drop1(model.C.Trimmed, test='Chisq')
