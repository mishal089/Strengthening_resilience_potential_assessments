require(here)
library(tidyverse)
library(corrplot)
library(Hmisc)
library(dplyr)
library(vegan)
library(ggfortify)
library(readxl)
library(ggplot2)
library(tibble)
library(magrittr)
library(simpleboot)
library(forcats)
library(kableExtra)
library(scales)

# Import detailed data ----------------------------------------------------

data_detailed<-read.csv(file='Detailed evaluation/review_detailed_eval_data.csv',header=T, stringsAsFactors = F)

exclude<-Cs(TS_2016_Eilat, AC_2019_Hawaii,	MW_2021_Chagos,	KH_2017_GBR,MG_2021_Palau, EHD_2017_PuertoRico, JM_2016_PNG)

data_detailed %<>% 
  filter(!Study.ID.1 %in% exclude)

data_detailed %<>%  mutate(What.type.of.resilience.potential.assessment.is.this. = case_when(
  Study.ID.1 == "AK_2013_Fiji" ~ "expert set of (normalised) resilience indicators",
  TRUE ~ What.type.of.resilience.potential.assessment.is.this.
))

# Import rapid data -------------------------------------------------------
data<-read.csv(file='Rapid evaluation/review_rapid_eval_data.csv',header=T, stringsAsFactors = F)

final_study_list<-data_detailed$Study.ID.1 %>% unique()


#Check if there are any studies missing in data 

final_study_list[!final_study_list %in% data$Study.ID.1]
#BS_2018_Samoa is called BS_2018_AmericanSamoa in rapid data

#change this in data
data$Study.ID.1 <- gsub("BS_2018_AmericanSamoa", "BS_2018_Samoa", data$Study.ID.1)


#filter the data to only include final_study_list

data %<>%
  filter(Study.ID.1 %in% final_study_list)

#Contains analytical framework column

#change the factor values to shorten, creates a new column called Resilience_inferences
data %<>%  mutate(`Analytical_fwork`=`Was.there.an.analytical.framework.to.combine.indicators.`  %>% 
                    replace(Study.ID.1 == "CM_2019_GBR","Model") %>% 
                    factor() %>%
                    fct_recode(`No/Unclear` = "No",
                               `No/Unclear` = "Unclear",
                               Other="Other: ",
                               `Model`="Other: mechanistic model",
                               `Model`="Other: Bayesian hierarchical statistical model",
                               `Other`="Other: geometric mean of component scores",
                               `Other`="Other: mean of the combined distributions (produced from bootstrapping) for each indicator",
                               `Model`="Other: mechanistic and simulation models",
                               `Model`="Other: Mechanistic model",
                               `Other`="Other: PCA",
                               `Other`="Other: TOPSIS method - Euclidean distance from ideal positive and negative position",
                               `Did not combine`="Did not combine indicators"))

factor_order<-c("Linear",  "Model","Other" ,"Unclear method", "Unsure if combined","Did not combine"  ,"No quantitative assessment")

data$Analytical_fwork <- factor(data$Analytical_fwork, levels = factor_order) 


# Merge two dataframes -------------------------------------------------------------------

# Merge the data frames by Study.ID.1
merged_data <- merge(data, data_detailed, by = "Study.ID.1")

# Select only the desired columns
selected_columns <- select(merged_data, Study.ID.1, Analytical_fwork, 
                           How.were.indicators.weighted.in.the.model., 
                           What.was.the.rationale.and.method.used.to.determine.weights., 
                           How.were.aggregated.final.scores.normalised.standardised.,
                           Were.variables.normalised.,
                           What.type.of.resilience.potential.assessment.is.this.)

selected_columns %<>% 
  filter(What.type.of.resilience.potential.assessment.is.this.=="expert set of (normalised) resilience indicators")

#issues
#DO_2011_Madagascar - did not combine, weight =  Equal - exclude from weight analysis
#Equal to Other: n/a (not combined)

#MS_2019_Thailand - Other, weight - Other: N/A (not combined), normalise - Other: not done/NA - check if Other is correct classification for analytical framework
#Variables : None (raw form), response is Other: - should be changed to Not combined in rapid data eval
#used a Used an NMDS to group sites on the basis of the Bray–Curtis similarity of benthic components - it is a way to combine indicators together for a site based on differences with other sites
#since it is not an anlytical approach to create scores, and rather just a way to compare site differences, not considered aggregated
#change from other to Did not combine

selected_columns$Analytical_fwork[which(selected_columns$Study.ID.1 == "MS_2019_Thailand")] <- "Did not combine"

#There are now 50 assessments which we can confirm produced composites 


# Weight ------------------------------------------------------------------
exclude_weight<-selected_columns %>% 
  filter(Analytical_fwork=="Did not combine"|Analytical_fwork=="Unsure if combined") %>% 
  pull(Study.ID.1)

weights<-selected_columns %>% 
        filter(!Study.ID.1 %in% exclude_weight) %>% 
        mutate(`weight`=`How.were.indicators.weighted.in.the.model.`)

weights$weight %>% unique()

weights %<>%
  mutate(weight=weight   %>% 
           fct_recode(
             `Unclear`="Other: unknown",
             `Unclear`= "Other: Unknown ",
             `Unclear`= "Other: Unknown",
             `Unclear`="Other: unclear",
             `Unclear` = "Other: ",
             `Unclear` = "Other: results not presented",
             `Multiple`= "Varied e.g. scenarios"))


ftable(weights$weight) #total - 50
#Equal Unclear Unequal Multiple
#29      10      10        1

ggplot(weights, aes(x= factor(weight))) +
  geom_bar(aes(fill=weight))+
  theme_bw()+
  xlab('How were indicators weighted in the model')+
  ylab('Number of assessments')+
  theme(legend.position = 'none')+
  theme( panel.border = element_blank(),
         axis.text = element_text(size=15),
         axis.line = element_line(colour = "black"),
         text = element_text(size=15))
#normalisation has its problems, but since we don't present results as proportion and only talk about 2 categories which are unaffected by those issues
#the main issue would be with the They weren't column being over inflated with assessments that did not combine results


# weight rationale --------------------------------------------------------

weights %<>%
  mutate(weight_rat=`What.was.the.rationale.and.method.used.to.determine.weights.`   %>% 
           fct_recode(
             `arbitrary`="",
             `arbitrary`= "arbitrary/unknown/undefined",
             `empirical evidence and local relevance`= "Other: local observations",
             `unclear`="Other: based on most unambiguous and most reliably quantitative indicators",
             `empirical evidence`="similar previous study") %>% 
           replace(Study.ID.1=="TP_2018_Indonesia","expert opinion/judgement") %>% 
           replace(Study.ID.1=="FJ_2022_SWIO","expert opinion/judgement") %>% 
           replace(Study.ID.1=="RS_2010_Mozambique","expert opinion/judgement") %>% 
           replace(Study.ID.1=="AM_2019_Wakatobi","expert opinion/judgement") %>% 
           replace(Study.ID.1=="JM_2015_CNMI","expert opinion/judgement") %>% 
           replace(Study.ID.1=="TM_2012_ KarimunjawaIsland","expert opinion/judgement") %>% 
           replace(Study.ID.1=="JM_2010_GBR","empirical evidence and local relevance") %>% 
           replace(Study.ID.1=="CL_2013_Mexico","empirical evidence and local relevance"))

weight_rationale$weight_rat %>% unique()

ftable(weights$weight_rat)


ggplot(weights, aes(x= factor(weight_rat))) +
  geom_bar(aes(fill=weight_rat))+
  theme_bw()+
  coord_flip()+
  xlab('How were indicators weighted')+
  ylab('Number of assessments')+
  theme(legend.position = 'none')+
  theme( panel.border = element_blank(),
         axis.text = element_text(size=15),
         axis.line = element_line(colour = "black"),
         text = element_text(size=15))

# Composite ---------------------------------------------------------------
ftable(selected_columns$Analytical_fwork)
# Linear Model Other Unclear method Unsure if combined Did not combine No quantitative assessment
# 
# 45     0     3              2                  4               9                          0

ggplot(selected_columns, aes(x= factor(Analytical_fwork))) +
  geom_bar(aes(fill=Analytical_fwork))+
  theme_bw()+
  xlab('Analytical framework')+
  ylab('Number of assessments')+
  theme(legend.position = 'none')+
  theme( panel.border = element_blank(),
         axis.text = element_text(size=15),
         axis.line = element_line(colour = "black"),
         text = element_text(size=15),
         axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
