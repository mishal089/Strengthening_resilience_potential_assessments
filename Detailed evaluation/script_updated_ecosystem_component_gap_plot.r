#this produces the res_component_gaps figure but using an updated criteria to identify gaps

#the input is from detail assessment, it is data after going through to lin
load("Detailed evaluation/data_for_update_eco_comp_gaps_plot.RDA")

#using the following criteria to identify gaps: for each ecosystem component, if any resilience component is represented, then that ecosystem component is considered covered

data$major_broad <- "NA"
data$component_gaps2<-NA
  
for (i in 1:nrow(data)) {
  major_components <- c("Coral", "Fish", "Competitors", "Environment", "Habitat")
  res_components <- strsplit(as.character(data[i, "Which.resilience.components.were.reported.quantified."]), ";")[[1]]
  missing_components <- c()
  component_gaps2<- c()
  
  if (length(grep("\\bCoral\\b", res_components)) == 0) { #coral doesn't work if with corralivores - add //b
    
    missing_components <- c(missing_components, "Coral")
  }
  if (length(grep("Fish", res_components)) == 0) {
    missing_components <- c(missing_components, "Fish")
  }
  if (length(grep("Competitors", res_components)) == 0) {
    missing_components <- c(missing_components, "Competitors")
  }
  if (length(grep("Environment", res_components)) == 0) {
    missing_components <- c(missing_components, "Environment")
  }
  if (length(grep("Habitat", res_components)) == 0) {
    missing_components <- c(missing_components, "Habitat")
  }
  if (length(grep("Corallivores", res_components)) == 0 & length(grep("biodiversity", res_components)) == 0) {
    component_gaps2 <- c(component_gaps2, "non-essential") #both non-essential missing
  }
  if (length(grep("Corallivores", res_components)) != 0|length(grep("biodiversity", res_components)) != 0) {
    component_gaps2 <- c(component_gaps2, "effectively covered") #both non-essential missing
  }
  
  
  data[i, "major_broad"] <- paste(missing_components, collapse = ";")
  data[i, "component_gaps2"] <- paste(component_gaps2, collapse = ";")
  
}

# if component_gaps2 == "unclear", then assign "unclear" to new_col.
# if nchar(major_broad) == 0, then assign "Other" to new_col.
# otherwise, assign the number of semicolons in major_broad to new_col.
# We use strsplit and length to count the number of semicolons in major_broad. Finally, we use as.character to convert the resulting number to a character.

data %<>%
  mutate(
    component_gaps2 = if_else(component_gaps == "unclear", "unclear", 
                              if_else(nchar(major_broad) > 0, major_broad, component_gaps2)),
    new_col = case_when(
      component_gaps2 == "unclear" ~ "Other",
    nchar(major_broad) == 0 ~ "Other",
    TRUE ~ as.character(lengths(strsplit(major_broad, ";")))
    ),
    new_col = case_when(
      new_col == 1 ~ "1 essential",
      new_col == 2 ~ "2 essential",
      new_col == 3 ~ "3 essential",
      TRUE ~ new_col
  )
)

observe<-data[,c('Which.resilience.components.were.reported.quantified.','component_gaps','major','major_broad','component_gaps2',"new_col")]

#perfect, now can create the plot using new_col and component_gaps2
df_split2 <- observe %>% 
  unnest(component_gaps2 = strsplit(component_gaps2, ";"))

#rename non-essential to complementary
df_split2$component_gaps2[df_split2$component_gaps2=='non-essential']<-'complementary'

#plot 2

# create a frequency table of component_gaps and major
df_table2 <- df_split2 %>% 
  count(new_col,component_gaps2)

df_component_gaps2 <- observe %>%
  group_by(new_col) %>% 
  count(new_col)

df_table2$total_count<-df_component_gaps2$n[match(df_table2$new_col,df_component_gaps2$new_col)]


#The invert abundance metrics/indicators were not considered as Other-biodiversity
#But they should have been
#need to check to make sure that this doesn't change the missing ecosystem components values (n assessments)

#NTL_2019_Vietnam - mollusca density; crustacea density; echinodermata density; polychaeta species; zooplankton density; phytoplankton density 
#AB_2011_SaudiArabia - mollusc and crustacean abundance
#LR_2014_Micronesia - invert density

#AB_2010_CaymanIsl - lobster 

#In total 4 assessments measured Other biodiversity component
#should add two more assessments to other biodiversity (AB_2011_SaudiArabia and LR_2014_Micronesia)

#currently missing component:
#AB_2011_SaudiArabia - effectively covered
#LR_2014_Micronesia - 3 essential missing

#Does not change anything with regards to missing ecosystem components



factor_order2<-c("3 essential","2 essential","1 essential","Other")

df_table2 %<>%  mutate(new_col= new_col %>% factor(levels = factor_order2))

ggplot(df_table2,aes(x = new_col, y = n, fill = component_gaps2)) +
  geom_rect(data = df_table2, aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = total_count),fill = 'grey', alpha = 0.1, inherit.aes = FALSE) +
  geom_bar(position = "dodge", stat="identity") +
  # geom_errorbar(aes(ymin = total_count, ymax = total_count), width = 0.85, size = 1, color = "black", linetype='dashed') +
  facet_grid(new_col ~ ., scales = "free", space = "free", switch = "y") +
  labs(
       x = "Number of missing ecosystem components",
       y = "Number of assessments",
       fill = "Ecosystem component") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom",
    legend.box = "horizontal",
    panel.grid.major.x = element_blank(),
    axis.text.y = element_blank(),
    # axis.ticks.x = element_blank(),
    # axis.ticks.length = unit(0.2, "cm"),
    # strip.text.x = element_text(hjust=0, size=14))
    panel.border = element_blank(),
    axis.text = element_text(size=15),
    axis.line = element_line(colour = "black"),
    text = element_text(size=15),
    strip.background = element_blank(),
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.major = element_blank(),  # remove major grid lines
    panel.grid.minor = element_blank())+
    guides(color = guide_legend(nrow = 3))
  

ggsave("res_component_gaps_updated.jpg", plot = last_plot(), width = 8.5, height = 7, dpi = 300)

