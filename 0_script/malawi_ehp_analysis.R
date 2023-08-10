#############################################################
## Linear Programming Function to design Malawi's Essential Health Package
## Created by: Sakshi Mohan; 17/01/2022
#############################################################

##############################
# 0 - Load librairies
##############################
# First install all the necessary packages
# Then load all packages
library(readxl) # extract data into excel files
library(lpSolve)
library(tidyverse)
library(fmsb) # for radar chart
library(plyr)
library(dplyr)
library(ggplot2)
library(forcats) # to reorder plots
library(xtable) # for LaTeX tables
library(tidyr)
library(scales) # to formal axis labels
library(viridis) # load viridis colour palette
library(writexl) # new package to write excel files without java dependency
library(testit) # for assertion commands

library(extrafont) # load fonts for graph
font_import()
loadfonts(device = "win")

##############################
# 1 - Set Working Directory
##############################
setwd("C:/Users/sm2511/Dropbox/York/Research Projects/Malawi EHP/Analysis/repo/")

###################################
# 2 - Load and set up data for LPP
###################################

# Load epi/cost/CE dataset 
#****************************************************
df <- read_excel("1_data/malawi_intervention_data.xlsx", sheet = "final_intervention_list",col_names = TRUE,col_types=NULL,na="",skip=1)

df <- df[-c(1),]  # remove first column
df <- df[c("code","category","intervention","ce_dalys", "ce_cost", 
           "pop_size", "pop_pin", "feascov", 
           "conscost", 
           "hr_medoff", "hr_clinoff", "hr_medass",
           "hr_nuroff", "hr_nurtech",
           "hr_pharm", "hr_pharmtech", "hr_pharmass",
           "hr_laboff", "hr_labtech", "hr_labass",
           "donor_funded")]
df <- na.omit(df) 


# Load HR availability dataset
#****************************************************
df_hr <- read_excel("1_data/malawi_hr_data.xlsx", sheet = "hr_constraint",col_names = TRUE,col_types=NULL,na="",skip=1)

cadres <- 4 # set number of cadres being considered

hr.time.constraint <- df_hr$'Total patient-facing time per year (minutes)'[1:cadres]
hr.size <- df_hr$'Total staff'[1:cadres]
hr.size <- as.numeric(gsub(",", "", hr.size))
hr.time.constraint <- as.numeric(gsub(",", "", hr.time.constraint))

###################################
# 3. Define optimization function
###################################
#outputs of this function are - 
find_optimal_package <- function(data.frame, objective_input, cet_input, 
                                 drug_budget_input, drug_budget.scale,  
                                 hr.time.constraint, hr.size, hr.scale, 
                                 use_feasiblecov_constraint, feascov_scale, compcov_scale, 
                                 compulsory_interventions, substitutes, task_shifting_pharm){ # % complements %
  intervention <<- data.frame$intervention
  code <<- data.frame$code # list of intervention codes
  category <<- data.frame$category # program/category of intervention
  dalys <<- as.numeric(as.character(data.frame$ce_dalys)) # Per person DALYs averted based on CE evidence
  drugcost <<- as.numeric(as.character(data.frame$conscost)) #  Per person cost of drugs and commodities
  maxcoverage <<- as.numeric(as.character(data.frame$feascov)) # Maximum possible coverage (demand constraint)
  pop_size <<- as.numeric(as.character(data.frame$pop_size)) # Target population
  pop_pin <<- as.numeric(as.character(data.frame$pop_pin)) # Proportion in need
  cases <<- pop_size * pop_pin # Total number of cases
  fullcost <<- as.numeric(as.character(data.frame$ce_cost)) # Full cost per patient based on CE evidence 
  hrneed <<- data.frame[c("hr_medoff", "hr_clinoff", "hr_medass",
                  "hr_nuroff", "hr_nurtech",
                  "hr_pharm", "hr_pharmtech", "hr_pharmass",
                  "hr_laboff", "hr_labtech", "hr_labass")] # Number of minutes of health worker time requires per intervention per person
  hrneed <- as.data.frame(apply(hrneed,2,as.numeric))
  
  n <- length(dalys) # number of interventions included in the analysis
  
  ###################################
  # 3.1 Set up LPP
  ###################################
  
  # Objective - maximize DALYs or Net Health per person X Total number of cases X Coverage
  #****************************************************
  # Define net health
  cet <- cet_input
  nethealth <<- dalys - fullcost/cet
  
  # Define objective
  if (objective_input == 'nethealth'){
    objective <<- nethealth * cases
  } else if (objective_input == 'dalys'){
    objective <<- dalys * cases
  } else{
    print('ERROR: objective_input can take values dalys or nethealth')	
  }
  
  # Constraints - 1. Drug Budget, 2. HR Requirements
  #****************************************************
  # 1. Drug Budget
  #----------------
  cons_drug <<- drugcost * cases # Drug budget *cons*traint Cost of drugs for the number of cases covered
  cons_drug.limit <<- drug_budget_input * drug_budget.scale
  cons_drug.limit_base <<- drug_budget_input # unscaled drug budget saved for post optimisation analyses
  
  # 2. HR Constraints
  #---------------------
  hr_minutes_need <- hrneed * cases[row(hrneed)] # HR minutes required to deliver intervention to all cases in need
  
  # Update HR constraints so that nurses, pharmacists, medical officers, etc. represent joint constraints
  medstaff.need <- hr_minutes_need[c("hr_medoff")] + hr_minutes_need[c("hr_clinoff")] + hr_minutes_need[c("hr_medass")] # Medical officer + Clinical officer + Medical Assistant
  nursingstaff.need <- hr_minutes_need[c("hr_nuroff")] + hr_minutes_need[c("hr_nurtech")] # Nurse officer + Nurse midwife
  pharmstaff.need <- hr_minutes_need[c("hr_pharm")] + hr_minutes_need[c("hr_pharmtech")] + hr_minutes_need[c("hr_pharmass")] # Pharmacist + Pharmacist Technician + Pharmacist Assistant
  labstaff.need <- hr_minutes_need[c("hr_laboff")] + hr_minutes_need[c("hr_labtech")] + hr_minutes_need[c("hr_labass")] # Lab officer + Lab technician + Lab assistant

  # Clean total minutes available per cadre  
  cons_hr.limit <- hr.time.constraint
  medstaffmins.limit <<- cons_hr.limit[1] 
  nursingstaffmins.limit <<- cons_hr.limit[2]
  pharmstaffmins.limit <<- cons_hr.limit[3] 
  labstaffmins.limit <<- cons_hr.limit[4] 
  
  reps <<- 2 # set the number of times that the matrix of interventions is duplicated
  
  # Define a function which duplicates a matrix horizontally
  duplicate_matrix_horizontally <- function(reps, matrix){
    matrix <- do.call(rbind, replicate(reps, matrix, simplify=FALSE))
  }
  
  if (task_shifting_pharm == 0){
    print("")
  } else if (task_shifting_pharm == 1){
    medstaff.need <- duplicate_matrix_horizontally(reps,as.matrix(medstaff.need))
    nursingstaff.need <- rbind(as.matrix(nursingstaff.need), as.matrix(nursingstaff.need + pharmstaff.need))
    pharmstaff.need <- rbind(as.matrix(pharmstaff.need), as.matrix(rep(0,n)))
    labstaff.need <- duplicate_matrix_horizontally(reps,as.matrix(labstaff.need))
  } else{
    print('ERROR: tash_shifting_pharm can take values 0 or 1')
  }
  
  # Clean total workforce size per cadre   
  hr_size.limit <- hr.size
  medstaff.limit <- hr_size.limit[1]
  nursingstaff.limit <- hr_size.limit[2]
  pharmstaff.limit <- hr_size.limit[3] 
  labstaff.limit <- hr_size.limit[4]
  
  medstaff.scale <- hr.scale[1]
  nursestaff.scale <- hr.scale[2]
  pharmstaff.scale <- hr.scale[3]
  labstaff.scale <- hr.scale[4]
  
  # Each list here represents the number of staff (of each cadre) needed to deliver each intervention to all cases in need. 
  # Eg. for each cesarean section, 45 minutes of medical staff's time is needed (or 104,200 minutes for 2316 cases). On average 39,900 minutes are available per medical staff each year (257.3 million minutes in total divided by 6,400 medical staff). This means that for 2136 cases, 2.16 medical staff are needed (2316*45/(257.3m/6400))
  
  cons_hr <<- cbind(medstaff.need/(medstaffmins.limit/medstaff.limit), nursingstaff.need/(nursingstaffmins.limit/nursingstaff.limit), pharmstaff.need/(pharmstaffmins.limit/pharmstaff.limit), labstaff.need/(labstaffmins.limit/labstaff.limit))
  cons_hr.saved <<- cons_hr
  
  cons_hr.limit_base <<- cbind(medstaff.limit, nursingstaff.limit, pharmstaff.limit, labstaff.limit)
  cons_hr.limit <- cbind(medstaff.limit * medstaff.scale, nursingstaff.limit * nursestaff.scale, pharmstaff.limit * pharmstaff.scale, labstaff.limit * labstaff.scale)
  
  colnames(cons_hr.limit) <- colnames(cons_hr)
  cons_hr.limit.saved <<- cons_hr.limit
  
  # Combine the constraints into one matrix
  #****************************************************
  # 1. HR
  #--------------------------------------
  cons_hr <<- as.matrix(cons_hr)
  cons_hr.limit <<- as.matrix(cons_hr.limit)
  
  # 2. Drug
  #--------------------------------------
  cons_drug <<-as.matrix(cons_drug)
  cons_drug.limit <<- as.matrix(cons_drug.limit)
  
  # 3. Max coverage
  #--------------------------------------
  cons.feascov <<- diag(x = cases, n, n)
  if (use_feasiblecov_constraint == 1){
    cons.feascov.limit <<- as.matrix(maxcoverage * feascov_scale * cases) # Maximum feasible coverage X scale factor applied to feasible coverage X Total number of cases in need
  } else if (use_feasiblecov_constraint == 0){
    cons.feascov.limit <<- as.matrix(cases) # Total number of cases in need
  } else{
    print('ERROR: use_feasiblecov_constraint can take values 0 or 1')
  }  
  
  nonneg.lim <<- as.matrix(rep(0,n)) # Optimal coverage output cannot be negative
  
  # 4. Compulsory interventions
  #--------------------------------------
  # These are interventions which are forced into the package for reasons outside constrained optimisation to maximise health benefit
  if (length(compulsory_interventions) > 0){
    comp.count <- length(compulsory_interventions)
    cons_compulsory <<- matrix(0L, length(compulsory_interventions), ncol = n)
    cons_compulsory.limit <<- matrix(0L, length(compulsory_interventions), ncol = 1)
    for (i in 1:length(compulsory_interventions)){
      a <- which(data.frame$code == compulsory_interventions[i])
      b <- data.frame$intervention[a]
      #print(paste("Compulsory intervention: ",b, "; Code: ", compulsory_interventions[i], "; Number ",a ))  # print suppressed
      cons_compulsory[i,a] <<- cases[a]
      cons_compulsory.limit[i] <<- cases[a] * maxcoverage[a] * feascov_scale * compcov_scale # the RHS is the maximum feasible number of cases X scale factor applied to feasible coverage X further scale factor applied to compulsory interventions
    }
    dim(cons_compulsory)
  } else if(length(compulsory_interventions) == 0){
    comp.count<- 1
    cons_compulsory <<- matrix(0L, 1, ncol = n)
    cons_compulsory.limit <<- matrix(0L, 1, ncol = 1)
  }  
  cons_compulsory <<- t(cons_compulsory) 
  
  ###### % Complementary interventions code left out for now %

  # 5. Substitute interventions
  #--------------------------------------
  substitutes = substitutes
  subs.count <- length(substitutes)
  cons_substitutes.limit <<- matrix(0L, length(substitutes), ncol = 1)
  cons_substitutes <<- matrix(0L, length(substitutes), ncol = n) 
  
  # First find the maximum number of feasible cases among the substitute interventions
  subsgrp_casesmax = matrix(0L, length(substitutes), ncol = 1)
  for (i in 1:subs.count){
    for (j in substitutes[i]){
      subsgrp_cases <- 0
      for (k in j){
        a <- which(data.frame$code == k)
        if (use_feasiblecov_constraint == 1){
          cases_max <- cases[a] * maxcoverage[a] * feascov_scale
        } else if (use_feasiblecov_constraint == 0){
          cases_max <- cases[a]
        }
        subsgrp_cases = cbind(subsgrp_cases,cases_max) 
      }
      subsgrp_casesmax[i] = max(subsgrp_cases)
      #print(paste("Group", i, "Cases max", subsgrp_casesmax[i]))  # print suppressed
    }
  }
  
  # Next define the constraint such that the sum of the cases for each substitute interventions is less than or equal to the maxumum feasible cases derived above
  # print("Substitutes")
  for (i in 1:subs.count){
    # print(paste("Substitute group", i))
    # print("------------------------------------------------------------")
    for (j in substitutes[i]){
      for (k in j){
        a <- which(data.frame$code == k)
        b <- data.frame$intervention[a]
             #print(paste("Intervention: ",b, "; Code: ", k, "; Maximum cases for intervention:", cons.feascov.limit[a],"; Number: ",a))  # print suppressed
        cons_substitutes[i,a] <<- cases[a] 
        cons_substitutes.limit[i] <<- subsgrp_casesmax[i]  # Maximum feasible number of cases among substituable interventions
      }
    }
     # print(paste("Maximum combined cases for group ",i, "= ", subsgrp_casesmax[i])) # print suppressed
  }  
  cons_substitutes <<- t(cons_substitutes)
  
  # Changes to constraints if task-shifting of pharmacist responsibility is allowed  
  #--------------------------------------------------------------------------------
  # Update the constraint matrices if task shifting is allowed
  if (task_shifting_pharm == 0){
    print("No task shifting of pharmaceutical tasks")
  } else if (task_shifting_pharm == 1){
    #1. Objective
    objective <<- duplicate_matrix_horizontally(reps, as.matrix(objective))
    #2. Drug budget constraint (cons_drug.limit does not need to be changed)
    cons_drug <<- duplicate_matrix_horizontally(reps, as.matrix(cons_drug))
    #3. Feasible coverage constraint
    cons.feascov <<- duplicate_matrix_horizontally(reps,as.matrix(cons.feascov))
    #4. Compulsory interventions
    cons_compulsory <<- duplicate_matrix_horizontally(reps,as.matrix(cons_compulsory))
    #6. Substitutes
    cons_substitutes <<- duplicate_matrix_horizontally(reps,as.matrix(cons_substitutes))  
  } else{
    print('ERROR: task_shifting_pharm can take values 0 or 1')
  }
  
  # Combine constraints 1-5
  cons.mat <- rbind(t(cons_drug), t(cons_hr), t(cons.feascov), t(cons.feascov), t(cons_compulsory), t(cons_substitutes)) # % cons_complements taken out for now
  cons.mat.limit <- rbind(cons_drug.limit, t(cons_hr.limit), cons.feascov.limit, nonneg.lim, cons_compulsory.limit, cons_substitutes.limit) # % cons_complements.limit taken out for now
  
  # Direction of relationship
  cons.dir <- rep("<=",1+cadres+n)
  cons.dir <- c(cons.dir,rep(">=",n), rep(">=",comp.count))
  cons.dir <- c(cons.dir,rep("<=",length(substitutes)))
  # % cons.dir <- c(cons.dir,rep("<=",length(complements))) % # taken out for now
  assert(length(cons.dir) == dim(cons.mat.limit)[1]) # Assert that the length of the directions list is the same as that of the constraints matrix
  
  ###################################
  # 3.2 - Run LPP
  ###################################
  solution.class <<- lp("max", objective, cons.mat, cons.dir, cons.mat.limit, compute.sens = TRUE)
  
  ###################################
  # 3.3 - Outputs	
  ###################################
  # Prepare solution for export into a .csv file
  #---------------------------------------------
  solution <<- as.data.frame(solution.class$solution)
  solution_hr <<- as.data.frame(solution.class$solution) # use this uncollapsed version of the dataframe for HR use calculations below
  # Collapse solution by intervention
  if (task_shifting_pharm == 1){
    for (i in 1:length(dalys)){
      for (j in 1:(reps-1)){
        solution[i,1] <<- solution[i,1] + solution[i+length(dalys)*j,1]
      }
    }
    solution <<- as.data.frame(solution[1:length(dalys),1])
  }
  
  # Number of interventions with a positive net health impact
  pos_nethealth.count <<- sum(nethealth > 0) 
  
  # Number of interventions in the optimal package
  intervention.count <<- sum(solution != 0)
  
  # DALY burden averted as a % of avertible DALY burden
  solution_dalysaverted <<- solution * cases * dalys # Dalys averted per intervention
  dalysavertible = cases * dalys # Total DALYs that can be averted at maximum coverage
  dalys_averted <<- round(sum(unlist(lapply(solution_dalysaverted, sum))),2)
  dalys_averted.prop <<- sum(unlist(lapply(solution_dalysaverted, sum)))/sum(unlist(lapply(dalysavertible, sum)))
  
  # Drugs and Commodities cost (% of budget available)
  solution_drugexp <<- solution*cons_drug[1:length(dalys),] # Total drug budget required per intervention for the  the optimal solution
  total_drug_exp <<- round(sum(unlist(lapply(solution_drugexp, sum))),2) # Total drug budget required for the  the optimal solution
  drug_exp.prop <<- total_drug_exp/cons_drug.limit_base # Proportion of drug budget used by the optimal solution
  
  # Total HR use (% of capacity)
  hr_cadres <- c("Clinical", "Nursing", "Pharmaceutical", "Lab")
  solution_hruse <<- unlist(solution_hr) * cons_hr  # Number of minutes per health worker cadre and intervention utlitised by the optimal solution
  if (task_shifting_pharm == 1){
    for (i in 1:length(dalys)){
      for (j in 1:(reps-1)){
        solution_hruse[i,] <<- solution_hruse[i,] + solution_hruse[i+length(dalys)*j,]
      }
    }
    solution_hruse <<- solution_hruse[1:length(dalys),]
  }
  total_hruse <<- colSums(solution_hruse, na.rm = FALSE, dims = 1) # Number of minutes per health worker cadre utlitised by the optimal solution
  hruse.prop <<- round(total_hruse/cons_hr.limit_base, 2)  # Proportion of HR time available used by the optimal solution
  colnames(hruse.prop) <<- hr_cadres
  
  # Cost-effectiveness Threshold
  icer <- fullcost/dalys
  temp <- cbind.data.frame(icer, solution, data.frame$intervention)
  temp['solution.class$solution'] =  as.numeric(temp[[2]])
  temp['icer'] =  as.numeric(temp[[1]])
  cet_soln <<- round(max(temp['icer'][temp['solution.class$solution'] > 0]),2) # previoiusly temp$icer[temp$solution > 0]
  a <- which(icer == max(temp['icer'][temp['solution.class$solution'] > 0])) # to check which included intervention has the highest ICER
  least.ce.intervention <- data.frame$intervention[a]
  
  outputs <- list("Total number of interventions in consideration" = length(dalys), 
                  "Number of interventions with positive net health impact" = pos_nethealth.count, 
                  "Number of interventions in the optimal package" = intervention.count,
                  "Net DALYs averted" = solution.class$objval,
                  "Total DALYs averted" = sum(unlist(lapply(solution_dalysaverted, sum))), 
                  "Proportion of DALY burden averted" = dalys_averted.prop , 
                  "Proportion of drug budget used" = drug_exp.prop, 
                  "Proportion of HR capacity used by cadre" = hruse.prop,
                  "CET based on solution" =  cet_soln
  )
  return(outputs)
}

#############################################################
# Function to generate resource use stacked bar charts
#############################################################
# Note that in order to run this function, find_optimal_package needs to be run first
gen_resourceuse_graphs <- function(plot_title, plot_subtitle, file_name){
  #pal <- viridisLite::viridis(10) # Create a viridis palette for the graph
  pal <- rainbow(12)
  
  ## Generate matrix representing HR and Drug budget use by the HBP solution run above
  #***********************************************************************************
  # HR Resource Use
  data_hr <- sweep(solution_hruse, 2, cons_hr.limit_base, FUN = '/')
  data_hr <- data_hr[,1:3]
  hr_cadres <- c("Clinical \nstaff", "Nursing \nstaff", "Pharmaceutical \nstaff") # , "Laboratory \nstaff"
  
  # Drug budget Use
  data_drug <- as.matrix(solution_drugexp)/cons_drug.limit_base
  
  assert(length(data_drug) == dim(data_hr)[1]) # Assert that the length of the directions list is the same as that of the constraints matrix
  
  # Combine all resource use matrices into one matrix
  data <- cbind(data_hr,data_drug)
  data <- as.matrix(data)
  data <- cbind(category,data)
  
  ## Convert to long form in order to apply ggplot 
  #***********************************************************************************
  data <- as.data.frame(data)
  colnames(data) <- c('category', hr_cadres, 'Drug \nbudget')
  data_long <<- gather(data, resource, use, 2:'Drug \nbudget', factor_key=TRUE)
  data_long$use <<- as.numeric(data_long$use) # convert use data to numeric
  
  ## Generate graph
  #***********************************************************************************
  p <- ggplot(data = data_long, aes(x = resource, y = use)) +
    geom_col(aes(fill = category), width = 0.7)
  #+geom_text(aes(y = lab_ypos, label = intcode, group =intcode), color = "white") # add data labels
  p <- p + guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
    scale_y_continuous(labels = percent)+ 
    geom_text(aes(label = stat(sprintf("%1.1f%%", round(100*y, digits = 2))), group = resource), stat = 'summary', fun = sum, vjust = -1, size=4)+
    theme_classic()# show total labels on the top
  titleformats <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = 17, hjust=0.5),  
                        legend.text = element_text(face = "italic", colour="black",family = "Helvetica"),
                        legend.title = element_blank(), # remove legend title
                        axis.title = element_text(family = "Helvetica", size = (15), colour = "black"),
                        axis.text.x = element_text(face="bold", color="black", size=10, angle=0),
                        axis.text.y = element_text(face="bold", color="black", size=10, angle=0),
                        legend.position="bottom",
                        plot.subtitle=element_text(size=12, hjust=0.5, face="italic", color="black")) 
  
  print(p + titleformats + labs( title= plot_title, subtitle = plot_subtitle, 
                                 x="Resource", y = "Percentage of resource required") + scale_fill_manual(values = pal))+
    guides(fill=guide_legend(nrow=4, byrow=TRUE))
  
  
  # Save graph with the assigned title
  ggsave(file_name, width = 20, height = 20, units = "cm")
}

##########################################################
# 2 - Set up common inputs for scenarios
##########################################################

## Pre-code complements and substitutes for the optimizations that follow 
##########################################################################
#  BASE FUNCTION INPUTS
data.frame <- df
cet.ochalek <- 65.8 # CET = 2016$ 61 (2020$ 65.758) (Ochalek et al, 2016)
cet.lomas <- 164.7 # This value is in 2020 USD (2017 USD 154)
base.cet <- cet.ochalek
base.drugbudget <- 203136642 + 22466304 # Donor budget + government budget (NLGFC Drug budget = MWK 12,000,000,000 =  14696876.40 USD)
cadres <- 4
base.hr <- rep(1,cadres)
no.hr.limit <- rep(9999999999,cadres) # when we assume no limit on Human resources
no.nurse.limit <- c(1,9999999999,rep(1,cadres-2)) # when we assume no limit on nurses
no.pharm.limit <- c(1,1,9999999999,rep(1,cadres-3))
no.drugbudget.limit <- 9999999999
no.cet <- 9999999999
subs_list <- list(subs1 = c("065", "066"), # Second-line ART without intensive monitoring; Second-line ART with intensive monitoring
                    subs2 = c("069","070"), # Viral Load + CD4 count (Clinical monitoring and quarterly tests); CD4 count (Clinical monitoring and quarterly tests)
                    subs3 = c("210", "211"), # Treatment of injuries (Fracture reduction); Treatment of injuries (Fracture fixation)
                    subs4 = c("227", "228"), # Colorectoral cancer (screening + treatment); Colorectoral cancer (treatment)
                    subs5 = c("232", "204"), # Prevention of cardiovascular disease; Prevention and treatment of cardiovascular disease
                    subs6 = c("236", "237"), # Vitamin A supplementation in infants and children 6-59 months; Vitamin A supplementation in infants and children 6-59 months + Deworming
                    subs7 = c("347", "241"), # Management of moderate acute malnutrition (children) with ready-to-use supplementary foods (RUSF); Management of moderate acute malnutrition (children)
                    subs8 = c("348", "351"), # Sugar fortification with vitamin-A; Fortification of sugar, oil, maize meal, & wheat flour with iron, vitamin A, and zinc
                    subs9 = c("349", "351"), # Vegetable oil fortification with vitamin A; Fortification of sugar, oil, maize meal, & wheat flour with iron, vitamin A, and zinc
                    subs10 = c("350", "351"), # Maize fortification with vitamin A, iron and zinc; Fortification of sugar, oil, maize meal, & wheat flour with iron, vitamin A, and zinc
                    subs11 = c("290", "291"), # Asthma: Low dose inhaled beclometasone + SABA; Asthma: Low dose inhaled beclometasone
                    subs12 = c("298", "299"), # (Full) Xpert for all patients with presumptive tuberculosis; Targeted Xpert for patients with presumptive tuberculosis (Smear negative, HIV positive, retreatment, contacts of MDR-TB cases)
                    subs13 = c("310", "311") # First line treatment of smear positive cases (95% coverage); Full DOTS (smear-positive, smear negative and Extrapulmonary cases)
)

##########################################################
# 3 Run scenarios and generate resource use graphs
##########################################################
# 1.	No constraints
#---------------------------------------------
find_optimal_package(data.frame = data.frame, objective_input = "nethealth", cet_input = no.cet, 
                     drug_budget_input = base.drugbudget, drug_budget.scale = no.drugbudget.limit,  
                     hr.time.constraint = hr.time.constraint, hr.size = hr.size, hr.scale = no.hr.limit, 
                     use_feasiblecov_constraint = 0, feascov_scale = 1, compcov_scale = 1, 
                     compulsory_interventions = NULL, substitutes = subs_list, task_shifting_pharm = 1)
gen_resourceuse_graphs(plot_title = "Scenario 1", plot_subtitle = "No constraints" , file_name = "2_outputs/figures/resourceuse_scen1.pdf")
scen1 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, cet_soln, drug_exp.prop, hruse.prop)
scen1_coverage = solution

# 2.	CET = 2016$ 61 (2020$ 65.8) (Ochalek et al, 2016)
#------------------------------------------------------------
find_optimal_package(data.frame = data.frame, objective_input = "nethealth", cet_input = cet.ochalek, 
                     drug_budget_input = base.drugbudget, drug_budget.scale = no.drugbudget.limit,  
                     hr.time.constraint = hr.time.constraint, hr.size = hr.size, hr.scale = no.hr.limit, 
                     use_feasiblecov_constraint = 0, feascov_scale = 1, compcov_scale = 1, 
                     compulsory_interventions = NULL, substitutes = subs_list, task_shifting_pharm = 1)
gen_resourceuse_graphs(plot_title = "Scenario 2", plot_subtitle = "CET = $66" , file_name = "2_outputs/figures/resourceuse_scen2.pdf")
scen2 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, cet_soln, drug_exp.prop, hruse.prop)
scen2_coverage = solution


# 3.	CET* = 2017$ 156 (2020$ 164.7) (Lomas et al, 2021)
#------------------------------------------------------------
find_optimal_package(data.frame = data.frame, objective_input = "nethealth", cet_input = cet.lomas, 
                     drug_budget_input = base.drugbudget, drug_budget.scale = no.drugbudget.limit,  
                     hr.time.constraint = hr.time.constraint, hr.size = hr.size, hr.scale = no.hr.limit, 
                     use_feasiblecov_constraint = 0, feascov_scale = 1, compcov_scale = 1, 
                     compulsory_interventions = NULL, substitutes = subs_list, task_shifting_pharm = 1)
gen_resourceuse_graphs(plot_title = "Scenario 3" , plot_subtitle = "CET = $165" , file_name = "2_outputs/figures/resourceuse_scen3.pdf")
scen3 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, cet_soln, drug_exp.prop, hruse.prop)
scen3_coverage = solution

# 4.	CET* + Demand constraint
#------------------------------------------------------------
find_optimal_package(data.frame = data.frame, objective_input = "nethealth", cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget.scale = no.drugbudget.limit,  
                     hr.time.constraint = hr.time.constraint, hr.size = hr.size, hr.scale = no.hr.limit, 
                     use_feasiblecov_constraint = 1, feascov_scale = 1, compcov_scale = 1, 
                     compulsory_interventions = NULL, substitutes = subs_list, task_shifting_pharm = 1)
gen_resourceuse_graphs(plot_title = "Scenario 4" , plot_subtitle = "CET = $66 + Demand constraint", file_name = "2_outputs/figures/resourceuse_scen4.pdf")
scen4 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, cet_soln, drug_exp.prop, hruse.prop)
scen4_coverage = solution

# 5.	CET* +  Demand constraint + Drug budget constraint [This is the scenario which was chosen by MOH Malawi to base the HBP on]
#---------------------------------------------------------------------------------------------------------------------------------
find_optimal_package(data.frame = data.frame, objective_input = "nethealth", cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget.scale = 1,  
                     hr.time.constraint = hr.time.constraint, hr.size = hr.size, hr.scale = no.hr.limit, 
                     use_feasiblecov_constraint = 1, feascov_scale = 1, compcov_scale = 1, 
                     compulsory_interventions = NULL, substitutes = subs_list, task_shifting_pharm = 1)
gen_resourceuse_graphs(plot_title = "Scenario 5" , plot_subtitle = "CET = $66 + Demand constraint + Drug Budget", file_name = "2_outputs/figures/resourceuse_scen5.pdf")
scen5 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, cet_soln, drug_exp.prop, hruse.prop)
scen5_coverage = solution

# 6.	CET* + Demand constraint + Drug budget constraint + Current HR constraint 
#---------------------------------------------------------------------------------------------------------------------------------
find_optimal_package(data.frame = data.frame, objective_input = "nethealth", cet_input = base.cet, 
                     drug_budget_input = base.drugbudget, drug_budget.scale = 1,  
                     hr.time.constraint = hr.time.constraint, hr.size = hr.size, hr.scale = base.hr, 
                     use_feasiblecov_constraint = 1, feascov_scale = 1, compcov_scale = 1, 
                     compulsory_interventions = NULL, substitutes = subs_list, task_shifting_pharm = 1)
gen_resourceuse_graphs(plot_title = "Scenario 6" , plot_subtitle = "CET = $66 + Demand constraint + Drug Budget \n+ HR constraint ", file_name = "2_outputs/figures/resourceuse_scen6.pdf")
scen6 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, cet_soln, drug_exp.prop, hruse.prop)
scen6_coverage = solution


# 8. TLM LCOA Scenario - CET_Lomas + Demand constraint + Drug budget constraint + Current HR constraint 
#---------------------------------------------------------------------------------------------------------------------------------
# [This has been called scenario 8 because scenario 7 is the scenario with donor constraints which has been excluded from this branch]
find_optimal_package(data.frame = data.frame, objective_input = "nethealth", cet_input = cet.lomas, 
                     drug_budget_input = base.drugbudget, drug_budget.scale = 1,  
                     hr.time.constraint = hr.time.constraint, hr.size = hr.size, hr.scale = base.hr, 
                     use_feasiblecov_constraint = 1, feascov_scale = 1, compcov_scale = 1, 
                     compulsory_interventions = NULL, substitutes = subs_list, task_shifting_pharm = 1)
gen_resourceuse_graphs(plot_title = "Scenario 8" , plot_subtitle = "CET = $165 + Demand constraint + Drug Budget \n+ HR constraint ", file_name = "2_outputs/figures/resourceuse_scen8.pdf")
scen8 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, cet_soln, drug_exp.prop, hruse.prop)
scen8_coverage = solution

# 9. TLM LCOA Scenario - CET_Lomas + Demand constraint + Drug budget constraint 
#---------------------------------------------------------------------------------------------------------------------------------
find_optimal_package(data.frame = data.frame, objective_input = "nethealth", cet_input = cet.lomas, 
                     drug_budget_input = base.drugbudget, drug_budget.scale = 1,  
                     hr.time.constraint = hr.time.constraint, hr.size = hr.size, hr.scale = no.hr.limit, 
                     use_feasiblecov_constraint = 1, feascov_scale = 1, compcov_scale = 1, 
                     compulsory_interventions = NULL, substitutes = subs_list, task_shifting_pharm = 1)
gen_resourceuse_graphs(plot_title = "Scenario 9" , plot_subtitle = "CET = $165 + Demand constraint + Drug Budget ", file_name = "2_outputs/figures/resourceuse_scen9.pdf")
scen9 = cbind.data.frame(pos_nethealth.count, intervention.count, dalys_averted, cet_soln, drug_exp.prop, hruse.prop)
scen9_coverage = solution

#############################################################
# 4. Extract a .xlsx to compare EHPs under different scenarios
#############################################################

scenarios = c("No constraints",
              "CET ($65.8)",
              "CET ($164.7)",
              "CET ($65.8) + Demand constraint",
              "CET ($65.8) + Demand constraint + Drug budget",
              "CET ($65.8) + Demand constraint + Drug budget + HR constraint",
              "CET ($164.7) + Demand constraint + Drug budget + HR constraint",
              "CET ($164.7) + Demand constraint + Drug budget")

summary = rbind(scen1, scen2, scen3, scen4, scen5, scen6, scen8, scen9)
summary = cbind(scenarios, summary)
colnames(summary) = c("Constraints applied", "Number of interventions with positive NHB", "Number of interventions in the optimal package", 
                      "Total DALYs averted", "Highest ICER in the HBP", "% of drug budget required",
                      "% of Clinical staff capacity required", "% of Nursing staff capacity required",
                      "% of Pharmaceutical staff capacity required", "% of Lab staff capacity required")

coverage_byscenario = cbind(data.frame$category, data.frame$code, data.frame$intervention, scen1_coverage, scen2_coverage, scen3_coverage, scen4_coverage, scen5_coverage, scen6_coverage, scen8_coverage, scen9_coverage)
colnames(coverage_byscenario) = c("Program", "Intervention code", "Intervention", scenarios)

sheets <- list("output_and_resourceuse" = summary, "optimal_coverage" = coverage_byscenario) 
write_xlsx(sheets,"2_outputs/tables/scenarios_results.xlsx")

#################################################################
# 5.  Marginal value of HSS - assuming task shifting
#----------------------------------------------------------------
#################################################################
capture.output(
  find_optimal_package(data.frame = data.frame, objective_input = "nethealth", cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget.scale = 1,  
                       hr.time.constraint = hr.time.constraint, hr.size = hr.size, hr.scale = base.hr, 
                       use_feasiblecov_constraint = 1, feascov_scale = 1, compcov_scale = 1, 
                       compulsory_interventions = NULL, substitutes = subs_list, task_shifting_pharm = 1)
  
)
nhb_base <- solution.class$objval

# i. Shadow price of medical staff
prop_1med = 1 / cons_hr.limit.saved[,1] 
capture.output(
  find_optimal_package(data.frame = data.frame, objective_input = "nethealth", cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget.scale = 1,  
                       hr.time.constraint = hr.time.constraint, hr.size = hr.size, hr.scale = c(1+prop_1med,1,1,1), 
                       use_feasiblecov_constraint = 1, feascov_scale = 1, compcov_scale = 1, 
                       compulsory_interventions = NULL, substitutes = subs_list, task_shifting_pharm = 1)
  
)
nhb_1med <- solution.class$objval
annual_salary_med <- 3684.67
value_1med <- (nhb_1med - nhb_base)/annual_salary_med

# ii. Shadow price of nursing staff
prop_1nurse = 1 / cons_hr.limit.saved[,2] 
capture.output(
  find_optimal_package(data.frame = data.frame, objective_input = "nethealth", cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget.scale = 1,  
                       hr.time.constraint = hr.time.constraint, hr.size = hr.size, hr.scale = c(1,1 + prop_1nurse,1,1), 
                       use_feasiblecov_constraint = 1, feascov_scale = 1, compcov_scale = 1, 
                       compulsory_interventions = NULL, substitutes = subs_list, task_shifting_pharm = 1)
  
)
nhb_1nurse <- solution.class$objval
annual_salary_nurse <- 3214.77
value_1nurse <- (nhb_1nurse - nhb_base)/annual_salary_nurse

# iii. Shadow price of pharma staff
prop_1pharm = 1 / cons_hr.limit.saved[,3] 
capture.output(
  find_optimal_package(data.frame = data.frame, objective_input = "nethealth", cet_input = base.cet, 
                       drug_budget_input = base.drugbudget, drug_budget.scale = 1,  
                       hr.time.constraint = hr.time.constraint, hr.size = hr.size, hr.scale = c(1,1,1+prop_1pharm,1), 
                       use_feasiblecov_constraint = 1, feascov_scale = 1, compcov_scale = 1, 
                       compulsory_interventions = NULL, substitutes = subs_list, task_shifting_pharm = 1)
  
)
nhb_1pharm <- solution.class$objval
annual_salary_pharm <- 2821.94
value_1pharm <- (nhb_1pharm - nhb_base)/annual_salary_pharm

# iii. Shadow price of drug budget
prop_1pharm = 1 / cons_hr.limit.saved[,3] 
capture.output(
  find_optimal_package(data.frame = data.frame, objective_input = "nethealth", cet_input = base.cet, 
                       drug_budget_input = base.drugbudget+1, drug_budget.scale = 1,  
                       hr.time.constraint = hr.time.constraint, hr.size = hr.size, hr.scale = c(1,1,1,1), 
                       use_feasiblecov_constraint = 1, feascov_scale = 1, compcov_scale = 1, 
                       compulsory_interventions = NULL, substitutes = subs_list, task_shifting_pharm = 1)
  
)
nhb_1drug <- solution.class$objval
value_1drug <- (nhb_1drug - nhb_base)

print("MARGINAL PRODUCTIVITY IN DALYS OF 1000 DOLLARS SPENT ON ...")
print("#####################################")
shadowprice_table = rbind(value_1drug, value_1med, value_1nurse, value_1pharm)
shadowprice_table = round(shadowprice_table*1000,2)

hs_components = c('Drug budget', 'Doctor/Medical officer time', 'Nurse time', 'Pharmacist time')

# Extract marginal value figure
pdf("2_outputs/figures/marginalvalue_hss.pdf")
par(mar=c(4,11,4,4))
y <- barplot(height = rev(t(shadowprice_table)), names = rev(hs_components), width = 1 ,
             space=NULL, las = 1, horiz=T,
             xlim=c(0,800), ylab = "", xlab = "Net DALYs averted",
             main="Marginal value of $1000 spent on Health systems components")
#text(y, 0, round(rev(t(shadowprice_table)),2),cex=1, pos=1)
x <-as.matrix(rev(t(shadowprice_table)))
text(x+100,y,labels=as.character(round(x,3)))
dev.off()
