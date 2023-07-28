# Linear constrained optimization for HBP* design

*[This branch represents the version of the code used for the 2023 Value in Health submission - "Revision of Malawi’s Health Benefits Package for primary and secondary care: a critical analysis of policy formulation". See master branch for the latest version.]*


This repository contains the script which uses intervention-level data on cost-effectiveness, size of eligible population,  consuambles cost
to generate an optimal health benefits package for a country, which maximises the population health impact given the chosen constraints. 


The model is replicated from the analysis performed for a previous study - Mohan S, Walker S, Sengooba F, et al. Supporting the revision of the health benefits package in Uganda: a constrained optimisation approach. Health Economics. 32(6): 1244-1255. https://doi.org/10.1002/hec.4664 

*Health Benefits Package

## Files of interest
### Data
1. **Clean input data on interventions**: _1_data/malawi_intervention_data.xlsx_
2. **HR Constraints** (not used for the 2023 VIH paper): _1_data/malawi_hr_data.xlsx_

### Script
1. **Linear Programming Function**: _0_script/malawi_ehp_analysis.R_

## Scenarios of interest
In Table S1 of the paper, results from two scenarios are included. 
1. Without donor constraints - Scenario 5 in _0_script/malawi_ehp_analysis.R_ (lines 548-552)
2. With Donor constraints - Scenario 7 in _0_script/malawi_ehp_analysis.R_ (lines 559-565)


## Acknowledgements 
The script was written by _Sakshi Mohan_. The authors of the paper submitted to VIH are _Emilia Connolly , Sakshi Mohan, Pakwanja Twea, Thulasoni Msuku, Andreas Kees, Lalit Sharma, Stephanie Heung, Dominic Nkhoma, Gerald Manthalu_.
