# Medicare Payment Statistical Analysis

**Business Value:** Achieved statistical analysis of Medicare Payments for 2500+ facilities in the United States with a goal of scrutinizing payment differences across states, predicted out of pocket payments and extra charges for 500+ DRG’s and identified top 5 overcharging facilities in Florida.

# Project Overview

**Data Source**

Data Source for Inpatient Prospective Payment System and Hospital Value Based Purchasing System data - Centers for Medicare & Medicaid Services -https://www.cms.gov/Medicare/Medicare

Data Source for Average Population and Average Household income across all the states in the United States - United States Census Bureau - https://www.census.gov/en.html

**Data Pre-processing**

i) Data Merging - We merged IPPS, HVP performance, Average Income and Average Population dataset by
performing an inner join on facility ID and state.

ii) Data Transformation

● Converted column names to lowercase and renamed variables to achieve
consistency. Splitted DRG definition into DRG code and DRG description.

● Verified if any features have missing values.

● Replaced missing values from performance variables with ‘Not Available’

● Converted continuous variables to numeric and categorical variables into factors.

● Scaled predictors for standardization and rounded fraction up to 4 decimal places.

● Performed Log Transformation for variables with right skewed distribution.

iii) Feature Engineering - Generated features which sketched Average Out of Pocket payment, Average Extra Payment and Percentage Payment Reduction. 

# Recommendations

i) Hospitals in the states of Hawaii, Utah, and Virginia should probe into their billing as they seem to have the highest out of pocket expenses, which has a higher chance of not being repaid since it is paid off by the patients.

ii) Medicare should probe into hospitals such as the Memorial Regional Hospital, Viera Hospital and Coral Gables Hospitals as they are among the highest in terms of extra charges billed to patients resulting in expensive medical treatment bills for the DRG’s as compared to other facilities.

iii) State of Hawaii has the maximum medicare payment considering other states having similar average household income and population.

iv) State of Virginia has the minimum medicare payment considering other states having similar average household income and population.

v) Hospitals should focus on improving their Total Performance Score in order to minimize losses in terms of payment cuts from Medicare. Out of the 2500 unique medical facilities in our dataset, only 105 providers had a TPS >= 59 (the threshold beyond which payment reductions kick in).



