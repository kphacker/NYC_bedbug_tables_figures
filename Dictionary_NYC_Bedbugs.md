# Proc_B_bedbug_tables_figures

Figure_1_git:

Figure_1_Appendix_Table_1: Figure 1 - New York City 311 data usage and bed bug related inquiries.  New York City 311 inquiries, bed bug related inquiries, which include official bed bug complaints registered to HPD, and bed bug related inquiries standardized by the total number of 311 inquiries from 2010–2019. Bed bug related inquires were extracted using a text search algorithm. Dates of the two bed bug disclosure laws are indicated by a dashed line. Appendix Table 1 - Total number of inquiries processed by 311 that included bed bugs as part of the description. Description of bed bug related inquires and the agencies that processed the request.

  Data_Processing_Figure_1_Appendix_Table_1.R - R code used to process and clean raw New York City Open Data 311 Inquires as well as the code to produce        Appendix Table 1
  
  Figure_1.R - R code starting with processed and cleaned databases from Data_Processing_Figure_1_Appendix_Table_1.R used to create Figure 1.
  
  month_311_calls_all_2010_2019_RD - cleaned dataset result from Data_Processing_Figure_1_Appendix_Table_1.R detailing the number of 311 call center inquiries per month from 2010 - 2020
  
  month_311_calls_BB_2010_2019_RD - cleaned dataset result from Data_Processing_Figure_1_Appendix_Table_1.R detailing the number of 311 call center inquiries that include information about bed bugs per month from 2010 - 2020 

Figure_2_git:

Figure_2: Figure 2 - Spatial temporal distribution of bed bug complaints in New York City. Spatial temporal distribution of official bed bug complaints registered to HPD from 2014–2019 standardized by the population per NTD area per 100,000. 

  Data_Processing_Figure_2.R - R code used to process geocoded data from the raw New York City Open Data Housing Complaints and Complaint Problems datasets. Data was processed is R and exported to QGIS for visualization 

  geocoded.csv - cleaned dataset result from Data_Processing_Figure_2.R detailing the official geocoded bed bug complaints from 2014 - 2020 from the department of Housing Maintenace and Preservation. Dataset was used to create Figure_2 in QGIS. 

Figure_3_git: 

Figure_3_Appendix_Table_2: Figure 3 - Official bed bug complaints for each borough per year. Official bed bug complaints registered to HPD standardized by borough population per 100,000 from 2014–2019. Appendix Table 2 - Bed bug complaints throughout the boroughs as processed by HPD by year.

  Data_processing_Figure_3_Appendix_Table_2.R - R code used to process and clean data from the raw New York City Open Data Housing Complaints and Complaint Problems datasets. The result creates the Proportion_CI.csv database which includes the total number of bed bug complaints per each borough from 2014 - 2020 as well as population estimates from the American Community Survey. 
  
  Figure_3_Appendix_Table_2.R - R code used to generate figure 3 from Proportion_CI.csv as well as proportions for Appendix Table 2 from the dataset created in Data_processing_Figure_3_Appendix_Table_2.R. This outputs a database including these propotions. 
  
  proportion_CI.csv - cleaned dataset reults of Data_processing_Figure_3_Appendix_Table_2.R and used to create Figure 3 and Appendix Table 2. 
  
  proportions_CI.csv - Dataset generated as a result of Figure_3_Appendix_Table_2.R and used for Appendix Table 2.

Figure_4_git: 

Figure_4_Table_2: Figure 4 - Longitudinal analysis of official bed bug and cockroach complaints registered to HPD. Graphical representation of the results of a linear harmonic model assessing the temporal relationship of official bed bug and cockroach complaints from 2014–2019. (A) Monthly official bed bug complaints standardized by the total number of 311 inquiries per 100,000 with monthly harmonic curve. (B) Monthly cockroach complaints standardized by the total number of 311 inquiries per 100,000 with monthly harmonic curve. (C) Official bed bug complaint residual pattern after extracting the seasonal oscillation from panel A.  (D) Official cockroach complaint residual pattern after extracting the seasonal oscillation from panel B. (E) Complete linear harmonic model assessing official bed bug complaints over time. (F) Complete linear harmonic model assessing official cockroach complaints over time.  Table 2 - Association between time and official bed bug and cockroach complaints accounting for seasonality. Model results of a linear harmonic model assessing the association between month and number of official bed bug complaints and cockroach complaints from 2014 – 2019. Bed bug and cockroach complaints were standardized by the total number of 311 inquiries and multiplied by 100,000. 
  
  Data_processing_Figure_4.R - R code used to process and clean data from raw New York City Open Data 311 Inquires, New York City Open Data Housing Complaints, and Complaint Problems datasets. The results creates the month_311_calls_all_2010_2019_RD.csv, standardized_count_bb_timeseries_clean.csv, standardized_count_roaches_timeseries_clean.csv databases, and 311_calls_complaints_standardaized.csv datasets. 
  
  Figure_4_Table_2.R - R code used to generate Figure 4 and Table 2 using the databases created in Data_processing_Figure_4.R.
  
  311_calls_complaints_standardaized.csv - Primary database used to create Figure_4_Table_2.R. Includes the raw, standardized by 311 inquiries, and standarized by population, counts for cockroach and bed bug complaints from 2014-2020. 
  
  month_311_calls_all_2010_2019_RD.csv - Total number of call center 311 inquiries from 2010 - 2020 per month generated by Data_processing_Figure_4.R. 
  
  standardized_count_bb_timeseries_clean.csv - Total number of bed bug complaints from HPD from 2014 - 2020 as well as number of complaints standardized by NYC population generated by Data_processing_Figure_4.R.
  
  standardized_count_roaches_timeseries_clean.csv - Total number of cockroach complaints from HPD from 2014 - 2020 as well as number of complaints standardized by NYC population generated by Data_processing_Figure_4.R.
  
Figure_5_git: 

Figure_5_Table_3: Figure 5 - Harmonic linear model assessing the temporal patterns of official bed bug complaints for each of the five NYC boroughs. Graphical representation of model results assessing official bed bug complaints per each of the NYC borough standardized by borough population from 2014–2019. Model fit assessed by R-squared and slope of the linear residual pattern when only extracting seasonality are reported. Table 3 - Association between time and official bed bug complaints for each borough accounting for seasonality.  Results of a linear harmonic model standardized per population size for each of the five New York City boroughs from 2014–2019. 
  
  Data_processing_Figure_5.R - R code used to process and clean data from raw New York City Open Data Housing Complaints and Complaint Problems datasets. The result creates the BB_complaints_borough_year_pop.csv datasets NYC_complaint_georef_2014_2019.csv. 
  
  Figure_5_Table_3.R - R code used to generate Figure 5 and Table 3 from the BB_complaints_borough_year_pop.csv. Results in the database nested_model_results_n.csv.
  
  NYC_complaint_georef_2014_2019.csv - Database that includes the joined Housing Complaints and Complaint Problems datasets, which results in geocoded Bed bug complaints from 2014-2020.
  
  BB_complaints_borough_year_pop.csv - Aggregated version of NYC_complaint_georef_2014_2019.csv which includes the total number of bed bug complaints per borough per year from 2014- 2020. 
  
  nested_model_results_n.csv - Model results from Figure_5_Table_3.R for each borough. 
  
Figure_6_git: 

Figure_6: Figure 6 - Comparison between building manager reported bed bug infestation and official bed bug complaints registered to HPD for 2018. Correlation between official bed bug complaints from residents and building manager reported infestation was high (R2 = 0.60).

  Figure_6: R code used to generate Figure 6 using deidentified geocoded information.

  NYC_NTA_2018.csv - Deidentified database of the number of landlord reported bed bug infestation and official bed bug complaints aggregegated to the NTA zone level. 





