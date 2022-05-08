## PredictingSectionEnrollmentLTCC


*This repository contains all files used in a project predicting section level enrollment counts at Lake Tahoe Community College using Machine Learning.*

## Project Overview
This project looked at how we could use popular regression models to forecast section level enrollment at Lake Tahoe Community College. The models were each given predictors based on section offerings such as time of day and day patterns, as well as course department data, term data, program date and historical enrollment trends. From this predictor space we sought to train models on historical data and then use the models to predict on a hold out validation set for an entire term. The models were trained on data from 2012-2017 and then asked to predict enrollments for each term in the 2018 academic year.

Additionally, an analysis of variable importance was done on the ensemble models to determine which variables were the strongest predictors of section enrollment.

In order to demonstrate the success of the models used a Tableau dashboard was created for decision makers to be able to compare how each model did on different courses in different departments. The dashboard is available to view on Tableau public. (link below)


## The files in the repository illustrates the following processes
* Pulling Data in SQL
* Performing EDA in R
* Running Models on Train and Test Data in R
* Communicating findings in a research paper
* Communicating findings in a Tableau Dashboard which can be found on Tableau Public
* https://public.tableau.com/profile/michael.atkinson#!/vizhome/ML_Proj_Dash/Dashboard1?publish=yes
* Predicting Section Growth Files
* ML_Section_Project_Queries.sql (SQL queries used to gather data)
* EDA_FP.R (R code for Exploratory Data Analysis)
* VI_Analysis.R (R code for Variable Importance Analysis)
* Modeling_FP.R (R code for running and comparing models)
* PredictingSectionEnrollmentLTCC.pdf (Research paper detailing procedure, findings, and literature review)
* ModelDash.csv (Data for Tableau Dashboard)
* ModelDash.hyper (Data Extract)
* ML_Proj_Dash.tbw (Tableau Dashboard for communicating results)
* C4P.Csv (Course Program Data)
* Sed_DT.csv (Date Time Section data)
* SECTT1.csv (Section Enrollment Data)

## Project Takeaways
The Lasso Regression, XGboost, and Random Forest all showed great promise as a means to forecast section enrollments on the terms used as validation data. By holding a term out and training the models on past terms, we have a good understanding of how they would perform. The residual behavior on the validation data showed the majority of predictions were within 4 enrollments of the actual observed enrollments. This number was improved by using an Ensemble forecast of the 3 best models and the majority of the predictions came within 3 enrollments on the validation data set.

The variable importance analysis showed that much of the predictive power in the models came from the previous enrollment patterns for the given sections. It would be interesting to explore how traditional Time Series models such as ARIMA would perform on the same data set in the future.




