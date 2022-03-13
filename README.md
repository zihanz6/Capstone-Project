### Overview
This Capstone project is conducted in conjunction with Community Health Plan of Washington, which is a local, Washington‐based Health Plan with long‐established ties to communities throughout the State. The primary objective is developing a valid prediction model on inpatient admission in the following 12 months. The second objective is applying Classification with No Discrimination (CND) technique to correct for racial bias embeded in the data, which refers to the substantial difference in incident rate of inpatient admission comparing different racial groups. 

### Study Population
The study population includes all CHPW members enrolled from Jan 2018 to Dec 2019 who are legal Washington state residents and enrolled in Apple Health (Washington State’s Medicaid Program), Medicare, or other Washington state health care programs.
List of exclusion criteria:
- pregnancy
- severe cancer
- end-stage renal disease
- transplant dialysis
- hospice
- death
- newborns

### Data Source and Key Variables
The project involves the use of in-house data on demographics information, healthcare cost, health resource utilization, health care program enrollment, disease diagnosis, social determinants of health (SDoH) and behavioral health information available within the health plan. The cost, utilization, diagnosis, SDoH and behavioral health data are collected through claims submitted by the health care providers to the health plan. The enrollment and demographic data are collected through enrollment files provided by Washington state. 

### Modeling Process
Our candidate prediction models include: penalized logistic regression, Gradient Boosting Trees and Bayesian Addictive Regression Trees (BART). •	After data extraction and initial data exploration, we randomly split the data into training set and test set with an 80:20 ratio. Each group member trains a class of model on the training set with 10-fold cross-validation approach, which is referred as training stage I. After that, we implement CND technique on the training set and repeat the cross-validation training process, which is referred as training stage II. The performance of the new optimal model selected from each class is evaluated on the test set. Finally, we choose the overall “best” model based on the F1 score from the training stage II.

### Result
The F1 score of cross-validated penalized logistic regression, Gradient Boosting Trees and BART is 0.240, 0.330 and 0.334, respectively. Hence, BART is the overall "best" model selected. After CND implementation, the racial differences in incident rate of inpatient admission is significantly reduced. Thus, CND application is considered as successful.

### Future Directions
Although the F1 score is not idea, it has been improved substantially compared to the previous prediction model developed by our sponsors. Given the large sample size and high diversity of our study population, future statisticians may consider splitting the study population into multiple subgroups based on certain characteristics and identify an optimal prediction model for each subgroup. Besides, imbalanced outcome distribution is a major obstacle to higher model performance. If time and resources allow, it would be worthwhile to try different techniques to handle the imbalanced dataset, such as applying Random Over-Sampling Examples (ROSE) or Synthetic Minority Oversampling Technique (SMOTE). Finally, trying a wider range of statistical methods might also contribute to the boost of prediction accuracy. For example, neural networks might have the potential to outperform the current models, given that they have the ability to implicitly detect complex non-linear associations between predictors and outcome, ability to detect all possible interactions between predictors, and availability of multiple training algorithms.   



