# Employee churn classifier: Project summary.

* Created a classification model in Rstudio to determine employee churn(attrition) with a F1 score of 65.

* Preprocessing techniques include: Discretisation, lumping, One-hot encoding, factorisation.

* Trained and optimised a Logistic regression and Elastic net with one standard error and cross validation to reach the best model.

* Trough feature importance extraction, the top causes for employee churn is ranked, analysed and presented

# Project describtion
The main objective of this project was to provide a fictional company with a tool to anticipate and classify employee churn. The project was constrained to a very small dataset and only usage of elastic nets (ridge and lasso) and logistic regression. The reasoning for only incoorporating simple algorithms was to ensure model transparrency and interpretability.

## Code and data used
**Dataset:** 35 features, 310 observations

**Rstudio version:** 3.6.2

**Packages used:** caret,recipes, ggplot2, dplyr, ROCR and DataExplorer

## Findings and implications
![Feature importance](/feature importance.png)

