# Employee churn classifier: Project summary.

* Created a classification model in Rstudio to determine employee churn(attrition) with a F1 score of 65.

* Preprocessing techniques include: Discretisation, lumping, One-hot encoding, factorisation.

* Trained and optimised a Logistic regression and Elastic net with one standard error and cross validation to reach the best model.

* Trough feature importance extraction, the top causes for employee churn is ranked, analysed and presented

# Project description
The main objective of this project was to provide a fictional company with a tool to anticipate and classify employee churn. The project was constrained to a very small dataset and only usage of elastic nets and logistic regression. The reasoning for only incoorporating simple algorithms was to ensure model transparrency and interpretability.

*The full project report can be found @ [](Employee%20churn%20prediction.pdf)

## Code and data used
**Dataset:** 35 features, 310 observations - Including information about employees such as salary, manager, senority, gener etc.

**Rstudio version:** 3.6.2

**Packages used:** caret, recipes, ggplot2, dplyr, ROCR and DataExplorer

## Findings and implications
As seen on the candidate model overview, the logistic regression is outperforming the elastic nets significantly.

![](candidate%20models.png)

The major benefit of fitting a linear model like logistic regression is that feature extraction can be used to infer impact and direction of inputs.
Trough coefficient analysis, it can be derrived that features such as hiring type, employer name, location and more is highly influencial on employee attrition

![Feature importance](feature%20importance.png)

