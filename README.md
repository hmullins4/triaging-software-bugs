In Summer 2024, I took ECO 4934 (Topics in Econometrics), where I and my classmates were tasked with building prediction models to determine which software bugs would be fixed using The Eclipse and Mozilla
Defect Tracking Dataset. First, we had to explore and clean the data --- dealing with missing values and irrelevant variables, as well as creating some of our own. Determining which variables would be most
important was certainly the most time-consuming aspect of this project; my teammates and I thoroughly evaluated the data to build the best models we could. After we gathered some insight into exactly what
factors might affect the likelihood of a bug being fixed, we began building our predicition models in R. We tried a variety of methods: a logistic model, a logistic model with LASSO regularization, a bagging
model, a random forest model, a boosting model, and these same methods again adding polynomial and interaction variables. After developing each model and predicting with testing data, we assessed the area
under the Receiver Operating Characteristic curve (as well as the true positive rate, false positive rate, true negative rate, and false negative rate), and determined our best model according to the results.
These results may be found in the Slides directory.
