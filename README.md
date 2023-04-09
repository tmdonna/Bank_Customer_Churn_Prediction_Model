# Bank_Customer_Churn_Prediction_Model

This project uses R and machine learning models like Logistic Regression, kNN, QDA/LDA, Tree Classification, and Random Forest to predict churn and identify significant variables. 80% accuracy through model evaluation techniques like confusion matrices and calculating error rates of each model was achieved.

Dataset 
The dataset is a bank customer dataset imported from Kaggle. The dataset contains information on 10,000 different customers. 
It has 14 columns of which 11 of them will be used to make the model (the 3 other columns are RowNumber, Customerid, and Surname which are not useful variables for building the model). Original Dataset contains approximately 20% of customers that churned. Training Dataset contains a total of 2036 rows with equal split between customers that exitted and stayed with the Bank. Testing Dataset used the remaining customers in the pool that exitted (1018 customers) and filled the rest with customers that stayed while maintaining the same proportion of the original dataset, resulting with a total of 5001 rows. Out of the 10,000 customers in the original dataset, 7037 were used for the project and results can be altered by changing the seed.

Data Dictionary 
CreditScore (Integer): CreditScore shows the credit score of the customer, where the maximum credit score is 850 and the minimum score is 350.   
Geography (String):	Geography is a categorical variable that shows the location where the customer is located. This dataset covers customers from three different countries, which are Spain, Germany, and France. 
Gender (String):	Gender is a categorical variable that shows the gender of the customer. The values can be either Male (M) or Female (F).  
Age (Integer):	Age is a continuous variable that shows the age of the customer.  	
Tenure  (Integer):	Tenure is a continuous variable that shows the period of time when the customer is with the bank, measured in years.  
Balance  (Floating Point):	Balance is a continuous variable that shows the total number of balances that the customer owns.  
NumOfProducts  (Integer): 	NumOfProducts shows the number of financial products they have, such as saving accounts, checking accounts, and credit cards.
HasCrCard (Boolean): 	HasCrCard shows whether customers have a credit card with the bank or not.  
IsActiveMember (Boolean):	IsActiveMember shows whether a customer is still an active member with the bank or not. 
Estimated Salary (Floating Point):	EstimatedSalary shows the estimated salary of customers.  	
Exited	(Boolean): 	Exited is a binary variable in which 1 stand for exit and 0 stands for not exit. This is also the dependent variables of this project

Results:
Decision Tree provides the lowest error at 20.23%
Logistic Regression accuracy can be improved by increase the cutoff to 60%
Age, IsActiveMember, NumberofProducts, Balance, Germany are significant variables for bank to protect their customer from exitting.
Age, IsActiveMember, NumberofProducts, and Balance are common significant variables for both Logistic Regression and Decision Tree
HasCr card with the bank or not and the estimated salary should not be considered to predict in our case.
People in Germany significantly tends to exit more than Spain and France.
