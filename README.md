# CATSR-Flight-Prediction-BTS

Orignal Source of the Data:

https://www.transtats.bts.gov/Fields.asp

These datasets should be downloaded first.
This will allow you to be able to run the scripts.
Datasets used for Scripts:
final_data.csv
Airlines.csv
Airport.csv
EQ.csv
Processed_Data.csv
cancellations_dep.csv

https://exchangelabsgmu-my.sharepoint.com/personal/hfotouhi_masonlive_gmu_edu/_layouts/15/onedrive.aspx?id=%2Fpersonal%2Fhfotouhi%5Fmasonlive%5Fgmu%5Fedu%2FDocuments%2FLarge%20Datasets&originalPath=aHR0cHM6Ly9leGNoYW5nZWxhYnNnbXUtbXkuc2hhcmVwb2ludC5jb20vOmY6L2cvcGVyc29uYWwvaGZvdG91aGlfbWFzb25saXZlX2dtdV9lZHUvRXZLakFVX0NVTVZPcnZpYkxuZTVtWFVCQXZMZ09DUDZidHVjSUlLRW5TTHVqZz9ydGltZT1VUGpoVVNmOTEwZw


This link contains rest of the large files (unable to upload to GitHUB):
act_arr.csv
act_dep.csv
data.csv
data_cleaned.csv
Datasets 690.zip


Programs needed to run scripts: R/Rstudio and Python
****The order of code scripts: ****

00 Merge Data.py
01 Clean Data.py
02 Visualize Cleaned Data.py
03 Data_Prepared_for_Modeling.R
04 Plots.R
05 Mapping.R
06 LSTM Time Series.py
07 ATASH(SVM,NN,RF).R
08 Kmean Clustering.py
09 LSTM Time Series (Categorical).py

****Descriptions of each: ****

Data Preparatation in python: 
Need to merge and clean the dataset:
Merging the datasets may require extensive computing power so Argo was used

00 Merge Data.py: 
This code shows how the zip file of the 24 datasets was merged using python.

01 Clean Data.py:
This code shows how the data was cleaned and prepared for analysis using python.

Data Exploration with Python:

02 Visualize Cleaned Data.py
This code was used for creating visualizations to see cleaned data using python.

Data Preparation for R:
03 Data_Prepared_for_Modeling.R
This code is for the data that was prepared for analysis in R.

Data Exploration with R:
04 Plots.R
This code is for the plots that were created in R as part of the analysis.

05 Mapping.R
This code was used for the data mapping done in R.

Modeling:
R:
07 ATASH(SVM,NN,RF).R
This is the code for the SVM, NN, RF analysis methods in R.

Python:
06 LSTM Time Series.py
This code was used to do a LSTM time series analysis using python.

08 Kmean Clustering.py
This code was used to perform Kmeans clustering analysis using python.

09 LSTM Time Series (Categorical).py
This code was used to conduct LSTM time series categorical analysis using python.

