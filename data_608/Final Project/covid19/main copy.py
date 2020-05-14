# https://data.world/covid-19-data-resource-hub/covid-19-case-counts/workspace/file?filename=COVID-19+Cases.csv
# https://www.tableau.com/covid-19-coronavirus-data-resources
# https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.apply.html

import pandas
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# from pandas.api.types import CategoricalDtype
# from plotnine import *
# from plotnine.data import mpg

cv_df_raw = pandas.read_csv("COVID19Cases.csv")
cv_df = cv_df_raw.copy()
cv_df = cv_df.drop(columns=["Difference", "Province_State", "Admin2", "Combined_Key", "Prep_Flow_Runtime", "Table_Names", "Lat", "Long", "FIPS"])

# print(cv_df)
# df.apply(lambda x: [1, 2], axis=1)

cv_confirmed = cv_df[(cv_df["Case_Type"] == "Confirmed") & (cv_df["Country_Region"] == "US")]
# males = df[(df[Gender]=='Male') & (df[Year]==2014)]

# cv_confirmed = cv_df[cv_confirmed_cases]
print(cv_confirmed)

# index = np.arange(len(label))
plt.bar(cv_confirmed["Case_Type"], cv_confirmed["Case_Type"])
plt.show()

# cv_confirmed.plot.bar().show();
# from ggplot import *
# ggplot(aes(x="Case_Type", y="Country_Region"), data=cv_confirmed) + geom_line()
# ggplot(cv_confirmed, aes(x='Case_Type', y='Country_Region')) + geom_point()
