# https://data.world/covid-19-data-resource-hub/covid-19-case-counts/workspace/file?filename=COVID-19+Cases.csv
# https://www.tableau.com/covid-19-coronavirus-data-resources
# https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.apply.html

import pandas
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

cv_df_raw = pandas.read_csv("COVID19_Cases.csv", low_memory=False)

# =========================================================================================================

cv_df_all_countries = cv_df_raw.copy()
# print(cv_df.columns)

cv_df_all_countries = cv_df_all_countries.drop(columns=["Difference", "Province_State", "Admin2", "iso2",	"iso3", "Combined_Key", "Prep_Flow_Runtime", "Lat", "Long", "FIPS", "People_Total_Tested_Count", "Population_Count", "People_Hospitalized_Cumulative_Count", "Data_Source", "Date"])
# cv_confirmed = cv_df[(cv_df["Case_Type"] == "Confirmed") & (cv_df["Country_Region"] == "US")]
cv_df_all_countries = cv_df_all_countries[(cv_df_all_countries["Case_Type"] == "Confirmed")]

# cv_df_all_countries_summary = cv_df_all_countries.groupby(["Country_Region"])
# .sum()

cv_df_all_countries_summary = cv_df_all_countries.groupby(["Country_Region"])["Cases"].sum().rename("Total_Cases").reset_index()

# Example of group and rename
# https://pbpython.com/pandas_transform.html

cv_df_all_countries_summary = cv_df_all_countries_summary[(cv_df_all_countries_summary["Total_Cases"] > 500000)]

plt.bar(cv_df_all_countries_summary["Country_Region"], cv_df_all_countries_summary["Total_Cases"])
plt.xticks(rotation=45, fontsize=8)

plt.ylabel('Cases')
plt.title('Total Case by Country')
# plt.xticks(ind, ('G1', 'G2', 'G3', 'G4', 'G5'))
# plt.yticks(np.arange(0, 81, 10))
# plt.legend((p1[0], p2[0]), ('Men', 'Women'))

# plt.grid(True)
# plt.show()

# =========================================================================================================

cv_df_1 = cv_df_raw.copy()

cv_df_1 = cv_df_1.drop(columns=["Difference", "Province_State", "Admin2", "iso2",	"iso3", "Combined_Key", "Prep_Flow_Runtime", "Lat", "Long", "FIPS", "People_Total_Tested_Count", "People_Hospitalized_Cumulative_Count", "Data_Source", "Date"])
# cv_confirmed = cv_df[(cv_df["Case_Type"] == "Confirmed") & (cv_df["Country_Region"] == "US")]

cv_df_1 = cv_df_1[(cv_df_1["Case_Type"] == "Confirmed")]

cv_df_1_summary = cv_df_1.groupby(["Country_Region"])["Cases", "Population_Count"].sum()

cv_df_1_summary = cv_df_1_summary.reset_index()

cv_df_1_summary["Cases_per_Population"] = ((cv_df_1_summary["Cases"] / cv_df_1_summary["Population_Count"]) * 100000)

cv_df_1_summary = cv_df_1_summary[(cv_df_1_summary["Cases"] > 500000)]

plt.bar(cv_df_1_summary["Country_Region"], cv_df_1_summary["Cases_per_Population"])
plt.xticks(rotation=45, fontsize=8)

plt.ylabel('Cases per Population')
plt.title('Total Case by Country per Population')
plt.style.use("seaborn")
# plt.grid(True)
plt.show()


cv_df_1_summary = cv_df_1_summary.sort_values("Cases_per_Population", ascending=False).head(10)

fig1, ax1 = plt.subplots()
ax1.pie(cv_df_1_summary["Cases_per_Population"], labels=cv_df_1_summary["Country_Region"], autopct='%1.1f%%', shadow=True, startangle=90)
ax1.axis('equal')  # Equal aspect ratio ensures that pie is drawn as a circle.
plt.title('Top 10 Countries (Population Ratio)')
# plt.show()

# =========================================================================================================

cv_df_tl = cv_df_raw.copy()
cv_df_tl = cv_df_tl.drop(columns=["Difference", "Province_State", "Admin2", "iso2",	"iso3", "Combined_Key", "Prep_Flow_Runtime", "Lat", "Long", "FIPS", "People_Total_Tested_Count", "Population_Count", "People_Hospitalized_Cumulative_Count", "Data_Source"])

# cv_df_tl = cv_df_tl[(cv_df_tl["Case_Type"] == "Confirmed")]
# cv_df_tl = cv_df_tl.head(100)

# plt.plot_date(cv_df_tl["Date"], cv_df_tl["Cases"])
# plt.show()

# =========================================================================================================

# 
# ["Cases", "Country_Region"]

# print(cv_df_all_countries_summary.apply(lambda x: x.reset_index(drop = True)))
# print(cv_df_all_countries_summary)


# plt.bar(cv_df_all_countries_summary["Country_Region"], cv_df_all_countries_summary["Cases"])
# plt.show()

# .plot(legend=True)
# print(cv_df_all_countries_summary.sum())

# plt.bar(cv_df_all_countries_summary["Country_Region"], cv_df_all_countries_summary["Cases"])
# plt.show()

# plt.plot([1,2,3,4])
# plt.ylabel('some numbers')
# plt.show()

# from pandas.api.types import CategoricalDtype
# from plotnine import *
# from plotnine.data import mpg

# cv_df_raw = pandas.read_csv("COVID19Cases.csv", low_memory=False)
# cv_df = cv_df_raw.copy()

# print(cv_df.columns)

# cv_df_raw1 = pandas.read_csv("COVID19Cases_old1.csv", low_memory=False)
# print(cv_df_raw1.columns)

# cv_df = cv_df.drop(columns=["Difference", "Province_State", "Admin2", "Combined_Key", "Prep_Flow_Runtime", "Table_Names", "Lat", "Long", "FIPS"])

# # print(cv_df)
# # df.apply(lambda x: [1, 2], axis=1)

# cv_confirmed = cv_df[(cv_df["Case_Type"] == "Confirmed") & (cv_df["Country_Region"] == "US")]
# # males = df[(df[Gender]=='Male') & (df[Year]==2014)]

# # cv_confirmed = cv_df[cv_confirmed_cases]
# print(cv_confirmed)

# # index = np.arange(len(label))
# plt.bar(cv_confirmed["Case_Type"], cv_confirmed["Case_Type"])
# plt.show()

# # cv_confirmed.plot.bar().show();
# # from ggplot import *
# # ggplot(aes(x="Case_Type", y="Country_Region"), data=cv_confirmed) + geom_line()
# # ggplot(cv_confirmed, aes(x='Case_Type', y='Country_Region')) + geom_point()
