# https://data.world/covid-19-data-resource-hub/covid-19-case-counts/workspace/file?filename=COVID-19+Cases.csv
# https://www.tableau.com/covid-19-coronavirus-data-resources
# https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.apply.html

import pandas
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import uuid

cv_df_raw = pandas.read_csv("COVID19_Cases.csv", low_memory=False)

# =========================================================================================================

cv_df_all_countries = cv_df_raw.copy()
# print(cv_df.columns)

cv_df_all_countries = cv_df_all_countries.drop(columns=["Difference", "Province_State", "Admin2", "iso2",	"iso3", "Combined_Key", "Prep_Flow_Runtime", "Lat", "Long", "FIPS", "People_Total_Tested_Count", "Population_Count", "People_Hospitalized_Cumulative_Count", "Data_Source", "Date"])
# cv_confirmed = cv_df[(cv_df["Case_Type"] == "Confirmed") & (cv_df["Country_Region"] == "US")]
cv_df_all_countries = cv_df_all_countries[(cv_df_all_countries["Case_Type"] == "Confirmed")]



#cv_df_all_countries = pandas.read_csv("cv_df_all_countries.csv", low_memory=False)


#cv_df_all_countries.to_csv('cv_df_all_countries.csv', index=False)








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
# plt.show()

cv_df_1_summary = cv_df_1_summary.sort_values("Cases_per_Population", ascending=False).head(10)

fig1, ax1 = plt.subplots()
ax1.pie(cv_df_1_summary["Cases_per_Population"], labels=cv_df_1_summary["Country_Region"], autopct='%1.1f%%', shadow=True, startangle=90)
ax1.axis('equal')  # Equal aspect ratio ensures that pie is drawn as a circle.
plt.title('Top 10 Countries (Population Ratio)')
# plt.show()

# =========================================================================================================

cv_df_tl = cv_df_raw.copy()
cv_df_tl = cv_df_tl.drop(columns=["Difference", "Province_State", "Admin2", "iso2",	"iso3", "Combined_Key", "Prep_Flow_Runtime", "Lat", "Long", "FIPS", "People_Total_Tested_Count", "Population_Count", "People_Hospitalized_Cumulative_Count", "Data_Source"])
cv_df_tl_us = cv_df_tl[(cv_df_tl["Case_Type"] == "Confirmed") & (cv_df_tl["Country_Region"] == "US")]

cv_df_tl_us = cv_df_tl_us.sort_values("Date", ascending=True)

# cv_df_tl_d_us
# cv_df_1_summary

cv_df_tl_d_us = cv_df_tl_us.groupby(["Country_Region", "Date"])["Cases"].sum()
cv_df_tl_d_us = cv_df_tl_d_us.reset_index()

cv_df_tl_d_us["avgcases"] = cv_df_tl_d_us["Cases"].rolling(15).mean()

plt.style.use("fivethirtyeight")
fig1, ax1 = plt.subplots()
ax1.plot_date(cv_df_tl_d_us["Date"], cv_df_tl_d_us["avgcases"], linestyle="solid")
plt.title('COVID-19 US Data')
plt.xticks(rotation=45, fontsize=6)
plt.show()


# ===============================
cv_df_tl_spain = cv_df_tl[(cv_df_tl["Case_Type"] == "Confirmed") & (cv_df_tl["Country_Region"] == "Spain")]

cv_df_tl_spain = cv_df_tl_spain.sort_values("Date", ascending=True)

cv_df_tl_d_spain = cv_df_tl_spain.groupby(["Country_Region", "Date"])["Cases"].sum()
cv_df_tl_d_spain = cv_df_tl_d_spain.reset_index()

cv_df_tl_d_spain["avgcases"] = cv_df_tl_d_spain["Cases"].rolling(15).mean()

plt.style.use("fivethirtyeight")
fig1, ax1 = plt.subplots()
ax1.plot_date(cv_df_tl_d_spain["Date"], cv_df_tl_d_spain["avgcases"], linestyle="solid")
plt.title('COVID-19 Spain Data')
plt.xticks(rotation=45, fontsize=6)
plt.show()
# ===============================

# ===============================
cv_df_tl_skorea = cv_df_tl[(cv_df_tl["Case_Type"] == "Confirmed") & (cv_df_tl["Country_Region"] == "Korea, South")]

cv_df_tl_skorea = cv_df_tl_skorea.sort_values("Date", ascending=True)

cv_df_tl_d_skorea = cv_df_tl_skorea.groupby(["Country_Region", "Date"])["Cases"].sum()
cv_df_tl_d_skorea = cv_df_tl_d_skorea.reset_index()

cv_df_tl_d_skorea["avgcases"] = cv_df_tl_d_skorea["Cases"].rolling(15).mean()

plt.style.use("fivethirtyeight")
fig1, ax1 = plt.subplots()
ax1.plot_date(cv_df_tl_d_skorea["Date"], cv_df_tl_d_skorea["avgcases"], linestyle="solid")
plt.title('COVID-19 South Korea Data')
plt.xticks(rotation=45, fontsize=6)
plt.show()
# ===============================

# ===============================
cv_df_tl_china = cv_df_tl[(cv_df_tl["Case_Type"] == "Confirmed") & (cv_df_tl["Country_Region"] == "China")]

cv_df_tl_china = cv_df_tl_china.sort_values("Date", ascending=True)

cv_df_tl_d_china = cv_df_tl_china.groupby(["Country_Region", "Date"])["Cases"].sum()
cv_df_tl_d_china = cv_df_tl_d_china.reset_index()

cv_df_tl_d_china["avgcases"] = cv_df_tl_d_china["Cases"].rolling(15).mean()

plt.style.use("fivethirtyeight")
fig1, ax1 = plt.subplots()
ax1.plot_date(cv_df_tl_d_china["Date"], cv_df_tl_d_china["avgcases"], linestyle="solid")
plt.title('COVID-19 China Data')
plt.xticks(rotation=45, fontsize=6)
plt.show()
# ===============================

# Italy
# Korea, South

# cv_df_1_summary["uqid1"] = cv_df_1_summary["Country_Region"] + "_" + cv_df_1_summary["Date"].replace("/", "_")
# cv_df_1_summary["mid"] = cv_df_1_summary.index

# cv_df_1_summary["uqid"] = uuid.uuid1()
# cv_df_1_summary = cv_df_1_summary.assign(uqid=uuid.uuid1())

# print(cv_df_1_summary)

# cv_df_1_summary_avg = cv_df_1_summary["Cases"].rolling(3).mean()
# cv_df_1_summary_avg = cv_df_1_summary_avg.reset_index()
# cv_df_1_summary_avg.rename(columns={"Cases": "Cases_avg1"})
# print(cv_df_1_summary_avg)
# print(pd.merge(cv_df_1_summary, cv_df_1_summary_avg))

# rolling = cv_df_1_summary.rolling(window=3)
# rolling_mean = rolling.mean()
# cv_df_1_summary_avg = cv_df_1_summary_avg.rename(columns={"Cases": "Cases_Avg3"})
# print(cv_df_1_summary_avg.head(1000))
# print(cv_df_1_summary)

# pd.merge(cv_df_1_summary, cv_df_1_summary_avg)
# cv_df_1_summary.merge(cv_df_1_summary_avg["Cases_Avg3"])
# print(cv_df_1_summary.head(1000))

# fig1, ax1 = plt.subplots()
# ax1.plot_date(cv_df_1_summary["Date"], cv_df_1_summary["avgcases"], linestyle="solid")
# plt.style.use("classic")
# plt.title('COVID-19 US Data')
# plt.xticks(rotation=45, fontsize=6)
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
