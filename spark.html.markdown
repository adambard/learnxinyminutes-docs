---
language: Spark
category: tool
tool: Spark
filename: learnspark-en.spark
contributors:
    - ["Scronge", "https://github.com/Scronge"]
---

[Spark](https://spark.apache.org/) is an open-source distributed data processing framework that enables large-scale data processing across clusters. This guide covers the basics of **Apache Spark** using PySpark, the Python API.

```python
# Setting Up Spark
from pyspark.sql import SparkSession

spark = SparkSession.builder \
    .appName("RealWorldExampleApp") \
    .getOrCreate()

# Working with Larger DataFrames
# Sample data for a retail dataset with multiple columns for complex queries
data = [
    ("Alice", "Electronics", 30, 200),
    ("Bob", "Clothing", 40, 150),
    ("Carol", "Electronics", 25, 300),
    ("Dave", "Home Goods", 35, 100),
    ("Eve", "Clothing", 28, 80),
    ("Frank", "Home Goods", 40, 120)
]
columns = ["Name", "Category", "Age", "PurchaseAmount"]

df = spark.createDataFrame(data, columns)
df.show()
# +-----+-----------+---+--------------+
# | Name|   Category|Age|PurchaseAmount|
# +-----+-----------+---+--------------+
# |Alice|Electronics| 30|           200|
# |  Bob|   Clothing| 40|           150|
# |Carol|Electronics| 25|           300|
# | Dave| Home Goods| 35|           100|
# |  Eve|   Clothing| 28|            80|
# |Frank| Home Goods| 40|           120|
# +-----+-----------+---+--------------+

# Transformations and Actions

# Filtering data to select customers over 30 with purchases above $100
filtered_df = df.filter((df.Age > 30) & (df.PurchaseAmount > 100))
filtered_df.show()
# +-----+-----------+---+--------------+
# | Name|   Category|Age|PurchaseAmount|
# +-----+-----------+---+--------------+
# |  Bob|   Clothing| 40|           150|
# |Frank| Home Goods| 40|           120|
# +-----+-----------+---+--------------+

# Grouping and Aggregations
# Calculate total purchases by category
category_totals = df.groupBy("Category").sum("PurchaseAmount")
category_totals.show()
# +-----------+------------------+
# |   Category|sum(PurchaseAmount)|
# +-----------+------------------+
# |Electronics|               500|
# |   Clothing|               230|
# | Home Goods|               220|
# +-----------+------------------+

# SQL Queries

# Registering DataFrame as a SQL temporary view
df.createOrReplaceTempView("customers")
high_spenders = spark.sql("SELECT Name, Category, PurchaseAmount FROM customers WHERE PurchaseAmount > 100")
high_spenders.show()
# +-----+-----------+--------------+
# | Name|   Category|PurchaseAmount|
# +-----+-----------+--------------+
# |Alice|Electronics|           200|
# |  Bob|   Clothing|           150|
# |Carol|Electronics|           300|
# |Frank| Home Goods|           120|
# +-----+-----------+--------------+

# Reading and Writing Files

# Reading from CSV with additional options
csv_df = spark.read.csv("path/to/large_retail_data.csv", header=True, inferSchema=True, sep=",")
csv_df.show(5)  # Display only first 5 rows for preview

# Writing DataFrame to Parquet format for optimized storage and access
df.write.parquet("output/retail_data")

# RDD Basics

# Creating an RDD and performing complex transformations
sales_data = [(1, 100), (2, 150), (3, 200), (4, 250)]
rdd = spark.sparkContext.parallelize(sales_data)

# Transformations to calculate discounts for each sale
discounted_sales_rdd = rdd.map(lambda x: (x[0], x[1] * 0.9))
print(discounted_sales_rdd.collect())
# Output: [(1, 90.0), (2, 135.0), (3, 180.0), (4, 225.0)]

# Ending the Spark Session

spark.stop()
