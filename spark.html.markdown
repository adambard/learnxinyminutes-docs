---
language: Spark
category: tool
tool: Spark
filename: learnspark.spark
contributors:
    - ["Scronge", "https://github.com/Scronge"]
---

[Spark](https://spark.apache.org/) is an open-source distributed data processing framework that enables large-scale data processing across clusters. This guide covers the basics of **Apache Spark** using PySpark, the Python API.

```python
# Setting Up Spark
from pyspark.sql import SparkSession

spark = SparkSession.builder \
    .appName("ExampleApp") \
    .getOrCreate()

# Working with DataFrames
data = [("Alice", 30), ("Bob", 40)]
columns = ["Name", "Age"]

df = spark.createDataFrame(data, columns)
df.show()
# +-----+---+
# | Name|Age|
# +-----+---+
# |Alice| 30|
# |  Bob| 40|
# +-----+---+

# Transformations and Actions

df_filtered = df.filter(df.Age > 35)
df_filtered.show()
# +----+---+
# |Name|Age|
# +----+---+
# | Bob| 40|
# +----+---+

# SQL Queries

df.createOrReplaceTempView("people")
spark.sql("SELECT * FROM people WHERE Age > 30").show()

# Reading and Writing Files

csv_df = spark.read.csv("path/to/file.csv", header=True, inferSchema=True)
df.write.parquet("output_path")

# RDD Basics

rdd = spark.sparkContext.parallelize([1, 2, 3, 4])

squared_rdd = rdd.map(lambda x: x ** 2)
print(squared_rdd.collect())
# Output: [1, 4, 9, 16]

# Ending the Spark Session

spark.stop()
