"""
SparkDataCheck Module

A data quality class for PySpark SQL-style DataFrames.
Provides validation and summarization methods for checking
and profiling data.
"""

from pyspark.sql import DataFrame
from pyspark.sql import functions as F
from functools import reduce
from pyspark.sql.types import *
import pandas as pd

class SparkDataCheck:
    pass

