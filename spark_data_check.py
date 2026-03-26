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
    """class wrapper for a Spark SQL DataFrame
        - provides functionality for data validation and summarization.
    """

    def __init__(self, dataframe):
        """Initialize with a Spark SQL DataFrame.
           Parameters
                - dataframe : pyspark.sql.DataFrame
        """
        self.df = dataframe

    # ----------------------------------------------------------------
    # Class methods for alternative constructors
    # ----------------------------------------------------------------

    @classmethod
    def from_csv(cls, spark, path):
        """Create an instance by reading a CSV file.
        Parameters:
            - spark (SparkSession): an active SparkSession
            - path (string): Path to the CSV file
        Return:
            - SparkDataCheck: a new instance that wraps a DataFrame
        """
        pass

    @classmethod
    def from_pandas(cls, spark, pandas_df):
        """Create an instance from a standard pandas DataFrame.
        Parameters:
            - spark (SparkSession): an active SparkSession
            - pandas_df (pd.DataFrame): a pandas DataFrame
        Return:
            - SparkDataCheck: a new instance that wraps a DataFrame
        """
        return spark.createDataFrame(pandas_df)

    # ----------------------------------------------------------------
    # Validation methods (modify self.df, return self for chaining)
    # ----------------------------------------------------------------

    def drop_duplicates(self, subset=None):
        """Drop duplicate rows from the DataFrame.
        Parameters:
            - subset (list of str, optional): Column names to consider for identifying duplicates.
              If None, considers all columns.
        Return:
            - SparkDataCheck (self): duplicates dropped from self.df
        """
        if subset is not None:
            self.df = self.df.dropDuplicates(subset)
        else:
            self.df = self.df.dropDuplicates()
        return self
