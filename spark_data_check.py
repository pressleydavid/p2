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

    # Acceptable numeric types for range checks
    NUMERIC_TYPES = {"int", "bigint", "long", "float", "double", "integer"}

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
        """Create a SparkDataCheck instance by reading a CSV file.
        Parameters:
            spark : pyspark.sql.SparkSession
                An active Spark session.
            path : str
                Path to the CSV file (local filesystem).

        Returns:
            SparkDataCheck
                A new instance wrapping the loaded DataFrame.
        """
        df = spark.read.load(
            path,
            format="csv",
            sep=",",
            inferSchema="true",
            header="true"
        )
        return cls(df)

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

    def check_range(self, col_name, lower=None, upper=None):
        """Check if values in a numeric column fall within [lower, upper].

        Appends a boolean column named '{col_name}_in_range' to the DataFrame.
        NULL values in the original column produce NULL in the result.

        Parameters
        ----------
        col_name : str
            Name of the numeric column to check.
        lower : numeric, optional
            Lower bound (inclusive). If None, no lower bound check.
        upper : numeric, optional
            Upper bound (inclusive). If None, no upper bound check.

        Returns
        -------
        self
            Returns self for method chaining.
        """
        # At least one bound must be provided
        if lower is None and upper is None:
            print("At least one of lower or upper must be provided.")
            return self

        # Check if column is numeric (inline)
        col_type = dict(self.df.dtypes).get(col_name, "")
        if col_type not in {"int", "bigint", "long", "float", "double", "integer"}:
            print(f"Column '{col_name}' is not numeric. No modification made.")
            return self

        # Build the range condition
        col = F.col(col_name)

        if lower is not None and upper is not None:
            condition = col.between(lower, upper)
        elif lower is not None:
            condition = col >= lower
        else:
            condition = col <= upper

        # Wrap in F.when to preserve NULLs
        result_col = F.when(col.isNull(), None).otherwise(condition)

        # Append the boolean column and return self for chaining
        self.df = self.df.withColumn(f"{col_name}_in_range", result_col)
        return self

def check_levels(self, col_name, levels):
    """Check if values in a string column are within a specified set.

    Appends a boolean column named '{col_name}_in_levels' to the DataFrame.
    NULL values in the original column produce NULL in the result.

    Parameters:
        col_name : str
            Name of the string column to check.
        levels : list
            List of acceptable string values.

    Returns:
        self
            Returns self for method chaining.
    """
    # Check if column is a string type (inline)
    col_type = dict(self.df.dtypes).get(col_name, "")
    if col_type != "string":
        print(f"Column '{col_name}' is not a string column. No modification made.")
        return self

    # Build the condition
    col = F.col(col_name)
    condition = col.isin(levels)

    # preserve NULLs
    result_col = F.when(col.isNull(), None).otherwise(condition)

    # Append the boolean column and return self for chaining
    self.df = self.df.withColumn(f"{col_name}_in_levels", result_col)
    return self

def check_missing(self, col_name):
    """Check if values in a column are NULL.

    Appends a boolean column named '{col_name}_is_missing' to the DataFrame.

    Parameters
        col_name : str
            Name of the column to check for missing values.

    Returns:
        self
            Returns self for method chaining.
    """
    # No type checking. any column can have NULLs
    result_col = F.col(col_name).isNull()

    # Append the boolean column and return self for chaining
    self.df = self.df.withColumn(f"{col_name}_is_missing", result_col)
    return self

