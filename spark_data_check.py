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
    NUMERICTYPES = {"int", "bigint", "long", "float", "double", "integer"}

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
        df = df.drop("_c0")  # drop the unnamed index column if it exists
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
        df = spark.createDataFrame(pandas_df)
        return cls(df)

    # ----------------------------------------------------------------
    # Helper method for type checking
    # ----------------------------------------------------------------
    def _is_numeric(self, col_name):
        """Check if a column has a numeric data type."""
        # Get the list of (column_name, type) tuples
        type_list = self.df.dtypes

        # turninto a dictionary so we can look up by column name
        type_dict = dict(type_list)

        # get type string for col_name
        col_type = type_dict.get(col_name, "")

        # Define what we consider numeric
        numeric_types = {"int", "bigint", "long", "float", "double", "integer"}

        # Check if our column's type is in that set
        if col_type in numeric_types:
            return True
        else:
            return False

    def _is_string(self, col_name):
        """Check if a column has a string data type."""
        # Get the list of (column_name, type) tuples
        type_list = self.df.dtypes

        # Turn it into a dictionary so we can look up by column name
        type_dict = dict(type_list)

        # Get the type string for our column
        col_type = type_dict.get(col_name, "")

        # Check if it's a string type
        if col_type == "string":
            return True
        else:
            return False

    # ----------------------------------------------------------------
    # Validation methods (modify self.df, return self for chaining)
    # ----------------------------------------------------------------

    def check_range(self, col_name, lower=None, upper=None):
        """Check if values in a numeric column fall within [lower, upper].

        Appends a boolean column named '{col_name}_in_range' to the DataFrame.
        NULL values in the original column produce NULL in the result.

        Parameters
            col_name : str
                Name of the numeric column to check.
            lower : numeric, optional
                Lower bound (inclusive). If None, no lower bound check.
            upper : numeric, optional
                Upper bound (inclusive). If None, no upper bound check.

        Returns
            self
                Returns self for method chaining.
        """
        # At least one bound must be provided
        if lower is None and upper is None:
            print("At least one of lower or upper must be provided.")
            return self

        # Check if column is numeric
        col_type = dict(self.df.dtypes).get(col_name, "")
        if col_type not in {"int", "bigint", "long", "float", "double", "integer"}:
            print(f"ERROR: Column '{col_name}' is not numeric. No modification made.")
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
        # Check if column is a string type
        col_type = dict(self.df.dtypes).get(col_name, "")
        if col_type != "string":
            print(f"ERROR: Column '{col_name}' is not a string column. No modification made.")
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


    # -------------------------------------------------------------------------------
    # Summarization methods (return summary pandas DataFrames, do not modify self.df)
    # -------------------------------------------------------------------------------

    def get_min_max(self, col_name=None, group_col=None):
        """Report min and max of numeric columns, optionally grouped.

        Parameters
            col_name : str, optional
                column name. Numeric. If None, all numeric columns are used.
            group_col : str, optional
                group by before computing min/max.

        Returns
            pandas.DataFrame or None
                pandas DataFrame with min/max values, or None if
                the specified column is not numeric.
        """
        # case 1: column name provided
        if col_name is not None:
            # Check if column is numeric
            col_type = dict(self.df.dtypes).get(col_name, "")
            if col_type not in {"int", "bigint", "long", "float", "double", "integer"}:
                print(f"ERROR: Column '{col_name}' is not numeric.")
                return None

            # Build the aggregation expressions using F.min and F.max
            # follows .groupBy().agg() pattern from the notes
            agg_exprs = [
                F.min(col_name).alias(f"{col_name}_min"),
                F.max(col_name).alias(f"{col_name}_max")
            ]

            # If a grouping variable was provided, group first then aggregate
            if group_col is not None:
                result = self.df.groupBy(group_col).agg(*agg_exprs)
            else:
                # No grouping — just aggregate the whole DataFrame
                result = self.df.agg(*agg_exprs)

            # Convert to regular pandas before returning
            return result.toPandas()

        # case 2: No column specified, use all numeric columns
        else:
            # find all numeric columns by checking dtypes
            numeric_types = {"int", "bigint", "long", "float", "double", "integer"}
            numeric_cols = []
            for name, dtype in self.df.dtypes:
                if dtype in numeric_types:
                    numeric_cols.append(name)

            # no numeric columns exist
            if len(numeric_cols) == 0:
                print("INFO: No numeric columns found in the DataFrame.")
                return None

            # case 2a: All numeric columns, no grouping
            if group_col is None:
                # build list of agg expressions for all numeric cols
                agg_exprs = []
                for c in numeric_cols:
                    agg_exprs.append(F.min(F.col(f"`{c}`")).alias(f"{c}_min"))
                    agg_exprs.append(F.max(F.col(f"`{c}`")).alias(f"{c}_max"))

                # one .agg() call with all expressions, then toPandas
                result = self.df.agg(*agg_exprs)
                return result.toPandas()

            # case 2b: All numeric columns, with grouping
            else:
                pandas_dfs = []
                for c in numeric_cols:
                    # for each numeric column, groupBy and get min/max
                    agg_exprs = [
                        F.min(F.col(f"`{c}`")).alias(f"{c}_min"),
                        F.max(F.col(f"`{c}`")).alias(f"{c}_max")
                    ]
                    one_result = (
                        self.df
                        .groupBy(group_col)
                        .agg(*agg_exprs)
                        .toPandas()
                    )
                    pandas_dfs.append(one_result)

                # reduce with pd.merge to combine all the DataFrames
                # into one wide DataFrame, joining on the group column.
                # reduce takes a function and a list, applying the function
                # cumulatively: merge(merge(df1, df2), df3), etc.
                combined = reduce(
                    lambda a, b: pd.merge(a, b, on=group_col),
                    pandas_dfs
                )
                return combined

    def get_counts(self, col1, col2=None):
        """Report counts for one or two string columns.

        Parameters
            col1 : str
                First string column (required).
            col2 : str, optional
                Second string column for cross-tabulation.

        Returns
            pandas.DataFrame or None
                A pandas DataFrame with counts, or None if columns
                are not strings.
        """
        # check if col1 is a string column
        col1_type = dict(self.df.dtypes).get(col1, "")
        if col1_type != "string":
            print(f"ERROR: Column '{col1}' is not a string column.")
            return None

        # if col2 was provided, check that one too
        if col2 is not None:
            col2_type = dict(self.df.dtypes).get(col2, "")
            if col2_type != "string":
                print(f"ERROR: Column '{col2}' is not a string column.")
                return None

            # two-column counts: groupBy both, count, and sort
            result = (
                self.df
                .groupBy(col1, col2)
                .count()
                .orderBy(col1, col2)
            )
        else:
            # Single-column counts: groupBy one, count, and sort
            result = (
                self.df
                .groupBy(col1)
                .count()
                .orderBy(col1)
            )

        # convert to regular pandas before returning
        return result.toPandas()



