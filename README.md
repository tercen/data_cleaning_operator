# Data cleaning operator

##### Description

The data_cleaning operator is designed to import and clean a dataset. 

##### Usage

Input projection|.
---|---
`row1`        | factor, variable name 
`row2`        | factor, variable type 
`column`        | factor, input data document ID

Input parameters|.
---|---
`input_var`        | parameter description

Output relations|.
---|---
`clean_variables`        | output variables

##### Details

The operator filters out and / or impute missing data, numerically encodes factors and dates. It also ensures that all variables are in the correct type.

The user needs to provide variable types as a text file (see example).

