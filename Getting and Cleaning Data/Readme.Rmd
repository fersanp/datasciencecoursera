---
title: "Readme"
author: "Fernanda Sánchez"
output: rmarkdown::github_document
---

There is a [CodeBook](https://github.com/fersanp/datasciencecoursera/blob/master/Getting%20and%20Cleaning%20Data/CodeBook.md) file describing the variables, units and other important stuff used in the dataset.

In order to collect and clean the dataset, I created one [R script](https://github.com/fersanp/datasciencecoursera/blob/master/Getting%20and%20Cleaning%20Data/run_analysis.R) that does the following:

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names.
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

The dataset zip and the tidy dataset are saved in the current working directory.
