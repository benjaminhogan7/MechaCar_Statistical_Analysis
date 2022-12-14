# MechaCar_Statistical_Analysis

## Linear Regression to Predict MPG
### Deliverable 1

* Which variables/coefficients provided a non-random amount of variance to the mpg values in the dataset?
*   Vehicle length and ground clearance

* Is the slope of the linear model considered to be zero? Why or why not?
*   The slope would not be considered zero based on the p-value

* Does this linear model predict mpg of MechaCar prototypes effectively? Why or why not?
*   Due to the R-squared value, this model does seem to predict mpg

Imported CSV file and table from the data

<img width="652" alt="Screen Shot 2022-12-07 at 10 46 17 PM" src="https://user-images.githubusercontent.com/108236450/206351903-0b46382e-9b9b-4c80-aa6a-e79182525516.png">

Linear Regression using lm() function with all table columns

<img width="601" alt="Screen Shot 2022-12-07 at 10 45 27 PM" src="https://user-images.githubusercontent.com/108236450/206351932-066d06c7-0498-4603-8277-a01deafb794e.png">

p-value and the r-squared value from the linear regression model using the summary() function

<img width="631" alt="Screen Shot 2022-12-07 at 10 45 35 PM" src="https://user-images.githubusercontent.com/108236450/206351956-0314d5c1-cfd6-41dd-94f4-589e0c443477.png">

## Summary Statistics on Suspension Coils
### Deliverable 2

* The design specifications for the MechaCar suspension coils dictate that the variance of the suspension coils must not exceed 100 pounds per square inch. Does the current manufacturing data meet this design specification for all manufacturing lots in total and each lot individually? Why or why not?
*   lots 1 and 2 meet the specs, lot 3 doesn't due to the "t" value in the t-tests.

Summarized Suspension Coil Table

<img width="343" alt="Screen Shot 2022-12-07 at 10 54 13 PM" src="https://user-images.githubusercontent.com/108236450/206352696-e21d164b-b89c-4963-8e99-8132dcd80aaa.png">

Total Summary

<img width="337" alt="Screen Shot 2022-12-07 at 10 56 01 PM" src="https://user-images.githubusercontent.com/108236450/206352926-09c05dff-fdab-4aef-82b8-2fa7378e3ca3.png">

Lot Summary

<img width="493" alt="Screen Shot 2022-12-07 at 10 57 32 PM" src="https://user-images.githubusercontent.com/108236450/206353065-04e29923-93b2-448e-bb9d-6a4eb61b7871.png">

## T-Tests on Suspension Coils
### Deliverable 3


<img width="414" alt="Screen Shot 2022-12-07 at 11 00 15 PM" src="https://user-images.githubusercontent.com/108236450/206354442-e9b0553c-d7f4-4467-ac03-c18105d71e3b.png">

Lot 1

<img width="415" alt="Screen Shot 2022-12-07 at 11 03 30 PM" src="https://user-images.githubusercontent.com/108236450/206354072-646ebb7b-7fc5-452d-b42f-1392c1dadfc8.png">

Lot 2

<img width="421" alt="Screen Shot 2022-12-07 at 11 04 22 PM" src="https://user-images.githubusercontent.com/108236450/206354088-3f698ad4-0ad8-45e7-b34e-6bafb416a263.png">

Lot 3

<img width="424" alt="Screen Shot 2022-12-07 at 11 04 29 PM" src="https://user-images.githubusercontent.com/108236450/206354104-bfd19e69-1139-4aea-bd44-ddffef838c3a.png">
