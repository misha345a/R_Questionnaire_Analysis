Quantitative Analysis of Macaroni’s Restuarant Survey Responses.
================
Michael Markin
3/22/2022

Below are the survey questions given to the restaurant employees. 78
total responses were given.

#### Answers from 1 to 7 (with 7 being “Strongly Agree”):

X1 - Learn valuable skills <br /> X2 - Enjoy the work assigned <br />
X3 - Supervisors give praise <br /> X4 - Receive adequate training
<br /> X5 - Paid fairly in comparison to competitors <br /> X6 -
Supervisors recognize potential <br /> X7 - Like working for this
establishment <br /> X8 - Pay fairly for work performed <br /> X9 -
Well-functioning team <br /> X10 - Supervisors are knowledgeable/helpful
<br /> X11 - Team members cooperate <br /> X12 - Overall pay is
reasonable <br />

X13 - Macaroni’s is the best organization to work for <br /> X14 -
Culture of belonging <br /> X15 - Recommend the work to friends <br />
X16 - Feel part of a family <br />

#### Answered on a 0-100 point scale (with 100 being the best score):

X17 - How likely to look for another job <br /> X18 - How often think
about quitting <br /> X23 - Level of performance <br />

#### Employee demographic questions

X19 - Part-time (0) or full-time worker (1) <br /> X20 - Female (0) or
Male (1) <br /> X21 - Age (yrs) <br /> X22 - Length of employment (yrs)
<br />

    ## Warning: package 'ggcorrplot' was built under R version 4.1.3

    ## Loading required package: ggplot2

## Dataset

``` r
# load the data
dataset <- read.csv("Macaronis_Questionnaire_Responses.csv")

# display top rows
head(dataset)
```

    ##   X1 X2 X3 X4 X5 X6 X7 X8 X9 X10 X11 X12 X13 X14 X15 X16 X17 X18 X19 X20 X21
    ## 1  5  5  5  5  5  5  5  5  4   5   3   4   5   5   5   4  50  75   1   0  35
    ## 2  5  5  4  3  3  4  5  3  3   5   3   3   5   5   6   4  30  35   0   0  61
    ## 3  5  5  4  3  3  4  5  3  3   5   3   3   5   5   6   6  25  20   0   0  58
    ## 4  5  5  7  3  2  7  4  3  2   5   2   3   5   5   5   4  65  85   1   0  19
    ## 5  5  5  7  3  5  7  5  5  3   6   4   5   6   5   6   5  45  40   0   0  31
    ## 6  5  4  5  3  3  5  5  5  3   6   4   4   4   4   5   4  75 100   1   0  18
    ##   X22 X23
    ## 1   1  20
    ## 2   2  20
    ## 3   2  25
    ## 4   1  25
    ## 5   3  60
    ## 6   1  25

``` r
# correlation matrix of the first 12 variables 
data.cor<- cor(dataset[,0:12], method = "spearman")

# correlation plot with labels
ggcorrplot(data.cor, hc.order = FALSE, lab = TRUE, type = "upper")
```

![](R_Questionnaire_Analysis_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## How related are the work environment perceptions to each other?

Because the variables is ordinal, the Spearman Correlation is the
appropriate coefficient to calculate. Covariance is very strongly shared
between variables with correlation coefficients over 0.81. Here, that
includes (X5, X12), (X8, X12), and (X3, X6). This indicates that the
favorability of wages (X12) strongly correlates to how well workers are
being paid in relation to competitors (X5) and their efforts (X8).
Praise from supervisors (X3) is also strongly correlated to how well a
worker feels that their potential is being recognized.

## How do these work perceptions influence the feeling that Macaroni’s is a good organization to work for, as measured by X13?

``` r
# analyze 
data_q2 <- dataset[,0:18]
data_q2 <- data_q2[, !colnames(data_q2) %in% c("X18", "X14","X15","X16","X17")]

data.cor1 <- cor(data_q2, data_q2$X13)
ggcorrplot(data.cor1, hc.order = FALSE, lab = TRUE)
```

![](R_Questionnaire_Analysis_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

There are several moderate to strong positive correlations between work
environment perceptions and X13. The strongest correlations are from X4
and X11. Employees feel that Macaroni’s is the best organization to work
for when they receive adequate training and their team members cooperate
well together.

## Is there a difference in likelihood to quit based on the employee’s full-time or part-time status, or by the length of employment, as measured by X22?

``` r
part_time_emp <- dataset[dataset$X19 == 0, ]['X18']
full_time_emp <- dataset[dataset$X19 == 1, ]['X18']

# unpaired two-sample t-test
t.test(part_time_emp, full_time_emp, var.equal = TRUE)
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  part_time_emp and full_time_emp
    ## t = -9.9859, df = 75, p-value = 2.031e-15
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -55.1766 -36.8234
    ## sample estimates:
    ## mean of x mean of y 
    ##  28.57143  74.57143

The unpaired two-sample t-test compares the X18 responses between
full-time and part-time employees. As shown by the very small p-value,
the results indicate that there is indeed a significant difference in
the likelihood to quit between the two groups.

``` r
# one-way anova
one.way <- aov(X18 ~ factor(X22), data = dataset)
summary(one.way)
```

    ##             Df Sum Sq Mean Sq F value   Pr(>F)    
    ## factor(X22)  2  27837   13918   23.98 9.34e-09 ***
    ## Residuals   74  42942     580                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

A one-way ANOVA was computed on the three groups of employees from X22.
Because the p-value is less than the significance level of 0.05, we can
conclude that there are very significant differences between the means
of the three groups.

``` r
# Tukey test
TukeyHSD(one.way)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = X18 ~ factor(X22), data = dataset)
    ## 
    ## $`factor(X22)`
    ##          diff       lwr        upr     p adj
    ## 2-1 -23.30309 -40.30861  -6.297557 0.0045008
    ## 3-1 -43.79310 -58.92388 -28.662324 0.0000000
    ## 3-2 -20.49002 -37.49555  -3.484490 0.0141517

A Tukey test was performed to compute multiple pairwise-comparisons. By
looking at the ‘diff’ column of the output, we can see that the average
scoring differs greatly for X18. With all comparisons having adjusted
p-values under 0.05, we can conclude that there is a significant mean
difference between all groups.

## Advice to management

Full-time employees think about quitting much more frequently than
part-time employees. Employee feelings about supervisor recognition and
group cooperation matter more than attitudes towards competitive pay.
