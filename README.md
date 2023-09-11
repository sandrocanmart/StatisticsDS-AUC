# StatisticsDS-AUC

## Project Work

The main core of this exam was compriend the correlation between the Data Mining and the Statistics. Such a strong connection was also enriched by the knowledge and in-depth study of the R programming language. For this exam the work was compriend how this rule merges the classification or general analysis tasks, using a specific and connected languages. 
The code in the repository regard  instead the implementation using a R language of a scientific paper regarding the Three way to calculate the AUC called "On the three-way equivalence of AUC in credit scoring with
tied scores" done by Guoping Zeng and Edward Zeng. The work talk about the way of evaluate models for the credit scoring, so in some sense the way we have for say that an applicant for a loan is or is not a good applicant for the future. The AUC, in that sense, is one of the metrics used, which walk exactly alongside the ROC curve. ROC curve shows the predictive power of a credit scoring model, the ability to identify
goods and bads applicant. To construct the ROC curve, for each score, the cumulative proportion of goods is plotted on the x-axis and the cumulative proportion of bads is plotted on the y-axis, though some users do swap the axes around. AUC, also known as the c-statistic, is a quantitative measure of ROC. It measures the predictive accuracy of a scoring model and is identified by the area which lies under the roc curve. 

The Methods for extract the AUC are three:
- one using a geometric calculation,
- the second counting a random pair of conconcordant,
- discordant or tied pair and the third one regard a special statistics called Wilcoxon rank-sum statistic.

The Geometric method assume that the area might be extract summing triangles, square or rectangles and trapezoid gained by the measure of cumulative distribution in the data(afferent to the two class "Goods" or "Bads"). The sum of that subarea, allow use to gain the total area under the curve. 
The second Method, said "Probability of correct ranking of a (good, bad) pair" analyze a random pair and, looking to a specific parameter of the dataset called Status, establish how that pair are *concordant*(if the one record in the pair is Good and one is Bad, and the score of the Good one is higher than the Bad one), *discordant*(if the one record in the pair is Good and one is Bad, and the score of the Good one is lower than the Bad one) or tied((if the one record in the pair is Good and one is Bad, and the score of the Good one is equal than the Bad one). After which, using appropriate measures, the AUC is obtainable 
The third methods, the Wilcoxon rank-sum statistic, which use the metric of Rank for costruct the area. 


## Final goal

Try to obtain the equivalence between this three methods.
