# Shiny Impact Analysis
### An interactive app that illustrates the impact of implementing a new cut score

After a standard setting, the psychometrician will provide an assessment program a recommended <strong> cut score </strong> for an examination form. A cut score determines which candidates will pass or fail an exam.

It is useful to know out of the candidates that have already tested, what is the percentage of candidates that will pass or fail based on this newly implemented cut score.

This Shiny app inputs a user-selected CSV files of candidate scores in the first tab. In the second tab, the user can specify the maximum score on the exam, the recommended cut score, as well as the standard error of the judge ratings. 

The app outputs a histogram of the candidate scores. When the cut score is adjusted, the histogram will mark which of the candidates will pass or fail the exam. The user can also specify the standard error, and the text will display the percentage of candidates that would pass with this cut score.
