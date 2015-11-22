Analysis of relation between Stars rating vs. most relevant Attributes and n-grams (words) in Reviews filtered by State and Category of Business.
========================================================
author: Jose Maria Vilaverde
date: November 22th 2015

Introduction
========================================================

In order to get top rate as business in a city is analyzed Where have to be established my business, which services I need to offer, what positive review words I need to be associated to my business and what negative review words I need to avoid

To do that analysis is used as input Business Category and City. As output: Top Rate, Neighborhood, Services, Top-5 positive words to promote, Top-5 negative words to avoid.

For example, If I want to open a business for category "dentist" in Arizona, I need to know where is the best place to open the business, services that I have to give to my customers, like "credit card accepted", and identify most relevant positive review words that I need to get from my customers and negative review words for my business category in Arizona.

Methods and Data
========================================================

Steps:

*ETL -> 1.Get_Data.R*

- Download data from https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/yelp_dataset_challenge_academic_dataset.zip and unzip it.
- Extract json information from files "business","checkin","tip","review" (Excluded "user", not relevant for my analysis)
- Store it into RDS file. Review is divided into files with 100.000 lines/file in order to make affordable the calculation for my computer. For review I obtained 16 files.
- Identify business per state.
- Filter files per business_id per State, create RDS files "checkin","tip","review" per State.


Slide With Code
========================================================

**List of top attributes**

List filtered, % of positive and negative over total must be over 2.5%, difference between positive and negative must be over 0.15 stars to be relevant.

\begin{table}[ht]
\centering
\begin{tabular}{lrrrrrrr}
  \hline
Attribute & positives & pos\_avg & negatives & neg\_avg & diff\_avg & pos\% & neg\% \\ 
  \hline
attributes.Parking.street &  72 & 4.07 & 600 & 3.71 & 0.36 & 10.71 & 89.29 \\ 
  attributes.Accepts Credit Cards & 639 & 3.76 &  33 & 3.50 & 0.26 & 95.09 & 4.91 \\ 
  attributes.Price Range & 647 & 3.76 &  25 & 3.58 & 0.18 & 96.28 & 3.72 \\ 
  attributes.Parking.garage &  61 & 3.90 & 611 & 3.73 & 0.17 & 9.08 & 90.92 \\ 
  attributes.Good For.breakfast &  20 & 3.90 & 652 & 3.74 & 0.16 & 2.98 & 97.02 \\ 
  attributes.Caters &  76 & 3.88 & 596 & 3.73 & 0.15 & 11.31 & 88.69 \\ 
   \hline
\end{tabular}
\end{table}

**Top-5 possitive and negative words:**

\begin{table}[ht]
\centering
\begin{tabular}{rlr}
  \hline
 & word & accumulate \\ 
  \hline
1 & good & 38.00 \\ 
  2 & range & 23.00 \\ 
  3 & food & 20.00 \\ 
  4 & place & 18.00 \\ 
  5 & service & 15.00 \\ 
   \hline
\end{tabular}
\end{table}
\begin{table}[ht]
\centering
\begin{tabular}{rlr}
  \hline
 & word & accumulate \\ 
  \hline
1 & food & 21.00 \\ 
  2 & time & 14.00 \\ 
  3 & good & 12.00 \\ 
  4 & place & 12.00 \\ 
  5 & no & 11.00 \\ 
   \hline
\end{tabular}
\end{table}

Slide With Plot
========================================================

**Map of Business:**

![plot of chunk mapBusiness](Presentation-figure/mapBusiness-1.png) 
