# MemeStocks Forecasting via Sentiment Analysis and Time Series Analysis 

Project Description: Calculation of sentiment scores using VADER on WallStreetBets subreddit posts from 2021 and analysis of its relationship with an artificial index created from the performance of the 50 most popular stocks during that period.

## Table of Contents

- [Introduction](#introduction)
- [Features](#features)
- [Analysis Book](#analysis-book)
- [Results](#results)
- [Contributing](#contributing)
- [License](#license)

## Introduction

This project focuses on analyzing sentiment scores from WallStreetBets subreddit posts using the VADER (Valence Aware Dictionary and sEntiment Reasoner) sentiment analysis tool. Additionally, the project explores the relationship between sentiment scores and an artificial index derived from the performance of the top 50 most popular stocks during the specified period.

## Features

- Sentiment analysis of WallStreetBets subreddit posts using VADER.
- Creation of an artificial index from the performance of the 50 most popular meme-stocks.
- Time series analysis and VAR (Vector Autoregression) models to assess the relationship between sentiment scores and the meme-stocks index.

## Analysis Book

This project's analysis is presented as a book, where each Jupyter notebook corresponds to a page:

1. [Notebook 1: Data Preprocessing](https://github.com/Fabio-Rossi-Hub/MemeStock-Sentiment-Forecasting/blob/main/notebook/01_Data_Preprocessing.ipynb)
2. [Notebook 2: Sentiment Analysis](https://github.com/Fabio-Rossi-Hub/MemeStock-Sentiment-Forecasting/blob/main/notebook/02_Sentiment_Analysis.ipynb)
3. [Notebook 3: R50 Stocks Index Creation](https://github.com/Fabio-Rossi-Hub/MemeStock-Sentiment-Forecasting/blob/main/notebook/03_Feature_Engineering.ipynb)
4. [Notebook 4: VAR Modeling](https://github.com/Fabio-Rossi-Hub/MemeStock-Sentiment-Forecasting/blob/main/notebook/04_VAR_Modeling.ipynb)

To view the complete analysis, start with Notebook 1 and proceed through each notebook.

## Results

The researched showed that the sentimet of the WallStreetBets community did not have enough predictive power to predict the returns of the MemeStocks as a whole by itself. However, it could have been used as a factor in a larger model.

Please note that the scope of this research is limited to the assessment of causality between sentiment and an artificial index of meme-stocks over a daily time frame of 140 days. Different time frames and different target variables may lead to different results.


## Contributing

Contributions are welcome! If you find a bug or want to expand this research, feel free to open an issue or submit a pull request.

## License

This project is licensed under the [MIT License](LICENSE).
