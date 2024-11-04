# Election Prediction

This repository provides all the necessary information to replicate Yingke He and Ziheng Zhong's USA election forecast paper.

## Paper Overview

This paper utlized data from [FiveThirtyEight](https://projects.fivethirtyeight.com/polls/president-general/2024/national/?ex_cid=abcpromo), published by ABC News. Using this polling data of the 2024 U.S. Presidential election, focusing on key candidates Donald Trump and Kamala Harris to provide a comprehensive view of the race. A Hierarchical Bayesian Model is employed to analyze the data, revealing notable differences between states. Pennsylvania, Ohio, Georgia, and Nevada are identified as critical battleground states, with trends favoring Trump. These findings highlight the dynamic nature of the U.S. political landscape and emphasize the pivotal role of swing states in shaping election outcomes.

## File Structure

The files for this research are organized in this repository as follows:

-   `data/raw_data`: the raw data used for analysis

-   `data/analysis_data`: the cleaned data used for fuurther analysis

-   `other/llm`: records of multiple conversations with ChatGPT that assisted in building this project

-   `other/sketches`: previews of the charts and models included in the final research paper

-   `paper`: files used to generate the paper, such as Quarto and bibliography files, as well as the research paper itself

-   `scripts`: R code used for data simulation, download data, clean data, charting and testing

## Large Language Model (LLM) usage statement

This project utilized ChatGPT (ChatGPT 4o model) to assist during the research, development, and writing processes. All records of usage can be found in the `other/llm` folder of this repository.