# Bayesian Analysis for Game Players Behaviors
 
![R](https://img.shields.io/badge/r-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white)
![Markdown](https://img.shields.io/badge/markdown-%23000000.svg?style=for-the-badge&logo=markdown&logoColor=white)

Starting from the analysis from the paper of ![Vuong et al.](https://direct.mit.edu/dint/article/3/4/606/107672/A-Multinational-Data-Set-of-Game-Players-Behaviors), I try to improve and adjust the prediction they make using a Linear Regression Gaussian model with a Multinomial Logistic Regression more suitable for categorical data in a Bayesian setup using MCMC and the software JAGS.

# The dataset

The dataset available ![here](https://www.scidb.cn/en/detail?dataSetId=cb5d36cce29f4e5695a586c9b85d04b6) aim to examine the relationship between game-playing, in-game behaviors, and environmental perceptions to fill in the gap of lacking resources for studying the effects of commercial video games. The target of the survey are Nintendo’s Animal Crossing: New Horizons (ACNH) game players.

# The inferential goal

We want to hypothesized that people holding an anti-anthropocentric (Anti_Anthro) perception would associate with the frequency of in-game behaviors that harm natural lifeforms. To test this hypothesis, they used three variables from the data set. The anti-anthropocentric perception is represented by the C12 variable, which measures the disagreement towards the statement “Humans were meant to rule over the rest of nature”. Is a categorical variable that assume value in {1, 2, 3, 4, 5}, with higher value means a higher level of disagreement. To explain the anti-anthropocentric, they use two variable E16 and E17, respectively the frequency action of taking wood (TakeWood) and cutting down a tree (CutTree). Each of them assuming value in {1, 2, 3, 4}, with higher value means a higher frequency on doing this action.

<img src="[https://your-image-url.type](https://github.com/giulio-derasmo/Bayesian-Analysis-for-Game-Players-Behaviors/blob/main/images/Dag_model.png)" width="80" height="80">


<p align="center">
    <img src="https://user-images.githubusercontent.com/50860347/147412786-183da6b0-990f-4016-9f2e-0719d8066f5b.png" style="width: 100%"/>
<p>
