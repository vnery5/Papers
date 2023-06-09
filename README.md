# Papers and Manuscripts

This repository aims to host the codes of the papers and manuscripts I have written along the years.
Here is a brief abstract of each one:

----

- In my undergrad monography, I study the [impacts of Brazil's Expenditure Ceiling on public and private investment and on the Central Bank's policy rate](https://github.com/vnery5/Papers/tree/main/Synthetic_Control_Expenditure_Ceiling) using the Synthetic Control method.
    - The 2016 Expenditure Ceiling was a turning point in Brazil’s fiscal policy, being grounded in classical economic principles according to which austerity would lead to interest rate declines and to investment expansions;
    - The findings indicate that while it caused Selic to fall, it did not boost private investment and had a negative impact on public capital expenditures, what, given its high fiscal multipliers, contributed to the stagnation that occurred after the 2015-16 crisis;
    - Overall, the results support the need for reform of the country’s fiscal framework in favor of a more flexible rule that does not harm public investment, without losing the commitment to fiscal sustainability in the medium and long term.

<p align="center">
<img src="Synthetic_Control_Expenditure_Ceiling/2_Controles_Sinteticos/Figuras/Investimento_Publico/Trends_InvPub.png" alt="Expenditure Ceiling Negative Impact on Public Investment" width="500"/>
</p>

----

- In my [monography](https://github.com/vnery5/Wage_Inequality_Decomposition) for PET-Economia/UnB, published in *[Laboratório de Economia](https://petecounb.files.wordpress.com/2022/12/revista_2022.pdf)*, I use a RIF Decomposition approach to understand what caused the fall in earnings inequality in Brazil between 2012 and 2015 and what caused it to rise in 2015-2020. Furthermore, I also try to understand what happened during the pandemic, exploiting the fact that the Government Emergency Aid was suspended in the first quarter of 2021.
    - The lower inequality and higher wages in the first half of the decade were caused by better labor force composition (more education) and stronger labor market structure (lower returns to education and experience)
    - In 2015-2020, average wages stagnated and inequality rose, which was due to more informality, educational progress concentrated in the upper part of the wage distribution and changes in unobservable and institutional characteristics. The fall in returns to education and experience - the main driver for inequality reduction so far in the century - stopped.
    - During the pandemic, we see a better composition of the labor force, which is due to the extinction of informal and low-paid services jobs. There is a huge fall on the educational premium in the lower part of the distribution, indicating that people with more years of education had to settle for lower wages in a period where there was no Emergency Aid.

<p align="center">
<img src="Images_Wage_Decomposition/decomposicao_loess_geral.png" alt="Overall Wage Distribution Decomposition" width="500"/>
</p>

<p align="center">
<img src="Images_Wage_Decomposition/decomposicao_loess_detalhada.png" alt="Detailed Wage Distribution Decomposition" width="500"/>
</p>

----

- Brazil is a country which was able to heavily expand its education network in the last decades. While this quantitative expansion was surely needed, the quality education has fallen behind international peers, which has led to a demand for more investment in schools. Using data from national exams, school census and other sources, we compute *panel models* in R in order to find [which infrastructure components](https://github.com/vnery5/Artigos/tree/main/Infrastructure_Effects_Education_Quality) lead to higher grades. The paper was published in *[Eco da Graduação](http://ecodagraduacao.com.br/index.php/ecodagraduacao/article/view/121)*.
    - After controlling for unobservables, we find that the internet connection is the main component that increases grades, specially in the Northeast of the country. The region is the poorest in Brazil, but, despite that, is the one with the most relative gains in educational quality in the recent years.


<p align="center">
<img src="Infrastructure_Effects_Education_Quality/Images/Efeitos_Variaveis.png" alt="Infrastructure Effects on Educational Quality" width="500"/>
</p>

----

- In a paper written for my undergrad Econometrics class, we try to understand the [effect of racial biases](https://github.com/vnery5/Artigos/tree/main/PNADC_Wage_Participation_Determinants) in wages in the Midwest region of Brazil. For that, we use a range of *panel methods* in Python and Stata, ranging from pooled OLS to fixed and random effects. 
    - We also use IVs to account for the endogeneity of education, finding that it has a exponential return to gates;
    - In all models and specifications, we find that non-white people earn sistemically less than their peers, even when controlling for observed and time-invariable unobservables. This effect is more pronounced in higher educations levels and in female wages, indicating that there is a "double discrimination" against black woman;
    - Furthermore, we try to understand the determinants of labor force participation, using *logit* and *probit* models. Being non-white "does not cause" lower labor participation, but the presence of young kids does, specially for woman. *Poisson* regressions revealed the the number of kids is impacted by higher education - effect which is higher for woman -, but with low adherence to the data.
    - Using the *Heckit* procedure for sample correction, we find a negative sample selection of the woman who are in the labor force, which goes against the literature. We interpret this as a consequence of the economic crisis in Brasil in 2014-16, indicating that woman were "forced" to accept jobs that pay less than their observable characteristics predicted in order to make a living.

<p align="center">
<img src="PNADC_Wage_Participation_Determinants/Images/media_rendahabprin_educ_cor.png" alt="Average Wage per Race and Educational Level" width="450"/>
</p>

----

- When the pandemic hit, there was a lot of uncertainty about what it would mean for international trade worldwide. In a paper published in *[Eco da Graduação](http://ecodagraduacao.com.br/index.php/ecodagraduacao/article/view/120)*, we discuss some of the evidence and theory available at the time and, using linear regressions in Python, try to predict the fall in Brazil's trade volume. The folder name is *[International_Trade_Pandemic](https://github.com/vnery5/Artigos/tree/main/International_Trade_Pandemic)*.

<p align="center">
<img src="International_Trade_Pandemic/Images/Proxy para a Elasticidade PIB-Comércio Total Brasileiro.png" alt="Average Wage per Race and Educational Level" width="450"/>
</p>
