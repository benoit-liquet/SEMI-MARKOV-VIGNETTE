# BIOSTATISTICS-SEMI-MARKOV-VIGNETTE

Here we provide a comprehensible detailed vignette to run SEMI-MARKOV models using R software packages.

We illustrate the inference of the semi-Markov model on two real datasets: _Stanford Heart Transplant data_ and _asthma control data_.

This vignette reproduces the results of our application section **Semi-Markov application in practice** from our submitted article:

_Estimation of Semi-Markov Multistate Models: A Comparison between the
Sojourn Times and Transition Intensities Approaches_ A. Asanjarani, B. Liquet and Y. Nazarathy. (2020).


## Getting Started

In this vignette we used the following R packages

```
library("p3state.msm")
library("mstate")
library(SemiMarkov)
library(flexsurv)
library(xtable)
library(eha)
library(mpr)
library(mstate)
```


## Section 4.1 Progressive Three-State Model for the Stanford Heart Transplant data 

- This analysis is presented [here](/Section4-1.md)
 

## Section 4.2 Reversible Semi-Markov Model for the Asthma Control data

- This analysis is presented [here](/Section4-2.md)



