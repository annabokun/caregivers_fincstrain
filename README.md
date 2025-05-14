# Financial Strain Among Co-Residing Family Caregivers of the Oldest Old

This repository contains the R scripts, data structure, and output used in a study examining whether adult children who co-reside with their oldest-old parents (aged 80+) experience heightened financial strain. The study focuses on Mexican American caregivers and uses data from the **Hispanic Established Populations for the Epidemiologic Study of the Elderly (HEPESE, Wave 7, 2010-11)**.

## 🔍 Research Questions

1. Does co-residence increase financial strain among adult child caregivers?
2. Does the relationship between co-residence and financial strain vary by household headship?

## 📦 Project Structure

## 🧠 Methods

- **Dataset**: HEPESE Wave 7 (2010–2011), Mexican American adults aged 80+ and their caregivers.
- **Sample**: N = 659 adult child caregivers.
- **Modeling**: Binary logistic regression and propensity score matching (PSM).
- **Key outcome**: Financial strain (binary), based on self-reported financial difficulties.
- **Key predictors**: Co-residence, caregiver headship status, caregiver demographics, and parental health.
- **Robustness**: PSM conducted using 1:1 nearest neighbor matching with a caliper of 0.2 SD, and covariate balance was verified.

## 📊 Outputs

- **Tables**: Descriptive statistics, regression outputs, PSM covariate balance.
- **Figures**: Covariate balance plots, forest plots from regression models for presentations.
- **Code**: Cleaning scripts, modeling pipeline, and visualization.

## 📌 Highlights

- Co-residence **doubles** the odds of financial strain among adult child caregivers.
- These findings are robust to PSM approaches and suggest the need for policy interventions like paid leave and caregiver tax credits.

## 🔧 R Packages Used

- `tidyverse`
- `MatchIt`
- `mice`
- `ggplot2`
- `broom`
- `gtsummary`
- `tableone`

## 📄 Citation

If referencing this work, please cite:

> Bokun, A. (2025). *Financial Strain Among Co-Residing Family Caregivers of the Oldest Old*. Working paper.

## 📝 Author

**Anna Bokun**  
Sociologist & Demographer  
Postdoctoral Researcher
Population Research Center, University of Texas at Austin

## 📬 Contact

Feel free to reach out via [GitHub](https://github.com/annabokun) or [email](mailto:anna.bokun@austin.utexas.edu) for questions or collaborations.

---


