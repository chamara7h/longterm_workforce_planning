# A hybrid predictive and prescriptive modelling framework for long-term mental healthcare workforce planning

## Authors

- **Harsha Chamara Hewage**  
  *Email:* [HalgamuweHewageHR@cardiff.ac.uk](mailto:HalgamuweHewageHR@cardiff.ac.uk)  
  *Affiliation:* Cardiff University, Data Lab for Social Good Group, Cardiff Business School, United Kingdom, CF10 3EU  
  *Corresponding Author*

- **Bahman Rostami-Tabar**  
  *Email:* [rostami-tabarb@cardiff.ac.uk](mailto:rostami-tabarb@cardiff.ac.uk)  
  *Affiliation:* Cardiff University, Data Lab for Social Good Group, Cardiff Business School, United Kingdom, CF10 3EU  
  
## Abstract

Over the past decade, severe staffing shortages in mental healthcare have worsened due to rising demand, further exacerbated by COVID-19. This demand is expected to grow over the next decade, necessitating proactive workforce planning to ensure sustainable service delivery. Despite its critical importance, the literature lacks a comprehensive model to address long-term workforce needs in mental healthcare. Additionally, our discussions with UK NHS mental health practitioners highlight the practical need for such a model. To bridge this gap, we propose a hybrid predictive-prescriptive modelling framework that integrates long-term probabilistic forecasting with an analytical stock-flow model for mental health workforce planning. Given the pivotal role of nurses, who comprise one-third of the mental health workforce, we focus on forecasting nursing headcount while ensuring the model’s adaptability to broader healthcare workforce planning. Using statistical and machine learning methods with real-world NHS data, we first identify key factors influencing workforce variations, develop a long-term forecasting model, and integrate it into an analytical stock-flow framework for policy analysis. Our findings reveal the unsustainable trajectory of current staffing plans and highlight the ineffectiveness of blanket policies, emphasizing the need for region-specific workforce strategies.

## Keywords

- OR in health service
- Forecasting
- Workforce planning
- Mental health
- Machine learning

## Repository Structure

- **data/**  
  This folder contains all the data used in the project, including NHS Wales, NHS England mental healthcare datasets, and all the processed (tidy) datasets.

- **scripts/**  
  This folder includes all scripts used in the study, organized into two main subfolders:
  - **Wales/** - Scripts for cleaning, preparing data and forecasting models.
  - **England/** - Scripts for cleaning, preparing data, forecasting models, analysing and evaluating the forecasts and stock-flow simulation.

- **system_dynamics/**  
  Contains the stock-flow data and AnyLoigc project files.

## How to Use

1. Clone the repository:
   ```sh
   git clone <repo-url>
   cd <repo-name>
   ```
2. Ensure all dependencies are installed.
3. Use scripts in `scripts/` for data preprocessing, forecasting, and analysis.

## Contact
For any inquiries, please contact the corresponding author: Harsha Chamara Hewage ([HalgamuweHewageHR@cardiff.ac.uk](mailto:HalgamuweHewageHR@cardiff.ac.uk)).

