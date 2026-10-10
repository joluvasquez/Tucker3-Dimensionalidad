# Tucker3: version comparisons and controlled analyses

This directory provides code, archived candidate fits, numerical results and reproducibility checks for DIFFIT and automated multiway scree selection. The comparisons use the **same 5,400 simulated arrays and 199,800 fitted candidates** (108 scenarios, 50 replications per scenario, 37 candidates per array).

## Download and reproduce

1. Download [Tucker3_reproducibility.zip](Tucker3_reproducibility.zip) and extract it.
2. Set R's working directory to the extracted folder containing reproduce.R and data/.
3. Run the commands below. The scripts displayed separately here are copies for inspection; run the copies inside the extracted package, where their inputs are available.

~~~r
source("reproduce.R", encoding="UTF-8")
# Install once if needed:
install.packages("openxlsx")
source("verify_controls_and_preprocessing.R", encoding="UTF-8")
source("controlled_comparisons.R", encoding="UTF-8")
~~~

These commands reuse archived fits; they do not rerun ALS. The first script requires only base R. The other two require openxlsx. Outputs are regenerated in verification/ and controlled_outputs/; existing outputs there are overwritten, but source data remain unchanged. A fresh full simulation is a separate, computationally expensive operation described in the package README.

## Files

| File | Purpose |
| --- | --- |
| [Tucker3_reproducibility.zip](Tucker3_reproducibility.zip) | Complete inputs, V1/V2 source programs, verification scripts, results and SHA-256 manifest |
| [Comparaciones_controladas_Tucker3.xlsx](Comparaciones_controladas_Tucker3.xlsx) | Excel results: summaries, contrasts, factors, dimensions, replicate selections, curves, variants and audit |
| [controlled_comparisons.R](controlled_comparisons.R) | Recompute controlled comparisons and export Excel from archived candidate fits |
| [reproduce.R](reproduce.R) | Independently recalculate D1, D2, S1, S2 and paired Monte Carlo summaries |
| [verify_controls_and_preprocessing.R](verify_controls_and_preprocessing.R) | Verify controlled selections, generated ranks/noise and empirical preprocessing |
| [summary_global.csv](summary_global.csv), [paired.csv](paired.csv) | Version-comparison summaries and paired contrasts |
| [controlled_summary.csv](controlled_summary.csv), [controlled_contrasts.csv](controlled_contrasts.csv) | Controlled-analysis summaries and contrasts |
| [empirical_selections.csv](empirical_selections.csv) | Selected empirical triplets and FIT |
| [SHA256SUMS.txt](SHA256SUMS.txt) | Integrity checks for the distributed files |

## Meaning of V1 and V2

V1 denotes the authors' initial implementation, not a claim of exact equivalence to an original publication. V2 denotes the alternative implemented rules. Neither label implies better empirical performance.

| Label | Rule |
| --- | --- |
| D1 (DIFFIT V1) | Excludes the initial gain relative to zero; ranks gain ratios against the largest subsequent gain, without a gain threshold |
| D2 (DIFFIT V2) | Includes the initial gain, uses sequential right-hand maxima and applies the threshold 100/(smax - 3) |
| S1 (scree V1) | Maximizes the absolute second difference of FIT on the ordered complexity curve |
| S2 (scree V2) | Maximizes the absolute second divided difference, accounting for unequal spacing in S = P + Q + R |

Both scree implementations are automated and exclude the curve endpoints. They do not evaluate human visual scree judgments. Complexity S counts mode components, not free parameters. Full numerical conventions and tie-breaking are implemented in the supplied scripts. CHull is not included.

## Version-comparison results

| Selector | Exact recovery (%) | Mean selected S | Mean FIT (%) |
| --- | ---: | ---: | ---: |
| D1 | 66.89 | 6.919 | 63.80 |
| D2 | 59.67 | 6.285 | 62.17 |
| S1 | 32.72 | 5.821 | 58.39 |
| S2 | 43.48 | 6.152 | 60.07 |

Exact recovery requires the complete triplet (P,Q,R), not only its sum. D2 minus D1 is -7.22 percentage points (95% Monte Carlo interval [-7.89, -6.55]); S2 minus S1 is +10.76 points [10.00, 11.51]. These are paired differences on the same arrays, with Monte Carlo standard errors stratified by scenario. Intervals are pointwise and unadjusted for multiple comparisons. Global summaries weight clustered scenarios 75% and random scenarios 25%.

## Controlled comparisons

Eight DIFFIT configurations cross inclusion/exclusion of the initial gain, threshold on/off, and all ratios/sequential-maxima ratios, while holding candidate fits and common numerical conventions fixed. Two scree configurations isolate the effect of complexity spacing. The Excel Metodo sheet describes every convention.

For DIFFIT, adding the initial gain without the threshold reduces recovery from 66.89% to 60.57%; adding the threshold with the initial gain retained reduces it to 59.67%. All-ratio and sequential-maxima variants select identical triplets in this experiment. The threshold without the initial gain leaves 27 arrays without a selection: these count as failures for recovery, while means of selected quantities exclude undefined values. Missingness must therefore be considered when comparing those means.

The controls diagnose implementation sensitivity within the existing experiment. They do not establish universal superiority or provide independent data on which to tune the rules.

## Verification and provenance

The complete run is dated 15 September 2026. Recalculation checks all 21,600 V1/V2 selections and 43,200 controlled DIFFIT selections against archived records. Verification logs are included in the package. Reproducing selections from stored fits does not certify bitwise reproducibility of a fresh stochastic ALS run.

The empirical dataset (24 territories, 12 activities, 2007–2024; thousands of soles at constant 2007 prices) and earlier empirical code are deposited at [Figshare, DOI 10.6084/m9.figshare.33440566](https://doi.org/10.6084/m9.figshare.33440566). Source: INEI SIRTOD. The Figshare dataset is labelled CC BY 4.0; cite the deposit and source when reusing it. This update does not assign an additional software license.

No manuscript or editorial correspondence is included.
