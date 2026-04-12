# Paper changes log

Summary of changes made to `paper/paper.qmd` to align the manuscript
with the implemented analysis code.

## Changes made

### 1. Fix adapt-delta ratchet description (Methods §Forecasting)

**Commit:** `460351a`

Paper said the adapt-delta parameter was "increased by 25 % of the
previous value". The code (`ratchet_control()` in
`R/pipeline_shared_inputs.R`) increases by 25 % of the **remaining
distance to 0.99**, i.e. `adapt_delta + (1 - adapt_delta) * 0.25`.
Reworded to match.

### 2. Clarify sampling configuration (Methods §Forecasting)

**Commit:** `4924fe9`

Paper said "5,000 posterior samples or 1500 iterations per chain" and
a fixed "4 cores". The code sets `samples = 5000` (total across
chains) and `cores = min(parallel::detectCores() - 1, 4)`. Removed
the ambiguous per-chain phrasing and noted the adaptive core count.

### 3. Add missing-data accumulation detail (Methods §Forecasting)

**Commit:** `8c8655e`

The paper did not explain that `fill_missing()` is called before
fitting with different accumulation windows: `initial_accumulate = 1`
for daily data and `initial_accumulate = 7` for weekly data
(`R/pipeline_main.R`, lines 78–80). Added a sentence.

### 4. Expand rescaled-weekly date mechanics (Methods §Forecasting)

**Commit:** `2bfed17`

The original text was vague ("mapped to one step on a compressed
pseudo-daily time axis"). Expanded to describe:

- Replacing weekly calendar dates with consecutive daily dates
  (`R/pipeline_rescaled_weekly.R`, lines 65–72).
- Dividing all delay and generation-time parameters by 7
  (lines 29–38).
- Training window of 10 pseudo-days / forecast horizon of 2
  pseudo-days (matching `train_window_rescaled = 10` and
  `test_window_rescaled = 2` in `R/pipeline_shared_inputs.R`).
- Remapping forecast dates back to calendar weeks after fitting
  (lines 175–186).

### 5. Detail forecast–observation alignment for CRPS (Methods §Scoring)

**Commit:** `5733d56`

Paper said predictions are "accumulated between observed report
dates" but did not explain the mechanism. Added a sentence describing
the join → cumulative-sum → differencing procedure implemented in
`join_and_score()` (`R/pipeline_shared_inputs.R`, lines 34–48).

### 6. Remove erroneous scoringRules citation (Methods §Scoring)

**Commit:** `a1cd38b`

Paper cited both `scoringutils` and `scoringRules`. Only
`scoringutils` is imported (in `R/score.R`); `scoringRules` is not
used anywhere. Removed the `scoringRules` citation and reference.

### 7. Define elapsed fitting time (Methods §Scoring)

**Commit:** `81419ef`

Paper referred to "elapsed fitting time" without defining it. Added
that it is the maximum across chains of the combined warmup and
sampling time, summed over all ratchets within a slide (matching
`elapsed_time()` in `R/pipeline_shared_inputs.R` lines 127–132 and
the accumulation in `R/pipeline_main.R` line 118).

### 8. Clarify geometric-mean CRPS computation (Methods §Scoring)

**Commit:** `3d8eab7`

Paper said "computed the geometric mean relative to the model using
daily data" without specifying the formula or stratification.
Clarified that the geometric mean is computed as the exponentiated
mean of the log-ratios, stratified by province, forecast type, and
evaluation resolution (matching `R/fig_crps_summary_all_provs.R`
line 50). Also fixed typo ("a time of" → "a time series of").

### 9. Draft author summary (Frontmatter)

**Commit:** `966a0ae`

Replaced the placeholder "Author summary to be inserted" with a
summary describing the motivation, approach, key findings, and
practical relevance.

### 10. Draft Results narrative (Results)

**Commit:** `142de5c`

The Results section contained only figure environment blocks and no
accompanying text. Added three sub-sections with prose describing:

- **Forecast performance:** CRPS tracks incidence level; daily and
  weekly targets perform comparably; rescaled weekly consistently
  higher.
- **Computational diagnostics:** rescaled weekly has highest ESS/sec;
  weekly model has the lowest and requires the most ratchets.
- **Relative performance across provinces:** geometric-mean CRPS
  ratios cluster near 2–3x for weekly-vs-daily on daily test data;
  ~1x on weekly test data; ~30–60x for rescaled weekly.

### 11. Add model parameter values (Gap A)

Insert a new paragraph in Methods §Forecasting (after the
observation-model paragraph) listing the specific distributional
forms and values used for fitting:

- **Incubation period:** LogNormal from epiparameter, truncated at
  the 99.9th percentile (citing Linton et al. 2020 and the
  epiparameter package).
- **Generation time:** Gamma(mean = 7.12, sd = 1.72, max = 10),
  citing Manica et al. 2022 (Alpha variant estimate).
- **Reporting delay:** LogNormal(meanlog = 0.58, sdlog = 0.47,
  max = 10), corresponding to approximately mean 2, sd 1 day.
- **Rt prior:** LogNormal(meanlog = 0.69, sdlog = 0.05),
  corresponding to approximately mean 2, sd 0.1.

Code references: `R/pipeline_main.R` lines 33–55.

### 12. Note hardcoded rescaled-weekly incubation period (Gap B)

Add a sentence to the Data §rescaled-weekly paragraph noting that
the rescaled pipeline uses a fixed LogNormal(mean = 5/7, sd = 1/7,
max = 14/7) incubation period rather than querying epiparameter at
runtime. The underlying day-scale values (mean = 5, sd = 1, max = 14)
approximate but may not exactly match the epiparameter distribution.

Code reference: `R/pipeline_rescaled_weekly.R` line 29.

### 13. Mention non-default stepsize (Gap C)

Add "and an initial step size of 0.1" to the MCMC configuration
sentence in Methods §Forecasting. `stepsize = 0.1` is a non-default
Stan control option; the default is 1.

Code reference: `R/pipeline_shared_inputs.R` line 184.

### 14. Document slide skip condition (Gap D)

Add a sentence stating that slides where the training window
contained fewer non-zero observations than twice the forecast horizon
were skipped, producing no forecast for that window.

Code references: `R/pipeline_main.R` line 89,
`R/pipeline_rescaled_weekly.R` line 85.

### 15. Cite generation-time source (Gap F)

Add `@manica_estimation_2022` citation alongside the generation
time parameter values in the new parameter paragraph.

Code reference: `R/pipeline_main.R` line 45 (code comment).

### 16. Add bibliography entries

Add two new entries to `paper/bibliography.bib`:

- `linton_incubation_2020` — Linton et al. (2020), "Incubation
  Period and Other Epidemiological Characteristics of 2019 Novel
  Coronavirus Infections with Right Truncation", Journal of Clinical
  Medicine, doi:10.3390/jcm9020538.
- `manica_estimation_2022` — Manica et al. (2022), "Estimation of the
  incubation period and generation time of SARS-CoV-2 Alpha and Delta
  variants from contact tracing data", Epidemiology and Infection,
  doi:10.1017/S0950268822001947.

### 17. Add Results section prose templates

The Results section currently contains only figure environments with
no accompanying narrative. Add prose templates with `[TODO: …]`
placeholders under three subsections:

- **Forecast performance:** Introduce EC as a representative
  province, reference `@fig-panel-scores-EC`, and include
  placeholders describing how CRPS tracks incidence level, the
  relative performance of daily vs weekly forecasts against daily
  and weekly observations, and rescaled-weekly performance.
- **Computational diagnostics:** Reference
  `@fig-panel-diagnostics-EC` and include placeholders for ESS/sec
  ordering between model types, ratchet frequency by model type,
  and consistency of patterns across provinces.
- **Relative performance across provinces:** Reference
  `@fig-crps-all-provs-summarised` and include placeholders for
  geometric-mean CRPS ratios (weekly-vs-daily on daily obs,
  weekly-vs-daily on weekly obs, rescaled-weekly on weekly obs),
  province-to-province variability, and the position of the
  national aggregate (RSA).

### 18. Add Discussion section prose templates

The Discussion currently has four paragraphs covering trade-offs,
stakeholders, public health alignment, and the evaluation framework.
Insert `[TODO: …]` template paragraphs between the existing text
(preserving all current paragraphs) for the following topics:

- **After para 1** (trade-offs intro): Summarise the key
  forecast-performance finding — daily and weekly inputs yield
  comparable CRPS while the rescaled weekly approach performs
  substantially worse — with placeholders for specific ratios.
- **After the above:** Computational trade-offs — the weekly model
  requires the most adaptive refits and has the lowest sampling
  efficiency, while the rescaled weekly model achieves high ESS/sec
  but poor predictive performance.
- **After para 3** (public health alignment): Limitations — single
  forecasting framework (EpiNow2), single disease and geography,
  hardcoded incubation period in the rescaled pipeline, and
  reliance on reported case data subject to surveillance artefacts.
- **After limitations:** Related work — Nash et al. (2023),
  Ogi-Gittins et al. (2025), Steyn et al. (2025) — extending
  the literature by jointly assessing forecast scoring with
  convergence diagnostics.
- **After para 4** (evaluation framework): Practical guidance for
  practitioners choosing between daily and weekly data inputs,
  including the adaptive tuning workflow and the combined use of
  scoring rules with convergence diagnostics.

---

## Not addressed

### Gap E — `rhat` vs `max_rhat` column mismatch

Fixed in a separate PR on the `fix/max-rhat-naming` branch (changing
`dgn$rhat` to `dgn$max_rhat` in `keep_running()` in
`R/pipeline_shared_inputs.R`). This is a code bug, not a paper gap.
