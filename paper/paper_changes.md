# Paper changes log

History of changes to `paper/paper.qmd` (and its supporting files), in
chronological order. This file records what **has** been done; `paper_plan.md`
records what is **still to** be done, including the items blocked on
regenerating `local/output/`.

> **Note on commit hashes.** An earlier revision of this log recorded hashes
> from before the `paper-updates` branch was rebased. Those commits still exist
> as unreachable objects, so `git show` finds them, but they are not on any
> branch and will disappear at the next `gc`. All hashes below have been
> re-derived from the current branch and verified reachable from `HEAD`.

## Origin

| Commit | Date | Change |
|---|---|---|
| `ffa5659` | 2025-12-23 | Transferred manuscript content from the PDF into `paper.qmd` |
| `810b422` | 2025-12-23 | General update to `paper.qmd` |
| `6ff8b52` | 2025-12-23 | Converted the title to sentence case |

## Pipeline figure and narrative pass (Feb 2026)

| Commit | Date | Change |
|---|---|---|
| `07e27ad` | 2026-02-17 | Updated the Methods and Results narrative to reference the pipeline figure; added `analysis_pipeline.mermaid` |
| `a519746` | 2026-02-17 | Applied a one-line code-review suggestion |

## Code-alignment pass (11 Apr 2026)

Ten commits reconciling the manuscript with what the code actually does. These
were previously logged as entries 1–10 against pre-rebase hashes.

| # | Commit | Change |
|---|---|---|
| 1 | `430ec0e` | **Adapt-delta ratchet description.** Paper said adapt-delta was "increased by 25% of the previous value". `ratchet_control()` increases by 25% of the **remaining distance to 0.99**, i.e. `adapt_delta + (1 - adapt_delta) * 0.25`. Reworded to match. |
| 2 | `4921e40` | **Sampling configuration.** Paper said "5,000 posterior samples or 1500 iterations per chain" and a fixed "4 cores". Code sets `samples = 5000` (total across chains) and `cores = min(parallel::detectCores() - 1, 4)`. Removed the per-chain phrasing and noted the adaptive core count. |
| 3 | `5c15fbe` | **Missing-data accumulation.** Added that the series is completed before fitting with different accumulation windows: 1 day for daily data, 7 days for weekly (`R/pipeline_main.R`). |
| 4 | `5ef5f8f` | **Rescaled-weekly date mechanics.** Expanded the vague "mapped to one step on a compressed pseudo-daily time axis" to describe replacing weekly dates with consecutive daily dates, dividing delay and generation-time parameters by 7, the 10/2 pseudo-day windows, and remapping forecast dates back to calendar weeks. |
| 5 | `c94f8aa` | **CRPS alignment procedure.** Described the join, cumulative-sum and differencing steps implemented in `join_and_score()`, replacing the unexplained "accumulated between observed report dates". |
| 6 | `8c58575` | **Removed erroneous scoringRules citation.** Only `scoringutils` is imported (`R/score.R`); `scoringRules` is used nowhere. Citation and reference removed. |
| 7 | `37bb643` | **Defined elapsed fitting time** as the maximum across chains of combined warmup and sampling time, summed over all ratchets within a slide. |
| 8 | `b891cc2` | **Geometric-mean CRPS computation.** Specified it as the exponentiated mean of the log-ratios, stratified by province, forecast type and evaluation resolution. Also fixed "a time of" → "a time series of". |
| 9 | `a7d5601` | **Author summary drafted**, replacing the "to be inserted" placeholder. |
| 10 | `82cd60f` | **Results narrative drafted** for all three figures — forecast performance, computational diagnostics, and relative performance across provinces. |

## Supplementary materials and prose completion (12 Apr 2026)

| Commit | Date | Change |
|---|---|---|
| `62ccef4` | 2026-04-12 | Large combined commit (details below) |
| `84fbddd` | 2026-04-12 | Fixed an author affiliation; extended `supplementary.qmd` |

`62ccef4` implemented, in a single commit, everything the previous revision of
this log listed as unfinished proposals in entries 11–16:

- **Model parameter values** added to Methods §Forecasting — incubation period
  (LogNormal from epiparameter, truncated at the 99.9th percentile), generation
  time (Gamma, mean 7.12, sd 1.72, max 10), reporting delay (LogNormal,
  meanlog 0.58, sdlog 0.47, max 10), and the $R_t$ prior (LogNormal,
  meanlog 0.69, sdlog 0.05).
- **Hardcoded rescaled incubation period** noted — a fixed
  LogNormal(mean = 5/7, sd = 1/7, max = 14/7) rather than a runtime epiparameter
  query. *Later deleted by `51032ec` and restored by `89a58a7`; see below.*
- **Non-default step size** of 0.1 documented (Stan's default is 1).
- **Slide skip condition** documented — slides whose training window held fewer
  non-zero observations than twice the forecast horizon were skipped.
- **Generation-time citation** `@manica_estimation_2022` added.
- **Bibliography entries** `linton_incubation_2020` and `manica_estimation_2022`
  added to `bibliography.bib`.

It also created `paper/supplementary.qmd`, added the `supplementary` and
`manuscripts` Make targets, and rendered both PDFs.

Two further entries in the previous revision of this log (17 and 18) proposed
adding `[TODO: …]` prose templates to the Results and Discussion. They are
**obsolete**: `82cd60f` and `62ccef4` wrote the actual prose, and no TODO
placeholders remain in `paper.qmd`.

## Regression and recovery (Aug 2026)

| Commit | Date | Change |
|---|---|---|
| `51032ec` | 2026-08-06 | **Labelled "Reformatted paper in terms of line lengths", but was a substantive rewrite** (+449/-81 lines) |
| `a799eac` | 2026-08-07 | Restored adapt-delta and stopping-criterion wording |
| `89a58a7` | 2026-08-07 | Restored Methods detail lost in the reflow |
| `93a2029` | 2026-08-07 | Replaced function names in the prose with descriptions of what they do |

Most of `51032ec` was genuine improvement — expanded weekly-accumulation and
rescaled-weekly mechanics, the three-way summary of what each data input
represents, and the slide skip condition moved to a better location. But a
word-diff against its parent shows it silently reverted or deleted five things:

1. **Adapt-delta wording** (undoing change 1): "25% of the remaining distance to
   0.99" became the malformed `$25/%$`, which also reads as 25% of the current
   value. Restored in `a799eac`.
2. **Stopping criterion**: "fewer than two of three diagnostic criteria" became
   "at most two". `keep_running()` continues while `(passingmcmc < 2)`, so two
   passing criteria is a *stopping* condition. Restored in `a799eac`.
3. **Core count** (undoing change 2): reverted to a flat "4 cores". Restored in
   `89a58a7`.
4. **Missing-data accumulation sentence** (undoing change 3): deleted outright.
   The Discussion still credited this mechanism for the paper's central finding
   and recommended it to practitioners, leaving it undescribed in Methods.
   Restored in `89a58a7`.
5. **Hardcoded rescaled incubation period**: deleted and replaced with the
   inaccurate claim that the incubation period was "divided by 7".
   `R/pipeline_rescaled_weekly.R:32` hardcodes an approximation instead, which
   is what Discussion limitation 3 already said. Corrected in `89a58a7`, which
   also documented the rescaled reporting delay (genuinely divided by 7).

**Lesson:** a commit labelled as formatting changed the meaning of three Methods
claims. Word-diff any future reflow commit (`git diff -w --word-diff=plain`)
before trusting the label.

`93a2029` then removed inline function references from the prose in favour of
describing what each function does, per the manuscript writing style now
recorded in `CLAUDE.md`. Package names are still cited.

## Pipeline regeneration and Results rewrite (Aug 2026)

| Commit | Date | Change |
|---|---|---|
| `901a369` | 2026-08-17 | Updated Results (Forecast performance, Computational diagnostics, Relative performance) to match `local/output/` regenerated post-`b5848ef`; see below |

`local/output/` was rebuilt in full on 2026-08-17 — all 10 provinces × 5
output types — the first full regeneration since the `keep_running()`
rhat/max_rhat fix (`b5848ef`, 2026-04-11). This unblocked the eight items
tracked in `paper_plan.md` section D. `901a369` applied the six
(D1–D6) that were mechanical or well-supported by the fresh numbers, plus one
adjacent factual correction found while recomputing the same table (D9: RSA is
the worst-performing province in all three CRPS scenarios, not central, as the
old text claimed):

- **ESS/sec bands** (D1) rose roughly 2–3 orders of magnitude across every
  configuration (faster fits post-fix). Daily and weekly are now similar
  order of magnitude and overlap, rather than weekly being clearly lowest —
  weekly actually has *higher* median ESS/sec than daily in 6 of 10
  provinces.
- **Ratchet counts** (D2) dropped from "typically 5–12 per slide for weekly"
  to a tail phenomenon: median is 0 refits for every configuration, with
  weekly needing at least one refit in ~20% of slides (daily and rescaled
  weekly both under 6%).
- **CRPS ratios** (D3) recomputed from fresh scores. The weekly-trained model
  evaluated at weekly resolution is now consistently >1× (1.2–1.7×, median
  1.4×) across all 10 provinces, rather than "close to or below 1×" as
  previously claimed.
- **Order-of-magnitude claim** (D4) confirmed against `fig_panel_scores_EC.png`,
  with the largest wave (Dec 2021–Jan 2022) noted as reaching three orders of
  magnitude rather than the general one-to-two.
- **"Daily slightly better during rapid change"** (D5) softened: daily is
  modestly better on ~70% of dates across provinces, but the margin is not
  consistently larger during rapid-change periods (larger in 6/10 provinces,
  smaller in 4/10).
- **Cross-province consistency** (D6) split into what held (CRPS ranking,
  ratchet-frequency ordering) and what didn't (daily-vs-weekly ESS/sec
  ordering, which flips in 4/10 provinces).

| `5a25800` | 2026-08-17 | Reframed the abstract, author summary, and Discussion's computational narrative (D7, D8) to match D1-D3; see below |

D7 (abstract/author-summary) and D8 (Discussion paragraph 3 and the
practitioner recommendations, including the now-inaccurate "expect 5–12
adaptive refits") required reframing the "striking counterpoint" narrative
rather than swapping numbers, since D1/D2 had already shown that "weekly is
consistently worst on every computational axis" no longer held:

- **Abstract/author summary** (D7): "comparable forecast performance"
  replaced with "modestly but consistently worse"; "lower computational
  efficiency" replaced with a refit-frequency framing, since raw ESS/sec no
  longer orders consistently between daily and weekly.
- **Discussion opening paragraph**: reframed, and in doing so fixed a
  pre-existing internal contradiction unrelated to the pipeline rerun — it
  had attributed "higher computational costs and diagnostic instability" to
  *daily* data, backwards from every other claim in the paper, where weekly
  is the harder-to-converge configuration.
- **Central-finding paragraph**: synced to the Results section's updated CRPS
  ratios (D3) instead of restating the old "near 1×" / "1.5–3×" / "30–60×"
  figures.
- **Computational paragraph** (the core of D8): replaced the "striking
  counterpoint" framing (weekly = worst efficiency + most ratchets) with the
  actual mechanism — weekly's steady-state sampling efficiency is
  statistically indistinguishable from daily's (higher median than daily in
  6/10 provinces), but it fails to converge cleanly in about 1 in 5 windows
  versus under 6% for daily and rescaled weekly, so the real computational
  cost is refit frequency, not typical sampling speed.
- **Recommendations paragraph**: dropped the now-false "expect 5–12 adaptive
  refits per forecast window"; replaced "best combination of accuracy and
  sampling efficiency" for daily (misleading given rescaled weekly's far
  higher raw ESS/sec) with "best combination of accuracy and reliable
  convergence."

Full before/after numbers and the reasoning for each item are in
`paper_plan.md` section D.

## Result numbers derived from the pipeline (20 Aug 2026)

The Results and Discussion no longer contain hand-written result numbers. See
`paper_plan.md` section F for the design and the rationale.

| Commit | Change |
|---|---|
| `046ee6e` | Lifted `read_scores()`, `crps_ratio_summary()` and `pop_order` into a new `R/summary_utils.R`, so the summary figure and the quoted ratios come from one computation. Folded the adaptive refit counts, slide dates and run times into `diagnostics_%.csv`, and repointed `R/fig_panel_diagnostics.R` at it. Verified the summary figure and all ten diagnostics panels are byte-identical afterwards. |
| `9b10ee8` | Added `R/summarise_results.R`, which reduces the scores and diagnostics to the 32 values the manuscript quotes and evaluates the nine directional claims the prose depends on, stopping before it writes anything if one fails. Committed `local/output/paper_summary.rds` via a `.gitignore` negation. |
| `bd02d74` | Replaced the 34 hand-written values in the Results and Discussion with inline references to that summary. |
| `de2faf7` | Gave the render workflow an R installation, without which the manuscript can no longer be built in CI. |

Three sentences changed shape rather than just numbers:

- **CRPS spread** (Results, forecast performance): "scores rise by one to two
  orders of magnitude" had no single derivable value behind it, and is now the
  10th-to-90th-percentile span. The "up to three orders of magnitude during the
  largest wave" clause stays prose — it describes a named date window in a
  figure — and is guarded by claim C9 instead.
- **Weekly's sampling-efficiency excursions**: "occasional excursions below 5"
  was also true of the daily and rescaled weekly models, so it drew no
  contrast. Replaced with weekly's actual minimum, which is three orders of
  magnitude below either of theirs.
- **Refit shares**: the daily and rescaled weekly models were given a shared
  "under 6%" bound; they are now stated separately.

The rounded hedges were dropped throughout in favour of exact values — "around
one-fifth of slides" became "20.4% of slides", "around 70% across provinces"
became "70.6% of dates". The hedges existed because the numbers were
hand-copied and might drift.

The abstract and author-summary keep hand-written prose, since Quarto does not
execute inline code in YAML front matter. Both are qualitative, and the claim
assertions fail the build if the direction they describe ever flips.

## Resolved elsewhere

### `rhat` vs `max_rhat` column mismatch

Previously listed here as "not addressed", pending a PR on the
`fix/max-rhat-naming` branch. That work **has landed** as `b5848ef`
(2026-04-11), reachable from `HEAD`.

This was more consequential than a naming tidy-up. `keep_running()` tested
`dgn$rhat`, but `get_stan_diagnostics()` only emits `max_rhat`, so `dgn$rhat`
was `NULL`, `NULL < 1.01` evaluated to `logical(0)`, and the element vanished
from the `c()`. The accept rule therefore ran on a **two**-element vector,
stopping only when both divergences and bulk ESS passed, instead of on any two
of three.

`local/output/` was regenerated in full on 2026-08-17, after this fix, and the
manuscript was brought into line with the post-fix numbers at `901a369` and
`5a25800`. See `paper_plan.md` section D for what each of those items changed;
the numbers are now re-derived on every render rather than transcribed.
