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

D7 (abstract/author-summary) and D8 (Discussion paragraph 3 and the
practitioner recommendations, including the now-inaccurate "expect 5–12
adaptive refits") are **not yet applied** — both require reframing the
"striking counterpoint" narrative rather than swapping numbers, and are still
open in `paper_plan.md` section D.

Full before/after numbers and the reasoning for each item are in
`paper_plan.md` section D.

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

Every artefact in `local/output/` predates this fix, so all quantitative results
in the manuscript are provisional. See `paper_plan.md` section D for the eight
claims blocked on regeneration.
