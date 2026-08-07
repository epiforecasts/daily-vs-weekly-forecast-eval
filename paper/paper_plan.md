# Paper plan

Working list of proposed changes to `paper/paper.qmd`, ordered by whether they
can be made now or must wait for the forecasting pipeline to be regenerated.

Companion to `paper_changes.md`, which logs changes already made. Record the
commit hash in the **Commit** column as each item lands.

## Background: why some items are blocked

Commit `b5848ef` (2026-04-11) fixed a latent bug in the adaptive fitting loop.
`keep_running()` tested `dgn$rhat`, but `get_stan_diagnostics()` only ever emits
`max_rhat`, so `dgn$rhat` was `NULL`, `NULL < 1.01` evaluated to `logical(0)`,
and that element vanished from the `c()`. `passingmcmc` was therefore a
**two**-element vector, and the loop stopped only when **both** divergences and
bulk ESS passed. Post-fix it is a three-element vector stopping on **any two of
three**.

Every artefact in `local/output/` is dated 25 Sep 2025 and so predates this fix.
Bulk ESS >= 400 is the criterion the weekly model struggles with — which is why
it ratchets most — and a fit passing divergences and $\hat{R}$ but failing ESS
now stops where it previously kept refitting. Expected consequences:

- weekly ratchet counts should **fall substantially**
- mean terminal `adapt_delta` should fall, shortening run times
- ESS/sec moves both ways at once (shorter runtimes raise it; accepting
  lower-ESS fits lowers the numerator) — **direction genuinely unknown**
- forecast draws shift via the `adapt_delta` path, so CRPS changes modestly
  rather than qualitatively

One knock-on to watch: the paper currently evidences "weekly is computationally
hardest" through ratchet counts. Post-fix that burden may relocate into *lower
achieved ESS* instead. The claim will probably survive; the quantity
demonstrating it may not.

## A. Applied

| # | Location | Change | Commit |
|---|---|---|---|
| A1 | `paper.qmd:269` | `$25/%$` was a malformed `25\%` that also read as 25% of the current value. `ratchet_control()` computes `adapt_delta + (1 - adapt_delta) * 0.25`, i.e. 25% of the remaining distance to 0.99. Restored the explicit wording. | `a799eac` |
| A2 | `paper.qmd:272` | "we continued only when **at most two** of three diagnostic criteria were met" contradicted `keep_running()`, which continues while `(passingmcmc < 2)`. Two passing criteria is a *stopping* condition. Changed to "fewer than two". | `a799eac` |
| A3 | `paper.qmd:207`, `232-235`, `468-472`, `550-552` | Removed inline function names from the prose in favour of describing what each does — the `epinow()` reference in Methods, and the three `fill_missing()` references in Methods and Discussion. Package names (EpiNow2, `cmdstanr`, `rstan`) retained, since they identify the software and carry the citations. Convention recorded in `CLAUDE.md` under "Manuscript writing style". | `93a2029` |

Both were regressions, not original errors: `430ec0e` (2026-04-11) fixed them,
and `51032ec` ("Reformatted paper in terms of line lengths") silently reverted
them while reflowing.

## B. Further regressions from `51032ec` — applied

A word-diff of `51032ec` against its parent shows it was a substantive rewrite,
not only a reflow. Most changes were improvements, but these were lost.

| # | Location | Change | Commit |
|---|---|---|---|
| B1 | `paper.qmd:262-265` | Text said "$4$ chains in parallel with $4$ cores". Code is `cores = min(parallel::detectCores() - 1, 4)`. Restored the prior accurate phrasing ("up to 4 cores, or one fewer than detected, whichever was smaller"). Reverts a regression of fix #2 in `paper_changes.md` (`4921e40`). | `89a58a7` |
| B2 | `paper.qmd:232-234` | The `fill_missing()` sentence had been deleted — "Before fitting, `fill_missing()` was called to insert any absent dates and accumulate observations, using an initial accumulation window of 1 day for daily data and 7 days for weekly data." Restored, reverting a regression of `5c15fbe`. See C1. | `89a58a7` |
| B3 | `paper.qmd:250-257` | The deleted note that the rescaled incubation period is *hardcoded* had been replaced by "the parameters of the incubation period and generation time were divided by 7", which is inaccurate. Corrected. See C2. | `89a58a7` |

## C. Methods/Discussion consistency gaps — applied

| # | Location | Change | Commit |
|---|---|---|---|
| C1 | `paper.qmd:460`, `541` vs Methods | The Discussion credits EpiNow2's `fill_missing()` for the paper's central finding (that weekly stays competitive), and recommends it to practitioners — but the mechanism was no longer described anywhere in Methods. Closed by B2. | `89a58a7` |
| C2 | `paper.qmd:250-257` vs `500` | Methods said the rescaled incubation period was "divided by 7"; Discussion limitation 3 says it was hardcoded as `LogNormal(mean = 5/7, sd = 1/7)`. The code (`R/pipeline_rescaled_weekly.R:32`) confirms the Discussion: it is a hardcoded approximation, *not* the epiparameter distribution scaled down. Methods corrected to match, and the generation-time rescaling (`7.12/7`, `1.72/7`, max `10/7`) stated explicitly. | `89a58a7` |
| C3 | `paper.qmd:262-265` | The same sentence omitted the reporting delay, which is also rescaled (`LogNormal(mean = 2/7, sd = 1/7, max = 10/7)`, `R/pipeline_rescaled_weekly.R:41`). Unlike the incubation period this genuinely *is* the day-scale distribution divided by 7. Added to the reporting-delay paragraph. | `89a58a7` |

Verified as correct and needing **no** change: "initial step size of $0.1$"
(`paper.qmd:260`) matches `control_opts$stepsize`; the bulk ESS >= 400 threshold
(`paper.qmd:281-282`) now matches `essmin = 400` after merging `8175fe0`.

## D. Blocked on pipeline regeneration

Do not finalise these until `local/output/` is rebuilt. Every entry is a
hardcoded range or a directional claim resting on one.

Line numbers below are current as of `89a58a7`.

| # | Location | Change | Exposure | Commit |
|---|---|---|---|---|
| D1 | `411`, `414-415`, `479-480`, `485` | ESS/sec bands (1--10 rescaled, 0.1--0.3 daily, 0.01--0.1 weekly) | High — numerator and denominator both move | |
| D2 | `415-419`, `478`, `553` | Ratchet counts (5--12 weekly, 0--2 daily) | **Highest** — `b5848ef` rewrites this loop directly | |
| D3 | `429-435`, `462-463` | CRPS ratios (1.5--3x, ~1x, 30--60x) | Moderate — ordering likely holds, bounds may not | |
| D4 | `393`, `401` | "one to two orders of magnitude" | Moderate | |
| D5 | `397` | Daily "slightly better during periods of rapidly changing incidence" | Moderate — fine-grained comparison | |
| D6 | `384-388`, `420`, `438-440` | "consistent across all nine provinces and RSA" robustness claims | Needs re-verification province by province | |
| D7 | `57`, `59` | Abstract and author summary directional claims | Low — likely hold, but confirm rather than assume | |
| D8 | `476-489` | Discussion paragraph 3 restates D1 and D2 throughout | Follows D1, D2 | |

## E. Safe to write out now (no pipeline dependency)

Recorded for completeness — these need no change on account of regeneration and
can be drafted or polished at any time.

- Introduction (`63-143`) in full
- Methods (`144-381`) in full — B1-B3 and C1-C3 are now applied
- All four figure captions (`153`, `405`, `423`, `443`) — they describe panels,
  axes, scales and colour mappings, never values
- Discussion: the mechanistic account of why pseudo-daily compression distorts
  the renewal dynamics and why `fill_missing()` preserves them (`464-472`); the
  two-stakeholder paragraph (`493-498`); resolution vs. public-health objectives
  (`500-504`); all four limitations (`506-520`); the related-work comparison to
  Nash, Ogi-Gittins and Steyn (`522-536`); the scoring-framework paragraph
  (`538-546`)
- Data and code availability (`371-381`), Acknowledgements, bibliography, and
  the pipeline schematic

## F. Optional: parameterise the Results numbers

Rather than re-hunting ~16 hardcoded ranges after the rerun, the ranges at the
D1-D4 lines could be replaced with inline R expressions reading
`local/output/score_*.rds` and `local/output/diagnostics_*.csv` at render time.
The Results prose would then re-derive itself from whatever the pipeline
produces, and the risk of a stale number surviving into submission drops to
zero. This couples `paper.qmd` to `local/`, which the figures already do, so it
widens an existing dependency rather than adding a new one.

| # | Change | Commit |
|---|---|---|
| F1 | Replace hardcoded ranges at D1-D4 with inline R expressions computed from `local/output/` | |
