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

Every artefact in `local/output/` was dated 25 Sep 2025 and so predated this
fix, until it was fully regenerated on 2026-08-17 (see section D). Bulk ESS >=
400 was the criterion the weekly model struggled with — which is why it
ratcheted most — and a fit passing divergences and $\hat{R}$ but failing ESS
now stops where it previously kept refitting. This section originally listed
expected consequences before the rerun; each is resolved below against the
actual post-fix numbers:

- weekly ratchet counts should **fall substantially** — confirmed, and further
  than expected: median ratchets is now 0 for *every* configuration, not just
  weekly (see D2).
- mean terminal `adapt_delta` should fall, shortening run times — confirmed;
  this is why ESS/sec rose by ~2-3 orders of magnitude across the board (D1).
- ESS/sec moves both ways at once, **direction genuinely unknown** — resolved:
  it rose for all three configurations, but the daily-vs-weekly *ordering*
  stopped being consistent across provinces (D1, D6).
- forecast draws shift via the `adapt_delta` path, so CRPS changes modestly
  rather than qualitatively — confirmed; CRPS ratios moved but stayed in a
  similar range (D3), except the weekly-vs-daily-at-weekly-resolution
  comparison flipped from "~1x" to "consistently >1x" (D3).

The anticipated knock-on **did** materialise: "weekly is computationally
hardest" no longer holds through ESS/sec (D1, D6) and now rests almost
entirely on refit *frequency* being higher, not on refits being typical (D2).

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

## D. Pipeline regenerated — Results/Discussion rewrite (applied)

`local/output/` was rebuilt in full on 2026-08-17 (all 10 provinces × 5 output
types, verified by file timestamp), well after `b5848ef` landed, so the block
below no longer applies: every quantity is now recomputed from post-fix
output. Figures were regenerated and committed at `e2441b4`. Numbers below
were computed directly from `local/output/diagnostics_*.csv`,
`local/output/forecast_*.rds` (`$timing`), and `local/output/score_*.rds`
across all 10 provinces, and cross-checked against `fig_panel_scores_EC.png`,
`fig_panel_diagnostics_EC.png`, and `fig_crps_summary_all_provs.png`.

**All nine items (D1–D9) are now applied** — D1–D6 and D9 at `901a369`, D7–D8
at `5a25800`. Location references below predate the edits; see
`paper_changes.md` for the chronological summary of what changed.

**Three of these were narrative reversals, not just tighter bounds** — flagged
below with ⚠. They changed what the paper argues, not just the numbers it
cites:

- ⚠ **Ratchets are now rare, not typical.** Median refits is 0 for every
  configuration. Only a tail of slides refit at all: daily in 3.7% of slides
  (max 1), rescaled weekly in 5.4% (max 11), weekly in 20.4% (max 11). "Weekly
  typically needs 5–12 ratchets per slide" no longer holds for any province.
- ⚠ **Daily vs. weekly ESS/sec ordering no longer holds consistently.** Weekly
  has *higher* median ESS/sec than daily in 6/10 provinces (EC, FS, GP, KZN,
  WC, RSA) and lower in the other 4. Both are now similar order of magnitude
  (tens–hundreds/sec) and overlap. Rescaled weekly remains unambiguously
  highest in every province.
- ⚠ **Weekly-trained forecasts evaluated at weekly resolution are no longer
  "comparable to or better than" daily.** All 10 provinces now show geometric
  mean CRPS ratio > 1 (range 1.23×–1.70×, median 1.41×) — consistently worse,
  not "near 1×."

| # | Location | Current text says | Proposed replacement | Exposure | Commit |
|---|---|---|---|---|---|
| D1 | `411-420` | ESS/sec bands (1--10 rescaled, 0.1--0.3 daily, 0.01--0.1 weekly, weekly lowest) | "The rescaled weekly model achieves the highest sampling efficiency, with tail ESS per second typically in the range of 400–5000 (interquartile range 2700–4100)... The daily and weekly models show broadly similar efficiency — daily typically 40–270 ESS per second (IQR 98–180), weekly typically 15–380 (IQR 57–210) — with weekly showing wider variability, including occasional excursions below 5, rather than a consistently lower level." | High ⚠ narrative reversal | `901a369` |
| D2 | `415-419` | Ratchet counts (5--12 weekly, 0--2 daily, rescale "in between") | "For every configuration, the majority of slides converge without any adaptive refit. Weekly required at least one refit in around one-fifth of slides (up to 11 in the most difficult cases), compared with under 6% of slides for both daily and rescaled weekly (maximum 1 and 11 respectively). Refitting is therefore a tail phenomenon rather than a routine cost, though weekly is disproportionately represented in that tail." | **Highest** ⚠ narrative reversal | `901a369` |
| D3 | `429-437` | CRPS ratios (1.5--3x, ~1x, 30--60x) | "weekly-trained model produces CRPS 1.3–1.9 times higher than the daily-trained baseline (median 1.5×)" @ daily eval; "1.2–1.7 times higher (median 1.4×) — consistently worse rather than comparable" @ weekly eval; "rescaled weekly... approximately 61–97 times higher (median 79×)" @ weekly eval | Moderate ⚠ narrative reversal (weekly-at-weekly case) | `901a369` |
| D4 | `393-394` | "one to two orders of magnitude" | Confirmed against `fig_panel_scores_EC.png`; append "— up to three orders of magnitude during the largest wave (December 2021–January 2022)" | Low — mostly holds | `901a369` |
| D5 | `397-399` | Daily "slightly better during periods of rapidly changing incidence" | "forecasts trained on daily data produce lower CRPS than those trained on weekly data on the majority of dates (around 70% across provinces), though this advantage is not consistently concentrated in periods of rapidly changing incidence" (effect confirmed but not concentrated as claimed: larger during rapid change in 6/10 provinces, smaller in 4/10) | Moderate — softened, not reversed | `901a369` |
| D6 | `384-388`, `420-422`, `438-440` | "consistent across all nine provinces and RSA" (blanket) | Split: forecast-accuracy ranking is consistent across all 10 (confirmed via `fig_crps_summary_all_provs.png`, kept as-is at 438-440); ratchet-frequency ordering (weekly highest) held in all 10; **ESS/sec ordering between daily and weekly did not order consistently** (see D1) — reworded at 420-422 accordingly | High ⚠ split finding | `901a369` |
| D9 | `440-442` | "RSA falls near the centre of the provincial distribution in all three scenarios" | Same recomputation used for D3 shows RSA is the maximum (worst-performing) province in all three CRPS scenarios, not central. Reworded to "falls at the upper, worst-performing end ... rather than the centre." Found and fixed alongside D3/D6 since it draws on the same table. | Low — factual correction, no reversal | `901a369` |
| D7 | `57`, `59` | Abstract/author-summary: "comparable forecast performance," weekly "lower computational efficiency" | Applied: "comparable" replaced with "modestly but consistently worse"; "lower computational efficiency" replaced with refit-frequency framing ("required more frequent adaptive model refitting... rather than uniformly lower raw sampling efficiency") | Low exposure, high visibility (abstract) | `5a25800` |
| D8 | Discussion opening paragraph, central-finding paragraph, computational paragraph, recommendations paragraph (all in `paper.qmd:457-583` as of `901a369`) | Restated D1/D2/D3's old numbers and the "striking counterpoint" framing throughout; opening paragraph also had a pre-existing, unrelated internal contradiction (attributed "higher computational costs and diagnostic instability" to *daily*, backwards from the rest of the paper) | Applied: computational paragraph reframed around the actual mechanism (weekly's steady-state ESS/sec is statistically indistinguishable from daily's; the real cost is refit frequency — ~1 in 5 windows vs <6% for daily/rescaled weekly); central-finding paragraph synced to D3's updated ratios; opening-paragraph contradiction fixed; recommendations paragraph drops the false "expect 5-12 adaptive refits" and softens "best combination of accuracy and sampling efficiency" for daily (misleading given rescaled weekly's far higher raw ESS/sec) to "best combination of accuracy and reliable convergence" | Follows D1, D2, D3 (all applied) | `5a25800` |

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
