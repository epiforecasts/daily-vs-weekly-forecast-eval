# Running the pipeline on the LSHTM HPC

Docs for the cluster: <https://hpcinfo.lshtm.ac.uk/index.html>. It runs SLURM on
Rocky Linux 8, with 14 compute nodes of 20–64 cores.

## What gets scheduled, and what doesn't

The expensive work is 30 independent units: 10 regions × {daily, weekly,
rescale}. Each is one `Rscript` invocation that loops over ~57 sliding windows,
refitting until `keep_running()` passes. Measured from a completed GP run
(`local/output/diagnostics_GP.csv`):

| type     | slides | total Stan time | slowest single fit |
| -------- | -----: | --------------: | -----------------: |
| daily    |     57 |          0.75 h |             34 min |
| weekly   |     57 |          1.29 h |             30 min |
| rescale  |     57 |            ~0 s |                1 s |

So roughly 1–4 h per unit, RSA being the slowest. That is one array job of 30
tasks, 4 cores each.

Everything else — data extraction, scoring, panel figures, the paper — takes
seconds to minutes and stays on the login node under plain `make`.

## Setup

### 1. Access

Request it through the [Service Desk](http://servicedesk.lshtm.ac.uk) using the
"Request LSHTM HPC" form, then:

```bash
ssh loginhpc.lshtm.ac.uk
```

The HPC sits behind the firewall, so you need to be on the school network, in
remote desktop, or coming through the Pryor SSH gateway.

### 2. Software environment

`renv.lock` pins **R 4.5.0**. The published module list only reaches `R/4.1.2`,
which is too old for EpiNow2 1.7.1 and scoringutils 2.1.0. Check what is
actually installed:

```bash
module avail R
```

If there is nothing ≥ 4.4, use the container route (the default in
`slurm/config.sh`). `rocker/r-ver` resolves packages from the Posit Package
Manager binary repo for Linux, so `renv::restore()` fetches a **precompiled**
EpiNow2 instead of spending ~40 minutes compiling its Stan models — which is
also the step most likely to be OOM-killed.

Clone the repo, then run the setup script on a compute node:

```bash
srun --pty --cpus-per-task=4 --mem=16G --time=02:00:00 bash
./slurm/setup_env.sh
```

That pulls the image (no root required) and restores the library into a shared
`RENV_PATHS_CACHE`, so the 30 array tasks share one copy rather than building
their own.

`cmdstanr` is not in `renv.lock`, so `R/pipeline_shared_inputs.R` falls back to
the `rstan` backend. That is the right choice here — no CmdStan bootstrap, and
EpiNow2 ships its models precompiled against rstan.

### 3. Point outputs at scratch

The cluster docs are explicit that storage is limited and not for archival. Set
`REFDIR` in `local.makefile` (gitignored, so it stays site-specific):

```make
REFDIR := /path/to/your/scratch/dvw
```

Scripts read this back via `make -s print-REFDIR` rather than keeping their own
copy, so there is one source of truth.

### 4. Prep on the login node

```bash
make hpc_prep
```

This writes `slurm/targets.txt` (the array manifest, generated from
`${PROVINCES}` so it cannot drift), creates `slurm/logs/`, and builds the data
extracts. The extracts step downloads from GitHub, so it has to happen
somewhere with outbound network access — compute nodes may not have it.

## Running

Test a single unit first:

```bash
sbatch --array=1-1 slurm/forecast_array.sh   # line 1 = "daily GP"
squeue -u "$USER"
```

Then the full set:

```bash
sbatch --array=1-$(wc -l < slurm/targets.txt)%10 slurm/forecast_array.sh
```

`%10` throttles to 10 concurrent tasks; the per-user cap is 40.

Each task calls `make` on its own target rather than `Rscript` directly, so the
job is idempotent — if something times out, resubmit and the completed targets
are skipped.

When the array finishes, back on the login node:

```bash
make all_scores
make all_panel_figs
```

## Things that will bite you

- **`--time` is a hard kill.** Default 1 h, maximum 168 h. Single fits reached
  34 minutes in the GP run and RSA carries larger counts, so the script asks for
  12 h rather than something tight.
- **`--mem` defaults to 1 GB per core**, nowhere near enough for four rstan
  chains. The script asks for 16 G.
- **Don't add `make -j`.** SLURM owns the concurrency; nesting the two
  oversubscribes the node.
- **Core counts.** `parallel::detectCores()` reports the whole node, not your
  allocation. `available_cores()` in `R/pipeline_shared_inputs.R` reads
  `SLURM_CPUS_PER_TASK` instead, and caps `data.table`'s thread pool to match.
  It is capped at 4 because there are only 4 chains.
- **`.Rprofile` is gitignored** but renv needs it; `install.R` regenerates it via
  `renv::activate()`. Don't be alarmed that a fresh clone lacks one.

## Troubleshooting

| Symptom | Likely cause |
| --- | --- |
| `Missing slurm/targets.txt` | `make hpc_prep` not run, or run from the wrong directory |
| Task exits immediately, empty log | `slurm/logs/` doesn't exist — SLURM can't create the log and kills the job |
| `renv` reports packages missing at runtime | `RENV_PATHS_CACHE` not visible from the compute node |
| Fits far slower than the table above | Oversubscription — check `SLURM_CPUS_PER_TASK` is reaching R |
| `install.R` OOM-killed | Compiling from source rather than using binaries; use the container |
