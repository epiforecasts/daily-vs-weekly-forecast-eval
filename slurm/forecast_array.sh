#!/bin/bash
#
# Forecast array job: one task per (scale, province) pair.
#
# Prerequisites (run once, on the login node):
#   make hpc_prep
#
# Submit:
#   sbatch --array=1-$(wc -l < slurm/targets.txt)%10 slurm/forecast_array.sh
#
# The --array on the command line overrides the directive below, so the job
# stays correct if ${PROVINCES} changes. See README_HPC.md.

#SBATCH --job-name=dvw_forecast
#SBATCH --array=1-30%10
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=16G
#SBATCH --time=12:00:00
#SBATCH --output=slurm/logs/forecast_%A_%a.out
#SBATCH --error=slurm/logs/forecast_%A_%a.err
#SBATCH --mail-type=END,FAIL,ARRAY_TASKS
#SBATCH --mail-user=james.azam@lshtm.ac.uk

set -euo pipefail

PROJECT_ROOT="${SLURM_SUBMIT_DIR:-$(pwd)}"
cd "${PROJECT_ROOT}"

# shellcheck source=slurm/config.sh
source slurm/config.sh

MANIFEST="slurm/targets.txt"
if [ ! -s "${MANIFEST}" ]; then
  echo "Missing ${MANIFEST}. Run 'make hpc_prep' first." >&2
  exit 1
fi

TASK_ID="${SLURM_ARRAY_TASK_ID:-1}"
line="$(sed -n "${TASK_ID}p" "${MANIFEST}")"
if [ -z "${line}" ]; then
  echo "No target at line ${TASK_ID} of ${MANIFEST} ($(wc -l < "${MANIFEST}") lines)." >&2
  exit 1
fi

read -r SCALE PROVINCE <<< "${line}"
TARGET="${REFDIR}/output/forecast_${SCALE}_${PROVINCE}.rds"

echo "=== task ${TASK_ID}: ${SCALE} ${PROVINCE} ==="
echo "host:    $(hostname)"
echo "cores:   ${SLURM_CPUS_PER_TASK:-unset}"
echo "target:  ${TARGET}"
echo "started: $(date --iso-8601=seconds)"

# Go through make rather than calling Rscript directly: the dependency logic
# stays in one place, and the task becomes idempotent, so resubmitting after a
# timeout skips whatever already completed.
hpc_exec make "${TARGET}"

echo "finished: $(date --iso-8601=seconds)"
