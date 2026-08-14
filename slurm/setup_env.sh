#!/bin/bash
#
# One-off environment setup: pull the container (if used) and restore the renv
# library into the shared cache.
#
# Run this on a compute node, not the login node -- restoring 150 packages is
# exactly the kind of work the scheduler exists for:
#
#   srun --pty --cpus-per-task=4 --mem=16G --time=02:00:00 bash
#   ./slurm/setup_env.sh
#
# Needs outbound network access for the package repos. If the compute nodes are
# firewalled, run it on the login node instead and accept the etiquette hit.

set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "${PROJECT_ROOT}"

# shellcheck source=slurm/config.sh
source slurm/config.sh

mkdir -p "${RENV_PATHS_CACHE}"

if [ -n "${SIF}" ] && [ ! -f "${SIF}" ]; then
  echo "=== pulling container to ${SIF} ==="
  # Pinned to the R version recorded in renv.lock; bump both together.
  singularity pull "${SIF}" docker://rocker/r-ver:4.5.0
fi

echo "=== restoring renv library ==="
echo "cache: ${RENV_PATHS_CACHE}"
hpc_exec Rscript --vanilla install.R

echo "=== checking the toolchain ==="
hpc_exec Rscript -e '
  cat("R:        ", as.character(getRversion()), "\n", sep = "")
  cat("EpiNow2:  ", as.character(packageVersion("EpiNow2")), "\n", sep = "")
  cat("backend:  ", if (requireNamespace("cmdstanr", quietly = TRUE)) "cmdstanr" else "rstan", "\n", sep = "")
  cat("cores:    ", Sys.getenv("SLURM_CPUS_PER_TASK", "unset"), "\n", sep = "")
'

echo "=== done ==="
