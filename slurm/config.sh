# Site configuration for the LSHTM HPC.
#
# Sourced by every script in this directory. Edit the values below to match
# your setup; see README_HPC.md for the rationale behind each one.
#
# Expects PROJECT_ROOT to be set by the caller.

# --- Software environment ---------------------------------------------------
# renv.lock pins R 4.5.0. If `module avail R` shows nothing that recent, run
# inside a container instead (the default):
#
#   singularity pull "${PROJECT_ROOT}/rocker.sif" docker://rocker/r-ver:4.5.0
#
# rocker/r-ver resolves packages from the Posit Package Manager binary repo for
# Linux, so renv::restore() pulls a precompiled EpiNow2 rather than spending
# ~40 minutes compiling its Stan models.
#
# Set SIF="" to use environment modules instead.
SIF="${SIF:-${PROJECT_ROOT}/rocker.sif}"

# Modules to load when SIF is empty, in order. R is a dependent module and only
# becomes visible after its compiler dependency is loaded.
R_MODULES="${R_MODULES:-gnu12 R}"

# --- Storage ----------------------------------------------------------------
# Shared renv cache, so the ~150 locked packages are restored once rather than
# once per array task. Must be visible from the compute nodes.
export RENV_PATHS_CACHE="${RENV_PATHS_CACHE:-${HOME}/.cache/R/renv}"

# Output tree. Read back from the Makefile rather than set here so it cannot
# drift from the REFDIR in local.makefile.
REFDIR="$(make -s -C "${PROJECT_ROOT}" print-REFDIR)"

# --- Runner -----------------------------------------------------------------
# hpc_exec <command...>
# Runs a command in the project's software environment, either inside the
# container or under the loaded modules.
hpc_exec() {
  if [ -n "${SIF}" ]; then
    # Singularity auto-binds $HOME and the cwd; bind REFDIR explicitly when it
    # lives outside the project tree (e.g. on scratch).
    binds=("--bind" "${PROJECT_ROOT}")
    case "${REFDIR}" in
      "${PROJECT_ROOT}"/*) ;;
      *) binds+=("--bind" "${REFDIR}") ;;
    esac

    singularity exec \
      "${binds[@]}" \
      --env "RENV_PATHS_CACHE=${RENV_PATHS_CACHE}" \
      "${SIF}" "$@"
  else
    for mod in ${R_MODULES}; do
      module load "${mod}"
    done
    "$@"
  fi
}
