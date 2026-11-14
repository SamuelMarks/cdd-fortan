#!/usr/bin/env bash
set -euo pipefail

# Installs a local Git pre-push hook that unconditionally blocks `git push`
# in compliance with the repository safety mandate.

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
HOOKS_DIR="${REPO_ROOT}/.git/hooks"

if [[ ! -d "${REPO_ROOT}/.git" ]]; then
  echo "Git repository not initialized yet at ${REPO_ROOT}; skipping hook copy."
  exit 0
fi

mkdir -p "${HOOKS_DIR}"
cat << 'EOF' > "${HOOKS_DIR}/pre-push"
#!/usr/bin/env bash
echo "ERROR: 'git push' is strictly prohibited in cdd-fortran." >&2
exit 1
EOF
chmod +x "${HOOKS_DIR}/pre-push"
echo "Installed pre-push blocker hook at ${HOOKS_DIR}/pre-push"
