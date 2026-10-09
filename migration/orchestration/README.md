# Migration issue and PR orchestration

The ten implementation issues are #91-#100 in `Philess/gh-copilot-demo`.
Their native GitHub blocked-by dependencies follow the approved migration plan.
Integration PR #90 uses `feat/music-store-migration` as its head and stays draft.
The first coding task, #91, is assigned to Copilot and has draft PR #101 targeting
the integration branch.

## Dependency-safe delegation

Run from a checkout of the feature branch using PowerShell and an authenticated
GitHub CLI with repository access. The installed CLI need not provide the newer
`gh agent-task` command; assignment uses the documented REST API.

```powershell
# Read-only preview: discover existing assignments, prerequisite PRs and bases.
pwsh -File .\migration\orchestration\delegate-ready-steps.ps1 `
  -ManifestPath .\migration\orchestration\steps.json

# Explicitly assign only ready, non-cutover steps.
pwsh -File .\migration\orchestration\delegate-ready-steps.ps1 `
  -ManifestPath .\migration\orchestration\steps.json -Execute
```

The helper runs one coordination pass; it is not an automatic background queue.
Re-run it after prerequisite PRs are reviewed and ready. It skips existing
Copilot assignments and refuses nonexistent bases. It does not merge PRs,
publish a live link, deploy resources, collect tokens, or bypass protection.

GitHub's issue-assignment API selects a starting branch but has no dependency
queuing or automatic stacked-PR rebasing. Issue links alone do not delegate
blocked steps. Keep the distinction between queued issues and started agent
jobs explicit.

## Stack and integration

- Baseline starts from the integration branch.
- Scaffold and infrastructure start from the ready baseline PR head.
- Data and frontend start from the ready scaffold PR head; API starts from data.
- Delivery has three prerequisites: integrate reviewed API, frontend and
  infrastructure changes into the feature branch before delegating delivery.
  Nested PR merges are valid once their merge commits are contained in the
  integration branch; they need not have originally targeted integration.
- Validation, documentation, and cutover artifacts follow their prerequisite PR
  heads, or the integration branch if those artifacts are already merged there.
- Retarget child PRs to an appropriate surviving branch before deleting any
  parent branch. Merge reviewed step PRs only within the feature stack.
- Use merge commits for the stack, not squash/rebase merges: the coordinator
  verifies prerequisite merge-commit ancestry rather than guessing whether
  equivalent patches have reached the integration branch.
- Do not merge integration PR #90 into `main` automatically.

PR discovery requires the coding PR to reference its issue number in the body.
Non-draft status and passing checks are coordination signals, not a substitute
for reviewing implementation, verifying acceptance criteria, or checking that
ancestor artifacts reached the selected base.

## Operational gates

Issue #100 delegates only a safe cutover readiness package, not an actual link
switch. Its dispatch requires an explicit helper flag and ready prerequisites;
live deployment and cutover still require separate approval and environment
inputs. Do not close the live-cutover plan task merely because a runbook exists.

Required code validation belongs to each implementation PR. Missing private
Azure access must be reported as unexecuted checks, not success. Preserve
original applications and unrelated local/untracked work throughout.
