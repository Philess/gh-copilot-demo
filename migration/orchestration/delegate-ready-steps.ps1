<#
.SYNOPSIS
    Read-only-by-default coordination pass for delegating migration-plan issues to the
    GitHub Copilot coding agent (cloud agent) using the documented REST
    `agent_assignment` issue-assignment API, with dependency-aware base-branch routing.

.DESCRIPTION
    Verified, documented mechanism only (no `gh agent-task`, which requires gh CLI
    >= 2.80.0 and is unavailable on the installed CLI):
      https://docs.github.com/en/copilot/how-tos/use-copilot-agents/cloud-agent/use-cloud-agent-via-the-api
      https://docs.github.com/en/rest/issues/assignees#add-assignees-to-an-issue

    Per manifest step this script:
      1. Skips (already-assigned) any issue already assigned to a Copilot agent bot.
      2. Resolves the ACTUAL IMPLEMENTING pull request for a prerequisite issue via
         Find-ClosingPullRequest: broad candidate discovery from the Issues Timeline
         API's cross-referenced events, narrowed to only PRs whose body contains a
         real GitHub closing keyword (close/closes/closed, fix/fixes/fixed,
         resolve/resolves/resolved) immediately followed by `#N` or `OWNER/REPO#N`
         for that exact issue number -- see
         https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/using-keywords-in-issues-and-pull-requests
         The manifest's own `integration_pr` (and any -ExcludePrNumbers) is excluded
         from candidates up front, as defense-in-depth against a tracking/status PR
         that merely lists every issue number in a table (verified live: PR #90 on
         Philess/gh-copilot-demo does exactly this for #91-#100, with no closing
         keywords in its body -- confirmed BOTH filters would independently exclude
         it). If zero PRs match, the step is 'blocked: no linked implementing PR
         yet'. If MORE THAN ONE PR's body closes the same issue number, the step is
         'blocked: ambiguous candidates #x, #y, ...' -- this script never guesses by
         picking the most-recently-updated PR.
      3. Resolves readiness:
           - 0 dependencies            -> baseline step; base_branch = -IntegrationBranch.
           - 1 dependency (chain)      -> requires the prerequisite step's implementing PR
                                          (per step 2) to be:
                                            * same-repository (head.repo.full_name == -Repo;
                                              i.e. not a fork PR) and
                                            * non-draft, and
                                            * have no FAILURE among required checks, and
                                            * have at least one APPROVED review with no later
                                              CHANGES_REQUESTED from the same reviewer
                                              (best-effort heuristic -- see LIMITATIONS), and
                                            * resolve to a base_branch:
                                                - open PR   -> its head branch (must exist)
                                                - merged PR -> prefer -IntegrationBranch if
                                                  the PR's merge_commit_sha is PROVABLY
                                                  CONTAINED in it (compare API status
                                                  ahead/identical); else fall back to the
                                                  PR's own base.ref, but ONLY if that ref
                                                  still exists AND the merge_commit_sha is
                                                  also provably contained there (stale/
                                                  rewritten base branches are refused, not
                                                  trusted blindly); else 'blocked' explaining
                                                  neither location contains the merge commit
                                                  (e.g. a nested/stacked merge that has not
                                                  yet been integrated upward).
           - >1 dependency (fan-in)    -> requires ALL prerequisite PRs to be MERGED, same-
                                          repository, with merge_commit_sha PROVABLY CONTAINED
                                          in -IntegrationBranch (compare API status
                                          ahead/identical) -- NOT merely `base.ref ==
                                          -IntegrationBranch`, because a valid stacked
                                          workflow may legitimately merge a fan-in PR into an
                                          intermediate/prerequisite branch before that branch
                                          is itself merged up into the integration branch.
                                          base_branch = -IntegrationBranch. This path requires
                                          real merge commits (no squash/rebase) to remain
                                          verifiable via merge_commit_sha containment -- a
                                          process constraint on the repository, not something
                                          this script can enforce itself.
      5. Verifies the chosen base_branch ref actually exists on the remote before ever
         planning/dispatching to it (never targets a nonexistent ref).
      6. The `migration-cutover` stage (or any step explicitly flagged stage=cutover) is
         NEVER dispatched by this script, even with -Execute, unless -AllowCutoverDispatch
         is also passed -- and even then it only assigns the issue to the agent; it never
         merges, deploys, or flips the published link itself.
      7. In dry-run (default) mode, performs GET-only reads (no POST/PATCH/mutation calls)
         and reports what WOULD be dispatched. Only with -Execute does it POST the
         assignment, and only for steps resolved as Ready.
      8. Returns one structured JSON object (stdout) describing every step's resolution,
         suitable for a parent process/agent to parse.

    ERROR HANDLING (strict, no silent defaults):
      Every GitHub CLI call is made through Invoke-GhApi, which separates stdout/stderr/
      exit code via a real child process (not merged streams) so a genuine HTTP status can
      be recovered from gh's stderr ("gh: <message> (HTTP <code>)") even on failure.
        - HTTP 404 is treated as a benign "does not exist" result ONLY for the two cases
          where absence is an expected, meaningful outcome: (a) a branch/ref existence
          check, and (b) a PR lookup that legitimately may not exist yet. Both cases are
          reported as 'blocked' with the concrete detail, never silently defaulted.
        - A 404 on an endpoint whose target is NOT optional (e.g. the step's own issue, or
          a PR number that a timeline event just told us exists) is treated as a hard error,
          because it indicates a broken reference, not an expected absence.
        - Any other non-2xx status (401/403/422/5xx), a process-level failure (exit code
          nonzero with no parseable HTTP status, e.g. network/DNS failure, gh not
          authenticated, rate limit exhaustion), or unparsable JSON is a hard error.
        - A hard error during a step's resolution sets that step's status to 'error' with
          the raw gh diagnostic preserved in `reason`; it is never coerced into 'blocked'
          or silently treated as "not found"/"false". The script's exit code is nonzero
          (2) if any step ends in 'error', so automation can detect degraded data and must
          not treat the JSON as a complete, trustworthy coordination result.

    LIMITATIONS (explicitly stated, not glossed over):
      - Review-approval eligibility is a best-effort heuristic over
        GET /repos/{owner}/{repo}/pulls/{number}/reviews (latest review state per reviewer
        login is APPROVED and no later CHANGES_REQUESTED from that same reviewer). This
        does NOT query branch protection's required-reviewer-count, required CODEOWNERS,
        or dismissed-review policy -- this script has no branch-protection/rulesets scope
        requested of it. If branch protection requires more/specific approvals than this
        heuristic finds, a step may be reported 'ready' here while GitHub itself would
        still refuse to merge the PR; this script dispatches issue assignments, not merges,
        so that gap does not cause any merge/deploy action, only a possibly-premature next
        assignment. If no reviews exist at all, the step is 'blocked' with that stated
        explicitly, never assumed approved.
      - "No failing required checks" uses the combined commit status + check-runs
        conclusions; it does not read the branch-protection "required status checks" list
        (same reason: no branch-protection scope requested), so it cannot distinguish a
        failing optional check from a failing required one -- any failing/neutral-failing
        check blocks, which is the conservative direction.
      - PR linkage is resolved via the Issues Timeline API's cross-referenced events as a
        broad candidate net, then narrowed to only PRs whose body contains a real GitHub
        closing keyword for the exact prerequisite issue number (see step 2 above). If an
        issue has not yet been referenced by any qualifying closing PR, the step reports
        'blocked: no linked implementing PR found yet', not an error. If more than one PR
        closes the same issue number, the step reports 'blocked: ambiguous candidates', not
        an error, and never arbitrarily picks one.
      - Merge-commit containment (compare API) only proves ancestry against the CURRENT
        state of a branch's history. If a branch is later force-pushed/rebased past that
        commit, a previously-contained merge commit could stop being provably contained --
        this script only ever reports the current, live answer; it does not cache or trust
        a prior run's result.

    Credentials: this script never reads, stores, prints, or exports any token. All
    GitHub calls are delegated to the already-authenticated `gh` CLI (`gh api`), which
    manages its own credential storage; nothing is echoed from `gh auth status`/token.

    This script never merges, pushes, deletes branches, edits workflows, or performs any
    deployment/cutover action. Its only possible mutation, and only under -Execute, is a
    single `POST /repos/{owner}/{repo}/issues/{issue}/assignees` call per Ready step.

.PARAMETER ManifestPath
    Path to a JSON manifest in either shape:
      (a) Published shape (preferred): a top-level OBJECT containing a `steps` array, and
          optionally a top-level `integration_pr` (int) -- the repo's own tracking/
          integration PR number, automatically excluded from PR-linkage candidate
          discovery (defense-in-depth on top of the closing-keyword filter):
            { "integration_pr": 90, "steps": [ { "step_id": "...", "issue_number": 123,
                           "dependencies": ["..."] }, ... ] }
      (b) Legacy shape: a top-level ARRAY of the same step objects (no integration_pr).
    Each step object fields:
      step_id             (string, required)  stable id, e.g. "migration-baseline"
      issue_number        (int,    required)  GitHub issue number for this step
      dependencies        (array of string, default []) step_id values this step depends on
      stage               (string, optional)  free-form label; "cutover" triggers the
                                               cutover safety gate in addition to step_id
      custom_instructions (string, optional)  passed through verbatim if present
      custom_agent        (string, optional)  passed through verbatim if present
      model               (string, optional)  passed through verbatim if present
    Unspecified optional fields are omitted from the outgoing payload entirely (the
    endpoint's `agent_assignment` object is kept minimal).

.PARAMETER Repo
    "OWNER/REPO" to operate against. Default: Philess/gh-copilot-demo

.PARAMETER IntegrationBranch
    Name of the shared integration branch used as the baseline base_branch and as the
    fan-in target. Default: feat/music-store-migration

.PARAMETER CopilotLogin
    Bot login assigned for delegation. Default: copilot-swe-agent[bot] (REST assignee
    login form; the bare login copilot-swe-agent is used for existing-assignment checks).

.PARAMETER ExcludePrNumbers
    Additional PR numbers to treat as non-candidates for PR-linkage discovery, beyond the
    manifest's own `integration_pr` (which is always excluded automatically when present).
    Default: empty array.

.PARAMETER Execute
    Switch. When absent (default), the script is in dry-run/preview (WhatIf) mode: it
    performs only read (GET) calls and reports planned actions without assigning anyone.
    When present, Ready steps are actually POSTed to the assignees endpoint.

.PARAMETER AllowCutoverDispatch
    Switch. Required in addition to -Execute before a step tagged stage "cutover" (or
    step_id migration-cutover) may be dispatched. Absent by default as a safety gate.
    This still only assigns the issue to the coding agent; it never performs the cutover,
    merge, deploy, or publish-link change itself.

.EXAMPLE
    Dry run (default; no mutation, no dispatch):
      pwsh -File .\delegate-ready-steps.ps1 -ManifestPath .\published-manifest.json

.EXAMPLE
    Execute (dispatches Ready, non-cutover steps only):
      pwsh -File .\delegate-ready-steps.ps1 -ManifestPath .\published-manifest.json -Execute

.EXAMPLE
    Execute including an explicitly approved cutover dispatch:
      pwsh -File .\delegate-ready-steps.ps1 -ManifestPath .\published-manifest.json `
        -Execute -AllowCutoverDispatch
#>
[CmdletBinding(SupportsShouldProcess = $true, ConfirmImpact = 'Medium')]
param(
    [Parameter(Mandatory = $true)]
    [string]$ManifestPath,

    [string]$Repo = 'Philess/gh-copilot-demo',

    [string]$IntegrationBranch = 'feat/music-store-migration',

    [string]$CopilotLogin = 'copilot-swe-agent[bot]',

    [int[]]$ExcludePrNumbers = @(),

    [switch]$Execute,

    [switch]$AllowCutoverDispatch
)

$ErrorActionPreference = 'Stop'

# Dry-run is the default. -WhatIf is honored automatically via SupportsShouldProcess;
# -Execute is required to actually mutate anything, as an explicit second opt-in on top
# of ShouldProcess, so accidental `-Confirm:$false` alone can never dispatch.
$DryRun = -not $Execute.IsPresent

function Write-Diag {
    param([string]$Message)
    Write-Verbose $Message
}

# ---------------------------------------------------------------------------
# Low-level gh invocation with clean stdout/stderr/exit-code separation.
# This intentionally does NOT use PowerShell's `2>&1` merge (which interleaves
# streams unpredictably with `gh`'s buffering) so that a real HTTP status code
# can be recovered from stderr even when the call failed.
# ---------------------------------------------------------------------------
function Invoke-GhProcess {
    param(
        [Parameter(Mandatory = $true)][string[]]$ArgumentList,
        [string]$StdInText = $null
    )
    $psi = [System.Diagnostics.ProcessStartInfo]::new()
    $psi.FileName = 'gh'
    foreach ($a in $ArgumentList) { $psi.ArgumentList.Add($a) }
    $psi.RedirectStandardOutput = $true
    $psi.RedirectStandardError = $true
    $psi.RedirectStandardInput = [bool]$StdInText
    $psi.UseShellExecute = $false
    $psi.CreateNoWindow = $true

    $proc = [System.Diagnostics.Process]::new()
    $proc.StartInfo = $psi
    [void]$proc.Start()
    if ($StdInText) {
        $proc.StandardInput.Write($StdInText)
        $proc.StandardInput.Close()
    }
    $stdout = $proc.StandardOutput.ReadToEnd()
    $stderr = $proc.StandardError.ReadToEnd()
    $proc.WaitForExit()

    return [pscustomobject]@{
        ExitCode = $proc.ExitCode
        StdOut   = $stdout
        StdErr   = $stderr
    }
}

# ---------------------------------------------------------------------------
# API result wrapper. Returns a structured, explicit outcome:
#   Ok=$true,  NotFound=$false -> success, Data populated
#   Ok=$true,  NotFound=$true  -> genuine HTTP 404 (caller decides if that's
#                                 meaningful-absence or still an error)
#   Ok=$false                  -> hard error (anything else); Error has detail.
#                                 Callers MUST surface this, never default it away.
# ---------------------------------------------------------------------------
function Invoke-GhApi {
    param(
        [Parameter(Mandatory = $true)][string]$Endpoint,
        [string]$Method = 'GET',
        [string[]]$ExtraArgs = @(),
        [string]$StdInText = $null
    )
    $args = @('api', $Endpoint, '-X', $Method, '-H', 'Accept: application/vnd.github+json', '-H', 'X-GitHub-Api-Version: 2022-11-28') + $ExtraArgs
    $r = Invoke-GhProcess -ArgumentList $args -StdInText $StdInText

    if ($r.ExitCode -eq 0) {
        $data = $null
        if (-not [string]::IsNullOrWhiteSpace($r.StdOut)) {
            try {
                $data = $r.StdOut | ConvertFrom-Json -Depth 30
            } catch {
                return [pscustomobject]@{
                    Ok = $false; NotFound = $false; Data = $null
                    Error = "gh api succeeded (exit 0) for '$Endpoint' but response body was not valid JSON: $($_.Exception.Message)"
                }
            }
        }
        return [pscustomobject]@{ Ok = $true; NotFound = $false; Data = $data; Error = $null }
    }

    # Non-zero exit: try to recover a real HTTP status from gh's stderr, e.g.
    # "gh: Not Found (HTTP 404)" or "gh: Bad credentials (HTTP 401)".
    $statusCode = $null
    if ($r.StdErr -match 'HTTP (\d+)') {
        $statusCode = [int]$Matches[1]
    }

    if ($statusCode -eq 404) {
        return [pscustomobject]@{
            Ok = $true; NotFound = $true; Data = $null
            Error = "HTTP 404 for '$Endpoint': $($r.StdErr.Trim())"
        }
    }

    $detail = if ($r.StdErr) { $r.StdErr.Trim() } else { $r.StdOut.Trim() }
    if ([string]::IsNullOrWhiteSpace($detail)) {
        $detail = "gh exited with code $($r.ExitCode) and produced no diagnostic output"
    }
    return [pscustomobject]@{
        Ok = $false; NotFound = $false; Data = $null
        Error = "gh api call failed for '$Endpoint' (exit $($r.ExitCode)$(if ($statusCode) { ", HTTP $statusCode" })): $detail"
    }
}

# ---------------------------------------------------------------------------
# A hard error raised anywhere during a single step's resolution. Caught once,
# at the per-step level in the main loop, so one step's genuine API failure
# can never be silently coerced into 'blocked' or 'false' and can never abort
# the whole batch either -- every other step still gets resolved independently.
# ---------------------------------------------------------------------------
function Assert-ApiOk {
    param([Parameter(Mandatory = $true)]$ApiResult, [string]$Context)
    if (-not $ApiResult.Ok) {
        throw "$Context -> $($ApiResult.Error)"
    }
}

function Test-BranchExists {
    <#
        Existence check for a ref on the remote (read-only; never creates/deletes).
        A genuine 404 here IS the meaningful, expected "does not exist yet" answer
        for this specific call, so it is returned as $false rather than thrown --
        this is the one case explicitly called out as acceptable to classify as
        'blocked', not 'error'. Any other failure (auth/network/5xx) still throws.
    #>
    param(
        [Parameter(Mandatory = $true)][string]$OwnerRepo,
        [Parameter(Mandatory = $true)][string]$Branch
    )
    if ([string]::IsNullOrWhiteSpace($Branch)) { return $false }
    $encoded = [uri]::EscapeDataString($Branch)
    $r = Invoke-GhApi -Endpoint "repos/$OwnerRepo/branches/$encoded"
    if ($r.Ok -and $r.NotFound) { return $false }
    Assert-ApiOk -ApiResult $r -Context "Checking branch existence for '$Branch' in $OwnerRepo"
    return [bool]($r.Data -and $r.Data.name)
}

function Get-IssueAssignees {
    <#
        The step's own issue number is NOT optional data -- if it 404s, that is a
        real configuration/reference error in the manifest, not an expected
        absence, so it throws rather than returning an empty assignee list.
    #>
    param(
        [Parameter(Mandatory = $true)][string]$OwnerRepo,
        [Parameter(Mandatory = $true)][int]$IssueNumber
    )
    $r = Invoke-GhApi -Endpoint "repos/$OwnerRepo/issues/$IssueNumber"
    if ($r.Ok -and $r.NotFound) {
        throw "Issue #$IssueNumber does not exist in $OwnerRepo (HTTP 404) -- this is the step's own issue_number, not an optional prerequisite; treating as a hard manifest error, not a benign absence."
    }
    Assert-ApiOk -ApiResult $r -Context "Fetching issue #$IssueNumber"
    if (-not $r.Data) { return @() }
    return @($r.Data.assignees | ForEach-Object { $_.login })
}

function Find-ClosingPullRequest {
    <#
        Resolves the ACTUAL IMPLEMENTING pull request for a prerequisite issue --
        deliberately NOT "any PR that merely mentions the issue number anywhere".

        Bug this replaces: the previous version picked the most-recently-updated
        PR among ALL Timeline cross-referenced events, which over-matched on PRs
        that simply name-drop an issue in a tracking/status section (e.g. the
        integration PR's own "Step issues and stack" table lists every #91-#100,
        and a sibling step's PR body might mention a prerequisite's issue number
        in prose without actually implementing/closing it). That is not harmless
        long-term: it could silently route a dependent task's base_branch onto a
        PR that never implements the prerequisite.

        Correct, GitHub-native signal: a PR only "closes" an issue via one of the
        documented closing keywords (close/closes/closed, fix/fixes/fixed,
        resolve/resolves/resolved) immediately followed by `#N` (same-repo) or
        `OWNER/REPO#N` (explicit), per
        https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/using-keywords-in-issues-and-pull-requests
        This is exactly the real Copilot coding-agent PR convention observed live
        on Philess/gh-copilot-demo PR #101's body: a line "- Fixes #91" appended
        under "<!-- START COPILOT CODING AGENT SUFFIX -->".

        Discovery still starts from the Timeline API's cross-referenced events
        (broad candidate net: https://docs.github.com/en/rest/issues/timeline),
        but every candidate is then required to pass the closing-keyword body
        check below before being treated as "the" implementing PR. PR numbers in
        -ExcludePrNumbers (the manifest's own integration/tracking PR, notably)
        are filtered out of the candidate set before that check even runs, as an
        explicit, defense-in-depth exclusion -- not relying solely on the fact
        that a well-formed tracking PR body shouldn't use closing keywords.

        Returns an object:
          Pr=<PR object>, Ambiguous=$false  -> exactly one qualifying PR found.
          Pr=$null,       Ambiguous=$false  -> no qualifying PR yet (legitimate,
                                               expected "blocked: not found yet").
          Pr=$null,       Ambiguous=$true,  Candidates=<2+ PR objects>
                                             -> more than one PR's body closes
                                               this exact issue number; refusing
                                               to guess which is authoritative.
        Throws on any hard error (own issue 404, a cross-referenced PR number
        that itself cannot be fetched, auth/network/5xx) -- never silently
        defaulted.
    #>
    param(
        [Parameter(Mandatory = $true)][string]$OwnerRepo,
        [Parameter(Mandatory = $true)][int]$IssueNumber,
        [int[]]$ExcludePrNumbers = @()
    )
    $r = Invoke-GhApi -Endpoint "repos/$OwnerRepo/issues/$IssueNumber/timeline?per_page=100" `
        -ExtraArgs @('-H', 'Accept: application/vnd.github.mockingbird-preview+json,application/vnd.github+json')
    if ($r.Ok -and $r.NotFound) {
        throw "Prerequisite issue #$IssueNumber does not exist in $OwnerRepo (HTTP 404) -- the dependency's own issue_number is not an optional field; treating as a hard manifest error."
    }
    Assert-ApiOk -ApiResult $r -Context "Fetching timeline for issue #$IssueNumber"

    $timeline = @($r.Data)
    $prNumbers = @()
    foreach ($event in $timeline) {
        if ($event.event -eq 'cross-referenced' -and $event.source -and $event.source.issue -and $event.source.issue.pull_request) {
            $prNumbers += [int]$event.source.issue.number
        }
    }
    $prNumbers = @($prNumbers | Select-Object -Unique | Where-Object { $ExcludePrNumbers -notcontains $_ })
    if ($prNumbers.Count -eq 0) {
        return [pscustomobject]@{ Pr = $null; Ambiguous = $false; Candidates = @() }
    }

    $repoEscaped = [regex]::Escape($OwnerRepo)
    $closingPattern = "(?i)\b(close[sd]?|fix(?:e[sd])?|resolve[sd]?)\b\s*:?\s*(?:$repoEscaped#|#)$IssueNumber\b"

    $matching = @()
    foreach ($num in $prNumbers) {
        $prResp = Invoke-GhApi -Endpoint "repos/$OwnerRepo/pulls/$num"
        if ($prResp.Ok -and $prResp.NotFound) {
            throw "Timeline for issue #$IssueNumber cross-references PR #$num, but GET /pulls/$num returned HTTP 404 in $OwnerRepo -- inconsistent state, not a benign absence."
        }
        Assert-ApiOk -ApiResult $prResp -Context "Fetching cross-referenced PR #$num for issue #$IssueNumber"
        if (-not $prResp.Data) { continue }
        $body = [string]$prResp.Data.body
        if ($body -and ($body -match $closingPattern)) {
            $matching += $prResp.Data
        }
    }

    if ($matching.Count -eq 0) {
        return [pscustomobject]@{ Pr = $null; Ambiguous = $false; Candidates = @() }
    }
    if ($matching.Count -gt 1) {
        return [pscustomobject]@{ Pr = $null; Ambiguous = $true; Candidates = $matching }
    }
    return [pscustomobject]@{ Pr = $matching[0]; Ambiguous = $false; Candidates = $matching }
}

function Test-CommitContainedIn {
    <#
        Is $CommitSha an ancestor of (contained within the history of) $Branch?
        Uses GET /repos/{owner}/{repo}/compare/{CommitSha}...{Branch} and the
        documented status semantics, verified live against this repo:
          compare(base=<older commit>, head=<branch ahead of it>) -> "ahead"
          compare(base=<same commit>,  head=<same ref>)            -> "identical"
          compare(base=<newer/divergent>, head=<branch behind it>) -> "behind"/"diverged"
        "ahead" or "identical" means $CommitSha's history is fully included in
        $Branch (contained). "behind"/"diverged" means it is not (yet, or ever,
        on that branch's current history) -- the branch may have been rebased/
        squashed past it, or the commit simply hasn't been integrated upward yet.
        A 404 here (bad/unknown sha, e.g. null/garbage) is NOT a benign absence
        for a real merge_commit_sha -- it throws; callers must check for a
        missing/null merge_commit_sha themselves before calling this.
    #>
    param(
        [Parameter(Mandatory = $true)][string]$OwnerRepo,
        [Parameter(Mandatory = $true)][string]$CommitSha,
        [Parameter(Mandatory = $true)][string]$Branch
    )
    $encodedBranch = [uri]::EscapeDataString($Branch)
    $r = Invoke-GhApi -Endpoint "repos/$OwnerRepo/compare/$CommitSha...$encodedBranch"
    if ($r.Ok -and $r.NotFound) {
        throw "Compare API returned HTTP 404 comparing commit $CommitSha against branch '$Branch' in $OwnerRepo -- unexpected for a real merge commit sha against an existing branch; not treated as a benign absence."
    }
    Assert-ApiOk -ApiResult $r -Context "Comparing commit $CommitSha against branch '$Branch'"
    return [bool]($r.Data -and $r.Data.status -in @('ahead', 'identical'))
}

function Test-RequiredChecksHaveFailure {
    <# Combined commit status + check-runs for a SHA. A 404 here (bad/missing SHA)
       is NOT an expected, meaningful absence -- it throws. #>
    param(
        [Parameter(Mandatory = $true)][string]$OwnerRepo,
        [Parameter(Mandatory = $true)][string]$HeadSha
    )
    $statusResp = Invoke-GhApi -Endpoint "repos/$OwnerRepo/commits/$HeadSha/status"
    if ($statusResp.Ok -and $statusResp.NotFound) {
        throw "Combined status for commit $HeadSha not found (HTTP 404) in $OwnerRepo -- unexpected for a PR head commit."
    }
    Assert-ApiOk -ApiResult $statusResp -Context "Fetching combined status for $HeadSha"
    $hasFailure = [bool]($statusResp.Data -and $statusResp.Data.state -eq 'failure')

    $checksResp = Invoke-GhApi -Endpoint "repos/$OwnerRepo/commits/$HeadSha/check-runs?per_page=100"
    if ($checksResp.Ok -and $checksResp.NotFound) {
        throw "Check-runs for commit $HeadSha not found (HTTP 404) in $OwnerRepo -- unexpected for a PR head commit."
    }
    Assert-ApiOk -ApiResult $checksResp -Context "Fetching check-runs for $HeadSha"
    if ($checksResp.Data -and $checksResp.Data.check_runs) {
        foreach ($run in $checksResp.Data.check_runs) {
            if ($run.conclusion -in @('failure', 'timed_out', 'cancelled', 'action_required')) {
                $hasFailure = $true
            }
        }
    }
    return $hasFailure
}

function Test-PullRequestApproved {
    <#
        Best-effort review-approval heuristic -- see script-level LIMITATIONS.
        Returns $true only if at least one reviewer's LATEST review is APPROVED
        and no reviewer's latest review is CHANGES_REQUESTED. Returns $false
        (with no reviews at all counted as not-approved, stated explicitly by
        the caller) rather than ever assuming approval. A 404 on the reviews
        endpoint for a real, existing PR number is a hard error, not a benign
        absence (an empty array, not 404, is how "no reviews yet" is expressed
        by this endpoint).
    #>
    param(
        [Parameter(Mandatory = $true)][string]$OwnerRepo,
        [Parameter(Mandatory = $true)][int]$PrNumber
    )
    $r = Invoke-GhApi -Endpoint "repos/$OwnerRepo/pulls/$PrNumber/reviews?per_page=100"
    if ($r.Ok -and $r.NotFound) {
        throw "Reviews endpoint for PR #$PrNumber returned HTTP 404 in $OwnerRepo -- unexpected for an existing PR."
    }
    Assert-ApiOk -ApiResult $r -Context "Fetching reviews for PR #$PrNumber"

    $reviews = @($r.Data)
    if ($reviews.Count -eq 0) { return $false }

    $latestByUser = @{}
    foreach ($rev in ($reviews | Sort-Object { [datetime]$_.submitted_at })) {
        if ($rev.user -and $rev.user.login -and $rev.state -in @('APPROVED', 'CHANGES_REQUESTED', 'COMMENTED', 'DISMISSED')) {
            $latestByUser[$rev.user.login] = $rev.state
        }
    }
    $states = @($latestByUser.Values)
    if ($states -contains 'CHANGES_REQUESTED') { return $false }
    return [bool]($states -contains 'APPROVED')
}

function Test-AlreadyCopilotAssigned {
    <#
        Case-insensitive match against every known Copilot coding-agent login alias.
        Verified live against Philess/gh-copilot-demo issue #91: after a real REST
        assignment with assignees=["copilot-swe-agent[bot]"], GET /issues/91 returns
        the assignee's login back as plain "Copilot" (not "copilot-swe-agent[bot]"),
        alongside the human "Philess". Only Copilot aliases count here -- a different
        bot such as "anthropic-code-agent" (also a valid suggestedActors entry on this
        repo) is intentionally NOT treated as an existing Copilot assignment.
    #>
    param([string[]]$Assignees, [string]$CopilotLogin)
    $knownCopilotAliases = @('Copilot', 'copilot-swe-agent[bot]', 'copilot-swe-agent', $CopilotLogin)
    foreach ($a in $Assignees) {
        foreach ($alias in $knownCopilotAliases) {
            if ($a -and $alias -and ($a.Trim() -ieq $alias.Trim())) { return $true }
        }
    }
    return $false
}

# ---------------------------------------------------------------------------
# Load manifest -- accepts either the published { "steps": [...] } object shape
# or a legacy bare array shape.
# ---------------------------------------------------------------------------
if (-not (Test-Path -LiteralPath $ManifestPath)) {
    throw "ManifestPath not found: $ManifestPath"
}
$manifestRaw = Get-Content -LiteralPath $ManifestPath -Raw
$manifestParsed = $manifestRaw | ConvertFrom-Json -Depth 30
if ($null -eq $manifestParsed) { throw "Manifest parsed to null; expected a JSON object with a 'steps' array, or a bare array." }

if ($manifestParsed -is [System.Collections.IEnumerable] -and -not ($manifestParsed.PSObject.Properties.Name -contains 'steps')) {
    # Legacy bare-array shape.
    $manifestSteps = @($manifestParsed)
} elseif ($manifestParsed.PSObject.Properties.Name -contains 'steps') {
    # Published shape: top-level object with a `steps` array.
    $manifestSteps = @($manifestParsed.steps)
} else {
    throw "Manifest must be either a top-level object with a 'steps' array, or a bare array of step objects."
}

$stepsById = @{}
foreach ($step in $manifestSteps) {
    if (-not $step.step_id) { throw "Manifest entry missing step_id: $($step | ConvertTo-Json -Compress)" }
    $stepsById[$step.step_id] = $step
}

# integration_pr (the repo's own tracking/integration PR, e.g. #90) is always excluded
# from PR-linkage candidate discovery, in addition to any explicit -ExcludePrNumbers.
$manifestIntegrationPr = @()
if (($manifestParsed.PSObject.Properties.Name -contains 'steps') -and
    ($manifestParsed.PSObject.Properties.Name -contains 'integration_pr') -and
    $manifestParsed.integration_pr) {
    $manifestIntegrationPr = @([int]$manifestParsed.integration_pr)
}
$effectiveExcludePrNumbers = @($ExcludePrNumbers + $manifestIntegrationPr | Select-Object -Unique)
Write-Diag "PR-linkage discovery will exclude PR numbers: $($effectiveExcludePrNumbers -join ', ')"

$results = New-Object System.Collections.Generic.List[object]

foreach ($step in $manifestSteps) {
    $stepId = $step.step_id
    $issueNumber = [int]$step.issue_number
    $deps = @()
    if ($step.dependencies) { $deps = @($step.dependencies) }
    $stageLabel = if ($step.PSObject.Properties.Name -contains 'stage' -and $step.stage) { [string]$step.stage } else { '' }
    $isCutover = ($stepId -eq 'migration-cutover') -or ($stageLabel -ieq 'cutover')

    $result = [ordered]@{
        step_id          = $stepId
        issue_number     = $issueNumber
        stage            = $stageLabel
        dependencies     = $deps
        status           = 'unknown'   # ready | blocked | error | already-assigned | dispatched | would-dispatch
        reason           = ''
        base_branch      = $null
        prerequisite_prs = @()
        payload_preview  = $null
        dry_run          = $DryRun
    }

    Write-Diag "Resolving $stepId (issue #$issueNumber, deps=$($deps -join ','))"

    try {
        # 1) Already-assigned guard (always checked, regardless of readiness path)
        $assignees = Get-IssueAssignees -OwnerRepo $Repo -IssueNumber $issueNumber
        if (Test-AlreadyCopilotAssigned -Assignees $assignees -CopilotLogin $CopilotLogin) {
            $result.status = 'already-assigned'
            $result.reason = "Issue #$issueNumber already has a Copilot agent assignee ($($assignees -join ', ')); skipping to avoid duplicate assignment."
            $results.Add([pscustomobject]$result)
            continue
        }

        # 2) Cutover safety gate (independent of dependency resolution)
        if ($isCutover -and -not $AllowCutoverDispatch.IsPresent) {
            $result.status = 'blocked'
            $result.reason = 'Cutover step is never auto-dispatched; rerun with -Execute -AllowCutoverDispatch after explicit human approval. This script never merges/deploys/cuts over on its own regardless.'
            $results.Add([pscustomobject]$result)
            continue
        }

        # 3) Dependency resolution -> base_branch
        $baseBranch = $null
        $blockedReason = $null
        $prereqSummaries = @()

        if ($deps.Count -eq 0) {
            # Baseline step
            $baseBranch = $IntegrationBranch
        }
        elseif ($deps.Count -eq 1) {
            # Chain: single prerequisite
            $depId = $deps[0]
            $depStep = $stepsById[$depId]
            if (-not $depStep) {
                $blockedReason = "Dependency step_id '$depId' not found in manifest."
            } else {
                $depIssue = [int]$depStep.issue_number
                $linkage = Find-ClosingPullRequest -OwnerRepo $Repo -IssueNumber $depIssue -ExcludePrNumbers $effectiveExcludePrNumbers
                if ($linkage.Ambiguous) {
                    $ambiguousNums = @($linkage.Candidates | ForEach-Object { "#$($_.number)" }) -join ', '
                    $blockedReason = "Ambiguous prerequisite PR linkage for step '$depId' (issue #$depIssue): more than one PR body closes this exact issue number ($ambiguousNums); refusing to guess which is authoritative."
                } elseif (-not $linkage.Pr) {
                    $blockedReason = "No linked implementing pull request found yet for prerequisite step '$depId' (issue #$depIssue) -- no PR body (excluding $($effectiveExcludePrNumbers -join ', ')) contains a closing keyword (closes/fixes/resolves #$depIssue)."
                } else {
                    $pr = $linkage.Pr
                    $sameRepo = [bool]($pr.head.repo -and $pr.head.repo.full_name -eq $Repo)
                    $prereqSummaries += [ordered]@{
                        step_id = $depId; issue_number = $depIssue; pr_number = $pr.number
                        state = $pr.state; merged = [bool]$pr.merged; draft = [bool]$pr.draft
                        head_ref = $pr.head.ref; base_ref = $pr.base.ref; same_repo = $sameRepo
                        merge_commit_sha = $pr.merge_commit_sha
                    }
                    if (-not $sameRepo) {
                        $blockedReason = "Prerequisite PR #$($pr.number) for '$depId' is from a fork ($($pr.head.repo.full_name)), not the same repository ($Repo); refusing to treat a cross-fork PR as an eligible prerequisite."
                    } elseif ($pr.draft) {
                        $blockedReason = "Prerequisite PR #$($pr.number) for '$depId' is still a draft."
                    } else {
                        $hasFailure = Test-RequiredChecksHaveFailure -OwnerRepo $Repo -HeadSha $pr.head.sha
                        if ($hasFailure) {
                            $blockedReason = "Prerequisite PR #$($pr.number) for '$depId' has a failing/required check."
                        } else {
                            $approved = Test-PullRequestApproved -OwnerRepo $Repo -PrNumber $pr.number
                            if (-not $approved) {
                                $blockedReason = "Prerequisite PR #$($pr.number) for '$depId' has no APPROVED review (best-effort heuristic over /pulls/$($pr.number)/reviews; this does not evaluate branch-protection required-approval count or CODEOWNERS, which this script has no scope to query -- stated explicitly, not assumed)."
                            } elseif (-not $pr.merged) {
                                # Still open: route to its own head branch, which must exist.
                                $candidateRef = $pr.head.ref
                                if (Test-BranchExists -OwnerRepo $Repo -Branch $candidateRef) {
                                    $baseBranch = $candidateRef
                                } else {
                                    $blockedReason = "Resolved head ref '$candidateRef' for open prerequisite '$depId' (PR #$($pr.number)) no longer exists; refusing to target a nonexistent ref."
                                }
                            } else {
                                # Merged: prefer the integration branch if the merge commit is
                                # provably contained in it; otherwise fall back to the PR's own
                                # base ref, but only if that ref still exists AND the merge
                                # commit is provably contained there too (never trusted blindly).
                                if ([string]::IsNullOrWhiteSpace($pr.merge_commit_sha)) {
                                    $blockedReason = "Prerequisite PR #$($pr.number) for '$depId' is merged but has no merge_commit_sha yet; cannot verify containment."
                                } elseif (Test-CommitContainedIn -OwnerRepo $Repo -CommitSha $pr.merge_commit_sha -Branch $IntegrationBranch) {
                                    $baseBranch = $IntegrationBranch
                                } elseif ((Test-BranchExists -OwnerRepo $Repo -Branch $pr.base.ref) -and
                                          (Test-CommitContainedIn -OwnerRepo $Repo -CommitSha $pr.merge_commit_sha -Branch $pr.base.ref)) {
                                    $baseBranch = $pr.base.ref
                                } else {
                                    $blockedReason = "Prerequisite PR #$($pr.number) for '$depId' is merged (commit $($pr.merge_commit_sha)), but that merge commit is NOT provably contained in either the integration branch '$IntegrationBranch' or the PR's own base ref '$($pr.base.ref)' -- nested merge not yet integrated upward (or the target base was rewritten/force-pushed); refusing to target either ref until this is resolved."
                                }
                            }
                        }
                    }
                }
            }
        }
        else {
            # Fan-in: every prerequisite PR must be merged, same-repository, and its
            # merge_commit_sha must be provably CONTAINED in the integration branch --
            # not merely `base.ref == IntegrationBranch`, since a valid stacked workflow
            # may legitimately merge a fan-in PR into an intermediate/prerequisite branch
            # before that branch is itself merged up into the integration branch.
            $allContained = $true
            $failures = @()
            foreach ($depId in $deps) {
                $depStep = $stepsById[$depId]
                if (-not $depStep) {
                    $allContained = $false
                    $failures += "dependency step_id '$depId' not found in manifest"
                    continue
                }
                $depIssue = [int]$depStep.issue_number
                $linkage = Find-ClosingPullRequest -OwnerRepo $Repo -IssueNumber $depIssue -ExcludePrNumbers $effectiveExcludePrNumbers
                if ($linkage.Ambiguous) {
                    $allContained = $false
                    $ambiguousNums = @($linkage.Candidates | ForEach-Object { "#$($_.number)" }) -join ', '
                    $failures += "'$depId' (issue #$depIssue): ambiguous PR linkage ($ambiguousNums); refusing to guess"
                    continue
                }
                if (-not $linkage.Pr) {
                    $allContained = $false
                    $failures += "'$depId' (issue #$depIssue): no linked implementing PR yet"
                    continue
                }
                $pr = $linkage.Pr
                $sameRepo = [bool]($pr.head.repo -and $pr.head.repo.full_name -eq $Repo)
                $prereqSummaries += [ordered]@{
                    step_id = $depId; issue_number = $depIssue; pr_number = $pr.number
                    state = $pr.state; merged = [bool]$pr.merged; draft = [bool]$pr.draft
                    head_ref = $pr.head.ref; base_ref = $pr.base.ref; same_repo = $sameRepo
                    merge_commit_sha = $pr.merge_commit_sha
                }
                if (-not $sameRepo) {
                    $allContained = $false
                    $failures += "'$depId' PR #$($pr.number) is from a fork ($($pr.head.repo.full_name)), not $Repo"
                } elseif (-not $pr.merged) {
                    $allContained = $false
                    $failures += "'$depId' PR #$($pr.number) not yet merged (state=$($pr.state))"
                } elseif ([string]::IsNullOrWhiteSpace($pr.merge_commit_sha)) {
                    $allContained = $false
                    $failures += "'$depId' PR #$($pr.number) is merged but has no merge_commit_sha yet"
                } elseif (-not (Test-CommitContainedIn -OwnerRepo $Repo -CommitSha $pr.merge_commit_sha -Branch $IntegrationBranch)) {
                    $allContained = $false
                    $failures += "'$depId' PR #$($pr.number) merge commit $($pr.merge_commit_sha) is NOT yet provably contained in integration branch '$IntegrationBranch' (merged into '$($pr.base.ref)' but not (yet) integrated upward)"
                }
            }
            if ($allContained) {
                if (Test-BranchExists -OwnerRepo $Repo -Branch $IntegrationBranch) {
                    $baseBranch = $IntegrationBranch
                } else {
                    $blockedReason = "Integration branch '$IntegrationBranch' does not exist; refusing to target a nonexistent ref."
                }
            } else {
                $blockedReason = "Fan-in prerequisites not all satisfied: $($failures -join '; ')"
            }
        }

        $result.prerequisite_prs = $prereqSummaries

        if ($blockedReason) {
            $result.status = 'blocked'
            $result.reason = $blockedReason
            $results.Add([pscustomobject]$result)
            continue
        }

        if (-not $baseBranch) {
            $result.status = 'blocked'
            $result.reason = 'Could not resolve a base_branch (unexpected state).'
            $results.Add([pscustomobject]$result)
            continue
        }

        if (-not (Test-BranchExists -OwnerRepo $Repo -Branch $baseBranch)) {
            $result.status = 'blocked'
            $result.reason = "Resolved base_branch '$baseBranch' does not exist on remote; refusing to dispatch to a nonexistent ref."
            $results.Add([pscustomobject]$result)
            continue
        }

        $result.base_branch = $baseBranch

        # 4) Build minimal payload (omit unspecified optional fields entirely)
        $agentAssignment = [ordered]@{
            target_repo = $Repo
            base_branch = $baseBranch
        }
        if ($step.PSObject.Properties.Name -contains 'custom_instructions' -and $step.custom_instructions) {
            $agentAssignment.custom_instructions = [string]$step.custom_instructions
        }
        if ($step.PSObject.Properties.Name -contains 'custom_agent' -and $step.custom_agent) {
            $agentAssignment.custom_agent = [string]$step.custom_agent
        }
        if ($step.PSObject.Properties.Name -contains 'model' -and $step.model) {
            $agentAssignment.model = [string]$step.model
        }

        $payload = [ordered]@{
            assignees        = @($CopilotLogin)
            agent_assignment = $agentAssignment
        }
        $result.payload_preview = $payload

        $target = "issue #$issueNumber ($stepId) -> base_branch '$baseBranch'"

        if ($DryRun) {
            $result.status = 'would-dispatch'
            $result.reason = "Dry run (default). Would POST assignment: $target. Re-run with -Execute to dispatch."
            $results.Add([pscustomobject]$result)
            continue
        }

        if ($isCutover -and -not $AllowCutoverDispatch.IsPresent) {
            # Defense in depth; already caught above, but never fall through on cutover.
            $result.status = 'blocked'
            $result.reason = 'Cutover dispatch blocked (AllowCutoverDispatch not set).'
            $results.Add([pscustomobject]$result)
            continue
        }

        if ($PSCmdlet.ShouldProcess($target, "POST /repos/$Repo/issues/$issueNumber/assignees")) {
            $bodyJson = $payload | ConvertTo-Json -Depth 10 -Compress
            $postResp = Invoke-GhApi -Endpoint "repos/$Repo/issues/$issueNumber/assignees" -Method 'POST' `
                -ExtraArgs @('--input', '-') -StdInText $bodyJson
            if ($postResp.Ok) {
                $result.status = 'dispatched'
                $result.reason = "Dispatched: $target."
            } else {
                $result.status = 'error'
                $result.reason = "Dispatch POST failed for $target`: $($postResp.Error)"
            }
        } else {
            $result.status = 'would-dispatch'
            $result.reason = "ShouldProcess declined (-WhatIf). Would POST assignment: $target."
        }

        $results.Add([pscustomobject]$result)
    }
    catch {
        # Hard error anywhere in this step's resolution -- surfaced explicitly,
        # never coerced into 'blocked' or a silent default. Other steps continue.
        $result.status = 'error'
        $result.reason = $_.Exception.Message
        $results.Add([pscustomobject]$result)
    }
}

$hadHardError = [bool]($results | Where-Object { $_.status -eq 'error' })

$summary = [ordered]@{
    repo               = $Repo
    integration_branch = $IntegrationBranch
    manifest_path      = (Resolve-Path -LiteralPath $ManifestPath).Path
    mode               = if ($DryRun) { 'dry-run' } else { 'execute' }
    allow_cutover      = [bool]$AllowCutoverDispatch.IsPresent
    generated_at_utc   = (Get-Date).ToUniversalTime().ToString('o')
    had_hard_error     = $hadHardError
    steps              = $results
}

$summary | ConvertTo-Json -Depth 12

if ($hadHardError) { exit 2 } else { exit 0 }
