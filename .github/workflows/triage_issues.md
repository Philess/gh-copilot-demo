---
on:
  schedule: daily
permissions:
  contents: read
  issues: read
  pull-requests: read
safe-outputs:
  add-labels:
    allowed: [bug, enhancement, question, feature, false-positive]  # restrict to specific labels
    max: 2                       # max labels (default: 3)
---

## Daily Issues Triage

This repo is public and is used as a sample for tutorial. External developers often create issues while running the workshop which must be identified as "false positive" issues and closed automatically by this workflow.

Only remove issues that has been created 2 at least two days ago to let the developer time to finish his workshop.

## What issues must be closed

- Issues about adding a Cart Feature: this is part of the workshop
- Issues that looks like tests: if the title, description doesn't look like a real issue for the evolution of the repo itself as an example code for a developer hands-on-lab

## What to do with others

Regarding the content of the issue, add a label to help categorize:
- bug
- question
- feature
- enhancement
