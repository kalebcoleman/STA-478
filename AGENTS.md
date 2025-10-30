# Repository Guidelines

## Project Structure & Module Organization
- Root contains package metadata (`DESCRIPTION`, `NAMESPACE`, `.Rproj`) and the `renv/` environment lock files.
- All production R scripts belong in `R/` (e.g., `best_subset_search.R`, `bootstrap_performance.R`, Monte Carlo utilities). Keep one exported function per file when possible.
- Tests live in `tests/testthat/`; mirror each new feature with a `test_<feature>.R` file that explains the behavior being checked.
- Reference PDFs and Rmds from STA478 reside in `docs/`. Treat them as read-only unless explicitly updating coursework notes.

## Coding Style & Naming Conventions
- Follow tidyverse-flavored R style: snake_case identifiers, `<-` for assignment, and ≤80-character lines.
- Indent with two spaces; group related code with brief comments.
- Document every exported function using roxygen (`@param`, `@return`, `@examples`, `@export`). Include usage snippets drawn from STA478 labs when helpful.
- Prefer base R and `stats` unless a package is already declared in `DESCRIPTION`; note any new dependency additions in the PR description.

## Testing Guidelines
- Tests use `testthat` (edition 3). Name files `test_<topic>.R` and explain each expectation in plain language.
- Provide at least one smoke or edge-case test for every new user-facing function. Reuse seeds where reproducibility matters.

## Agent-Specific Notes
- Prioritize maintainability over minimal edits; favor clarity even if the diff is slightly larger.
- When generating new helpers, supply roxygen examples, update `NAMESPACE`, and add a matching test file.
- Default to tidyverse-aligned idioms when writing examples, but avoid introducing tidyverse dependencies unless cleared in advance.
