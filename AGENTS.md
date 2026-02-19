# Session Log & Repository Guidelines

## Latest Session (104d0967-0479-4ec7-989b-13b823fa8af0)
**Task**: Simplify `edoc-save-current-edition()` to use Emacs customization API
- Modified `pws-system.org` (lines 1170-1185) to use `customize-save-variable`
- Regenerated `edoc.el` via `org-babel-tangle`
- Committed and pushed: "Simplify edoc-save-current-edition with customize-save-variable"
- Status: ✅ Complete - function now uses standard Emacs customization instead of manual file parsing

## Project Structure & Module Organization
The repository is Emacs-first: production code lives in `*.el` modules (e.g., `edoc-org-program.el`, `edoc-dashboard.el`), while their literate Org counterparts (`pws-*.org`) are the single sources of truth.  Tests reside under `test-*.el`, and working content (editions, assets) is pulled from the edition directories managed by `edoc-dashboard`.  Always edit the Org files and re-tangle (`M-x org-babel-tangle`) to regenerate the corresponding Elisp.

## Build, Test, and Development Commands
- `emacs -Q --batch -l ert -l test-roman-numerals.el -f ert-run-tests-batch-and-exit`: run the available ERT suite headlessly.
- `emacs -Q --batch -l edoc-dashboard.el -f edoc-dashboard-refresh`: rebuild dashboard buffers to confirm UI changes.
- `emacs -Q --batch -l pws-org-program.org --eval '(org-babel-tangle)'`: non-interactive tangling of all literate files.

## Coding Style & Naming Conventions
Write Emacs Lisp in UTF-8 with two-space indentation and no tabs.  Use lexical binding (`-*- lexical-binding: t -*-`) and prefer `cl-lib` helpers only when required.  Internal helpers are named with the `edoc--` or `pws-` prefixes; interactive commands use full words (e.g., `edoc-export-relatori-mailinglist`).  Keep line length reasonable (<100 chars) and favour ASCII punctuation inside strings unless Markdown output demands UTF-8 symbols.

## Testing Guidelines
Tests use ERT; place new suites in `test-*.el` and name groups `test-edoc-*`.  Every feature touching programme generation should stub minimal Org fixtures and assert on the produced plist or Markdown.  Run the batch ERT command before submitting; aim for deterministic tests without network or filesystem side effects outside the workspace.

## Commit & Pull Request Guidelines
Commits follow the short, imperative mood pattern (“Add mailing list exporter”), scoped to a focused change set and referencing the Org source that was tangled.  When proposing a pull request, include:
- Summary of functional impact and updated commands.
- Mention of regenerated `*.el` files and relevant `org-babel-tangle` steps.
- Test evidence (ERT command output) and screenshots when UI buffers change.
Link issues with `Refs #123` when applicable and request review from maintainers responsible for the touched module (`edoc-*` vs `pws-*`).

