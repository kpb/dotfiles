# 001 Adopt Conventional Commits

## Date

01 December 2025

## Status

Accepted

## Context

This repository currently has inconsistent commit message styles, which makes it harder to understand change history,
automate changelogs, and safely derive version numbers. Even though this is a single person project, I want to use best
practices and follow a simple, documented commit message convention.

## Decision

I will adopt [Conventional Commits][conventional-commits] v1.0.0 for all commits in this project. In particular:

- Commit messages **must** follow the pattern:
  `type[optional scope]: short description`
- Standard types include (at minimum): `feat`, `fix`, `docs`, `chore`, `refactor`, `test`, `build`, `ci`.
- Breaking changes **must** be indicated using `!` (e.g., `feat!:`) **or** a `BREAKING CHANGE:` footer as defined in the spec.
- Future tooling (e.g., commit linting and automated release/versioning) **will assume** this format.

## Consequences

- **Positive**
  - Easier to understand the intent and impact of changes via `feat` / `fix` / and the _scope_ of the change.
  - Enables automated changelog generation and release flows aligned with [Semantic Versioning][semver].
  - Provides consistent expectations.

- **Negative / Risks**
  - Existing commit history remains inconsistent; some tooling benefits apply only from this decision forward.

---

[conventional-commits]: https://www.conventionalcommits.org/en/v1.0.0/
[semver]: https://semver.org/
