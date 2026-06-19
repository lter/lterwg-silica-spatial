# Repository Style Notes

For R scripts in this repository:

- Use `librarian::shelf(...)` for package loading.
- Do not add `#!/usr/bin/env Rscript` shebangs.
- Do not use `print()` statements for console output in new or actively maintained scripts.
- Avoid defensive preflight blocks like `if (!file.exists(...)) stop(...)` unless explicitly requested.
- Keep setup code explicit and readable: direct paths, direct reads, and clear object names are preferred over environment-variable wrappers and generic helper functions.
- For QA/QC scripts, prefer clearly named variable lists and one direct check per diagnostic over dense one-size-fits-all `case_when()` blocks. Simple `grep(..., value = TRUE)` is fine for obvious repeated column families.
- QA/QC plots and generated tables should save to the project Box data folder, not into the GitHub repo, unless the user explicitly requests repo-local output.
- Do not add Python files or Python calls to this repository.
- Do not touch Nick Lyon's original spatial extraction scripts under `03_spatial_extraction/extraction_scripts/` unless the user explicitly asks for edits there. Existing style in those scripts is an explicit exception to the repo cleanup preferences.
