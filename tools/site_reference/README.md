# Site-Reference Tools

This folder contains reusable tools for the AppEEARS site-reference table.

| Script | Purpose |
|---|---|
| `validate_site_reference.R` | Check coordinates, versions, sources, and watershed fields |
| `assign_major_drainage_basins.R` | Assign and review major drainage basins |

Typical use:

```bash
Rscript tools/site_reference/validate_site_reference.R --input PATH
Rscript tools/site_reference/assign_major_drainage_basins.R --input PATH --output PATH
```

Keep reviewed basin exceptions in a TSV file rather than adding site names to
the scripts.
