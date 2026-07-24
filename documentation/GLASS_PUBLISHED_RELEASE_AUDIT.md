# Site-Reference and Watershed Audit

Updated: 2026-07-23

The GlASS site-reference audit checked coordinates, drainage areas, site
aliases, discharge metadata, shapefile names, coordinate systems, and source
citations against agency, LTER, PI, repository, and publication records.

The review found misspelled site names, encoding problems, incorrect coordinate
assignments, drainage-area unit errors, duplicate aliases, missing provenance,
and watersheds associated with the wrong station. Some polygons were rejected
because they were corrupt, incomplete, far from the sampling outlet, or
inconsistent with the reported drainage area.

Reported drainage areas remain the table authority when supported by a source.
Sites without a defensible watershed remain blank rather than receiving a
fallback polygon that failed review.

The publication table records first inclusion, accepted chemistry and discharge
data, and accepted spatial data separately. Row-level decisions and citations
remain in the site-reference table rather than in this summary.
