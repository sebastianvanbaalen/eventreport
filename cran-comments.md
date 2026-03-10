Changes in this version:
- Added citation information for the associated methods paper.
- Added note that users of the enclosed MAVERICK dataset should also cite the data paper.
- Updated DESCRIPTION, CITATION, startup message, and documentation.

── R CMD check results ───────────────────────────────────────────────────────────────────────────────────── eventreport 0.1.2 ────
Duration: 59.8s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded

## Win-Builder (R-devel)

0 errors | 0 warnings | 1 note

The NOTE flags "Baalen" and "glund" (from "Höglund") as possibly
misspelled words in DESCRIPTION. These are the authors' surnames
and are spelled correctly.

## URL check note

`urlchecker::url_check()` reports 403 (Forbidden) errors for several DOI
links (doi.org/10.1093/isq/sqag014, doi.org/10.1093/jopres/xjaf012,
doi.org/10.1111/ajps.12398, doi.org/10.1177/0022343314523807). These DOIs
are valid and resolve correctly in a browser. The 403 responses are caused
by the publisher servers rejecting automated requests.
