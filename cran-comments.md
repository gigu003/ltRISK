## R CMD check results

0 errors | 0 warnings | 0 notes

The local `--as-cran` check rebuilt the vignette and generated both the PDF and
HTML manuals successfully. CRAN incoming network checks could not query a
configured Bioconductor mirror, but the check itself completed with status OK.

## Test environments

- local macOS Tahoe 26.5.2, R 4.5.2
- GitHub Actions is configured for macOS and Windows with R release, and
  Ubuntu with R devel, release, and oldrel-1. Results will be confirmed before
  submission.

## Submission type

This is a new submission.

## Additional checks

- `urlchecker::url_check()`: all declared package URLs are correct.
- The GitHub repository is used as the temporary pkgdown canonical URL. The
  GitHub Pages URL will replace it after the first successful site deployment.
- `spelling::spell_check_package()`: only package-specific terms, abbreviations, function names, author names, and LaTeX fragments were reported; these have been added to `inst/WORDLIST` where appropriate.
- External regression tests reproduce the published Fay et al. (2003) results
  and all 190 age intervals in the NCI DevCan 6.7.5 SEER 21 all-sites Gamma
  confidence-interval table at the displayed precision.
- Because this is the initial public release and the pre-release interfaces had
  no external users, experimental `ltr`, `cumrisk`, `cumrate`, and `estimate`
  interfaces were removed rather than retained as deprecated aliases.

## Notes

None.