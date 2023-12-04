## Version 0.6.1

- Revised documentation for `ggscreeplot()`
- `data(crime)` used in README giving a more complete example.
- Fixed links reported as 301s

## Version 0.6.0

- Use `geom_polygon()` rather than `geom_path()` for ellipses to allow them to be filled.
- Added `var.factor` argument to expand or reflect the variable vectors
- Moved points/labels code earlier so ellipses and variable vectors are not obscured
- Replaced internal calculation of ellipses with `stat_ellipse()`
- Now allow ellipses to be filled (`geom_polygon()`) or unfilled (`geom_path()`)
- Added hex logo
- Added iris example to README
- Removed Imports: dplyr, purrr as no longer needed

## Version 0.56

- Fixed many documentation errors and warnings
- Use roxygen2 for documentation
- added `varname.color` to replace fixed `muted("red")`
- tweaked arrow style
- increased default thickness of variable vectors
- add `point.size` argument


