## Test environments

* Windows 10, R version 4.2.3 (2023-03-15 ucrt)
* win-builder, R Under development (unstable) (2023-12-29 r85751 ucrt)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Version 0.6.2

This is a modest upgrade to the initial release, adding a number of features.

- corrected small bugs in `ggbiplot() and `ggscreeplot()`
- `reflect()` now also reflects supplementary variables from `FactoMineR::PCA()`
- added support for `ade4::dudi.pca()`
- `ggbiplot()` gains an `axis.title` argument
- `ggscreeplot()` gains `color`, `shape`, `linetype` and `linewidth` arguments
- Added `get_SVD()` intended the simplify the interface to various PCA functions.
- Now use `get_SVD()` in `ggbiplot()` and `ggscreeplot()`
- Extend some examples, requiring Depends:ggplot2

