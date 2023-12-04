# Re: biplots

https://stackoverflow.com/questions/6578355/plotting-pca-biplot-with-ggplot2

This discussion reveals how quirky it is to produce biplots, and consequently, a wide range of
suggested solutions that address different sets of design criteria, leading to a collection
of idiosyncratic examples, none of which are at all general.


**Criteria for biplots**

* PCA can be computed in with a variety of functions, but they differ substantially in how the components
in the returned object are named. This causes confusion in seeking a general solution. E.g., `obj <- stats::prcomp()` 
calls the loadings `obj$rotation` and the observation scores `obj$x` and names the
columns `PC1:PCp`, while `stats::princomp()` calls these `loadings` and `scores`
but names the components `Comp.1 : Comp.p`. 

* PCA loadings are used to draw the variable vectors, but as eigenvectors they have an arbitrary direction.
It should be possible to specify `reflect = c(TRUE, FALSE)` in the call to say that the X variable should
be plotted as -X.

* There are different scalings for the observation points and variable vectors, but in the usual "symmetric"
PCA solution, it is the relative lengths of the variable vectors that matter.  It should be possible to
specify an expansion factor (as in `stats::biplot(..., expand=))` to multiply the variable vectors by
a factor to make them more visible.

* Observation scores in PCA space: Sometimes you want to label them all, sometimes, label none, sometimes
label only a selected few.


