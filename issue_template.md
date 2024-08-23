---
name: Bug report or feature request
about: Describe a bug you've seen or make a case for a new feature
---

# PLEASE READ: Making a new issue for parsnip


Please follow the template below. 

If the question is related at all to a specific data analysis, please include a **minimal reprex** (reproducible example). If you've never heard of a reprex before, start by reading "[What is a reprex](https://github.com/tidyverse/reprex#what-is-a-reprex)", and follow the advice further down that page.

Tips: 

 * Here is a good example issue: [#139](https://github.com/tidymodels/parsnip/issues/139#issue-404108897)
 
 * **Issues without a reprex will have a lower priority than the others.** 

 * We don't want you to use confidential data; you can blind the data or simulate other data to demonstrate the issue. The functions [`caret::twoClassSim()`](https://www.rdocumentation.org/packages/caret/versions/6.0-84/topics/SLC14_1) or [`caret::SLC14_1()`](https://www.rdocumentation.org/packages/caret/versions/6.0-84/topics/SLC14_1) might be good tools to simulate data for you. 

 * Unless the problem is explicitly about parallel processing, please run sequentially. 

   * Even if it about parallel processing, please make sure that it runs sequentially first.

 * Please use `set.seed()` to ensure any randomness in your code is reproducible.

 * Please check <https://stackoverflow.com/> or <https://community.rstudio.com/> to see if someone has already asked the same question (see: [Yihui's Rule](https://yihui.name/en/2017/08/so-gh-email/)). 

 * You might need to install these:

```r
install.packages(c("reprex", "sessioninfo"), repos = "http://cran.r-project.org")
```

<br>

When are ready to file the issue, please delete the parts above this line:
< -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->

## The problem

I'm having trouble with ... or 

Have you considered ... 

## Reproducible example

Copy your code to the clipboard and run:

```r
reprex::reprex(si = TRUE)
```
