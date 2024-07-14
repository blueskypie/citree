# citree 0.1.1

* Cases leading to crashes of `partykit::ctree()` are handled, e.g. `Inf`
     and `-Inf` are converted to `NA` to avoid the following errors:
      " 'breaks' are not unique"
