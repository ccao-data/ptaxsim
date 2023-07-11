---
name: Release checklist (package)
about: Steps to take before releasing a new PTAXSIM package version
title: "Package release [VERSION]"
labels: release
---

# Package Release Checklist

- [ ] Make your code updates and commit them in a branch
- [ ] Make sure your code updates meet styling and linting requirements:
  - *Styling*: Run `styler::style_pkg()` in the console
  - *Linting*: Run `lintr::lint_package()` in the console
- [ ] Build any documentation updates by running `devtools::document()` in the console
- [ ] Run the build and test processes locally to check for errors:
  - *Build*: In RStudio, go to the **Build** tab, then hit **Check**
  - *Test*: In RStudio, go to the **Build** tab, then hit **Test** OR `devtools::test()` in the console
- [ ] Check the vignettes by building them locally. Run `pkgdown::build_site()` in the console
- [ ] Rebuild the README manually. Knit `README.Rmd` from within RStudio
- [ ] Increment the package version in the `DESCRIPTION` file appropriately, following the schema laid out in the README
- [ ] If the code updates were so substantial that the current database also needs to be updated, follow the database release checklist issue template
- [ ] Push the code updates to GitHub. Wait for the resulting CI pipeline to finish
- [ ] If there are no pipeline errors, merge the branch to master
- [ ] Wait for the merge CI pipeline to finish. If there are no errors, cut a new release from master. Create a new git tag with the version number and title the release with the same version number. Be sure to add a changelog detailing what you updated
