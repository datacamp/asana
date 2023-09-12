
<!-- README.md is generated from README.Rmd. Please edit that file -->
asana
=====

An R package for accessing the Asana API.

Installation
------------

One day, you will be able to install the released version of `asana` from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("asana")
```

For now, install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("datacamp/asana")
```

Get an API token from Asana by doing the following steps. More information is under "Personal Access Token" [here](https://asana.com/guide/help/api/api).

- Click your profile photo from the topbar in Asana
- Select My Profile Settings…
- Open the Apps tab
- Click Manage Developer Apps
- Click + Create New Personal Access Token

Make sure to set it in your `.Renviron` file. You can run `usethis::edit_r_environ()` to automatically pull up your `.Renviron` file in RStudio for editing.

```
ASANA_ACCESS_TOKEN="xxxxxx"
```

where `xxxxx` is the Token generated.

<!-- just checking if i still have write access to the repo -->



To add a new task,  simply:
``` r
project_id <- "XXXXXXXXX"
asn_tasks_create(
  projects = project_id,
  name =  "testing R!"
  )
```
