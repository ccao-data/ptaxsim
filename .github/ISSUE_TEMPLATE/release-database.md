---
name: Release checklist (database)
about: Steps to take before releasing a new PTAXSIM database version
title: "Database release [VERSION]"
labels: release
---

# Database Release Checklist

- [ ] Make your code updates and commit them in a branch
- [ ] Make any necessary updates to the raw data. If necessary, force add the raw data files if they are ignored by git. Be sure to update `.gitattributes` such that the raw data files are tracked by git LFS
- [ ] Run the raw data scripts (anything in `data-raw/`) that prepare and clean the data. These scripts will save the cleaned data to a staging area in S3. Ensure that the relevant S3 keys in the PTAXSIM bucket are updated using the AWS console or API
- [ ] Inside `data-raw/create_db.R`, increment the `db_version` variable following the [schema outlined above](#schema)
- [ ] If necessary, also increment the `requires_pkg_version` variable in `data-raw/create_db.R`
- [ ] Increment the database versions in `DESCRIPTION` file:
  - `Config/Requires_DB_Version`: This is the minimum database version required for this version of the package. It should be incremented whenever there is a breaking change
  - `Config/Wants_DB_Version`: This is the maximum database version required for this version of the package. It is the version of the database pulled from S3 during CI/testing on GitHub
- [ ] If necessary, be sure to update the SQL statements in `data-raw/create_db.sql`. These statements define the structure of the database
- [ ] Run the database generation script `data-raw/create_db.R`. This will create the SQLite database file by pulling data from S3. The file will be generated in a temporary directory (usually `/tmp/Rtmp...`), then compressed using `pbzip2` (required for this script)
- [ ] Using the command line, grab the final compressed database file from the temporary directory (found at `db_path` after running `data-raw/create_db.R`) and move it to the project directory. Rename the file `ptaxsim-<TAX_YEAR>.<MAJOR VERSION>.<MINOR VERSION>.db.bz2`
- [ ] Decompress the database file for local testing using `pbzip2`. The typical command will be something like `pbzip2 -d -k ptaxsim-2021.0.2.db.bz2`
- [ ] Rename the decompressed local database file to `ptaxsim.db` for local testing. This is the file name that the unit tests and vignettes expect
- [ ] Use [sqldiff](https://www.sqlite.org/sqldiff.html) or a similar tool to compare the new database file to the previous version. Ensure that the changes are expected
- [ ] Restart R. Then run the unit tests (`devtools::test()` in the console) and vignettes (`pkgdown::build_site()` in the console) locally
- [ ] Knit the `README.Rmd` file to update the database link at the top of the README. The link is pulled from the `ptaxsim.db` file's `metadata` table
- [ ] If necessary, update the database diagrams in the README with any new fields or tables
- [ ] Move the compressed database file to S3 for public distribution. The typical command will be something like `aws s3 mv ptaxsim-2021.0.2.db.bz2 s3://ccao-data-public-us-east-1/ptaxsim/ptaxsim-2021.0.2.db.bz2`
- [ ] Use the S3 console (or API) to make the database file public via an ACL
- [ ] Push the code updates on GitHub. Wait for the resulting CI pipeline to finish
- [ ] If there are no pipeline errors, merge the branch to master
