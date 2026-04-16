# Changelog

## 1.1.0

### Background

This release features a significant number of changes to the database schema
and to package functions to accommodate changes that the Clerk and the
Treasurer made to the data they report in 2024. Source data from the Clerk and
the Treasurer forms the core of the PTAXSIM database, so any changes they make
need to be reflected in this package, too.

The Clerk and the Treasurer changed their data reporting this past year because
they both completed a migration from a legacy AS/400 mainframe system to a
new integrated property tax system of record.

Read on for a detailed description of the changes we made to the PTAXSIM
database and functions to handle these changes in the source data.

### Breaking changes

- **Added new `agency.agency_*_24` columns to handle changing agency numbers
  in 2024**.
    - The new columns include:
      - `agency.agency_change_24` (boolean, required): Whether the agency's
        number changed in 2024.
      - `agency.agency_num_24` (string, optional): The agency's new number
        starting in 2024. Null if the agency number did not change in 2024.
      - `agency.agency_name_24` (string, optional): The agency's name starting in
        2024. Null if the agency number did not change in 2024.
      - **How this change affects you**: If you maintain code that analyzes
        agencies over time, and you want to update your code to include 2024
        data, you will need to use these new columns to handle changes to
        agency numbers. (_TK: Link to vignette_)
- **Added a new column `agency_fund.fund_type_num` to handle changing fund
  numbers in 2024**. Prior to 2024, fund numbers (`agency_fund.fund_num`)
  consisted of three digits, and funds with the same `fund_num` in different
  agencies always referred to the same type of fund. In 2024, the Clerk changed
  their fund numbers so that they consist of six digits, and are no longer
  guaranteed to refer to the same type of fund across agencies. However, the
  first three digits of a fund number are still guaranteed to refer to a
  consistent fund "type", so we introduced a new column
  `agency_fund.fund_type_num` to represent this new notion of a "fund type".
    - **How this change affects you**: If you use `agency_fund.fund_num` to
      track certain types of funds across agencies, you will need to switch to
      `agency_fund.fund_type_num`.
- **Changed the `agency_fund_info` table so that it is now unique by
  `(agency_num, fund_num)` instead of `(fund_num)`**. As mentioned above,
  fund identifiers are no longer consistent across agencies. As a result, we
  have changed the `agency_fund_info` table so that it includes `agency_num`
  in its primary key.
    - **How this change affects you**: If you use the `agency_fund_info` table
      and treat it as unique by `fund_num`, you will need to update your
      code to add `agency_num` when joining to this table.
- **Added a new table `pin_tif_distribution` and associated lookup function
  `lookup_pin_tif()` to handle a methodological change to the TIF
  revenue share calculation.**
    - `lookup_pin_tif()` will never return rows with `year < 2024`.
    - `lookup_tif()` will never return rows with `year > 2024`.
    - **How this change affects you**:
      - Update `tax_bill()` function calls in TIF counterfactuals to pass in
        `pin_tif_dt` for years starting in 2024
- **Dropped a few columns that the Clerk has removed from its agency reports**.
    - These columns are:
      - `agency_fund.ptell_reduced_ind` (PTELL-reduced levy indicator)
      - `agency.reduction_type` (PTELL-reduced levy type)
      <!-- TODO: Is this true? -->
      - `agency.total_non_cap_ext` (Total non-capped extension for an agency)
      - **How this change affects you**: If you use any of the columns listed
        above, you will need to remove them from your code.
- **Consolidated non-Cook County EAV columns into a new column
  `agency.cty_overlap_eav`**.
    - These columns have been removed and their values are now included in the
      `agency.cty_overlap_eav` column:
      - `agency.cty_dupage_eav`
      - `agency.cty_lake_eav`
      - `agency.cty_will_eav`
      - `agency.cty_kane_eav`
      - `agency.cty_mchenry_eav`
      - `agency.cty_dekalb_eav`
      - `agency.cty_grundy_eav`
      - `agency.cty_kankakee_eav`
      - `agency.cty_kendall_eav`
      - `agency.cty_lasalle_eav`
      - `agency.cty_livingston_eav`

### Improvements

- **Added a new exemption column `pin.exe_vet_dis_100`**. This column
  corresponds to the new 100% disability level for the [Veterans with
  Disabilities Exemption](https://www.cookcountyassessoril.gov/veterans-disabilities-exemption).
  The lookup function `lookup_pin()` now returns this column.
    - **How this change affects you**: You may use this column if you would
      like to analyze this exemption.
- **Added a new column `agency.authority_num`**. We expect this column to be
  more useful in future years when the Clerk continues migrating from the
  core "Agency" entity to the "Authority" entity.
    - **How this change affects you**: We recommend ignoring this column for
      now, because it is currently duplicative of `agency_num` and we don't
      have full confidence in our understanding of the Clerk's plans for this
      column in the future. However, it remains available if you are interested
      in investigating it.
- **Added support for pre-release database versions**. The code now supports
  database versions with pre-release suffixes like `2024.0.0-alpha.1` to
  indicate database versions that are unreleased but available for testing.
    - **How this change affects you**: This change will not affect you as a
      PTAXSIM user. It is only useful for PTAXSIM maintainers and beta testers.
- **Updated the TIF vignette to demonstrate the correct way to handle the new
  TIF revenue share calcluation** ([#77](https://github.com/ccao-data/ptaxsim/pull/77))
    - **How this change affects you**: You should read the latest version of the
      vignette if you use PTAXSIM for TIF analysis.

### Bug fixes

- **Lowered the memory usage of the `pin_geometry` extraction script**
  ([#67](https://github.com/ccao-data/ptaxsim/pull/67)). This script previously
  struggled to complete on our 160gb server. It now runs correctly.
    - **How this change affects you**: This change will not affect you as a
      PTAXSIM user. It is only useful for PTAXSIM maintainers.
