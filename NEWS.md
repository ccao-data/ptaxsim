# ptaxsim 1.1.0

## Background

This release adds support for tax year 2024. It is intended for use with the
`2024.0.x` database version.

The release also features a significant number of changes to the database schema
and to package functions to accommodate changes that the Clerk and the
Treasurer made to the data they report in 2024. Source data from the Clerk and
the Treasurer forms the core of the PTAXSIM database, so any changes they make
need to be reflected in this package, too.

The Clerk and the Treasurer changed their data reporting this past year because
they both completed a migration from a legacy AS/400 mainframe system to a
new integrated property tax system of record.

The main changes that the Clerk and the Treasurer made in 2024 include:

- **Changed the TIF revenue share calculation so that it is performed at the
  PIN-level rather than the tax-code-level**. This means that TIF increment is
  now calculated based on the difference between the frozen and current taxable
  EAV _for each individual PIN in a TIF_, rather than the difference between the
  frozen and current taxable EAV of its tax code as a whole. As a result of this
  change, a TIF can now have a positive increment even if the total taxable EAV
  of its tax code has declined since its establishment, as long as individual
  PINs within the tax code have experienced EAV growth during that time.
- **Changed a small number of agencies to funds**. An example is the City of
  Chicago library fund, whose agency number (`030210001`) had a non-zero levy
  in the `agency` table in 2023, but has a $0 levy in 2024. Instead, the library
  fund's levy is listed in the `agency_fund` table for 2024 using the agency
  number for the City of Chicago (`030210000`). By reporting this kind of levy
  as a fund underneath a parent agency rather than an independent agency, the
  format of the Clerk's data is now more similar to the format that
  agencies like the City of Chicago use for their own internal accounting.
    - In 2024, only 78 agencies were affected by this change, representing
      roughly 4% of all agencies. All of the changed agencies fit into one
      of the following categories:
        - Mental health districts
        - Public health funds
        - General assistance funds
        - Road and bridge funds
        - Library funds
    - As part of this change, the Clerk also consolidated levy adjustments for
      every agency that changed to a fund in 2024, rolling them into a single
      levy adjustment that applies to the parent agency as a whole. For example,
      the Clerk used to report separate levy adjustments using the fund number
      `408000` for the Berwyn general assistance fund (agency number `020020002`),
      the Berwyn mental health fund (agency number `020020004`), and the Berwyn
      public health fund (`020020005`); prior to 2024, each of these funds had
      its own row in the `agency_fund` table. However, in 2024 they consolidated
      those levy adjustments into a single fund with number `408000` for the
      township agency (`020020000`), meaning we can no longer analyze levy
      adjustments on a per-fund basis in Berwyn.
    - To see the full list of agencies and funds that changed in this manner in
      2024, run the following SQL queries against the 2024 PTAXSIM database:

```sql
-- See all agencies that have changed to funds
SELECT * FROM agency_crosswalk;

-- See the same change at the fund level
SELECT * FROM agency_fund_crosswalk;
```

- **Switched from three-digit to six-digit fund numbers to add a greater level
  of detail to funds**. Prior to 2024, fund numbers (`agency_fund.fund_num`)
  consisted of three digits, and funds with the same `fund_num` in different
  agencies always had the same `fund_name`. In 2024, the Clerk changed their
  fund numbers so that they consist of six digits, and they are no longer
  guaranteed to share the same name across agencies. The Clerk also added many
  new six-digit funds that were previously aggregated into larger three-digit
  funds. For these new six-digit funds, the first three digits of the fund
  number always matches the three-digit number for the aggregate fund that
  existed prior to 2024.
    - To see the full list of funds that are new in 2024, run the following
      query against the 2024 PTAXSIM database:

```sql
SELECT * FROM agency_fund_info WHERE fund_num NOT LIKE '%000'
```

- **Removed a number of minor columns from agency reports**. None of these
  columns were used in any core PTAXSIM functions, so we expect that their
  absence will not affect most users.
- **Added a new "authority number" for each agency in agency reports**. So far,
  authority numbers appear to have a 1:1 relationship to agency numbers, though
  we expect this may change in future years.

Read on for a detailed description of the changes we made to the PTAXSIM
database and functions to handle these changes in the source data.

## Breaking changes

- **Added a new table `pin_tif_distribution` and associated lookup function
  [`lookup_pin_tif()`](https://ccao-data.github.io/ptaxsim/reference/lookup_pin_tif.html)
  to handle the methodological change to the TIF revenue share calculation**.
  The [`tax_bill()`](https://ccao-data.github.io/ptaxsim/reference/tax_bill.html)
  function now also accepts an optional `pin_tif_dt` argument that you can use for post-2024 TIF
  counterfactuals.
    - Note that there are now two different TIF lookup functions, each of
      which applies to a different set of years:
      - [`lookup_pin_tif()`](https://ccao-data.github.io/ptaxsim/reference/lookup_pin_tif.html)
        will only return rows where `year >= 2024`.
      - [`lookup_tif()`](https://ccao-data.github.io/ptaxsim/reference/lookup_tif.html)
        will only return rows where `year < 2024`.
    - **How this change affects you**: If you maintain any TIF counterfactuals,
      you will need to update your use of the `tax_bill()` function to pass in
      an altered `pin_tif_dt` for any tax years that you analyze beyond 2023.
      For an example of this type of change, see the [Tinkering with TIFs
      vignette](https://ccao-data.github.io/ptaxsim/articles/tifs.html), which
      we have updated to include a TIF counterfactual with data for tax year 2024.
- **Added new tables `agency_crosswalk` and `agency_fund_crosswalk` to support
  tracking agencies that have changed to funds in 2024**. You can use these
  tables to analyze agencies over time, even if the Clerk switched to reporting
  them as funds in 2024.
    - **How this change affects you**: If you maintain code that analyzes
      agencies or funds over time, and you want to update your code to include
      2024 data, you should use the crosswalk tables to determine whether the
      Clerk changed any of the agencies or funds that interest you in 2024. If
      any of your agencies or funds have changed, you will need to use
      the `agency_num_final` and `fund_num_final` columns to join pre- and
      post-2024 data. For an example using the City of Chicago Library Fund to
      show how to handle this type of change, see the vignette [Tracking taxing
      agency revenue over time](https://ccao-data.github.io/ptaxsim/articles/agencies.html).
        - **⚠️ Warning**: If you maintain code that specifically analyzes levy
          adjustments for any agencies that changed to become funds in 2024,
          you will also need to update your analysis to handle the fact that
          the Clerk consolidated levy adjustments for these funds into their
          parent agency in 2024. For a detailed example, see the
          [Caveat: Levy adjustment funds require special
          handling](https://ccao-data.github.io/ptaxsim/articles/agencies.html##caveat-levy-adjustment-funds-require-special-handling)
          section of the vignette.
- **Added a new column `agency_fund.fund_type_num` to handle changing fund
  numbers in 2024**. In 2024, the Clerk changed their fund numbers so that
  they consist of six digits instead of three, and they are no longer
  guaranteed to refer to the same `fund_name` across agencies. However, the
  first three digits of a fund number are still guaranteed to refer to a
  consistent fund "type", so we introduced a new column
  `agency_fund.fund_type_num` to represent the notion of a "fund type" that
  is consistent across agencies.
    - **How this change affects you**: If you use `agency_fund.fund_num` to
      track specific funds across agencies, you will need to switch to
      using `agency_fund.fund_type_num` for that purpose.
- **Appended three trailing zeros (`000`) to all fund numbers in `agency_fund`
  and `agency_fund_info` for years prior to 2024**. This change is necessary
  in order to align three-digit pre-2024 fund numbers with their new six-digit
  representations in 2024.
    - **How this change affects you**: If you maintain code that references
      specific three-digit `fund_num` values, you will need to update these
      values to append three trailing zeros. Alternatively, you could update
      your code to reference `fund_type_num` instead of `fund_num`, per the
      bullet point above.
- **Changed the `agency_fund_info` table so that it is now unique by
  `(agency_num, fund_num)` instead of `(fund_num)`**. Since fund numbers are
  no longer consistent across agencies, we can't use them as the exclusive basis
  for the `agency_fund_info` table anymore. To handle this change, we have
  altered the `agency_fund_info` table so that it includes `agency_num`
  in its primary key.
    - **How this change affects you**: If you use the `agency_fund_info` table
      and treat it as unique by `fund_num`, you will need to update your
      code to add `agency_num` when joining to this table.
- **Added new exemption columns `pin.exe_vet_dis_100` and `pin.exe_wwii`**.
  These columns correspond to the new 100% disability level for the [Veterans with
  Disabilities Exemption](https://www.cookcountyassessoril.gov/veterans-disabilities-exemption)
  and the [WWII Exemption](https://www.cookcountyassessoril.gov/wwii-veterans-exemption),
  respectively. The lookup function
  [`lookup_pin()`](https://ccao-data.github.io/ptaxsim/reference/lookup_pin.html)
  now returns these columns, and the `tax_bill()` function expects these
  columns to be present in the `pin_dt` datatable argument.
    - **How this change affects you**: If you maintain code that constructs a
      `pin_dt` datatable to pass into `tax_bill()`, you will need to update it
      to ensure you are including these new exemption columns.
- **Dropped a few columns that the Clerk has removed from its agency reports**.
    - The dropped columns are:
      - `agency_fund.ptell_reduced_ind` (PTELL-reduced levy indicator)
      - `agency.reduction_type` (PTELL-reduced levy type)
      - `agency.total_reduced_levy` (PTELL reduced levy total)
    - **How this change affects you**: If you use any of the columns listed
      above, you will need to remove them from your code. Feel free to [open an
      issue](https://github.com/ccao-data/ptaxsim/issues/new) to ask for help
      replacing these columns; we're interested to know if and how PTAXSIM users
      are using them.
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
    - **How this change affects you**: If you use any of the columns listed
      above, you will need to remove them from your code. If `cty_overlap_eav`
      does not work for your purposes, [open an
      issue](https://github.com/ccao-data/ptaxsim/issues/new) and we'll do our
      best to help you update your analysis.

## Improvements

- **Added a new column `agency.authority_num`**. We expect this column to be
  more useful in future years when the Clerk continues migrating from the
  core "Agency" entity to the "Authority" entity.
    - **How this change affects you**: We recommend ignoring this column for
      now, because it is currently duplicative of `agency_num` and we don't
      have full confidence in our understanding of the Clerk's plans for this
      column in the future. However, it remains available if you are interested
      in investigating it.
- **Added support for pre-release database versions**. The code now supports
  database versions with pre-release suffixes like `2024.0.0-alpha.1`.
  Databases tagged with pre-release versions are unreleased but available for
  testing.
    - **How this change affects you**: This change will not affect most PTAXSIM
      users. It is only useful for PTAXSIM maintainers and pre-release testers.
- **Updated the [Tinkering with TIFs
  vignette](https://ccao-data.github.io/ptaxsim/articles/tifs.html) to
  demonstrate the correct way to handle the new TIF revenue share calcluation**
  ([#77](https://github.com/ccao-data/ptaxsim/pull/77)).
    - **How this change affects you**: You should read the latest version of the
      vignette if you use PTAXSIM for TIF counterfactuals.
- **Added a new vignette [Tracking taxing agency revenue over
  time](https://ccao-data.github.io/ptaxsim/articles/agencies.html)
  to demonstrate the correct way to analyze agencies and funds over time given
  the 2024 change that switched some agencies to funds**.
  ([#84](https://github.com/ccao-data/ptaxsim/pull/84)).
    - **How this change affects you**: You should read the vignette if you use
      PTAXSIM to analyze specific agencies over time so that you understand how
      to update your code.
- **Corrected how the `tax_bill` function treats PINs with an EAV that is less
  than $150, now defaulting the final tax output to zero**. This aligns with the
  Treasurer's treatment of these PINs, so the tax bill calculation for these
  PINs will now match the what is shown on the final tax bill
  ([#91](https://github.com/ccao-data/ptaxsim/pull/91)).
    - **How this change affects you**: This improves the accuracy of the
    `tax_bill` function to match what is displayed the Treasurer's tax bills.
    This change has no impact on how to use the `tax_bill` function.

## Bug fixes

- **Lowered the memory usage of the `pin_geometry` extraction script**
  ([#67](https://github.com/ccao-data/ptaxsim/pull/67)). This script previously
  struggled to complete on our 160gb server. It now executes without any issues.
    - **How this change affects you**: This change will not affect you as a
      PTAXSIM user. It is only useful for PTAXSIM maintainers.
