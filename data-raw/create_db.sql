PRAGMA foreign_keys = ON;

/** agencies **/
CREATE TABLE agencies (
    year                    int                                        NOT NULL,
    agency_num              varchar(9)                                 NOT NULL,
    agency_name             varchar(255)                               NOT NULL,
    home_rule_ind           boolean                                    NOT NULL,
    total_eav               bigint   CHECK(total_eav >= 0)             NOT NULL,
    total_levy              bigint   CHECK(total_levy >= 0)            NOT NULL,
    total_ext               double   CHECK(total_ext >= 0)             NOT NULL,
    PRIMARY KEY (year, agency_num)
);
CREATE INDEX ix_agencies_agency_num ON agencies(agency_num);


/** agencies_detail **/
CREATE TABLE agencies_detail (
    year                    int                                        NOT NULL,
    agency_num              varchar(9)                                 NOT NULL,
    fund_num                varchar(3)                                 NOT NULL,
    fund_name               varchar(255)                               NOT NULL,
    fund_levy               bigint   CHECK(fund_levy >= 0)             NOT NULL,
    fund_rate               double   CHECK(fund_rate >= 0)             NOT NULL,
    PRIMARY KEY (year, agency_num, fund_num),
    FOREIGN KEY (year, agency_num) REFERENCES agencies(year, agency_num)
);
CREATE INDEX ix_agencies_detail_agency_num ON agencies_detail(agency_num);


/** cpis **/
CREATE TABLE cpis (
    year                    int                                        NOT NULL,
    cpi                     double                                     NOT NULL,
    ptell_cook              double                                     NOT NULL,
    levy_year               int                                        NOT NULL,
    PRIMARY KEY (levy_year)
);
CREATE INDEX ix_cpis_year ON cpis(year);


/** eq_factors **/
CREATE TABLE eq_factors (
    year                    int                                        NOT NULL,
    equalization_factor     double                                     NOT NULL,
    PRIMARY KEY (year)
);


/** pins **/
CREATE TABLE pins (
    year                    int                                        NOT NULL,
    pin                     varchar(14)                                NOT NULL,
    class                   varchar(3)                                 NOT NULL,
    av                      int   CHECK(av >= 0)                       NOT NULL,
    tax_bill_total          double   CHECK(tax_bill_total >= 0)        NOT NULL,
    tax_code_num            varchar(5)                                 NOT NULL,
    exe_homeowner           int   CHECK(exe_homeowner >= 0)            NOT NULL,
    exe_senior              int   CHECK(exe_senior >= 0)               NOT NULL,
    exe_longtime_homeowner  int   CHECK(exe_longtime_homeowner >= 0)   NOT NULL,
    exe_freeze              int   CHECK(exe_freeze >= 0)               NOT NULL,
    exe_disabled            int   CHECK(exe_disabled >= 0)             NOT NULL,
    exe_vet_returning       int   CHECK(exe_vet_returning >= 0)        NOT NULL,
    exe_vet_dis_lt50        int   CHECK(exe_vet_dis_lt50 >= 0)         NOT NULL,
    exe_vet_dis_50_69       int   CHECK(exe_vet_dis_50_69 >= 0)        NOT NULL,
    exe_vet_dis_ge70        int   CHECK(exe_vet_dis_ge70 >= 0)         NOT NULL,
    exe_abate               int   CHECK(exe_abate >= 0)                NOT NULL,
    PRIMARY KEY (year, pin)
);
CREATE INDEX ix_pins_pin ON pins(pin);
CREATE INDEX ix_pins_tax_code_num ON pins(tax_code_num);
CREATE INDEX ix_pins_class ON pins(class);


/** tax_codes **/
CREATE TABLE tax_codes (
    year                    int                                        NOT NULL,
    agency_num              varchar(9)                                 NOT NULL,
    agency_rate             double   CHECK(agency_rate >= 0)           NOT NULL,
    tax_code_num            varchar(5)                                 NOT NULL,
    tax_code_rate           double   CHECK(tax_code_rate >= 0)         NOT NULL,
    PRIMARY KEY (year, agency_num, tax_code_num)
);
CREATE INDEX ix_tax_codes_agency_num ON tax_codes(agency_num);
CREATE INDEX ix_tax_codes_tax_code_num ON tax_codes(tax_code_num);


/** tif_summaries **/
CREATE TABLE tif_summaries (
    year                    int                                        NOT NULL,
    agency_num              varchar(9)                                 NOT NULL,
    tif_name                varchar                                    NOT NULL,
    municipality            varchar                                    NOT NULL,
    prev_year_revenue       double   CHECK(prev_year_revenue >= 0)     NOT NULL,
    curr_year_revenue       double   CHECK(curr_year_revenue >= 0)     NOT NULL,
    first_year              int                                        NOT NULL,
    tif_cancelled_this_year boolean                                    NOT NULL,
    tif_new_this_year boolean                                          NOT NULL,
    PRIMARY KEY (year, agency_num)
);
CREATE INDEX ix_tif_summaries_agency_num ON tif_summaries(agency_num);


/** tif_distributions **/
CREATE TABLE tif_distributions (
    year                    int                                        NOT NULL,
    agency_num              varchar(9)                                 NOT NULL,
    tax_code_num            varchar(5)                                 NOT NULL,
    tax_code_rate           double   CHECK(tax_code_rate >= 0)         NOT NULL,
    tax_code_eav            bigint   CHECK(tax_code_eav >= 0)          NOT NULL,
    tax_code_frozen_eav     bigint   CHECK(tax_code_frozen_eav >= 0)   NOT NULL,
    tax_code_revenue        bigint   CHECK(tax_code_revenue >= 0)      NOT NULL,
    tax_code_distribution_percent double CHECK(tax_code_distribution_percent >= 0) NOT NULL,
    PRIMARY KEY (year, agency_num, tax_code_num)
);
CREATE INDEX ix_tif_distributions_agency_num ON tif_distributions(agency_num);
CREATE INDEX ix_tif_distributions_tax_code_num ON tif_distributions(tax_code_num);
