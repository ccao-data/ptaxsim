PRAGMA foreign_keys = ON;

/** agency **/
CREATE TABLE agency (
    year                    int                                        NOT NULL,
    agency_num              varchar(9)                                 NOT NULL,
    home_rule_ind           boolean                                    NOT NULL,
    agg_ext_base_year       int      CHECK(agg_ext_base_year >= 2003)          ,
    lim_numerator           bigint   CHECK(lim_numerator >= 0)                 ,
    lim_denominator         bigint   CHECK(lim_denominator >= 0)               ,
    lim_rate                double   CHECK(lim_rate >= 0)                      ,
    prior_eav               bigint   CHECK(prior_eav >= 0)             NOT NULL,
    curr_new_prop           bigint   CHECK(curr_new_prop >= 0)         NOT NULL,
    cty_cook_eav            bigint   CHECK(cty_cook_eav >= 0)          NOT NULL,
    cty_dupage_eav          bigint   CHECK(cty_dupage_eav >= 0)        NOT NULL,
    cty_lake_eav            bigint   CHECK(cty_lake_eav >= 0)          NOT NULL,
    cty_will_eav            bigint   CHECK(cty_will_eav >= 0)          NOT NULL,
    cty_kane_eav            bigint   CHECK(cty_kane_eav >= 0)          NOT NULL,
    cty_mchenry_eav         bigint   CHECK(cty_mchenry_eav >= 0)       NOT NULL,
    cty_dekalb_eav          bigint   CHECK(cty_dekalb_eav >= 0)        NOT NULL,
    cty_grundy_eav          bigint   CHECK(cty_grundy_eav >= 0)        NOT NULL,
    cty_kankakee_eav        bigint   CHECK(cty_kankakee_eav >= 0)      NOT NULL,
    cty_kendall_eav         bigint   CHECK(cty_kendall_eav >= 0)       NOT NULL,
    cty_lasalle_eav         bigint   CHECK(cty_lasalle_eav >= 0)       NOT NULL,
    cty_livingston_eav      bigint   CHECK(cty_livingston_eav >= 0)    NOT NULL,
    cty_total_eav           bigint   CHECK(cty_total_eav >= 0)         NOT NULL,
    pct_burden              double   CHECK(pct_burden >= 0
                                     AND   pct_burden <= 1)            NOT NULL,
    total_levy              bigint   CHECK(total_levy >= 0)            NOT NULL,
    total_max_levy          bigint   CHECK(total_max_levy >= 0)        NOT NULL,
    total_prelim_rate       double   CHECK(total_prelim_rate >= 0)     NOT NULL,
    total_reduced_levy      bigint   CHECK(total_reduced_levy >= 0)            ,
    total_final_levy        bigint   CHECK(total_final_levy >= 0)      NOT NULL,
    total_final_rate        double   CHECK(total_final_rate >= 0)      NOT NULL,
    reduction_type          varchar                                            ,
    reduction_pct           double   CHECK(reduction_pct >= 0
                                     AND   reduction_pct <= 1)                 ,
    total_non_cap_ext       double   CHECK(total_non_cap_ext >= 0)             ,
    total_ext               double   CHECK(total_ext >= 0)             NOT NULL,
    PRIMARY KEY (year, agency_num)
);
CREATE INDEX ix_agency_agency_num ON agency(agency_num);
CREATE INDEX ix_agency_home_rule_ind ON agency(home_rule_ind);


/** agency_info **/
CREATE TABLE agency_info (
    agency_num              varchar(9)                                 NOT NULL,
    agency_name             varchar                                    NOT NULL,
    agency_name_short       varchar                                    NOT NULL,
    agency_name_original    varchar                                    NOT NULL,
    major_type              varchar(21)                                NOT NULL,
    minor_type              varchar(10)                                NOT NULL,
    PRIMARY KEY (agency_num)
);
CREATE INDEX ix_agency_info_major_type ON agency_info(major_type);
CREATE INDEX ix_agency_info_minor_type ON agency_info(minor_type);


/** agency_fund **/
CREATE TABLE agency_fund (
    year                    int                                        NOT NULL,
    agency_num              varchar(9)                                 NOT NULL,
    fund_num                varchar(3)                                 NOT NULL,
    levy                    bigint   CHECK(levy >= 0)                  NOT NULL,
    loss_pct                double   CHECK(loss_pct >= 0 
                                     AND   loss_pct <= 1)              NOT NULL,
    levy_plus_loss          bigint   CHECK(levy_plus_loss >= 0)        NOT NULL,
    rate_ceiling            double   CHECK(rate_ceiling >= 0)          NOT NULL, 
    max_levy                bigint   CHECK(max_levy >= 0)              NOT NULL,
    prelim_rate             double   CHECK(prelim_rate >= 0)           NOT NULL,
    ptell_reduced_levy      bigint   CHECK(ptell_reduced_levy >= 0)            ,
    ptell_reduced_ind       boolean                                    NOT NULL,
    final_levy              bigint   CHECK(final_levy >= 0)            NOT NULL,
    final_rate              double   CHECK(final_rate >= 0)            NOT NULL,
    PRIMARY KEY (year, agency_num, fund_num),
    FOREIGN KEY (year, agency_num) REFERENCES agency(year, agency_num)
);
CREATE INDEX ix_agency_fund_agency_num ON agency_fund(agency_num);
CREATE INDEX ix_agency_fund_fund_num ON agency_fund(fund_num);


/** agency_fund_info **/
CREATE TABLE agency_fund_info (
    fund_num                varchar(3)                                 NOT NULL,
    fund_name               varchar                                    NOT NULL,
    capped_ind              boolean                                    NOT NULL,
    PRIMARY KEY (fund_num)
);
CREATE INDEX ix_agency_fund_info_capped_ind ON agency_fund_info(capped_ind);
    

/** cpi **/
CREATE TABLE cpi (
    year                    int                                        NOT NULL,
    cpi                     double                                     NOT NULL,
    ptell_cook              double                                     NOT NULL,
    levy_year               int                                        NOT NULL,
    PRIMARY KEY (levy_year)
);
CREATE INDEX ix_cpi_year ON cpi(year);


/** eq_factor **/
CREATE TABLE eq_factor (
    year                    int                                        NOT NULL,
    eq_factor               double                                     NOT NULL,
    PRIMARY KEY (year)
);


/** pin **/
CREATE TABLE pin (
    year                    int                                        NOT NULL,
    pin                     varchar(14)                                NOT NULL,
    class                   varchar(3)                                 NOT NULL,
    tax_code_num            varchar(5)                                 NOT NULL,
    tax_bill_total          double   CHECK(tax_bill_total >= 0)        NOT NULL,
    av_mailed               int   CHECK(av_mailed >= 0)                NOT NULL,
    av_certified            int   CHECK(av_certified >= 0)             NOT NULL,
    av_board                int   CHECK(av_board >= 0)                 NOT NULL,
    av_clerk                int   CHECK(av_clerk >= 0)                 NOT NULL,
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
CREATE INDEX ix_pin_pin ON pin(pin);
CREATE INDEX ix_pin_tax_code_num ON pin(tax_code_num);
CREATE INDEX ix_pin_township_code ON pin(substr(tax_code_num, 1, 2));
CREATE INDEX ix_pin_class ON pin(class);


/** tax_code **/
CREATE TABLE tax_code (
    year                    int                                        NOT NULL,
    agency_num              varchar(9)                                 NOT NULL,
    agency_rate             double   CHECK(agency_rate >= 0)           NOT NULL,
    tax_code_num            varchar(5)                                 NOT NULL,
    tax_code_rate           double   CHECK(tax_code_rate >= 0)         NOT NULL,
    PRIMARY KEY (year, agency_num, tax_code_num)
);
CREATE INDEX ix_tax_code_agency_num ON tax_code(agency_num);
CREATE INDEX ix_tax_code_tax_code_num ON tax_code(tax_code_num);


/** tif **/
CREATE TABLE tif (
    year                    int                                        NOT NULL,
    agency_num              varchar(9)                                 NOT NULL,
    prev_year_revenue       double   CHECK(prev_year_revenue >= 0)     NOT NULL,
    curr_year_revenue       double   CHECK(curr_year_revenue >= 0)     NOT NULL,
    first_year              int                                        NOT NULL,
    cancelled_this_year     boolean                                    NOT NULL,
    PRIMARY KEY (year, agency_num)
);
CREATE INDEX ix_tif_agency_num ON tif(agency_num);


/** tif_distribution **/
CREATE TABLE tif_distribution (
    year                    int                                        NOT NULL,
    agency_num              varchar(9)                                 NOT NULL,
    tax_code_num            varchar(5)                                 NOT NULL,
    tax_code_rate           double   CHECK(tax_code_rate >= 0)         NOT NULL,
    tax_code_eav            bigint   CHECK(tax_code_eav >= 0)          NOT NULL,
    tax_code_frozen_eav     bigint   CHECK(tax_code_frozen_eav >= 0)   NOT NULL,
    tax_code_revenue        bigint   CHECK(tax_code_revenue >= 0)      NOT NULL,
    tax_code_distribution_pct double CHECK(tax_code_distribution_pct >= 0) NOT NULL,
    PRIMARY KEY (year, agency_num, tax_code_num)
);
CREATE INDEX ix_tif_distribution_agency_num ON tif_distribution(agency_num);
CREATE INDEX ix_tif_distribution_tax_code_num ON tif_distribution(tax_code_num);
