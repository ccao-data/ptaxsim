/** pin_geometry_raw **/
CREATE TABLE pin_geometry_raw (
    pin10                   varchar(10)                                NOT NULL,
    start_year              int                                        NOT NULL,
    end_year                int                                        NOT NULL,
    longitude               double                                     NOT NULL,
    latitude                double                                     NOT NULL,
    geometry                text                                       NOT NULL,
    PRIMARY KEY (pin10, start_year, end_year)
) WITHOUT ROWID;


/** pin_geometry **/
CREATE VIEW IF NOT EXISTS pin_geometry AS
SELECT y.year, pr.pin10, pr.longitude, pr.latitude, pr.geometry
FROM (
    SELECT DISTINCT year
    FROM agency
) AS y
CROSS JOIN pin_geometry_raw pr
    ON y.year >= pr.start_year
    AND y.year <= pr.end_year
