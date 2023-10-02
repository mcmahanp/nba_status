# Dependencies

To run this code, you will need recent versions of R and Python. Our analysis was run with R 4.3.1 and Python 3.10.12. Python is used primarily for scraping basketball-reference.com, and depends on the `requests`, `beautifulsoup4`, and `lxml` packages. The bulk of the analysis is written in R and depends on the following R packages:

- `data.table`, `rio` (data i/o and wrangling)
- `glue` (string formatting model specification)
- `lubridate` (handling dates)
- `sandwich`, `miceadds` (logit with robust standard errors)
- `margins` (average marginal effects)
- `modelsummary` (nice model output)
- `flextable`, `kableExtra` (.docx and .xlsx output)


If you have [Nix](https://nix.dev/) installed, you can create the exact environment used in our analysis with the included `default.nix`. Just run `nix-build` from the root directory to install all necessary dependencies (this may take a while), and `nix-shell` for an interactive shell. 


# Running the analysis

The replication relies on the Stata data file `andata.dta` produced from Biegert, Kühhirt & Van Lancker's replication package. The code and base data are available at <https://doi.org/10.17605/osf.io/ntwdy>. We include `andata.dta` created using Stata 17.0 BE based on the replication package as downloaded in August of 2023.

The full code for analysis consists of six files:
- `01_replication_data_to_csv.R`: Convert BKVL's .dta to a .csv.
- `02_scrape_br.py`: Scrape game logs, player data, and season data from basketball-reference.com. _Note: this script makes close to 15,000 requests to basketball-reference.com. With the necessary rate limiting, it will take up to **24 hours** to run. By default the script caches its data, which amounts to approximately 4.6GB uncompressed._
- `03_process_and_merge.R`: Process game logs and player data, and merge into a single dataset at the player–season level.
- `04_replication.R`: Replicate BKVL's analysis using their data and corrected data with comparisons.
- `05_expanded_analysis.R`: Our main analyses.
- `06_forest_plot.R`: Create the summary forest plot comparing models.

With the dependencies (listed above) installed the full analysis can be run with the following shell commands. (Note that the second script, `02_scrape_br.py` is quite time- and disk-intensive. It will take close to 24 hours to run and will download approximately 4.6GB of data to your computer.)

```{bash}
Rscript scripts/01_replication_data_to_csv.R
python scripts/02_scrape_br.py
Rscript scripts/03_process_and_merge.R
Rscript scripts/04_replication.R
Rscript scripts/05_expanded_analysis.R
Rscript scripts/06_forest_plot.R
```

Processed data are stored in the `data` directory, saved model-fit objects are stored in the `models` directory, and tables and figures are stored in the `output` directory.