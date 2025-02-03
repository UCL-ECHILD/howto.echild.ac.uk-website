# SUBDOMAIN.echild.ac.uk website (Quarto template)

Template repo for the ECHILD SUBDOMAIN (Quarto) websites at <https://SUBDOMAIN.echild.ac.uk>.

## SETUP

To use this template:

1.  Create a new GitHub repo from this template and clone it locally 
    (`gh repo create UCL-ECHILD/[NEW-SUBDOMAIN].echild.ac.uk-website --public --template=UCL-ECHILD/SUBDOMAIN.echild.ac.uk-website-quarto-template --clone` 
    - don't forget to replace `[NEW-SUBDOMAIN]`!)
1.  Remove/replace all instances of `SUBDOMAIN` across new repo 
1.  Add content/images/favicons/etc.
1.  Update `renv.lock` file (see [R renv documentation](https://rstudio.github.io/renv/articles/renv.html))
1.  Update `pyproject.toml` file (see [Python Poetry documentation](https://python1.poetry.org/docs/basic1.usage/))
1.  Amend the `echild.ac.uk` DNS records on Cloudflare
    1.  Create SUBDOMAIN pages project (`Workers & Pages > Create [button] > Pages > Upload assets [button]`)
    1.  Add DNS CNAME record for SUBDOMAIN (`Workers & Pages > SUBDOMAIN Pages project > Custom Domains`)
    1.  Edit Bulk Redirect list (`SUBDOMAIN.pages.dev` -> `SUBDOMAIN.echild.ac.uk`)
1.  Enable repo access to required GitHub Organization Secrets (`CLOUDFLARE_ID` and `CLOUDFLARE_PAGES_TOKEN`)
1.  Uncomment lines 4-6 in `.github/workflows/publish.yml`
1.  Remove this SETUP section from this `README.md`
1.  Make and push a commit to the GitHub hosted repo
