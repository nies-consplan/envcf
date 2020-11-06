Environmental Crowdfunding
---------------

Data sets and source code for "What determines the success and failure of environmental crowdfunding?".

## Workflow

```
workflow/
 |- 01-data_setup.R
 |- 02-project_text_mining.R
 |- 03-success_context.R
 |- 04-figure_output.R
```

## Reproducibility

### 1. Set up dictionary

```bash
git clone --depth 1 https://github.com/neologd/mecab-ipadic-neologd.git inst/mecab-ipadic-neologd
cd inst/mecab-ipadic-neologd
# git checkout 22cf24465a1710028b5c5915540045ac45d167dc
./bin/install-mecab-ipadic-neologd --create_user_dic -n -y
```

### 2-a. renv

We used version R version 4.0.2.

All packages to be used are installed via [renv](https://rstudio.github.io/renv/). For that, you first need to have renv installed.

```r
if (!requireNamespace("remotes"))
  install.packages("remotes")
remotes::install_github('rstudio/renv@0.12.2')
```

Download or clone this repository locally and set it to your working directory.

```r
renv::restore()
```

### 2-b. Running on Docker

```bash
docker-compose up --build
docker-compose run rver
```
