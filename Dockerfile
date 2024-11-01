FROM rocker/r-ver:4.4.2@sha256:9e8785e82c06d91a9ca95f37470426309cee4d2613ad172dc6c494dd875cebb4

ENV RENV_VERSION 0.12.2
ENV RENV_PATHS_CACHE_HOST /opt/local/renv/cache
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

RUN set -x && \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    chromium-browser \
    libcurl4-openssl-dev \
    libgit2-dev \
    libpng-dev \
    libssl-dev \
    libmecab-dev \
    libxml2-dev \
    mecab \
    mecab-ipadic-utf8 \
    zlib1g-dev && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/*

WORKDIR /home/envcf
