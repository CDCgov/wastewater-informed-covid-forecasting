FROM docker.io/rocker/r-base:latest
ENV R_BUILD_REPOS="'https://packagemanager.posit.co/cran/__linux__/jammy/latest'"

ADD ./.ContainerBuildRprofile /.Rprofile

RUN apt-get update
RUN apt-get install -y ibcurl4-openssl-dev=8.9.1
RUN Rscript -e "install.packages('pak')"
RUN Rscript -e "pak::pkg_install('github::stan-dev/cmdstanr')"
RUN Rscript -e "cmdstanr::install_cmdstan()"
RUN Rscript -e "pak::pkg_install('github::cdcgov/ww-inference-model')"
RUN Rscript -e "pak::pkg_install('argparser')"

ADD . /.

RUN Rscript -e "pak::pkg_install('local::wweval')"
