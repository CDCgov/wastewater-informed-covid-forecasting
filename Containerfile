FROM docker.io/rocker/r-ver:4.5.0

RUN Rscript -e "install.packages('pak')"
RUN Rscript -e "pak::pkg_install('github::stan-dev/cmdstanr')"
RUN Rscript -e "cmdstanr::install_cmdstan()"
RUN Rscript -e "pak::pkg_install('github::cdcgov/ww-inference-model')"
RUN Rscript -e "pak::pkg_install('argparser')"

ADD . /.

RUN Rscript -e "pak::pkg_install('local::wweval')"
