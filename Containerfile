FROM docker.io/rocker/r-ver:4.5.0

RUN Rscript -e "install.packages('pak')"
RUN Rscript -e "pak::pkg_install('github::stan-dev/cmdstanr')"
RUN Rscript -e "cmdstanr::install_cmdstan()"
RUN Rscript -e "pak::pkg_install('github::cdcgov/ww-inference-model')"
RUN Rscript -e "pak::pkg_install('argparser')"
RUN Rscript -e "pak::pkg_install('github::cdcgov/forecasttools@v0.1.7')"


ADD . /.

RUN Rscript -e "\
    pak::repo_add(hubverse = 'https://hubverse-org.r-universe.dev'); \
    pak::local_install('wweval') \
"
