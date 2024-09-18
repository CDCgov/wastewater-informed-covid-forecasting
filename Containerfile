FROM docker.io/rocker/r-base:latest
ENV R_BUILD_REPOS="'https://packagemanager.posit.co/cran/__linux__/jammy/latest'"

ADD ./.ContainerBuildRprofile /.Rprofile

RUN Rscript -e "install.packages('pak')"
RUN Rscript -e "pak::pkg_install('github::stan-dev/cmdstanr')"
RUN Rscript -e "cmdstanr::install_cmdstan()"


ADD . /.

RUN Rscript -e "pak::pkg_install('local::wweval')"
RUN Rscript -e "pak::pkg_install('argparser')"

RUN mkdir -p stanmodels
RUN Rscript -e "cfaforecastrenewalww::compile_model( \
  'cfaforecastrenewalww/inst/stan/renewal_ww_hosp_site_level_inf_dynamics.stan', \
  'cfaforecastrenewalww/inst/stan', \
  'stanmodels' \
)"
