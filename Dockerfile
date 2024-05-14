FROM rocker/shiny:latest

# system dependencies
RUN apt-get update && apt-get install -y \
    sudo \
    libcurl4-gnutls-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    pandoc \
    pandoc-citeproc \
    libcairo2-dev \
    texlive-latex-base \
    texlive-fonts-recommended \
    texlive-fonts-extra \
    texlive-latex-extra \
    libgdal-dev\
    libudunits2-dev\
    libhdf5-dev \
    patch

# install R packages 
RUN R -e "install.packages('INLA',repos=c(getOption('repos'),INLA='https://inla.r-inla-download.org/R/testing'), dep=TRUE)"
RUN R -e "install.packages('devtools')"
RUN R -e "devtools::install_github('simon-smart88/disagapp')"

# copy app files
COPY ./inst/shiny/ /srv/shiny-server/disagapp

# select port
EXPOSE 3838

# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

# Add configuration to enable log preservation
RUN echo 'preserve_logs true;' >> /etc/shiny-server/shiny-server.conf

USER shiny

# run app
CMD ["/usr/bin/shiny-server"]

