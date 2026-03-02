FROM rocker/shiny:4.5.1

RUN apt-get update && apt-get install -y \
  python3 python3-pip \
  libgl1 libglib2.0-0 \
  libcurl4-openssl-dev libssl-dev libxml2-dev \
  && rm -rf /var/lib/apt/lists/*

RUN pip3 install --no-cache-dir tensorflow h5py pillow numpy

RUN R -e "install.packages(c('shiny','dplyr','readr','stringr','tibble','htmltools','base64enc','reticulate','tensorflow','keras'), repos='https://cloud.r-project.org')"

WORKDIR /srv/shiny-server/app
COPY . /srv/shiny-server/app

EXPOSE 3838
CMD ["/usr/bin/shiny-server"]