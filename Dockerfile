FROM rocker/shiny:4.5.1

# 1) System deps + python venv
RUN apt-get update && apt-get install -y \
  python3 python3-venv python3-pip \
  libgl1 libglib2.0-0 \
  libcurl4-openssl-dev libssl-dev libxml2-dev \
  && rm -rf /var/lib/apt/lists/*

# 2) Create a virtual environment for python packages (fix PEP 668)
RUN python3 -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

# 3) Upgrade pip in the venv + install python deps
RUN pip install --upgrade pip setuptools wheel
RUN pip install --no-cache-dir tensorflow h5py pillow numpy

# 4) R packages
RUN R -e "install.packages(c('shiny','dplyr','readr','stringr','tibble','htmltools','base64enc','reticulate','tensorflow','keras'), repos='https://cloud.r-project.org')"

# 5) App
WORKDIR /srv/shiny-server/app
COPY . /srv/shiny-server/app

EXPOSE 3838

# 6) Ensure reticulate uses our venv python
ENV RETICULATE_PYTHON="/opt/venv/bin/python"

CMD ["/usr/bin/shiny-server"]
