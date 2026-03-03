# Utilisation d'une image de base R Shiny stable
FROM rocker/shiny:4.2.1

# 1. Installation des dépendances système (Linux)
# On ajoute libgdal, libproj et libgeos pour le package 'sf' (cartes)
# On ajoute python3-venv pour une gestion propre de reticulate
RUN apt-get update && apt-get install -y \
    python3 \
    python3-pip \
    python3-venv \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev \
    libgl1 \
    libglib2.0-0 \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# 2. Installation de TensorFlow et des dépendances Python au niveau système
RUN pip3 install --no-cache-dir tensorflow h5py pillow numpy

# 3. Configuration de l'environnement pour R
# Cela force 'reticulate' à utiliser le python système où on a installé TF
ENV RETICULATE_PYTHON=/usr/bin/python3

# 4. Installation des packages R
# J'ai ajouté tous les packages présents dans ton app.R (sf, leaflet, bslib, etc.)
RUN R -e "install.packages(c(\
    'shiny', \
    'shinyjs', \
    'dplyr', \
    'readr', \
    'stringr', \
    'tibble', \
    'tidyr', \
    'stringi', \
    'htmltools', \
    'base64enc', \
    'bslib', \
    'bsicons', \
    'leaflet', \
    'sf', \
    'reticulate', \
    'tensorflow', \
    'keras'\
    ), repos='https://cloud.r-project.org')"

# 5. Préparation du répertoire de l'application
WORKDIR /srv/shiny-server/app
COPY . /srv/shiny-server/app

# Donner les permissions d'écriture (nécessaire si l'app crée des fichiers .rds d'historique)
RUN chmod -R 777 /srv/shiny-server/app

# 6. Exposition du port et lancement
EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
