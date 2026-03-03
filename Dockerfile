# 1. Image de base R Shiny stable
FROM rocker/shiny:4.2.1

# 2. Installation des dépendances système Linux
# Nécessaire pour Python, TensorFlow et les cartes (libgdal, libproj)
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

# 3. Installation de TensorFlow et dépendances Python
RUN pip3 install --no-cache-dir tensorflow h5py pillow numpy

# 4. Configuration de l'environnement pour R
# Force reticulate à utiliser le Python où TensorFlow est installé
ENV RETICULATE_PYTHON=/usr/bin/python3

# 5. Installation des packages R nécessaires à AgriSave
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

# 6. NETTOYAGE ET INSTALLATION DE L'APP
# Supprime l'application "Welcome to Shiny" par défaut
RUN rm -rf /srv/shiny-server/*

# Définit le répertoire de travail à la racine du serveur
WORKDIR /srv/shiny-server/

# Copie tous tes fichiers (app.R, dossier www, etc.) directement ici
COPY . /srv/shiny-server/

# Donne les droits d'écriture pour l'historique et les comptes utilisateurs (.rds)
RUN chmod -R 777 /srv/shiny-server/

# 7. Lancement du serveur
EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
