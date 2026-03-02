# ------------------------------------------------------------
# AgriSave - app.R (CODE COMPLET FUSIONNÉ)
# ✅ Login/Inscription AVANT accès à l'accueil (page login au démarrage)
# ✅ Bibliothèque avec filtres intégrés (Tous / Cultures▾ / Pathologies▾) + Recherche
# ✅ Historique des dernières consultations (3 max) + affichage sur Accueil
# ✅ Bottom bar toujours visible (au premier plan) -> MAIS masquée tant que non connecté
# ✅ Pages Carte (Leaflet) + Communauté (posts, photos, réponses, likes, tri) FONCTIONNELLES
# ✅ Flèche retour + titre en haut (page-topbar) partout
# ------------------------------------------------------------
library(shiny)
library(reticulate)
library(tensorflow)

ensure_h5py <- local({
  done <- FALSE
  function() {
    if (isTRUE(done)) return(invisible(TRUE))
    reticulate::py_require("h5py")
    done <<- TRUE
    invisible(TRUE)
  }
})

ensure_h5py()
model <- keras::load_model_hdf5("www/model_plants.h5")

library(bslib)
library(bsicons)
library(shinyjs)

library(readr)
library(dplyr)
library(stringr)
library(tibble)

# AJOUTS (carte + communauté)
library(tidyr)
library(leaflet)
library(sf)
library(stringi)
library(base64enc)

# ----------------------------
# 0) DATA (CSV + images train)
# ----------------------------
CSV_PATH <- "www/PlantVillage_Pathologies_France.csv"  # ✅ enlève l'espace avant .csv
THUMB_DIR <- "www/thumbs"                              # ✅ dossier miniatures

if (!file.exists(CSV_PATH)) stop("CSV introuvable : ", CSV_PATH)
if (!dir.exists(THUMB_DIR)) stop("Dossier thumbs introuvable : ", THUMB_DIR)

df_csv <- readr::read_csv(CSV_PATH, show_col_types = FALSE) %>%
  dplyr::mutate(
    class_id = stringr::str_extract_all(`Nom de la pathologie`, "\\([^()]+\\)") %>%
      lapply(function(v) {
        v <- gsub("^\\(|\\)$", "", v)     # enlève ( )
        v <- v[grepl("___", v)]           # garde les identifiants PlantVillage
        if (length(v) == 0) NA_character_ else tail(v, 1)
      }) %>%
      unlist()
  )

# ✅ 1 image par classe seulement (dans www/thumbs/)
thumb_files <- list.files(THUMB_DIR, pattern = "\\.jpg$", ignore.case = TRUE, full.names = FALSE)

img_index <- tibble::tibble(
  class_id  = tools::file_path_sans_ext(thumb_files),
  thumb_url = file.path("thumbs", thumb_files)   # ✅ Shiny sert automatiquement www/
)

library_data <- img_index %>%
  dplyr::left_join(df_csv, by = "class_id") %>%
  dplyr::filter(!is.na(Description))

all_cultures <- sort(unique(library_data$`Nom de la culture`))
all_pathologies <- sort(unique(library_data$`Nom de la pathologie`))

# ----------------------------
# 1) UI
# ----------------------------
ui <- page_fillable(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      /* FOND SMARTPHONE */
      body {
        background: #e5e7eb !important;
        display: flex !important;
        justify-content: center !important;
        align-items: center !important;
        min-height: 100vh !important;
        padding: 20px !important;
        margin: 0 !important;
      }
     
      /* CADRE TELEPHONE */
      .phone-container {
        width: 375px !important;
        max-width: 100% !important;
        height: 812px !important;
        max-height: 90vh !important;
        background: #f8fafc !important;
        border-radius: 40px !important;
        box-shadow: 0 25px 50px rgba(0,0,0,0.3) !important;
        overflow: hidden !important;
        border: 8px solid #1f2937 !important;
        position: relative !important;
      }
     
      .phone-notch {
        position: absolute !important;
        top: 8px !important;
        left: 50% !important;
        transform: translateX(-50%) !important;
        width: 150px !important;
        height: 30px !important;
        background: #1f2937 !important;
        border-radius: 15px !important;
        z-index: 60 !important;
      }
     
      /* HEADER */
      .app-header {
        position: relative !important;
        z-index: 55 !important;
        padding-top: 45px !important;
      }
     
      .header-title {
        font-weight: 700 !important;
        color: white !important;
        font-style: italic !important;
        text-shadow: 1px 1px 2px rgba(0,0,0,0.4) !important;
      }
      .header-title-main { font-size: 1.15rem !important; }
      .header-title-sub { font-size: 0.72rem !important; font-weight: 600 !important; }
     
      .logo-mobile {
        height: 44px !important;
        width: auto !important;
        border-radius: 12px !important;
        box-shadow: 0 4px 12px rgba(255,255,255,0.8) !important;
        border: 3px solid rgba(255,255,255,0.9) !important;
      }

      /* CONTENU + NAV (fixe en bas et au premier plan) */
      .main-content {
        flex-grow: 1;
        overflow-y: auto;
        padding-bottom: 120px; /* espace pour la bottom bar */
      }
      .bottom-nav {
        position: absolute !important;
        left: 0 !important;
        right: 0 !important;
        bottom: 0 !important;
        z-index: 80 !important;
        background: #fff !important;
        border-top-left-radius: 25px;
        border-top-right-radius: 25px;
        box-shadow: 0 -6px 25px rgba(0,0,0,0.15);
        padding: 6px 8px !important;
      }

      /* Search bar */
      .search-bar {
        background: #ffffff !important;
        border: 1px solid #d1d5db !important;
        border-radius: 12px !important;
        padding: 10px 12px !important;
        box-shadow: 0 2px 10px rgba(0,0,0,0.06) !important;
      }
      .search-input input { border: none !important; outline: none !important; box-shadow: none !important; }
      .search-input .form-control:focus { box-shadow: none !important; }

      /* Cartes biblio */
      .biblio-card { transition: all 0.3s ease !important; border: 2px solid transparent !important; }
      .biblio-card:hover {
        transform: translateY(-5px) !important;
        border-color: #4a7c59 !important;
        box-shadow: 0 8px 25px rgba(74,124,89,0.2) !important;
      }
      .biblio-img { height: 105px; object-fit: cover; border-radius: 8px 8px 0 0; }
     
      /* Cartes biblio - UNIFORMISATION */
      .biblio-title {
        font-size: 0.8rem !important; /* Taille réduite identique à la photo 1 */
        font-weight: 700 !important;
        line-height: 1.2 !important;
        margin-top: 5px !important;
        margin-bottom: 2px !important;
      }
     
      .biblio-culture {
        font-size: 0.75rem !important;
        margin-bottom: 0 !important;
      }
     
      /* Ajustement du container de texte pour réduire le cadre blanc */
      .biblio-card .card-body {
        padding: 8px !important;
      }

      /* Ligne filtres */
      .tabs-row {
        display: flex;
        justify-content: space-between;
        align-items: center;
        gap: 10px;
        padding: 0 12px;
      }
      .tab-btn {
        background: transparent !important;
        border: none !important;
        font-weight: 700 !important;
        color: #111827 !important;
        padding: 6px 4px !important;
      }
      .tab-btn.active { color: #4a7c59 !important; }

      .tab-select .form-select {
        border: none !important;
        background: transparent !important;
        font-weight: 700 !important;
        color: #111827 !important;
        padding-left: 0 !important;
        padding-right: 18px !important;
        box-shadow: none !important;
      }
      .tab-select .form-select:focus { box-shadow: none !important; }

      .hint-line{
        padding: 0 12px;
        font-size: 0.82rem;
        color: #6b7280;
      }

      /* --- ACCUEIL --- */
      .hero-box { height: 120px !important; border-radius: 16px !important; }
      .hero-text { font-size: 0.88rem !important; line-height: 1.15rem !important; }

      .steps-title{ font-size: 0.95rem !important; font-weight: 700 !important; color: #111827 !important; }
      .steps-icons{ display: flex !important; gap: 10px !important; justify-content: flex-end !important; }
      .icon-circle { width: 34px !important; height: 34px !important; }

      .section-title { font-size: 1.05rem !important; font-style: italic !important; font-weight: 800 !important; margin-bottom: 6px !important; }

      .scan-btn-box {
        width: 150px !important;
        height: 60px !important;
        border: 2px solid #2d5a3a !important;
        border-radius: 8px !important;
        background: #fff !important;
        box-shadow: 0 8px 18px rgba(0,0,0,0.10) !important;
      }

      .suivi-pill{
        background: linear-gradient(135deg, #1a472a, #2d5a3a) !important;
        color: #fff !important;
        border: none !important;
        border-radius: 16px !important;
        padding: 12px 14px !important;
        font-size: 1.05rem !important;
        font-weight: 800 !important;
        box-shadow: 0 10px 22px rgba(26,71,42,0.35) !important;
      }

      .hscroll { gap: 8px !important; }
      .hcard { min-width: 145px !important; max-width: 145px !important; }
      .hcard .biblio-img { height: 80px !important; }
      .acc-pad { padding-left: 12px !important; padding-right: 12px !important; }

      /* --- BARRE TITRE CENTRÉE AVEC FLECHE PROCHE --- */
      .page-topbar {
  display: flex;
  justify-content: center;
  align-items: center;
  gap: 6px;
  padding: 12px 16px;
  border-bottom: 1px solid #e5e7eb;
}
      .page-title {
  font-size: 1.3rem;
  font-weight: 700;
  font-style: italic;
  margin: 0;
  color: #111827;   /* 🔥 couleur uniforme */
}
      .camera-page { padding-top: 18px; }
      .back-arrow-btn {
        background: transparent !important;
        border: none !important;
        font-size: 1.3rem;
        padding: 0 !important;
        color: #1f2937 !important;
      }

      /* --- COMMUNAUTÉ --- */
      .comm-card {
        background: white !important;
        border-radius: 14px !important;
        box-shadow: 0 4px 14px rgba(0,0,0,0.08) !important;
        padding: 12px !important;
        margin-bottom: 12px !important;
      }
      .comm-meta { font-size: 0.8rem !important; color: #6c757d !important; }
      .comm-avatar {
        width: 34px !important;
        height: 34px !important;
        border-radius: 50% !important;
        background: #e9ecef !important;
        display: inline-flex !important;
        align-items: center !important;
        justify-content: center !important;
        font-weight: 700 !important;
        color: #4a7c59 !important;
        flex-shrink: 0 !important;
      }
      .comm-reply {
        background: #f8f9fa !important;
        border-radius: 12px !important;
        padding: 10px !important;
        margin-top: 8px !important;
        border: 1px solid rgba(0,0,0,0.06) !important;
      }
      .comm-reply-meta { font-size: 0.75rem !important; color: #6c757d !important; }
      .comm-actions {
        display:flex;
        gap:8px;
        margin-top:10px;
        align-items:center;
        justify-content: space-between;
      }
      .comm-like { border-radius: 999px !important; }
      .comm-replies-toggle { font-size: 0.85rem; }

      /* --- LOGIN PAGE --- */
      .login-wrap{
        padding: 18px 18px 24px 18px;
      }
      .login-card{
        background:#ffffff;
        border-radius: 16px;
        box-shadow: 0 10px 25px rgba(0,0,0,0.10);
        padding: 16px;
      }
    "))
  ),
  
  theme = bs_theme(version = 5, primary = "#4a7c59", base_font = font_google("Roboto")),
  
  div(class = "phone-container",
      div(class = "phone-notch"),
      
      div(style = "height: 100%; display: flex; flex-direction: column; position: relative;",
          
          # HEADER
          div(class = "app-header",
              div(class = "d-flex justify-content-between align-items-center p-3",
                  style = "background: linear-gradient(135deg, #1a472a 0%, #2d5a3a 50%, #386641 100%);
                           border-bottom: 4px solid #66bb6a;
                           min-height: 66px;
                           box-shadow: 0 4px 15px rgba(26, 71, 42, 0.4);",
                  
                  div(class = "flex-shrink-0 pe-2",
                      tags$img(src = "logo.png", height = "44px", class = "logo-mobile")
                  ),
                  
                  div(class = "flex-grow-1 text-center px-2",
                      div(class = "header-title header-title-main", "Bienvenue sur AgriSave"),
                      div(class = "header-title header-title-sub", "Protégez vos cultures en un seul clic")
                  ),
                  
                  div(class = "flex-shrink-0 ps-2 d-flex flex-column gap-1",
                      # boutons désactivés tant que pas connecté (géré côté server)
                      actionButton("btn_profil", icon("user"),
                                   class = "btn btn-outline-light border-2 rounded-circle p-1",
                                   style = "width: 34px; height: 34px; background: rgba(255,255,255,0.2);"),
                      actionButton("btn_menu", icon("bars"),
                                   class = "btn btn-outline-light border-2 p-1",
                                   style = "width: 34px; height: 34px; background: rgba(255,255,255,0.2);")
                  )
              )
          ),
          
          # CONTENU
          div(id = "main_content", class = "main-content",
              uiOutput("page_content")
          ),
          
          # NAVIGATION BAS (masquée tant que non connecté)
          uiOutput("bottom_nav")
      )
  )
)

# ----------------------------
# 2) SERVER
# ----------------------------
server <- function(input, output, session) {
  
  # =========================================================
  # CHARGER LE MODELE UNE SEULE FOIS (AU DEMARRAGE)
  # =========================================================


  
  #model <- keras::load_model_tf("www/model_plants.keras")
  #model <- keras::load_model_hdf5("www/model_plants.h5")
  #model <- keras::load_model_tf("www/model_plants_tf")
  
  image_size <- 224L
  
  # =========================================================
  # 1) VARIABLES REACTIVES ET CHARGEMENT DE LA MÉMOIRE
  # =========================================================
  
  # Fichiers de sauvegarde
  HIST_FILE <- "www/historique_save.rds"
  ACCUEIL_FILE <- "www/accueil_save.rds"
  
  # --- Initialisation de rv$historique (Suivi) ---
  if (file.exists(HIST_FILE)) {
    initial_hist_suivi <- readRDS(HIST_FILE)
  } else {
    initial_hist_suivi <- tibble::tibble(
      date = as.POSIXct(character()),
      class_id = character(),
      photo_src = character()
    )
  }
  
  rv <- reactiveValues(
    diagnostic_data = NULL,
    confidence = NULL,
    is_from_scan = FALSE,
    last_photo_src = NULL,
    historique = initial_hist_suivi, # Mémoire chargée ici
    lock = FALSE                     # On garde le lock ici
  )
  
  # --- Initialisation de history (Accueil/Bibliothèque) ---
  if (file.exists(ACCUEIL_FILE)) {
    # On définit la variable réactive AVEC le contenu du fichier
    history <- reactiveVal(readRDS(ACCUEIL_FILE))
  } else {
    history <- reactiveVal(
      tibble::tibble(
        time = as.POSIXct(character()),
        source = character(),
        culture = character(),
        pathologie = character(),
        class_id = character(),
        thumb_url = character()
      )
    )
  }
  
  ## Chargement de l'historique et sauvegarde à la connexion ###########################"
  
  
  
  # =========================================================
  # OBSERVER LA PHOTO ET FAIRE LA PREDICTION
  # =========================================================
  observeEvent(input$input_photo, {
    
    req(input$input_photo)
    img_path <- input$input_photo$datapath
    
    # 1. SAUVEGARDE DE LA PHOTO POUR LE SUIVI
    dir.create("www/uploads", showWarnings = FALSE, recursive = TRUE)
    ext <- tools::file_ext(input$input_photo$name)
    new_name <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sample(10000, 1), ".", ext)
    dest <- file.path("www/uploads", new_name)
    file.copy(img_path, dest, overwrite = TRUE)
    rv$last_photo_src <- file.path("uploads", new_name)
    
    # 2. IMAGE POUR LE MODÈLE
    img <- keras::image_load(img_path, target_size = c(image_size, image_size))
    img_array <- keras::image_to_array(img)
    img_array <- array(img_array, dim = c(1, dim(img_array)))
    
    # 3. PRÉDICTION
    pred <- model$predict(img_array)
    class_index <- which.max(pred)
    
    # RÉCUPÉRATION DES NOMS DE CLASSES (Remplace flux_train)
    class_names <- sort(basename(list.dirs(TRAIN_DIR, recursive = FALSE)))
    predicted_class <- class_names[class_index]
    
    # 4. RECHERCHE DANS LE CSV
    diagnostic <- library_data %>%
      dplyr::filter(class_id == predicted_class)
    
    # SÉCURITÉ : évite le crash si le CSV ne correspond pas
    if (nrow(diagnostic) == 0) {
      showNotification("Erreur : Maladie détectée mais absente du dictionnaire (CSV).", type = "error")
      return()
    }
    
    rv$diagnostic_data <- diagnostic
    
    # 5. MISE À JOUR DU SUIVI + MÉMOIRE RDS
    rv$historique <- dplyr::bind_rows(
      tibble::tibble(
        date      = Sys.time(),
        class_id  = predicted_class,
        photo_src = rv$last_photo_src
      ),
      rv$historique
    )
    
    # On écrit physiquement sur le disque pour la prochaine connexion
    saveRDS(rv$historique, "www/historique_save.rds")
  })
  
  # =========================================================
  # AFFICHAGE DU RESULTAT
  # =========================================================
  output$diagnostic_card <- renderUI({
    req(rv$diagnostic_data)
    
    diag <- rv$diagnostic_data
    class_id <- diag$class_id[1]
    
    shiny::column(
      width = 6,
      tags$a(
        href = "javascript:void(0);",
        # On remplace card_onclick(class_id) par ceci :
        onclick = sprintf(
          "Shiny.setInputValue('selected_class', '%s', {priority: 'event'});
           Shiny.setInputValue('clicked_from_scan', Math.random());",
          class_id
        ),
        style = "text-decoration:none; color: inherit;",
        div(
          class = "card biblio-card h-100",
          div(
            class = "biblio-thumb",
            tags$img(
              src = rv$last_photo_src,
              style = "width:100%; height:100%; object-fit:cover;"
            )
          ),
          div(
            class = "card-body text-center",
            div(class = "biblio-title text-success", diag$`Nom de la pathologie`[1]),
            div(class = "biblio-culture text-muted", diag$`Nom de la culture`[1])
          )
        )
      )
    )
  })
  
  ################################### Suivi des cultures########################""
  
  observeEvent(input$go_suivi, {
    go_page("suivi")
  })
  
  
  output$suivi_hist <- renderUI({
    # Si vide → message propre
    if (is.null(rv$historique) || nrow(rv$historique) == 0) {
      return(
        div(
          class = "p-4 text-center",
          style = "background:#f8f9fa; border-radius:16px; color:#2d5a3a;",
          h5("🌿 Aucun diagnostic enregistré"),
          p("Faites votre premier scan pour voir apparaître vos cultures ici.")
        )
      )
    }
    
    # On joint avec le CSV pour avoir culture/pathologie
    hist_join <- rv$historique %>%
      dplyr::filter(!is.na(class_id), class_id != "") %>%
      dplyr::left_join(library_data, by = "class_id")
    
    if (nrow(hist_join) == 0) {
      return(
        div(class = "p-4 text-center",
            h5("🌿 Aucun diagnostic valide trouvé"))
      )
    }
    
    div(
      class = "row g-3",
      lapply(seq_len(nrow(hist_join)), function(i) {
        one <- hist_join[i, ]
        
        shiny::column(
          width = 6,
          tags$a(
            href = "javascript:void(0);",
            # Utilisation de sprintf pour garantir que l'ID est bien passé
            onclick = sprintf("Shiny.setInputValue('selected_class', '%s', {priority: 'event'});", one$class_id),
            style = "text-decoration:none; color: inherit;",
            div(
              class = "card biblio-card h-100",
              div(
                class = "biblio-thumb",
                tags$img(
                  src = one$photo_src, # On utilise le chemin stocké dans le join
                  style = "width:100%; height:100%; object-fit:cover;"
                )
              ),
              div(
                class = "card-body text-center",
                # Rappel : Ces classes .biblio-title utilisent vos réglages de police
                div(class = "biblio-title text-success", one$`Nom de la pathologie`),
                div(class = "biblio-culture text-muted", one$`Nom de la culture`)
              )
            )
          )
        )
      })
    )
  })
  
  
  
  observeEvent(input$retour_suivi, {
    go_page("accueil")
  })
  
  #########################################################################################################"
  # Helpers
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  norm_region <- function(x) {
    x %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      stringr::str_to_lower() %>%
      stringr::str_replace_all("[^a-z0-9]+", " ") %>%
      stringr::str_squish()
  }
  
  # ✅ LOGIN GATE
  is_logged_in <- reactiveVal(FALSE)
  
  # ✅ Démarrage sur login
  current_page <- reactiveVal("login")
  
  # ✅ Historique de navigation (pile)
  nav_history <- reactiveVal(character())
  
  # Aller vers une page + mémoriser la page actuelle
  go_page <- function(page) {
    old <- current_page()
    if (!is.null(old) && old != page) {
      # ne garde pas "login" dans l'historique (optionnel mais conseillé)
      if (old != "login") nav_history(c(nav_history(), old))
    }
    current_page(page)
  }
  
  # Retour à la page précédente
  go_back <- function() {
    h <- nav_history()
    if (length(h) == 0) {
      current_page("accueil")  # fallback
    } else {
      last <- tail(h, 1)
      nav_history(head(h, -1))
      current_page(last)
    }
  }
  
  # Bibliothèque filtres
  current_filter <- reactiveVal("tous")
  selected_culture <- reactiveVal("")
  selected_pathologie <- reactiveVal("")
  
  # ===========================================================================
  # 👤 USERS DB (persistant) : Garder en memoire les données après inscription
  # ========================================================================
  USERS_FILE <- "users_db.rds"
  
  load_users <- function() {
    if (file.exists(USERS_FILE)) {
      readRDS(USERS_FILE)
    } else {
      data.frame(
        email = character(0),
        pass  = character(0),
        prenom = character(0),
        nom = character(0),
        stringsAsFactors = FALSE
      )
    }
  }
  
  save_users <- function(df) {
    saveRDS(df, USERS_FILE)
  }
  
  users_db <- reactiveVal(load_users())
  
  # Carte états
  ##############################################"""
  selected_region <- reactiveVal(NULL)
  selected_culture_map <- reactiveVal(NULL)
  
  # =========================================================
  # HISTORIQUE (3 max)
  # =========================================================
  MAX_HISTORY <- 3
  
  ############################################J'ai supprimé#######################################################""
  
  add_to_history <- function(source, culture, pathologie, class_id, thumb_url) {
    h <- history()
    
    new_row <- tibble(
      time = Sys.time(),
      source = source,
      culture = culture %||% "",
      pathologie = pathologie %||% "",
      class_id = class_id %||% "",
      thumb_url = thumb_url %||% ""
    )
    
    if (nrow(h) > 0) h <- h %>% filter(class_id != new_row$class_id)
    h <- bind_rows(new_row, h)
    if (nrow(h) > MAX_HISTORY) h <- h[1:MAX_HISTORY, ]
    
    history(h)
    saveRDS(h, "www/accueil_save.rds") # ✅ Écriture physique
  }
  
  # =========================================================
  # COMMUNAUTÉ (posts + user)
  # =========================================================
  comm_posts <- reactiveVal(list())
  current_user <- reactiveVal(NULL)
  
  # ----------------------------
  # COMMUNAUTÉ : badge utilisateur (SANS "pas connecté")
  # ----------------------------
  output$comm_user_badge <- renderUI(NULL)
  
  # ----------------------------
  # Bouton caméra -> ouvre fileInput caché
  # ----------------------------
  observeEvent(input$comm_btn_photo, {
    req(is_logged_in())
    shinyjs::click("comm_photo")
  })
  
  # ----------------------------
  # Aperçu photo sélectionnée
  # ----------------------------
  output$comm_photo_preview <- renderUI({
    req(is_logged_in())
    
    f <- input$comm_photo
    if (is.null(f)) return(NULL)
    
    div(class="mt-2",
        div(class="text-muted", style="font-size:0.85rem;", paste("Photo :", f$name)),
        tags$img(src = f$datapath, style="width:100%; border-radius:12px; object-fit:cover;")
    )
  })
  
  # ----------------------------
  # Publier un post (texte + photo)
  # ----------------------------
  observeEvent(input$comm_publier, {
    req(is_logged_in())
    u <- current_user()
    req(!is.null(u))
    
    txt <- trimws(input$comm_question %||% "")
    f <- input$comm_photo
    
    if (txt == "" && is.null(f)) {
      showNotification("Écris un message ou ajoute une photo.", type="error")
      return()
    }
    
    # photo -> data URI pour affichage (simple et portable)
    photo_uri <- NULL
    if (!is.null(f)) {
      ext <- tools::file_ext(f$name)
      mime <- if (tolower(ext) %in% c("jpg","jpeg")) "image/jpeg" else "image/png"
      b64 <- base64enc::base64encode(f$datapath)
      photo_uri <- paste0("data:", mime, ";base64,", b64)
    }
    
    post <- list(
      id = paste0("p_", as.integer(Sys.time() * 1000)),
      author = u$name,              # ✅ Nom + Prénom déjà dans u$name
      when = Sys.time(),
      text = txt,
      photo = photo_uri,
      liked_by = character(0),
      replies = list(),
      show_all_replies = FALSE
    )
    
    comm_posts(append(list(post), comm_posts()))  # plus récents en haut
    
    updateTextInput(session, "comm_question", value = "")
    shinyjs::reset("comm_photo")
    showNotification("Publication ajoutée ✅", type="message")
  })
  
  # ----------------------------
  # FEED (avec likes + replies + toggle)
  # ----------------------------
  output$comm_feed <- renderUI({
    req(is_logged_in())
    u <- current_user()
    req(!is.null(u))
    uid <- u$id
    
    posts <- comm_posts()
    
    if (!is.null(input$comm_sort) && input$comm_sort == "old") {
      posts <- rev(posts)
    }
    
    if (length(posts) == 0) {
      return(
        div(class = "text-center text-muted py-4",
            icon("comments", class = "fa-2x mb-2"),
            div("Aucune publication pour le moment."),
            div(style="font-size:0.9rem;", "Soyez le premier à poser une question.")
        )
      )
    }
    
    tagList(
      lapply(posts, function(p) {
        initial <- toupper(substr(p$author, 1, 1))
        
        liked <- !is.null(p$liked_by) && (uid %in% p$liked_by)
        likes_count <- if (is.null(p$liked_by)) 0 else length(p$liked_by)
        n_replies <- if (is.null(p$replies)) 0 else length(p$replies)
        
        div(class = "comm-card",
            div(class = "d-flex gap-2 align-items-start",
                div(class = "comm-avatar", initial),
                div(style = "flex:1;",
                    div(class = "d-flex justify-content-between align-items-center",
                        tags$b(p$author),
                        span(class = "comm-meta", format(p$when, "%d/%m %H:%M"))
                    ),
                    if (!is.null(p$text) && p$text != "") div(p$text),
                    if (!is.null(p$photo)) {
                      div(class="mt-2",
                          tags$img(
                            src = p$photo,
                            style = "width:100%; border-radius:12px; object-fit:cover;"
                          )
                      )
                    },
                    
                    div(class = "comm-actions",
                        div(class="d-flex gap-2",
                            actionButton(
                              paste0("like_btn_", p$id),
                              label = paste0(ifelse(liked, "❤️ ", "🤍 "), likes_count),
                              class = paste("btn btn-sm", ifelse(liked, "btn-success", "btn-outline-success"), "comm-like")
                            ),
                            actionButton(
                              paste0("reply_btn_", p$id),
                              label = paste0("Répondre", if (n_replies > 0) paste0(" (", n_replies, ")") else ""),
                              icon = icon("reply"),
                              class = "btn btn-outline-success btn-sm"
                            )
                        ),
                        if (n_replies > 1) {
                          actionButton(
                            paste0("toggle_replies_", p$id),
                            label = ifelse(isTRUE(p$show_all_replies), "Voir moins", "Voir plus"),
                            class = "btn btn-link text-success comm-replies-toggle"
                          )
                        }
                    ),
                    
                    # Réponses
                    if (!is.null(p$replies) && length(p$replies) > 0) {
                      
                      replies_to_show <- p$replies
                      if (!isTRUE(p$show_all_replies) && length(replies_to_show) > 1) {
                        replies_to_show <- tail(replies_to_show, 1)
                      }
                      
                      tagList(lapply(replies_to_show, function(rp) {
                        div(class="comm-reply",
                            div(class="d-flex justify-content-between align-items-center",
                                tags$b(rp$author),
                                span(class="comm-reply-meta", format(rp$when, "%d/%m %H:%M"))
                            ),
                            if (!is.null(rp$text) && rp$text != "") div(rp$text),
                            if (!is.null(rp$photo)) {
                              div(class="mt-2",
                                  tags$img(
                                    src = rp$photo,
                                    style="width:100%; border-radius:10px; object-fit:cover;"
                                  )
                              )
                            }
                        )
                      }))
                    }
                )
            )
        )
      })
    )
  })
  
  # ----------------------------
  # OBSERVERS dynamiques : LIKE / TOGGLE / REPLY
  # ----------------------------
  observe({
    req(is_logged_in())
    u <- current_user(); req(!is.null(u))
    uid <- u$id
    
    posts <- comm_posts()
    if (length(posts) == 0) return()
    
    lapply(posts, function(p) {
      # LIKE
      observeEvent(input[[paste0("like_btn_", p$id)]], {
        posts2 <- comm_posts()
        idx <- which(vapply(posts2, function(x) x$id, character(1)) == p$id)
        if (length(idx) != 1) return()
        
        lb <- posts2[[idx]]$liked_by
        if (is.null(lb)) lb <- character(0)
        
        if (uid %in% lb) posts2[[idx]]$liked_by <- setdiff(lb, uid)
        else posts2[[idx]]$liked_by <- unique(c(lb, uid))
        
        comm_posts(posts2)
      }, ignoreInit = TRUE)
      
      # TOGGLE réponses
      observeEvent(input[[paste0("toggle_replies_", p$id)]], {
        posts2 <- comm_posts()
        idx <- which(vapply(posts2, function(x) x$id, character(1)) == p$id)
        if (length(idx) != 1) return()
        
        posts2[[idx]]$show_all_replies <- !isTRUE(posts2[[idx]]$show_all_replies)
        comm_posts(posts2)
      }, ignoreInit = TRUE)
      
      # REPLY (modal)
      observeEvent(input[[paste0("reply_btn_", p$id)]], {
        
        showModal(modalDialog(
          title = paste("Répondre à", p$author),
          textAreaInput("reply_text", NULL, placeholder = "Votre réponse...", width="100%"),
          footer = tagList(
            modalButton("Annuler"),
            actionButton("send_reply", "Envoyer", class="btn-success")
          ),
          easyClose = TRUE
        ))
        
        observeEvent(input$send_reply, {
          posts2 <- comm_posts()
          idx <- which(vapply(posts2, function(x) x$id, character(1)) == p$id)
          if (length(idx) != 1) return()
          
          rt <- trimws(input$reply_text %||% "")
          if (rt == "") {
            showNotification("Écris une réponse.", type="error")
            return()
          }
          
          rp <- list(author = u$name, when = Sys.time(), text = rt, photo = NULL)
          
          if (is.null(posts2[[idx]]$replies)) posts2[[idx]]$replies <- list()
          posts2[[idx]]$replies <- append(posts2[[idx]]$replies, list(rp))
          
          comm_posts(posts2)
          removeModal()
        }, ignoreInit = TRUE, once = TRUE)
        
      }, ignoreInit = TRUE)
    })
  })
  
  # ✅ Bottom nav affichée seulement si connecté
  output$bottom_nav <- renderUI({
    req(isTRUE(is_logged_in()))
    div(class = "bottom-nav d-flex justify-content-around align-items-center",
        actionButton("nav_accueil", label = "Accueil", icon = icon("house"),
                     class = "btn btn-link text-success d-flex flex-column align-items-center p-1 fw-bold",
                     style = "font-size: 0.72rem;"),
        actionButton("nav_scan_bas", label = NULL, icon = icon("camera"),
                     class = "btn bg-success text-white rounded-circle p-3",
                     style = "width: 58px; height: 58px; margin-top: -32px;
                                    box-shadow: 0 6px 20px rgba(76,175,80,0.4);"),
        actionButton("nav_aide", label = "Aide", icon = icon("circle-question"),
                     class = "btn btn-link text-success d-flex flex-column align-items-center p-1 fw-bold",
                     style = "font-size: 0.72rem;")
    )
  })
  
  # ----------------------------
  # NAVIGATION BAS (protégée)
  # ----------------------------
  observeEvent(input$nav_accueil, { req(is_logged_in()); go_page("accueil") })
  observeEvent(input$nav_aide, { req(is_logged_in()); go_page("aide") })
  observeEvent(input$nav_scan_bas, { req(is_logged_in()); go_page("camera") })
  observeEvent(input$btn_scan_rapide, { req(is_logged_in()); go_page("camera") })
  
  # MENU -> pages (protégé)
  observeEvent(input$go_bibliotheque, { req(is_logged_in()); removeModal(); go_page("bibliotheque") })
  observeEvent(input$go_carte,        { req(is_logged_in()); removeModal(); go_page("carte") })
  observeEvent(input$go_communaute,   { req(is_logged_in()); removeModal(); go_page("communaute") })
  observeEvent(input$go_apropos,      { req(is_logged_in()); removeModal(); go_page("A propos") })
  
  # RETOURS
  observeEvent(input$back_biblio,   { req(is_logged_in()); go_back() })
  observeEvent(input$retour_camera, { req(is_logged_in()); go_back() })
  observeEvent(input$back_generic,  { req(is_logged_in()); go_back() })
  
  # ----------------------------
  # Bibliothèque : logique onglets intégrés
  # ----------------------------
  observeEvent(input$tab_tous, {
    req(is_logged_in())
    current_filter("tous")
    selected_culture("")
    selected_pathologie("")
  })
  
  observeEvent(input$tab_culture_select, {
    req(is_logged_in())
    current_filter("cultures")
    selected_culture(input$tab_culture_select %||% "")
  }, ignoreInit = TRUE)
  
  observeEvent(input$tab_patho_select, {
    req(is_logged_in())
    current_filter("pathologies")
    selected_pathologie(input$tab_patho_select %||% "")
  }, ignoreInit = TRUE)
  
  # Helper clic carte (bibliothèque)
  card_onclick <- function(class_id) {
    sprintf(
      "Shiny.setInputValue('selected_class', '%s', {priority: 'event'}); return false;",
      gsub("'", "\\\\'", class_id)
    )
  }
  
  # =========================================================
  # CARTE : préparer data_long depuis TON df_csv
  # =========================================================
  data_long <- reactive({
    df <- df_csv %>% mutate(across(everything(), ~ as.character(.x)))
    
    df %>%
      mutate(`Régions de culture en France` = str_replace_all(`Régions de culture en France`, "\\s*,\\s*", ",")) %>%
      tidyr::separate_rows(`Régions de culture en France`, sep = ",") %>%
      mutate(
        region = str_trim(`Régions de culture en France`),
        culture = str_trim(`Nom de la culture`),
        region_key = norm_region(region)
      ) %>%
      filter(region != "", culture != "") %>%
      distinct(region, culture, region_key)
  })
  
  regions_for_selected_culture <- reactive({
    sc <- selected_culture_map()
    if (is.null(sc) || sc == "") return(character(0))
    
    dl <- data_long()
    dl %>%
      filter(trimws(culture) == trimws(sc)) %>%
      pull(region_key) %>%
      unique()
  })
  
  popup_html_for_region <- function(sr) {
    dl <- data_long()
    sr_key <- norm_region(sr)
    
    cultures_region <- dl %>%
      filter(region_key == sr_key) %>%
      arrange(culture) %>%
      pull(culture) %>%
      unique()
    
    n <- length(cultures_region)
    lst <- if (n == 0) {
      "Aucune culture dans le CSV"
    } else {
      paste0("<ul><li>", paste(cultures_region, collapse = "</li><li>"), "</li></ul>")
    }
    
    list(
      n = n,
      html = paste0(
        "<b>Région : </b>", sr, "<br/>",
        "<b>Nombre de cultures : </b>", n, "<br/>",
        "<b>Liste :</b>", lst
      )
    )
  }
  
  # Initialiser les menus région/culture quand on entre dans la page carte
  observeEvent(current_page(), {
    req(is_logged_in())
    req(current_page() == "carte")
    
    dl <- data_long()
    if (is.null(dl) || nrow(dl) == 0) {
      showNotification("Carte: aucune donnée Région/Culture trouvée dans le CSV.", type = "error", duration = 8)
      return()
    }
    
    regions <- sort(unique(dl$region))
    cultures <- sort(unique(dl$culture))
    
    rv$lock <- TRUE
    on.exit({ rv$lock <- FALSE }, add = TRUE)
    
    updateSelectInput(session, "sel_region",
                      choices = c("— Région —" = "", regions), selected = "")
    updateSelectInput(session, "sel_culture",
                      choices = c("— Culture —" = "", cultures), selected = "")
    
    selected_region(NULL)
    selected_culture_map(NULL)
  })
  
  observeEvent(input$sel_region, {
    req(is_logged_in())
    sr <- input$sel_region
    if (is.null(sr) || sr == "") selected_region(NULL) else selected_region(sr)
  }, ignoreInit = TRUE)
  
  observeEvent(input$sel_culture, {
    req(is_logged_in())
    sc <- input$sel_culture
    if (is.null(sc) || sc == "") selected_culture_map(NULL) else selected_culture_map(sc)
  }, ignoreInit = TRUE)
  
  observeEvent(input$reset_filters, {
    req(is_logged_in())
    dl <- data_long()
    
    rv$lock <- TRUE
    on.exit({ rv$lock <- FALSE }, add = TRUE)
    
    regions <- sort(unique(dl$region))
    cultures <- sort(unique(dl$culture))
    
    updateSelectInput(session, "sel_region",
                      choices = c("— Région —" = "", regions), selected = "")
    updateSelectInput(session, "sel_culture",
                      choices = c("— Culture —" = "", cultures), selected = "")
    
    selected_region(NULL)
    selected_culture_map(NULL)
  })
  
  regions_sf <- reactive({
    geo_path <- "regions.geojson"
    
    if (!file.exists(geo_path)) {
      ok <- tryCatch({
        download.file(
          "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/regions.geojson",
          destfile = geo_path,
          mode = "wb"
        )
        TRUE
      }, error = function(e) FALSE)
      
      if (!ok) return(NULL)
    }
    
    reg <- tryCatch(sf::st_read(geo_path, quiet = TRUE), error = function(e) NULL)
    if (is.null(reg)) return(NULL)
    if (!("nom" %in% names(reg))) return(NULL)
    
    reg$region_key <- norm_region(reg$nom)
    reg
  })
  
  output$map_regions <- renderLeaflet({
    req(is_logged_in())
    reg <- regions_sf()
    validate(need(!is.null(reg), "Impossible de charger la carte : regions.geojson introuvable ou téléchargement bloqué."))
    
    leaflet(reg) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = "white", fillOpacity = 0, color = "#555555", weight = 1, layerId = ~nom)
  })
  
  observeEvent(input$map_regions_shape_click, {
    req(is_logged_in())
    click <- input$map_regions_shape_click
    req(click$id)
    
    rv$lock <- TRUE
    on.exit({ rv$lock <- FALSE }, add = TRUE)
    
    updateSelectInput(session, "sel_region", selected = click$id)
    selected_region(click$id)
  })
  
  observeEvent(list(selected_region(), selected_culture_map()), {
    req(is_logged_in())
    req(current_page() == "carte")
    
    reg <- regions_sf()
    req(!is.null(reg))
    
    sr <- selected_region()
    sc <- selected_culture_map()
    
    proxy <- leafletProxy("map_regions", data = reg) %>% clearShapes()
    
    proxy <- proxy %>% addPolygons(
      data = reg, fillColor = "white", fillOpacity = 0,
      color = "#555555", weight = 1, layerId = ~nom
    )
    
    if (!is.null(sr) && sr != "") {
      reg_sel <- reg[reg$nom == sr, ]
      if (nrow(reg_sel) == 1) {
        info <- popup_html_for_region(sr)
        proxy %>% addPolygons(
          data = reg_sel,
          fillColor = "red", fillOpacity = 0.7,
          color = "#555555", weight = 2,
          label = paste0(sr, " — ", info$n, " cultures"),
          popup = info$html, layerId = ~nom
        )
      }
      return()
    }
    
    if (!is.null(sc) && sc != "") {
      keys <- regions_for_selected_culture()
      reg_sel <- reg[reg$region_key %in% keys, ]
      
      if (nrow(reg_sel) > 0) {
        popups <- vapply(reg_sel$nom, function(rn) popup_html_for_region(rn)$html, character(1))
        nvals  <- vapply(reg_sel$nom, function(rn) popup_html_for_region(rn)$n, numeric(1))
        
        proxy %>% addPolygons(
          data = reg_sel,
          fillColor = "red", fillOpacity = 0.7,
          color = "#555555", weight = 2,
          label = paste0(reg_sel$nom, " — ", nvals, " cultures"),
          popup = popups, layerId = ~nom
        )
      }
    }
  }, ignoreInit = TRUE)
  
  # =========================================================
  # LOGIN / INSCRIPTION : maintenant AVANT l'accueil
  # =========================================================
  observeEvent(input$do_login, {
    
    email <- trimws(input$login_email %||% "")
    pass  <- trimws(input$login_pass %||% "")
    
    if (email == "" || pass == "") {
      showNotification("Entrez votre email et mot de passe.", type = "error")
      return()
    }
    
    db <- users_db()
    
    row <- db[db$email == email & db$pass == pass, , drop = FALSE]
    
    if (nrow(row) == 0) {
      showNotification("Email ou mot de passe incorrect ❌", type = "error")
      return()
    }
    
    current_user(list(
      id = paste0("u_", as.integer(Sys.time())),
      name = paste(row$prenom[1], row$nom[1]),
      email = row$email[1]
    ))
    
    is_logged_in(TRUE)
    nav_history(character())  # reset historique
    go_page("accueil")
    
    showNotification(paste0("Bienvenue ", row$prenom[1], " ✅"), type = "message")
  })
  
  
  ###
  observeEvent(input$do_signin, {
    
    prenom <- trimws(input$signin_prenom %||% "")
    nom    <- trimws(input$signin_nom %||% "")
    email  <- trimws(input$signin_email %||% "")
    pass   <- trimws(input$signin_pass %||% "")
    
    if (prenom == "" || nom == "" || email == "" || pass == "") {
      showNotification("Veuillez remplir tous les champs.", type = "error")
      return()
    }
    
    db <- users_db()
    
    # email déjà inscrit
    if (email %in% db$email) {
      showNotification("Cet email est déjà inscrit. Connectez-vous.", type = "warning")
      return()
    }
    
    # ajouter utilisateur
    db <- rbind(db, data.frame(
      email  = email,
      pass   = pass,
      prenom = prenom,
      nom    = nom,
      stringsAsFactors = FALSE
    ))
    
    users_db(db)
    save_users(db)
    
    # connecter automatiquement après inscription
    current_user(list(
      id = paste0("u_", as.integer(Sys.time())),
      name = paste(prenom, nom),
      email = email
    ))
    
    is_logged_in(TRUE)
    nav_history(character())  # reset historique
    go_page("accueil")
    
    showNotification(paste0("Inscription réussie ✅ Bienvenue ", prenom, " !"), type = "message")
  })
  
  # ----------------------------
  # NOTEZ NOUS : rating étoiles
  # ----------------------------
  rating_val <- reactiveVal(0)
  
  observeEvent(input$rate_1, { rating_val(1) })
  observeEvent(input$rate_2, { rating_val(2) })
  observeEvent(input$rate_3, { rating_val(3) })
  observeEvent(input$rate_4, { rating_val(4) })
  observeEvent(input$rate_5, { rating_val(5) })
  
  output$rating_text <- renderUI({
    r <- rating_val()
    if (r == 0) {
      tags$span(class = "text-muted", "Cliquez sur une étoile pour noter l'application.")
    } else {
      tags$span(style = "font-weight:700; color:#1a472a;",
                paste0("Merci ! Vous avez donné ", r, "/5 ⭐"))
    }
  })
  
  # Mettre les étoiles en couleur (jaune) selon la note
  observe({
    r <- rating_val()
    ids <- paste0("rate_", 1:5)
    
    for (i in 1:5) {
      if (i <= r) {
        shinyjs::runjs(sprintf(
          "document.getElementById('%s') && (document.getElementById('%s').style.color = '#f59e0b');",
          ids[i], ids[i]
        ))
      } else {
        shinyjs::runjs(sprintf(
          "document.getElementById('%s') && (document.getElementById('%s').style.color = '#9ca3af');",
          ids[i], ids[i]
        ))
      }
    }
  })
  
  # ------------------------------------------------------------
  # UI PAGES
  # ------------------------------------------------------------
  output$page_content <- renderUI({
    page <- current_page()
    
    # ✅ sécurité : si pas connecté, on force login
    if (!isTRUE(is_logged_in()) && page != "login") {
      current_page("login")
      page <- "login"
    }
    
    if (page == "login") {
      tagList(
        div(class="login-wrap",
            div(class="login-card",
                h3("Connexion", class="fw-bold text-success text-center mb-2"),
                p("Connectez-vous pour accéder à AgriSave.", class="text-muted text-center mb-3"),
                
                tabsetPanel(
                  tabPanel("Connexion",
                           div(class="mt-3",
                               textInput("login_email", "Email", placeholder = "ex: moi@agrisave.com"),
                               passwordInput("login_pass", "Mot de passe"),
                               actionButton("do_login", "Se connecter", class = "btn-success w-100 mt-2")
                           )
                  ),
                  tabPanel("Inscription",
                           div(class="mt-3",
                               textInput("signin_prenom", "Prénom"),
                               textInput("signin_nom", "Nom"),
                               textInput("signin_email", "Email"),
                               passwordInput("signin_pass", "Créer un mot de passe"),
                               actionButton("do_signin", "S'inscrire", class = "btn-success w-100 mt-2")
                           )
                  )
                )
            )
        )
      )
      
    } else if (page == "accueil") {
      tagList(
        div(class = "m-3 shadow-lg overflow-hidden position-relative hero-box",
            tags$img(
              src = "hero.jpg.jpeg",
              style = "width:100%; height:100%; object-fit:cover; filter: brightness(0.92);"
            ),
            div(
              style = "position:absolute; top:50%; left:50%; transform:translate(-50%,-50%);
                   width:92%; text-align:center; color:white; font-weight:700;
                   text-shadow: 2px 2px 4px black;",
              span(class = "hero-text",
                   "Parce que vivre est un droit, ici nous défendons une seule cause : La survie des plantes")
            )
        ),
        
        div(class = "d-flex justify-content-end align-items-center acc-pad mt-2",
            div(style = "text-align:right;",
                div(class = "steps-title mb-1", "L'application en 3 étapes"),
                div(class = "steps-icons",
                    div(class = "icon-circle", icon("camera")),
                    div(class = "icon-circle", icon("magnifying-glass")),
                    div(class = "icon-circle", icon("seedling"))
                )
            )
        ),
        
        div(class = "acc-pad",
            div(class = "section-title text-dark", "Scan rapide"),
            div(class = "d-flex justify-content-center mb-3",
                actionButton(
                  "btn_scan_rapide",
                  label = NULL,
                  icon = icon("camera", class = "fa-2x text-success"),
                  class = "btn scan-btn-box"
                )
            )
        ),
        ##################################################""
        div(class = "acc-pad mb-3",
            actionLink("go_suivi",
                       label = "suivi de mes cultures",
                       class = "suivi-pill text-center d-block",
                       style = "text-decoration: none;")
        ),
        #######################################"
        div(class = "acc-pad",
            uiOutput("recent_views")
        )
      )
      
    } else if (page == "bibliotheque") {
      tagList(
        div(class="page-topbar",
            actionButton("back_biblio", label = NULL, icon = icon("arrow-left"), class = "back-arrow-btn"),
            h3("Bibliothèque", class="page-title")
        ),
        div(class = "px-3 mb-2",
            div(class = "search-bar d-flex align-items-center gap-2",
                icon("magnifying-glass", class = "text-muted"),
                textInput("biblio_search", label = NULL, placeholder = "Rechercher", width = "100%") %>%
                  tagAppendAttributes(class = "search-input")
            )
        ),
        div(class = "tabs-row mb-1",
            actionButton("tab_tous", "Tous", class = "tab-btn"),
            div(class = "tab-select",
                selectInput("tab_culture_select", label = NULL,
                            choices = c("Cultures" = "", all_cultures),
                            width = "135px")
            ),
            div(class = "tab-select",
                selectInput("tab_patho_select", label = NULL,
                            choices = c("Pathologies" = "", all_pathologies),
                            width = "135px")
            )
        ),
        div(class = "hint-line mb-2",
            "Astuce : choisissez une culture/pathologie avec ▾, ou utilisez Rechercher."
        ),
        div(class = "px-3 pb-4", uiOutput("biblio_content"))
      )
      
    } else if (page == "camera") {
      tagList(
        div(class="page-topbar",
            actionButton("retour_camera",
                         label = NULL,
                         icon = icon("arrow-left"),
                         class = "back-arrow-btn"),
            h3("Caméra", class="page-title")
        ),
        div(class = "camera-page d-flex flex-column align-items-center justify-content-start p-4 text-center",
            div(class = "mb-3 mt-2", icon("camera", class = "fa-4x text-success")),
            h2(
              tags$span("📸", style = "margin-right:12px; display:inline-block;"),
              "Analyse en cours",
              class = "text-success mb-3 fw-bold"
            ),
            p("Veuillez prendre une photo de la feuille affectée", class = "text-muted mb-4 fs-6"),
            div(class = "w-100 mb-4",
                fileInput("input_photo", label = NULL, accept = c("image/*"),
                          capture = "environment", placeholder = "Appuyez sur l'appareil photo..."),
                uiOutput("diagnostic_card")
            )
        )
      )
      
    } else if (page == "aide") {
      
      tagList(
        
        # --- BARRE TITRE AVEC FLECHE ---
        div(class="page-topbar",
            actionButton("back_generic",
                         label = NULL,
                         icon = icon("arrow-left"),
                         class = "back-arrow-btn"),
            h3("Aide", class="page-title")
        ),
        
        div(class = "p-3",
            
            # Titre texte
            div(class = "text-center mb-3",
                p("Découvrez comment utiliser AgriSave pour protéger vos cultures.", class = "text-muted mb-0")
            ),
            
            # 3 étapes
            div(class = "card shadow-sm mb-3",
                div(class = "card-body",
                    h5(class = "fw-bold text-success mb-2", "📷 Camera/ Diagnostique"),
                    tags$ol(class = "mb-0",
                            tags$li("Prenez une photo (Caméra) ou importez une image."),
                            tags$li("L'application analyse la photo pour détecter la pathologie."),
                            tags$li("Vous recevez les symptômes et les recommandations adaptées.")
                    )
                )
            ),
            
            # Bibliothèque
            div(class = "card shadow-sm mb-3",
                div(class = "card-body",
                    h5(class = "fw-bold text-success mb-2", "📚 Bibliothèque"),
                    tags$ul(class = "mb-0",
                            tags$li(tags$b("Rechercher : "), "tapez un nom de culture ou de pathologie."),
                            tags$li(tags$b("Filtrer : "), "choisissez une Culture▾ ou une Pathologie▾."),
                            tags$li(tags$b("Consulter : "), "cliquez sur une image pour voir la fiche complète (description, symptômes, recommandations).")
                    )
                )
            ),
            
            # Communauté
            div(class = "card shadow-sm mb-3",
                div(class = "card-body",
                    h5(class = "fw-bold text-success mb-2", "👥 Communauté"),
                    p(class = "text-muted mb-2",
                      "La Communauté vous permet d’échanger avec d’autres agriculteurs :"),
                    tags$ul(class = "mb-0",
                            tags$li(tags$b("Poser une question : "),
                                    "décrivez votre problème (culture, symptômes, période, région) et ajoutez une photo si possible."),
                            tags$li(tags$b("Partager une observation : "),
                                    "publiez une photo d’une maladie rencontrée et vos solutions appliquées."),
                            tags$li(tags$b("Répondre aux autres : "),
                                    "proposez des conseils ou des pratiques préventives."),
                            tags$li(tags$b("Bonnes pratiques : "),
                                    "restez courtois, soyez précis, et évitez les traitements dangereux non recommandés.")
                    ),
                    div(class = "mt-3 p-2 rounded",
                        style = "background:#f1f5f9; border-left: 4px solid #4a7c59;",
                        tags$b("Astuce : "),
                        "Plus votre description est claire (photo nette + symptômes + culture), plus les réponses seront utiles."
                    )
                )
            ),
            
            # Conseils photo
            div(class = "card shadow-sm mb-3",
                div(class = "card-body",
                    h5(class = "fw-bold text-success mb-2", "📸 Conseils pour une bonne photo"),
                    tags$ul(class = "mb-0",
                            tags$li("Bonne lumière (idéalement naturelle)."),
                            tags$li("Feuille nette et centrée (éviter le flou)."),
                            tags$li("Montrez la zone malade + un plan plus large si possible."),
                            tags$li("Fond simple (éviter trop d’éléments derrière).")
                    )
                )
            ),
            
            # Navigation
            div(class = "card shadow-sm mb-0",
                div(class = "card-body",
                    h5(class = "fw-bold text-success mb-2", "🧭 Navigation"),
                    tags$ul(class = "mb-0",
                            tags$li(tags$b("Accueil : "), "scan rapide + accès aux fonctionnalités."),
                            tags$li(tags$b("Caméra : "), "prendre/importer une photo."),
                            tags$li(tags$b("Aide : "), "mode d'emploi."),
                            tags$li(tags$b("Menu : "), "bibliothèque, carte, communauté, à propos.")
                    )
                )
            )
        )
      ) ###################################################################################################################""""
    } else if (page == "suivi") {
      
      tagList(
        div(class="page-topbar",
            actionButton("retour_suivi",
                         label = NULL,
                         icon = icon("arrow-left"),
                         class = "back-arrow-btn"),
            h3("Suivi de mes cultures", class="page-title")
        ),
        # AJOUT DU BOUTON ICI
        div(class="px-3 mb-3",
            actionButton("clear_all_hist", "🗑️ Effacer tout mon historique",
                         class="btn btn-outline-danger w-100",
                         style="border-radius:12px; font-weight:600;")
        ),
        
        div(class = "acc-pad",
            uiOutput("suivi_hist")
        )
      ) #################################################################################################################"""
    } else if (page == "carte") {
      tagList(
        div(class="page-topbar",
            actionButton("back_generic", label = NULL, icon = icon("arrow-left"), class = "back-arrow-btn"),
            h3("Carte", class="page-title")
        ),
        div(class = "px-3 mt-2 text-center",
            p("Sélectionnez une région ou une culture.", class="text-muted mb-2")
        ),
        div(class = "px-3 mb-2 d-flex gap-2",
            selectInput("sel_region", NULL, choices = c("— Région —" = ""), selected = "", width = "50%"),
            selectInput("sel_culture", NULL, choices = c("— Culture —" = ""), selected = "", width = "50%")
        ),
        div(class = "px-3",
            actionButton("reset_filters", "Réinitialiser", class = "btn btn-outline-success w-100 mb-2")
        ),
        leafletOutput("map_regions", height = 520)
      )
      
    } else if (page == "communaute") {
      
      tagList(
        # ✅ Topbar identique aux autres (sans icône, sans vert)
        div(class="page-topbar",
            actionButton("back_generic",
                         label = NULL,
                         icon = icon("arrow-left"),
                         class = "back-arrow-btn"),
            h3("Communauté", class="page-title")
        ),
        
        # Texte d’accueil (tu peux garder)
        div(class="p-3",
            div(class = "p-2 rounded mt-1",
                style = "background: #e9ecef; font-size: 0.9rem;",
                tags$b("Bienvenue dans la communauté !"), tags$br(),
                "Décrivez les pathologies ou partagez les photos de vos plantes malades afin d’obtenir des conseils et des solutions grâce à l’entraide de la communauté."
            )
        ),
        
        
        # Barre de publication (question + photo)
        div(class = "px-3 mb-2",
            div(class = "d-flex align-items-center gap-2",
                textInput("comm_question", NULL, placeholder = "Posez une question", width = "100%"),
                actionButton(
                  "comm_btn_photo",
                  label = NULL,
                  icon = icon("camera"),
                  class = "btn btn-success",
                  style = "height: 38px; width: 90px;"
                )
            ),
            
            # ✅ fileInput caché (plus de "Browse...")
            div(style = "display:none;",
                fileInput(
                  "comm_photo", NULL,
                  accept = c("image/*"),
                  capture = "environment"
                )
                
            ),
            
            
            # ✅ Aperçu photo (miniature + nom fichier)
            uiOutput("comm_photo_preview"),
            
            actionButton("comm_publier", "Publier", class = "btn btn-success w-100 mt-2")
        ),
        
        # Fil (placeholder pour l’instant)
        div(class = "px-3 pb-4",
            selectInput(
              "comm_sort",
              NULL,
              choices = c("Plus récents d'abord" = "new", "Plus anciens d'abord" = "old"),
              selected = "new",
              width = "100%"
            ),
            uiOutput("comm_feed")
        ),
        
      )  
      
      
    } else if (page == "A propos") {
      tagList(
        div(class="page-topbar",
            actionButton("back_generic", label = NULL, icon = icon("arrow-left"), class = "back-arrow-btn"),
            h3("A propos", class="page-title")
        ),
        div(class="p-3",
            h5("Description", class="fw-bold"),
            actionButton(
              "btn_lire_description",
              "Lire la description",
              class = "btn w-100",
              style = "background:#4a7c59; border:none; color:white; font-weight:600; border-radius:8px;"
            ),
            h5("Notre équipe", class="fw-bold mt-3"),
            p(tags$b("Céline THIOMBIANO & Elisée DAGBO"), " – Étudiantes en Master 1 ECAP."),
            h5("Nous contacter", class="fw-bold mt-3"),
            tags$a("thiombianoceline123@gmail.com", href="mailto:thiombianoceline123@gmail.com"),
            br(),
            tags$a("dagboelisee@gmail.com", href="mailto:dagboelisee@gmail.com")
        ),
        
        div(class = "mt-4 text-center",
            
            h5("Notez nous", class = "fw-bold mt-4 text-center"),
            
            div(
              style = "display:flex; justify-content:center; gap:14px; align-items:center;",
              actionButton("rate_1", "★", class = "btn p-0 border-0 bg-transparent", style = "font-size:30px;"),
              actionButton("rate_2", "★", class = "btn p-0 border-0 bg-transparent", style = "font-size:30px;"),
              actionButton("rate_3", "★", class = "btn p-0 border-0 bg-transparent", style = "font-size:30px;"),
              actionButton("rate_4", "★", class = "btn p-0 border-0 bg-transparent", style = "font-size:30px;"),
              actionButton("rate_5", "★", class = "btn p-0 border-0 bg-transparent", style = "font-size:30px;")
            ),
            
            div(class = "text-center mt-2", uiOutput("rating_text"))
        )
      )
    } else {
      div(class = "p-4 text-center",
          h3(tools::toTitleCase(page)),
          p("Section en construction..."),
          actionButton("back_generic", "← Retour Accueil", class = "btn btn-success mt-3 w-100")
      )
    }
  })
  
  # ----------------------------
  # Historique : rendu horizontal
  # ----------------------------
  output$recent_views <- renderUI({
    req(is_logged_in())
    h <- history()
    if (nrow(h) == 0) {
      return(
        div(class = "text-center py-2",
            icon("clock", class = "fa-2x text-muted mb-2"),
            div(class = "text-muted", "Aucune consultation pour le moment.")
        )
      )
    }
    div(class = "hscroll",
        lapply(seq_len(nrow(h)), function(i) {
          src_img <- if (!is.na(h$thumb_url[i]) && nzchar(h$thumb_url[i])) {
            paste0(
              URLencode(dirname(h$thumb_url[i]), reserved = TRUE), "/",
              URLencode(basename(h$thumb_url[i]), reserved = TRUE)
            )
          } else "logo.png"
          
          onclick_js <- sprintf(
            "Shiny.setInputValue('selected_class', '%s', {priority:'event'});",
            gsub("'", "\\\\'", h$class_id[i])
          )
          
          tags$a(
            href = "#",
            onclick = onclick_js,
            style = "text-decoration:none; color: inherit;",
            div(class = "card biblio-card hcard",
                div(class = "card-body p-2 text-center",
                    tags$img(src = src_img, class = "card-img-top img-fluid w-100 biblio-img"),
                    h6(h$pathologie[i], class = "fw-bold mt-2 mb-1 text-success small"),
                    p(h$culture[i], class = "text-muted mb-0", style = "font-size:0.78rem;")
                )
            )
          )
        })
    )
  })
  
  # ----------------------------
  # Bibliothèque - contenu filtré
  # ----------------------------
  output$biblio_content <- renderUI({
    req(is_logged_in())
    x <- library_data
    
    q <- input$biblio_search
    q <- if (is.null(q)) "" else tolower(trimws(q))
    
    f <- current_filter()
    
    if (f == "cultures" && nzchar(selected_culture())) {
      x <- x %>% filter(`Nom de la culture` == selected_culture())
    } else if (f == "pathologies" && nzchar(selected_pathologie())) {
      x <- x %>% filter(`Nom de la pathologie` == selected_pathologie())
    }
    
    if (nzchar(q)) {
      x <- x %>% filter(
        str_detect(tolower(`Nom de la culture`), fixed(q)) |
          str_detect(tolower(`Nom de la pathologie`), fixed(q))
      )
    }
    
    if (nrow(x) == 0) {
      return(
        div(class = "text-center py-5",
            icon("search", class = "fa-3x text-muted mb-3"),
            h5("Aucun résultat", class = "text-muted"),
            p("Essayez un autre mot ou changez le filtre.", class = "text-muted")
        )
      )
    }
    
    div(class = "row g-3",
        lapply(seq_len(nrow(x)), function(i) {
          src_img <- paste0(
            URLencode(dirname(x$thumb_url[i]), reserved = TRUE), "/",
            URLencode(basename(x$thumb_url[i]), reserved = TRUE)
          )
          
          column(6,
                 tags$a(
                   href = "#",
                   onclick = card_onclick(x$class_id[i]),
                   style = "text-decoration:none; color: inherit;",
                   div(class = "card biblio-card h-100",
                       div(class = "card-body p-2 text-center",
                           tags$img(src = src_img, class = "card-img-top img-fluid w-100 biblio-img"),
                           h6(x$`Nom de la pathologie`[i],
                              class = "card-title fw-bold mt-2 mb-1 text-success small"),
                           p(x$`Nom de la culture`[i],
                             class = "card-text text-muted mb-0",
                             style = "font-size: 0.8rem;")
                       )
                   )
                 )
          )
        })
    )
  })
  ##########################################################################################################################""
  # Modal fiche - NETTOYAGE DE L'HISTORIQUE RAPIDE
  observeEvent(input$selected_class, {
    req(is_logged_in(), input$selected_class)
    
    row <- library_data %>% filter(class_id == input$selected_class) %>% slice(1)
    
    # Image par défaut (Bibliothèque)
    src_img <- paste0(URLencode(dirname(row$thumb_url), TRUE), "/", URLencode(basename(row$thumb_url), TRUE))
    
    # On regarde si on vient du SUIVI (scan perso)
    item_suivi <- data.frame()
    if (!is.null(rv$historique) && nrow(rv$historique) > 0) {
      item_suivi <- rv$historique %>% filter(class_id == input$selected_class) %>% slice(1)
    }
    
    # On détecte si c'est un scan en direct
    is_scan <- !is.null(input$clicked_from_scan)
    # On détecte si c'est un clic depuis le suivi
    is_suivi <- nrow(item_suivi) > 0
    
    # LOGIQUE D'AFFICHAGE DE L'IMAGE DANS LA MODAL
    if (is_scan) {
      src_img <- rv$last_photo_src
      shinyjs::runjs("Shiny.setInputValue('clicked_from_scan', null);")
    } else if (is_suivi) {
      src_img <- item_suivi$photo_src
    }
    
    # Affichage de la Modal
    showModal(modalDialog(
      title = row$`Nom de la pathologie`,
      easyClose = TRUE,
      footer = modalButton("Fermer"),
      size = "m",
      div(
        tags$img(src = src_img, style = "width:100%; max-height:220px; object-fit:cover; border-radius:14px; margin-bottom:12px;"),
        tags$p(tags$b("Culture : "), row$`Nom de la culture`),
        tags$p(tags$b("Description : "), row$Description),
        tags$p(tags$b("Symptômes : "), row$`Symptômes`),
        tags$p(tags$b("Recommandations : "), row$`Recommandations détaillées`)
      )
    ))
    
    # ✅ RÉGLAGE HISTORIQUE RAPIDE (Accueil)
    # ON AJOUTE UNIQUEMENT si ce n'est PAS un scan ET PAS un élément du suivi
    if (!is_scan && !is_suivi) {
      # On utilise l'image standard de la bibliothèque pour l'accueil
      img_biblio <- paste0(URLencode(dirname(row$thumb_url), TRUE), "/", URLencode(basename(row$thumb_url), TRUE))
      add_to_history("consultation", row$`Nom de la culture`, row$`Nom de la pathologie`, row$class_id, img_biblio)
    }
  }, ignoreInit = TRUE)
  
  ############################################################################################################""
  # ----------------------------
  # Profil / Menu : bloqués tant que pas connecté
  # ----------------------------
  observeEvent(input$btn_profil, {
    
    u <- current_user()
    
    # ---------------------------------
    # CAS 1 : PAS CONNECTÉ
    # ---------------------------------
    if (is.null(u)) {
      
      showModal(modalDialog(
        title = "Mon Espace",
        tabsetPanel(
          tabPanel("Connexion",
                   div(class = "mt-3",
                       textInput("login_email", "Email", placeholder = "ex: moi@agrisave.com"),
                       passwordInput("login_pass", "Mot de passe"),
                       actionButton("do_login", "Se connecter", class = "btn-success w-100 mt-2")
                   )),
          tabPanel("Inscription",
                   div(class = "mt-3",
                       textInput("signin_prenom", "Prénom"),
                       textInput("signin_nom", "Nom"),
                       textInput("signin_email", "Email"),
                       passwordInput("signin_pass", "Créer un mot de passe"),
                       actionButton("do_signin", "S'inscrire", class = "btn-primary w-100 mt-2")
                   ))
        ),
        footer = modalButton("Fermer"),
        easyClose = TRUE
      ))
      
    } else {
      
      # ---------------------------------
      # CAS 2 : UTILISATEUR CONNECTÉ
      # ---------------------------------
      
      showModal(modalDialog(
        title = "Mon Profil",
        easyClose = TRUE,
        footer = NULL,
        
        div(class="text-center mb-3",
            icon("user-circle", class="fa-4x text-success mb-2"),
            h4(u$name, class="fw-bold text-success"),
            p(u$email, class="text-muted")
        ),
        
        actionButton("logout_btn", "Se déconnecter",
                     class="btn btn-danger w-100"),
        
        br(), br(),
        modalButton("Fermer")
      ))
    }
  })
  
  observeEvent(input$logout_btn, {
    current_user(NULL)
    removeModal()
    showNotification("Vous êtes déconnecté.", type = "message")
  })
  
  observeEvent(input$btn_menu, {
    if (!isTRUE(is_logged_in())) {
      current_page("login")
      return()
    }
    showModal(modalDialog(
      title = "Menu",
      div(class = "d-grid gap-2",
          actionButton("go_bibliotheque", "📚 Bibliothèque", icon = icon("book"), class = "btn-light text-start"),
          actionButton("go_carte", "🗺️ Carte", icon = icon("map"), class = "btn-light text-start"),
          actionButton("go_communaute", "👥 Communauté", icon = icon("users"), class = "btn-light text-start"),
          actionButton("go_apropos", "ℹ️ A Propos", icon = icon("info-circle"), class = "btn-light text-start")
      ),
      footer = modalButton("Fermer"), easyClose = TRUE
    ))
  })
  
  # Description (inchangé)
  observeEvent(input$btn_lire_description, {
    showModal(modalDialog(
      title = "Description",
      easyClose = TRUE,
      footer = modalButton("Fermer"),
      p("AgriSave est une application développée dans le cadre du cours de R Shiny.
      Elle a été conçu pour permettre aux agriculteurs de faire un diagnostotique rapide de leurs cultures en cas de detection d'une éventuelle pathologie et d'avoir des recommandations pour le traitement. Cela grace à l'intégration d'une fonctionnalité de prise de photo des cultures, ou à la consultation de la bibliothèque de pathologies presente dans le menu.
      Nous somme parties de la construction d'un modèle de prédiction qui permettra de reconnaitre les les pathologies des cultures une fois chargées sur l'application. Et pour ce faire, nous avons eu recours à deux base de données d'images, issues du site Kaggle, dont l'une est pour l'apprentissage et l'autre pour la validation. Nous disposons au total de 29 types de pathologies de cultures,
      avec chacune plusieurs images pour faciliter la reconnaisance, on note cependant que certaines d'entre ces cultures sont saines.")
    ))
  }, ignoreInit = TRUE)
  ##################################################################################################################################
  # --- LOGIQUE DE SUPPRESSION ---
  observeEvent(input$clear_all_hist, {
    showModal(modalDialog(
      title = "Supprimer l'historique ?",
      "Cette action effacera vos scans et vos consultations récentes. Confirmer ?",
      footer = tagList(
        modalButton("Annuler"),
        actionButton("confirm_delete", "Oui, tout effacer", class="btn-danger")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$confirm_delete, {
    # 1. Vider les variables dans l'application
    rv$historique <- tibble(date=as.POSIXct(character()), class_id=character(), photo_src=character())
    history(tibble(time=as.POSIXct(character()), source=character(), culture=character(),
                   pathologie=character(), class_id=character(), thumb_url=character()))
    
    # 2. Vider les fichiers sur le disque
    saveRDS(rv$historique, HIST_FILE)
    saveRDS(history(), ACCUEIL_FILE)
    
    removeModal()
    showNotification("Historique effacé. Au retour, tout sera vide.", type="message")
  })
  
}

shinyApp(ui, server)