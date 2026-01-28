######################### VRAI CODE QUI MARCHE ##################


### PASTEL

# app.R — DataViz Pro (v2.0) — MAJ UI : tabs univariée, corrélations dans bivariée,
# complétude dans vue d'ensemble, suppression Dashboard

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(tidyr)
library(readxl)
library(viridis)
library(DescTools)
library(e1071)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)   # optionnel (si tu veux afficher l'arbre)
library(nnet)         # pour multinom si jamais cible multiclasses
library(randomForest)



# =========================
# UI
# =========================
ui <- dashboardPage(
  skin = "purple",
  
  dashboardHeader(
    title = "StatStudio",
    titleWidth = 400
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("🏠 Accueil", tabName = "home"),
      menuItem("📁 Chargement", tabName = "upload"),
      menuItem("🔍 Vue d'ensemble", tabName = "overview"),
      menuItem("📊 Analyse Univariée", tabName = "univariate"),
      menuItem("📈 Analyse Bivariée", tabName = "bivariate"),
      menuItem("🤖 Modélisation", tabName = "modeling"),
      menuItem("📘 Étude de cas", tabName = "case_study")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
  /* Fond et couleurs générales */
  .content-wrapper, .right-side { 
    background: linear-gradient(135deg, #EAF2FF 0%, #EAF7F3 50%, #FFF2EC 100%);
    background-attachment: fixed;
  }
  
  /* Style des box */
  .box { 
    border-radius: 20px;
    box-shadow: 0 10px 28px rgba(27, 42, 65, 0.14);
    border: none;
    backdrop-filter: blur(10px);
    background: rgba(255, 255, 255, 0.94) !important;
    transition: transform 0.3s ease, box-shadow 0.3s ease;
  }
  
  .box:hover {
    transform: translateY(-5px);
    box-shadow: 0 16px 40px rgba(27, 42, 65, 0.18);
  }
  
  .box-header {
    border-radius: 20px 20px 0 0;
    background: linear-gradient(135deg, #7FB7D8 0%, #8FCFB9 60%, #F7B79D 100%) !important;
    color: white !important;
    font-weight: bold;
    padding: 20px;
  }
  
  .box-header .box-title {
    color: white !important;
    font-size: 18px;
    text-shadow: 2px 2px 4px rgba(0,0,0,0.12);
  }
  
  /* Small boxes (value boxes) */
  .small-box { 
    border-radius: 20px;
    box-shadow: 0 8px 22px rgba(27, 42, 65, 0.14);
    border: none;
    transition: transform 0.3s ease;
  }
  
  .small-box:hover {
    transform: scale(1.05);
  }
  
  .small-box > .inner {
    padding: 20px;
  }
  
  .small-box h3 {
    font-size: 42px;
    font-weight: bold;
    text-shadow: 2px 2px 4px rgba(0,0,0,0.10);
  }
  
  .small-box p {
    font-size: 16px;
    font-weight: 500;
  }
  
  /* Boutons */
  .btn-primary { 
    background: linear-gradient(135deg, #7FB7D8 0%, #8FCFB9 100%) !important;
    border: none !important;
    border-radius: 25px;
    padding: 12px 30px;
    font-weight: bold;
    box-shadow: 0 5px 15px rgba(127, 183, 216, 0.35);
    transition: all 0.3s ease;
  }
  
  .btn-primary:hover {
    transform: translateY(-2px);
    box-shadow: 0 8px 20px rgba(127, 183, 216, 0.50);
  }
  
  .btn-success { 
    background: linear-gradient(135deg, #8FCFB9 0%, #F7B79D 100%) !important;
    border: none !important;
    border-radius: 25px;
    padding: 12px 30px;
    font-weight: bold;
    box-shadow: 0 5px 15px rgba(143, 207, 185, 0.35);
  }
  
  /* Inputs et selects */
  .selectize-input, .form-control { 
    border-radius: 15px;
    border: 2px solid #DDE6F1;
    transition: all 0.3s ease;
  }
  
  .selectize-input:focus, .form-control:focus {
    border-color: #7FB7D8;
    box-shadow: 0 0 10px rgba(127, 183, 216, 0.30);
  }
  
  /* Sidebar */
  .main-sidebar {
    background: linear-gradient(180deg, #2D3A45 0%, #26323B 100%) !important;
  }
  
  .sidebar-menu > li > a {
    border-radius: 10px;
    margin: 5px 10px;
    transition: all 0.3s ease;
  }
  
  .sidebar-menu > li > a:hover {
    background: rgba(255, 255, 255, 0.10) !important;
    transform: translateX(5px);
  }
  
  .sidebar-menu > li.active > a {
    background: linear-gradient(135deg, #7FB7D8 0%, #8FCFB9 100%) !important;
    border-radius: 10px;
  }
  
  /* Header */
  .main-header .navbar {
    background: linear-gradient(135deg, #7FB7D8 0%, #8FCFB9 100%) !important;
    border: none;
  }
  
  .main-header .logo {
    background: linear-gradient(135deg, #7FB7D8 0%, #8FCFB9 100%) !important;
    font-weight: bold;
    text-shadow: 2px 2px 4px rgba(0,0,0,0.12);
  }
  
  /* Tab panels */
  .nav-tabs-custom {
    border-radius: 15px;
    box-shadow: 0 5px 15px rgba(27, 42, 65, 0.10);
  }
  
  .nav-tabs-custom > .nav-tabs > li.active > a {
    background: linear-gradient(135deg, #7FB7D8 0%, #8FCFB9 100%);
    color: white !important;
    border-radius: 10px 10px 0 0;
    border: none;
  }
  
  .nav-tabs-custom > .nav-tabs > li > a {
    border-radius: 10px 10px 0 0;
    transition: all 0.3s ease;
  }
  
  .nav-tabs-custom > .nav-tabs > li > a:hover {
    background: rgba(127, 183, 216, 0.12);
  }
  
  /* DataTables */
  .dataTables_wrapper {
    border-radius: 15px;
    overflow: hidden;
  }
  
  table.dataTable thead th {
    background: linear-gradient(135deg, #7FB7D8 0%, #8FCFB9 100%);
    color: white;
    font-weight: bold;
  }
  
  table.dataTable tbody tr:hover {
    background-color: rgba(127, 183, 216, 0.10);
  }
  
  /* Stats divs personnalisés */
  .stat-card {
    padding: 15px;
    margin: 10px 0;
    background: linear-gradient(135deg, rgba(127, 183, 216, 0.10) 0%, rgba(143, 207, 185, 0.10) 100%);
    border-radius: 15px;
    border-left: 5px solid #7FB7D8;
    transition: all 0.3s ease;
  }
  
  .stat-card:hover {
    transform: translateX(5px);
    box-shadow: 0 5px 15px rgba(27, 42, 65, 0.10);
  }
  
  /* Scrollbar personnalisée */
  ::-webkit-scrollbar { width: 10px; }
  ::-webkit-scrollbar-track {
    background: rgba(255, 255, 255, 0.15);
    border-radius: 10px;
  }
  ::-webkit-scrollbar-thumb {
    background: linear-gradient(135deg, #7FB7D8 0%, #8FCFB9 100%);
    border-radius: 10px;
  }
  ::-webkit-scrollbar-thumb:hover {
    background: linear-gradient(135deg, #8FCFB9 0%, #7FB7D8 100%);
  }
  

/* ===== FIX DROPDOWN SELECTIZE COUPÉ PAR LES BOXES ===== */

/* base : toutes les boxes ont un z-index propre */
.box { position: relative; z-index: 1; }

/* quand tu cliques dans un input/select de la box, cette box passe au-dessus des autres */
.box:focus-within { z-index: 99999 !important; }

/* dropdown selectize toujours au-dessus */
.selectize-control { position: relative; z-index: 100000 !important; }
.selectize-dropdown { z-index: 100001 !important; }

/* plotly en dessous (sinon il peut recouvrir) */
.html-widget, .plotly { position: relative; z-index: 0 !important; }

/* ===== HOME LANDING PAGE ===== */

.home-hero{
  padding: 34px 34px;
  border-radius: 26px;
  background: linear-gradient(135deg, rgba(127,183,216,.35) 0%, rgba(143,207,185,.30) 55%, rgba(247,183,157,.30) 100%);
  box-shadow: 0 16px 45px rgba(27,42,65,.14);
  border: 1px solid rgba(255,255,255,.8);
  backdrop-filter: blur(10px);
}

.home-hero h1{
  margin: 0;
  font-size: 38px;
  font-weight: 900;
  letter-spacing: -0.5px;
  color: #1b2a41;
}

.home-hero p{
  margin-top: 12px;
  font-size: 16px;
  line-height: 1.6;
  color: rgba(27,42,65,.85);
  max-width: 820px;
}

.home-cta{
  margin-top: 18px;
  display:flex;
  gap: 12px;
  flex-wrap: wrap;
}

.btn-ghost{
  background: rgba(255,255,255,.75) !important;
  border: 1px solid rgba(127,183,216,.55) !important;
  border-radius: 25px !important;
  padding: 12px 22px !important;
  font-weight: 800 !important;
  color: #1b2a41 !important;
}

.btn-ghost:hover{ transform: translateY(-2px); }

.feature-grid{
  display: grid;
  grid-template-columns: repeat(3, minmax(0, 1fr));
  gap: 16px;
}

@media(max-width: 992px){
  .feature-grid{ grid-template-columns: 1fr; }
}

.feature-card{
  padding: 18px;
  border-radius: 22px;
  background: rgba(255,255,255,.92);
  border: 1px solid rgba(221,230,241,.9);
  box-shadow: 0 10px 28px rgba(27,42,65,.10);
  transition: transform .25s ease, box-shadow .25s ease;
  min-height: 150px;
}

.feature-card:hover{
  transform: translateY(-4px);
  box-shadow: 0 16px 40px rgba(27,42,65,.14);
}

.feature-title{
  font-weight: 900;
  font-size: 16px;
  color: #1b2a41;
  margin-bottom: 6px;
}

.feature-desc{
  font-size: 14px;
  color: rgba(27,42,65,.78);
  line-height: 1.55;
}

.section-title{
  font-size: 20px;
  font-weight: 900;
  color: #1b2a41;
  margin: 6px 0 14px;
}

.section-sub{
  color: rgba(27,42,65,.70);
  margin-top: -6px;
  margin-bottom: 16px;
}

.quick-steps{
  display:grid;
  grid-template-columns: repeat(3, minmax(0, 1fr));
  gap: 14px;
}

@media(max-width: 992px){
  .quick-steps{ grid-template-columns: 1fr; }
}

.step-card{
  padding: 16px;
  border-radius: 22px;
  background: rgba(255,255,255,.88);
  border: 1px solid rgba(221,230,241,.9);
}

.step-num{
  width: 34px; height: 34px;
  border-radius: 50%;
  display:flex; align-items:center; justify-content:center;
  font-weight: 900;
  background: linear-gradient(135deg, #7FB7D8 0%, #8FCFB9 100%);
  color: white;
  margin-bottom: 10px;
}

.home-footer{
  padding: 14px 8px;
  color: rgba(27,42,65,.55);
  font-size: 12px;
  text-align: center;
}


"))),
    
    
    
    tabItems(
      # -------------------
      # Upload
      # -------------------
      tabItem(
        tabName = "upload",
        fluidRow(
          box(
            title = "📂 Importation des données",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            fileInput(
              "file", "Choisissez un fichier",
              accept = c(".csv", ".txt", ".data", ".dat",".xlsx", ".xls", ".rds"),
              placeholder = "Formats: CSV, TXT, DATA, XLSX, XLS, RDS"
            ),
            
            conditionalPanel(
              condition = "input.file",
              hr(),
              h4("📄 Options de lecture (fichiers texte)"),
              fluidRow(
                column(3, checkboxInput("txt_header", "Header", TRUE)),
                column(3, selectInput(
                  "txt_sep", "Séparateur",
                  choices = c("Auto"="auto", ","=",", ";"=";", "Tab"="\t", " "=" ", "|"="|"),
                  selected = "auto"
                )),
                column(3, selectInput("txt_dec", "Décimal", choices = c("."=".", ","=","), selected = ".")),
                column(3, selectInput(
                  "txt_encoding", "Encodage",
                  choices = c("UTF-8"="UTF-8", "Latin1"="latin1", "Auto"="auto"),
                  selected = "UTF-8"
                ))
              )
            ),
            
            conditionalPanel(
              condition = "output.fileUploaded",
              hr(),
              h4("⚙️ Options de prétraitement"),
              fluidRow(
                column(4,
                       checkboxInput("remove_na", "Supprimer les valeurs manquantes", FALSE),
                       checkboxInput("normalize", "Normaliser les variables numériques", FALSE)
                ),
                column(4,
                       checkboxInput("remove_outliers", "Traiter les outliers (Z-score > 3)", FALSE),
                       checkboxInput("dummy", "Créer des variables dummy", FALSE)
                ),
                column(4,
                       numericInput("na_threshold", "Seuil de NA (%) pour suppression colonne", value = 50, min = 0, max = 100, step = 5)
                )
              ),
              actionButton("process", "✨ Appliquer les transformations", class = "btn-primary btn-lg", style = "margin-top: 15px;")
            )
          )
        ),
        
        fluidRow(
          conditionalPanel(
            condition = "output.fileUploaded",
            valueBoxOutput("nrows", width = 3),
            valueBoxOutput("ncols", width = 3),
            valueBoxOutput("nmissing", width = 3),
            valueBoxOutput("noutliers", width = 3)
          )
        )
      ),
      
      tabItem(
        tabName = "modeling",
        
        fluidRow(
          box(
            title = "🎯 Configuration du problème",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            fluidRow(
              column(4,
                     selectInput("mdl_target", "Variable cible (classification) :", choices = NULL, width = "100%"),
                     helpText("Astuce: choisis une variable qualitative (factor/character).")
              ),
              column(4,
                     uiOutput("mdl_features_ui")
              ),
              column(
                4,
                sliderInput("mdl_split", "Train (%)", min = 50, max = 90, value = 80, step = 5),
                numericInput("mdl_seed", "Seed", value = 42, min = 1),
                
                selectInput(
                  "mdl_sampling",
                  "Gestion du déséquilibre (train) :",
                  choices = c(
                    "Aucune" = "none",
                    "Upsampling" = "up",
                    "Downsampling" = "down",
                    "SMOTE" = "smote"
                  ),
                  selected = "none"
                )
              )
            ),
            
            hr(),
            
            fluidRow(
              column(4,
                     checkboxInput("mdl_center_scale", "Centrer / Réduire (numériques)", FALSE),
                     checkboxInput("mdl_dummy", "Dummification (catégorielles)", FALSE)
              ),
              column(4,
                     selectInput("mdl_metric", "Métrique d'optimisation (CV) :",
                                 choices = c("Accuracy"="Accuracy", "Kappa"="Kappa"),
                                 selected = "Accuracy"),
                     checkboxGroupInput(
                       "mdl_models",
                       "Modèles à comparer (min 2) :",
                       choices = c(
                         "Logistique / Multinom" = "glm",
                         "Arbre (CART)" = "rpart",
                         "Random Forest" = "rf"
                       ),
                       selected = c("glm", "rpart")
                     ),
                     selectInput(
                       "mdl_cv_method",
                       "Validation croisée :",
                       choices = c("cv", "repeatedcv", "boot632"),
                       selected = "repeatedcv"
                     ),
                     numericInput("mdl_repeats", "Répétitions (si repeatedcv)", value = 3, min = 1, max = 10),
                     numericInput("mdl_tuneLength", "tuneLength (si applicable)", value = 10, min = 1, max = 50)
              ),
              column(4,
                     numericInput("mdl_folds", "CV folds", value = 5, min = 2, max = 10),
                     actionButton("mdl_train", "🚀 Entraîner & comparer", class = "btn-primary btn-lg", style = "width:100%; margin-top: 22px;")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "📊 Résultats comparatifs",
            width = 6,
            status = "info",
            solidHeader = TRUE,
            DTOutput("mdl_metrics_table"),
            tags$hr(),
            uiOutput("mdl_notes")
          ),
          box(
            title = "🧩 Matrices de confusion",
            width = 6,
            status = "warning",
            solidHeader = TRUE,
            uiOutput("mdl_cm_ui"))
        ),
        
        fluidRow(
          box(
            title = "📈 ROC / AUC (si binaire)",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("mdl_roc_plot", height = "450px")
          )
        ),
        
        fluidRow(
          box(
            title = "⭐ Feature importance (Top 20)",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            selectInput("mdl_imp_model", "Importance à afficher :", choices = NULL),
            plotlyOutput("mdl_imp_plot", height = "500px")
          )
        )
      ),
      
      tabItem(
        tabName = "case_study",
        
        ## ===============================
        ## PROBLÉMATIQUE = le texte en titre
        ## ===============================
        fluidRow(
          box(
            width = 12,
            status = "info",
            solidHeader = TRUE,
            
            h2(
              "Peut-on prédire le risque de dépression chez un étudiant à partir de ses caractéristiques personnelles, académiques et contextuelles ?",
              style = "font-size:26px; font-weight:900; text-align:center; line-height:1.25;"
            ),
            h4("Le jeu de données utilisé dans cette étude est un jeu de données issu de la plateforme Kaggle. 
               Il comprend plusieurs centaines d’observations et un ensemble de variables décrivant les caractéristiques personnelles, 
               académiques et contextuelles des étudiants. Ces variables portent notamment sur le genre, la pression académique, le stress financier, 
               les habitudes de sommeil, la satisfaction vis-à-vis des études et l’historique familial de troubles mentaux. 
               L’objectif est d’exploiter ces informations afin d’analyser et de prédire la présence ou non de symptômes dépressifs chez les étudiants.")
          )
        ),
        
        ## ===============================
        ## TITRE
        ## ===============================
        ## ===============================
        ## DONNÉES – ANALYSE UNIVARIÉE (mini-cartes)
        ## ===============================
        fluidRow(
          box(
            width = 12,
            status = "info",
            solidHeader = TRUE,
            title = "📊 Analyse univariée : Synthèse",
            uiOutput("case_uni_keycards")
          )
        ),
        
        ## ===============================
        ## ANALYSE BIVARIÉE (ZONE TEXTE)
        ## ===============================
        fluidRow(
          box(
            title = span("📈 Analyse bivariée", style = "font-size:24px; font-weight:700;"),
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            
            h4("Dépression × Genre", style="font-weight:700;"),
            p(em("La relation entre la dépression et le genre a été analysée à l’aide d’une table de contingence et
            du V de Cramer. Les résultats mettent en évidence une association statistiquement significative entre ces 
            deux variables (p-value < 2.2e-16), indiquant que la répartition des individus dépressifs diffère selon le genre. 
            Toutefois, la valeur du V de Cramer reste modérée, suggérant que si le genre est lié à la dépression, 
            il ne constitue pas à lui seul un facteur explicatif majeur. Cette variable apparaît donc comme un facteur contextuel pertinent, 
            mais insuffisant pour expliquer à elle seule la présence de symptômes dépressifs.")),
            
            h4("Dépression × Âge", style="font-weight:700;"),
            p(em("La relation entre la dépression et l’âge, variable numérique, 
                 a été étudiée à l’aide d’un boxplot et d’un test statistique (t-test), 
                 complété par le calcul du rapport de corrélation η². Les résultats montrent une 
                 différence statistiquement significative entre les distributions d’âge des étudiants 
                 dépressifs et non dépressifs (p-value < 2.2e-16). Néanmoins, la valeur relativement 
                 faible de η² indique que l’âge n’explique qu’une faible part de la variabilité observée. 
                 Ainsi, bien que l’âge soit statistiquement associé à la dépression, son pouvoir explicatif 
                 reste limité.")),
            
            h4("Dépression × Antécédents de pensées suicidaires", style="font-weight:700;"),
            p(em("L’analyse de la relation entre la dépression et la variable “Have you ever had suicidal thoughts” 
            met en évidence une association forte, confirmée par un V de Cramer élevé et une p-value très significative. 
            Les individus ayant déjà déclaré des pensées suicidaires sont proportionnellement beaucoup plus 
            nombreux parmi les étudiants dépressifs. Cette variable apparaît comme l’un des facteurs les plus 
            fortement liés à la dépression dans le jeu de données,
            traduisant une cohérence clinique entre les symptômes dépressifs et les pensées suicidaires.")),
            
            h4("Dépression × Niveau de diplôme (Degree)", style="font-weight:700;"),
            p(em("L’analyse montre que la dépression n’est pas répartie uniformément selon le niveau de diplôme. Certaines catégories (notamment les niveaux intermédiaires ou en transition académique) présentent des effectifs plus élevés d’étudiants dépressifs.
            Néanmoins, le V de Cramer est faible (~0.14), ce qui indique un effet limité malgré une significativité statistique (p-value < 2.2e-16). Le niveau de diplôme semble davantage refléter un contexte académique qu’un facteur explicatif direct de la dépression.")),
            
            h4("Dépression × Stress financier (Financial Stress)", style="font-weight:700;"),
            p(em("Le stress financier est clairement plus élevé chez les étudiants dépressifs 
            que chez les non dépressifs. La différence observée est très significative 
            (p-value < 2.2e-16) et la valeur de η² (~0.13) est parmi les plus élevées pour 
            les variables numériques.Cela indique que le stress financier constitue un facteur 
            explicatif important, plus discriminant que l’âge ou la satisfaction académique. 
            Cette variable joue un rôle central dans la compréhension du risque de dépression chez 
            les étudiants.")),
            
            h4("Dépression × Durée de sommeil", style="font-weight:700;"),
            p(em("Les étudiants dormant moins de 5 heures par nuit sont clairement sur-représentés dans le groupe dépressif, tandis que les durées de sommeil plus longues (7–8 heures et plus) sont davantage associées à l’absence de dépression.
            La relation est statistiquement significative (p-value < 2.2e-16), 
                 mais le V de Cramer reste faible (~0.10), indiquant un effet modéré. 
                 La durée de sommeil apparaît comme un facteur aggravant, pertinent 
                 lorsqu’il est combiné à d’autres variables (stress, satisfaction, pression académique).")),
            
            h4("Dépression × Antécédents de pensées suicidaires", style="font-weight:700;"),
            p(em("L’analyse de la relation entre la dépression et la variable “Have you ever had suicidal thoughts” 
            met en évidence une association forte, confirmée par un V de Cramer élevé et une p-value très significative. 
            Les individus ayant déjà déclaré des pensées suicidaires sont proportionnellement beaucoup plus 
            nombreux parmi les étudiants dépressifs. Cette variable apparaît comme l’un des facteurs les plus 
            fortement liés à la dépression dans le jeu de données,
            traduisant une cohérence clinique entre les symptômes dépressifs et les pensées suicidaires."))
          )
        ),
        
        ## ===============================
        ## CHOIX MÉTHODOLOGIQUES (tabBox)
        ## ===============================
        fluidRow(
          box(
            title = span("🧩 Choix méthodologiques", style = "font-size:24px; font-weight:700;"),
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            tabBox(
              width = 12,
              
              tabPanel("Traitement des données",
                       h4("Nettoyage et préparation", style="font-weight:700;"),
                       p(em("Dans l’application, plusieurs options de prétraitement ont 
                            été mises à disposition afin de s’adapter à la qualité et à 
                            la structure des jeux de données importés, tout en laissant à l’utilisateur la maîtrise des choix méthodologiques.")),
                       tags$ul(
                         tags$li(strong("Suppression des valeurs manquantes"), " : ", em("Dans le cadre de cette étude, le jeu de données initial comportait un nombre suffisant d’observations, ce qui a permis de supprimer les lignes incomplètes sans perte significative d’information. Nous avons donc privilégié ce choix méthodologique afin de travailler sur des données complètes et cohérentes. La suppression des valeurs manquantes a ainsi permis d’éviter l’introduction de biais ou d’erreurs liés à une imputation approximative.")),
                         tags$li(strong("Traitement des valeurs aberrantes (outliers)"), " : ", em("L’application intègre une fonctionnalité de détection et de traitement des valeurs aberrantes basée sur la méthode du Z-score. Les observations présentant une valeur absolue du Z-score supérieure à un seuil prédéfini (|Z| > 3) sont considérées comme atypiques et peuvent être exclues du jeu de données. Cette option a été intégrée sur nos données afin de limiter l’influence d’observations extrêmes susceptibles de fausser les analyses statistiques et les performances des modèles, en particulier pour les variables numériques.")),
                         tags$li(strong("Normalisation et dummification"), " : ", em("L’application propose des options de normalisation et de dummification afin de s’adapter à des jeux de données variés. Dans le cadre de cette étude, ces transformations n’ont pas été appliquées, car les variables étaient majoritairement catégorielles et présentaient des modalités suffisantes pour être exploitées directement par les modèles retenus, sans nécessiter de traitement supplémentaire.")),
                       )
              ),
              
              tabPanel("Suppression / sélection de variables",
                       h4("Choix des variables", style="font-weight:700;"),
                       tags$ul(
                         tags$li(em("Certaines variables ont été supprimées en amont de la modélisation afin d’éviter d’introduire des informations non pertinentes ou non exploitables. La variable identifiant (ID) a été exclue, car elle n’apporte aucune information explicative et ne peut contribuer à la prédiction.
                                    La variable profession a également été supprimée : l’ensemble des individus du jeu de données étant des étudiants, cette variable ne présente aucune variabilité et n’est donc pas discriminante.")),
                         tags$li(em("Par ailleurs, deux variables liées à la vie professionnelle ont été retirées, celles-ci ne contenant que des valeurs manquantes. Ce résultat est cohérent avec la nature de la population étudiée, exclusivement composée d’étudiants, pour lesquels ces informations ne sont pas renseignées.")),
                         tags$li(em("L’ensemble des autres variables a été conservé. Leur pertinence a été validée à l’issue de l’analyse bivariée, qui a mis en évidence des relations statistiques significatives ou informatives avec la variable cible. Ces variables ont donc été jugées importantes pour la compréhension du phénomène étudié et pour l’entraînement des modèles de classification.")),
                       )
              ),
              
              tabPanel("Choix des modèles",
                       h4("Modèles testés", style="font-weight:700;"),
                       tags$ul(
                         tags$li(strong("Régression logistique"), " : ", em("La régression logistique a été retenue car elle est particulièrement adaptée à la nature de notre jeu de données, dont la variable cible est binaire. Elle permet de modéliser simplement la relation entre la présence de symptômes dépressifs et des variables explicatives hétérogènes, à la fois numériques et catégorielles. Dans l’application, ce modèle a été intégré afin de fournir une base de référence robuste, facilement applicable à des jeux de données de petite ou moyenne taille. La mise à disposition de la régression logistique dans l’outil permet ainsi à l’utilisateur d’obtenir rapidement un premier niveau d’analyse,très adapté à un jeu de donnée binaire.")),
                         tags$li(strong("Arbre de décision (CART)"), " : ", em("L’arbre de décision a été choisi en raison de sa capacité à gérer naturellement des variables catégorielles et à capturer des relations non linéaires, ce qui correspond bien aux caractéristiques de notre jeu de données. Plusieurs variables décrivent des niveaux ou des catégories (stress, sommeil, satisfaction académique), pour lesquelles ce type de modèle est particulièrement pertinent. Son intégration dans l’application répond également à un objectif de lisibilité et de pédagogie : les règles de décision issues de l’arbre permettent de comprendre facilement comment différentes combinaisons de facteurs conduisent à un risque plus ou moins élevé. Ce modèle a donc été mis à disposition afin de traiter des jeux de données mixtes.")),
                         tags$li(strong("Random Forest"), " : ", em("Le Random Forest est un modèle de classification capable de traiter aussi bien des problèmes binaires que multiclasse. Il a été intégré dans l’application afin de pouvoir analyser des jeux de données plus complexes, comportant un nombre important de variables explicatives et des relations potentiellement non linéaires entre celles-ci. Ce modèle repose sur un ensemble d’arbres de décision construits à partir de sous-échantillons aléatoires des données et des variables, ce qui permet de réduire le risque de sur-apprentissage et d’améliorer la capacité de généralisation. Dans le cadre de l’application, le Random Forest offre ainsi une méthode de classification robuste et performante, particulièrement adaptée aux structures de données hétérogènes."))
                       )
              ),
            )
          )
        ),
        
        ## ===============================
        ## MODÈLES (comme avant)
        ## ===============================
        fluidRow(
          column(
            6,
            box(
              title = "📊 Modèle 1 – Régression logistique",
              width = 12,
              status = "success",
              solidHeader = TRUE,
              p(em("La régression logistique présente les meilleures performances globales et constitue le modèle de référence de l’étude. Elle affiche une accuracy d’environ 84 % et une excellente capacité de discrimination (AUC supérieure à 0,9), indiquant une séparation très efficace entre étudiants dépressifs et non dépressifs, avec une performance globalement équilibrée entre les classes. L’analyse des coefficients met en évidence des variables fortement associées au risque de dépression, en particulier la pression académique, la présence de pensées suicidaires et le stress financier, qui augmentent significativement la probabilité d’être classé comme dépressif. À l’inverse, les variables liées au mode de vie, telles que le sommeil ou les habitudes alimentaires, ainsi que la satisfaction des études, jouent un rôle plus modéré. Grâce à sa bonne interprétabilité et à ses performances élevées, ce modèle apparaît comme particulièrement pertinent pour l’analyse et la prise de décision."))
            )
          ),
          column(
            6,
            box(
              title = "🌳 Modèle 2 – Arbre de décision (CART)",
              width = 12,
              status = "success",
              solidHeader = TRUE,
              p(em("L’arbre de décision obtient des performances légèrement inférieures à la régression logistique, avec une accuracy d’environ 83 % et une AUC proche de 0,86, traduisant une capacité de discrimination correcte mais moins fine. En revanche, ce modèle apporte une forte valeur explicative. L’analyse de l’importance des variables montre que la pression académique constitue le facteur le plus discriminant, suivie des pensées suicidaires et du stress financier, confirmant leur rôle central dans la prédiction de la dépression. Des variables telles que l’âge et certaines habitudes de vie interviennent de manière secondaire, tandis que d’autres facteurs ont un impact marginal."))
            )
          )
        ),
        
        ## ===============================
        ## COMPARAISON
        ## ===============================
        fluidRow(
          box(
            title = span("⚖️ Comparaison des modèles", style = "font-size:24px; font-weight:700;"),
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            p(em("La comparaison des modèles met en évidence une cohérence forte des résultats, tant en termes de performances que d’identification des variables clés. La régression logistique se distingue par la meilleure performance prédictive (accuracy et AUC les plus élevées), tandis que l’arbre de décision apporte une lecture intuitive des facteurs de risque. Dans les deux cas, la pression académique, les pensées suicidaires et le stress financier ressortent systématiquement comme les variables les plus déterminantes."))
          )
        )
      ),
      
      
      
      # -------------------
      # Overview (version originale + ajout Complétude)
      # -------------------
      tabItem(
        tabName = "overview",
        
        fluidRow(
          box(title = "📋 Aperçu des données", width = 12, status = "info", solidHeader = TRUE,
              DTOutput("data_preview"))
        ),
        
        fluidRow(
          box(title = "📊 Structure des données", width = 6, status = "warning", solidHeader = TRUE,
              verbatimTextOutput("data_structure")),
          box(title = "📈 Statistiques descriptives", width = 6, status = "success", solidHeader = TRUE,
              DTOutput("summary_stats"))
        ),
        
        fluidRow(
          box(title = "🎯 Types de variables", width = 6, status = "primary", solidHeader = TRUE,
              plotlyOutput("var_types_plot", height = "300px")),
          box(title = "❓ Valeurs manquantes par variable", width = 6, status = "danger", solidHeader = TRUE,
              plotlyOutput("missing_plot", height = "300px"))
        ),
        
        # ✅ Ajout demandé : uniquement le graphique Complétude
        fluidRow(
          box(title = "❓ Complétude", width = 6, status = "info", solidHeader = TRUE,
              plotlyOutput("dash_completeness", height = "300px")),
          box(
            title = "⚖️ Déséquilibre des classes",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            fluidRow(
              column(4, selectInput("target_var", "Variable cible (qualitative) :", choices = NULL, width = "100%")),
              column(8, uiOutput("class_balance_msg"))
            ),
            plotlyOutput("class_balance_plot", height = "300px")
          )
        )
        
      ),
      
      
      # -------------------
      # Univariate (grand graphique + onglets)
      # -------------------
      tabItem(
        tabName = "univariate",
        
        fluidRow(
          box(
            title = "🎛️ Paramètres (Univariée)", width = 4, status = "primary", solidHeader = TRUE,
            selectInput("uni_var", "Variable :", choices = NULL, width = "100%"),
            checkboxInput("show_percent", "Afficher pourcentages (catégorielle)", TRUE)
          ),
          
          box(
            title = "📊 Univariée — vues complémentaires", width = 8, status = "info", solidHeader = TRUE,
            uiOutput("uni_tabs_ui")
          )
        ),
        
        fluidRow(
          box(title = "📐 Statistiques", width = 12, status = "success", solidHeader = TRUE,
              uiOutput("uni_stats"))
        )
      ),
      
      # -------------------
      # Bivariate (+ corrélations en dessous)
      # -------------------
      tabItem(
        tabName = "bivariate",
        fluidRow(
          box(
            title = "🎛️ Sélection", width = 12, status = "primary", solidHeader = TRUE,
            fluidRow(
              column(4, selectInput("bi_var1", "Variable 1:", choices = NULL, width = "100%")),
              column(4, selectInput("bi_var2", "Variable 2:", choices = NULL, width = "100%")),
              column(4,
                     conditionalPanel(
                       condition = "output.both_numeric",
                       checkboxInput("show_regression", "Régression", TRUE),
                       checkboxInput("show_r2", "R²", TRUE)
                     ),
                     conditionalPanel(
                       condition = "output.one_cat_one_num",
                       selectInput("bi_plot_type", "Type:",
                                   choices = c("Boxplot"="box", "Violin"="violin"),
                                   selected = "box"
                       )
                     )
              )
            )
          )
        ),
        
        fluidRow(
          box(title = "📊 Visualisation", width = 8, status = "info", solidHeader = TRUE,
              plotlyOutput("bi_plot", height = "500px")),
          box(title = "📏 Mesures & Tests", width = 4, status = "warning", solidHeader = TRUE,
              uiOutput("bi_stats"))
        ),
        
        # ✅ Corrélations déplacées ici (en bas)
        fluidRow(
          box(title = "🔗 Corrélations (Numériques)", width = 6, status = "primary", solidHeader = TRUE,
              plotlyOutput("corr_plot", height = "600px")),
          box(title = "🎨 V de Cramer (Catégorielles)", width = 6, status = "info", solidHeader = TRUE,
              plotlyOutput("cramer_plot", height = "600px"))
        )
        
      ),
      
      # -------------------
      # About
      # -------------------
      tabItem(
        tabName = "home",
        
        fluidRow(
          column(
            12,
            div(
              class = "home-hero",
              tags$span(style="font-weight:900; color: rgba(27,42,65,.75);", "StatStudio • EDA + Modélisation"),
              tags$h1("Analyse, visualise et entraîne des modèles : en quelques clics."),
              tags$p("StatStudio est une application Shiny orientée exploration des données (univariée/bivariée), contrôle qualité (NA, complétude, outliers, déséquilibre) et modélisation (CV, comparaison multi-modèles, ROC/AUC, importance des variables)."),
              
              div(
                class = "home-cta",
                actionButton("go_upload", "📁 Importer un dataset", class="btn btn-primary btn-lg"),
                actionButton("go_overview", "🔍 Vue d’ensemble", class="btn btn-ghost btn-lg"),
                actionButton("go_modeling", "🤖 Lancer la modélisation", class="btn btn-ghost btn-lg")
              )
            )
          )
        ),
        
        tags$br(),
        
        fluidRow(
          box(
            width = 12, status = "info", solidHeader = TRUE,
            title = "✨ Ce que tu peux faire",
            div(
              class = "feature-grid",
              div(class="feature-card",
                  div(class="feature-title", "Qualité & préparation"),
                  div(class="feature-desc", "Gestion des NA (seuil), suppression lignes, outliers (Z-score), normalisation, dummification, typage intelligent (num/factor).")
              ),
              div(class="feature-card",
                  div(class="feature-title", "Univariée riche"),
                  div(class="feature-desc", "Histogramme/barplot, boxplot, Q-Q plot, densité, stats descriptives, Shapiro-Wilk et messages interprétables.")
              ),
              div(class="feature-card",
                  div(class="feature-title", "Bivariée + métriques"),
                  div(class="feature-desc", "Heatmap cat×cat, box/violin cat×num, scatter num×num + régression, + Pearson, η², V de Cramer.")
              ),
              div(class="feature-card",
                  div(class="feature-title", "Comparaison de modèles"),
                  div(class="feature-desc", "Train/test split, CV repeated, sampling (up/down/SMOTE), logistique/CART/RF, tableau comparatif et matrices de confusion.")
              ),
              div(class="feature-card",
                  div(class="feature-title", "ROC/AUC + importance"),
                  div(class="feature-desc", "Courbes ROC/AUC en binaire, affichage des probabilités et feature importance Top 20 (si disponible).")
              ),
              div(class="feature-card",
                  div(class="feature-title", "Étude de cas"),
                  div(class="feature-desc", "Une page narrative : problématique, synthèses univariées/bivariées, choix méthodo, interprétation & comparaison.")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            title = "🚀 Démarrage rapide",
            tags$div(class="section-sub", "Le parcours recommandé pour exploiter l’application proprement :"),
            div(
              class="quick-steps",
              div(class="step-card",
                  div(class="step-num", "1"),
                  tags$b("Importer"),
                  tags$p(style="margin:6px 0 0;", "Va dans “Chargement”, importe ton fichier et applique si besoin le prétraitement.")
              ),
              div(class="step-card",
                  div(class="step-num", "2"),
                  tags$b("Comprendre"),
                  tags$p(style="margin:6px 0 0;", "“Vue d’ensemble” puis “Univariée/Bivariée” pour identifier les variables importantes et les signaux.")
              ),
              div(class="step-card",
                  div(class="step-num", "3"),
                  tags$b("Prédire"),
                  tags$p(style="margin:6px 0 0;", "“Modélisation” : split, CV, comparaison de modèles, ROC/AUC et importance des variables.")
              )
            )
          )
        ),
        
        div(class="home-footer", "Développé par Zara Zulfiqar & Rhofra Adahchour • StatStudio")
      )
    )
  )
)

# =========================
# SERVER
# =========================
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    raw_data = NULL,
    processed_data = NULL,
    numeric_vars = NULL,
    categorical_vars = NULL,
    read_meta = list(sep = NA, header = NA, encoding = NA, ext = NA, ncol = NA)
  )
  
  current_data <- reactive({ rv$processed_data })
  
  observeEvent(input$go_upload,   { updateTabItems(session, "sidebar", "upload") })
  observeEvent(input$go_overview, { updateTabItems(session, "sidebar", "overview") })
  observeEvent(input$go_modeling, { updateTabItems(session, "sidebar", "modeling") })
  
  
  output$case_uni_keycards <- renderUI({
    
    fluidRow(
      column(3,
             div(class = "uni-card",
                 h4("🎯 Variable cible"),
                 p("Variable binaire indiquant la présence ou l’absence de dépression, on observe 40% d'élèves ayant répondu qu'il n'ont pas de dépression, et 60% ayant répondu positivement.")
             )
      ),
      column(3,
             div(class = "uni-card",
                 h4("🎂 Âge"),
                 p("Distribution adultescentrée sur les jeunes, compris entre 18 et 35 ans, avec majoritairement des étudiants de 20 ans, 22ans et 28 ans")
             )
      ),
      column(3,
             div(class = "uni-card",
                 h4("🎓 Genre"),
                 p("Répartition relativement équilibrée entre les catégories. On retrouve 40% de femmes et 60% d'hommes dans le dataset.")
             )
      ),
      column(3,
             div(class = "uni-card",
                 h4("💰 Stress financier"),
                 p("Niveaux de stress compris entre une échelle de 1 à 5. On voit que la classe modale est le 5 avec la majorité des élèves ayant répondu positivement à un niveau de stress financier elevé.")
             )
      ),
      column(3,
             div(class = "uni-card",
                 h4("🎓 Satisfaction académique"),
                 p("La satisfaction académique présente une distribution contrastée, avec une part non négligeable d’individus peu satisfaits. Cela suggère un lien potentiel avec le bien-être psychologique.")
             )
      ),
      
    )
  })
  
  output$class_balance_msg <- renderUI({
    req(current_data(), input$target_var)
    data <- current_data()
    var <- input$target_var
    validate(need(var %in% names(data), "Variable cible invalide."))
    
    x <- data[[var]]
    if (!(is.factor(x) || is.character(x))) {
      return(tags$p("Choisis plutôt une variable qualitative (factor/character)."))
    }
    
    tab <- sort(table(x, useNA = "ifany"), decreasing = TRUE)
    if (sum(tab) == 0) return(tags$p("Variable vide."))
    
    major_pct <- round(100 * (max(tab) / sum(tab)), 1)
    msg <- if (major_pct >= 70) {
      paste0("⚠️ Déséquilibre détecté : classe majoritaire = ", major_pct, "%")
    } else {
      paste0("✅ Répartition plutôt équilibrée : classe majoritaire = ", major_pct, "%")
    }
    
    tags$div(
      style = "padding:10px; background: rgba(255,255,255,0.65); border-radius: 12px;",
      tags$strong(msg)
    )
  })
  
  output$class_balance_plot <- renderPlotly({
    req(current_data(), input$target_var)
    data <- current_data()
    var <- input$target_var
    validate(need(var %in% names(data), "Variable cible invalide."))
    
    x <- data[[var]]
    validate(need(is.factor(x) || is.character(x), "Variable cible non qualitative."))
    
    df <- as.data.frame(table(x, useNA = "ifany"))
    colnames(df) <- c("Classe", "Effectif")
    
    plot_ly(
      df,
      x = ~reorder(Classe, Effectif),
      y = ~Effectif,
      type = "bar",
      marker = list(color = "#605ca8")
    ) %>% layout(xaxis = list(title = var), yaxis = list(title = "Effectif"))
  })
  
  
  output$fileUploaded <- reactive({ !is.null(rv$raw_data) })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  # -------- Helpers (anti subscript out of bounds)
  ensure_valid_selection <- function(id, choices) {
    if (length(choices) == 0) return()
    current <- input[[id]]
    if (is.null(current) || !(current %in% choices)) {
      updateSelectInput(session, id, choices = choices, selected = choices[1])
    } else {
      updateSelectInput(session, id, choices = choices, selected = current)
    }
  }
  
  # --- helper: essaie plusieurs séparateurs et choisit celui qui "fait le plus de colonnes"
  detect_best_sep <- function(path, header = TRUE) {
    seps <- list(comma = ",", semi = ";", tab = "\t", pipe = "|", space = "")
    best <- list(sep = ",", score = -Inf, ncol = 1)
    
    for (s in seps) {
      cf <- try(count.fields(path, sep = s, quote = "\""), silent = TRUE)
      if (inherits(cf, "try-error")) next
      ncol_est <- suppressWarnings(max(cf, na.rm = TRUE))
      if (!is.finite(ncol_est)) next
      
      # score favorise + de colonnes et penalise l'instabilité
      score <- ncol_est - suppressWarnings(sd(cf, na.rm = TRUE))
      if (is.na(score)) score <- ncol_est
      
      if (score > best$score) best <- list(sep = s, score = score, ncol = ncol_est)
    }
    
    best$sep
  }
  
  # --- helper: nettoyage + typage "intelligent"
  smart_type <- function(df) {
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    
    # valeurs manquantes fréquentes UCI
    df[df == "?" | df == "NA" | df == "N/A" | df == ""] <- NA
    
    for (nm in names(df)) {
      x <- df[[nm]]
      
      # si déjà numeric/factor, on laisse
      if (is.numeric(x) || is.factor(x)) next
      
      # si character : tentative de conversion numeric (si 90% convertibles)
      if (is.character(x)) {
        x_trim <- trimws(x)
        # remplace virgule décimale par point si besoin
        x_num <- suppressWarnings(as.numeric(gsub(",", ".", x_trim)))
        
        ok_rate <- mean(!is.na(x_num) | is.na(x_trim), na.rm = TRUE)
        
        if (is.finite(ok_rate) && ok_rate >= 0.90) {
          df[[nm]] <- x_num
        } else {
          # si peu de modalités -> factor
          n_unique <- length(unique(x_trim[!is.na(x_trim)]))
          if (n_unique > 0 && n_unique <= 30) {
            df[[nm]] <- as.factor(x_trim)
          } else {
            df[[nm]] <- x_trim
          }
        }
      }
    }
    
    df
  }
  
  # --- helper: lecture texte robuste
  read_text_robust <- function(path, header, sep_choice, dec, enc) {
    sep <- sep_choice
    if (sep == "auto") sep <- detect_best_sep(path, header = header)
    # option "space" UI correspond à sep = "" (whitespace)
    if (identical(sep, " ")) sep <- ""
    
    df <- tryCatch(
      read.table(
        path,
        header = header,
        sep = sep,
        dec = dec,
        stringsAsFactors = FALSE,
        fill = TRUE,
        quote = "\"",
        comment.char = "",
        fileEncoding = enc
      ),
      error = function(e) {
        # fallback encoding latin1
        read.table(
          path,
          header = header,
          sep = sep,
          dec = dec,
          stringsAsFactors = FALSE,
          fill = TRUE,
          quote = "\"",
          comment.char = "",
          fileEncoding = "latin1"
        )
      }
    )
    
    list(data = df, sep_used = sep)
  }
  
  
  updateVariables <- function() {
    req(rv$processed_data)
    data <- rv$processed_data
    all_vars <- names(data)
    
    rv$numeric_vars <- names(data)[sapply(data, is.numeric)]
    rv$categorical_vars <- names(data)[sapply(data, function(x) is.character(x) | is.factor(x))]
    
    ensure_valid_selection("uni_var", all_vars)
    ensure_valid_selection("bi_var1", all_vars)
    
    if (length(all_vars) >= 2) {
      cur <- input$bi_var2
      sel <- if (!is.null(cur) && cur %in% all_vars) cur else all_vars[2]
      updateSelectInput(session, "bi_var2", choices = all_vars, selected = sel)
    } else {
      ensure_valid_selection("bi_var2", all_vars)
    }
    # cible: plutôt catégorielles/factors
    target_choices <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
    if (length(target_choices) == 0) target_choices <- names(data)
    ensure_valid_selection("target_var", target_choices)
    
    # Modélisation : cible = plutôt catégorielle si possible
    mdl_target_choices <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
    if (length(mdl_target_choices) == 0) mdl_target_choices <- names(data)
    ensure_valid_selection("mdl_target", mdl_target_choices)
    
    
  }
  
  # -------- Load data
  observeEvent(input$file, {
    req(input$file)
    ext <- tolower(tools::file_ext(input$file$name))
    
    tryCatch({
      # --- lecture selon extension
      if (ext %in% c("csv", "txt", "data", "dat")) {
        
        enc <- if (input$txt_encoding == "auto") "UTF-8" else input$txt_encoding
        
        res <- read_text_robust(
          path = input$file$datapath,
          header = input$txt_header,
          sep_choice = input$txt_sep,
          dec = input$txt_dec,
          enc = enc
        )
        
        df <- res$data
        sep_used <- res$sep_used
        
        # meta lecture
        rv$read_meta <- list(
          sep = sep_used,
          header = input$txt_header,
          encoding = enc,
          ext = ext,
          ncol = ncol(df)
        )
        
        # message debug lecture (comme tu as vu)
        showNotification(
          paste0('📥 Lecture: sep="', ifelse(sep_used == "", "whitespace", sep_used),
                 '" | header=', input$txt_header,
                 " | colonnes=", ncol(df)),
          type = "message",
          duration = 5
        )
        
        # warning si 1 seule colonne -> séparateur faux
        if (ncol(df) == 1) {
          showNotification(
            "⚠️ Une seule colonne détectée. Essaie un autre séparateur ( ;  / Tab / espace ) ou mets 'Auto'.",
            type = "warning",
            duration = 8
          )
        }
        
        rv$raw_data <- smart_type(df)
        
      } else if (ext %in% c("xlsx", "xls")) {
        
        rv$raw_data <- readxl::read_excel(input$file$datapath) |> as.data.frame()
        rv$raw_data <- smart_type(rv$raw_data)
        
        rv$read_meta <- list(sep = NA, header = NA, encoding = NA, ext = ext, ncol = ncol(rv$raw_data))
        
      } else if (ext == "rds") {
        
        rv$raw_data <- readRDS(input$file$datapath)
        rv$raw_data <- smart_type(rv$raw_data)
        
        rv$read_meta <- list(sep = NA, header = NA, encoding = NA, ext = ext, ncol = ncol(rv$raw_data))
        
      } else {
        stop("Format non supporté: ", ext)
      }
      
      rv$processed_data <- rv$raw_data
      updateVariables()
      showNotification("✅ Données chargées!", type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(paste("❌ Erreur:", e$message), type = "error", duration = 6)
    })
  })
  
  
  # -------- Process data
  observeEvent(input$process, {
    req(rv$raw_data)
    data <- rv$raw_data
    
    withProgress(message = "Traitement...", value = 0, {
      incProgress(0.2, detail = "Colonnes NA...")
      na_prop <- colMeans(is.na(data)) * 100
      data <- data[, na_prop < input$na_threshold, drop = FALSE]
      
      if (input$remove_na) {
        incProgress(0.2, detail = "Suppression NA...")
        data <- na.omit(data)
      }
      
      if (input$remove_outliers) {
        incProgress(0.2, detail = "Outliers...")
        numeric_cols <- sapply(data, is.numeric)
        if (any(numeric_cols)) {
          for (col in names(data)[numeric_cols]) {
            z <- abs(scale(data[[col]]))
            data <- data[as.numeric(z) < 3 | is.na(z), , drop = FALSE]
          }
        }
      }
      
      if (input$normalize) {
        incProgress(0.2, detail = "Normalisation...")
        numeric_cols <- sapply(data, is.numeric)
        if (any(numeric_cols)) data[numeric_cols] <- scale(data[numeric_cols])
      }
      
      if (input$dummy) {
        incProgress(0.2, detail = "Dummy...")
        cat_cols <- sapply(data, function(x) is.character(x) | is.factor(x))
        if (any(cat_cols)) {
          data <- tryCatch(
            {
              model.matrix(~ . - 1, data = data) %>% as.data.frame()
            },
            error = function(e) {
              showNotification("⚠️ Erreur dummy (variables non compatibles)", type = "warning", duration = 4)
              data
            }
          )
        }
      }
    })
    
    rv$processed_data <- data
    updateVariables()
    showNotification("✨ Transformations OK!", type = "message", duration = 3)
  })
  
  # -------------------
  # Value boxes (Upload)
  # -------------------
  output$nrows <- renderValueBox({
    req(current_data())
    valueBox(nrow(current_data()), "Observations", icon = icon("database"), color = "blue")
  })
  
  output$ncols <- renderValueBox({
    req(current_data())
    valueBox(ncol(current_data()), "Variables", icon = icon("columns"), color = "green")
  })
  
  output$nmissing <- renderValueBox({
    req(current_data())
    valueBox(sum(is.na(current_data())), "Valeurs manquantes", icon = icon("question-circle"), color = "yellow")
  })
  
  output$noutliers <- renderValueBox({
    req(current_data())
    data <- current_data()
    numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
    n_out <- if (ncol(numeric_data) > 0) sum(abs(scale(numeric_data)) > 3, na.rm = TRUE) else 0
    valueBox(n_out, "Outliers potentiels", icon = icon("exclamation-triangle"), color = "red")
  })
  
  # -------------------
  # Overview (ex-Dashboard)
  # -------------------
  output$dash_nrows <- renderValueBox({
    req(current_data())
    valueBox(nrow(current_data()), "Observations", icon = icon("database"), color = "aqua")
  })
  
  output$dash_ncols <- renderValueBox({
    req(current_data())
    valueBox(ncol(current_data()), "Variables", icon = icon("columns"), color = "green")
  })
  
  output$dash_numeric <- renderValueBox({
    req(current_data())
    n_num <- sum(sapply(current_data(), is.numeric))
    valueBox(n_num, "Numériques", icon = icon("hashtag"), color = "blue")
  })
  
  output$dash_categorical <- renderValueBox({
    req(current_data())
    n_cat <- sum(sapply(current_data(), function(x) is.character(x) | is.factor(x)))
    valueBox(n_cat, "Catégorielles", icon = icon("tags"), color = "purple")
  })
  
  output$dash_var_types <- renderPlotly({
    req(current_data())
    data <- current_data()
    types <- sapply(data, function(x) {
      if (is.numeric(x)) "Numérique"
      else if (is.character(x) | is.factor(x)) "Catégorielle"
      else "Autre"
    })
    type_counts <- table(types)
    plot_ly(
      labels = names(type_counts),
      values = as.numeric(type_counts),
      type = "pie",
      marker = list(colors = viridis(length(type_counts)))
    ) %>% layout(title = "")
  })
  
  output$dash_completeness <- renderPlotly({
    req(current_data())
    data <- current_data()
    total <- nrow(data) * ncol(data)
    miss <- sum(is.na(data))
    completeness <- if (total > 0) (1 - miss / total) * 100 else 0
    missing_pct <- 100 - completeness
    
    plot_ly(
      labels = c("Complètes", "Manquantes"),
      values = c(completeness, missing_pct),
      type = "pie",
      marker = list(colors = c("#00a65a", "#dd4b39"))
    ) %>% layout(title = "")
  })
  
  output$dash_top_missing <- renderPlotly({
    req(current_data())
    data <- current_data()
    missing_counts <- colSums(is.na(data))
    missing_df <- data.frame(Variable = names(missing_counts), Count = as.numeric(missing_counts)) %>%
      filter(Count > 0) %>% arrange(desc(Count)) %>% head(10)
    
    if (nrow(missing_df) > 0) {
      plot_ly(
        missing_df,
        x = ~reorder(Variable, Count),
        y = ~Count,
        type = "bar",
        marker = list(color = "#dd4b39")
      ) %>% layout(xaxis = list(title = ""), yaxis = list(title = "NA"))
    } else {
      plot_ly() %>% layout(title = "Aucune NA")
    }
  })
  
  output$dash_quick_stats <- renderUI({
    req(current_data())
    data <- current_data()
    numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
    
    if (ncol(numeric_data) > 0) {
      stats <- list(
        "Variables numériques" = ncol(numeric_data),
        "Moyenne générale" = round(mean(unlist(numeric_data), na.rm = TRUE), 2),
        "Médiane générale" = round(median(unlist(numeric_data), na.rm = TRUE), 2),
        "SD moyen" = round(mean(sapply(numeric_data, sd, na.rm = TRUE)), 2)
      )
      
      tagList(lapply(names(stats), function(name) {
        tags$div(
          style = "padding: 15px; margin: 10px 0; background-color: #f4f6f9; border-radius: 5px; border-left: 4px solid #605ca8;",
          tags$strong(paste0(name, ":")),
          tags$span(style = "float: right; font-size: 18px;", stats[[name]])
        )
      }))
    } else {
      tags$p("Aucune variable numérique")
    }
  })
  
  # -------------------
  # Overview outputs
  # -------------------
  output$data_preview <- renderDT({
    req(current_data())
    datatable(current_data(), options = list(pageLength = 10, scrollX = TRUE), class = "cell-border stripe")
  })
  
  output$data_structure <- renderPrint({
    req(current_data())
    str(current_data())
  })
  
  output$summary_stats <- renderDT({
    req(current_data())
    data <- current_data()
    numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
    
    if (ncol(numeric_data) > 0) {
      stats <- data.frame(
        Variable = names(numeric_data),
        Min = sapply(numeric_data, min, na.rm = TRUE),
        Q1 = sapply(numeric_data, quantile, 0.25, na.rm = TRUE),
        Médiane = sapply(numeric_data, median, na.rm = TRUE),
        Moyenne = sapply(numeric_data, mean, na.rm = TRUE),
        Q3 = sapply(numeric_data, quantile, 0.75, na.rm = TRUE),
        Max = sapply(numeric_data, max, na.rm = TRUE),
        SD = sapply(numeric_data, sd, na.rm = TRUE)
      )
      datatable(stats, options = list(pageLength = 10, scrollX = TRUE)) %>%
        formatRound(columns = 2:8, digits = 2)
    }
  })
  
  output$var_types_plot <- renderPlotly({
    req(current_data())
    data <- current_data()
    types <- sapply(data, function(x) {
      if (is.numeric(x)) "Numérique"
      else if (is.character(x) | is.factor(x)) "Catégorielle"
      else "Autre"
    })
    type_counts <- table(types)
    plot_ly(
      labels = names(type_counts),
      values = as.numeric(type_counts),
      type = "pie",
      marker = list(colors = viridis(length(type_counts)))
    ) %>% layout(title = "")
  })
  
  output$missing_plot <- renderPlotly({
    req(current_data())
    data <- current_data()
    missing_counts <- colSums(is.na(data))
    missing_df <- data.frame(Variable = names(missing_counts), Count = as.numeric(missing_counts)) %>%
      filter(Count > 0) %>% arrange(desc(Count))
    
    if (nrow(missing_df) > 0) {
      plot_ly(
        missing_df,
        x = ~reorder(Variable, Count),
        y = ~Count,
        type = "bar",
        marker = list(color = ~Count, colorscale = "Reds")
      ) %>% layout(xaxis = list(title = ""), yaxis = list(title = "NA"))
    } else {
      plot_ly() %>% layout(title = "Aucune NA")
    }
  })
  
  # -------------------
  # Univariate outputs
  # -------------------
  output$is_numeric_uni <- reactive({
    req(current_data(), input$uni_var)
    data <- current_data()
    input$uni_var %in% names(data) && is.numeric(data[[input$uni_var]])
  })
  outputOptions(output, "is_numeric_uni", suspendWhenHidden = FALSE)
  
  # UI tabs univariée (grand bloc + onglets)
  output$uni_tabs_ui <- renderUI({
    req(current_data(), input$uni_var)
    data <- current_data()
    var <- input$uni_var
    validate(need(var %in% names(data), "Variable invalide (elle a peut-être été supprimée)."))
    
    if (is.numeric(data[[var]])) {
      tabBox(
        width = 12, height = "520px",
        tabPanel("Distribution", plotlyOutput("uni_plot", height = "450px")),
        tabPanel("Outliers / Répartition", plotlyOutput("uni_boxplot", height = "450px")),
        tabPanel("Normalité", plotlyOutput("uni_qq", height = "450px")),
        tabPanel("Densité", plotlyOutput("uni_density", height = "450px"))
      )
    } else {
      tabBox(
        width = 12, height = "520px",
        tabPanel("Répartition", plotlyOutput("uni_plot", height = "450px")),
        tabPanel("Pie", plotlyOutput("uni_pie", height = "450px"))
      )
    }
  })
  
  output$uni_plot <- renderPlotly({
    req(current_data(), input$uni_var)
    data <- current_data()
    var <- input$uni_var
    validate(need(var %in% names(data), "Variable invalide."))
    
    if (is.numeric(data[[var]])) {
      plot_ly(data, x = ~data[[var]], type = "histogram", name = "Histogramme",
              marker = list(color = "#605ca8")) %>%
        layout(xaxis = list(title = var), yaxis = list(title = "Fréquence"))
    } else {
      freq_table <- table(data[[var]])
      freq_df <- data.frame(Category = names(freq_table), Count = as.numeric(freq_table)) %>%
        arrange(desc(Count))
      
      if (isTRUE(input$show_percent)) {
        freq_df$Percent <- round(freq_df$Count / sum(freq_df$Count) * 100, 1)
        freq_df$Label <- paste0(freq_df$Category, " (", freq_df$Percent, "%)")
      } else {
        freq_df$Label <- freq_df$Category
      }
      
      plot_ly(freq_df, x = ~reorder(Label, Count), y = ~Count, type = "bar",
              marker = list(color = "#605ca8")) %>%
        layout(xaxis = list(title = var), yaxis = list(title = "Fréquence"))
    }
  })
  
  output$uni_boxplot <- renderPlotly({
    req(current_data(), input$uni_var)
    data <- current_data()
    var <- input$uni_var
    validate(need(var %in% names(data), "Variable invalide."))
    
    if (is.numeric(data[[var]])) {
      plot_ly(data, y = ~data[[var]], type = "box", name = var,
              marker = list(color = "#605ca8")) %>%
        layout(yaxis = list(title = var))
    }
  })
  
  output$uni_qq <- renderPlotly({
    req(current_data(), input$uni_var)
    data <- current_data()
    var <- input$uni_var
    validate(need(var %in% names(data), "Variable invalide."))
    
    if (is.numeric(data[[var]])) {
      x <- na.omit(data[[var]])
      validate(need(length(x) >= 3, "Pas assez de données pour un Q-Q plot."))
      qqdata <- qqnorm(x, plot.it = FALSE)
      
      plot_ly(x = ~qqdata$x, y = ~qqdata$y, type = "scatter", mode = "markers",
              marker = list(color = "#605ca8")) %>%
        add_trace(x = ~qqdata$x, y = ~qqdata$x, type = "scatter", mode = "lines",
                  line = list(color = "red"), name = "Théorique") %>%
        layout(xaxis = list(title = "Quantiles théoriques"),
               yaxis = list(title = "Quantiles observés"))
    }
  })
  
  output$uni_density <- renderPlotly({
    req(current_data(), input$uni_var)
    data <- current_data()
    var <- input$uni_var
    validate(need(var %in% names(data), "Variable invalide."))
    
    if (is.numeric(data[[var]])) {
      x <- na.omit(data[[var]])
      validate(need(length(x) >= 2, "Pas assez de données pour une densité."))
      dens <- density(x)
      
      plot_ly(x = ~dens$x, y = ~dens$y, type = "scatter", mode = "lines", fill = "tozeroy",
              fillcolor = "rgba(96, 92, 168, 0.3)", line = list(color = "#605ca8")) %>%
        layout(xaxis = list(title = var), yaxis = list(title = "Densité"))
    }
  })
  
  output$uni_pie <- renderPlotly({
    req(current_data(), input$uni_var)
    data <- current_data()
    var <- input$uni_var
    validate(need(var %in% names(data), "Variable invalide."))
    
    if (!is.numeric(data[[var]])) {
      freq_table <- table(data[[var]])
      plot_ly(labels = names(freq_table), values = as.numeric(freq_table), type = "pie",
              marker = list(colors = viridis(length(freq_table)))) %>%
        layout(title = "")
    }
  })
  
  output$uni_stats <- renderUI({
    req(current_data(), input$uni_var)
    data <- current_data()
    var <- input$uni_var
    validate(need(var %in% names(data), "Variable invalide."))
    
    if (is.numeric(data[[var]])) {
      x <- na.omit(data[[var]])
      
      shapiro_test <- tryCatch({
        if (length(x) >= 3 && length(x) <= 5000) shapiro.test(x) else NULL
      }, error = function(e) NULL)
      
      stats_list <- list(
        "Moyenne" = mean(x, na.rm = TRUE),
        "Médiane" = median(x, na.rm = TRUE),
        "Écart-type" = sd(x, na.rm = TRUE),
        "Minimum" = min(x, na.rm = TRUE),
        "Maximum" = max(x, na.rm = TRUE),
        "Q1" = quantile(x, 0.25, na.rm = TRUE),
        "Q3" = quantile(x, 0.75, na.rm = TRUE),
        "Asymétrie" = e1071::skewness(x, na.rm = TRUE),
        "Aplatissement" = e1071::kurtosis(x, na.rm = TRUE)
      )
      
      html_output <- lapply(names(stats_list), function(name) {
        tags$div(
          style = "padding: 10px; margin: 5px 0; background-color: #f4f6f9; border-radius: 5px;",
          tags$strong(paste0(name, ":")),
          tags$span(style = "float: right;", round(stats_list[[name]], 3))
        )
      })
      
      if (!is.null(shapiro_test)) {
        ok <- shapiro_test$p.value > 0.05
        shapiro_div <- tags$div(
          style = paste0(
            "padding: 10px; margin: 10px 0; background-color: ",
            if (ok) "#d4edda" else "#f8d7da",
            "; border-radius: 5px; border-left: 4px solid ",
            if (ok) "#28a745" else "#dc3545", ";"
          ),
          tags$h5("🧪 Test de Shapiro-Wilk"),
          tags$p(paste0("p-value: ", round(shapiro_test$p.value, 4))),
          tags$p(if (ok) "✅ Distribution normale" else "❌ Non normale")
        )
        html_output <- c(html_output, list(shapiro_div))
      }
      
      tagList(html_output)
      
    } else {
      freq_table <- table(data[[var]])
      mode_val <- names(freq_table)[which.max(freq_table)]
      n_categories <- length(freq_table)
      
      tagList(
        tags$div(
          style = "padding: 10px; margin: 5px 0; background-color: #f4f6f9; border-radius: 5px;",
          tags$strong("Catégories:"),
          tags$span(style = "float: right;", n_categories)
        ),
        tags$div(
          style = "padding: 10px; margin: 5px 0; background-color: #f4f6f9; border-radius: 5px;",
          tags$strong("Mode:"),
          tags$span(style = "float: right;", mode_val)
        ),
        tags$div(
          style = "padding: 10px; margin: 5px 0; background-color: #f4f6f9; border-radius: 5px;",
          tags$strong("Fréquence:"),
          tags$span(style = "float: right;", max(freq_table))
        ),
        tags$div(
          style = "padding: 10px; margin: 5px 0; background-color: #f4f6f9; border-radius: 5px;",
          tags$strong("% du mode:"),
          tags$span(style = "float: right;", paste0(round(max(freq_table) / sum(freq_table) * 100, 1), "%"))
        )
      )
    }
  })
  
  # -------------------
  # Bivariate outputs
  # -------------------
  output$both_numeric <- reactive({
    req(current_data(), input$bi_var1, input$bi_var2)
    data <- current_data()
    (input$bi_var1 %in% names(data)) && (input$bi_var2 %in% names(data)) &&
      is.numeric(data[[input$bi_var1]]) && is.numeric(data[[input$bi_var2]])
  })
  outputOptions(output, "both_numeric", suspendWhenHidden = FALSE)
  
  output$one_cat_one_num <- reactive({
    req(current_data(), input$bi_var1, input$bi_var2)
    data <- current_data()
    validate(need(input$bi_var1 %in% names(data) && input$bi_var2 %in% names(data), FALSE))
    is_num1 <- is.numeric(data[[input$bi_var1]])
    is_num2 <- is.numeric(data[[input$bi_var2]])
    (is_num1 && !is_num2) || (!is_num1 && is_num2)
  })
  outputOptions(output, "one_cat_one_num", suspendWhenHidden = FALSE)
  
  output$bi_plot <- renderPlotly({
    req(current_data(), input$bi_var1, input$bi_var2)
    data <- current_data()
    var1 <- input$bi_var1
    var2 <- input$bi_var2
    validate(need(var1 %in% names(data) && var2 %in% names(data), "Variables invalides."))
    
    is_num1 <- is.numeric(data[[var1]])
    is_num2 <- is.numeric(data[[var2]])
    
    if (is_num1 && is_num2) {
      df <- data %>% select(all_of(c(var1, var2))) %>% drop_na()
      validate(need(nrow(df) >= 3, "Pas assez de données (après NA)."))
      
      p <- plot_ly(df, x = ~.data[[var1]], y = ~.data[[var2]],
                   type = "scatter", mode = "markers",
                   marker = list(color = "#605ca8", opacity = 0.6)) %>%
        layout(xaxis = list(title = var1), yaxis = list(title = var2))
      
      if (isTRUE(input$show_regression)) {
        fit <- lm(df[[var2]] ~ df[[var1]])
        p <- p %>% add_trace(x = ~df[[var1]], y = ~fitted(fit),
                             type = "scatter", mode = "lines",
                             line = list(color = "red"), name = "Régression")
      }
      p
      
    } else if (!is_num1 && !is_num2) {
      cont_table <- table(data[[var1]], data[[var2]])
      validate(need(all(dim(cont_table) > 0), "Table de contingence vide."))
      
      plot_ly(z = cont_table, x = colnames(cont_table), y = rownames(cont_table),
              type = "heatmap", colorscale = "Viridis") %>%
        layout(xaxis = list(title = var2), yaxis = list(title = var1))
      
    } else {
      if (!is_num1) {
        cat_var <- var1
        num_var <- var2
      } else {
        cat_var <- var2
        num_var <- var1
      }
      
      df <- data %>% select(all_of(c(cat_var, num_var))) %>% drop_na()
      validate(need(nrow(df) >= 3, "Pas assez de données (après NA)."))
      
      if (identical(input$bi_plot_type, "violin")) {
        plot_ly(df, x = ~.data[[cat_var]], y = ~.data[[num_var]],
                type = "violin",
                box = list(visible = TRUE),
                meanline = list(visible = TRUE),
                marker = list(color = "#605ca8")) %>%
          layout(xaxis = list(title = cat_var), yaxis = list(title = num_var))
      } else {
        plot_ly(df, x = ~.data[[cat_var]], y = ~.data[[num_var]],
                type = "box",
                marker = list(color = "#605ca8")) %>%
          layout(xaxis = list(title = cat_var), yaxis = list(title = num_var))
      }
    }
  })
  
  output$bi_stats <- renderUI({
    req(current_data(), input$bi_var1, input$bi_var2)
    data <- current_data()
    var1 <- input$bi_var1
    var2 <- input$bi_var2
    validate(need(var1 %in% names(data) && var2 %in% names(data), "Variables invalides."))
    
    is_num1 <- is.numeric(data[[var1]])
    is_num2 <- is.numeric(data[[var2]])
    
    if (is_num1 && is_num2) {
      df <- data %>% select(all_of(c(var1, var2))) %>% drop_na()
      validate(need(nrow(df) >= 3, "Pas assez de données (après NA)."))
      
      cor_val <- cor(df[[var1]], df[[var2]])
      cor_test <- cor.test(df[[var1]], df[[var2]])
      
      fit <- lm(df[[var2]] ~ df[[var1]])
      r_squared <- summary(fit)$r.squared
      
      result <- tagList(
        tags$h4("📊 Corrélation de Pearson"),
        tags$div(
          style = "padding: 10px; margin: 5px 0; background-color: #f4f6f9; border-radius: 5px;",
          tags$strong("Coefficient:"),
          tags$span(style = "float: right;", round(cor_val, 4))
        ),
        tags$div(
          style = "padding: 10px; margin: 5px 0; background-color: #f4f6f9; border-radius: 5px;",
          tags$strong("p-value:"),
          tags$span(style = "float: right;", format.pval(cor_test$p.value, digits = 4))
        )
      )
      
      if (isTRUE(input$show_r2)) {
        result <- tagList(
          result,
          tags$div(
            style = "padding: 10px; margin: 5px 0; background-color: #f4f6f9; border-radius: 5px;",
            tags$strong("R²:"),
            tags$span(style = "float: right;", round(r_squared, 4))
          )
        )
      }
      
      tagList(
        result,
        tags$div(
          style = "padding: 10px; margin: 10px 0; background-color: #d4edda; border-radius: 5px; border-left: 4px solid #28a745;",
          tags$p(
            style = "margin: 5px;",
            if (abs(cor_val) > 0.7) "✅ Corrélation forte"
            else if (abs(cor_val) > 0.3) "⚠️ Corrélation modérée"
            else "❌ Corrélation faible"
          )
        )
      )
      
    } else if (!is_num1 && !is_num2) {
      cont_table <- table(data[[var1]], data[[var2]])
      validate(need(all(dim(cont_table) > 1), "Pas assez de modalités pour un test."))
      
      cramer_v <- DescTools::CramerV(cont_table)
      chi_test <- suppressWarnings(chisq.test(cont_table))
      
      tagList(
        tags$h4("📊 V de Cramer"),
        tags$div(
          style = "padding: 10px; margin: 5px 0; background-color: #f4f6f9; border-radius: 5px;",
          tags$strong("Coefficient:"),
          tags$span(style = "float: right;", round(cramer_v, 4))
        ),
        tags$div(
          style = "padding: 10px; margin: 5px 0; background-color: #f4f6f9; border-radius: 5px;",
          tags$strong("Chi² p-value:"),
          tags$span(style = "float: right;", format.pval(chi_test$p.value, digits = 4))
        )
      )
      
    } else {
      if (!is_num1) {
        cat_var <- var1
        num_var <- var2
      } else {
        cat_var <- var2
        num_var <- var1
      }
      
      df <- data %>% select(all_of(c(cat_var, num_var))) %>% drop_na()
      validate(need(nrow(df) >= 3, "Pas assez de données (après NA)."))
      
      groups <- split(df[[num_var]], df[[cat_var]])
      overall_mean <- mean(df[[num_var]], na.rm = TRUE)
      
      between_var <- sum(sapply(groups, function(g) length(g) * (mean(g, na.rm = TRUE) - overall_mean)^2))
      total_var <- sum((df[[num_var]] - overall_mean)^2, na.rm = TRUE)
      eta_squared <- if (total_var > 0) between_var / total_var else NA_real_
      
      n_groups <- length(unique(df[[cat_var]]))
      
      if (n_groups == 2) {
        test_result <- t.test(df[[num_var]] ~ df[[cat_var]])
        test_name <- "t-test"
        test_stat <- unname(test_result$statistic)
        test_pval <- test_result$p.value
      } else {
        formula <- as.formula(paste(num_var, "~", cat_var))
        test_result <- aov(formula, data = df)
        test_summary <- summary(test_result)
        
        test_name <- "ANOVA"
        test_stat <- test_summary[[1]][["F value"]][1]
        test_pval <- test_summary[[1]][["Pr(>F)"]][1]
      }
      
      tagList(
        tags$h4("📊 Rapport de corrélation (η²)"),
        tags$div(
          style = "padding: 10px; margin: 5px 0; background-color: #f4f6f9; border-radius: 5px;",
          tags$strong("η²:"),
          tags$span(style = "float: right;", if (is.na(eta_squared)) "NA" else round(eta_squared, 4))
        ),
        tags$hr(),
        tags$h4(paste("🧪", test_name)),
        tags$div(
          style = "padding: 10px; margin: 5px 0; background-color: #f4f6f9; border-radius: 5px;",
          tags$strong("Statistique:"),
          tags$span(style = "float: right;", round(test_stat, 4))
        ),
        tags$div(
          style = "padding: 10px; margin: 5px 0; background-color: #f4f6f9; border-radius: 5px;",
          tags$strong("p-value:"),
          tags$span(style = "float: right;", format.pval(test_pval, digits = 4))
        )
      )
    }
  })
  
  # -------------------
  # Corrélations (affichées dans bivariée)
  # -------------------
  output$corr_plot <- renderPlotly({
    req(current_data())
    data <- current_data()
    numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
    
    if (ncol(numeric_data) >= 2) {
      cor_matrix <- cor(numeric_data, use = "complete.obs")
      
      plot_ly(
        z = cor_matrix,
        x = colnames(cor_matrix),
        y = rownames(cor_matrix),
        type = "heatmap",
        colorscale = list(c(0, "navy"), c(0.5, "white"), c(1, "red")),
        zmid = 0, zmin = -1, zmax = 1,
        text = round(cor_matrix, 3),
        hovertemplate = "X: %{x}<br>Y: %{y}<br>Corrélation: %{z:.3f}<extra></extra>"
      ) %>% layout(title = "Matrice de corrélations de Pearson", xaxis = list(title = ""), yaxis = list(title = ""))
    } else {
      plot_ly() %>% layout(title = "Minimum 2 variables numériques requises")
    }
  })
  
  output$cramer_plot <- renderPlotly({
    req(current_data())
    data <- current_data()
    cat_data <- data[, sapply(data, function(x) is.character(x) | is.factor(x)), drop = FALSE]
    
    if (ncol(cat_data) >= 2) {
      n_vars <- ncol(cat_data)
      var_names <- names(cat_data)
      cramer_matrix <- matrix(NA, n_vars, n_vars, dimnames = list(var_names, var_names))
      
      for (i in seq_len(n_vars)) {
        for (j in seq_len(n_vars)) {
          if (i == j) {
            cramer_matrix[i, j] <- 1
          } else if (i < j) {
            cramer_matrix[i, j] <- tryCatch({
              DescTools::CramerV(table(cat_data[[i]], cat_data[[j]]))
            }, error = function(e) NA_real_)
            cramer_matrix[j, i] <- cramer_matrix[i, j]
          }
        }
      }
      
      plot_ly(
        z = cramer_matrix,
        x = colnames(cramer_matrix),
        y = rownames(cramer_matrix),
        type = "heatmap",
        colorscale = "Viridis",
        zmin = 0, zmax = 1,
        text = round(cramer_matrix, 3),
        hovertemplate = "X: %{x}<br>Y: %{y}<br>V de Cramer: %{z:.3f}<extra></extra>"
      ) %>% layout(title = "Matrice du V de Cramer", xaxis = list(title = ""), yaxis = list(title = ""))
    } else {
      plot_ly() %>% layout(title = "Minimum 2 variables catégorielles requises")
    }
  })
  
  output$mdl_features_ui <- renderUI({
    req(current_data(), input$mdl_target)
    data <- current_data()
    
    all_vars <- names(data)
    target <- input$mdl_target
    if (!target %in% all_vars) return(NULL)
    
    feats <- setdiff(all_vars, target)
    
    selectizeInput(
      "mdl_features",
      "Variables explicatives :",
      choices = feats,
      selected = feats,
      multiple = TRUE,
      options = list(plugins = list("remove_button"), placeholder = "Choisis des features...")
    )
  })
  
  calc_prf_macro <- function(truth, pred) {
    truth <- factor(truth)
    pred  <- factor(pred, levels = levels(truth))
    
    cm <- table(truth, pred)
    classes <- rownames(cm)
    
    per_class <- lapply(classes, function(cl){
      tp <- cm[cl, cl]
      fp <- sum(cm[, cl]) - tp
      fn <- sum(cm[cl, ]) - tp
      
      precision <- if ((tp + fp) == 0) NA_real_ else tp / (tp + fp)
      recall    <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
      f1        <- if (is.na(precision) || is.na(recall) || (precision + recall) == 0) NA_real_ else 2 * precision * recall / (precision + recall)
      
      c(precision = precision, recall = recall, f1 = f1)
    })
    
    mat <- do.call(rbind, per_class)
    colMeans(mat, na.rm = TRUE)
  }
  
  make_cm_heatmap <- function(cm_table, title = "Matrice de confusion") {
    df <- as.data.frame(cm_table)
    colnames(df) <- c("Réel", "Prédit", "N")
    
    plot_ly(
      df,
      x = ~Prédit,
      y = ~Réel,
      z = ~N,
      type = "heatmap",
      colorscale = "Blues",
      hovertemplate = "Réel: %{y}<br>Prédit: %{x}<br>N: %{z}<extra></extra>"
    ) %>%
      add_trace(
        type = "scatter",
        mode = "text",
        x = df$Prédit,
        y = df$Réel,
        text = df$N,
        textfont = list(color = "black")
      ) %>%
      layout(
        title = title,
        xaxis = list(title = "Classe prédite"),
        yaxis = list(title = "Classe réelle")
      )
  }
  
  
  mdl_results <- eventReactive(input$mdl_train, {
    req(current_data(), input$mdl_target, input$mdl_features)
    set.seed(input$mdl_seed)
    
    data <- current_data()
    
    target <- input$mdl_target
    feats  <- input$mdl_features
    
    validate(need(target %in% names(data), "Cible invalide."))
    
    # ✅ FIX : on garde uniquement les features encore existantes
    feats <- intersect(feats, setdiff(names(data), target))
    validate(need(length(feats) >= 1, "Sélectionne au moins 1 feature valide (certaines colonnes ont peut-être été supprimées)."))
    
    df <- data[, c(target, feats), drop = FALSE]
    
    
    # target en factor
    if (is.character(df[[target]])) df[[target]] <- as.factor(df[[target]])
    if (!is.factor(df[[target]])) df[[target]] <- as.factor(df[[target]])
    
    # supprime lignes NA sur cible
    df <- df[!is.na(df[[target]]), , drop = FALSE]
    validate(need(nrow(df) >= 30, "Dataset trop petit pour entraîner proprement (>= 30 lignes conseillées)."))
    
    # retire niveaux trop rares (optionnel mais utile)
    taby <- table(df[[target]])
    if (any(taby < 2)) {
      keep_levels <- names(taby)[taby >= 2]
      df <- df[df[[target]] %in% keep_levels, , drop = FALSE]
      df[[target]] <- droplevels(df[[target]])
    }
    validate(need(nlevels(df[[target]]) >= 2, "La cible doit avoir au moins 2 classes."))
    
    # split stratifié
    idx <- createDataPartition(df[[target]], p = input$mdl_split/100, list = FALSE)
    train_df <- df[idx, , drop = FALSE]
    test_df  <- df[-idx, , drop = FALSE]
    validate(need(nrow(test_df) >= 10, "Test set trop petit : augmente la taille du dataset ou baisse Train(%)."))
    
    # prétraitement
    x_train <- train_df[, feats, drop = FALSE]
    y_train <- train_df[[target]]
    x_test  <- test_df[, feats, drop = FALSE]
    y_test  <- test_df[[target]]
    
    # dummification + centering/scaling via caret
    pp <- NULL
    if (isTRUE(input$mdl_dummy)) {
      dv <- dummyVars(~ ., data = x_train, fullRank = TRUE)
      x_train <- as.data.frame(predict(dv, newdata = x_train))
      x_test  <- as.data.frame(predict(dv, newdata = x_test))
    } else {
      # si chars restants, convertit en factor (caret aime pas chars)
      for (nm in names(x_train)) if (is.character(x_train[[nm]])) x_train[[nm]] <- as.factor(x_train[[nm]])
      for (nm in names(x_test))  if (is.character(x_test[[nm]]))  x_test[[nm]]  <- as.factor(x_test[[nm]])
    }
    
    if (isTRUE(input$mdl_center_scale)) {
      pp <- preProcess(x_train, method = c("center","scale"))
      x_train <- predict(pp, x_train)
      x_test  <- predict(pp, x_test)
    }
    
    train_ready <- data.frame(x_train, .target = y_train)
    test_ready  <- data.frame(x_test,  .target = y_test)
    
    
    # CV control + entraînement multi-modèles
    is_binary <- nlevels(train_ready$.target) == 2
    
    validate(need(length(input$mdl_models) >= 2, "Choisis au moins 2 modèles à comparer."))
    
    metric_user <- input$mdl_metric
    # si binaire, ROC est souvent plus pertinent pour comparer des modèles
    if (is_binary && metric_user %in% c("Accuracy","Kappa")) {
      metric_user <- "ROC"
    }
    
    ctrl <- trainControl(
      method = "repeatedcv",
      number = input$mdl_folds,
      repeats = 3,
      classProbs = TRUE,
      summaryFunction = twoClassSummary,
      sampling = if (input$mdl_sampling == "none") NULL else input$mdl_sampling,
      savePredictions = "final"
    )
    
    
    # caret: twoClassSummary attend un facteur avec 2 niveaux (le 1er = "positive class")
    if (is_binary) {
      lv <- levels(train_ready$.target)
      train_ready$.target <- factor(train_ready$.target, levels = lv)
      test_ready$.target  <- factor(test_ready$.target,  levels = lv)
    }
    
    # Définition des méthodes caret
    model_specs <- list(
      glm   = list(name = "Logistique / Multinom", method = if (is_binary) "glm" else "multinom"),
      rpart = list(name = "Arbre (CART)",          method = "rpart"),
      rf    = list(name = "Random Forest",         method = "rf")
    )
    
    xtr <- train_ready[, setdiff(names(train_ready), ".target"), drop = FALSE]
    ytr <- train_ready$.target
    xte <- test_ready[,  setdiff(names(test_ready),  ".target"), drop = FALSE]
    yte <- test_ready$.target
    
    fits <- list()
    
    for (m in input$mdl_models) {
      spec <- model_specs[[m]]
      if (is.null(spec)) next
      
      if (spec$method == "glm") {
        fits[[m]] <- train(
          x = xtr, y = ytr,
          method = "glm",
          family = binomial(),
          metric = metric_user,
          trControl = ctrl
        )
      } else if (spec$method == "multinom") {
        fits[[m]] <- train(
          x = xtr, y = ytr,
          method = "multinom",
          metric = input$mdl_metric,
          trControl = ctrl,
          trace = FALSE
        )
      } else {
        fits[[m]] <- train(
          x = xtr, y = ytr,
          method = spec$method,
          metric = metric_user,
          trControl = ctrl,
          tuneLength = input$mdl_tuneLength
        )
      }
    }
    
    validate(need(length(fits) >= 2, "Impossible d'entraîner au moins 2 modèles (vérifie la cible/features)."))
    
    all_results <- list()
    cms  <- list()
    rocs <- list()
    imps <- list()
    
    for (m in names(fits)) {
      fit <- fits[[m]]
      
      pred <- predict(fit, newdata = xte)
      pred <- factor(pred, levels = levels(yte))
      cm <- caret::confusionMatrix(pred, yte)
      cms[[m]] <- cm
      
      prf <- calc_prf_macro(yte, pred)
      
      # ROC/AUC (si binaire)
      auc_val <- NA_real_
      roc_obj <- NULL
      if (is_binary) {
        probs <- predict(fit, newdata = xte, type = "prob")
        pos_class <- levels(yte)[1]
        roc_obj <- pROC::roc(response = yte, predictor = probs[[pos_class]],
                             levels = rev(levels(yte)), quiet = TRUE)
        auc_val <- as.numeric(pROC::auc(roc_obj))
      }
      rocs[[m]] <- roc_obj
      
      # importance (si dispo)
      imps[[m]] <- tryCatch({
        vi <- caret::varImp(fit, scale = TRUE)$importance
        data.frame(Feature = rownames(vi), Importance = vi[,1], row.names = NULL) |>
          dplyr::arrange(dplyr::desc(Importance))
      }, error = function(e) NULL)
      
      all_results[[m]] <- data.frame(
        Modèle = model_specs[[m]]$name,
        Code = m,
        Accuracy = as.numeric(cm$overall["Accuracy"]),
        Precision_macro = as.numeric(prf["precision"]),
        Recall_macro = as.numeric(prf["recall"]),
        F1_macro = as.numeric(prf["f1"]),
        AUC = auc_val,
        row.names = NULL
      )
    }
    
    results <- dplyr::bind_rows(all_results)
    
    list(
      is_binary = is_binary,
      fits = fits,
      cms = cms,
      results = results,
      rocs = rocs,
      imps = imps
    )
  })    
  
  
  output$mdl_metrics_table <- renderDT({
    req(mdl_results())
    res <- mdl_results()$results
    DT::datatable(res, options = list(pageLength = 5, scrollX = TRUE)) %>%
      DT::formatRound(columns = c("Accuracy","Precision_macro","Recall_macro","F1_macro","AUC"), digits = 3)
  })
  
  
  # --- Matrices de confusion dynamiques (selon modèles sélectionnés)
  output$mdl_cm_ui <- renderUI({
    req(mdl_results())
    cms <- mdl_results()$cms
    
    tabs <- lapply(names(cms), function(m){
      tabPanel(
        title = m,
        tags$br(),
        tags$h4("🧾 Matrice (texte)"),
        verbatimTextOutput(paste0("cm_txt_", m)),
        tags$hr(),
        tags$h4("🎨 Matrice (visuelle)"),
        plotlyOutput(paste0("cm_plot_", m), height = "420px")
      )
    })
    
    do.call(tabBox, c(list(width = 12), tabs))
  })
  
  
  observe({
    req(mdl_results())
    cms <- mdl_results()$cms
    
    for (m in names(cms)) {
      local({
        mm <- m
        
        output[[paste0("cm_txt_", mm)]] <- renderPrint({
          mdl_results()$cms[[mm]]
        })
        
        output[[paste0("cm_plot_", mm)]] <- renderPlotly({
          cm_obj <- mdl_results()$cms[[mm]]
          make_cm_heatmap(cm_obj$table, title = paste0("Matrice — ", mm))
        })
      })
    }
  })
  
  
  # --- Mise à jour du select pour l'importance
  observe({
    req(mdl_results())
    choices <- names(mdl_results()$imps)
    updateSelectInput(session, "mdl_imp_model",
                      choices = choices,
                      selected = if (length(choices) > 0) choices[1] else character(0))
  })
  
  output$mdl_notes <- renderUI({
    req(mdl_results())
    if (mdl_results()$is_binary) {
      tags$div(
        style="padding:10px; background: rgba(255,255,255,0.65); border-radius: 12px;",
        tags$strong("✅ Binaire : ROC/AUC affichés + probabilités utilisées.")
      )
    } else {
      tags$div(
        style="padding:10px; background: rgba(255,255,255,0.65); border-radius: 12px;",
        tags$strong("ℹ️ Multiclasse : ROC non affiché (on garde Precision/Recall/F1 macro + Accuracy).")
      )
    }
  })
  
  
  output$mdl_roc_plot <- renderPlotly({
    req(mdl_results())
    out <- mdl_results()
    
    if (!out$is_binary) {
      return(plot_ly() %>% layout(title = "ROC disponible uniquement en classification binaire"))
    }
    
    p <- plot_ly()
    for (m in names(out$rocs)) {
      roc_obj <- out$rocs[[m]]
      if (is.null(roc_obj)) next
      df <- data.frame(fpr = 1 - roc_obj$specificities, tpr = roc_obj$sensitivities)
      aucv <- as.numeric(pROC::auc(roc_obj))
      p <- p %>% add_trace(
        data = df, x = ~fpr, y = ~tpr, type = "scatter", mode = "lines",
        name = paste0(m, " (AUC=", round(aucv, 3), ")")
      )
    }
    
    p %>%
      add_trace(x = c(0,1), y = c(0,1), type = "scatter", mode = "lines",
                name = "Baseline", line = list(dash = "dash")) %>%
      layout(
        xaxis = list(title = "False Positive Rate"),
        yaxis = list(title = "True Positive Rate"),
        title = "Courbes ROC (test set)"
      )
  })
  
  
  
  output$mdl_imp_plot <- renderPlotly({
    req(mdl_results(), input$mdl_imp_model)
    out <- mdl_results()
    
    imp <- out$imps[[input$mdl_imp_model]]
    
    if (is.null(imp) || nrow(imp) == 0) {
      return(plot_ly() %>% layout(title = "Importance non disponible pour ce modèle."))
    }
    
    imp <- head(imp, 20)
    plot_ly(
      imp,
      x = ~Importance,
      y = ~reorder(Feature, Importance),
      type = "bar",
      orientation = "h"
    ) %>%
      layout(
        xaxis = list(title = "Importance"),
        yaxis = list(title = ""),
        title = paste0("Top 20 — ", input$mdl_imp_model)
      )
  })
  
  
}





shinyApp(ui = ui, server = server)


