# Génère les jeux de données des labs 3, 4 et 5 (boutique en ligne fictive "Solea").
# Usage (depuis la racine du projet) : Rscript scripts/generer_donnees.R
# Les données sont fictives et reproductibles (graine fixe).

set.seed(2024)
dir.create("data", showWarnings = FALSE)

# ---- Catalogue produits (prix fixes) --------------------------------------
catalogue <- data.frame(
  produit = c("Lampe de bureau", "Coussin décoratif", "Set de casseroles", "Plaid en laine",
              "Tapis de yoga", "Haltères 5 kg", "Gourde isotherme", "Corde à sauter",
              "Crème hydratante", "Huile d'argan", "Savon naturel", "Parfum d'ambiance"),
  categorie = rep(c("Maison", "Sport", "Beauté"), each = 4),
  prix_unitaire = c(34.90, 19.90, 79.00, 45.50,
                    24.90, 29.90, 17.50, 9.90,
                    22.00, 15.90, 6.50, 27.00),
  stringsAsFactors = FALSE
)

# ---- Clients ----------------------------------------------------------------
n_clients <- 130
clients <- data.frame(
  client_id = sprintf("C%03d", 1:n_clients),
  segment = sample(c("Standard", "Premium", "Nouveau"), n_clients, replace = TRUE, prob = c(.6, .2, .2)),
  age = sample(18:70, n_clients, replace = TRUE),
  canal = sample(c("Réseaux sociaux", "Moteur de recherche", "Bouche-à-oreille", "Publicité"),
                 n_clients, replace = TRUE, prob = c(.35, .3, .2, .15)),
  stringsAsFactors = FALSE
)
# Format international : virgule comme séparateur, point décimal
write.csv(clients, "data/clients.csv", row.names = FALSE, fileEncoding = "UTF-8")

# ---- Ventes "vérité" -------------------------------------------------------
n <- 400
jours <- seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day")
poids_mois <- c(1, 1, 1, 1, 1, 1, .8, .8, 1, 1.1, 1.8, 2.2)
date <- sort(sample(jours, n, replace = TRUE,
                    prob = poids_mois[as.integer(format(jours, "%m"))]))
idx <- sample(nrow(catalogue), n, replace = TRUE)
verite <- data.frame(
  id_commande = sprintf("CMD%04d", 1:n),
  date = date,
  client_id = sample(clients$client_id[1:120], n, replace = TRUE),
  produit = catalogue$produit[idx],
  categorie = catalogue$categorie[idx],
  quantite = sample(1:5, n, replace = TRUE, prob = c(.4, .3, .15, .1, .05)),
  prix_unitaire = catalogue$prix_unitaire[idx],
  remise = sample(c(0, .05, .10, .15, .20), n, replace = TRUE, prob = c(.5, .15, .15, .1, .1)),
  ville = sample(c("Paris", "Lyon", "Marseille", "Lille", "Bordeaux", "Nantes"), n,
                 replace = TRUE, prob = c(.3, .2, .15, .15, .1, .1)),
  mode_paiement = sample(c("Carte", "Paypal", "Virement"), n, replace = TRUE, prob = c(.6, .3, .1)),
  stringsAsFactors = FALSE
)

# ---- Version "sale" (export type Excel français) --------------------------
sale <- verite
sale$date <- format(sale$date, "%d/%m/%Y")

abime_texte <- function(x, part = 0.25) {
  i <- sample(length(x), round(part * length(x)))
  for (k in i) {
    x[k] <- switch(sample(3, 1),
                   toupper(x[k]),
                   tolower(x[k]),
                   paste0(x[k], " "))
  }
  x
}
sale$categorie <- abime_texte(sale$categorie)
# Certaines saisies de "Beauté" perdent leur accent
i_beaute <- which(grepl("eaut", sale$categorie, ignore.case = TRUE))
i_sans_accent <- sample(i_beaute, round(0.3 * length(i_beaute)))
sale$categorie[i_sans_accent] <- chartr("éÉ", "eE", sale$categorie[i_sans_accent])
sale$ville <- abime_texte(sale$ville)

sale$date[sample(n, 8)] <- NA
sale$quantite[sample(n, 6)] <- NA
sale$quantite[sample(n, 4)] <- c(250, 100, -2, 0)
sale$prix_unitaire[sample(n, 10)] <- NA
sale$remise[sample(n, 30)] <- NA
sale$mode_paiement[sample(n, 15)] <- NA

# 12 doublons exacts insérés à côté de l'original
dup <- sample(n, 12)
sale <- rbind(sale, sale[dup, ])
sale <- sale[order(c(seq_len(n), dup + 0.5)), ]

# Écriture façon Excel FR : séparateur ";" et virgule décimale ; NA -> cellule vide
write.table(sale, "data/ventes_brutes.csv", sep = ";", dec = ",", row.names = FALSE,
            na = "", quote = FALSE, fileEncoding = "UTF-8")

# ---- Nettoyage de référence (corrigé du lab 3) ----------------------------
v <- read.csv2("data/ventes_brutes.csv", na.strings = c("", "NA"), fileEncoding = "UTF-8")
v <- v[!duplicated(v), ]
maj1 <- function(x) { x <- tolower(trimws(x)); paste0(toupper(substr(x, 1, 1)), substring(x, 2)) }
v$categorie <- maj1(v$categorie)
v$categorie[v$categorie == "Beaute"] <- "Beauté"
v$ville <- maj1(v$ville)
v$date <- as.Date(v$date, format = "%d/%m/%Y")
v <- v[!is.na(v$date), ]
v <- v[!is.na(v$quantite) & v$quantite > 0 & v$quantite <= 20, ]
prix_ref <- ave(v$prix_unitaire, v$produit, FUN = function(x) median(x, na.rm = TRUE))
v$prix_unitaire <- ifelse(is.na(v$prix_unitaire), prix_ref, v$prix_unitaire)
v$remise[is.na(v$remise)] <- 0
v$mode_paiement[is.na(v$mode_paiement)] <- "Inconnu"
rownames(v) <- NULL
stopifnot(!anyNA(v),
          setequal(v$categorie, c("Maison", "Sport", "Beauté")),
          setequal(v$ville, c("Paris", "Lyon", "Marseille", "Lille", "Bordeaux", "Nantes")))
write.csv(v, "data/ventes_propres.csv", row.names = FALSE, fileEncoding = "UTF-8")

# ---- Table enrichie (départ du lab 5) ---------------------------------------
e <- v
e$montant <- round(e$quantite * e$prix_unitaire * (1 - e$remise), 2)
e$mois <- as.integer(format(e$date, "%m"))
e <- merge(e, clients, by = "client_id", all.x = TRUE, sort = FALSE)
e <- e[order(e$id_commande), ]
rownames(e) <- NULL
write.csv(e, "data/ventes_enrichies.csv", row.names = FALSE, fileEncoding = "UTF-8")

cat("brutes:", nrow(sale), "| propres:", nrow(v), "| enrichies:", nrow(e), "\n")
