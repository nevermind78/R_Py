// Affichage des data frames sous forme de tableaux HTML (à la Jupyter Notebook).
//
// Principe :
//  1. Au démarrage de webR, du code R intercepte l'affichage des data frames / tibbles
//     (et de View()) et mémorise les objets affichés pendant l'exécution d'une cellule.
//  2. Après chaque exécution, ils sont convertis en JSON par R (qwebr_tables_json()) puis
//     rendus en tableaux HTML défilables (lignes et colonnes, en-tête et index fixes).
//  3. Le bouton « Tableau » de la cellule bascule entre la sortie texte et ces tableaux.
//     Option de cellule : `#| df-html: true` affiche directement les tableaux.

// Code R exécuté une fois au démarrage de webR
globalThis.qwebrDataframeViewR = String.raw`# Affichage des data frames sous forme de tableaux HTML (à la Jupyter).
#
# Ce code R est exécuté une fois au démarrage de webR (voir qwebr-dataframe-view.js).
#  - Il intercepte l'affichage des data frames / tibbles (print) et de View(), et
#    mémorise les objets affichés pendant l'exécution d'une cellule.
#    L'affichage texte habituel n'est pas modifié.
#  - qwebr_tables_json() les convertit en JSON pour que le navigateur les rende en HTML.
# View() et qwebr_tables_json() vivent dans un environnement attaché (« qwebr:tables ») :
# elles n'apparaissent pas dans ls() et ne polluent pas l'environnement global.
local({
  env <- new.env()

  stash <- function(x) {
    options(qwebr.tables = c(getOption("qwebr.tables"), list(x)))
    invisible(x)
  }

  # Depuis R 4.x, la recherche des méthodes S3 ignore les environnements attachés :
  # on instrumente donc les méthodes d'affichage existantes avec trace().
  # Chaque data frame affiché est mémorisé, puis affiché normalement.
  hook <- function(x) invisible(stash(x))
  tryCatch({
    try(suppressMessages(untrace("print.data.frame", where = baseenv())), silent = TRUE)
    suppressMessages(trace("print.data.frame", tracer = bquote(.(hook)(x)),
                           print = FALSE, where = baseenv()))
    # Les tibbles (dplyr, readr...) : méthode enregistrée qui se rabat sur pillar via NextMethod()
    registerS3method("print", "tbl_df", function(x, ...) {
      hook(x)
      NextMethod()
    }, envir = baseenv())
  }, error = function(e) NULL)

  # Équivalent de View() de RStudio : affiche le tableau sans sortie texte
  env$View <- function(x, title) {
    stash(if (is.data.frame(x)) x else as.data.frame(x))
    invisible(x)
  }

  env$qwebr_tables_json <- function(max_rows = 1000L, max_cols = 100L) {
    tables <- getOption("qwebr.tables")
    options(qwebr.tables = list())
    if (!length(tables)) return("[]")

    esc <- function(s) {
      s <- gsub("[[:cntrl:]]", " ", s)
      s <- gsub("\\", "\\\\", s, fixed = TRUE)
      gsub("\"", "\\\"", s, fixed = TRUE)
    }
    quote_json <- function(s) ifelse(is.na(s), "null", paste0("\"", esc(s), "\""))
    array_json <- function(s) paste0("[", paste(quote_json(s), collapse = ","), "]")

    # Mise en forme d'une colonne : NA conservé, nombres formatés comme print()
    fmt <- function(v) {
      out <- tryCatch({
        if (is.factor(v)) as.character(v)
        else if (is.character(v)) v
        else if (is.numeric(v)) format(v, digits = 7, trim = TRUE)
        else if (inherits(v, c("Date", "POSIXct"))) format(v)
        else if (is.list(v)) vapply(v, function(e) paste(format(e), collapse = ", "), "")
        else as.character(v)
      }, error = function(e) rep("?", length(v)))
      out <- as.character(out)
      out[is.na(v)] <- NA_character_
      out
    }
    type_of <- function(v) {
      if (is.factor(v)) "fct"
      else if (is.integer(v)) "int"
      else if (is.numeric(v)) "dbl"
      else if (is.character(v)) "chr"
      else if (is.logical(v)) "lgl"
      else if (inherits(v, "Date")) "date"
      else if (inherits(v, "POSIXct")) "dttm"
      else if (is.list(v)) "list"
      else class(v)[1]
    }

    one <- function(x) {
      nr <- nrow(x)
      nc <- ncol(x)
      shown <- as.data.frame(x[seq_len(min(nr, max_rows)), seq_len(min(nc, max_cols)), drop = FALSE])
      cols <- vapply(shown, function(v) array_json(fmt(v)), "")
      paste0(
        "{\"kind\":", quote_json(class(x)[1]),
        ",\"nrow\":", nr, ",\"ncol\":", nc,
        ",\"shown_rows\":", nrow(shown), ",\"shown_cols\":", ncol(shown),
        ",\"names\":", array_json(names(shown)),
        ",\"types\":", array_json(vapply(shown, type_of, "")),
        ",\"rownames\":", array_json(rownames(shown)),
        ",\"cols\":[", paste(cols, collapse = ","), "]}"
      )
    }

    paste0("[", paste(vapply(tables, one, ""), collapse = ","), "]")
  }

  if ("qwebr:tables" %in% search()) detach("qwebr:tables", character.only = TRUE)
  attach(env, name = "qwebr:tables", warn.conflicts = FALSE)
})
`;

// Installe les fonctions R (appelé par l'initialisation de webR)
globalThis.qwebrInstallDataframeView = async function () {
  try {
    await mainWebR.evalRVoid(qwebrDataframeViewR);
  } catch (e) {
    console.warn("qwebr : affichage des data frames indisponible", e);
  }
};

// Remet à zéro la liste des data frames mémorisés (avant d'exécuter une cellule)
globalThis.qwebrResetDataframes = async function () {
  try {
    await mainWebR.evalRVoid("options(qwebr.tables = list())");
  } catch (e) { /* sans effet sur la sortie texte */ }
};

// Récupère (au format JSON, puis objets JS) les data frames affichés par la cellule
globalThis.qwebrCollectDataframes = async function () {
  try {
    return JSON.parse(await mainWebR.evalRString("qwebr_tables_json()"));
  } catch (e) {
    return [];
  }
};

// Construit le bloc HTML d'un data frame
globalThis.qwebrBuildDataframeBlock = function (t) {
  const fr = (n) => n.toLocaleString("fr-FR");
  const block = document.createElement("div");
  block.className = "qwebr-df-block";

  // Ligne d'information : type, dimensions, éventuelle troncature
  const meta = document.createElement("div");
  meta.className = "qwebr-df-meta";
  const kind = document.createElement("span");
  kind.className = "qwebr-df-kind";
  kind.textContent = t.kind === "data.frame" ? "data.frame" : (t.kind === "tbl_df" ? "tibble" : t.kind);
  meta.appendChild(kind);
  meta.appendChild(document.createTextNode(` ${fr(t.nrow)} ligne${t.nrow > 1 ? "s" : ""} × ${fr(t.ncol)} colonne${t.ncol > 1 ? "s" : ""}`));
  if (t.shown_rows < t.nrow || t.shown_cols < t.ncol) {
    const note = document.createElement("span");
    note.className = "qwebr-df-note";
    const parts = [];
    if (t.shown_rows < t.nrow) parts.push(`${fr(t.shown_rows)} premières lignes`);
    if (t.shown_cols < t.ncol) parts.push(`${fr(t.shown_cols)} premières colonnes`);
    note.textContent = `Aperçu : ${parts.join(" et ")}`;
    meta.appendChild(note);
  }
  block.appendChild(meta);

  // Zone défilante (lignes et colonnes)
  const scroll = document.createElement("div");
  scroll.className = "qwebr-df-scroll";
  scroll.tabIndex = 0;
  const table = document.createElement("table");
  table.className = "qwebr-df";

  const numeric = t.types.map((ty) => ty === "int" || ty === "dbl");

  // En-tête : nom + type de chaque colonne
  const thead = document.createElement("thead");
  const headRow = document.createElement("tr");
  const corner = document.createElement("th");
  corner.className = "qwebr-df-idx";
  headRow.appendChild(corner);
  t.names.forEach((name, j) => {
    const th = document.createElement("th");
    if (numeric[j]) th.className = "qwebr-df-num";
    const label = document.createElement("div");
    label.className = "qwebr-df-name";
    label.textContent = name;
    const type = document.createElement("div");
    type.className = "qwebr-df-type";
    type.textContent = `<${t.types[j]}>`;
    th.append(label, type);
    headRow.appendChild(th);
  });
  thead.appendChild(headRow);
  table.appendChild(thead);

  // Corps du tableau
  const tbody = document.createElement("tbody");
  for (let i = 0; i < t.shown_rows; i++) {
    const tr = document.createElement("tr");
    const idx = document.createElement("th");
    idx.className = "qwebr-df-idx";
    idx.scope = "row";
    idx.textContent = t.rownames[i];
    tr.appendChild(idx);
    for (let j = 0; j < t.shown_cols; j++) {
      const td = document.createElement("td");
      const value = t.cols[j][i];
      if (numeric[j]) td.className = "qwebr-df-num";
      if (value === null) {
        td.classList.add("qwebr-df-na");
        td.textContent = "NA";
      } else {
        td.textContent = value;
        if (value.length > 30) td.title = value;
      }
      tr.appendChild(td);
    }
    tbody.appendChild(tr);
  }
  table.appendChild(tbody);

  scroll.appendChild(table);
  block.appendChild(scroll);
  return block;
};

// Bouton « Tableau » / « Texte » d'une cellule
globalThis.qwebrIsDataframeMode = function (id) {
  const area = document.getElementById(`qwebr-interactive-area-${id}`);
  return !!area && area.classList.contains("qwebr-df-mode");
};

globalThis.qwebrSetDataframeMode = function (id, on) {
  const area = document.getElementById(`qwebr-interactive-area-${id}`);
  const button = document.getElementById(`qwebr-button-table-${id}`);
  if (!area || !button) return;
  area.classList.toggle("qwebr-df-mode", on);
  button.title = on ? "Revenir à la sortie texte" : "Afficher les data frames sous forme de tableau";
  button.innerHTML = on
    ? '<i class="fa-solid fa-align-left"></i> <span>Texte</span>'
    : '<i class="fa-solid fa-table"></i> <span>Tableau</span>';
};

// Ajoute les tableaux à la sortie d'une cellule et met à jour le bouton
globalThis.qwebrShowDataframes = function (elements, options, tables, textIsEmpty) {
  const id = elements.id;
  const area = elements.outputCodeDiv.closest(".qwebr-interactive-area, .qwebr-noninteractive-area");
  const button = document.getElementById(`qwebr-button-table-${id}`);

  if (!tables.length) {
    if (area) area.classList.remove("qwebr-df-mode");
    if (button) {
      button.classList.add("qwebr-hidden");
      qwebrSetDataframeMode(id, false);
    }
    return;
  }

  const container = document.createElement("div");
  container.className = "qwebr-df-area";
  tables.forEach((t) => container.appendChild(qwebrBuildDataframeBlock(t)));
  elements.outputCodeDiv.appendChild(container);

  // Tableaux affichés d'emblée : option df-html, cellule sans sortie texte (View()),
  // ou l'utilisateur était déjà en mode tableau lors de l'exécution précédente
  const requested = options["df-html"] === "true" || options["df-html"] === true;
  const wasOn = qwebrIsDataframeMode(id);
  if (button) {
    button.classList.remove("qwebr-hidden");
    qwebrSetDataframeMode(id, requested || textIsEmpty || wasOn);
  } else if (area) {
    area.classList.toggle("qwebr-df-mode", requested || textIsEmpty);
  }
};
