-- Solutions verrouillées.
--
-- Les blocs  :::: {.corrige} ... ::::  contiennent une solution (généralement un encadré
-- « Solution » repliable). Deux modes, choisis par la métadonnée `solutions` :
--
--   visibles      (profil « enseignant ») : rien n'est modifié, les solutions s'affichent.
--   verrouillees  (profil « etudiant »)   : le texte de la solution n'apparaît JAMAIS dans
--                 la page. Il est écrit dans .solutions-src/ (dossier local, ignoré par git
--                 et hors du site), et la page reçoit un emplacement vide. Le script
--                 scripts/chiffrer-solutions.ts chiffre ensuite chaque solution avec le code
--                 de la séance et l'insère dans la page ; le navigateur ne la déchiffre que
--                 si l'étudiant saisit le bon code.
--   Si le script ne s'exécute pas, l'emplacement reste vide : aucune fuite possible.

local SRC_DIR = ".solutions-src"

local function escape_attr(s)
  return (s:gsub("&", "&amp;"):gsub('"', "&quot;"):gsub("<", "&lt;"):gsub(">", "&gt;"))
end

function Pandoc(doc)
  local mode = pandoc.utils.stringify(doc.meta.solutions or "verrouillees")
  if mode == "visibles" then
    return nil
  end

  -- Quarto fournit à Pandoc un fichier temporaire : on prend le vrai nom du document
  local input = (quarto and quarto.doc and quarto.doc.input_file) or PANDOC_STATE.input_files[1] or "page"
  local normalise = input:gsub(string.char(92), "/")  -- chemins Windows -> "/"
  local page = normalise:match("([^/]+)%.%w+$") or "page"
  local count = 0

  local blocks = doc.blocks:walk({
    Div = function(div)
      if not div.classes:includes("corrige") then
        return nil
      end
      count = count + 1

      -- Si le bloc ne contient qu'un encadré (callout), on en extrait le titre et le corps.
      -- À ce stade, Quarto a déjà converti l'encadré en nœud interne : un Div dont le
      -- premier enfant est le titre et le second le corps.
      local title = "Solution"
      local content = div.content
      if #content == 1 and content[1].t == "Div" then
        local inner = content[1]
        if inner.attributes["__quarto_custom_type"] == "Callout" and #inner.content >= 2 then
          title = pandoc.utils.stringify(inner.content[1]):gsub("^%s+", ""):gsub("%s+$", "")
          content = inner.content[2].content
        elseif inner.attributes["title"] then
          title = inner.attributes["title"]
          content = inner.content
        end
      end

      pandoc.system.make_directory(SRC_DIR, true)
      local id = page .. "-" .. count
      local file = assert(io.open(SRC_DIR .. "/" .. id .. ".html", "w"))
      -- identifier_prefix : évite des identifiants (cb1...) en double avec le reste de la page
      file:write(pandoc.write(pandoc.Pandoc(content), "html", { identifier_prefix = id .. "-" }))
      file:close()

      return pandoc.RawBlock("html", string.format(
        '<div class="solution-verrou" data-page="%s" data-sol="%s" data-titre="%s"></div>',
        escape_attr(page), escape_attr(id), escape_attr(title)))
    end
  })

  return pandoc.Pandoc(blocks, doc.meta)
end
