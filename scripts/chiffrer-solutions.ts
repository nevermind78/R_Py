// Chiffre les solutions dans les pages générées (version étudiants).
//
// Exécuté automatiquement par Quarto après chaque génération (project > post-render).
//  - Lit les solutions déposées par filters/solutions.lua dans .solutions-src/
//  - Les chiffre avec le code de la séance (AES-GCM, clé dérivée par PBKDF2-SHA256)
//  - Les insère dans les emplacements <div class="solution-verrou"> des pages HTML
//
// Les codes ne sont JAMAIS écrits dans le site. Ils sont lus dans :
//   1. le fichier local  codes-solutions.txt   (une ligne par séance :  r-seance-3: mon code)
//   2. ou les variables d'environnement  SOLUTIONS_CODE_R_SEANCE_3  (pratique pour la CI)
// Sans code pour une séance, ses solutions restent indisponibles (jamais en clair).
//
// Générer des codes solides :   quarto run scripts/chiffrer-solutions.ts generer

const ITERATIONS = 600_000; // doit rester cohérent avec _includes/solutions.html (lu dans data-iter)
const LONGUEUR_MIN = 10;
const SRC_DIR = ".solutions-src";
const CODES_FILE = "codes-solutions.txt";

const MOTS = ("tortue violet marteau nuage citron pirate jardin cheval orange fusee lampe voilier rocher cerise " +
  "panier moulin cactus etoile fenetre ballon sable ruban bougie dragon miroir savon tambour valise pomme " +
  "renard bateau glacier piano canard tunnel radis cuisine foret carotte montagne bonbon clavier sirop " +
  "poisson brique tableau planete couteau chapeau papier crayon camion ourson banane lion herisson " +
  "magnolia zebre olive tomate poivre chocolat biscuit bijou cloche echelle escargot gomme hibou igloo " +
  "jumelles koala lunette meteore navire oiseau parapluie quartz robot sardine tapis usine volcan wagon " +
  "yaourt zeppelin abeille buffle cerf dauphin epingle flamant girafe hamac iris jonquille kiwi lavande " +
  "mouton noisette oignon perle quenelle rideau sapin tulipe").split(" ");

// Même normalisation que dans le navigateur : la casse et les espaces autour sont sans importance
const normaliser = (code: string) => code.normalize("NFC").trim().toLowerCase();

function base64(octets: Uint8Array): string {
  let s = "";
  for (let i = 0; i < octets.length; i += 0x8000) {
    s += String.fromCharCode(...octets.subarray(i, i + 0x8000));
  }
  return btoa(s);
}

function aleatoire(n: number): number {
  const a = new Uint32Array(1);
  crypto.getRandomValues(a);
  return a[0] % n;
}

function genererCode(): string {
  const mots = Array.from({ length: 4 }, () => MOTS[aleatoire(MOTS.length)]);
  return `${mots.join("-")}-${10 + aleatoire(90)}`;
}

function lireCodes(): Record<string, string> {
  const codes: Record<string, string> = {};
  try {
    for (const ligne of Deno.readTextFileSync(CODES_FILE).split(/\r?\n/)) {
      if (ligne.trim().startsWith("#")) continue;
      const m = ligne.match(/^\s*([\w-]+)\s*[:=]\s*(.+?)\s*$/);
      if (m) codes[m[1]] = m[2];
    }
  } catch { /* fichier absent : on utilise les variables d'environnement */ }
  return codes;
}

function codePour(page: string, codes: Record<string, string>): string | undefined {
  const env = Deno.env.get("SOLUTIONS_CODE_" + page.toUpperCase().replace(/[^A-Z0-9]/g, "_"));
  return codes[page] ?? env;
}

async function deriverCle(code: string, sel: Uint8Array): Promise<CryptoKey> {
  const base = await crypto.subtle.importKey("raw", new TextEncoder().encode(normaliser(code)), "PBKDF2", false, ["deriveKey"]);
  return crypto.subtle.deriveKey(
    { name: "PBKDF2", salt: sel, iterations: ITERATIONS, hash: "SHA-256" },
    base,
    { name: "AES-GCM", length: 256 },
    false,
    ["encrypt"],
  );
}

async function chiffrerPage(chemin: string, codes: Record<string, string>): Promise<void> {
  let html = await Deno.readTextFile(chemin);
  const re = /<div class="solution-verrou" data-page="([^"]+)" data-sol="([^"]+)" data-titre="([^"]*)"><\/div>/g;
  const trouves = [...html.matchAll(re)];
  if (!trouves.length) return;

  const page = trouves[0][1];
  const code = codePour(page, codes);
  if (!code) {
    console.warn(`  ⚠ ${page} : aucun code défini -> ${trouves.length} solution(s) indisponible(s) (rien n'est publié en clair)`);
    return;
  }
  if (normaliser(code).length < LONGUEUR_MIN) {
    console.warn(`  ⚠ ${page} : code trop court (< ${LONGUEUR_MIN} caractères) -> solutions indisponibles. Générez-en un : quarto run scripts/chiffrer-solutions.ts generer`);
    return;
  }

  const sel = crypto.getRandomValues(new Uint8Array(16));
  const cle = await deriverCle(code, sel);
  let n = 0;
  const remplacements = new Map<string, string>();
  for (const m of trouves) {
    const id = m[2];
    let clair: string;
    try {
      clair = await Deno.readTextFile(`${SRC_DIR}/${id}.html`);
    } catch {
      console.warn(`  ⚠ ${id} : fichier source introuvable (regénérez le site)`);
      continue;
    }
    const iv = crypto.getRandomValues(new Uint8Array(12));
    const chiffre = new Uint8Array(
      await crypto.subtle.encrypt({ name: "AES-GCM", iv }, cle, new TextEncoder().encode(clair)),
    );
    remplacements.set(
      m[0],
      m[0].replace(
        "></div>",
        ` data-iter="${ITERATIONS}" data-salt="${base64(sel)}" data-iv="${base64(iv)}" data-ct="${base64(chiffre)}"></div>`,
      ),
    );
    n++;
  }
  for (const [ancien, nouveau] of remplacements) html = html.replace(ancien, () => nouveau);
  await Deno.writeTextFile(chemin, html);
  console.log(`  ✔ ${page} : ${n} solution(s) chiffrée(s)`);
}

async function main() {
  if (Deno.args[0] === "generer") {
    console.log("# À copier dans codes-solutions.txt (fichier ignoré par git). Un code par séance :");
    for (let n = 1; n <= 10; n++) console.log(`r-seance-${n}: ${genererCode()}`);
    return;
  }

  const sortie = Deno.env.get("QUARTO_PROJECT_OUTPUT_DIR") ?? "docs";
  const codes = lireCodes();
  console.log("Chiffrement des solutions...");
  let vu = false;
  try {
    for await (const f of Deno.readDir(sortie)) {
      if (f.isFile && f.name.endsWith(".html")) {
        vu = true;
        await chiffrerPage(`${sortie}/${f.name}`, codes);
      }
    }
  } catch (e) {
    console.warn(`  ⚠ dossier de sortie introuvable (${sortie}) : ${e}`);
  }
  if (!vu) console.log("  (aucune page à traiter)");
}

await main();
