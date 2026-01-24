# ============================================================================
# export_docs.R
# Exporta la red general interactiva a /docs para pagina web
# ============================================================================

cat("📦 Exportando docs...\n")

docs_dir <- "docs"
dir.create(docs_dir, showWarnings = FALSE, recursive = TRUE)

src_html <- "outputs/figures/red_general_todos_interactiva.html"
dst_html <- file.path(docs_dir, "red_general_todos_interactiva.html")

if (file.exists(src_html)) {
  file.copy(src_html, dst_html, overwrite = TRUE)
  cat("✅ HTML copiado:", dst_html, "\n")
} else {
  cat("⚠️  No se encontro:", src_html, "\n")
}

src_assets <- "outputs/figures/red_general_todos_interactiva_files"
dst_assets <- file.path(docs_dir, "red_general_todos_interactiva_files")

if (dir.exists(src_assets)) {
  dir.create(dst_assets, showWarnings = FALSE, recursive = TRUE)
  files <- list.files(src_assets, recursive = TRUE, full.names = TRUE)
  if (length(files) > 0) {
    rel_paths <- sub(paste0("^", src_assets, "/"), "", files)
    for (i in seq_along(files)) {
      src <- files[i]
      rel <- rel_paths[i]
      dst <- file.path(dst_assets, rel)
      dir.create(dirname(dst), showWarnings = FALSE, recursive = TRUE)
      file.copy(src, dst, overwrite = TRUE)
    }
    cat("✅ Assets copiados:", dst_assets, "\n")
  }
} else {
  cat("⚠️  No se encontro:", src_assets, "\n")
}

index_path <- file.path(docs_dir, "index.html")
index_lines <- c(
  "<!DOCTYPE html>",
  "<html lang=\"es\">",
  "<head>",
  "  <meta charset=\"utf-8\" />",
  "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />",
  "  <title>familiaRes | Red General</title>",
  "  <style>",
  "    body { font-family: Arial, sans-serif; margin: 0; padding: 0; }",
  "    header { padding: 16px 24px; background: #111827; color: #fff; }",
  "    main { padding: 16px 24px; }",
  "    iframe { width: 100%; height: 900px; border: none; }",
  "    .note { color: #555; margin-top: 8px; }",
  "  </style>",
  "</head>",
  "<body>",
  "  <header>",
  "    <h1>familiaRes</h1>",
  "    <p>Red general de personajes y familias (Chile)</p>",
  "  </header>",
  "  <main>",
  "    <h2>Red interactiva</h2>",
  "    <iframe src=\"red_general_todos_interactiva.html\"></iframe>",
  "    <p class=\"note\">Si no carga, verifica que los assets esten en la carpeta docs.</p>",
  "  </main>",
  "</body>",
  "</html>"
)
writeLines(index_lines, index_path, useBytes = TRUE)
cat("✅ Index creado:", index_path, "\n")

cat("✅ Docs exportados.\n")
