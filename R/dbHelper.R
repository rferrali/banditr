#' @export

bulkInsert <- function(conn, df, table, path, bulk = NULL, keep = FALSE, query = NULL) {
  local_fpath <- paste0(path, "pivot")
  cols_table <- tryCatch(RODBC::sqlColumns(conn, table)$COLUMN_NAME,
                         error = function(e) {
                           stop(paste0("Table ",
                                       table, " not found."))
                         })
  if(any(!colnames(df) %in% cols_table)) stop(paste0("Erreur d'insertion: des colonnes du data.frame R n'ont pas ete trouvees dans la table ",
                                                     table, "."))
  if(any(!cols_table %in% colnames(df))) stop(paste0("Erreur d'insertion: des colonnes de la table ",
                                                     table, " n'ont pas ete trouvees dans le data.frame R."))
  df <- df[,cols_table]
  if (nrow(df) == 0) return()
  write.table(df, local_fpath, sep = "\t", row.names = F, quote = F, col.names = F, na = "")
  n_orig <- RODBC::sqlQuery(conn, paste0("SELECT COUNT(*) AS c FROM ", table))$c

  if(is.null(query)) query <- "BULK INSERT %s FROM '%s'
                                      WITH
                                      (FIELDTERMINATOR = '\\t',
                                      ROWTERMINATOR = '\\n',
                                      CODEPAGE = 'ACP')"
  query <- sprintf(query, table, normalizePath(local_fpath))
  RODBC::sqlQuery(conn, query)
  # sqlQuery(conn, "EXEC xp_cmdshell 'net use H: /delete';")
  n_final <- RODBC::sqlQuery(conn, paste0("SELECT COUNT(*) AS c FROM ", table))$c
  if(n_orig + nrow(df) != n_final) stop(paste0("Erreur d'insertion dans la table ", table,
                                               ". Verifier que les types de donnees sont connformes et que les conntraintes table sont satisfaites. Fichier pivot : ",
                                               local_fpath))
  if(!keep) file.remove(local_fpath)
}
