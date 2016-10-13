production_queries <- RJSONIO::fromJSON(
  '~/Projects/gloograph/inst/exdata/production_queries.JSON'
)

devtools::use_data(production_queries
                   , production_queries
                   , overwrite = T)
