# app/server.R

server <- function(input, output, session) {
  for (nm in names(DATA_FILES)) {
    local({
      n  <- nm
      mt <- DATASET_INFO[name == n, metric][1L]
      pair_server(n, metric = mt)   # data loaded lazily inside the module
    })
  }
}
