library(paws.storage)
library(readr)
library(httr2)


r2_client <- function() {
  paws.storage::s3(config = list(
    credentials = list(creds = list(
      access_key_id     = Sys.getenv("R2_ACCESS_KEY_ID"),
      secret_access_key = Sys.getenv("R2_SECRET_ACCESS_KEY")
    )),
    endpoint = paste0("https://", Sys.getenv("R2_ACCOUNT_ID"), ".r2.cloudflarestorage.com"),
    region   = "auto"
  ))
}

upload_csv_to_r2 <- function(df, key, bucket = "cra-chart-data",
                             cache_control = "public, max-age=900") {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  write_csv(df, tmp)

  r2_client()$put_object(
    Bucket       = bucket,
    Key          = key,
    Body         = readBin(tmp, "raw", n = file.size(tmp)),
    ContentType  = "text/csv",
    CacheControl = cache_control
  )
  message("Uploaded: ", key)
  invisible(TRUE)
}

upload_geojson_to_r2 <- function(sf_obj, key, bucket = "cra-chart-data",
                                 cache_control = "public, max-age=900") {
  tmp <- tempfile(fileext = ".geojson")
  on.exit(unlink(tmp))

  sf_obj <- st_transform(sf_obj, 4326)   # GeoJSON should be WGS84 lon/lat

  st_write(
    sf_obj, tmp,
    driver        = "GeoJSON",
    delete_dsn    = TRUE,
    quiet         = TRUE,
    layer_options = c("RFC7946=YES", "COORDINATE_PRECISION=5")
  )

  r2_client()$put_object(
    Bucket       = bucket,
    Key          = key,
    Body         = readBin(tmp, "raw", n = file.size(tmp)),
    ContentType  = "application/geo+json",
    CacheControl = cache_control
  )
  message("Uploaded: ", key)
  invisible(TRUE)
}