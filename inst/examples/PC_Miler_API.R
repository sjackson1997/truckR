
PC_MILER_API_KEY <- Sys.getenv("PC_MILER_API_KEY")

base_url <- "https://pcmiler.alk.com/apis/rest/v1.0/Service.svc/route/routeReports"


# GET Examples ----
req <- c(
  "?stops=-75.173297%2C39.942892%3B-74.83153%2C39.61703%3B-74.438942%2C39.362469&reports=Mileage",
  "?stops=-75.173297%2C39.942892%3B-74.83153%2C39.61703%3B-74.438942%2C39.362469&reports=Detail",
  "?stops=-75.173297%2C39.942892%3B-74.83153%2C39.61703%3B-74.438942%2C39.362469&reports=CalcMiles"
)

lst_out <- list()

for (i in seq_along(req)) {
  THE_REQUEST <- paste0(base_url, req[i])

  THE_RESPONSE <- httr::GET(THE_REQUEST, httr::add_headers(Authorization = PC_MILER_API_KEY))
  THE_RESPONSE
  THE_RESPONSE$content
  lst_out <- append(lst_out, httr::content(THE_RESPONSE))
}

lst_out

# Route Reports (POST) -----

route_url_post <- "https://pcmiler.alk.com/apis/rest/v1.0/Service.svc/route/routeReports?dataVersion=Current"

# Example #1 ----
# df_lanes <- readRDS("Data/pcmiler/sample_lanes2.rds")
df_lanes <- readRDS("C:/Users/KQV2764/OneDrive - MDLZ/AdvancedDataAnalyst/Projects_in_R/Resource_Files/Data/pcmiler/sample_lanes2.rds")
# df_lanes <- read_csv("Data/pcmiler/Kenny.csv",
# 										 col_names = TRUE,
# 										 locale = locale(encoding = "latin1"))
#
# df_lanes %>%
# 	as_tibble() %>%
# 	print(n=30)







# Function to convert the complex list structure into a dataframe

# Example usage with your list (assuming it's stored in a variable named 'report_list')
