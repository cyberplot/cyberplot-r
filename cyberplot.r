library("httr")
library("data.table")
library("jsonlite")

DEFAULT_URL <- "127.0.0.1"
PORT <- "5000"
UPLOAD_PATH <- "/api/upload/"

cyberplot.new <- function(dataTable, id, name, serverUrl = NULL) {
    dataTableTemp <- dataTable

    # if data table contains row labels, populate a new column with them
    if(rownames(dataTable)[1] != 1) {
        dataTableTemp$Label <- rownames(dataTable)
        dataTableTemp <- dataTableTemp[, c(ncol(dataTableTemp), 1:(ncol(dataTableTemp) - 1))]
    }

    dataFile <- tempfile("data")
    fwrite(dataTableTemp, dataFile)

    usedUrl <- if(missing(serverUrl)) DEFAULT_URL else serverUrl
    usedUrl <- paste(usedUrl, PORT, sep = ":")
    usedUrl <- paste(usedUrl, UPLOAD_PATH, sep = "")

    metadata <- list(json = list(name = name, identifier = id))
    metadataJson <- jsonlite::toJSON(metadata, pretty = TRUE, auto_unbox = TRUE)

    req <- POST(usedUrl,
        body = list(
            json = metadataJson,
            file = upload_file(dataFile, type = "application/octet-stream")
        ),
        add_headers("Content-Type" = "multipart/form-data") #, verbose()
    )

    unlink(dataFile)

    status <- req["status_code"]
    if(status == 201) {
        print("Dataset uploaded.")
    }
    else {
        print("General upload error.")
    }
}

cyberplot.new(swiss, id = "346f9f436c0d99fbb937658af47a09f9", name = "Swiss")
