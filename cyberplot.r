library("httr")
library("data.table")
library("jsonlite")

DEFAULT_URL <- "127.0.0.1"
PORT <- "5000"
UPLOAD_PATH <- "/api/dataset_upload/"

cyberplot.new <- function(dataTable, id, name, serverUrl, matrix = FALSE) {
    if(missing(name)) {
        print("Please specify dataset name.")
    }

    cyberplot.__upload(dataTable, id, name, serverUrl, 0, matrix)
}

cyberplot.update <- function(dataTable, id, serverUrl, matrix = FALSE) {
    cyberplot.__upload(dataTable, id, NULL, serverUrl, 1, matrix)
}

cyberplot.__upload <- function(dataTable, id, name, serverUrl, updating, matrix) {
    if(missing(id)) {
        print("Please specify identifier.")
        return
    }

    dataTableTemp <- dataTable
    
    datasetType <- "multivariate"
    containsHeader <- 1
    if(matrix) {
        datasetType <- "matrix"
        colnames(dataTableTemp) <- NULL
        containsHeader <- 0
    }
    else {
        # if data table contains row labels, populate a new column with them
        if(rownames(dataTable)[1] != 1) {
            dataTableTemp$Label <- rownames(dataTable)
        }
    }

    dataTableTemp <- dataTableTemp[, c(ncol(dataTableTemp), 1:(ncol(dataTableTemp) - 1))]
    
    dataFile <- tempfile("data")
    fwrite(dataTableTemp, dataFile)

    usedUrl <- if(missing(serverUrl)) DEFAULT_URL else serverUrl
    usedUrl <- paste(usedUrl, PORT, sep = ":")
    usedUrl <- paste(usedUrl, UPLOAD_PATH, sep = "")

    metadata <- list(json = list(name = name, type = datasetType, identifier = id, containsHeader = containsHeader, updating = updating))
    metadataJson <- jsonlite::toJSON(metadata, pretty = TRUE, auto_unbox = TRUE)

    req <- POST(usedUrl,
        body = list(
            json = metadataJson,
            file = upload_file(dataFile, type = "application/octet-stream")
        ),
        add_headers("Content-Type" = "multipart/form-data")
    )

    unlink(dataFile)

    status <- req["status_code"]
    if(status == 201) {
        print("Dataset uploaded.")
    }
    else {
        print(content(req, as="parsed")["result"])
    }
}

cyberplot.new(iris, id = "cdc7b22590c2e861ee5390ac5285acb0", name = "Iris Dataset")