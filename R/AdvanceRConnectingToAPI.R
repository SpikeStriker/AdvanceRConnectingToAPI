#' @name AdvanceRConnectingToAPI-class
#' @title A Reference Class to pull data for Sweden economy from Kolada.
#' @description This class extracts KPI data from kolada across multiple municipality and/or years. 
#' @field kpi List. Captures the list of codes of one or more KPI for which the data is to be extracted.
#' @field year List. Captures the list of one or more year for which the data is to be extracted.
#' @field municipality List. Captures the list of codes of one or more municipality for which the data is to be extracted.
#' @field searchText Character. String to be used as search string when identifying KPI(s) or municipality(ies).
#' @field searchTextIsKPI Logical. (Default=TRUE). FALSE=Municipality Search, TRUE=KPI search. Used only when searchText is specified. 
#' @field sweText Logical. (Default=TRUE). TRUE when search string is provided in Swedish language but FALSE when provided in English (takes longer to process). Available only for KPIs. Note: Translation is done using "mymemory API". Respective restrictions apply.
#' @field url Character. The URL that was used to fetch the data.
#' @field fetchedData Data.frame The fetched data stored in a structred data frame.
#' @field status Numeric. The http request response code.
#' @import methods
#' @import httr
#' @import jsonlite
#' @import tidyr
#' @import dplyr
#' @importFrom methods setClass setMethod new
#' @export AdvanceRConnectingToAPI
#' @examples
#' \dontrun{
#'   apiCallKolada <- AdvanceRConnectingToAPI$new(kpi = list("N25026"), year = list("2020", "2022"))
#'   apiCallKolada$summaryStatsData()
#' }
#' 

AdvanceRConnectingToAPI <- setRefClass("AdvanceRConnectingToAPI",
                             fields=list(
                               kpi="list",
                               year="list",
                               municipality="list",
                               searchText="character",
                               searchTextIsKPI="logical",
                               sweText="logical",
                               url="character",
                               fetchedData="data.frame",
                               status="numeric"
                             ),
                             methods = list(
                               initialize=function(kpi=NULL,year=NULL,municipality=NULL,searchText=NULL,searchTextIsKPI=TRUE,sweText=TRUE){
                                 .self$kpi<<-list(kpi)
                                 .self$year<<-list(year)
                                 .self$municipality<<-list(municipality)
                                 .self$searchText<<-as.character(searchText)
                                 .self$searchTextIsKPI<<-as.logical(searchTextIsKPI)
                                 .self$sweText<<-as.logical(sweText)
                                 cntInputs<-sum(is.null(kpi),is.null(year),is.null(municipality))
                                 if((cntInputs!=1)&&(is.null(searchText))){
                                   stop("Either two fields (KPI, Municipality or Year) or Search Text must be provided.")
                                 }else if((cntInputs==1)&&(!is.null(searchText))){
                                   stop("Either two fields (KPI, Municipality or Year) or Search Text must be provided.")
                                 }
                                 if(cntInputs==1){
                                   url <<- "http://api.kolada.se/v2/data"
                                   if(!is.null(kpi)){url<<-paste0(url,"/kpi/",paste(kpi,collapse=","))}
                                   if(!is.null(municipality)){url<<-paste0(url,"/municipality/",paste(municipality,collapse=","))}
                                   if(!is.null(year)){url<<-paste0(url,"/year/",paste(year,collapse=","))}
                                   status<<-status_code(GET(url))
                                   data <- content(GET(url), "text")
                                   fetchedDatalist<-fromJSON(data)$values
                                   if(length(fetchedDatalist)==0){
                                     cat("No valid data fetched. Diagnostics Required")
                                   }else{
                                     fetchedData<<-as.data.frame(flatten(fetchedDatalist))
                                     fetchedData<<-unnest(fetchedData, cols = c(values))
                                   }
                                 }else if(!is.null(searchText)){
                                   url <<- "http://api.kolada.se/v2"
                                   if(searchTextIsKPI){
                                     if(sweText){
                                       url<<-paste0(url,"/kpi?title=",URLencode(searchText))
                                     }else{
                                       translation<-function(textTranslate,langp="en|swe"){
                                         transUrl <- "https://api.mymemory.translated.net/get"
                                         queryString <- list(langpair=langp,q=textTranslate,mt="1",onlyprivate="0",de="a@b.c")
                                         response <- VERB("GET", transUrl,query = queryString)
                                         transDf<-fromJSON(content(response,"text",encoding="UTF-8"))
                                         if(length(transDf$matches)<=10){
                                           return(textTranslate)
                                         }else{
                                           transDf<-as.data.frame(flatten(transDf$matches))
                                           transDf<-transDf[transDf$quality>=max(transDf$quality),]
                                           transDf<-transDf[transDf$match>=max(transDf$match),]
                                           return(transDf$translation[1])}
                                       }
                                       url<<-paste0(url,"/kpi?title=",URLencode(translation(searchText)))
                                     }
                                   }else{
                                     url<<-paste0(url,"/municipality?title=",URLencode(searchText))
                                   }
                                   status<<-status_code(GET(url))
                                   data <- content(GET(url), "text")
                                   
                                   fetchedDatalist<-fromJSON(data)$values
                                   if(length(fetchedDatalist)==0){
                                     cat("No valid data fetched. Diagnostics Required")
                                   }else{
                                     fetchedData<<-as.data.frame(flatten(fetchedDatalist))
                                   }
                                 }
                                 cat("URL used to Fetch Data:",url)
                               },
                               summaryStatsData = function(self.fetchedData){
                                 if(!is.null(fetchedData)){
                                   if(colnames(fapi$fetchedData)[1]=="kpi"){
                                     cat("Summary Statistics of Value Column")
                                     print(fetchedData %>%
                                             group_by(kpi, municipality, period,gender,status) %>%
                                             summarise(summary = list(summary(value))) %>%
                                             unnest_wider(summary))
                                     
                                     # summary(fapi$fetchedData$value)
                                     cat("First 2 rows of fetched Data: \n")
                                     head(fetchedData,2)}else{
                                       cat("Available only for data requests not when searching metadata")
                                     }
                                 }else{
                                   cat("No data was fetched")
                                 }
                                 
                               }
                             )
)
