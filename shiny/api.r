library(data.table)

infractionsFile <- "data/infractions.csv"
agentsFile <- "data/agents.csv"

infractions <- fread(infractionsFile)
infractions$type <- factor(infractions$type,
                           levels = c("Unexcused Absence",
                                      "Tardy",
                                      "Left Early",
                                      "Long Break"))
infractions$date <- as.character(infractions$date)
infractions$agent <- as.character(infractions$agent)
agents <- as.data.table(read.csv(agentsFile))


getInfractions <- function(name) {
  return(infractions %>% filter(agent == name))
}

getThreeMonths <- function(name) {
  return(infractions %>% filter(agent == name, as.Date(date) > (Sys.Date() - 90)))
}

getAllInfractions <- function() {
  return(infractions)
}

getNames <- function() {
  return(agents$agents)
}

getTypes <- function() {
  return(levels(infractions$type))
}

addAgent <- function(name) {
  agents <<- rbind(agents, list(name))
  write.csv(agents, agentsFile, row.names = FALSE)
}

addInfraction <- function(type, date, agent) {
  infractions <<- rbind(infractions, list(type, as.character(date), agent))
  fwrite(infractions, infractionsFile)
}

deleteInfraction <- function(rownum) {
  infractions <<- infractions[-rownum,]
  fwrite(infractions, infractionsFile)
}

deleteAgent <- function(a_name) {
  agents <<- agents %>% filter(agents != a_name)
  infractions <<- infractions %>% filter(agent != a_name)
  write.csv(agents, agentsFile, row.names = FALSE)
  fwrite(infractions, infractionsFile)
}

