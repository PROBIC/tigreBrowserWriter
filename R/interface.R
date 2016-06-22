initializeDb <- function(dbPath, datasetName, datasetSpecies='', datasetSource='', datasetPlatform='', datasetSaveLocation='', datasetFigureFile='') {
    db <- .openConnection(dbPath)
    createTables(db)
    datasetId <- .addAndGetDatasetId(db, datasetName, datasetSpecies, datasetSource, datasetPlatform, datasetDescription, datasetSaveLocation, datasetFigureFilename)
    return list(db=db, datasetId=datasetId, experimentIds=list(),
                regulatorIds=list())
}

insertAliases <- function(db, aliasType, aliases, aliasSource='', aliasDescription='') {
    alias_id <- .addAndGetAliasId(db$db, db$datasetId, aliasType, aliasSource, aliasDescription)

    gene_ids <- .addAndGetProbeGeneIds(db$db, names(aliases))

    dbBeginTransaction(db$db)
    for (i in seq_along(gene_ids[[1]])) {
        probe <- gene_ids[i, 1]
        gene_id <- gene_ids[i, 2]
        .addGeneAliases(db$db, alias_id, gene_id, list(probe=aliases[probe]))
    }
    dbCommit(db$db)
}

insertResults <- function(db, experimentName, regulatorName, figurePath, loglikelihoods, baselineloglikelihoods=NA, experimentDesc='', loopVariable=2, modelTranslation=FALSE, numberOfParameters=NA, parameterNames=NA, experimentProducer='', experimentTimestamp='', parameters=NA) {
    regId <- .addAndGetRegulatorId(db$db, regulatorName, db$datasetId)
    experimentId <- .addAndGetExperimentId(db$db, experimentName, experimentDesc, db$datasetId, regulatorId=regId, loopVariable=loopVariable, modelTranslation=modelTranslation, numberOfParameters=numberOfParameters, parameterNames=parameterNames, producer=experimentProducer, timestamp=experimentTimestamp)
    .addResults(db$db, experimentId, loglikelihoods, baseloglikelihoods, parameters)
    db$experimentIds$experimentName <- experimentId
    return (db)
}

insertFigures <- function(db, experimentName, regulatorName, filename, name='', description='', priority=0) {
    tryCatch(experimentId <- db$experimentIds$experimentName,
             stop("Insert results for experiment before inserting figures."))
    .addFigures(db$db, experimentId, filename=filename, name=name, description=description, priority=priority, figureData=NULL)
    return (db)
}

insertSupplementaryData <- function(db, name, suppData, regulatorName=NA, source='', platform='', description='') {
    if (!is.na(regulatorName)) {
        regulatorId <- .addAndGetRegulatorId(db$db, regulatorName, db$datasetId)
    } else {
        regulatorId <- NA
    }
    if (is.logical(suppData)) {
        type = 0
    } else if (is.factor(suppData)) {
        type = 1
    } else {
        type = 2
    }
    .addAndGetSupplementaryDataId(db$db, name, regulatorId, type, source, platform, description)
    .addSupplementaryData(db$db, suppDatasetId, suppData)    
    return (db)
}

insertZScores <- function(db, zscores) {
    .insertZscores(db$db, db$datasetId, zscores)
}
