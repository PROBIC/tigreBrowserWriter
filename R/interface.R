# tigrebrowserWriter user-facing API code
# Copyright (C) 2016 Antti Honkela
#
# Portions copied from tigre Bioconductor package
# Copyright (C) 2010 Miika-Petteri Matikainen, Antti Honkela and
# Aalto University
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

initializeDb <- function(dbPath, datasetName, datasetSpecies='', datasetSource='', datasetPlatform='', datasetDescription='', datasetSaveLocation='', datasetFigureFilename='') {
    db <- .openConnection(dbPath)
    .createTables(db)
    datasetId <- .addAndGetDatasetId(db, datasetName, datasetSpecies, datasetSource, datasetPlatform, datasetDescription, datasetSaveLocation, datasetFigureFilename)
    return (list(db=db, datasetId=datasetId, datasetName=datasetName,
                 experimentIds=list(), regulatorIds=list()))
}

insertAliases <- function(db, aliasType, aliases, aliasSource='', aliasDescription='') {
    alias_id <- .addAndGetAliasId(db$db, db$datasetId, aliasType, aliasSource, aliasDescription)

    gene_ids <- .addAndGetProbeGeneIds(db$db, names(aliases))

    dbBegin(db$db)
    for (i in seq_along(gene_ids[[1]])) {
        probe <- gene_ids[i, 1]
        gene_id <- gene_ids[i, 2]
        .addGeneAliases(db$db, alias_id, gene_id, list(probe=aliases[probe]))
    }
    dbCommit(db$db)
    return (db)
}

insertResults <- function(db, experimentName, regulatorName, figurePath, loglikelihoods, baselineloglikelihoods=NA, experimentDesc='', loopVariable=2, modelTranslation=FALSE, numberOfParameters=NA, parameterNames=NA, experimentProducer='', experimentTimestamp='', parameters=NA) {
    if (experimentDesc == '')
        experimentDesc <- experimentName
    regId <- .addAndGetRegulatorId(db$db, regulatorName, db$datasetId)
    experimentId <- .addAndGetExperimentId(db$db, experimentName, experimentDesc, db$datasetId, regulatorId=regId, loopVariable=loopVariable, modelTranslation=modelTranslation, numberOfParameters=numberOfParameters, parameterNames=parameterNames, producer=experimentProducer, timestamp=experimentTimestamp)
    .addResults(db$db, experimentId, loglikelihoods, baselineloglikelihoods, parameters)
    db$experimentIds[[experimentName]] <- experimentId
    return (db)
}

insertFigures <- function(db, experimentName, regulatorName, filename, name='', description='', priority=0) {
    tryCatch(experimentId <- db$experimentIds[[experimentName]],
             error = function(e) stop("Insert results for experiment before inserting figures."))
    .addFigures(db$db, experimentId, filename=filename, name=name, description=description, priority=priority, figureData=NULL)
    return (db)
}

insertSupplementaryData <- function(db, name, suppData, regulatorName=NA, source='', platform='', description='') {
    if (!is.na(regulatorName)) {
        regulatorId <- .addAndGetRegulatorId(db$db, regulatorName, db$datasetId)
    } else {
        regulatorId <- NA
    }
    if (description == '')
        description <- name
    if (is.logical(suppData)) {
        type = 0
    } else if (is.factor(suppData)) {
        type = 1
    } else {
        type = 2
    }
    suppDatasetId <- .addAndGetSupplementaryDataId(db$db, name, regulatorId, type, source, platform, description)
    .addSupplementaryData(db$db, suppDatasetId, suppData)    
    return (db)
}

insertZScores <- function(db, zscores) {
    .addZscores(db$db, db$datasetId, zscores)
    return (db)
}

closeDb <- function(db, experimentSet='') {
    rootId <- .addAndGetExperimentSetId(db$db, 'All experiments', NA)
    if (experimentSet != '')
        setId <- .addAndGetExperimentSetId(db$db, paste(experimentSet, ' (', datasetName, ')', sep=''), rootId)
    else
        setId <- rootId
    for (i in seq_along(db$experimentIds)) {
        .addExperimentSetExperiments(db$db, setId, db$experimentIds[[i]])
    }
    dbDisconnect(db$db)
    return (list())
}
