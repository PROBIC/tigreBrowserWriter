source('gpsimDatabase.R')
source('interface.R')
db = initializeDb("testdb.sqlite", "testdataset")
aliases = c("aliasA", "aliasB", "aliasC")
names(aliases) = c("A", "B", "C")
db = insertAliases(db, "testalias", aliases)

logl = c(-4, -2, 0)
names(logl) = names(aliases)

baselogl = c(0, -1, 0)
names(baselogl) = names(aliases)

db = insertResults(db, "testexperiment", "testregulator", "", logl, baselineloglikelihoods=baselogl)

db = insertFigures(db, "testexperiment", "testregulator", "http://foo.invalid/")

zscores = c(1, 2, 3)
names(zscores) = names(aliases)

db = insertZScores(db, zscores)

db = insertSupplementaryData(db, "supptest", zscores)
boolsupp = c(TRUE, TRUE, FALSE)
names(boolsupp) = names(aliases)
db = insertSupplementaryData(db, "supptest_bool", boolsupp)
