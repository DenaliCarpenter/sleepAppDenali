#' Convert character variable to numeric factor
#'
#' This function takes in a character vector, and returns a numeric factor vector.
#'
#' @param x character vector
#' @return a numeric factor variable
#' @export
fromCharToFactorNum <- function(x){
  result <- as.factor(as.numeric(as.factor(x)))

  return(result)
}

#' Create data frame of unique profiles in a dataset
#'
#' This function takes in a list of attributes and a dataframe and returns a data frame of
#' the existing profiles in the dataset.
#'
#' @param attributes a list of attributes
#' @param dataFrame the data frame where the attributes came from
#' @return a data frame of the profiles
#' @export
getProfs <- function(attributes, dataFrame){

  result <- dataFrame %>%
    dplyr::mutate_at(attributes, fromCharToFactorNum) %>%
    dplyr::select(attributes) %>%
    dplyr::distinct()

  return(result)

}

#' Create a data frame of all possible profiles
#'
#' This function takes in the attributes of a data frame and returns
#' a data frame of all of the possible profiles.
#'
#' @param attributes list of attributes
#' @param idColumnName a string of the column that identifies respondents
#' @param dataFrame the data frame where the attributes came from
#' @param replaceNames a boolean to tell the function to replace the colnames
#' @return a dataframe of all possible profiles
#' @export
getAllProfs <- function(attributes, idColumnName, dataFrame, replaceNames = TRUE){

  namedLevelList <- list()

  for(i in 1:length(attributes)){
    namedLevelList[toString(attributes[i])] <- unique(dataFrame[,toString(attributes[i])])
  }

  if(replaceNames == TRUE) {

    result <- expand.grid(namedLevelList) %>%
      mutate_at(attributes[attributes != idColumnName], fromCharToFactorNum) %>%
      distinct()

  } else {
    result <- expand.grid(namedLevelList) %>%
      distinct()
  }

  return(result)

}

#' Get a data frame of the preferences for each profile
#'
#' This function creates a data frame with the preferences for each profile.
#'
#' @param attributes list of attributes
#' @param profs a data frame of the profiles that exist in the data frame provided
#' @param idColumnName a string of the column that identifies respondents
#' @param answerColumnName a string of the column that identifies the survey responses
#' @param dataFrame the data frame where the attributes and profs came from
#' @return a data frame of the preferences for each profile that exists in the data frame
#' @export
getPrefs <- function(attributes, profs, idColumnName, answerColumnName, dataFrame){

  profsWithRowID <- profs %>%
    tibble::rowid_to_column("profile")


  result <- dataFrame %>%
    dplyr::mutate_at(attributes, fromCharToFactorNum) %>%
    left_join(profsWithRowID) %>%
    mutate(profile = paste0("profile", profile)) %>%
    dplyr::select(idColumnName, profile, answerColumnName) %>%
    tidyr::pivot_wider(names_from = profile, values_from = answerColumnName, values_fill = NA) %>%
    dplyr::distinct() %>%
    tibble::column_to_rownames(idColumnName) %>%
    dplyr::mutate_all(as.character)

  print("Fixing missing values")

  for(i in 1:ncol(result)){

    result[result[,i] == "NULL", i] <- NA

    svMisc::progress(i)
    if(i == ncol(result)){
      cat("Done!\n")
    }
  }

  result <- result %>%
    dplyr::mutate_all(as.numeric)

  return(result)

}

#' Predict the missing values from all possible preferences
#'
#' This creates a random forest model based on the data frames provided, and predicts the responses
#' for each profile for each respondent. This function has only been tested on the data provided.
#'
#' @param attributes list of attributes
#' @param allProfs a dataframe of all possible profiles for each respondent
#' @param idColumnName a string of the column that identifies respondents
#' @param answerColumnName a string of the column that identifies the survey responses
#' @param dataFrameExp a data frame of the experiment data
#' @param dataFrameSurvey a data frame of the survey data
#' @return a data frame with the existing responses and predicted responses for each profile from each individual
#' @export
getPredictedPrefs <- function(attributes, allProfs, idColumnName, answerColumnName, dataFrameExp, dataframeSurvey){

  #create a dataframe without RespondentIDs and create a number for each profile
  profileNums <- allProfs %>%
    select(-idColumnName) %>%
    distinct() %>%
    rowid_to_column("profile")


  fullData <- dataFrameExp %>%
    full_join(dataframeSurvey) %>%
    as.data.frame()

  for(i in 1:ncol(fullData)){

    fullData[is.na(fullData[,i]), i] <- 0

  }

  cleanSurveyData <- dataframeSurvey

  for(i in 1:ncol(cleanSurveyData)){

    cleanSurveyData[is.na(cleanSurveyData[,i]), i] <- 0

  }

  #building random forest model for prediction
  set.seed(123)
  split <- initial_split(fullData, prop = .8)
  train <- training(split)
  test  <- testing(split)

  formula <- paste0(answerColumnName, " ~ .")

  modelOne <- ranger(formula, data = train)
  results <- predict(modelOne, test)$prediction

  resultsDF <- test %>%
    dplyr::select(idColumnName, answerColumnName) %>%
    bind_cols(prediction = results)

  rSqd <- cor(resultsDF[answerColumnName], resultsDF$prediction)^2

  print(paste0("Testing R-Squared = ", rSqd))
  print("Training Random Forest model on full dataset")
  finalModel <- ranger(formula, data = fullData)
  print("Final model created.")

  allProfs <- allProfs %>%
    full_join(dataFrameExp) %>%
    full_join(cleanSurveyData)

  noAnswer <- allProfs %>%
    filter(is.na(allProfs[answerColumnName]))

  answerDF <- allProfs %>%
    filter(!is.na(allProfs[answerColumnName]))

  print("Predicting missing entires.")
  predictedAnswer <- predict(finalModel, noAnswer)$prediction

  newAnswer <- noAnswer %>%
    dplyr::select(-answerColumnName) %>%
    bind_cols(answer = predictedAnswer)

  fullExperinmentResults <- newAnswer %>%
    bind_rows(answerDF)

  allProfs <- allProfs %>%
    full_join(profileNums) %>%
    dplyr::select(attributes, profile) %>%
    mutate_at(attributes[attributes != idColumnName], fromCharToFactorNum)

  result <- fullExperinmentResults %>%
    mutate_at(attributes[attributes != idColumnName], fromCharToFactorNum) %>%
    left_join(allProfs) %>%
    mutate(profile = paste0("profile", profile)) %>%
    dplyr::select(idColumnName, profile, answer) %>%
    pivot_wider(names_from = profile, values_from = answer, values_fill = NA) %>%
    distinct() %>%
    column_to_rownames(idColumnName) %>%
    mutate_all(as.character) %>%
    mutate_all(as.numeric)

  return(result)
}

#' Get a data frame of levels in order as they appear
#'
#' This function identifies the levels of each attribute provided
#' in the order they appear in the data frame.
#'
#' @param attributes a list of attributes
#' @param dataFrame the data frame where the attributes came from
#' @return a data frame of the levels
#' @export
getLevels <- function(attributes, dataFrame){

  result <- list()

  for (i in 1:length(attributes)) {
    
    order <- dataFrame %>%
      dplyr::select(dplyr::contains(attributes[i])) %>%
      mutate_at(attributes[i], fromCharToFactorNum)

    result[i] <- dataFrame %>%
      dplyr::select(dplyr::contains(c(attributes[i]))) %>%
      dplyr::bind_cols(orderNum = order[[1]]) %>%
      dplyr::distinct() %>%
      dplyr::mutate(orderNum = as.numeric(orderNum)) %>%
      dplyr::arrange(orderNum) %>%
      dplyr::select(attributes[i]) %>%
      as.list()

  }

  result <- as.data.frame(unlist(result))

  return(result)
}


#' Get mode of vector
#'
#' This function finds the most common element in a vector
#'
#' @param x a vector
#' @return mode
#' @export
getMode <- function(x){
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}