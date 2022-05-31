#' Gives groups of indices for cross validation
#'
#' @slot k number of folds
#' @slot validation whether to have a validation set
#' @slot test whether to have a test set
#' @slot data contains the sample groupings (not intended for user)
setClass("CrossValidation",
         representation(k="integer",
                        validation="logical",
                        test="logical",
                        data="integer"),
         prototype(validation=FALSE, test=TRUE))

#' Classic k-fold cross validation
setClass("KFoldCrossValidation", contains="CrossValidation")

#' k-fold cross validation that respects group frequencies
setClass("StratifiedKFoldCrossValidation",
         contains="CrossValidation")

# Assigns 1:n to k roughly-equal sized groups.
# Returns a vector of groups for each element.
#' @noRd
k_fold_groups <- function(n, k) {
  q <- floor(n / k)
  r <- n %% k
  do.call(c, lapply(1:k, function(g) {
    delta <- 0
    if (g <= r) {
      delta <- 1
    }
    reps <- q + delta
    rep(g, reps)
  }))
}

#' Shuffle vector
#'
#' @param x vector to shuffle
#' @return shuffled version of x
#' @noRd
shuffle <- function(x) {
  sample(x, length(x))
}

#' Returns new k-fold cross-validation on n objects
#'
#' @param n number of items
#' @param k number of folds
#' @param validation whether to have a validation set for each fold
#' @param test whether to have a test set for each fold
#' @return KFoldCrossValidation object
#' @importFrom methods new
#' @export
k_fold_cross_validation <- function(n, k, validation=FALSE, test=TRUE) {
  data <- shuffle(k_fold_groups(n, k))
  new("KFoldCrossValidation",
      data=data, k=k,
      validation=validation, test=test)
}

# Splits a factor into a list of indices with
# each list at the same level
#' @noRd
split_by_level <- function(groups) {
  lvls <- levels(groups)
  lapply(lvls, function(x) which(groups == x))
}
# split_by_level(iris$Species)

# Splits vec into a list of vectors belong to k folds
#' @noRd
split_k_fold <- function(vec, k) {
  groups <- shuffle(k_fold_groups(length(vec), k))
  lapply(1:k, function(i) {
    vec[groups == i]
  })
}

#' @noRd
stratified_k_fold_splits <- function(groups, k) {
  # Split by level then split by fold
  tmp <- split_by_level(groups)
  tmp <- lapply(tmp, function(x) split_k_fold(x, k))
  # Collect indices for each split and assign
  data <- integer(length(groups))
  for (i in 1:k) {
    for (lv in tmp) {
      data[lv[[i]]] <- i
    }
  }
  data
}
# groups <- iris$Species
# fold <- stratified_k_fold_splits(groups, 7)
# table(fold, groups)

#' Stratified k-fold cross-validation
#'
#' Stratified cross-validation yields sets of samples
#' in roughly the same proportions as in the original dataset.
#'
#' @param groups factor of the groups each sample belongs to
#' @param k number of folds of cross-validation
#' @param validation whether to have a validation set for each fold
#' @param test whether to have a test set for each fold
#' @return StratifiedKFoldCrossValidation object
#' @importFrom methods new
#' @export
stratified_k_fold_cross_validation <- function(groups, k,
                                               validation=FALSE,
                                               test=TRUE) {
  data <- stratified_k_fold_splits(groups, k)
  new("StratifiedKFoldCrossValidation",
      data=data, k=k, validation=validation, test=test)
}

#' Represents a set of indices
#'
#' @slot train the indices of items in the training set
#' @slot validation the indices of items in the validation set
#' @slot test the indices of items in the testing set
setClass("IndexTuple",
         representation(train="integer",
                        validation="integer",
                        test="integer"))

# Get the indices belonging to certain folds
#' @noRd
get_indices <- function(cv, folds, exclude=FALSE) {
  if (!exclude) {
    which(cv@data %in% folds)
  } else {
    which(!(cv@data %in% folds))
  }
}

# Addition but 1-based and wraps around k
#' @noRd
add_wrap <- function(x, delta, k) {
  mod <- (x-1+delta) %% k
  mod + 1
}

#' Gets the folds needed for the train, validation, and test sets
#'
#' (Not intended for end user)
#'
#' @param cv CrossValidation-derived object
#' @param i which fold
#' @return list of folds to use in train, validation, and test sets
#' @export
setGeneric("get_folds", function(cv, i) {
  standardGeneric("get_folds")
})

#' @rdname get_folds
setMethod("get_folds", signature(cv="CrossValidation",
                                 i="integer"),
          function(cv, i) {
            validation <- NULL
            test <- NULL
            if (cv@validation && cv@test) {
              validation <- i
              test <- add_wrap(i, 1, cv@k) # i+1
            } else if (cv@validation) {
              validation <- i
            } else if (cv@test) {
              test <- add_wrap(i, 1, cv@k) # i+1
            }
            list(validation=validation, test=test)
          })

#' Returns the ith fold of cross-validation
#'
#' @param cv CrossValidation-derived object
#' @param i which fold
#' @return IndexTuple object containing the indices
#'    for train, validation, test sets
#' @export
setGeneric("get_fold", function(cv, i) {
  standardGeneric("get_fold")
})
#' @rdname get_fold
#' @importFrom methods new
setMethod("get_fold", signature(cv="CrossValidation",
                                i="integer"),
          function(cv, i) {
            folds <- get_folds(cv, i)
            train <- get_indices(cv, c(folds$validation, folds$test), TRUE)
            validation <- get_indices(cv, folds$validation)
            test <- get_indices(cv, folds$test)
            new("IndexTuple", train=train, test=test, validation=validation)
          })
# cv <- stratified_k_fold_cross_validation(
#   iris$Species, 4L, validation=TRUE)
# get_fold(cv, 4L)

#' CrossValidation that respects time series order
#'
#' @slot all_preceding_periods whether to include all preceding periods
#'    when constructing training set
setClass("TimeSeriesCrossValidation",
         contains="CrossValidation",
         representation(all_preceding_periods="logical"),
         prototype(all_preceding_periods=TRUE))

#' Time series cross-validation
#'
#' @param n length of time series
#' @param k number of folds of cross-validation
#' @param validation whether validation set is needed
#' @param test whether test set is needed
#' @param all_preceding_periods whether to include all history up to
#'    the current training set.
#' @return TimeSeriesCrossValidation object
#' @importFrom methods new
#' @export
time_series_cross_validation <- function(n,
                                         k,
                                         validation = FALSE,
                                         test = TRUE,
                                         all_preceding_periods = TRUE) {
  k_needed <- k
  if (validation) k_needed <- k_needed+1
  if (test) k_needed <- k_needed+1
  data <- k_fold_groups(n, k_needed)
  new("TimeSeriesCrossValidation",
      data=data,
      validation=validation,
      test=test,
      k=as.integer(k),
      all_preceding_periods=all_preceding_periods)
}

#' @rdname get_folds
setMethod("get_folds", signature(cv="TimeSeriesCrossValidation",
                                 i="integer"),
          function(cv, i) {
            validation <- NULL
            test <- NULL
            if (cv@validation && cv@test) {
              validation <- i+1
              test <- i+2
            } else if (cv@validation) {
              validation <- i+1
            } else if (cv@test) {
              test <- i+1
            }
            list(validation=validation, test=test)
          })

#' @rdname get_fold
#' @importFrom methods new
setMethod("get_fold", signature(cv="TimeSeriesCrossValidation",
                                i="integer"),
          function(cv, i) {
            folds <- get_folds(cv, i)
            train <- if (cv@all_preceding_periods) {
              which(cv@data <= i) # all preceding folds
            } else {
              which(cv@data == i) # only fold i
            }
            validation <- get_indices(cv, folds$validation)
            test <- get_indices(cv, folds$test)
            new("IndexTuple", train=train, test=test, validation=validation)
          })
