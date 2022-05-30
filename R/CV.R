#' @importFrom coro generator yield
#' @keywords internal
wrap_function <- function(f, k) {
  generator(function() {
    for (i in 1:k) {
      yield(f(i))
    }
  })()
}

#' @keywords internal
csapply <- function(v, f) {
  do.call(c, sapply(v, f))
}

#' Generates k groups of roughly equal size
#'
#' @param n number of items to be grouped
#' @param k number of groups
#' @return vector of group membership of n items
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
# length(k_fold_groups(100,7))
# table(k_fold_groups(100,7))

#' Shuffle vector
#'
#' @param x vector to shuffle
#' @return shuffled version of x
shuffle <- function(x) {
  sample(x, length(x))
}

#' Pick out the indices that are in group i
#' @keywords internal
pick <- function(groups, i) {
  which(groups == i)
}

#' @keywords internal
pick_not <- function(groups, i) {
  which(groups != i)
}

#' @keywords internal
# Pick indices for training set
pick_train <- function(groups, i, whole_history = FALSE) {
  if (whole_history) {
    which(groups <= i)
  } else {
    pick(groups, i)
  }
}

#' Regular cross-validation split
#'
#' @param n number of items
#' @param k number of folds
#' @return generator that returns the indices for fold 1,2,...,k
#' @export
cv_split <- function(n, k) {
  groups <- shuffle(k_fold_groups(n, k))
  wrap_function(function(i) {
    test <- pick(groups, i)
    train <- pick_not(groups, i)
    list(train=train, test=test)
  }, k)
}

# Splits vector v into k disjoint folds.
# Returns list of those folds
#' @keywords internal
split_folds <- function(v, k) {
  groups <- shuffle(k_fold_groups(length(v), k))
  lapply(1:k, function(i) v[groups == i])
}

#' Stratified cross-validation
#'
#' @param groups factor of group membership of items
#' @param k number of groups
#' @return generator that returns the indices for fold 1,2,...,k
#' @export
stratified_cv_split <- function(groups, k) {
  lvl <- levels(groups)
  # For each group...
  indices <- lapply(lvl, function(x) pick(groups, x))
  # Split the indices into k parts
  index_splits <- lapply(indices, function(inds) split_folds(inds, k))
  # Get the indices of ith fold of each original group
  wrap_function(function(i) {
    csapply(index_splits, function(inds) {
      inds[i]
    })
  }, k)
}
# f <- stratified_cv_split(iris$Species, 3)
# table(iris$Species[f()])
# table(iris$Species[f()])
# table(iris$Species[f()])

#' Regular time series cross-validation
#'
#' @param n length of time series
#' @param k number of folds
#' @param whole_history whether to include all past data in the training set
#' @return generator that returns indices for fold 1,2,...,k
#' @export
time_series_split <- function(n, k, whole_history = TRUE) {
  groups <- k_fold_groups(n, k + 1) # Need extra fold for training
  # Returns the ith fold of cross validation
  wrap_function(function(i) {
    train <- pick_train(groups, i, whole_history)
    test <- pick(groups, i + 1)
    list(train = train, test = test)
  }, k)
}

# Guaranteed to have test set at least test_size * length(v)
#' @keywords internal
simple_split <- function(v, test_size) {
  if (test_size <= 0 || test_size >= 1) {
    stop("The fraction of test set must be between 0 and 1.")
  }
  n <- length(v)
  cutoff <- floor(n * (1 - test_size))
  train <- v[1:cutoff]
  test <- v[(cutoff + 1):n]
  list(train = train, test = test)
}

#' Time series cross-validation with no overlapping data
#'
#' @param n length of time series
#' @param k number of folds
#' @param test_size fraction of samples in each fold to use as testing set.
#'    Must be a number between 0 and 1 (exclusive).
#' @return a generator that returns indices for fold 1,2,...,k
#' @export
blocking_time_series_split <- function(n, k, test_size) {
  groups <- k_fold_groups(n, k) # exactly k folds
  wrap_function(function(i) {
    indices <- pick(groups, i)
    simple_split(indices, test_size)
  }, k)
}

#' Time series cross-validation with validation set
#'
#' @param n length of time series
#' @param k number of folds
#' @param whole_history whether to include all of the past at each fold
#' @return generator that returns indices for fold 1,2,...,k
#' @export
day_forward_chaining_split <- function(n, k, whole_history = TRUE) {
  groups <- k_fold_groups(n, k + 2) # need validation and test sets
  wrap_function(function(i) {
    train <- pick_train(groups, i, whole_history)
    validation <- pick(groups, i + 1)
    test <- pick(groups, i + 2)
    list(train = train,
         validation = validation,
         test = test)
  }, k)
}

#' Helper function for plot_splits
#' @keywords internal
make_graphable <- function(fold, dataset) {
  category <- do.call(c, lapply(names(dataset), function(nam) {
    rep(nam, length(dataset[[nam]]))
  }))
  index <- do.call(c, dataset)
  # Part of data frame that we can graph
  data.frame(fold=fold,
             index=index,
             category=category)
}

#' Plot samples in cross-validation scheme
#'
#' @param splits generator returned by other functions in this package
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot geom_point aes scale_y_discrete scale_x_continuous
#' @importFrom ggplot2 scale_color_discrete ggtitle theme_classic
#' @export
plot_splits <- function(splits) {
  lst <- coro::collect(splits)
  # Create the data for plotting
  dat <- do.call(rbind, lapply(1:length(lst), function(i) {
    make_graphable(i, lst[[i]])
  }))
  # Make plot
  ggplot(dat, aes(.data$index, factor(.data$fold))) +
    geom_point(aes(colour = .data$category)) +
    scale_y_discrete("Fold", limits=rev) +
    scale_x_continuous("Index") +
    scale_color_discrete("Dataset") +
    ggtitle("Indices Of Samples In Cross-Validation Scheme") +
    theme_classic()
}

#f <- day_forward_chaining_split(30, 4)
#f <- cv_split(30, 4)
#plot_splits(f)
