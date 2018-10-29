## ---- Pima Indians Diabetes Loading --------

pid_load <- function() {
    data(PimaIndiansDiabetes, package = "mlbench")
    PimaIndiansDiabetes
}

pid_holdout <- function(df) {
    set.seed(200)
    partition_idx <- caret::createDataPartition(df$diabetes, p = 4/5, list = FALSE)
    training_df <- PimaIndiansDiabetes[ partition_idx,]
    training_df
}

pid_training <- function(df) {
    set.seed(200)
    partition_idx <- caret::createDataPartition(df$diabetes, p = 4/5, list = FALSE)
    test_df <- PimaIndiansDiabetes[ -partition_idx,]
    test_df
}

#' Clean Pima Indians Diabetes data
pid_clean <- function(df) {
    df <- df %>% mutate_at(vars(glucose, pressure, triceps, insulin, mass), function(x) ifelse(x != 0, x, na_dbl))
    df
}

if (!exists("pid_cluster")) {
    pid_cluster <- NULL
}

start_cluster <- function() {
    if (is.null(pid_cluster)) {
        pid_cluster <<- parallel::makeCluster(6)
        doParallel::registerDoParallel(pid_cluster)
    } else {
        message("Cluster is already running")
    }
}

stop_cluster <- function() {
    if (!is.null(pid_cluster)) {
        parallel::stopCluster(pid_cluster)
        pid_cluster <<- NULL
    }
}


