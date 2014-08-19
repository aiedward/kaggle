## Script to load data into workspace

if (!"train" %in% ls()) {
    train <- read.csv("./data/train.csv")
}

if (!"test" %in% ls()) {
    test <- read.csv("./data/test.csv")
}

if (!"gendermodel" %in% ls()) {
    gendermodel <- read.csv("./data/gendermodel.csv")
}

if (!"genderclassmodel" %in% ls()) {
    genderclassmodel <- read.csv("./data/genderclassmodel.csv")
}
