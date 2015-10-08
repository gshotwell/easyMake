a <- rio::import("tests/mtcars.csv")
export(a, "tests/a.tsv")
save(a, "tests/sv.RData")
load(a, "tests/sv.RData")
