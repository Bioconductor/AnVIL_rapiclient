context("Operations testing")

test_that("temp_fun returns expected output based on request type", {

    petstore_spec <- system.file("extdata", "sample_specs", "petstore.json",
        package = "rapiclient")
    api <- get_api(petstore_spec)
    api_ops <- get_operation_definitions(api, NULL)

    param_values <- expression({
        if (length(formals()) > 0) {
            l1 <- as.list(mget(names(formals()), environment()))
            l1 <- l1[lapply(l1, mode) != "name"]
            l1[!vapply(l1, is.null, logical(1))]
        } else {
            list()
        }
    })

    funs <- get_query_funs(api_ops, api, param_values, identity, NULL)

    getuser <- funs[["getUserByName"]]
    delop <- funs[["deleteUser"]]
    updateuser <- funs[["updateUser"]]
    addpet <- funs[["addPet"]]

    getAction <- function(query_fun) {
        attributes(query_fun)[["definition"]][["action"]]
    }

    expect_identical(class(delop), c("rapi_operation", "function"))
    expect_identical(class(getuser), c("rapi_operation", "function"))
    expect_identical(class(updateuser), c("rapi_operation", "function"))
    expect_identical(class(addpet), c("rapi_operation", "function"))

    expect_identical(getAction(getuser), "get")
    expect_identical(getAction(delop), "delete")
    expect_identical(getAction(updateuser), "put")
    expect_identical(getAction(addpet), "post")
})
