context("Check that schemas are populated in APIs")

test_that("schema functions are returning what is expected", {

    .gsunname <- function(api, schema_name) {
        rapi <- get_schema(api, schema_name)
        attributes(rapi)$name <- NULL
        attributes(rapi)$class <- NULL
        rapi
    }

    petstore_spec <- system.file("extdata", "sample_specs", "petstore.json",
        package = "rapiclient")
    api <- get_api(petstore_spec)
    apijson <- jsonlite::fromJSON(petstore_spec)

    schema_names <- names(apijson$definitions)
    expect_equal(names(get_schemas(api)), schema_names)

    for (sname in schema_names) {
        aname <- .gsunname(api, sname)
        expect_equal(aname, apijson$definitions[[sname]])
    }

    # openAPIs
    openapi <- list(
        components = list(
            schemas = list(
                fun1 = list(
                    properties = list(
                        id = list(type = "integer", format = "int64")
                    )
                ),
                fun2 = list(
                    format = "text"
                )
            )
        )
    )
    funinfo1 <- openapi$components$schemas$fun1
    funinfo2 <- openapi$components$schemas$fun2
    rfun1 <- .gsunname(openapi, "fun1")
    expect_equal(funinfo, rfun1)
    rfun2 <- .gsunname(openapi, "fun2")
    expect_equal(funinfo2, rfun2)
    fun1 <- get_schema_function(get_schema(openapi, "fun1"))
    expect_true(is.function(fun1))
})
