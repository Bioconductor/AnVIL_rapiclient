context("Check that schemas are populated in APIs")

test_that("schema functions are returning what is expected", {

    .getSchemaUnname <- function(api, schema_name) {
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
        aname <- .getSchemaUnname(api, sname)
        expect_equal(aname, apijson$definitions[[sname]])
    }
})
