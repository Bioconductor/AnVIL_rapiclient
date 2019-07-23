context("Operations testing")

test_that("temp_fun returns expected output based on request type", {

    petstore_spec <-
        system.file("extdata", "sample_specs", "petstore.json", package = "rapiclient")
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

    getUserbyName <- api_ops[["getUserByName"]]
    deleteUser <- api_ops[["deleteUser"]]
    updateUser <- api_ops[["updateUser"]]
    addPet <- api_ops[["addPet"]]

    get_query_fun(deleteUser, x)

    tmp_fun <- function(op) {
        x <- eval(param_values)
        request_json <- get_message_body(op, x)

        do_op <- switch(op$action,
            post = function(...) httr::POST(..., body = request_json),
            put = function(...) httr::PUT(..., body = request_json),
            get = httr::GET,
            delete = httr::DELETE
        )

        result <- do_op(
          url = get_url(x),
          config = get_config(),
          httr::content_type("application/json"),
          httr::accept_json(),
          httr::add_headers(.headers = .headers)
        )
        handle_response(result)
    }

    tmp_fun(getUserbyName)

})
