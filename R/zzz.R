cl_env <- new.env()
assign("max_page_size", 1000, envir = cl_env)
utils::globalVariables("request")