# Check Country Parameter
check_country <- function(country, resource) {
  if (rlang::is_null(country)) return(NULL)
  country <- as.character(country)

  # Country is valid for all resources, so no restrictions here
  if (!all(grepl("^[0-9]+$", country))) {
    cli::cli_abort("{.field country} codes must be {.emph numeric}.")
  }
  return(country)
}

# Check Conflict Parameter
check_conflict <- function(conflict, resource) {
  if (rlang::is_null(conflict)) return(NULL)
  conflict <- as.character(conflict)

  # Conflict parameter is NOT valid for 'onesided' resource
  if (resource == "onesided") {
    cli::cli_abort("{.field conflict} parameter is not valid for the {.val onesided} resource.")
  }

  if (!all(grepl("^[0-9]+$", conflict))) {
    cli::cli_abort("{.field conflict} IDs must be {.emph numeric}.")
  }
  return(conflict)
}

# Check Year Parameter
check_year <- function(year, resource) {
  if (rlang::is_null(year)) return(NULL)
  year <- as.character(year)

  # Year is valid for all resources
  if (!all(grepl("^[0-9]{4}$", year))) {
    cli::cli_abort("{.field year} must be in {.emph YYYY} format.")
  }
  return(year)
}

# Check Conflict Incompatibility Parameter
check_conflict_incompatibility <- function(incompatibility, resource) {
  if (rlang::is_null(incompatibility)) return(NULL)
  incompatibility <- as.character(incompatibility)

  # Conflict Incompatibility is NOT valid for 'nonstate' and 'onesided'
  if (resource %in% c("nonstate", "onesided")) {
    cli::cli_abort("{.field conflict_incompatibility} parameter is not valid for the {.val nonstate} or {.val onesided} resources.")
  }

  if (!all(grepl("^[0-9]+$", incompatibility))) {
    cli::cli_abort("{.field conflict_incompatibility} codes must be {.emph numeric}.")
  }
  return(incompatibility)
}

# Check Conflict Type Parameter
check_conflict_type <- function(type, resource) {
  if (rlang::is_null(type)) return(NULL)
  type <- as.character(type)

  # Conflict Type is NOT valid for 'nonstate' and 'onesided'
  if (resource %in% c("nonstate", "onesided")) {
    cli::cli_abort("{.field conflict_type} parameter is not valid for the {.val nonstate} or {.val onesided} resources.")
  }

  if (!all(grepl("^[0-9]+$", type))) {
    cli::cli_abort("{.field conflict_type} codes must be {.emph numeric}.")
  }
  return(type)
}


# Check Dyad Parameter for UCDP GED and Other Relevant Resources
check_dyad <- function(dyad, resource) {
  if (rlang::is_null(dyad)) return(NULL)
  dyad <- as.character(dyad)

  # Dyad is valid for 'gedevents', 'dyadic', 'battledeaths', and 'onesided'
  if (!resource %in% c("gedevents", "dyadic", "battledeaths", "onesided")) {
    cli::cli_abort("{.field dyad} parameter is only valid for the {.val gedevents}, {.val dyadic}, {.val battledeaths}, and {.val onesided} resources.")
  }

  if (!all(grepl("^[0-9]+$", dyad))) {
    cli::cli_abort("{.field dyad} codes must be {.emph numeric}.")
  }
  return(dyad)
}

# Check Actor Parameter for UCDP GED
check_actor <- function(actor, resource) {
  if (rlang::is_null(actor)) return(NULL)
  actor <- as.character(actor)

  # Actor is only valid for 'gedevents' resource
  if (resource != "gedevents") {
    cli::cli_abort("{.field actor} parameter is only valid for the {.val gedevents} resource.")
  }

  if (!all(grepl("^[0-9]+$", actor))) {
    cli::cli_abort("{.field actor} codes must be {.emph numeric}.")
  }
  return(actor)
}


# Check Org Parameter
check_org <- function(org, resource) {
  if (rlang::is_null(org)) return(NULL)
  org <- as.character(org)

  # Org is only valid for the 'nonstate' resource
  if (resource != "nonstate") {
    cli::cli_abort("{.field org} parameter is only valid for the {.val nonstate} resource.")
  }

  if (!all(grepl("^[0-9]+$", org))) {
    cli::cli_abort("{.field org} codes must be {.emph numeric}.")
  }
  org
}



# Check Geography Parameter for UCDP GED
check_geography <- function(geography, resource) {
  if (rlang::is_null(geography)) return(NULL)
  geography <- as.character(geography)

  # Geography is only valid for the 'gedevents' resource
  if (resource != "gedevents") {
    cli::cli_abort("{.field geography} parameter is only valid for the {.val gedevents} resource.")
  }

  # Validate format: two coordinates in the format y0 x0,y1 x1
  if (!grepl("^(-?\\d+\\.?\\d*)\\s(-?\\d+\\.?\\d*),(-?\\d+\\.?\\d*)\\s(-?\\d+\\.?\\d*)$", geography)) {
    cli::cli_abort("{.field geography} must be in the format {.emph y0 x0,y1 x1} with latitude and longitude values.")
  }
  return(geography )
}

# Check StartDate Parameter for UCDP GED
check_start_date <- function(start_date, resource) {
  if (rlang::is_null(start_date)) return(NULL)
  start_date <- as.character(start_date)

  # StartDate is only valid for 'gedevents' resource
  if (resource != "gedevents") {
    cli::cli_abort("{.field start_date} parameter is only valid for the {.val gedevents} resource.")
  }

  # Validate date format: YYYY-MM-DD
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", start_date)) {
    cli::cli_abort("{.field start_date} must be in {.emph YYYY-MM-DD} format.")
  }
  return(start_date)
}

# Check EndDate Parameter for UCDP GED
check_end_date <- function(end_date, resource) {
  if (rlang::is_null(end_date)) return(NULL)
  end_date <- as.character(end_date)

  # EndDate is only valid for 'gedevents' resource
  if (resource != "gedevents") {
    cli::cli_abort("{.field end_date} parameter is only valid for the {.val gedevents} resource.")
  }

  # Validate date format: YYYY-MM-DD
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", end_date)) {
    cli::cli_abort("{.field end_date} must be in {.emph YYYY-MM-DD} format.")
  }
  return(end_date)
}

# Check TypeOfViolence Parameter for UCDP GED
check_type_of_violence <- function(type_of_violence, resource) {
  if (rlang::is_null(type_of_violence)) return(NULL)
  type_of_violence <- as.character(type_of_violence)

  # TypeOfViolence is only valid for 'gedevents' resource
  if (resource != "gedevents") {
    cli::cli_abort("{.field type_of_violence} parameter is only valid for the {.val gedevents} resource.")
  }

  if (!all(grepl("^[0-9]+$", type_of_violence))) {
    cli::cli_abort("{.field type_of_violence} codes must be {.emph numeric}.")
  }
  return(type_of_violence)
}
