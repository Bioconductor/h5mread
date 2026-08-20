### =========================================================================
### Some low-level utilities
### -------------------------------------------------------------------------
###
### Nothing in this file is exported.
###


add_prefix_to_basename <- function(name, prefix=".")
{
    stopifnot(isSingleString(name), isSingleString(prefix))
    slash_idx <- which(safeExplode(name) == "/")
    if (length(slash_idx) == 0L) {
        dname <- ""
        bname <- name
    } else {
        last_slash_idx <- max(slash_idx)
        dname <- substr(name, start=1L, stop=last_slash_idx)
        bname <- substr(name, start=last_slash_idx+1L, stop=nchar(name))
    }
    paste0(dname, prefix, bname)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### normarg_h5_filepath() and normarg_h5_name()
###

normarg_h5_filepath <- function(path, what1="'filepath'", what2="the dataset")
{
    if (!isSingleString(path))
        stop(wmsg(what1, " must be a single string specifying the path ",
                  "to the HDF5 file where ", what2, " is located"))
    file_path_as_absolute(path)  # return absolute path in canonical form
}

normarg_h5_name <- function(name, what1="'name'",
                                  what2="the name of a dataset",
                                  what3="")
{
    if (!isSingleString(name))
        stop(wmsg(what1, " must be a single string specifying ",
                  what2, " in the HDF5 file", what3))
    if (name == "")
        stop(wmsg(what1, " cannot be the empty string"))
    if (substr(name, start=1L, stop=1L) == "/") {
        name <- sub("^/*", "/", name)  # only keep first leading slash
    } else {
        name <- paste0("/", name)
    }
    name
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### compute_max_string_size()
###

### Computes the value to pass to the 'size' argument of
### rhdf5::h5createDataset().
### NOTE: No longer used. As per rhdf5's recommendation (starting with
### version 2.57.10), variable-length strings should be used rather than
### fixed-size strings when creating an H5T_STRING dataset.
compute_max_string_size <- function(x)
{
    ## We want this to work on any array-like object, not just ordinary
    ## arrays, so we must use type() instead of is.character().
    if (type(x) != "character")
        return(NULL)
    if (length(x) == 0L)
        return(0L)
    ## Calling nchar() on 'x' will trigger block processing if 'x' is a
    ## DelayedArray object, so it could take a while.
    max(nchar(x, type="bytes", keepNA=FALSE))
}

