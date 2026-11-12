### =========================================================================
### Some low-level utilities
### -------------------------------------------------------------------------
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### .H5Fopen()
### .H5Dopen()
### .H5Gopen()
###

### An undocumented feature of rhdf5::H5Fopen(), rhdf5::H5Dopen(), and
### rhdf5::H5Gopen() is that they won't necessarily throw an error when
### they fail to open the file, dataset, or group, but they can actually
### return a FALSE (with a message).
### The three thin wrappers below detect this situation and throw an error.

.H5Fopen <- function(name, flags=h5default("H5F_ACC_RD"), fapl=NULL)
{
    fid <- suppressMessages(rhdf5::H5Fopen(name, flags=flags, fapl=fapl))
    if (!is(fid, "H5IdComponent"))
        stop(wmsg("failed to open HDF5 file '", name, "'"))
    fid
}

.H5Dopen <- function(h5loc, name, dapl=NULL)
{
    did <- suppressMessages(rhdf5::H5Dopen(h5loc, name, dapl=dapl))
    if (!is(did, "H5IdComponent"))
        stop(wmsg("failed to open HDF5 dataset '", name, "'"))
    did
}

.H5Gopen <- function(h5loc, name)
{
    gid <- suppressMessages(rhdf5::H5Gopen(h5loc, name))
    if (!is(gid, "H5IdComponent"))
        stop(wmsg("failed to open HDF5 group '", name, "'"))
    gid
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### h5exists()
###

h5exists <- function(filepath, name)
{
    if (is(filepath, "H5File")) {
        fid <- as(filepath, "H5IdComponent")
    } else {
        fid <- .H5Fopen(filepath, flags="H5F_ACC_RDONLY")
        on.exit(H5Fclose(fid))
    }
    H5Lexists(fid, name)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### h5isdataset() and h5isgroup()
###

h5isdataset <- function(filepath, name)
{
    if (is(filepath, "H5File")) {
        fid <- as(filepath, "H5IdComponent")
    } else {
        fid <- .H5Fopen(filepath, flags="H5F_ACC_RDONLY")
        on.exit(H5Fclose(fid))
    }
    did <- try(.H5Dopen(fid, name), silent=TRUE)
    ans <- !inherits(did, "try-error")
    if (ans)
        H5Dclose(did)
    ans
}

h5isgroup <- function(filepath, name)
{
    if (is(filepath, "H5File")) {
        fid <- as(filepath, "H5IdComponent")
    } else {
        fid <- .H5Fopen(filepath, flags="H5F_ACC_RDONLY")
        on.exit(H5Fclose(fid))
    }
    gid <- try(.H5Gopen(fid, name), silent=TRUE)
    ans <- !inherits(gid, "try-error")
    if (ans)
        H5Gclose(gid)
    ans
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### h5dim() and h5chunkdim()
###

### Returns an object of class H5IdComponent representing an H5 dataset ID.
.get_h5dataset <- function(filepath, name)
{
    if (substr(name, 1L, 1L) != "/")
        name <- paste0("/", name)
    group <- gsub("(.*/)[^/]*$", "\\1", name)
    name <- gsub(".*/([^/]*)$", "\\1", name)
    if (is(filepath, "H5File")) {
        fid <- as(filepath, "H5IdComponent")
    } else {
        fid <- .H5Fopen(filepath, flags="H5F_ACC_RDONLY")
        on.exit(H5Fclose(fid))
    }
    gid <- .H5Gopen(fid, group)
    on.exit(H5Gclose(gid), add=TRUE)
    .H5Dopen(gid, name)
}

### Used in the HDF5Array package!
### The error message tries to provide more context than the very generic
### error message issued by S4Arrays:::as_integer_dim().
stop_if_big_h5_dimensions <- function(dim, filepath, name,
                                      of_what="HDF5 dataset",
                                      about_what=of_what)
{
    stopifnot(is.numeric(dim))
    if (all(dim <= .Machine$integer.max))
        return(invisible(NULL))
    dim_in1string <- paste0(dim, collapse=" x ")
    if (is(filepath, "H5File"))
        filepath <- path(filepath)
    supports_what <- of_what
    if (endsWith(supports_what, "matrix")) {
        supports_what <- sub("matrix$", "matrices", supports_what)
    } else if (!endsWith(supports_what, "s")) {
        supports_what <- paste0(supports_what, "s")
    }
    stop(wmsg("Dimensions of ", of_what, " are too big: ", dim_in1string),
         "\n\n  ",
         wmsg("(This error is about ", about_what, " '", name, "' ",
              "from HDF5 file '", filepath, "'.)"),
         "\n\n  ",
         wmsg("Please note that the HDF5Array package only ",
              "supports ", supports_what, " that have all their ",
              "dimensions <= 2^31-1 (= .Machine$integer.max)."))
}

### The TENxMatrixSeed() constructor from the HDF5Array package calls h5dim()
### with 'as.integer=FALSE' in order to get the dimension of a 1D array of
### length >= 2^31.
h5dim <- function(filepath, name, as.integer=TRUE)
{
    did <- .get_h5dataset(filepath, name)
    on.exit(H5Dclose(did), add=TRUE)
    sid <- H5Dget_space(did)
    on.exit(H5Sclose(sid), add=TRUE)
    dim <- H5Sget_simple_extent_dims(sid)$size
    if (as.integer) {
        stop_if_big_h5_dimensions(dim, filepath, name)
        dim <- as.integer(dim)
    }
    dim
}

### Returns NULL or an integer vector parallel to 'h5dim(filepath, name)'.
h5chunkdim <- function(filepath, name, adjust=FALSE)
{
    did <- .get_h5dataset(filepath, name)
    on.exit(H5Dclose(did), add=TRUE)
    pid <- H5Dget_create_plist(did)
    on.exit(H5Pclose(pid), add=TRUE)
    if (H5Pget_layout(pid) != "H5D_CHUNKED")
        return(NULL)
    ## We use rev() to invert the order of the dimensions returned by
    ## H5Pget_chunk(). It seems that H5Pget_chunk() should take care of
    ## this though, for consistency with how rhdf5 handles the order of the
    ## dimensions everywhere else (e.g. see ?H5Sget_simple_extent_dims).
    chunkdim <- rev(H5Pget_chunk(pid))
    stop_if_big_h5_dimensions(chunkdim, filepath, name,
                    of_what="HDF5 dataset chunks",
                    about_what="the dimensions of the chunks in HDF5 dataset")
    chunkdim <- as.integer(chunkdim)
    if (adjust) {
        dim <- h5dim(filepath, name, as.integer=FALSE)
        ## A sanity check that should never fail.
        stopifnot(length(chunkdim) == length(dim))
        chunkdim <- as.integer(pmin(dim, chunkdim))
    }
    chunkdim
}

