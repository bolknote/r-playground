#!/usr/bin/env Rscript

MAGIC_NONE  = 0x0000000L # No flags
MAGIC_DEBUG = 0x0000001L # Turn on debugging
MAGIC_SYMLINK = 0x0000002L # Follow symlinks
MAGIC_COMPRESS = 0x0000004L # Check inside compressed files
MAGIC_DEVICES = 0x0000008L # Look at the contents of devices
MAGIC_MIME_TYPE = 0x0000010L # Return the MIME type
MAGIC_CONTINUE = 0x0000020L # Return all matches
MAGIC_CHECK = 0x0000040L # Print warnings to stderr
MAGIC_PRESERVE_ATIME = 0x0000080L # Restore access time on exit
MAGIC_RAW = 0x0000100L # Don't convert unprintable chars
MAGIC_ERROR = 0x0000200L # Handle ENOENT etc as real errors
MAGIC_MIME_ENCODING = 0x0000400L # Return the MIME encoding
MAGIC_MIME = MAGIC_MIME_TYPE + MAGIC_MIME_ENCODING
MAGIC_APPLE = 0x0000800L # Return the Apple creator/type
MAGIC_EXTENSION = 0x1000000L # Return a /-separated list of extensions
MAGIC_COMPRESS_TRANSP = 0x2000000L # Check inside compressed files but not report compression
MAGIC_NODESC = MAGIC_EXTENSION + MAGIC_MIME + MAGIC_APPLE
MAGIC_NO_CHECK_COMPRESS = 0x0001000L # Don't check for compressed files
MAGIC_NO_CHECK_TAR = 0x0002000L # Don't check for tar files
MAGIC_NO_CHECK_SOFT = 0x0004000L # Don't check magic entries
MAGIC_NO_CHECK_APPTYPE = 0x0008000L # Don't check application type
MAGIC_NO_CHECK_ELF = 0x0010000L # Don't check for elf details
MAGIC_NO_CHECK_TEXT = 0x0020000L # Don't check for text files
MAGIC_NO_CHECK_CDF = 0x0040000L # Don't check for cdf files
MAGIC_NO_CHECK_TOKENS = 0x0100000L # Don't check tokens
MAGIC_NO_CHECK_ENCODING = 0x0200000L # Don't check text encodings

library(methods)

FInfo <- setRefClass("FInfo",
	methods = c(
		initialize = function(flag, magicfile = NULL) {
			dyn.load(paste0('finfo', .Platform$dynlib))
			.C('finfo_open', as.integer(flag), as.character(magicfile))
		},
		buffer = function(string) {
			.C('finfo_buffer', string, out = "")$out
		},
		file = function(filename) {
			.C('finfo_file', filename, out = "")$out
		}
	)
)

# Example

fi <- FInfo$new(MAGIC_NONE)
cat(fi$file('/Users/bolk/Downloads/hipstory-vladimir-lenin.jpg'))
