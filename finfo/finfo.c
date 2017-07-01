#include <magic.h>
#include <R.h>
#include <string.h>

static magic_t magic = NULL;

void finfo_open(int *flags, char** magicfile)
{
    if (NULL == magic) {
        magic = magic_open(*flags);

        if (NULL == magic) {
            error("Unable to initialize the library");
        }
    }

    if (0 != magic_load(magic, magicfile ? *magicfile : NULL)) {
        magic_close(magic);
        error("Can't load magic database: %s\n", magic_error(magic));
    }
}

void finfo_buffer(char** buffer, const char** out)
{
    *out = strdup(magic_buffer(magic, *buffer, strlen(*buffer)));
}

void finfo_file(char** filename, const char** out)
{
    *out = strdup(magic_file(magic, *filename));
}
