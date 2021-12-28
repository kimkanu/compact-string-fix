#define ENCODING ASCII
#define TYPE ASCII
#define DESCRIPTION ASCII
#define LONG_DESCRIPTION ASCII. \
        Note that not all characters can be encoded in ASCII, \
        if encoding is not possible the function will raise an error.
#define IF_NOT_REPRESENT(a) a
#define IF_FIXED(a,b) b
#include "specialized.include"
