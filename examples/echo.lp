
Echo prints each command line argument on a line by itself. This 
documentation chunk extends until the beginning of the named code
chunk below. 

<<echo.c>>=
/* <<copyright>> */
#include <stdio.h>

int main(int argc, char** argv)
{
        int i;
        for (i=0; i<argc; i++)
                puts(argv[i]);
        return 0;
}

@ By keeping the copyright notice in a chunk by itself it is easy to
include it in several files. This documenation chunk starts with 
an @ followed by a space and extends until the beginning of the next 
chunk.

<<copyright>>= 
This code is in the public domain.

<<copyright>>= 
This code is part of the documentation for Lipsum.