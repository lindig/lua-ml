# Sortfile

This code is for the following task: sort a textfile according to the last
number in each line. This challenge was given student applicants by friends
and I took it to develop a program in C with some attention to performance.

## Compiling

To compile this code, use a Makefile that can be extracted from this file
using

        lipsum tangle Makefile sortlines.lp > Makefile
        make
        
<<Makefile>>=
all: sortlines

sortlines.c: sortlines.lp
	lipsum tangle -f cpp $@ $< > $@

sortlines: sortlines.c
	gcc -O -o $@ $<
@

## Types and Main Function
		
The idea is to read the file into memory, to build up an array of pointers
to each line in the file, to sort the array, and finally to emit the lines in
sorting order using the sorted array.

<<sortlines.c>>=
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

<<type definitions>>
<<read file into memory>>
<<scan lines>>
<<sort predicate cmp>>
<<main function>>
@ 

Possible challenges include multiple numbers in a line and trying to avoid to
scan a line multiple times for the detection of the last number in it.

The key data structure is `LINE`. It contains a pointer into the file, the
length of the line and an `int n` for the value of last number in that line.

<<type definitions>>=
typedef struct line {
	char *line; /* byte sequence */
	int length;
	int  n;	/* sort key */
} LINE;      

@ The sort predicate on lines is defined by `cmp`. It derives an integer by
subtracting the numbers belonging to the two lines being compared. The sign of
the integer signals the order to the sort function.

<<sort predicate cmp>>=
static int cmp(const LINE *x, const LINE *y)
{
	return x->n - y->n;
}

@ The main function reads the file into memory, scans it into lines, sorts the
lines, and emits them.

<<main function>>=
int main (int argc, const char * argv[]) 
{
	int n,l;
	LINE *lines;
	char *buffer;
        long bufsize;

	if (argc != 2) {
		fprintf(stderr,"usage: %s file\n",argv[0]);
		return 1;
	}

	buffer = readfile(argv[1], &bufsize);
	if (!buffer)
		return 1;
	if (!bufsize)
		return 0; /* file empty */

	lines = scan_buffer(&n, buffer, bufsize);
	if (!lines) {
		perror("can't allocate");
		return 1;
	}

	/* sort line array and emit each line to stdout */
	mergesort(lines, n, sizeof(LINE), 
		(int (*)(const void *, const void *))cmp);
	for (l=0; l<n; l++)
		fwrite(lines[l].line, lines[l].length, 1, stdout);

	free(lines);
	free(buffer);
	return 0;
}
@


## Reading a file into memory

Reading a file into memory requires to allocate the right amount of memory.
We actually allocate one more byte and add a newline if the last line does not
end with a newline already. This makes line scanning more regular. Otherwise
we have to pay attention to errors as not being able to read the file or
running out of memory.

Function `readfile` returns a buffer allocated for the file and writes the
size of the buffer to `bufsize`.

        
<<read file into memory>>=
static char *readfile(const char *name, long *bufsize)
{
	long size, read;
	char *buffer;
	FILE *file;
	
	file = fopen(name, "rb");
	if (!file) {
		perror("can't open file");
		return NULL;
	}
			   
	fseek(file, 0, SEEK_END);
	size = ftell(file);
	fseek(file, 0, SEEK_SET);
	
	/* we allocate one more byte that we might fill with \n */
	buffer = malloc(size+1);
	if (!buffer) {
		perror("can't allocate memory");
		return NULL;
	} 
	read = fread(buffer, 1, size, file);
	if (read != size) {
		perror("reading from file failed");
		return NULL;
	}
	fclose(file);

	/* if the last character in the file is not a \n we add it */
	if (*(buffer+size-1) != '\n') {
		*(buffer+size) = '\n';
		size++;
	}
	
	*bufsize = size;
	return buffer;
}
@

## Parsing a buffer into lines

The `scan_buffer` routine scans the buffer byte by byte and each line it finds
it adds to a `LINE` struct. Since we can't know the number of lines, an
initial number `m` is guessed. When it is exceeded, ``2*m`` are
allocated (and again doubling if necessary).

The tricky bit is to recognize the last number (and sort key) in a line and
entering it into the `LINE` struct. Rather than relying to `atoi(3)` I'm using
a small hack `C2I` that computes the value of a single digit and from there
computes the number when we find more digits.

When we find a new number we never know wether it is the last number in a
line. Hence we read it but overwrite any previous result. The `outside` flag
is true, if we are outside a sequence of digits and if so we can go a little
faster.

We don't recognize negative numbers.

<<scan lines>>=
#define C2I(c) ((c - '0'))        
        
LINE *scan_buffer(int *n, char *buffer, long bufsize) 

{
	char *c = buffer;
	char *line = buffer;
	int outside = 1; /* true iff outside of digits sequence */
	int number = -1; /* last number we read */
	
	int m = 100 + bufsize/40; /* max number of lines we can store */
	LINE *lines = malloc(m*sizeof(LINE));
	if (!lines) {
		return NULL;
	}
	*n = 0; /* number of lines read */
	LINE *l = lines; /* current line */

	
	while(c < buffer+bufsize) {
		switch (*c) {
			case '\n':
				/* store line */
				l->line   = line;
				l->n      = number;
				l->length = c-line+1;
				(*n)++;
				
				/* prepare for next line */
				number = -1;
				outside = 1;
				l++;
				c++;
				line = c;

				/* make room for more lines to store */
				if (*n == m) {
					lines = realloc(lines, 2*m*sizeof(LINE));
					if (!lines) {
						return NULL;
					}
					m *= 2;
					l = &lines[*n];
				}
				
				break;
				
			case '0': /* scan a number */
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
				if (outside) {
					number = C2I(*c);
					/* we are inside a digit sequence now */
					outside = 0; 
				} else {
					number = number * 10 + C2I(*c);
				}
				c++;
				break;
			default:
				outside = 1; /* outside a digit sequence */
				c++;
				break;
		}
	}
	return lines;
}

