/*
 * random-number generator for testing
 */
static unsigned long next = 1;

/*
 - range - generate a random number within an inclusive range
 *
 * Algorithm from ANSI C standard.  Limitation:  max-min <= 32767.
 */
int
range(min, max)
int min;
int max;
{
	register int temp;

	next = next * 1103515245 + 12345;
	temp = (int)((next/65536)%32768);
	return(temp%(max - min + 1) + min);
}

/*
 - seed - seed random number generator
 */
void
seed(n)
long n;
{
	next = (unsigned long)n;
}
