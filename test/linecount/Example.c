
#include "stdio.h"

int main()
{
	int count = 0;
	int last_newline = 1, c;
	while ((c = getchar()) != EOF) {
		if (last_newline)
			count++;
		last_newline = (c == '\n');
	}
	printf("%i\n", count);
	return 0;
}
