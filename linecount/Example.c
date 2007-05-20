
#include "stdio.h"

int main()
{
	int count;
	int last_newline = 0, c;
	c = getchar();
	if (c == EOF)
		count = 0;
	else
	{
		count = 1;
		while ((c = getchar()) != EOF) {
			if (last_newline = (c == '\n'))
				count++;
		}
		if (last_newline)
			count--;
	}
	printf("%i\n", count);
	return 0;
}
