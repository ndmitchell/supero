
#include "stdio.h"

int main()
{
	int i = 0;
	int last = 0;
	while ((last = getchar()) != EOF) {
		if (last == '\n')
			i++;
	}
	if (last == '\n')
		i--;
	printf("%i\n", i);
	return 0;
}
