
#define _MT /* stop inlining for getchar() */

#include "stdio.h"

int main()
{
	int i = 0;
	while (getchar() != EOF)
		i++;
	printf("%i\n", i);
	return 0;
}
