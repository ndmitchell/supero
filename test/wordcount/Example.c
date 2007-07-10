
#define __NO_CTYPE_INLINES /* stop inlining for isspace() */
#define _MT /* stop inlining for getchar() */


#include "stdio.h"
#include "ctype.h"

int main()
{
	int i = 0;
	int c, last_space = 1, this_space;
	while ((c = getchar()) != EOF) {
		this_space = isspace(c);
		if (last_space && !this_space)
			i++;
		last_space = this_space;
	}
	printf("%i\n", i);
	return 0;
}
