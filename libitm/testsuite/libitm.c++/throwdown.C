// { dg-do compile }

#include <libitm.h>

static void throwit() {
	throw 1;
}

void tranfunc() {
	__transaction_atomic {
		throwit();
	}
}
