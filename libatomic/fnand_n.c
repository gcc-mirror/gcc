#define LAT_FNAND_N
#define NAME	nand
#define OP(X,Y)	~((X) & (Y))
#include "fop_n.c"
#undef LAT_FNAND_N
