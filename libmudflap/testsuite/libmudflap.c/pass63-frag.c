/* Check -Wno-mudflap flag */
/* { dg-do compile } */
/* { dg-options "-fmudflap -Wno-mudflap" } */

extern char x[];
int main() { return x[3]; }  /* { dg-bogus "mudflap cannot track" } */
