#include <stdlib.h>
#include <ctype.h>

int main ()
{
  char* buf = "hello"; 
  return ! ((toupper (buf[0]) == 'H' && toupper ('z') == 'Z' &&
             tolower (buf[4]) == 'o' && tolower ('X') == 'x' &&
             isdigit (buf[3])) == 0 && isalnum ('4'));
}

/* { dg-warning "cannot track unknown size extern .__ctype." "Solaris __ctype declared without size" { target *-*-solaris2.* } 0 } */
