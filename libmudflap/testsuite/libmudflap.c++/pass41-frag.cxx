#include <string>
#include <iostream>

int
main (int argc, char *argv[])
{
    std::string myStr = "Hello, World!";
    std::cout << myStr << std::endl;
    return 0;
}

/* Ignore a warning that is irrelevant to the purpose of this test.  */
/* { dg-prune-output ".*mudflap cannot track unknown size extern.*" } */
