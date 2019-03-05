#include <stdlib.h>
#include <assert.h>

extern int runTests(void);
extern int lib_init(void);
extern int lib_term(void);

int main(int argc, char* argv[])
{
    if (!lib_init()) return EXIT_SUCCESS;
    const int res = runTests() ? EXIT_SUCCESS : EXIT_FAILURE;
    if (!lib_term()) return EXIT_FAILURE;
    return res;
}
