#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>
#include <assert.h>

extern void* rt_loadLibrary(const char*);
extern int rt_unloadLibrary(void*);
extern int rt_init(void);
extern int rt_term(void);

int main(int argc, char* argv[])
{
    if (!rt_init()) return EXIT_FAILURE;
    const size_t pathlen = strrchr(argv[0], '/') - argv[0] + 1;
    char *name = malloc(pathlen + sizeof("lib.so"));
    memcpy(name, argv[0], pathlen);
    memcpy(name+pathlen, "lib.so", sizeof("lib.so"));

    void *dlib = rt_loadLibrary(name);
    free(name);
    assert(dlib);

    int (*runTests)(void) = dlsym(dlib, "runTests");
    assert(runTests());
    assert(rt_unloadLibrary(dlib));
    if (!rt_term()) return EXIT_FAILURE;
    return EXIT_SUCCESS;
}
