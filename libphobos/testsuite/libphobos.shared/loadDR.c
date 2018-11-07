#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>
#include <assert.h>

int main(int argc, char* argv[])
{
    if (argc != 2)
        return EXIT_FAILURE;
    void *h = dlopen(argv[1], RTLD_LAZY); // load druntime
    assert(h != NULL);

    int (*rt_init)(void) = dlsym(h, "rt_init");
    int (*rt_term)(void) = dlsym(h, "rt_term");
    void* (*rt_loadLibrary)(const char*) = dlsym(h, "rt_loadLibrary");
    int (*rt_unloadLibrary)(void*) = dlsym(h, "rt_unloadLibrary");

    int res = EXIT_FAILURE;
    if (!rt_init()) goto Lexit;

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

    if (rt_term()) res = EXIT_SUCCESS;

Lexit:
    assert(dlclose(h) == 0);
    return res;
}
