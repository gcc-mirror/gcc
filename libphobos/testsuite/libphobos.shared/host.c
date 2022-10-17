#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>
#include <assert.h>

int main(int argc, char* argv[])
{
#if defined(__FreeBSD__)
    // workaround for Bugzilla 14824
    void *druntime = dlopen(argv[1], RTLD_LAZY); // load druntime
    assert(druntime);
#endif
#if defined(__DragonFly__)
    // workaround for Bugzilla 14824
    void *druntime = dlopen(argv[1], RTLD_LAZY); // load druntime
    assert(druntime);
#endif

    const size_t pathlen = strrchr(argv[0], '/') - argv[0] + 1;
    char *name = malloc(pathlen + sizeof("plugin1.so"));
    memcpy(name, argv[0], pathlen);
    memcpy(name+pathlen, "plugin1.so", sizeof("plugin1.so"));

    void* plugin1 = dlopen(name, RTLD_LAZY);
    name[pathlen + sizeof("plugin1.so") - 5] = '2';
    void* plugin2 = dlopen(name, RTLD_LAZY);

    int (*plugin1_init)() = dlsym(plugin1, "plugin_init");
    int (*plugin1_term)() = dlsym(plugin1, "plugin_term");
    int (*runTests1)() = dlsym(plugin1, "runTests");
    int (*plugin2_init)() = dlsym(plugin2, "plugin_init");
    int (*plugin2_term)() = dlsym(plugin2, "plugin_term");
    int (*runTests2)() = dlsym(plugin2, "runTests");
    assert(plugin1_init());
    assert(runTests1());
    assert(plugin2_init());
    assert(runTests2());

    assert(plugin1_term());
    assert(dlclose(plugin1) == 0);
    assert(runTests2());

    name[pathlen + sizeof("plugin1.so") - 5] = '1';
    plugin1 = dlopen(name, RTLD_LAZY);
    plugin1_init = dlsym(plugin1, "plugin_init");
    plugin1_term = dlsym(plugin1, "plugin_term");
    runTests1 = dlsym(plugin1, "runTests");
    assert(plugin1_init());
    assert(runTests1());
    assert(runTests2());

    assert(plugin2_term());
    assert(dlclose(plugin2) == 0);
    assert(runTests1());

    assert(plugin1_term());
    assert(dlclose(plugin1) == 0);

    free(name);

#if defined(__FreeBSD__)
    dlclose(druntime);
#endif
#if defined(__DragonFly__)
    dlclose(druntime);
#endif
    return EXIT_SUCCESS;
}
