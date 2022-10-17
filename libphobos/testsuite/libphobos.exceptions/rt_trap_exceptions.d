// { dg-shouldfail "uncaught exception" }
// { dg-output "gcc.deh.*: uncaught exception" }
// Code adapted from
// http://arsdnet.net/this-week-in-d/2016-aug-07.html
extern extern(C) __gshared bool rt_trapExceptions;
extern extern(C) int _d_run_main(int, char**, void*) @system;

extern(C) int main(int argc, char** argv) {
    rt_trapExceptions = false;
    return _d_run_main(argc, argv, &_main);
}

int _main() {
    throw new Exception("this will abort");
}
