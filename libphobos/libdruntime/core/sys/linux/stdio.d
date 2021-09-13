/**
  * D header file for GNU stdio.
  *
  * Copyright: Danny Milosavljevic 2014
  * License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>.
  * Authors: Danny Milosavljevic
  */
module core.sys.linux.stdio;
version (CRuntime_Glibc):
public import core.sys.posix.stdio;
import core.sys.posix.sys.types : ssize_t, off64_t = off_t;
import core.sys.linux.config : __USE_FILE_OFFSET64;
import core.stdc.stdio : FILE;
import core.stdc.stddef : wchar_t;

@system:

extern(C) nothrow
{
    alias ssize_t function(void *cookie, char *buf, size_t size) cookie_read_function_t;
    alias ssize_t function(void *cookie, const(char) *buf, size_t size) cookie_write_function_t;
    alias int function(void *cookie, off64_t *offset, int whence) cookie_seek_function_t;
    alias int function(void *cookie) cookie_close_function_t;

    struct cookie_io_functions_t
    {
        cookie_read_function_t read;
        cookie_write_function_t write;
        cookie_seek_function_t seek;
        cookie_close_function_t close;
    }
    FILE* fopencookie(in void* cookie, in char* mode, cookie_io_functions_t io_funcs);
    void setbuffer(FILE *stream, char *buf, size_t size); // note: _DEFAULT_SOURCE
}

unittest
{
    static int flag = 0;
    static int written = 0;
    static int closed = 0;
    // Note: this test needs to run on both a 32 and a 64 bit machine, both have to pass.
    import core.stdc.errno : errno, EBADF;
    //import core.sys.posix.sys.stat : off64_t;
    import core.stdc.stdio : FILE, fflush, fileno, fprintf, fgetc, EOF, fclose;
    import core.stdc.string : memcpy, memset;
    extern (C) ssize_t specialRead(void *cookie, char *buf, size_t size) nothrow
    {
        memset(buf, 'a', size);
        return size;
    }
    extern (C) int specialClose(void* cookie) nothrow
    {
        ++closed;
        return 0;
    }
    extern (C) ssize_t specialWrite(void *cookie, const(char) *buf, size_t size) nothrow
    {
        int* c = cast(int*) cookie;
        flag = *c;
        written += size;
        return size;
    }
    int dummy = 42;
    cookie_io_functions_t fns =
    {
        read: &specialRead,
        write: &specialWrite,
        close: &specialClose,
    };
    FILE* f = fopencookie(&dummy, "r+", fns);
    assert(f !is null);
    //File.open();
    //auto g = File(f);
    assert(fileno(f) == -1 && errno == EBADF);
    assert(fprintf(f, "hello") == "hello".length);
    //assert(errno == EBADF);
    assert(fflush(f) == 0);
    assert(written == "hello".length);
    // Note: do not swap reading and writing here.
    int c = 0;
    while ((c = fgetc(f)) != EOF)
    {
        assert(c == 'a');
        break; // endless otherwise
    }
    assert(c == 'a');
    assert(fclose(f) == 0);
    assert(closed == 1);
    assert(flag == dummy);
    //stdin.getFP();
    //assert(stdin.fileno() == 0);
}

unittest
{ /* setbuffer */
    char buf;
    int c;
    byte[10] dummy = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    FILE* f = fmemopen(dummy.ptr, 10, "r");
    assert(f !is null);
    setbuffer(f, &buf, 1);
    assert(fgetc(f) == 1);
    assert(fgetc(f) == 2);
    assert(fgetc(f) == 3);
    assert(fgetc(f) == 4);
    assert(fclose(f) == 0);
}
