// Written in the D programming language.

/**
 * Read and write memory mapped files.
 *
 * Memory mapped files are a mechanism in operating systems that allows
 * file access through virtual memory. After opening a file with `MmFile`,
 * the contents can be read from or written to with standard slice / pointer operations.
 * Changes to the memory are automatically reflected in the underlying file.
 *
 * Memory mapping can increase I/O performance of large files, compared to buffered
 * read / write operations from `std.file` and `std.stdio`. However, I/O errors are
 * not handled as safely: when for example the disk that the file is on gets removed,
 * reading from it may result in a segfault.
 *
 * Copyright: Copyright The D Language Foundation 2004 - 2009.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   $(HTTP digitalmars.com, Walter Bright),
 *            Matthew Wilson
 * References: $(LINK https://en.wikipedia.org/wiki/Memory-mapped_file)
 * Source:    $(PHOBOSSRC std/mmfile.d)
 *
 * $(SCRIPT inhibitQuickIndex = 1;)
 */
/*          Copyright The D Language Foundation 2004 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE_1_0.txt or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module std.mmfile;

import core.stdc.errno;
import core.stdc.stdio;
import core.stdc.stdlib;
import std.conv, std.exception, std.stdio;
import std.file;
import std.path;
import std.string;

import std.internal.cstring;

//debug = MMFILE;

version (Windows)
{
    import core.sys.windows.winbase;
    import core.sys.windows.winnt;
    import std.utf;
    import std.windows.syserror;
}
else version (Posix)
{
    import core.sys.posix.fcntl;
    import core.sys.posix.sys.mman;
    import core.sys.posix.sys.stat;
    import core.sys.posix.unistd;
}
else
{
    static assert(0);
}

/**
 * MmFile objects control the memory mapped file resource.
 */
class MmFile
{
    /**
     * The mode the memory mapped file is opened with.
     */
    enum Mode
    {
        read,            /// Read existing file
        readWriteNew,    /// Delete existing file, write new file
        readWrite,       /// Read/Write existing file, create if not existing
        readCopyOnWrite, /// Read/Write existing file, copy on write
    }

    /**
     * Open memory mapped file filename for reading.
     * File is closed when the object instance is deleted.
     * Throws:
     *  - On POSIX, $(REF ErrnoException, std, exception).
     *  - On Windows, $(REF WindowsException, std, windows, syserror).
     */
    this(string filename) scope
    {
        this(filename, Mode.read, 0, null);
    }

    version (linux) this(File file, Mode mode = Mode.read, ulong size = 0,
            void* address = null, size_t window = 0) scope
    {
        // Save a copy of the File to make sure the fd stays open.
        this.file = file;
        this(file.fileno, mode, size, address, window);
    }

    version (linux) private this(int fildes, Mode mode, ulong size,
            void* address, size_t window) scope
    {
        int oflag;
        int fmode;

        final switch (mode)
        {
        case Mode.read:
            flags = MAP_SHARED;
            prot = PROT_READ;
            oflag = O_RDONLY;
            fmode = 0;
            break;

        case Mode.readWriteNew:
            assert(size != 0);
            flags = MAP_SHARED;
            prot = PROT_READ | PROT_WRITE;
            oflag = O_CREAT | O_RDWR | O_TRUNC;
            fmode = octal!660;
            break;

        case Mode.readWrite:
            flags = MAP_SHARED;
            prot = PROT_READ | PROT_WRITE;
            oflag = O_CREAT | O_RDWR;
            fmode = octal!660;
            break;

        case Mode.readCopyOnWrite:
            flags = MAP_PRIVATE;
            prot = PROT_READ | PROT_WRITE;
            oflag = O_RDWR;
            fmode = 0;
            break;
        }

        fd = fildes;

        // Adjust size
        stat_t statbuf = void;
        errnoEnforce(fstat(fd, &statbuf) == 0);
        if (prot & PROT_WRITE && size > statbuf.st_size)
        {
            // Need to make the file size bytes big
            lseek(fd, cast(off_t)(size - 1), SEEK_SET);
            char c = 0;
            core.sys.posix.unistd.write(fd, &c, 1);
        }
        else if (prot & PROT_READ && size == 0)
            size = statbuf.st_size;
        this.size = size;

        // Map the file into memory!
        size_t initial_map = (window && 2*window<size)
            ? 2*window : cast(size_t) size;
        auto p = mmap(address, initial_map, prot, flags, fd, 0);
        if (p == MAP_FAILED)
        {
            errnoEnforce(false, "Could not map file into memory");
        }
        data = p[0 .. initial_map];
    }

    /**
     * Open memory mapped file filename in mode.
     * File is closed when the object instance is deleted.
     * Params:
     *  filename = name of the file.
     *      If null, an anonymous file mapping is created.
     *  mode = access mode defined above.
     *  size =  the size of the file. If 0, it is taken to be the
     *      size of the existing file.
     *  address = the preferred address to map the file to,
     *      although the system is not required to honor it.
     *      If null, the system selects the most convenient address.
     *  window = preferred block size of the amount of data to map at one time
     *      with 0 meaning map the entire file. The window size must be a
     *      multiple of the memory allocation page size.
     * Throws:
     *  - On POSIX, $(REF ErrnoException, std, exception).
     *  - On Windows, $(REF WindowsException, std, windows, syserror).
     */
    this(string filename, Mode mode, ulong size, void* address,
            size_t window = 0) scope
    {
        this.filename = filename;
        this.mMode = mode;
        this.window = window;
        this.address = address;

        version (Windows)
        {
            void* p;
            uint dwDesiredAccess2;
            uint dwShareMode;
            uint dwCreationDisposition;
            uint flProtect;

            final switch (mode)
            {
            case Mode.read:
                dwDesiredAccess2 = GENERIC_READ;
                dwShareMode = FILE_SHARE_READ;
                dwCreationDisposition = OPEN_EXISTING;
                flProtect = PAGE_READONLY;
                dwDesiredAccess = FILE_MAP_READ;
                break;

            case Mode.readWriteNew:
                assert(size != 0);
                dwDesiredAccess2 = GENERIC_READ | GENERIC_WRITE;
                dwShareMode = FILE_SHARE_READ | FILE_SHARE_WRITE;
                dwCreationDisposition = CREATE_ALWAYS;
                flProtect = PAGE_READWRITE;
                dwDesiredAccess = FILE_MAP_WRITE;
                break;

            case Mode.readWrite:
                dwDesiredAccess2 = GENERIC_READ | GENERIC_WRITE;
                dwShareMode = FILE_SHARE_READ | FILE_SHARE_WRITE;
                dwCreationDisposition = OPEN_ALWAYS;
                flProtect = PAGE_READWRITE;
                dwDesiredAccess = FILE_MAP_WRITE;
                break;

            case Mode.readCopyOnWrite:
                dwDesiredAccess2 = GENERIC_READ | GENERIC_WRITE;
                dwShareMode = FILE_SHARE_READ | FILE_SHARE_WRITE;
                dwCreationDisposition = OPEN_EXISTING;
                flProtect = PAGE_WRITECOPY;
                dwDesiredAccess = FILE_MAP_COPY;
                break;
            }

            if (filename != null)
            {
                hFile = CreateFileW(filename.tempCStringW(),
                        dwDesiredAccess2,
                        dwShareMode,
                        null,
                        dwCreationDisposition,
                        FILE_ATTRIBUTE_NORMAL,
                        cast(HANDLE) null);
                wenforce(hFile != INVALID_HANDLE_VALUE, "CreateFileW");
            }
            else
                hFile = INVALID_HANDLE_VALUE;

            scope(failure)
            {
                if (hFile != INVALID_HANDLE_VALUE)
                {
                    CloseHandle(hFile);
                    hFile = INVALID_HANDLE_VALUE;
                }
            }

            int hi = cast(int)(size >> 32);
            hFileMap = CreateFileMappingW(hFile, null, flProtect,
                    hi, cast(uint) size, null);
            wenforce(hFileMap, "CreateFileMapping");
            scope(failure)
            {
                CloseHandle(hFileMap);
                hFileMap = null;
            }

            if (size == 0 && filename != null)
            {
                uint sizehi;
                uint sizelow = GetFileSize(hFile, &sizehi);
                wenforce(sizelow != INVALID_FILE_SIZE || GetLastError() != ERROR_SUCCESS,
                    "GetFileSize");
                size = (cast(ulong) sizehi << 32) + sizelow;
            }
            this.size = size;

            size_t initial_map = (window && 2*window<size)
                ? 2*window : cast(size_t) size;
            p = MapViewOfFileEx(hFileMap, dwDesiredAccess, 0, 0,
                    initial_map, address);
            wenforce(p, "MapViewOfFileEx");
            data = p[0 .. initial_map];

            debug (MMFILE) printf("MmFile.this(): p = %p, size = %d\n", p, size);
        }
        else version (Posix)
        {
            void* p;
            int oflag;
            int fmode;

            final switch (mode)
            {
            case Mode.read:
                flags = MAP_SHARED;
                prot = PROT_READ;
                oflag = O_RDONLY;
                fmode = 0;
                break;

            case Mode.readWriteNew:
                assert(size != 0);
                flags = MAP_SHARED;
                prot = PROT_READ | PROT_WRITE;
                oflag = O_CREAT | O_RDWR | O_TRUNC;
                fmode = octal!660;
                break;

            case Mode.readWrite:
                flags = MAP_SHARED;
                prot = PROT_READ | PROT_WRITE;
                oflag = O_CREAT | O_RDWR;
                fmode = octal!660;
                break;

            case Mode.readCopyOnWrite:
                flags = MAP_PRIVATE;
                prot = PROT_READ | PROT_WRITE;
                oflag = O_RDWR;
                fmode = 0;
                break;
            }

            if (filename.length)
            {
                fd = .open(filename.tempCString(), oflag, fmode);
                errnoEnforce(fd != -1, "Could not open file "~filename);

                stat_t statbuf;
                if (fstat(fd, &statbuf))
                {
                    //printf("\tfstat error, errno = %d\n", errno);
                    .close(fd);
                    fd = -1;
                    errnoEnforce(false, "Could not stat file "~filename);
                }

                if (prot & PROT_WRITE && size > statbuf.st_size)
                {
                    // Need to make the file size bytes big
                    .lseek(fd, cast(off_t)(size - 1), SEEK_SET);
                    char c = 0;
                    core.sys.posix.unistd.write(fd, &c, 1);
                }
                else if (prot & PROT_READ && size == 0)
                    size = statbuf.st_size;
            }
            else
            {
                fd = -1;
                flags |= MAP_ANON;
            }
            this.size = size;
            size_t initial_map = (window && 2*window<size)
                ? 2*window : cast(size_t) size;
            p = mmap(address, initial_map, prot, flags, fd, 0);
            if (p == MAP_FAILED)
            {
                if (fd != -1)
                {
                    .close(fd);
                    fd = -1;
                }
                errnoEnforce(false, "Could not map file "~filename);
            }

            data = p[0 .. initial_map];
        }
        else
        {
            static assert(0);
        }
    }

    /**
     * Flushes pending output and closes the memory mapped file.
     */
    ~this() scope
    {
        debug (MMFILE) printf("MmFile.~this()\n");
        unmap();
        data = null;
        version (Windows)
        {
            wenforce(hFileMap == null || CloseHandle(hFileMap) == TRUE,
                    "Could not close file handle");
            hFileMap = null;

            wenforce(!hFile || hFile == INVALID_HANDLE_VALUE
                    || CloseHandle(hFile) == TRUE,
                    "Could not close handle");
            hFile = INVALID_HANDLE_VALUE;
        }
        else version (Posix)
        {
            version (linux)
            {
                if (file !is File.init)
                {
                    // The File destructor will close the file,
                    // if it is the only remaining reference.
                    return;
                }
            }
            errnoEnforce(fd == -1 || fd <= 2
                    || .close(fd) != -1,
                    "Could not close handle");
            fd = -1;
        }
        else
        {
            static assert(0);
        }
    }

    /* Flush any pending output.
     */
    void flush()
    {
        debug (MMFILE) printf("MmFile.flush()\n");
        version (Windows)
        {
            FlushViewOfFile(data.ptr, data.length);
        }
        else version (Posix)
        {
            int i;
            i = msync(cast(void*) data, data.length, MS_SYNC);   // sys/mman.h
            errnoEnforce(i == 0, "msync failed");
        }
        else
        {
            static assert(0);
        }
    }

    /**
     * Gives size in bytes of the memory mapped file.
     */
    @property ulong length() const
    {
        debug (MMFILE) printf("MmFile.length()\n");
        return size;
    }

    /**
     * Forwards `length`.
     */
    alias opDollar = length;

    /**
     * Read-only property returning the file mode.
     */
    Mode mode()
    {
        debug (MMFILE) printf("MmFile.mode()\n");
        return mMode;
    }

    /**
     * Returns entire file contents as an array.
     */
    void[] opSlice()
    {
        debug (MMFILE) printf("MmFile.opSlice()\n");
        return opSlice(0,size);
    }

    /**
     * Returns slice of file contents as an array.
     */
    void[] opSlice(ulong i1, ulong i2)
    {
        debug (MMFILE) printf("MmFile.opSlice(%lld, %lld)\n", i1, i2);
        ensureMapped(i1,i2);
        size_t off1 = cast(size_t)(i1-start);
        size_t off2 = cast(size_t)(i2-start);
        return data[off1 .. off2];
    }

    /**
     * Returns byte at index i in file.
     */
    ubyte opIndex(ulong i)
    {
        debug (MMFILE) printf("MmFile.opIndex(%lld)\n", i);
        ensureMapped(i);
        size_t off = cast(size_t)(i-start);
        return (cast(ubyte[]) data)[off];
    }

    /**
     * Sets and returns byte at index i in file to value.
     */
    ubyte opIndexAssign(ubyte value, ulong i)
    {
        debug (MMFILE) printf("MmFile.opIndex(%lld, %d)\n", i, value);
        ensureMapped(i);
        size_t off = cast(size_t)(i-start);
        return (cast(ubyte[]) data)[off] = value;
    }


    // return true if the given position is currently mapped
    private int mapped(ulong i)
    {
        debug (MMFILE) printf("MmFile.mapped(%lld, %lld, %d)\n", i,start,
                data.length);
        return i >= start && i < start+data.length;
    }

    // unmap the current range
    private void unmap()
    {
        debug (MMFILE) printf("MmFile.unmap()\n");
        version (Windows)
        {
            wenforce(!data.ptr || UnmapViewOfFile(data.ptr) != FALSE, "UnmapViewOfFile");
        }
        else
        {
            errnoEnforce(!data.ptr || munmap(cast(void*) data, data.length) == 0,
                    "munmap failed");
        }
        data = null;
    }

    // map range
    private void map(ulong start, size_t len)
    {
        debug (MMFILE) printf("MmFile.map(%lld, %d)\n", start, len);
        void* p;
        if (start+len > size)
            len = cast(size_t)(size-start);
        version (Windows)
        {
            uint hi = cast(uint)(start >> 32);
            p = MapViewOfFileEx(hFileMap, dwDesiredAccess, hi, cast(uint) start, len, address);
            wenforce(p, "MapViewOfFileEx");
        }
        else
        {
            p = mmap(address, len, prot, flags, fd, cast(off_t) start);
            errnoEnforce(p != MAP_FAILED);
        }
        data = p[0 .. len];
        this.start = start;
    }

    // ensure a given position is mapped
    private void ensureMapped(ulong i)
    {
        debug (MMFILE) printf("MmFile.ensureMapped(%lld)\n", i);
        if (!mapped(i))
        {
            unmap();
            if (window == 0)
            {
                map(0,cast(size_t) size);
            }
            else
            {
                ulong block = i/window;
                if (block == 0)
                    map(0,2*window);
                else
                    map(window*(block-1),3*window);
            }
        }
    }

    // ensure a given range is mapped
    private void ensureMapped(ulong i, ulong j)
    {
        debug (MMFILE) printf("MmFile.ensureMapped(%lld, %lld)\n", i, j);
        if (!mapped(i) || !mapped(j-1))
        {
            unmap();
            if (window == 0)
            {
                map(0,cast(size_t) size);
            }
            else
            {
                ulong iblock = i/window;
                ulong jblock = (j-1)/window;
                if (iblock == 0)
                {
                    map(0,cast(size_t)(window*(jblock+2)));
                }
                else
                {
                    map(window*(iblock-1),cast(size_t)(window*(jblock-iblock+3)));
                }
            }
        }
    }

private:
    string filename;
    void[] data;
    ulong  start;
    size_t window;
    ulong  size;
    Mode   mMode;
    void*  address;
    version (linux) File file;

    version (Windows)
    {
        HANDLE hFile = INVALID_HANDLE_VALUE;
        HANDLE hFileMap = null;
        uint dwDesiredAccess;
    }
    else version (Posix)
    {
        int fd;
        int prot;
        int flags;
        int fmode;
    }
    else
    {
        static assert(0);
    }
}

/// Read an existing file
@system unittest
{
    import std.file;
    std.file.write(deleteme, "hello"); // deleteme is a temporary filename
    scope(exit) remove(deleteme);

    // Use a scope class so the file will be closed at the end of this function
    scope mmfile = new MmFile(deleteme);

    assert(mmfile.length == "hello".length);

    // Access file contents with the slice operator
    // This is typed as `void[]`, so cast to `char[]` or `ubyte[]` to use it
    const data = cast(const(char)[]) mmfile[];

    // At this point, the file content may not have been read yet.
    // In that case, the following memory access will intentionally
    // trigger a page fault, causing the kernel to load the file contents
    assert(data[0 .. 5] == "hello");
}

/// Write a new file
@system unittest
{
    import std.file;
    scope(exit) remove(deleteme);

    scope mmfile = new MmFile(deleteme, MmFile.Mode.readWriteNew, 5, null);
    assert(mmfile.length == 5);

    auto data = cast(ubyte[]) mmfile[];

    // This write to memory will be reflected in the file contents
    data[] = '\n';

    mmfile.flush();

    assert(std.file.read(deleteme) == "\n\n\n\n\n");
}

@system unittest
{
    import core.memory : GC;
    import std.file : deleteme;

    const size_t K = 1024;
    size_t win = 64*K; // assume the page size is 64K
    version (Windows)
    {
        /+ these aren't defined in core.sys.windows.windows so let's use default
         SYSTEM_INFO sysinfo;
         GetSystemInfo(&sysinfo);
         win = sysinfo.dwAllocationGranularity;
         +/
    }
    else version (Posix)
    {
        import core.sys.posix.unistd;
        win = cast(size_t) sysconf(_SC_PAGESIZE);
    }
    string test_file = std.file.deleteme ~ "-testing.txt";
    MmFile mf = new MmFile(test_file,MmFile.Mode.readWriteNew,
            100*K,null,win);
    ubyte[] str = cast(ubyte[])"1234567890";
    ubyte[] data = cast(ubyte[]) mf[0 .. 10];
    data[] = str[];
    assert( mf[0 .. 10] == str );
    data = cast(ubyte[]) mf[50 .. 60];
    data[] = str[];
    assert( mf[50 .. 60] == str );
    ubyte[] data2 = cast(ubyte[]) mf[20*K .. 60*K];
    assert( data2.length == 40*K );
    assert( data2[$-1] == 0 );
    mf[100*K-1] = cast(ubyte)'b';
    data2 = cast(ubyte[]) mf[21*K .. 100*K];
    assert( data2.length == 79*K );
    assert( data2[$-1] == 'b' );

    destroy(mf);

    std.file.remove(test_file);
    // Create anonymous mapping
    auto test = new MmFile(null, MmFile.Mode.readWriteNew, 1024*1024, null);
}

version (linux)
@system unittest // https://issues.dlang.org/show_bug.cgi?id=14868
{
    import std.file : deleteme;
    import std.typecons : scoped;

    // Test retaining ownership of File/fd

    auto fn = std.file.deleteme ~ "-testing.txt";
    scope(exit) std.file.remove(fn);
    File(fn, "wb").writeln("Testing!");
    scoped!MmFile(File(fn));

    // Test that unique ownership of File actually leads to the fd being closed

    auto f = File(fn);
    auto fd = f.fileno;
    {
        auto mf = scoped!MmFile(f);
        f = File.init;
    }
    assert(.close(fd) == -1);
}

// https://issues.dlang.org/show_bug.cgi?id=14994
// https://issues.dlang.org/show_bug.cgi?id=14995
@system unittest
{
    import std.file : deleteme;
    import std.typecons : scoped;

    // Zero-length map may or may not be valid on OSX and NetBSD
    version (OSX)
        import std.exception : verifyThrown = collectException;
    version (NetBSD)
        import std.exception : verifyThrown = collectException;
    else
        import std.exception : verifyThrown = assertThrown;

    auto fn = std.file.deleteme ~ "-testing.txt";
    scope(exit) std.file.remove(fn);
    verifyThrown(scoped!MmFile(fn, MmFile.Mode.readWrite, 0, null));
}

@system unittest
{
    MmFile shar = new MmFile(null, MmFile.Mode.readWrite, 10, null, 0);
    void[] output = shar[0 .. $];
}

@system unittest
{
    import std.file : deleteme;
    auto name = std.file.deleteme ~ "-test.tmp";
    scope(exit) std.file.remove(name);

    std.file.write(name, "abcd");
    {
        scope MmFile mmf = new MmFile(name);
        string p;

        assert(mmf[0] == 'a');
        p = cast(string) mmf[];
        assert(p[1] == 'b');
        p = cast(string) mmf[0 .. 4];
        assert(p[2] == 'c');
    }
    {
        scope MmFile mmf = new MmFile(name, MmFile.Mode.read, 0, null);
        string p;

        assert(mmf[0] == 'a');
        p = cast(string) mmf[];
        assert(mmf.length == 4);
        assert(p[1] == 'b');
        p = cast(string) mmf[0 .. 4];
        assert(p[2] == 'c');
    }
    std.file.remove(name);
    {
        scope MmFile mmf = new MmFile(name, MmFile.Mode.readWriteNew, 4, null);
        char[] p = cast(char[]) mmf[];
        p[] = "1234";
        mmf[3] = '5';
        assert(mmf[2] == '3');
        assert(mmf[3] == '5');
    }
    {
        string p = cast(string) std.file.read(name);
        assert(p[] == "1235");
    }
    {
        scope MmFile mmf = new MmFile(name, MmFile.Mode.readWriteNew, 4, null);
        char[] p = cast(char[]) mmf[];
        p[] = "5678";
        mmf[3] = '5';
        assert(mmf[2] == '7');
        assert(mmf[3] == '5');
        assert(cast(string) mmf[] == "5675");
    }
    {
        string p = cast(string) std.file.read(name);
        assert(p[] == "5675");
    }
    {
        scope MmFile mmf = new MmFile(name, MmFile.Mode.readWrite, 4, null);
        char[] p = cast(char[]) mmf[];
        assert(cast(char[]) mmf[] == "5675");
        p[] = "9102";
        mmf[2] = '5';
        assert(cast(string) mmf[] == "9152");
    }
    {
        string p = cast(string) std.file.read(name);
        assert(p[] == "9152");
    }
    std.file.remove(name);
    {
        scope MmFile mmf = new MmFile(name, MmFile.Mode.readWrite, 4, null);
        char[] p = cast(char[]) mmf[];
        p[] = "abcd";
        mmf[2] = '5';
        assert(cast(string) mmf[] == "ab5d");
    }
    {
        string p = cast(string) std.file.read(name);
        assert(p[] == "ab5d");
    }
    {
        scope MmFile mmf = new MmFile(name, MmFile.Mode.readCopyOnWrite, 4, null);
        char[] p = cast(char[]) mmf[];
        assert(cast(string) mmf[] == "ab5d");
        p[] = "9102";
        mmf[2] = '5';
        assert(cast(string) mmf[] == "9152");
    }
    {
        string p = cast(string) std.file.read(name);
        assert(p[] == "ab5d");
    }
}
