/* VMRuntime.java -- VM interface to Runtime
   Copyright (C) 2003 Free Software Foundation

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

package java.lang;

import java.io.File;
import java.io.IOException;

/**
 * VMRuntime represents the interface to the Virtual Machine.
 *
 * @author Jeroen Frijters
 */
final class VMRuntime
{
    /**
     * No instance is ever created.
     */
    private VMRuntime() 
    {
    }

    /**
     * Returns the number of available processors currently available to the
     * virtual machine. This number may change over time; so a multi-processor
     * program want to poll this to determine maximal resource usage.
     *
     * @return the number of processors available, at least 1
     */
    static native int availableProcessors();

    /**
     * Find out how much memory is still free for allocating Objects on the heap.
     *
     * @return the number of bytes of free memory for more Objects
     */
    static native long freeMemory();

    /**
     * Find out how much memory total is available on the heap for allocating
     * Objects.
     *
     * @return the total number of bytes of memory for Objects
     */
    static native long totalMemory();

    /**
     * Returns the maximum amount of memory the virtual machine can attempt to
     * use. This may be <code>Long.MAX_VALUE</code> if there is no inherent
     * limit (or if you really do have a 8 exabyte memory!).
     *
     * @return the maximum number of bytes the virtual machine will attempt
     *         to allocate
     */
    static native long maxMemory();

    /**
     * Run the garbage collector. This method is more of a suggestion than
     * anything. All this method guarantees is that the garbage collector will
     * have "done its best" by the time it returns. Notice that garbage
     * collection takes place even without calling this method.
     */
    static native void gc();

    /**
     * Run finalization on all Objects that are waiting to be finalized. Again,
     * a suggestion, though a stronger one than {@link #gc()}. This calls the
     * <code>finalize</code> method of all objects waiting to be collected.
     *
     * @see #finalize()
     */
    static native void runFinalization();

    /**
     * Run finalization on all finalizable Objects (even live ones). This
     * should only be called immediately prior to VM termination.
     *
     * @see #finalize()
     */
    static native void runFinalizationForExit();

    /**
     * Tell the VM to trace every bytecode instruction that executes (print out
     * a trace of it).  No guarantees are made as to where it will be printed,
     * and the VM is allowed to ignore this request.
     *
     * @param on whether to turn instruction tracing on
     */
    static native void traceInstructions(boolean on);

    /**
     * Tell the VM to trace every method call that executes (print out a trace
     * of it).  No guarantees are made as to where it will be printed, and the
     * VM is allowed to ignore this request.
     *
     * @param on whether to turn method tracing on
     */
    static native void traceMethodCalls(boolean on);

    /**
     * Native method that actually sets the finalizer setting.
     *
     * @param value whether to run finalizers on exit
     */
    static native void runFinalizersOnExit(boolean value);

    /**
     * Native method that actually shuts down the virtual machine.
     *
     * @param status the status to end the process with
     */
    static native void exit(int status);

    /**
     * Load a file. If it has already been loaded, do nothing. The name has
     * already been mapped to a true filename.
     *
     * @param filename the file to load
     * @param loader class loader, or <code>null</code> for the boot loader
     * @return 0 on failure, nonzero on success
     */
    static native int nativeLoad(String filename, ClassLoader loader);

    /**
     * Map a system-independent "short name" to the full file name.
     *
     * @param libname the short version of the library name
     * @return the full filename
     */
    static native String mapLibraryName(String libname);

    /**
     * Execute a process. The command line has already been tokenized, and
     * the environment should contain name=value mappings. If directory is null,
     * use the current working directory; otherwise start the process in that
     * directory.  If env is null, then the new process should inherit
     * the environment of this process.
     *
     * @param cmd the non-null command tokens
     * @param env the environment setup
     * @param dir the directory to use, may be null
     * @return the newly created process
     * @throws NullPointerException if cmd or env have null elements
     */
    static Process exec(String[] cmd, String[] env, File dir)
	throws IOException {
      return VMProcess.exec(cmd, env, dir);
    }

    /**
     * This method is called by Runtime.addShutdownHook() when it is
     * called for the first time. It enables the VM to lazily setup
     * an exit handler, should it so desire.
     */
    static void enableShutdownHooks()
    {
    }
} // class VMRuntime
