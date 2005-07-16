The GNU classpath native layer
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To enable GNU classpath to run on a diverse array of different hardware
platforms, a new native software layer has been added.  This layer hide all
machine and hardware dependent issues except common available definitions,
which are ANSI C functions.  For each targets system where the GNU classpath
library is to be used, a specific set of native layer functions have to be
provided.  A generic set of functions is provided for Unix like systems
(currently tested only with Linux).  For a new target system, some or
all native layer functions have to be rewritten.  The following scheme
illustrate the native layer.

            Java API
      ----------------------
     |    Java classes      |
     | -------------------- |
     |  C native functions  |
     | -------------------- |
  >> |    C native layer    |  <<
     | -------------------- |
     |   operating system   |
     | -------------------- |
     |       hardware       |
      ----------------------

The C native layer is implemented as a set of C pre-processor native macros.
These macros expand to the appropriated native code.  Macros are used
instead function calls to give optimal performance and small code size.
Of course in special cases, a macro can also expand to a function call
if this is needed.  This approach provide a flexible and efficient
implementation of the native layer.

The naming pattern for native macros is like follows:

  TARGET_NATIVE_<module name>_<macro name>

where <module name> is a name of a module, e. g. FILE; <macro name> is
the name of the specific native macro, e. g. OPEN_READ.

The parameters for the macro use in general (with a few exceptions) the
scheme

  <parameter>,<parameter>,...,<result>

where <parameter> is input/output parameter and <result> is the result
code TARGET_NATIVE_OK or TARGET_NATIVE_ERROR.  Specific error codes
and error strings can be gotten with

  TARGET_NATIVE_LAST_ERROR and
  TARGET_NATIVE_LAST_ERROR_STRING

(see also file target_generic.h).

For a single target system there exists two sets of native macros in
the files

  a) <target name>/target_native_<module name>.h
  b) generic/target_generic_<module name>.h

The macros in "a" are target specific implementations of native
functions, the macros in "b" are generic implementations (for Unix) of
the same native functions.  If a native macro is not defined in the file
"a", then the definition in file "b" is used (there is a check to see if
a native macros is already defined elsewhere).  This technique enables
"a" to 'overwrite' single generic native macros with specific native
macros for a specific target.  In the default case, where only the
generic implementation of the native macros is used, the files in the
directory '<target name>' are empty except for the mandatory include of the
generic header file in the directory 'generic' at the end.  Please
look at the existing Linux target specific files.

The directory and file structure is as follows.

  native
   ...
    | 
    |--- target
    |      |
    |      |--- Linux
    |      |      |--- target_native_<module name>.h
    |      |      |--- ...
    |      |     ...
    |      |--- ...
    |      |--- generic
    |      |      |--- target_generic_<module name>.h
    |      |      |--- ...
   ...    ...    ...
   

Include hierarchy is as follows.

  native file <name>
    --> include 'target_native_<module name>.h'
                  ...
                  <target specific definitions>
                  ...
                  --> include 'target_generic_<module name>.h'
                                ...
                                <generic definitions>
                                ...

When writing native code, please take care with the following.

 - Use _only_ ANSI C specific functions directly which are available
   on all target systems with the same parameters, e. g. strdup() is
   not an ANSI C function, thus is is not available on all systems; mkdir()
   expect on some systems different parameters.

   !!!Do NOT use this functions in your native code!!!

   Instead

   * if a function is not available, create a native macro in the file

     <target name>/target_native_<module name>.h

   * if it is a generic function, include a generic implementation in

     generic/target_generic_<module name>.h

   * Then use this macro in your native code.

 - Avoid _all_ OS specific data types and constants, e. g. structures or error
   numbers.  Instead, wrap them in a native macro and convert the values to
   basic scalar types like char, int, double or long.

 - Take care with 64 bit values; the are machine dependent.  Not all
   target system support 64 bit operations.  The macros in
   target_generic_math_int.h give a set of macros implementing 64 bit
   operations and constants.

 - Avoid - if possible - non-reentrant functions.  Non-reentrant functions
   cause strange problems on some multitasking systems.

 - Avoid - if possible - dynamic data types created by malloc() and similar
   functions.  Instead use (local) static variables to avoid stack usage.
   On some target systems, dynamic memory management is either slow or even
   dangerous.  Moreover malloc()-calls can cause fragmentation of the system
   memory, which could result in a system crash or an application failure.

For some examples, please look in the current implementation for
Linux in the directory 'target/Linux' and the generic implementation in
the directory 'target/generic'.


                                        aicas GmbH, February 2003


