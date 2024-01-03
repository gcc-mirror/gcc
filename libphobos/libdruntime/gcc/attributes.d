// GNU D Compiler attribute support declarations.
// Copyright (C) 2021-2024 Free Software Foundation, Inc.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

module gcc.attributes;

// Private helper templates.
private struct Attribute(A...)
{
    A arguments;
}

private enum bool isStringValue(alias T) = is(typeof(T) == string);

private enum bool isStringOrIntValue(alias T)
    = is(typeof(T) == string) || is(typeof(T) == int);

private template allSatisfy(alias F, T...)
{
    static if (T.length == 0)
        enum allSatisfy = true;
    else static if (T.length == 1)
        enum allSatisfy = F!(T[0]);
    else
    {
        enum allSatisfy = allSatisfy!(F, T[ 0  .. $/2])
            && allSatisfy!(F, T[$/2 ..  $ ]);
    }
}

/**
 * Generic entrypoint for applying GCC attributes to a function or type.
 * There is no type checking done, as well as no deprecation path for
 * attributes removed from the compiler.  So the recommendation is to use any
 , of the other UDAs available unless it is a target-specific attribute.
 *
 * Function attributes introduced by the @attribute UDA are used in the
 * declaration of a function, followed by an attribute name string and
 * any arguments separated by commas enclosed in parentheses.
 *
 * Example:
 * ---
 * import gcc.attributes;
 *
 * @attribute("regparm", 1) int func(int size);
 * ---
 */
@system
auto attribute(A...)(A arguments)
    if (A.length > 0 && is(A[0] == string))
{
    return Attribute!A(arguments);
}


///////////////////////////////////////////////////////////////////////////////
//
// Supported common attributes exposed by GDC.
//
///////////////////////////////////////////////////////////////////////////////

/**
 * The `@alloc_size` attribute may be applied to a function that returns a
 * pointer and takes at least one argument of an integer or enumerated type.
 * It indicates that the returned pointer points to memory whose size is given
 * by the function argument at `sizeArgIdx`, or by the product of the arguments
 * at `sizeArgIdx` and `numArgIdx`.  Meaningful sizes are positive values less
 * than `ptrdiff_t.max`.  Unless `zeroBasedNumbering` is true, argument
 * numbering starts at one for ordinary functions, and at two for non-static
 * member functions.
 *
 * If `numArgIdx` is less than `0`, it is taken to mean there is no argument
 * specifying the element count.
 *
 * Example:
 * ---
 * import gcc.attributes;
 *
 * @alloc_size(1) extern(C) void* malloc(size_t size);
 * @alloc_size(3,2) extern(C) void* reallocarray(void *ptr, size_t nmemb,
 *                                               size_t size);
 * @alloc_size(1,2) void* my_calloc(size_t element_size, size_t count,
 *                                  bool irrelevant);
 * ---
 */
auto alloc_size(int sizeArgIdx)
{
    return attribute("alloc_size", sizeArgIdx);
}

/// ditto
auto alloc_size(int sizeArgIdx, int numArgIdx)
{
    return attribute("alloc_size", sizeArgIdx, numArgIdx);
}

/// ditto
auto alloc_size(int sizeArgIdx, int numArgIdx, bool zeroBasedNumbering)
{
    return attribute("alloc_size", sizeArgIdx, numArgIdx, zeroBasedNumbering);
}

auto alloc_size(A...)(A arguments)
{
    assert(false, "alloc_size attribute argument value is not an integer constant");
}

/**
 * The `@always_inline` attribute inlines the function independent of any
 * restrictions that otherwise apply to inlining.  Failure to inline such a
 * function is diagnosed as an error.
 *
 * Example:
 * ---
 * import gcc.attributes;
 *
 * @always_inline int func();
 * ---
 */
enum always_inline = attribute("always_inline");

/**
 * The `@cold` attribute on functions is used to inform the compiler that the
 * function is unlikely to be executed.  The function is optimized for size
 * rather than speed and on many targets it is placed into a special subsection
 * of the text section so all cold functions appear close together, improving
 * code locality of non-cold parts of program.  The paths leading to calls of
 * cold functions within code are considered to be cold too.
 *
 * Example:
 * ---
 * import gcc.attributes;
 *
 * @cold int func();
 * ---
 */
enum cold = attribute("cold");

/**
 * The `@flatten` attribute is used to inform the compiler that every call
 * inside this function should be inlined, if possible.  Functions declared with
 * attribute `@noinline` and similar are not inlined.
 *
 * Example:
 * ---
 * import gcc.attributes;
 *
 * @flatten int func();
 * ---
 */
enum flatten = attribute("flatten");

/**
 * The `@no_icf` attribute prevents a functions from being merged with another
 * semantically equivalent function.
 *
 * Example:
 * ---
 * import gcc.attributes;
 *
 * @no_icf int func();
 * ---
 */
enum no_icf = attribute("no_icf");

/**
 * The `@no_sanitize` attribute on functions is used to inform the compiler
 * that it should not do sanitization of any option mentioned in
 * sanitize_option.  A list of values acceptable by the `-fsanitize` option
 * can be provided.
 *
 * Example:
 * ---
 * import gcc.attributes;
 *
 * @no_sanitize("alignment", "object-size") void func1() { }
 * @no_sanitize("alignment,object-size") void func2() { }
 * ---
 */

auto no_sanitize(A...)(A arguments)
    if (allSatisfy!(isStringValue, arguments))
{
    return attribute("no_sanitize", arguments);
}

auto no_sanitize(A...)(A arguments)
    if (!allSatisfy!(isStringValue, arguments))
{
    assert(false, "no_sanitize attribute argument not a string constant");
}

/**
 * The `@noclone` attribute prevents a function from being considered for
 * cloning - a mechanism that produces specialized copies of functions and
 * which is (currently) performed by interprocedural constant propagation.
 *
 * Example:
 * ---
 * import gcc.attributes;
 *
 * @noclone int func();
 * ---
 */
enum noclone = attribute("noclone");

/**
 * The `@noinline` attribute prevents a function from being considered for
 * inlining.  If the function does not have side effects, there are
 * optimizations other than inlining that cause function calls to be optimized
 * away, although the function call is live.  To keep such calls from being
 * optimized away, put `asm { ""; }` in the called function, to serve as a
 * special side effect.
 *
 * Example:
 * ---
 * import gcc.attributes;
 *
 * @noinline int func();
 * ---
 */
enum noinline = attribute("noinline");

/**
 * The `@noipa` attribute disables interprocedural optimizations between the
 * function with this attribute and its callers, as if the body of the function
 * is not available when optimizing callers and the callers are unavailable when
 * optimizing the body.  This attribute implies `@noinline`, `@noclone`, and
 * `@no_icf` attributes.  However, this attribute is not equivalent to a
 * combination of other attributes, because its purpose is to suppress existing
 * and future optimizations employing interprocedural analysis, including those
 * that do not have an attribute suitable for disabling them individually.
 *
 * This attribute is supported mainly for the purpose of testing the compiler.
 *
 * Example:
 * ---
 * import gcc.attributes;
 *
 * @noipa int func();
 * ---
 */
enum noipa = attribute("noipa");

/**
 * The `@optimize` attribute is used to specify that a function is to be
 * compiled with different optimization options than specified on the command
 * line.  Valid `arguments` are constant non-negative integers and strings.
 * Multiple arguments can be provided, separated by commas to specify multiple
 * options.  Each numeric argument specifies an optimization level.  Each string
 * argument that begins with the letter O refers to an optimization option such
 * as `-O0` or `-Os`.  Other options are taken as suffixes to the `-f` prefix
 * jointly forming the name of an optimization option.
 *
 * Not every optimization option that starts with the `-f` prefix specified by
 * the attribute necessarily has an effect on the function.  The `@optimize`
 * attribute should be used for debugging purposes only.  It is not suitable in
 * production code.
 *
 * Example:
 * ---
 * import gcc.attributes;
 *
 * @optimize(2) double fn0(double x);
 * @optimize("2") double fn1(double x);
 * @optimize("s") double fn2(double x);
 * @optimize("Ofast") double fn3(double x);
 * @optimize("-O2") double fn4(double x);
 * @optimize("tree-vectorize") double fn5(double x);
 * @optimize("-ftree-vectorize") double fn6(double x);
 * @optimize("no-finite-math-only", 3) double fn7(double x);
 * ---
 */
auto optimize(A...)(A arguments)
    if (allSatisfy!(isStringOrIntValue, arguments))
{
    return attribute("optimize", arguments);
}

auto optimize(A...)(A arguments)
    if (!allSatisfy!(isStringOrIntValue, arguments))
{
    assert(false, "optimize attribute argument not a string or integer constant");
}

/**
 * The `@register` attribute specifies that a local or `__gshared` variable
 * is to be given a register storage-class in the C99 sense of the term, and
 * will be placed into a register named `registerName`.
 *
 * The variable needs to boiled down to a data type that fits the target
 * register.  It also cannot have either thread-local or `extern` storage.
 * It is an error to take the address of a register variable.
 *
 * Example:
 * ---
 * import gcc.attributes;
 *
 * @register("ebx") __gshared int ebx = void;
 *
 * void func() { @register("r10") long r10 = 0x2a; }
 * ---
 */
auto register(string registerName)
{
    return attribute("register", registerName);
}

auto register(A...)(A arguments)
{
    assert(false, "register attribute argument not a string constant");
}

/**
 * The `@restrict` attribute specifies that a function parameter is to be
 * restrict-qualified in the C99 sense of the term.  The parameter needs to
 * boil down to either a pointer or reference type, such as a D pointer,
 * class reference, or a `ref` parameter.
 *
 * Example:
 * ---
 * import gcc.attributes;
 *
 * void func(@restrict ref const float[16] array);
 * ---
 */
enum restrict = attribute("restrict");

/**
 * The `@section` attribute specifies that a function lives in a particular
 * section.  For when you need certain particular functions to appear in
 * special sections.
 *
 * Some file formats do not support arbitrary sections so the section attribute
 * is not available on all platforms.  If you need to map the entire contents
 * of a module to a particular section, consider using the facilities of the
 * linker instead.
 *
 * Example:
 * ---
 * import gcc.attributes;
 *
 * @section("bar") extern void func();
 * ---
 */
auto section(string sectionName)
{
    return attribute("section", sectionName);
}

auto section(A...)(A arguments)
{
    assert(false, "section attribute argument not a string constant");
}

/**
 * The `@simd` attribute enables creation of one or more function versions that
 * can process multiple arguments using SIMD instructions from a single
 * invocation. Specifying this attribute allows compiler to assume that such
 * versions are available at link time (provided in the same or another module).
 * Generated versions are target-dependent and described in the corresponding
 * Vector ABI document. For x86_64 target this document can be found here.
 * https://sourceware.org/glibc/wiki/libmvec?action=AttachFile&do=view&target=VectorABI.txt
 * 
 * The `@simd_clones` attribute is the same as `@simd`, but also includes a
 * `mask` argument.  Valid masks values are `notinbranch` or `inbranch`, and
 * instructs the compiler to generate non-masked or masked clones
 * correspondingly.
 *
 * Example:
 * ---
 * import gcc.attributes;
 *
 * @simd double sqrt(double x);
 * @simd("notinbranch") double atan2(double y, double x);
 * ---
 */
enum simd = attribute("simd");

auto simd_clones(string mask)
{
    if (mask == "notinbranch" || mask == "inbranch")
        return attribute("simd", mask);
    else
    {
        assert(false, "unrecognized parameter `" ~ mask
               ~ "` for `gcc.attribute.simd_clones`");
    }
}

auto simd_clones(A...)(A arguments)
{
    assert(false, "simd_clones attribute argument not a string constant");
}

/**
 * The `@symver` attribute creates a symbol version on ELF targets.  The syntax
 * of the string parameter is `name@nodename`.  The `name` part of the parameter
 * is the actual name of the symbol by which it will be externally referenced.
 * The `nodename` portion should be the name of a node specified in the version
 * script supplied to the linker when building a shared library.  Versioned
 * symbol must be defined and must be exported with default visibility.
 *
 * Finally if the parameter is `name@@nodename` then in addition to creating a
 * symbol version (as if `name@nodename` was used) the version will be also used
 * to resolve `name` by the linker.
 *
 * Example:
 * ---
 * import gcc.attributes;
 *
 * @symver("foo@VERS_1") int foo_v1();
 * ---
 */
auto symver(A...)(A arguments)
    if (allSatisfy!(isStringValue, arguments))
{
    return attribute("symver", arguments);
}

auto symver(A...)(A arguments)
    if (!allSatisfy!(isStringValue, arguments))
{
    assert(false, "symver attribute argument not a string constant");
}

/**
 * The `@target` attribute is used to specify that a function is to be
 * compiled with different target options than specified on the command line.
 * One or more strings can be provided as arguments, separated by commas to
 * specify multiple options.  Each string consists of one or more
 * comma-separated suffixes to the `-m` prefix jointly forming the name of a
 * machine-dependent option.
 *
 * The target attribute can be used for instance to have a function compiled
 * with a different ISA (instruction set architecture) than the default.
 *
 * The options supported are specific to each target.
 *
 * Example:
 * ---
 * import gcc.attributes;
 *
 * @target("arch=core2") void core2_func();
 * @target("sse3") void sse3_func();
 * ---
 */
auto target(A...)(A arguments)
    if (allSatisfy!(isStringValue, arguments))
{
    return attribute("target", arguments);
}

auto target(A...)(A arguments)
    if (!allSatisfy!(isStringValue, arguments))
{
    assert(false, "target attribute argument not a string constant");
}

/**
 * The `@target_clones` attribute is used to specify that a function be cloned
 * into multiple versions compiled with different target `options` than
 * specified on the command line.  The supported options and restrictions are
 * the same as for `@target` attribute.
 *
 * It also creates a resolver function that dynamically selects a clone suitable
 * for current architecture.  The resolver is created only if there is a usage
 * of a function with `@target_clones` attribute.
 *
 * Example:
 * ---
 * import gcc.attributes;
 *
 * @target_clones("sse4.1,avx,default") double func(double x);
 * ---
 */
auto target_clones(A...)(A arguments)
    if (allSatisfy!(isStringValue, arguments))
{
    return attribute("target_clones", arguments);
}

auto target_clones(A...)(A arguments)
    if (!allSatisfy!(isStringValue, arguments))
{
    assert(false, "target attribute argument not a string constant");
}

/**
 * The `@used` attribute, annotated to a function, means that code must be
 * emitted for the function even if it appears that the function is not
 * referenced.  This is useful, for example, when the function is referenced
 * only in inline assembly.
 *
 * Example:
 * ---
 * import gcc.attributes;
 *
 * @used __gshared int var = 0x1000;
 * ---
 */
enum used = attribute("used");

/**
 * The `@visibility` attribute affects the linkage of the declaration to which
 * it is attached. It can be applied to variables, types, and functions.
 *
 * There are four supported visibility_type values: `default`, `hidden`,
 * `protected`, or `internal` visibility.
 *
 * Example:
 * ---
 * import gcc.attributes;
 *
 * @visibility("protected") void func() {  }
 * ---
 */
auto visibility(string visibilityName)
{
    return attribute("visibility", visibilityName);
}

auto visibility(A...)(A arguments)
{
    assert(false, "visibility attribute argument not a string constant");
}

/**
 * The `@weak` attribute causes a declaration of an external symbol to be
 * emitted as a weak symbol rather than a global.  This is primarily useful in
 * defining library functions that can be overridden in user code, though it can
 * also be used with non-function declarations.  The overriding symbol must have
 * the same type as the weak symbol.  In addition, if it designates a variable
 * it must also have the same size and alignment as the weak symbol.
 *
 * Weak symbols are supported for ELF targets, and also for a.out targets when
 * using the GNU assembler and linker.
 *
 * Example:
 * ---
 * import gcc.attributes;
 *
 * @weak int func() { return 1; }
 * ---
 */
enum weak = attribute("weak");

/**
 * The `@noplt` attribute is the counterpart to option `-fno-plt`. Calls to
 * functions marked with this attribute in position-independent code do not use
 * the PLT in position-independent code.
 *
 * In position-dependant code, a few targets also convert call to functions
 * that are marked to not use the PLT to use the GOT instead.
 *
 * Example:
 * ---
 * import gcc.attributes;
 *
 * @noplt int func();
 *
 * ---
 */
enum noplt = attribute("noplt");

///////////////////////////////////////////////////////////////////////////////
//
// Attributes defined for compatibility with LDC.
//
///////////////////////////////////////////////////////////////////////////////

/**
 * Specifies that the function returns `null` or a pointer to at least a
 * certain number of allocated bytes. `sizeArgIdx` and `numArgIdx` specify
 * the 0-based index of the function arguments that should be used to calculate
 * the number of bytes returned.
 *
 * Example:
 * ---
 * import gcc.attributes;
 *
 * @allocSize(0) extern(C) void* malloc(size_t size);
 * @allocSize(2,1) extern(C) void* reallocarray(void *ptr, size_t nmemb,
 *                                              size_t size);
 * @allocSize(0,1) void* my_calloc(size_t element_size, size_t count,
 *                                 bool irrelevant);
 * ---
 */
auto allocSize(int sizeArgIdx, int numArgIdx = int.min)
{
    return alloc_size(sizeArgIdx, numArgIdx, true);
}

auto allocSize(A...)(A arguments)
{
    assert(false, "allocSize attribute argument value is not an integer constant");
}

/**
 * When applied to a global symbol, the compiler, assembler, and linker are
 * required to treat the symbol as if there is a reference to the symbol that
 * it cannot see (which is why they have to be named).  For example, it
 * prevents the deletion by the linker of an unreferenced symbol.
 *
 * Example:
 * ---
 * import gcc.attributes;
 *
 * @assumeUsed __gshared int var = 0x1000;
 * ---
 */
alias assumeUsed = used;

/// This attribute has no effect.
enum dynamicCompile = false;

/// ditto
enum dynamicCompileConst = false;

/// ditto
enum dynamicCompileEmit = false;

/**
 * Explicitly sets "fast-math" for a function, enabling aggressive math
 * optimizations.  These optimizations may dramatically change the outcome of
 * floating point calculations (e.g. because of reassociation).
 *
 * Example:
 * ---
 * import gcc.attributes;
 *
 * @fastmath
 * double dot(double[] a, double[] b) {
 *     double s = 0;
 *     foreach(size_t i; 0..a.length)
 *     {
 *         // will result in vectorized fused-multiply-add instructions
 *         s += a * b;
 *     }
 *     return s;
 * }
 * ---
 */
enum fastmath = optimize("Ofast");

/**
 * Sets the visibility of a function or global variable to "hidden".
 * Such symbols aren't directly accessible from outside the DSO
 * (executable or DLL/.so/.dylib) and are resolved inside the DSO
 * during linking. If unreferenced within the DSO, the linker can
 * strip a hidden symbol.
 * An `export` visibility overrides this attribute.
 */
enum hidden = visibility("hidden");

/**
 * Adds GCC's "naked" attribute to a function, disabling function prologue /
 * epilogue emission.
 * Intended to be used in combination with basic `asm` statement.  While using
 * extended `asm` or a mixture of basic `asm` and D code may appear to work,
 * they cannot be depended upon to work reliably and are not supported.
 *
 * Example:
 * ---
 * import gcc.attributes;
 *
 * @naked void abort() {
 *     asm { "ud2"; }
 * }
 * ---
 */
enum naked = attribute("naked");

/**
 * Disables a particular sanitizer for this function.
 * Valid sanitizer names are all names accepted by `-fsanitize=` commandline option.
 * Multiple sanitizers can be disabled by applying this UDA multiple times, e.g.
 * `@noSanitize("address") `@noSanitize("thread")` to disable both ASan and TSan.
 */
alias noSanitize = no_sanitize;

/**
 * Sets the optimization strategy for a function.
 * Valid strategies are "none", "optsize", "minsize". The strategies are
 * mutually exclusive.
 *
 * Example:
 * ---
 * import gcc.attributes;
 *
 * @optStrategy("none")
 * int func() {
 *     return call();
 * }
 * ---
 */
auto optStrategy(string strategy)
{
    if (strategy == "none")
        return optimize("O0");
    else if (strategy == "optsize" || strategy == "minsize")
        return optimize("Os");
    else
    {
        assert(false, "unrecognized parameter `" ~ strategy
               ~ "` for `gcc.attribute.optStrategy`");
    }
}

auto optStrategy(A...)(A arguments)
{
    assert(false, "optStrategy attribute argument value is not a string constant");
}

/**
 * When applied to a function, specifies that the function should be optimzed
 * by Graphite, GCC's polyhedral optimizer. Useful for optimizing loops for
 * data locality, vectorization and parallelism.
 *
 * Experimental!
 *
 * Only effective when GDC was built with ISL included.
 */
enum polly = optimize("loop-parallelize-all", "loop-nest-optimize");
