/**
 * This module contains UDA's (User Defined Attributes) either used in
 * the runtime or special UDA's recognized by compiler.
 *
 * $(SCRIPT inhibitQuickIndex = 1;)
 * $(BOOKTABLE Cheat Sheet,
 * $(THEAD Attribute Name, Linkage, Description)
 * $(TROW $(LREF gnuAbiTag), C++,
 *         Declares an ABI tag on a C++ symbol.)
 * $(TROW $(LREF mustuse),,
 *          Ensures that values of a struct or union type are not discarded.)
 * $(TROW $(LREF optional), Objective-C,
 *         Makes an Objective-C interface method optional.)
 * $(TROW $(LREF selector), Objective-C,
 *          Attaches an Objective-C selector to a method.)
 * $(TROW $(LREF standalone),,
 *          Marks a shared module constructor as not depending on any
 *          other module constructor being run first.)
 * $(TROW $(LREF weak),,
 *         Specifies that a global symbol should be emitted with weak linkage.)
 * )
 *
 * Copyright: Copyright Jacob Carlborg 2015.
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Jacob Carlborg
 * Source:    $(DRUNTIMESRC core/_attribute.d)
 */

/*          Copyright Jacob Carlborg 2015.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module core.attribute;

version (GNU)
    public import gcc.attributes;

version (LDC)
    public import ldc.attributes;

version (D_ObjectiveC)
{
    version = UdaOptional;
    version = UdaSelector;
}

version (Posix)
    version = UdaGNUAbiTag;

version (CoreDdoc)
{
    version = UdaGNUAbiTag;
    version = UdaOptional;
    version = UdaSelector;
}

/**
 * Use this attribute to specify that a global symbol should be emitted with
 * weak linkage. This is primarily useful in defining library functions that
 * can be overridden by user code, though it can also be used with shared and
 * static variables too.
 *
 * The overriding symbol must have the same type as the weak symbol. In
 * addition, if it designates a variable it must also have the same size and
 * alignment as the weak symbol.
 *
 * Quote from the LLVM manual: "Note that weak linkage does not actually allow
 * the optimizer to inline the body of this function into callers because it
 * doesn’t know if this definition of the function is the definitive definition
 * within the program or whether it will be overridden by a stronger
 * definition."
 *
 * This attribute is only meaningful to the GNU and LLVM D compilers. The
 * Digital Mars D compiler emits all symbols with weak linkage by default.
 */
version (DigitalMars)
{
    enum weak;
}
else
{
    // GDC and LDC declare this attribute in their own modules.
}

/**
 * Use this attribute to attach an Objective-C selector to a method.
 *
 * This is a special compiler recognized attribute, it has several
 * requirements, which all will be enforced by the compiler:
 *
 * $(UL
 *  $(LI
 *      The attribute can only be attached to methods or constructors which
 *      have Objective-C linkage. That is, a method or a constructor in a
 *      class or interface declared as $(D_CODE extern(Objective-C)).
 *  )
 *
 *  $(LI It cannot be attached to a method or constructor that is a template)
 *
 *  $(LI
 *      The number of colons in the string need to match the number of
 *      arguments the method accept.
 *  )
 *
 *  $(LI It can only be used once in a method declaration)
 * )
 *
 * Examples:
 * ---
 * extern (Objective-C)
 * class NSObject
 * {
 *  this() @selector("init");
 *  static NSObject alloc() @selector("alloc");
 *  NSObject initWithUTF8String(in char* str) @selector("initWithUTF8String:");
 *  ObjcObject copyScriptingValue(ObjcObject value, NSString key, NSDictionary properties)
 *      @selector("copyScriptingValue:forKey:withProperties:");
 * }
 * ---
 */
version (UdaSelector) struct selector
{
    string selector;
}

/**
 * Use this attribute to make an Objective-C interface method optional.
 *
 * An optional method is a method that does **not** have to be implemented in
 * the class that implements the interface. To safely call an optional method,
 * a runtime check should be performed to make sure the receiver implements the
 * method.
 *
 * This is a special compiler recognized attribute, it has several
 * requirements, which all will be enforced by the compiler:
 *
 * * The attribute can only be attached to methods which have Objective-C
 *   linkage. That is, a method inside an interface declared as `extern (Objective-C)`
 *
 * * It can only be used for methods that are declared inside an interface
 * * It can only be used once in a method declaration
 * * It cannot be attached to a method that is a template
 *
 * Examples:
 * ---
 * import core.attribute : optional, selector;
 *
 * extern (Objective-C):
 *
 * struct objc_selector;
 * alias SEL = objc_selector*;
 *
 * SEL sel_registerName(in char* str);
 *
 * extern class NSObject
 * {
 *     bool respondsToSelector(SEL sel) @selector("respondsToSelector:");
 * }
 *
 * interface Foo
 * {
 *     @optional void foo() @selector("foo");
 *     @optional void bar() @selector("bar");
 * }
 *
 * class Bar : NSObject
 * {
 *     static Bar alloc() @selector("alloc");
 *     Bar init() @selector("init");
 *
 *     void bar() @selector("bar")
 *     {
 *     }
 * }
 *
 * extern (D) void main()
 * {
 *     auto bar = Bar.alloc.init;
 *
 *     if (bar.respondsToSelector(sel_registerName("bar")))
 *         bar.bar();
 * }
 * ---
 */
version (UdaOptional)
    enum optional;

/**
 * Use this attribute to declare an ABI tag on a C++ symbol.
 *
 * ABI tag is an attribute introduced by the GNU C++ compiler.
 * It modifies the mangled name of the symbol to incorporate the tag name,
 * in order to distinguish from an earlier version with a different ABI.
 *
 * This is a special compiler recognized attribute, it has a few
 * requirements, which all will be enforced by the compiler:
 *
 * $(UL
 *  $(LI
 *      There can only be one such attribute per symbol.
 *  )
 *  $(LI
 *      The attribute can only be attached to an `extern(C++)` symbol
 *      (`struct`, `class`, `enum`, function, and their templated counterparts).
 *  )
 *  $(LI
 *      The attribute cannot be applied to C++ namespaces.
 *      This is to prevent confusion with the C++ semantic, which allows it to
 *      be applied to namespaces.
 *  )
 *  $(LI
 *      The string arguments must only contain valid characters
 *      for C++ name mangling which currently include alphanumerics
 *      and the underscore character.
 *  )
 * )
 *
 * This UDA is not transitive, and inner scope do not inherit outer scopes'
 * ABI tag. See examples below for how to translate a C++ declaration to D.
 * Also note that entries in this UDA will be automatically sorted
 * alphabetically, hence `gnuAbiTag("c", "b", "a")` will appear as
 * `@gnuAbiTag("a", "b", "c")`.
 *
 * See_Also:
 * $(LINK2 https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangle.abi-tag, Itanium ABI spec)
 * $(LINK2 https://gcc.gnu.org/onlinedocs/gcc/C_002b_002b-Attributes.html, GCC attributes documentation).
 *
 * Examples:
 * ---
 * // ---- foo.cpp
 * struct [[gnu::abi_tag ("tag1", "tag2")]] Tagged1_2
 * {
 *     struct [[gnu::abi_tag ("tag3")]] Tagged3
 *     {
 *         [[gnu::abi_tag ("tag4")]]
 *         int Tagged4 () { return 42; }
 *     }
 * }
 * Tagged1_2 inst1;
 * // ---- foo.d
 * @gnuAbiTag("tag1", "tag2") struct Tagged1_2
 * {
 *     // Notice the repetition
 *     @gnuAbiTag("tag1", "tag2", "tag3") struct Tagged3
 *     {
 *         @gnuAbiTag("tag1", "tag2", "tag3", "tag4") int Tagged4 ();
 *     }
 * }
 * extern __gshared Tagged1_2 inst1;
 * ---
 */
version (UdaGNUAbiTag) struct gnuAbiTag
{
    string[] tags;

    this(string[] tags...) @safe pure nothrow
    {
        this.tags = tags.dup;
    }
}

/**
 * Use this attribute to ensure that values of a `struct` or `union` type are
 * not discarded.
 *
 * The value of an expression is considered to be discarded if
 *
 * $(UL
 *  $(LI
 *      the expression is the top-level expression in a statement or the
 *      left-hand expression in a comma expression, and
 *  )
 *  $(LI
 *      the expression is not an assignment (`=`, `+=`, etc.), increment
 *      (`++`), or decrement (`--`) expression.
 *  )
 * )
 *
 * If the declaration of a `struct` or `union` type has the `@mustuse`
 * attribute, the compiler will emit an error any time a value of that type
 * would be discarded.
 *
 * Currently, `@mustuse` is only recognized by the compiler when attached to
 * `struct` and `union` declarations. To allow for future expansion, attaching
 * `@mustuse` to a `class`, `interface`, `enum`, or function declaration is
 * currently forbidden, and will result in a compile-time error. All other uses
 * of `@mustuse` are ignored.
 *
 * Examples:
 * ---
 * @mustuse struct ErrorCode { int value; }
 *
 * extern(C) ErrorCode doSomething();
 *
 * void main()
 * {
 *     // error: would discard a value of type ErrorCode
 *     //doSomething();
 *
 *     ErrorCode result;
 *     // ok: value is assigned to a variable
 *     result = doSomething();
 *
 *     // ok: can ignore the value explicitly with a cast
 *     cast(void) doSomething();
 * }
 * ---
 */
enum mustuse;

/**
 * Use this attribute to indicate that a shared module constructor does not depend on any
 * other module constructor being run first. This avoids errors on cyclic module constructors.
 *
 * However, it is now up to the user to enforce safety.
 * The module constructor must be marked `@system` as a result.
 * Prefer to refactor the module constructor causing the cycle so it's in its own module if possible.
 *
 * This is only allowed on `shared` static constructors, not thread-local module constructors.
 */
enum standalone;
