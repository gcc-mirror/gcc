/* Create classes from their modules and names.
 *
 * Copyright: Copyright (C) D Language Foundation 2023
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Walter Bright, Steven Schveighoffer
 * Source:    $(DRUNTIMESRC core/_factory.d)
 */

module core.factory;

/**
 * Create instance of class specified by the module symbol and a string
 * representing the name of the class.
 * The class must either have no constructors or have
 * a default constructor.
 * Params:
 *   mod = symbol representing the module that the class is in
 *   classname = string representing the name of the class
 * Returns:
 *   null if failed
 * Example:
 * ---
 * module foo.bar;
 *
 * class C
 * {
 *     this() { x = 10; }
 *     int x;
 * }
 *
 * void main()
 * {
 *     auto c = cast(C)factory!(foo.bar)("C");
 *     assert(c !is null && c.x == 10);
 * }
 * ---
 */
Object factory(alias mod)(string classname)
{
    foreach(cl; _getModuleClasses!mod)
    {
        if (cl.stringof == classname)
            return cl.classinfo.create();
    }
    return null;
}

@system unittest
{
    Object valid_obj = factory!object("Object");
    Object invalid_obj = factory!object("__this_class_doesnt_exist__");

    assert(valid_obj !is null);
    assert(invalid_obj is null);
}

/**************************************
 * Retrieve as a tuple all the types of the top level classes in the module mod.
 */
private template _getModuleClasses(alias mod) {
   alias result = _AliasSeq!();
   static foreach(m; __traits(allMembers, mod))
      static if(is(__traits(getMember, mod, m) == class))
         result = _AliasSeq!(result, __traits(getMember, mod, m));
   alias _getModuleClasses = result;
}

private template _AliasSeq(TList...) { alias _AliasSeq = TList; }
