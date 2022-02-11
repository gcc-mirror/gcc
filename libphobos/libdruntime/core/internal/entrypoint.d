/**
 This module contains the code for C main and any call(s) to initialize the
 D runtime and call D main.

  Copyright: Copyright Digital Mars 2000 - 2019.
  License: Distributed under the
       $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
     (See accompanying file LICENSE)
  Source: $(DRUNTIMESRC core/_internal/_entrypoint.d)
*/
module core.internal.entrypoint;

/**
A template containing C main and any call(s) to initialize druntime and
call D main.  Any module containing a D main function declaration will
cause the compiler to generate a `mixin _d_cmain();` statement to inject
this code into the module.
*/
template _d_cmain()
{
    extern(C)
    {
        int _d_run_main(int argc, char **argv, void* mainFunc);

        int _Dmain(char[][] args);

        int main(int argc, char **argv)
        {
            return _d_run_main(argc, argv, &_Dmain);
        }

        // Solaris, for unknown reasons, requires both a main() and an _main()
        version (Solaris)
        {
            int _main(int argc, char** argv)
            {
                return main(argc, argv);
            }
        }
    }
}
