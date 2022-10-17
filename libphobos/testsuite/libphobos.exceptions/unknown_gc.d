// { dg-require-effective-target shared }
// { dg-options "-shared-libphobos" }
// { dg-shouldfail "unknowngc" }
// { dg-output "No GC was initialized, please recheck the name of the selected GC \\('unknowngc'\\)." }
import core.memory;

extern(C) __gshared string[] rt_options = [ "gcopt=gc:unknowngc" ];

void main()
{
    // GC initialized upon first call -> Unknown GC error is thrown
    GC.enable();
}
