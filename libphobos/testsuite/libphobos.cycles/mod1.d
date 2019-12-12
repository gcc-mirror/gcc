// { dg-additional-sources "mod2.d mod3.d" }
module mod1;
import mod2;

shared int x;
shared static this()
{
    x = 1;
}
