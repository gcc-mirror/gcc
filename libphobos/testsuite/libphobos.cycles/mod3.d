// { dg-additional-sources "mod1.d mod2.d" }
module mod3;
import mod2;

shared int x;
shared static this()
{
    x = 3;
}
