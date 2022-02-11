/*******************************************/
// https://issues.dlang.org/show_bug.cgi?id=20613

extern(C) int main() @nogc nothrow pure
{
    auto s = "F";
    final switch(s)
    {
    case "A": break;
    case "B": break;
    case "C": break;
    case "D": break;
    case "E": break;
    case "F": break;
    case "G": break;
    }
    return 0;
}
