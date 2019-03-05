/**
For testing only.
Provides a struct with UDA's defined in an external module.
Useful for validating behavior with member privacy.
*/
module std.internal.test.uda;

enum Attr;

struct HasPrivateMembers
{
  @Attr int a;
  int b;
  @Attr private int c;
  private int d;
}
