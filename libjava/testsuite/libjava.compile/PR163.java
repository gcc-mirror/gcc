// Test case for the PR gcj/163

// Uninitalized final variables should not be permitted.

class PR163
{
  final int foo;
}
