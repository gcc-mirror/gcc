// { dg-options "-funittest" }
// { dg-shouldfail "unittest_assert msg" }
// { dg-output "core.exception.AssertError@.*: unittest_assert msg" }
unittest
{
    assert(0, "unittest_assert msg");
}

void main()
{
}
