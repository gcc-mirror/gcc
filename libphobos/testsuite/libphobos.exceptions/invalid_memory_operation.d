// { dg-shouldfail "Invalid memory operation" }
// { dg-output "core.exception.InvalidMemoryOperationError@.*: Invalid memory operation" }
struct S
{
    ~this()
    {
        new int;
    }
}

void main()
{
    new S;
}
