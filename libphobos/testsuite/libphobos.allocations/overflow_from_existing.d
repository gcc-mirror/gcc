// { dg-shouldfail "Memory allocation failed" }
// { dg-output "core.exception.OutOfMemoryError@.*: Memory allocation failed" }
void main()
{
    void[] buffer;
    buffer.length = 1;
    buffer.length = size_t.max;
}
