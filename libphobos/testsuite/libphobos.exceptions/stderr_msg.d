// { dg-shouldfail "stderr_msg msg" }
// { dg-output "object.Exception@.*: stderr_msg msg" }
void main()
{
    throw new Exception("stderr_msg msg");
}
