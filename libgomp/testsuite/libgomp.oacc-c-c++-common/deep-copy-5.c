#include <assert.h>
#include <stdlib.h>
#include <openacc.h>

struct node
{
  struct node *next;
  int val;
};

int
sum_nodes (struct node *head)
{
  int i = 0, sum = 0;

#pragma acc parallel reduction(+:sum) present(head[:1])
  {
    for (; head != NULL; head = head->next)
      sum += head->val;
  }

  return sum;
}

void
insert (struct node *head, int val)
{
  struct node *n = (struct node *) malloc (sizeof (struct node));

  if (head->next)
    acc_detach ((void **) &head->next);

  n->val = val;
  n->next = head->next;
  head->next = n;

  acc_copyin (n, sizeof (struct node));
  acc_attach((void **) &head->next);

  if (n->next)
    acc_attach ((void **) &n->next);
}

void
destroy (struct node *head)
{
  while (head->next != NULL)
    {
      acc_detach ((void **) &head->next);
      struct node * n = head->next;
      head->next = n->next;
      if (n->next)
	acc_detach ((void **) &n->next);

      acc_delete (n, sizeof (struct node));
      if (head->next)
	acc_attach((void **) &head->next);

      free (n);
    }
}

int
main ()
{
  struct node list = { .next = NULL, .val = 0 };
  int i;

  acc_copyin (&list, sizeof (struct node));

  for (i = 0; i < 10; i++)
    insert (&list, 2);

  assert (sum_nodes (&list) == 10 * 2);

  destroy (&list);

  acc_delete (&list, sizeof (struct node));

  return 0;
}
