/* insque(3C) routines
   This file is in the public domain.  */

/*
NAME
	insque, remque -- insert, remove an element from a queue

SYNOPSIS
	struct qelem {
	  struct qelem *q_forw;
	  struct qelem *q_back;
	  char q_data[];
	};

	void insque (struct qelem *elem, struct qelem *pred)

	void remque (struct qelem *elem)

DESCRIPTION
	Routines to manipulate queues built from doubly linked lists.
	The insque routine inserts ELEM in the queue immediately after
	PRED.  The remque routine removes ELEM from its containing queue.
*/


struct qelem {
  struct qelem *q_forw;
  struct qelem *q_back;
};


void
insque (elem, pred)
  struct qelem *elem;
  struct qelem *pred;
{
  elem -> q_forw = pred -> q_forw;
  pred -> q_forw -> q_back = elem;
  elem -> q_back = pred;
  pred -> q_forw = elem;
}


void
remque (elem)
  struct qelem *elem;
{
  elem -> q_forw -> q_back = elem -> q_back;
  elem -> q_back -> q_forw = elem -> q_forw;
}
