! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

use openacc
implicit none (type, external)
integer,pointer :: a, b(:)
integer,allocatable :: c, d(:)

call acc_attach(a)  ! ICE
call acc_attach_async(b, 4)
call acc_attach(c)

call acc_detach(a)
call acc_detach_async(b, 4)
call acc_detach_finalize(c)
call acc_detach_finalize_async(d,7)
end

! { dg-final { scan-tree-dump-times "acc_attach \\(&a\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "acc_attach_async \\(&\\(integer\\(kind=4\\)\\\[0:\\\] \\*\\) b.data, 4\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "acc_attach \\(&c\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "acc_detach \\(&a\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "acc_detach_async \\(&\\(integer\\(kind=4\\)\\\[0:\\\] \\*\\) b.data, 4\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "acc_detach_finalize \\(&c\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "acc_detach_finalize_async \\(&\\(integer\\(kind=4\\)\\\[0:\\\] \\* restrict\\) d.data, 7\\);" 1 "original" } }
