let rec stevilo_nzz n l k = match n, l ,k with
| 0, _, 0 -> 1 
| 0, 0, _ -> 1 
| 0, _, _ -> 1
| a, 1, c -> stevilo_nzz (a - 1) l c
| a, b, c -> stevilo_nzz (a - 1) (b - 1) (c -1) + stevilo_nzz (a - 1) l c