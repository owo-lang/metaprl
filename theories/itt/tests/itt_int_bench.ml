extends Itt_int_test


interactive test0 :
sequent { v1 : int; v2 : int; v3 : int; v4 : int; v5 : int; 
(((7 *@ 'v4) +@ (6 *@ 'v1)) +@ (8 *@ 'v5)) +@ (((3 *@ 'v4) +@ (7 *@ 'v2)) +@ ((8 *@ 'v1) +@ (2 *@ 'v4))) = (8 *@ 'v3) +@ (((0 *@ 'v2) +@ (1 *@ 'v1)) +@ (8 *@ 'v4)) in int;
(0 *@ 'v3) +@ (2 *@ 'v1) <= 4 *@ 'v4;
((8 *@ 'v1) +@ ((2 *@ 'v4) +@ (1 *@ 'v2))) +@ (1 *@ 'v1) = (((7 *@ 'v3) +@ (2)) +@ ((6 *@ 'v2) +@ (2))) +@ (5) in int;
4 *@ 'v4 = (((4 *@ 'v1) +@ (7 *@ 'v3)) +@ (3 *@ 'v5)) +@ (9) in int;
1 *@ 'v1 < 1 *@ 'v4;
2 *@ 'v5 = 7 *@ 'v4 in int;
(((9 *@ 'v4) +@ (4)) +@ ((0 *@ 'v3) +@ (0 *@ 'v3))) +@ (((5 *@ 'v3) +@ (8 *@ 'v4)) +@ ((6 *@ 'v1) +@ (3 *@ 'v1))) <> (((1 *@ 'v1) +@ (7)) +@ ((7 *@ 'v4) +@ (2 *@ 'v3))) +@ (1);
2 *@ 'v2 > ((4 *@ 'v4) +@ (0 *@ 'v2)) +@ ((8 *@ 'v4) +@ (0 *@ 'v5));
7 *@ 'v4 <> (4 *@ 'v4) +@ (1 *@ 'v2);
((5 *@ 'v5) +@ (1 *@ 'v2)) +@ (2 *@ 'v2) <> 2 *@ 'v4 >- "false" }

interactive test1 :
sequent { v1 : int; v2 : int; v3 : int; v4 : int; v5 : int; 
(((3 *@ 'v4) +@ (7 *@ 'v1)) +@ ((8) +@ (4 *@ 'v2))) +@ (((3) +@ (5 *@ 'v4)) +@ ((1 *@ 'v3) +@ (9 *@ 'v3))) > 0 *@ 'v2;
(2 *@ 'v4) +@ (9) < 3 *@ 'v5;
(8 *@ 'v4) +@ (4 *@ 'v2) = (5 *@ 'v4) +@ ((7 *@ 'v2) +@ ((9 *@ 'v2) +@ (6 *@ 'v1))) in int;
(4 *@ 'v4) +@ (6 *@ 'v5) < (2 *@ 'v4) +@ (((8) +@ (1 *@ 'v4)) +@ ((0 *@ 'v3) +@ (8 *@ 'v4)));
7 *@ 'v3 <> (3 *@ 'v3) +@ (8);
3 = 3 *@ 'v5 in int;
8 *@ 'v4 < 2 *@ 'v2;
((0) +@ (8 *@ 'v1)) +@ (((4 *@ 'v1) +@ (1 *@ 'v1)) +@ (3 *@ 'v4)) = ((5 *@ 'v3) +@ ((0 *@ 'v3) +@ (5 *@ 'v3))) +@ (7) in int;
((8 *@ 'v5) +@ ((4 *@ 'v3) +@ (0 *@ 'v4))) +@ (4 *@ 'v5) < ((2 *@ 'v4) +@ (7 *@ 'v1)) +@ (((5 *@ 'v4) +@ (1 *@ 'v5)) +@ (4 *@ 'v5));
((2 *@ 'v3) +@ ((4 *@ 'v3) +@ (2 *@ 'v4))) +@ (0 *@ 'v4) = 4 *@ 'v1 in int >- "false" }

interactive test2 :
sequent { v1 : int; v2 : int; v3 : int; v4 : int; v5 : int; 
5 *@ 'v2 < ((9) +@ ((0 *@ 'v4) +@ (9 *@ 'v1))) +@ (((7) +@ (0 *@ 'v2)) +@ (6 *@ 'v2));
(9) +@ (7 *@ 'v4) < (3) +@ (((3 *@ 'v4) +@ (1 *@ 'v3)) +@ ((2) +@ (7 *@ 'v5)));
(((7 *@ 'v5) +@ (2 *@ 'v3)) +@ ((3 *@ 'v2) +@ (6 *@ 'v2))) +@ ((4 *@ 'v5) +@ (3 *@ 'v2)) > (3 *@ 'v2) +@ ((0 *@ 'v4) +@ ((4 *@ 'v3) +@ (0 *@ 'v5)));
2 *@ 'v2 <= 9 *@ 'v3;
3 *@ 'v1 > (6 *@ 'v2) +@ ((7 *@ 'v5) +@ (4 *@ 'v4));
7 *@ 'v1 > 4;
(6) +@ (((4 *@ 'v1) +@ (0 *@ 'v5)) +@ ((4) +@ (3))) > 8 *@ 'v4;
8 *@ 'v5 = 4 in int;
(((7 *@ 'v2) +@ (0 *@ 'v3)) +@ ((8) +@ (4 *@ 'v1))) +@ (((4 *@ 'v3) +@ (9 *@ 'v1)) +@ ((0 *@ 'v3) +@ (8))) > 5 *@ 'v5;
(((0) +@ (2)) +@ (5 *@ 'v3)) +@ (((0 *@ 'v5) +@ (0 *@ 'v5)) +@ (6)) >= 8 *@ 'v2 >- "false" }

interactive test3 :
sequent { v1 : int; v2 : int; v3 : int; v4 : int; v5 : int; 
(1) +@ (0 *@ 'v4) <= 0 *@ 'v2;
7 *@ 'v3 = 2 *@ 'v2 in int;
(((7 *@ 'v1) +@ (2 *@ 'v5)) +@ (0 *@ 'v5)) +@ ((4 *@ 'v2) +@ ((1 *@ 'v3) +@ (8 *@ 'v3))) <> (((8) +@ (6 *@ 'v5)) +@ ((6) +@ (8 *@ 'v2))) +@ ((6) +@ (9));
0 *@ 'v3 > 5 *@ 'v5;
(0 *@ 'v3) +@ (3 *@ 'v1) > 2 *@ 'v3;
(1 *@ 'v4) +@ (5 *@ 'v2) <> 0 *@ 'v4;
0 *@ 'v3 <= 4 *@ 'v5;
7 *@ 'v1 <> ((8 *@ 'v3) +@ ((2 *@ 'v3) +@ (0 *@ 'v3))) +@ ((7) +@ (3 *@ 'v1));
6 < (2 *@ 'v2) +@ (3 *@ 'v2);
7 < 9 *@ 'v5 >- "false" }

interactive test4 :
sequent { v1 : int; v2 : int; v3 : int; v4 : int; v5 : int; 
7 *@ 'v2 > (4) +@ (4 *@ 'v1);
7 *@ 'v4 < (1 *@ 'v3) +@ (9 *@ 'v2);
2 *@ 'v2 <> 4 *@ 'v5;
(((8 *@ 'v4) +@ (1 *@ 'v1)) +@ (9 *@ 'v5)) +@ (3 *@ 'v1) <> 3 *@ 'v3;
5 *@ 'v1 < 7 *@ 'v3;
3 *@ 'v4 < (0) +@ (6 *@ 'v5);
9 *@ 'v5 < ((4 *@ 'v4) +@ (6 *@ 'v4)) +@ (2 *@ 'v1);
(5 *@ 'v2) +@ (((2) +@ (6 *@ 'v1)) +@ ((3) +@ (4))) = ((2 *@ 'v1) +@ (4 *@ 'v4)) +@ (4 *@ 'v5) in int;
(3 *@ 'v2) +@ (((0 *@ 'v2) +@ (7 *@ 'v1)) +@ ((2 *@ 'v3) +@ (1 *@ 'v5))) <= (4 *@ 'v5) +@ (7);
(5 *@ 'v2) +@ (0 *@ 'v4) >= (8) +@ ((2 *@ 'v1) +@ ((8 *@ 'v3) +@ (9 *@ 'v5))) >- "false" }

interactive test5 :
sequent { v1 : int; v2 : int; v3 : int; v4 : int; v5 : int; 
((2 *@ 'v2) +@ ((1 *@ 'v5) +@ (8 *@ 'v2))) +@ (7 *@ 'v1) >= (8 *@ 'v3) +@ (1 *@ 'v4);
2 *@ 'v5 >= 4 *@ 'v4;
(7 *@ 'v3) +@ (2 *@ 'v2) <= 9 *@ 'v3;
((6 *@ 'v2) +@ ((3) +@ (8))) +@ (0 *@ 'v2) <= ((2) +@ ((6 *@ 'v1) +@ (7 *@ 'v4))) +@ ((1) +@ (1 *@ 'v1));
(6 *@ 'v3) +@ ((0) +@ ((4 *@ 'v4) +@ (4))) < 7 *@ 'v4;
(7 *@ 'v2) +@ ((5) +@ ((0 *@ 'v3) +@ (2))) < (((9 *@ 'v3) +@ (8 *@ 'v5)) +@ ((7 *@ 'v3) +@ (0 *@ 'v5))) +@ ((2 *@ 'v5) +@ (8 *@ 'v4));
6 *@ 'v1 < (((5 *@ 'v4) +@ (1 *@ 'v1)) +@ ((5 *@ 'v3) +@ (3 *@ 'v5))) +@ (((5 *@ 'v1) +@ (1 *@ 'v2)) +@ ((0 *@ 'v3) +@ (0 *@ 'v3)));
(((6) +@ (7 *@ 'v5)) +@ ((5 *@ 'v4) +@ (3 *@ 'v2))) +@ (((4) +@ (7 *@ 'v4)) +@ ((3 *@ 'v4) +@ (5))) <> 0;
(8 *@ 'v2) +@ ((3 *@ 'v2) +@ (8 *@ 'v4)) <= 7 *@ 'v4;
(2 *@ 'v2) +@ (4 *@ 'v5) <> 4 *@ 'v3 >- "false" }

interactive test6 :
sequent { v1 : int; v2 : int; v3 : int; v4 : int; v5 : int; 
2 > (8 *@ 'v1) +@ (5 *@ 'v1);
1 *@ 'v5 <> (1 *@ 'v2) +@ (((0 *@ 'v2) +@ (9 *@ 'v2)) +@ (2 *@ 'v2));
6 <= 3 *@ 'v3;
(9) +@ (4 *@ 'v3) > (4 *@ 'v5) +@ (0 *@ 'v1);
6 *@ 'v2 > (((4 *@ 'v4) +@ (5)) +@ ((8 *@ 'v2) +@ (3 *@ 'v3))) +@ (7 *@ 'v1);
(((7) +@ (0 *@ 'v1)) +@ (7 *@ 'v3)) +@ (((5 *@ 'v3) +@ (9 *@ 'v3)) +@ (6 *@ 'v1)) <= ((3 *@ 'v1) +@ (6 *@ 'v2)) +@ (8 *@ 'v5);
(7) +@ (4 *@ 'v1) < (((7 *@ 'v4) +@ (2 *@ 'v2)) +@ ((7 *@ 'v1) +@ (7))) +@ (((5 *@ 'v1) +@ (0 *@ 'v1)) +@ (1 *@ 'v1));
6 *@ 'v4 < (6 *@ 'v1) +@ (6 *@ 'v3);
6 *@ 'v2 <> (1 *@ 'v1) +@ (9 *@ 'v4);
(((0) +@ (1 *@ 'v4)) +@ ((5 *@ 'v2) +@ (1))) +@ (((6 *@ 'v2) +@ (4 *@ 'v4)) +@ ((7 *@ 'v1) +@ (2 *@ 'v5))) < ((5 *@ 'v3) +@ (1 *@ 'v2)) +@ (7 *@ 'v3) >- "false" }

interactive test7 :
sequent { v1 : int; v2 : int; v3 : int; v4 : int; v5 : int; 
4 *@ 'v4 = (0 *@ 'v1) +@ (2 *@ 'v1) in int;
(1) +@ (((8 *@ 'v5) +@ (6 *@ 'v2)) +@ (3 *@ 'v2)) <> 6 *@ 'v1;
1 *@ 'v3 = 3 *@ 'v3 in int;
1 *@ 'v3 >= ((7 *@ 'v5) +@ (5)) +@ ((1) +@ ((8 *@ 'v3) +@ (8 *@ 'v2)));
(2 *@ 'v5) +@ (((3) +@ (6 *@ 'v1)) +@ ((9 *@ 'v4) +@ (8 *@ 'v5))) <> 4 *@ 'v1;
(7 *@ 'v3) +@ (0 *@ 'v1) >= (4 *@ 'v5) +@ (5 *@ 'v2);
3 *@ 'v5 <= (3 *@ 'v3) +@ (5 *@ 'v2);
(4 *@ 'v1) +@ (8 *@ 'v2) > 3 *@ 'v5;
((2 *@ 'v3) +@ ((6 *@ 'v4) +@ (0))) +@ ((9 *@ 'v5) +@ (4 *@ 'v4)) <= 8 *@ 'v1;
((6 *@ 'v5) +@ (8 *@ 'v3)) +@ ((4) +@ (7 *@ 'v1)) = (1 *@ 'v1) +@ ((9 *@ 'v3) +@ ((1 *@ 'v5) +@ (7 *@ 'v5))) in int >- "false" }

interactive test8 :
sequent { v1 : int; v2 : int; v3 : int; v4 : int; v5 : int; 
4 *@ 'v4 <> (((3 *@ 'v1) +@ (0 *@ 'v3)) +@ (3 *@ 'v5)) +@ (4);
(6 *@ 'v1) +@ (5 *@ 'v1) > 6 *@ 'v1;
((8 *@ 'v3) +@ ((0 *@ 'v4) +@ (2 *@ 'v2))) +@ ((3) +@ ((8 *@ 'v3) +@ (9 *@ 'v5))) >= (0 *@ 'v1) +@ ((9 *@ 'v4) +@ ((0 *@ 'v4) +@ (8 *@ 'v4)));
(2 *@ 'v3) +@ (7 *@ 'v1) = 1 *@ 'v3 in int;
7 *@ 'v1 > ((3 *@ 'v2) +@ (5)) +@ (((7 *@ 'v4) +@ (1)) +@ (7 *@ 'v4));
(((2 *@ 'v4) +@ (1 *@ 'v3)) +@ (2 *@ 'v5)) +@ ((1 *@ 'v4) +@ ((9 *@ 'v4) +@ (8 *@ 'v5))) > (8) +@ (((0 *@ 'v5) +@ (2 *@ 'v2)) +@ (0 *@ 'v4));
(0) +@ ((0 *@ 'v3) +@ ((4 *@ 'v2) +@ (6))) < (((0 *@ 'v1) +@ (3 *@ 'v4)) +@ ((8 *@ 'v2) +@ (8 *@ 'v3))) +@ ((8 *@ 'v3) +@ ((4 *@ 'v2) +@ (9 *@ 'v2)));
(8 *@ 'v1) +@ ((8 *@ 'v3) +@ (4 *@ 'v4)) > (((9 *@ 'v3) +@ (0 *@ 'v1)) +@ ((7 *@ 'v5) +@ (9))) +@ (4 *@ 'v2);
7 *@ 'v1 > 6 *@ 'v1;
(8) +@ ((5) +@ ((8 *@ 'v2) +@ (9 *@ 'v5))) < 2 *@ 'v2 >- "false" }

interactive test9 :
sequent { v1 : int; v2 : int; v3 : int; v4 : int; v5 : int; 
8 >= 6 *@ 'v2;
((9 *@ 'v3) +@ ((4 *@ 'v4) +@ (0))) +@ (1 *@ 'v1) >= (9 *@ 'v5) +@ (5 *@ 'v3);
((2 *@ 'v4) +@ (1 *@ 'v5)) +@ (4 *@ 'v4) <> (((2 *@ 'v5) +@ (0 *@ 'v5)) +@ (1 *@ 'v3)) +@ ((6) +@ ((4 *@ 'v2) +@ (0)));
(2) +@ ((1 *@ 'v4) +@ ((8 *@ 'v3) +@ (6))) > (1) +@ ((8 *@ 'v5) +@ (2 *@ 'v5));
(0 *@ 'v3) +@ (6 *@ 'v1) > (0) +@ ((1 *@ 'v4) +@ (1 *@ 'v3));
(6 *@ 'v1) +@ (1) = ((5 *@ 'v5) +@ (2 *@ 'v4)) +@ (8 *@ 'v5) in int;
(((7) +@ (4)) +@ ((6 *@ 'v4) +@ (8 *@ 'v1))) +@ (6 *@ 'v4) < (((5 *@ 'v4) +@ (7 *@ 'v1)) +@ ((6) +@ (0 *@ 'v1))) +@ ((3 *@ 'v1) +@ (2 *@ 'v1));
4 *@ 'v1 = 6 *@ 'v4 in int;
(((9) +@ (9 *@ 'v2)) +@ (2 *@ 'v1)) +@ (((7 *@ 'v5) +@ (0 *@ 'v3)) +@ ((6 *@ 'v3) +@ (9 *@ 'v3))) >= ((8 *@ 'v5) +@ (0 *@ 'v4)) +@ (6 *@ 'v3);
0 *@ 'v1 <> 6 *@ 'v5 >- "false" }
