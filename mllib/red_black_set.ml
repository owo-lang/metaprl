(*
 * Build a set using a red-black tree.
 * Every node in the tree is colored either black or red.
 * A read black tree has the following invariants:
 *    1. Every leaf is colored black
 *    2. All children of every red node are black.
 *    3. Every path from the root to a leaf has the
 *       same number of black nodes as every other path.
 *
 * We get some corollaries:
 *    1. The longest path from the root to a leaf is
 *       at most twice as long as the shortest path.
 *    2. Both children of a red node are either leaves,
 *       or they are both not.
 *)
open Format

(*
 * Our ordered type includes a printer.
 *)
module type OrderedType =
sig
   type t

   val print : t -> unit
   val compare : t -> t -> int
end

(*
 * Make an ordered type with a default printer.
 *)
module MakeDefaultOrder (Ord : Set.OrderedType) =
struct
   type t = Ord.t
   let print x = print_string "*"
   let compare = Ord.compare
end

(*
 * Our set has a printer.
 *)
module type S =
sig
   type elt
   type t

   val empty : t
   val is_empty : t -> bool
   val mem : t -> elt -> bool
   val add : elt -> t -> t
   val make : elt -> t
   val remove : elt -> t -> t
   val union : t -> t -> t
   val elements : t -> elt list
   val iter : (elt -> unit) -> t -> unit
   val cardinal : t -> int
   val mem_filt : t -> elt list -> elt list
   val fst_mem_filt : t -> (elt * 'a) list -> (elt * 'a) list
   val not_mem_filt : t -> elt list -> elt list
   val intersectp : t -> t -> bool
   val of_list : elt list -> t

   val print : t -> unit
end

(*
 * Make the set.
 *)
module MakeDebug (Ord : OrderedType) =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type elt = Ord.t

   (*
    * Table is a binary tree.
    * Color is kept in the label to save space.
    *)
   type tree =
      Leaf
    | Red of elt * tree * tree * int
    | Black of elt * tree * tree * int

   (*
    * The tree is always balanced, so we don't need
    * extra mutable fields.
    *)
   type t = tree

   (*
    * Path into the tree.
    *)
   type path =
      Left of tree
    | Right of tree
    | Delete of tree

   (*
    * Exception for unchanged tree during insertion.
    *)
   exception Unchanged

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Size of a table.
    *)
   let cardinality = function
      Red (_, _, _, size) ->
         size
    | Black (_, _, _, size) ->
         size
    | Leaf ->
         0

   let cardinal = cardinality

   (*
    * Add two nodes.
    *)
   let new_black key left right =
      Black (key, left, right, cardinality left + cardinality right + 1)

   let new_red key left right =
      Red (key, left, right, cardinality left + cardinality right + 1)

   (************************************************************************
    * DEBUGGING                                                            *
    ************************************************************************)

   (*
    * Check the size of the set.
    *)
   let rec check_size = function
      Black (_, left, right, size) ->
         if size <> check_size left + check_size right then
            raise (Invalid_argument "check_size");
         size

    | Red (_, left, right, size) ->
         if size <> check_size left + check_size right then
            raise (Invalid_argument "check_size");
         size

    | Leaf ->
         1

   (*
    * Check the red-invariant.
    *)
   let rec check_red = function
      Red (_, left, right, _) ->
         begin
         match left, right with
            Red _, _
          | _, Red _ ->
               raise (Failure "Red_black_set.check_red")

          | _ ->
               check_red left;
               check_red right
         end
    | Black (_, left, right, _) ->
         check_red left;
         check_red right

    | Leaf ->
         ()

   (*
    * Check the black invariant.
    *)
   let rec black_depth i = function
      Black (_, left, _, _) ->
         black_depth (succ i) left
    | Red (_, left, _, _) ->
         black_depth i left
    | Leaf ->
         i

   let rec check_black_aux i j = function
      Black (_, left, right, _) ->
         check_black_aux i (succ j) left;
         check_black_aux i (succ j) right
    | Red (_, left, right, _) ->
         check_black_aux i j left;
         check_black_aux i j right
    | Leaf ->
         if j <> i then
            raise (Failure "Red_black_set.check_black")

   let check_black tree =
      check_black_aux (black_depth 0 tree) 0 tree

   (*
    * Check that all the nodes are sorted.
    *)
   let rec check_sort_lt key = function
      Black (key', left, right, _) ->
         if Ord.compare key' key >= 0 then
            raise (Failure "Red_black_set.check_sort");
         check_sort_lt key' left;
         check_sort_gt_lt key' key right

    | Red (key', left, right, _) ->
         if Ord.compare key' key >= 0 then
            raise (Failure "Red_black_set.check_sort");
         check_sort_lt key' left;
         check_sort_gt_lt key' key right

    | Leaf ->
         ()

   and check_sort_gt key = function
      Black (key', left, right, _) ->
         if Ord.compare key' key <= 0 then
            raise (Failure "Red_black_set.check_sort");
         check_sort_gt_lt key key' left;
         check_sort_gt key right

    | Red (key', left, right, _) ->
         if Ord.compare key' key <= 0 then
            raise (Failure "Red_black_set.check_sort");
         check_sort_gt_lt key key' left;
         check_sort_gt key right

    | Leaf ->
         ()

   and check_sort_gt_lt key key' = function
      Black (key'', left, right, _) ->
         if Ord.compare key'' key <= 0 || Ord.compare key'' key' >= 0 then
            raise (Failure "Red_black_set.check_sort");
         check_sort_gt_lt key key'' left;
         check_sort_gt_lt key'' key' right

    | Red (key'', left, right, _) ->
         if Ord.compare key'' key <= 0 || Ord.compare key'' key' >= 0 then
            raise (Failure "Red_black_set.check_sort");
         check_sort_gt_lt key key'' left;
         check_sort_gt_lt key'' key' right

    | Leaf ->
         ()

   let check_sort = function
      Black (key, left, right, _) ->
         check_sort_lt key left;
         check_sort_gt key right
    | Red _ ->
         raise (Failure "Red_black_set.check_sort: root is red")
    | Leaf ->
         ()

   (*
    * Perform all the checks.
    *)
   let check tree =
      check_red tree;
      check_black tree;
      check_sort tree;
      check_size tree;
      tree

   (*
    * Print the tree.
    *)
   let rec print tree =
      print_space ();
      match tree with
         Black (key, left, right, size) ->
            print_string "(";
            open_hvbox 0;
            print_string "Black";
            print_space ();
            Ord.print key;
            print_string ":";
            print_int size;
            print left;
            print right;
            print_string ")";
            close_box ()

       | Red (key, left, right, size) ->
            print_string "(";
            open_hvbox 0;
            print_string "Red";
            print_space ();
            Ord.print key;
            print_string ":";
            print_int size;
            print left;
            print right;
            print_string ")";
            close_box ()

       | Leaf ->
            print_string "Leaf"

   let print_path_entry = function
      Left tree ->
         print_space ();
         print_string "Left";
         open_hvbox 0;
         print tree;
         close_box ()
    | Right tree ->
         print_space ();
         print_string "Right";
         open_hvbox 0;
         print tree;
         close_box ()

    | Delete tree ->
         print_space ();
         print_string "Delete";
         open_hvbox 1;
         print tree;
         close_box ()

   let print_path path =
      open_vbox 0;
      List.iter print_path_entry path;
      print_newline ()

   (************************************************************************
    * INSERTION                                                            *
    ************************************************************************)

   (*
    * Insert an entry into the tree.
    *)
   let rec insert key = function
      Black (key0, left0, right0, size0) ->
         begin
            let comp = Ord.compare key key0 in
               if comp = 0 then
                  raise Unchanged
               else if comp < 0 then
                  match left0 with
                     Black _
                   | Leaf ->
                        (*
                         * Ok even if child becomes red.
                         *)
                        Black (key0, insert key left0, right0, succ size0)

                   | Red (key1, left1, right1, size1) ->
                        let comp = Ord.compare key key1 in
                           if comp = 0 then
                              raise Unchanged
                           else if comp < 0 then
                              match insert key left1, right0 with
                                 Red _ as node, Red (key2, left2, right2, size2) ->
                                    (*
                                     * Recoloring:
                                     *
                                     *     key0:b             key0:r
                                     *    /     \             /    \
                                     *  key1:r key2:r      key1:b  key2:b
                                     *   /    \             /   \
                                     * key2:r right1     key2:r right1
                                     *)
                                    Red (key0,
                                         Black (key1, node, right1, succ size1),
                                         Black (key2, left2, right2, size2),
                                         succ size0)
                               | Red _ as node, _ ->
                                    (*
                                     * Rotation:
                                     *
                                     *      key0:b             key1:b
                                     *     /     \             /    \
                                     *   key1:r key2:b      key3:r  key0:b
                                     *   /    \                    /    \
                                     * key3:r right1             right1 key2:r
                                     *)
                                    Black (key1,
                                           node,
                                           new_red key0 right1 right0,
                                           succ size0)
                               | node, _ ->
                                    (*
                                     * Inline:
                                     *
                                     *        key0:b         key0:b
                                     *        /    \         /    \
                                     *     key1:r key2    key1:r  key2
                                     *     /   \          /    \
                                     *  key3:b right1  key3:b right1
                                     *)
                                    Black (key0,
                                           Red (key1, node, right1, size1),
                                           right0,
                                           succ size0)
                           else
                              match insert key right1, right0 with
                                 Red _ as node, Red (key2, left2, right2, size2) ->
                                    (*
                                     * Recoloring:
                                     *
                                     *       key0:b              key0:r
                                     *       /    \              /   \
                                     *    key1:r key2:r       key1:b key2:b
                                     *    /   \               /   \
                                     *  left1 node:r        left1 node:r
                                     *)
                                    Red (key0,
                                         Black (key1, left1, node, succ size1),
                                         Black (key2, left2, right2, size2),
                                         succ size0)
                               | Red (key3, left3, right3, size3), _ ->
                                    (*
                                     * Rotation:
                                     *
                                     *       key0:b              key3:b
                                     *       /    \             /      \
                                     *    key1:r  right0     key1:r    key0:r
                                     *    /   \              /    \    /    \
                                     *  left1 key3:r     left1 left3 right3 right0
                                     *        /   \
                                     *      left3 right3
                                     *)
                                    Black (key3,
                                           new_red key1 left1 left3,
                                           new_red key0 right3 right0,
                                           succ size0)
                               | node3, _ ->
                                    (*
                                     * Inline:
                                     *
                                     *      key0:b
                                     *      /    \
                                     *   key1:r  right0
                                     *   /     \
                                     * left1  node3:b
                                     *)
                                    Black (key0,
                                           Red (key1, left1, node3, succ size1),
                                           right0,
                                           succ size0)
               else
                  (* comp > 0 *)
                  match right0 with
                     Black _
                   | Leaf ->
                        (*
                         * Node can be replaced even if it becomes red.
                         *)
                        Black (key0, left0, insert key right0, succ size0)

                   | Red (key2, left2, right2, size2) ->
                        let comp = Ord.compare key key2 in
                           if comp = 0 then
                              raise Unchanged
                           else if comp < 0 then
                              match left0, insert key left2 with
                                 Red (key1, left1, right1, size1), (Red _ as node) ->
                                    (*
                                     * Recoloring:
                                     *
                                     *       key0:b              key0:r
                                     *       /    \              /   \
                                     *    key1:r key2:r       key1:b key2:b
                                     *           /   \               /   \
                                     *        node:r right2       node:r right2
                                     *)
                                    Red (key0,
                                         Black (key1, left1, right1, size1),
                                         Black (key2, node, right2, succ size2),
                                         succ size0)
                               | _, Red (key3, left3, right3, size3) ->
                                    (*
                                     * Rotate:
                                     *
                                     *       key0:b                  key3:b
                                     *       /    \                  /     \
                                     *    key1:b  key2:r          key0:r   key2:r
                                     *            /    \          /   \    /     \
                                     *         key3:r  right2 left0 left3 right3 right2
                                     *         /   \
                                     *      left3 right3
                                     *)
                                    Black (key3,
                                           new_red key0 left0 left3,
                                           new_red key2 right3 right2,
                                           succ size0)
                               | _, node3 ->
                                    (*
                                     * Inline:
                                     *
                                     *      key0:b
                                     *      /    \
                                     *   left0  key2:r
                                     *          /   \
                                     *      key3:b right2
                                     *)
                                    Black (key0,
                                           left0,
                                           Red (key2, node3, right2, succ size2),
                                           succ size0)
                           else
                              match left0, insert key right2 with
                                 Red (key1, left1, right1, size1), (Red _ as node) ->
                                    (*
                                     * Recoloring:
                                     *
                                     *     key0:b                  key0:r
                                     *     /    \                  /   \
                                     *   key1:r key2:r          key1:b key2:b
                                     *          /    \                 /    \
                                     *        left2  node:r          left2 node:r
                                     *)
                                    Red (key0,
                                         Black (key1, left1, right1, size1),
                                         Black (key2, left2, node, succ size2),
                                         succ size0)
                               | _, (Red _ as node) ->
                                    (*
                                     * Rotation:
                                     *
                                     *      key0:b                 key2:b
                                     *      /    \                 /    \
                                     *   left0:b key2:r         key0:r node:r
                                     *           /   \          /   \
                                     *         left2 node:r left0:b left2
                                     *)
                                    Black (key2,
                                           new_red key0 left0 left2,
                                           node,
                                           succ size0)
                               | _, node3 ->
                                    (*
                                     * Inline:
                                     *
                                     *     key0:b
                                     *     /   \
                                     * left0:b key2:r
                                     *         /    \
                                     *       left2 node3:b
                                     *)
                                    Black (key0,
                                           left0,
                                           Red (key2, left2, node3, succ size2),
                                           succ size0)
         end
    | Leaf ->
         (* Leaf is colored red *)
         Red (key, Leaf, Leaf, 1)

    | Red _ ->
         (* Red nodes will not come up *)
         raise (Invalid_argument "Red_black_set.insert")

   (*
    * Add an element to the set.
    *)
   let add key = function
      Leaf ->
         Black (key, Leaf, Leaf, 1)
    | node ->
         try
            match insert key node with
               Red (key, left, right, size) ->
                  Black (key, left, right, size)
             | tree ->
                  tree
         with
            Unchanged ->
               node

   (************************************************************************
    * REMOVAL                                                              *
    ************************************************************************)

   (*
    * Construct a path during the removal.
    *)
   let rec delete key path node =
      match node with
         Black (key', left, right, _) ->
            let comp = Ord.compare key key' in
               if comp = 0 then
                  match left, right with
                     Leaf, Leaf ->
                        lift_black key path Leaf
                   | Red (key, left, right, size), Leaf ->
                        lift key path (Black (key, left, right, size))
                   | _ ->
                        delete_min (Delete node :: path) right
               else if comp < 0 then
                  delete key (Left node :: path) left
               else
                  delete key (Right node :: path) right
       | Red (key', left, right, _) ->
            let comp = Ord.compare key key' in
               if comp = 0 then
                  match right with
                     Leaf ->
                        lift key path Leaf
                   | _ ->
                        delete_min (Delete node :: path) right
               else if comp < 0 then
                  delete key (Left node :: path) left
               else
                  delete key (Right node :: path) right
       | Leaf ->
            raise Not_found

   and delete_min path node =
      match node with
         Black (key, Leaf, Leaf, _) ->
            lift_black key path Leaf
       | Black (key, Leaf, Red (key', left, right, size), _) ->
            lift key path (Black (key', left, right, size))
       | Red (key, Leaf, Leaf, _) ->
            lift key path Leaf
       | Black (_, left, _, _) ->
            delete_min (Left node :: path) left
       | Red (_, left, _, _) ->
            delete_min (Left node :: path) left
       | Leaf ->
            raise Not_found

   (*
    * Copy the tree with no need to propagate black.
    *)
   and lift key path node =
      match path, node with
         Left (Black (key0, _, right0, size0)) :: path, left ->
            lift key path (Black (key0, left, right0, pred size0))
       | Left (Red (key0, _, right0, size0)) :: path, left ->
            lift key path (Red (key0, left, right0, pred size0))
       | Right (Black (key0, left0, _, size0)) :: path, right ->
            lift key path (Black (key0, left0, right, pred size0))
       | Right (Red (key0, left0, _, size0)) :: path, right ->
            lift key path (Red (key0, left0, right, pred size0))
       | Delete (Black (key0, left0, _, size0)) :: path, right ->
            lift key path (Black (key, left0, right, pred size0))
       | Delete (Red (key0, left0, _, size0)) :: path, right ->
            lift key path (Red (key, left0, right, pred size0))
       | [], node ->
            node
       | Left Leaf :: _, _
       | Right Leaf :: _, _
       | Delete Leaf :: _, _ ->
            raise (Invalid_argument "lift")

   (*
    * Propagate the extra black up the tree.
    *)
   and lift_black key path node =
      match path, node with
         Left (Black (key0, _, right0, size0)) :: path, left ->
            begin
               match right0 with
                  Black (key2, left2, right2, size2) ->
                     begin
                        match left2, right2 with
                           _, Red (key3, left3, right3, size3) ->
                              (*
                               *    key0:b                 key2:b
                               *   /     \                 /    \
                               * left:bb key2:b          key0:b right2:b
                               *         /   \           /    \
                               *      left2  right2:r  left:b left2
                               *)
                              lift key path (**)
                                 (Black (key2,
                                         new_black key0 left left2,
                                         Black (key3, left3, right3, size3),
                                         pred size0))

                         | Red (key3, left3, right3, size3), _ ->
                              (*
                               *      key0:b                    key3:b
                               *      /    \                  /       \
                               *   left:bb key2:b          key0:b     key2:b
                               *           /    \          /    \     /     \
                               *        key3:r right2:b left:b left3 right3 right2:b
                               *        /   \
                               *     left3 right3
                               *)
                              lift key path (**)
                                 (Black (key3,
                                         new_black key0 left left3,
                                         new_black key2 right3 right2,
                                         pred size0))

                         | _ ->
                              (*
                               *     key0:b                 key0:bb
                               *     /    \                 /    \
                               * left:bb  key2:b       left:b    key2:r
                               *          /    \                 /    \
                               *       left2:b right2:b       left2:b right2:b
                               *)
                              lift_black key path (**)
                                 (Black (key0,
                                         left,
                                         Red (key2, left2, right2, size2),
                                         pred size0))
                     end

                | Red (key2, left2, right2, size2) ->
                     begin
                        match left2 with
                           Black (key3, Red (key4, left4, right4, _), d, _) ->
                              (*
                               *     key0:b                   key2:b
                               *     /    \                   /    \
                               *  left:bb key2:r           key4:b right2:b
                               *          /    \           /     \
                               *        key3:b right2:b  key0:r   key3:r
                               *        /   \           /   \    /     \
                               *     key4:r  d     left:b left4 right4 d
                               *     /   \
                               *   left4 right4
                               *)
                              lift key path (**)
                                 (Black (key2,
                                         new_black key4 (**)
                                            (new_red key0 left left4)
                                            (new_red key3 right4 d),
                                         right2,
                                         pred size0))

                         | Black (key3, c, Red (key4, left4, right4, size4), _) ->
                              (*
                               *     key0:b                   key2:b
                               *     /    \                   /    \
                               * left:bb  key2:r            key3:r right2
                               *          /    \            /    \
                               *       key3:b  right2     key0:b key4:b
                               *       /    \             /    \
                               *      c    key4:r       left:b  c
                               *)
                              lift key path (**)
                                 (Black (key2,
                                         new_red key3 (**)
                                            (new_red key0 left c)
                                            (Black (key4, left4, right4, size4)),
                                            right2,
                                            pred size0))

                         | Black (key3, c, d, _) ->
                              (*
                               *     key0:b               key2:b
                               *     /    \               /     \
                               * left:bb key2:r         key0:b  right2:b
                               *         /   \          /     \
                               *      key3:b right2:b left:b  key3:r
                               *      /   \                   /   \
                               *     c:b  d:b                c:b   d:b
                               *)
                              lift key path (**)
                                 (Black (key2,
                                         new_black key0 left (new_red key3 c d),
                                         right2,
                                         pred size0))

                         | Red _
                         | Leaf ->
                              raise (Invalid_argument "lift_black1")
                     end

                | Leaf ->
                     raise (Invalid_argument "lift_black2")
            end

       | Right (Black (key0, left0, _, size0)) :: path, right ->
            begin
               match left0 with
                  Black (key1, left1, right1, size1) ->
                     begin
                        match left1, right1 with
                           Red (key3, left3, right3, size3), _ ->
                              (*
                               *        key0:b              key1:b
                               *        /    \              /    \
                               *      key1:b right:bb   left1:b key0:b
                               *      /    \                    /    \
                               *  left1:r right1            right1   right:b
                               *)
                              lift key path (**)
                                 (Black (key1,
                                         Black (key3, left3, right3, size3),
                                         new_black key0 right1 right,
                                         pred size0))

                         | _, Red (key3, left3, right3, size3) ->
                              (*
                               *      key0:b                    key3:b
                               *      /     \                 /        \
                               *    key1:b  right:bb        key1:b     key0:b
                               *    /    \                 /    \      /    \
                               * left1:b key3:r         left1:b left3 right3 right
                               *         /    \
                               *       left3 right3
                               *)
                              lift key path (**)
                                 (Black (key3,
                                         new_black key1 left1 left3,
                                         new_black key0 right3 right,
                                         pred size0))

                         | _ ->
                              (*
                               *        key0:b                 key0:bb
                               *        /    \                 /    \
                               *    key1:b  right:bb      key1:r    right:bb
                               *    /    \                /    \
                               * left1:b right1:b     left1:b right1:b
                               *)
                              lift_black key path (**)
                                 (Black (key0,
                                         Red (key1, left1, right1, size1),
                                         right,
                                         pred size0))

                     end

                | Red (key1, left1, right1, size1) ->
                     begin
                        match right1 with
                           Black (key3, d, Red (key4, left4, right4, _), _) ->
                              (*
                               *        key0:b                key1:b
                               *        /     \               /    \
                               *    key1:r   right:bb    left1:b  key4:b
                               *    /    \                        /    \
                               * left1:b key3:b              key3:r    key0:r
                               *         /   \               /   \     /    \
                               *        d    key4:r         d  left4 right4 right:b
                               *             /   \
                               *          left4 right4
                               *)
                              lift key path (**)
                                 (Black (key1,
                                         left1,
                                         new_black key4 (**)
                                            (new_red key3 d left4)
                                            (new_red key0 right4 right),
                                            pred size0))

                         | Black (key3, Red (key4, left4, right4, size4), c, _) ->
                              (*
                               *     key0:b                 key1:b
                               *     /    \                 /    \
                               *  key1:r  right:bb       left1  key3:r
                               *  /    \                        /    \
                               * left1 key3:b                 key4:b key0:b
                               *       /   \                         /   \
                               *    key4:r c                        c   right:b
                               *)
                              lift key path (**)
                                 (Black (key1,
                                         left1,
                                         new_red key3 (**)
                                            (Black (key4, left4, right4, size4))
                                            (new_black key0 c right),
                                            pred size0))

                         | Black (key3, c, d, size3) ->
                              (*
                               *      key0:b               key1:b
                               *      /    \               /    \
                               *   key1:r  right:bb     left1  key0:b
                               *   /   \                       /    \
                               * left1 key3:b               key3:r right:b
                               *       /   \                /    \
                               *      c:b  d:b            c:b    d:b
                               *)
                              lift key path (**)
                                 (Black (key1,
                                         left1,
                                         new_black key0 (Red (key3, c, d, size3)) right,
                                         pred size0))


                         | Red _
                         | Leaf ->
                              raise (Invalid_argument "lift_black3")
                     end

                | Leaf ->
                     raise (Invalid_argument "lift_black4")
            end

       | Left (Red (key0, _, right0, size0)) :: path, left ->
            begin
               match right0 with
                  Black (key2, left2, right2, size2) ->
                     begin
                        match left2, right2 with
                           _, Red (key3, left3, right3, size3) ->
                              (*
                               *     key0:r                   key2:r
                               *     /    \                   /    \
                               *  left:bb key2:b           key0:b right2:b
                               *          /    \           /    \
                               *       left2:b right2:r left:b left2:b
                               *)
                              lift key path (**)
                                 (Red (key2,
                                       new_black key0 left left2,
                                       Black (key3, left3, right3, size3),
                                       pred size0))

                         | Red (key3, left3, right3, size3), _ ->
                              (*
                               *     key0:r                   key3:b
                               *     /    \                  /       \
                               * left:bb  key2:b          key0:r     key2:r
                               *          /    \         /   \       /    \
                               *        key3:r right2 left:b left3 right3 right2
                               *        /   \
                               *     left3 right3
                               *)
                              lift key path (**)
                                 (Black (key3,
                                         new_red key0 left left3,
                                         new_red key2 right3 right2,
                                         pred size0))

                         | _ ->
                              (*
                               *     key0:r                  key0:b
                               *    /     \                  /    \
                               * left:bb  key2:b          left:b key2:r
                               *          /    \                 /   \
                               *     left2:b  right2:b      left2:b right2:b
                               *)
                              lift key path (**)
                                 (Black (key0,
                                         left,
                                         Red (key2, left2, right2, size2),
                                         pred size0))
                     end
                | Red _
                | Leaf ->
                     raise (Invalid_argument "lift_black5")
            end

       | Right (Red (key0, left0, _, size0)) :: path, right ->
            begin
               match left0 with
                  Black (key1, left1, right1, size1) ->
                     begin
                        match left1, right1 with
                           Red (key3, left3, right3, size3), _ ->
                              (*
                               *       key0:r                key1:r
                               *       /    \                /    \
                               *    key1:b  right:bb      left1:b key0:b
                               *   /     \                        /    \
                               * left1:r right1                right1 right:b
                               *)
                              lift key path (**)
                                 (Red (key1,
                                       Black (key3, left3, right3, size3),
                                       new_black key0 right1 right,
                                       pred size0))

                         | _, Red (key3, left3, right3, size3) ->
                              (*
                               *       key0:r                 key3:b
                               *       /    \                /       \
                               *     key1:b right:bb      key1:r    key0:r
                               *     /    \               /    \    /    \
                               *  left1  key3:r        left1 left3 right3 right:b
                               *         /    \
                               *       left3 right3
                               *)
                              lift key path (**)
                                 (Black (key3,
                                         new_red key1 left1 left3,
                                         new_red key0 right3 right,
                                         pred size0))

                         | _ ->
                              (*
                               *        key0:r              key0:b
                               *        /    \              /    \
                               *     key1:b right:bb     key1:r right:b
                               *     /   \               /    \
                               * left1:b right1:b     left1:b right1:b
                               *)
                              lift key path (**)
                                 (Black (key0,
                                         Red (key1, left1, right1, size1),
                                         right,
                                         pred size0))

                     end
                | Red _
                | Leaf ->
                     raise (Invalid_argument "lift_black6")
            end

       | Delete (Black (_, left0, right0, size0)) :: path, node ->
            lift_black key (Right (Black (key, left0, right0, size0)) :: path) node

       | Delete (Red (_, left0, right0, size0)) :: path, node ->
            lift_black key (Right (Red (key, left0, right0, size0)) :: path) node

       | [], node ->
            node

       | Left Leaf :: _, _
       | Right Leaf :: _, _
       | Delete Leaf :: _, _ ->
            raise (Invalid_argument "lift_black7")

   (*
    * Remove the item.
    *)
   let remove key tree =
      try delete key [] tree with
         Not_found ->
            tree

   (************************************************************************
    * UNION & INTERSECTION                                                 *
    ************************************************************************)

   (*
    * Build a path into a tree.
    *)
   let rec initial_path path node =
      match node with
         Black (_, Leaf, _, _) ->
            Left node :: path
       | Red (_, Leaf, _, _) ->
            Left node :: path
       | Black (_, left, _, _) ->
            initial_path (Left node :: path) left
       | Red (_, left, _, _) ->
            initial_path (Left node :: path) left
       | Leaf ->
            raise (Invalid_argument "initial_path")

   let key_of_path = function
      Left (Black (key, _, _, _)) :: _ ->
         key
    | Left (Red (key, _, _, _)) :: _ ->
         key
    | Right (Black (key, _, _, _)) :: _ ->
         key
    | Right (Red (key, _, _, _)) :: _ ->
         key
    | _ ->
         raise (Invalid_argument "key_of_path")

   let rec next_path = function
      Left (Black (_, _, Leaf, _)) :: path ->
         next_path path
    | Left (Red (_, _, Leaf, _)) :: path ->
         next_path path
    | Left (Black (_, _, right, _)) :: path ->
         initial_path path right
    | Left (Red (_, _, right, _)) :: path ->
         initial_path path right
    | Right  _ :: path ->
         next_path path
    | [] ->
         raise Not_found
    | _ ->
         raise (Invalid_argument "next_path")

   (*
    * Get the elements of the list.
    *)
   let rec to_list_aux elements = function
      Black (key, left, right, _) ->
         to_list_aux (key :: to_list_aux elements right) left
    | Red (key, left, right, _) ->
         to_list_aux (key :: to_list_aux elements right) left
    | Leaf ->
         elements

   let to_list = to_list_aux []

   let elements = to_list

   let rec reverse elements = function
      h :: t ->
         reverse (h :: elements) t
    | [] ->
         elements

   let rec merge elements elements1 elements2 =
      match elements1, elements2 with
         key1 :: tl1, key2 :: tl2 ->
            let comp = Ord.compare key1 key2 in
               if comp = 0 then
                  merge (key1 :: elements) tl1 tl2
               else if comp < 0 then
                  merge (key1 :: elements) tl1 elements2
               else
                  merge (key2 :: elements) elements1 tl2
       | _, [] ->
            reverse elements1 elements
       | [], _ ->
            reverse elements2 elements

   (*
    * Log of a number.
    *)
   let rec log2 i x =
      if 1 lsl i >= x then
         i
      else
         log2 (succ i) x

   (*
    * Build a set from a list.
    *)
   let rec log2 i j =
      if 1 lsl i >= j then
         i
      else
         log2 (succ i) j

   let rec of_array depth max_depth elements off len =
      if len = 1 then
         if depth = max_depth then
            Red (elements.(off), Leaf, Leaf, 1)
         else
            Black (elements.(off), Leaf, Leaf, 1)
      else if len = 2 then
         Black (elements.(off + 1), Red (elements.(off), Leaf, Leaf, 1), Leaf, 2)
      else
         let len2 = len lsr 1 in
            Black (elements.(off + len2),
                   of_array (succ depth) max_depth elements off len2,
                   of_array (succ depth) max_depth elements (off + len2 + 1) (len - len2 - 1),
                   len)

   let of_list = function
      [] ->
         Leaf
    | [key] ->
         Black (key, Leaf, Leaf, 1)
    | elements ->
         let elements = Array.of_list elements in
         let length = Array.length elements in
         let max_depth = pred (log2 1 (succ length)) in
            of_array 0 max_depth elements 0 length

   (*
    * Union flattens the two trees,
    * merges them, then creates a new tree.
    *)
   let union s1 s2 =
      let elements1 = to_list s1 in
      let elements2 = to_list s2 in
         of_list (merge [] elements1 elements2)

   (*
    * See if two sets intersect.
    *)
   let rec intersect_aux path1 path2 =
      let key1 = key_of_path path1 in
      let key2 = key_of_path path2 in
      let comp = Ord.compare key1 key2 in
         if comp = 0 then
            true
         else if comp < 0 then
            intersect_aux (next_path path1) path2
         else
            intersect_aux path1 (next_path path2)

   let intersectp s1 s2 =
      match s1, s2 with
         Leaf, _
       | _, Leaf ->
            false
       | _ ->
            let path1 = initial_path [] s1 in
            let path2 = initial_path [] s2 in
               try intersect_aux path1 path2 with
                  Not_found ->
                     false

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Search without reorganizing the tree.
    *)
   let rec mem tree key =
      match tree with
         Black (key', left, right, _) ->
            let comp = Ord.compare key key' in
               if comp = 0 then
                  true
               else if comp < 0 then
                  mem left key
               else
                  mem right key

       | Red (key', left, right, _) ->
            let comp = Ord.compare key key' in
               if comp = 0 then
                  true
               else if comp < 0 then
                  mem left key
               else
                  mem right key

       | Leaf ->
            false

   (*
    * An empty tree is just a leaf.
    *)
   let empty = Leaf

   let is_empty = function
      Leaf ->
         true
    | _ ->
         false

   let make key =
      Black (key, Leaf, Leaf, 1)

   (*
    * Iterate a function over the hashtable.
    *)
   let rec iter f = function
      Black (key, left, right, _) ->
         iter f left;
         f key;
         iter f right
    | Red (key, left, right, _) ->
         iter f left;
         f key;
         iter f right
    | Leaf ->
         ()

   (*
    * Intersection.
    *)
   let rec mem_filt s = function
      [] ->
         []
    | (h :: t) as l ->
         if mem s h then
            let rem = mem_filt s t in
               if rem == t then
                  l
               else
                  h :: rem
         else
            mem_filt s t

   let rec not_mem_filt s = function
      [] ->
         []
    | (h :: t) as l ->
         if mem s h then
            not_mem_filt s t
         else
            let rem = not_mem_filt s t in
               if rem == t then
                  l
               else
                  h :: rem

   let rec fst_mem_filt s = function
      [] ->
         []
    | (((v, _) as h) :: t) as l ->
         if mem s v then
            let rem = fst_mem_filt s t in
               if rem == t then
                  l
               else
                  h :: rem
         else
            fst_mem_filt s t
end

(*
 * Make the default.
 *)
module Make (Ord : Set.OrderedType) =
   MakeDebug (MakeDefaultOrder (Ord))

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
