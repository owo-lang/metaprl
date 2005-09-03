module type SetSig = sig
   type 'a set
   val empty : 'a set
   val add : 'a -> 'a set -> 'a set
   val mem : 'a -> 'a set -> bool
end;;

module type ChooseSetSig = sig
    include SetSig
    type 'a choice = Element of 'a | Empty
    val choose : 'a set -> 'a choice
end;;

module Sets : sig
   module Set : SetSig
   module ChooseSet : ChooseSetSig
end = struct
   module Set = struct
      type 'a set = 'a list
      let empty = []
      let add x l = x :: l
      let mem x l = List.mem x l
   end

   module ChooseSet = struct
      include Set
      type 'a choice = Element of 'a | Empty
      let choose = function
       | x :: _ -> Element x
       | [] -> Empty
   end
end;;