(*
 * This is the information about modules.
 * Its really derived from Filter_cache
 *)

open Printf

open Debug
open Imp_dag

open Theory

open Filter_summary_type
open Filter_summary
open Filter_summary_util
open Filter_cache
open Filter_debug
open Filter_util
open Filter_prog
open Infix

include Package_type

module Package : PackageSig =
struct
   (*
    * A package may have a signature, and an implementation
    * as a cache.
    *)
   type package =
      { mutable pack_status : status;
        pack_name : string;
        mutable pack_sig  : StrFilterCache.sig_info option;
        mutable pack_info : StrFilterCache.info option
      }
   
   (*
    * Built from a filter cache.
    *)
   type t =
      { pack_cache : StrFilterCache.t;
        pack_dag : package ImpDag.t;
        mutable pack_packages : package ImpDag.node list
      }
   
   (*
    * Create the cache.
    *)
   let create path =
      { pack_cache = StrFilterCache.create path;
        pack_dag = ImpDag.create ();
        pack_packages = []
      }
   
   (*
    * See if a theory is already loaded.
    *)
   let is_theory_loaded name =
      let rec search = function
         { thy_name = name' } :: t ->
            if name = name' then
               true
            else
               search t
       | [] ->
            false
      in
         search (get_theories ())
   
   (*
    * Get a theory by name.
    *)
   let get_theory name =
      let rec search = function
         thy :: t ->
            if thy.thy_name = name then
               thy
            else
               search t
       | [] ->
            raise Not_found
      in
         search (get_theories ())
   
   (*
    * Get the refiner.
    *)
   let refiner { pack_name = name } =
      (get_theory name).thy_refiner

   (*
    * Get the list of display forms.
    *)
   let dforms { pack_name = name } =
      (get_theory name).thy_dformer
   
   (*
    * Get the name of the package.
    *)
   let name { pack_name = name } =
      name
   
   (*
    * Get the filename for the package.
    *)
   let filename pack = function
      { pack_info = Some info } ->
         StrFilterCache.filename pack.pack_cache info
    | { pack_info = None } ->
         raise (Failure "Package_info.filename: package is not implemented")
   
   (*
    * Get the status of the package.
    *)
   let status { pack_status = status } =
      status
   
   (*
    * Set the status of the package.
    *)
   let set_status pack status =
      pack.pack_status <- status

   (*
    * Get the items in the module.
    *)
   let info = function
      { pack_info = Some info } ->
         StrFilterCache.info info
    | { pack_info = None } ->
         raise (Failure "Package_info.info: package is not implemented")
   
   let sig_info = function
      { pack_sig = Some info } ->
         info
    | { pack_sig = None; pack_info = Some info } as pack ->
         let info = StrFilterCache.sig_info info InterfaceType in
            pack.pack_sig <- Some info;
            info
    | { pack_sig = None; pack_info = None } ->
         raise (Failure "Package_info.sig_info: package not implemented")
   
   (*
    * DAG access.
    *)
   let get_node { pack_dag = dag; pack_packages = packages } info =
      let rec search = function
         node :: t ->
            let info' = ImpDag.node_value dag node in
               if info' == info then
                  node
               else
                  search t
       | [] ->
            raise Not_found
      in
         search packages

   let roots { pack_dag = dag } =
      List.map (ImpDag.node_value dag) (ImpDag.roots dag)

   let parents pack info =
      let { pack_dag = dag } = pack in
      let node = get_node pack info in
         List.map (ImpDag.node_value dag) (ImpDag.node_out_edges dag node)
   
   let children pack info =
      let { pack_dag = dag } = pack in
      let node = get_node pack info in
         List.map (ImpDag.node_value dag) (ImpDag.node_in_edges dag node)

   (*
    * Get a node by its name.
    *)
   let load_check dag name node =
      let { pack_name = name' } = ImpDag.node_value dag node in
         name' = name

   let is_loaded { pack_dag = dag; pack_packages = packages } name =
      List.exists (load_check dag name) packages

   let get_package { pack_dag = dag; pack_packages = packages } name =
      List_util.find packages (load_check dag name)
   
   (*
    * Add a parent edge.
    * We only allow parents with toplevel names.
    *)
   let insert_parent pack node = function
      [parent] ->
         begin
            try
               let pnode = get_package pack parent in
                  ImpDag.add_edge pack.pack_dag node pnode
            with
               Not_found ->
                  raise (Failure "Package_info.maybe_add_package: parent is not defined")
         end
    | path ->
         raise (Failure ("Package_info.insert_parent: parent is not toplevel: " ^ string_of_path path))

   (*
    * Add an implementation package.
    * This replaces any current version of the package,
    * and adds the edges to the parents.
    *)
   let add_implementation pack info =
      let { pack_dag = dag; pack_packages = packages } = pack in
      let { pack_name = name; pack_info = info' } = info in
      let rec remove = function
         node :: t ->
            let { pack_name = name' } = ImpDag.node_value dag node in
               if name' = name then
                  begin
                     ImpDag.delete dag node;
                     t
                  end
               else
                  node :: remove t
       | [] ->
            []
      in
      let node = ImpDag.insert dag info in
      let parents =
         match info' with
            Some info ->
               StrFilterCache.parents info
          | None ->
               raise (Invalid_argument "Package_info.add_implementation")
      in
         pack.pack_packages <- node :: (remove packages);
         List.iter (insert_parent pack node) parents
   
   (*
    * Add a signature package.
    * This does nothing if the package already exists,
    * otherwise it adds the package, and creates
    * the edges to the parents.
    *)
   let maybe_add_package pack path sig_info =
      match path with
         [name] ->
            if not (is_loaded pack name) then
               let { pack_dag = dag; pack_packages = packages } = pack in
               let parents = Filter_summary.parents sig_info in
               let pinfo =
                  { pack_status = ReadOnly;
                    pack_sig = Some sig_info;
                    pack_info = None;
                    pack_name = name
                  }
               in
               let node = ImpDag.insert dag pinfo in
                  pack.pack_packages <- node :: packages;
                  List.iter (insert_parent pack node) parents
       | _ ->
            raise (Failure "Package_info.maybe_add_package: nested modules are not implemented")

   (*
    * When a module is inlined, add the resources and infixes.
    * The info is the _signature_.  Later we may want to replace
    * it with the implementation.
    *)
   let inline_hook pack root_path cache (path, info) (paths, resources) =
      (* Include all the resources *)
      if debug_resource then
         eprintf "Inline_hook: %s, %s%t" (string_of_path root_path) (string_of_path path) eflush;
      let add_resource rsrc =
         if debug_resource then
            eprintf "Adding resource: %s.%s%t" (string_of_path path) rsrc.resource_name eflush;
         StrFilterCache.add_resource cache path rsrc
      in
      let nresources, nresources' =
         (*
          * nresources: all the resources
          * nresources': just the new ones
          *)
         let rec collect nresources nresources' = function
            rsrc::tl ->
               if mem_resource rsrc nresources then
                  collect nresources nresources' tl
               else
                  collect (rsrc :: nresources) (rsrc :: nresources') tl
          | [] ->
               nresources, nresources'
         in
            collect resources [] (get_resources info)
      in
         List.iter add_resource nresources';
         
         (* Add all the infix words *)
         List.iter add_infix (get_infixes info);
         
         (* Add this node to the pack *)
         maybe_add_package pack path info;
         
         (* Add the path to the list of parents *)
         path :: paths, nresources

   (*
    * Get a loaded theory.
    *)
   let get pack name =
      let { pack_dag = dag } = pack in
         ImpDag.node_value dag (get_package pack name)

   (*
    * Save a package.
    * This happens only if it is modified.
    *)
   let save pack = function
      { pack_status = ReadOnly } ->
         raise (Failure "Package is read-only")
    | { pack_status = Unmodified } ->
         ()
    | { pack_status = Modified; pack_info = Some info } ->
         StrFilterCache.save info
    | { pack_status = Modified; pack_info = None } ->
         raise (Invalid_argument "Package_info.save")

   (*
    * Create an empty package.
    *)
   let create_package pack name =
      let info =
         { pack_status = Modified;
           pack_sig = None;
           pack_info = Some (StrFilterCache.create_cache pack.pack_cache (**)
                                name ImplementationType InterfaceType);
           pack_name = name
         }
      in
         add_implementation pack info;
         info
   
   (*
    * Load a package.
    * We search for the description, and load it.
    * If the ML file has already been loaded, retrieve
    * the refiner and display forms.  Else construct the code
    * to be evaluated, and return it.
    *)
   let load pack name =
      try
         let loc = 0, 0 in
         let { pack_cache = cache } = pack in
         let path = [name] in
         let info, _ =
            StrFilterCache.load cache name ImplementationType InterfaceType (inline_hook pack path) ([], [])
         in
         let mlexists =
            let filename = StrFilterCache.filename cache info in
            let filename = (Filename_util.root filename) ^ ".ml" in
               Sys.file_exists filename
         in
         let info' =
            { pack_status = if mlexists then ReadOnly else Unmodified;
              pack_sig = None;
              pack_info = Some info;
              pack_name = name
            }
         in
            add_implementation pack info';
            if is_theory_loaded name then
               let unit = <:expr< () >> in
                  (<:str_item< $exp: unit$ >>)
            else
               (* Wrap the theory up in a module and evaluate it *)
               let items = extract_str (StrFilterCache.info info) (StrFilterCache.resources info) name in
               let mn = String.capitalize name in
               let me = (<:module_expr< struct $list:List.map fst items$ end >>) in
                  (<:str_item< module $mn$ = $me$ >>)
      with
         Sys_error _ ->
             raise (Failure ("Package_info.load: " ^ name ^ " not found"))
end

(*
 * $Log$
 * Revision 1.5  1998/04/17 01:30:41  jyh
 * Editor is almost constructed.
 *
 * Revision 1.4  1998/04/16 14:55:44  jyh
 * Upgrading packages.
 *
 * Revision 1.3  1998/04/15 22:28:49  jyh
 * Converting packages from summaries.
 *
 * Revision 1.2  1998/04/15 12:39:32  jyh
 * Updating editor packages to Filter_summarys.
 *
 * Revision 1.1  1997/08/06 16:17:16  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.2  1996/10/23 15:17:50  jyh
 * First working version of dT tactic.
 *
 * Revision 1.1  1996/09/02 19:33:23  jyh
 * Semi-working package management.
 *
 * Revision 1.2  1996/05/21 02:25:29  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.1  1996/05/20 16:59:55  jyh
 * This is an intermediate form of the editor with modules
 * before debugging.  Will be removing theoryGraph files next.
 *
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
