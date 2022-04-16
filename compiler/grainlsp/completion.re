open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_typed;

//  CompletionItemKind
let item_kind_completion_text = 1;
let item_kind_completion_function = 3;
let item_kind_completion_constructor = 4;
let item_kind_completion_variable = 6;
let item_kind_completion_struct = 22;

// maps Grain types to LSP CompletionItemKind
let rec get_kind = (desc: Types.type_desc) =>
  switch (desc) {
  | TTyVar(_) => item_kind_completion_variable
  | TTyArrow(_) => item_kind_completion_function
  | TTyTuple(_) => item_kind_completion_struct
  | TTyRecord(_) => item_kind_completion_struct
  | TTyConstr(_) => item_kind_completion_constructor
  | TTySubst(s) => get_kind(s.desc)
  | TTyLink(t) => get_kind(t.desc)
  | _ => item_kind_completion_text
  };

let get_module_exports = (mod_ident, compiled_code: Typedtree.typed_program) => {
  switch (Env.find_module(mod_ident, None, compiled_code.env)) {
  | lookup =>
    switch (lookup.md_filepath) {
    | None => []
    | Some(p) =>
      let mtype: Grain_typed.Types.module_type = lookup.md_type;
      switch (mtype) {
      | TModSignature(sigs) =>
        let fnsigs =
          List.filter_map(
            (s: Types.signature_item) => {
              switch (s) {
              | TSigValue(ident, vd) =>
                let string_of_value_description = (~ident: Ident.t, vd) => {
                  Format.asprintf(
                    "%a",
                    Printtyp.value_description(ident),
                    vd,
                  );
                };
                let item: Rpc.completion_item = {
                  label: ident.name,
                  kind: 3,
                  detail: string_of_value_description(~ident, vd),
                  documentation: "",
                };
                Some(item);

              | TSigType(ident, td, recstatus) =>
                let string_of_type_declaration = (~ident: Ident.t, td) => {
                  Format.asprintf(
                    "%a",
                    Printtyp.type_declaration(ident),
                    td,
                  );
                };
                let item: Rpc.completion_item = {
                  label: ident.name,
                  kind: item_kind_completion_struct,
                  detail: string_of_type_declaration(~ident, td),
                  documentation: "",
                };
                Some(item);
              | _ => None
              }
            },
            sigs,
          );
        fnsigs;
      | _ => []
      };
    }
  | exception _ => []
  };
};

let process_resolution =
    (
      ~id,
      ~compiled_code: Hashtbl.t(string, Typedtree.typed_program),
      ~cached_code: Hashtbl.t(string, Typedtree.typed_program),
      ~documents,
      request,
    ) => {
  // right now we just resolve nothing to clear the client's request
  // In future we may want to send more details back with Graindoc details for example
  Rpc.send_completion(
    ~output=stdout,
    ~id,
    [],
  );
};

let process_completion =
    (
      ~id,
      ~compiled_code: Hashtbl.t(string, Typedtree.typed_program),
      ~cached_code: Hashtbl.t(string, Typedtree.typed_program),
      ~documents,
      request,
    ) => {
  switch (Utils.get_text_document_uri_and_position(request)) {
  | Some(location) =>
    let completable =
      Utils.get_original_text(
        documents,
        location.uri,
        location.line,
        location.char,
      );
    switch (completable) {
    | Nothing => Rpc.send_completion(stdout, id, [])
    | Lident(text) =>
      switch (Hashtbl.find_opt(cached_code, location.uri)) {
      | None => Rpc.send_completion(stdout, id, [])
      | Some(compiled_code) =>
        let modules = Env.get_all_modules(compiled_code.env);
        let first_char = text.[0];

        let completions =
          switch (first_char) {
          | 'A' .. 'Z' =>
            // autocomplete modules
            switch (String.rindex(text, '.')) {
            | exception exn =>
              let modules = Env.get_all_modules(compiled_code.env);
              let types = Env.get_all_type_names(compiled_code.env);

              let converted_modules =
                List.map(
                  (m: Ident.t) => {
                    let item: Rpc.completion_item = {
                      label: m.name,
                      kind: 9,
                      detail: "",
                      documentation: "",
                    };
                    item;
                  },
                  modules,
                );

              let converted_types =
                List.map(
                  (t: Ident.t) => {
                    let item: Rpc.completion_item = {
                      label: t.name,
                      kind: 22,
                      detail: "",
                      documentation: "",
                    };
                    item;
                  },
                  types,
                );

              converted_modules @ converted_types;
            | pos =>
              // find module name

              let mod_name = String.sub(text, 0, pos);
              let ident: Ident.t = {name: mod_name, stamp: 0, flags: 0};
              let mod_ident: Path.t = PIdent(ident);

              // only look up completions for imported modules
              if (!List.exists((m: Ident.t) => m.name == mod_name, modules)) {
                [];
              } else {
                get_module_exports(mod_ident, compiled_code);
              };
            }

          | _ =>
            // Autocompete anything in scope
            let values: list((Ident.t, Types.value_description)) =
              Env.get_all_values(compiled_code.env);

            List.map(
              ((i: Ident.t, l: Types.value_description)) => {
                let item: Rpc.completion_item = {
                  label: i.name,
                  kind: get_kind(l.val_type.desc),
                  detail: Utils.lens_sig(l.val_type, ~env=compiled_code.env),
                  documentation: "",
                };
                item;
              },
              values,
            );
          };

        Rpc.send_completion(stdout, id, completions);
      }
    };

  | _ => Rpc.send_completion(stdout, id, [])
  };
};
