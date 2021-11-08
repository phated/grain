open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_typed;

type completable =
  | Nothing
  | Lident(string);

let findCompletable = (text, offset) => {
  let rec loop = i => {
    i < 0
      ? Lident(String.sub(text, i + 1, offset - (i + 1)))
      : (
        switch (text.[i]) {
        | 'a' .. 'z'
        | 'A' .. 'Z'
        | '0' .. '9'
        | '.'
        | '_' => loop(i - 1)
        | _ =>
          i == offset - 1
            ? Nothing : Lident(String.sub(text, i + 1, offset - (i + 1)))
        }
      );
  };
  loop(offset - 1);
};

let processCompletion =
    (
      log,
      id,
      json,
      compiledCode: Stdlib__hashtbl.t(string, Typedtree.typed_program),
      documents,
    ) => {
  log("@@@@!!!@@@@@!!!@@  completion  requested");
  log("\n\n\n\n\n\n");
  let params = Yojson.Safe.Util.member("params", json);

  let textDocument = Yojson.Safe.Util.member("textDocument", params);
  let position = Yojson.Safe.Util.member("position", params);

  let uri =
    Yojson.Safe.Util.member("uri", textDocument)
    |> Yojson.Safe.Util.to_string_option;

  let line =
    Yojson.Safe.Util.member("line", position)
    |> Yojson.Safe.Util.to_int_option;

  let char =
    Yojson.Safe.Util.member("character", position)
    |> Yojson.Safe.Util.to_int_option;

  let context = Yojson.Safe.Util.member("context", json);

  switch (uri) {
  | Some(u) => log("URI is " ++ u)
  | None => log("uri not found")
  };

  switch (uri, line, char) {
  | (Some(u), Some(l), Some(c)) =>
    let ln = l + 1;

    log("completing on line " ++ string_of_int(l));
    log("completing at char " ++ string_of_int(c));

    let completable =
      if (!Hashtbl.mem(documents, u)) {
        log("Can't find source code for " ++ u);
        Nothing;
      } else {
        log("looking up line");
        let sourceCode = Hashtbl.find(documents, u);
        // try and find the code we are completing in the original source

        let lines = String.split_on_char('\n', sourceCode);

        log(
          "we have " ++ string_of_int(List.length(lines)) ++ " line of code",
        );

        let line = List.nth(lines, l);

        log("\n\n\n@@@@@ editing on line " ++ line);

        //UNICODE??  FIX ME

        let completable = findCompletable(line, c);

        let _ =
          switch (completable) {
          | Nothing => log("nothing completable found")
          | Lident(ident) => log("Lets complete on " ++ ident)
          };
        completable;
      };

    switch (completable) {
    | Nothing => log("Nothing to complete")
    | Lident(text) =>
      log("Completing " ++ text);
      log("looking for code for " ++ u);
      if (!Hashtbl.mem(compiledCode, u)) {
        log("Can't find compiled code for " ++ u);
      } else {
        let compiledCode = Hashtbl.find(compiledCode, u);

        let firstChar = text.[0];

        let completions =
          switch (firstChar) {
          | 'A' .. 'Z' =>
            if (String.contains(text, '.')) {
              // find module name

              let pos = String.rindex(text, '.');
              let modName = String.sub(text, 0, pos);
              let ident: Ident.t = {name: modName, stamp: 0, flags: 0};
              //let mod_ident: Path.t = PExternal(PIdent(ident), "", 0);
              let mod_ident: Path.t = PIdent(ident);
              let lookup: Types.module_declaration =
                Env.find_module(mod_ident, None, compiledCode.env);
              switch (lookup.md_filepath) {
              | None =>
                log("no module path found");
                [];
              | Some(p) =>
                log("found module at " ++ p);
                let mtype: Grain_typed.Types.module_type = lookup.md_type;
                switch (mtype) {
                | TModSignature(sigs) =>
                  let fnsigs =
                    List.filter_map(
                      (s: Types.signature_item) => {
                        switch (s) {
                        | TSigValue(ident, vd) =>
                          let string_of_value_description =
                              (~ident: Ident.t, vd) => {
                            Format.asprintf(
                              "%a",
                              Printtyp.value_description(ident),
                              vd,
                            );
                          };
                          //  log(string_of_value_description(~ident, vd));
                          let item: Rpc.completionItem = {
                            label: ident.name,
                            kind: 3,
                            detail: string_of_value_description(~ident, vd),
                          };
                          Some(item);

                        | TSigType(ident, td, recstatus) =>
                          log("IS A TSigType");
                          let string_of_type_declaration =
                              (~ident: Ident.t, td) => {
                            (
                              ident.name,
                              Format.asprintf(
                                "%a",
                                Printtyp.type_declaration(ident),
                                td,
                              ),
                            );
                          };
                          // log(string_of_type_declaration(~ident, td));
                          None;
                        | _ =>
                          log("Not a TSigType");
                          None;
                        }
                      },
                      sigs,
                    );
                  fnsigs;
                //Rpc.sendCompletion(log, stdout, id, fnsigs);
                | _ =>
                  log("not a TModSignature");
                  [];
                };
              };
            } else {
              let modules = Env.get_all_modules(compiledCode.env);

              let modules =
                List.map(
                  (m: Ident.t) => {
                    let item: Rpc.completionItem = {
                      label: m.name,
                      kind: 9,
                      detail: "",
                    };
                    item;
                  },
                  modules,
                );

              let values: list((Ident.t, Types.value_description)) =
                Env.get_all_values(log, compiledCode.env);

              let upperVals =
                List.filter_map(
                  ((i: Ident.t, l: Types.value_description)) => {
                    switch (i.name.[0]) {
                    | 'A' .. 'Z' =>
                      let item: Rpc.completionItem = {
                        label: i.name,
                        kind: 12,
                        detail: Utils.lens_sig(l.val_type),
                      };
                      Some(item);
                    | _ => None
                    }
                  },
                  values,
                );

              modules @ upperVals;
            }

          | _ =>
            let values: list((Ident.t, Types.value_description)) =
              Env.get_all_values(log, compiledCode.env);

            log(
              "we have " ++ string_of_int(List.length(values)) ++ " values",
            );

            List.map(
              ((i: Ident.t, l: Types.value_description)) => {
                let item: Rpc.completionItem = {
                  label: i.name,
                  kind: 12,
                  detail: Utils.lens_sig(l.val_type),
                };
                item;
              },
              values,
            );
          };

        Rpc.sendCompletion(log, stdout, id, completions);
      };
    };

  | _ => ()
  };
};
