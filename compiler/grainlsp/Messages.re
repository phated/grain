open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_typed;
open Grain_diagnostics;

let getSigFromStmt = (stmt: Grain_typed__Typedtree.toplevel_stmt) =>
  switch (stmt.ttop_desc) {
  | TTopImport(import_declaration) => "import declaration"
  | TTopForeign(export_flag, value_description) => "foreign"
  | TTopData(data_declarations) => "data declaration"
  | TTopLet(export_flag, rec_flag, mut_flag, value_bindings) =>
    if (List.length(value_bindings) > 0) {
      let vbs: Typedtree.value_binding = List.hd(value_bindings);
      Utils.lens_sig(vbs.vb_expr.exp_type);
    } else {
      "";
    }

  | TTopExpr(expression) =>
    let expr_type = expression.exp_type;
    Utils.lens_sig(expr_type);
  | TTopException(export_flag, type_exception) => "exception"
  | TTopExport(export_declarations) => "export"
  };

let getLenses = (typed_program: Typedtree.typed_program) => {
  let stmts = typed_program.statements;

  let lenses = ref([]);

  module Iterator =
    TypedtreeIter.MakeIterator({
      include TypedtreeIter.DefaultIteratorArgument;
    });
  List.iter(
    (stmt: Grain_typed__Typedtree.toplevel_stmt) => {
      let (file, startline, startchar, sbol) =
        Utils.get_raw_pos_info(stmt.ttop_loc.loc_start);

      let signature = getSigFromStmt(stmt);

      let lens: Rpc.lens_t = {line: startline, signature};

      lenses := List.append(lenses^, [lens]);
      Iterator.iter_toplevel_stmt(stmt);
    },
    stmts,
  );
  lenses^;
};

let processGetLenses = (log, id, json, compiledCode) => {
  let params = Yojson.Safe.Util.member("params", json);

  let textDocument = Yojson.Safe.Util.member("textDocument", params);

  let uri =
    Yojson.Safe.Util.member("uri", textDocument)
    |> Yojson.Safe.Util.to_string_option;
  switch (uri) {
  | None => Rpc.sendLenses(log, stdout, id, [])
  | Some(u) =>
    if (Hashtbl.mem(compiledCode, u)) {
      let compiledCode = Hashtbl.find(compiledCode, u);
      let lenses = getLenses(compiledCode);
      Rpc.sendLenses(log, stdout, id, lenses);
    } else {
      Rpc.sendLenses(log, stdout, id, []);
    }
  };
};

let getHover = (log, id, json, compiledCode) => {
  switch (Utils.getTextDocumenUriAndPosition(json)) {
  | (Some(uri), Some(line), Some(char)) =>
    if (Hashtbl.mem(compiledCode, uri)) {
      let ln = line + 1;

      let compiledCode = Hashtbl.find(compiledCode, uri);
      let node = Utils.findBestMatch(compiledCode, ln, char);
      switch (node) {
      | Some(stmt) =>
        let (signature, loc) = Utils.getHoverFromStmt(log, stmt, ln, char);

        let range =
          switch (loc) {
          | None => Utils.loc_to_range(stmt.ttop_loc)
          | Some(l) => Utils.loc_to_range(l)
          };
        Rpc.sendHover(log, stdout, id, signature, range);
        ();
      | None => ()
      };
    } else {
      ();
    }
  | _ => ()
  };
};

let gotoDefinition = (log, id, json, compiledCode) => {
  log("@@@@  go to definition requested");
  switch (Utils.getTextDocumenUriAndPosition(json)) {
  | (Some(uri), Some(line), Some(char)) =>
    let ln = line + 1;

    let compiledCode = Hashtbl.find(compiledCode, uri);
    let node = Utils.findBestMatch(compiledCode, ln, char);
    switch (node) {
    | Some(stmt) =>
      let node = Utils.getNodeFromStmt(log, stmt, ln, char);
      switch (node) {
      | NoNode(err) => log(err)
      | Expression(e) =>
        let desc = e.exp_desc;
        switch (desc) {
        | TExpIdent(path, _, _) =>
          let lookup = Env.find_value(path, compiledCode.env);

          let loc = lookup.val_loc;
          let (_, startline, startchar, _) =
            Utils.get_raw_pos_info(loc.loc_start);

          let txt = Utils.print_path(path);
          log("!!!!!Expr defn for " ++ txt);
          log(
            "!!!!!start "
            ++ string_of_int(startline)
            ++ string_of_int(startchar),
          );

          let range = Utils.loc_to_range(loc);

          Rpc.sendGoToDefinition(log, stdout, id, uri, range);

        | _ => log("No defn")
        };
      | Pattern(p) => log("!!!!!Pattern defn")
      };
      ();
    | _ => log("!!!! no defn match found")
    };
  | _ => log("!!!! params missing")
  };
};

let compile_typed = (log, filename) => {
  Grain_utils.Config.base_path := "/Users/marcus/Projects/grain";
  Grain_utils.Config.stdlib_dir := Some("/Users/marcus/Projects/grain/stdlib");

  switch (Compile.compile_file(~hook=stop_after_typed, filename)) {
  | exception exn =>
    log("Exception");
    let bt =
      if (Printexc.backtrace_status()) {
        Some(Printexc.get_backtrace());
      } else {
        None;
      };
    Grain_parsing.Location.report_exception(Format.err_formatter, exn);
    Option.iter(
      s =>
        if (Grain_utils.Config.debug^) {
          prerr_string("Backtrace:\n");
          prerr_string(s);
          prerr_string("\n");
          log(s);
          log("\n");
        },
      bt,
    );
    None;
  | {cstate_desc: TypeChecked(typed_program)} => Some(typed_program)
  | _ =>
    log("Invalid compilation state");
    None; // failwith("Invalid compilation state")
  };
};

let signatureHelp =
    (
      log,
      id,
      json,
      compiledCode:
        Stdlib__hashtbl.t(string, Grain_typed__Typedtree.typed_program),
      documents,
    ) => {
  log("@@@@!!!@@@@@!!!@@  signature help requested");
  switch (Utils.getTextDocumenUriAndPosition(json)) {
  | (Some(uri), Some(line), Some(char)) =>
    let ln = line + 1;

    let completable =
      Completion.getOriginalText(log, documents, uri, line, char - 1);

    switch (completable) {
    | Nothing => log("no completable code found")
    | Lident(text) =>
      log("looking for code for " ++ uri);
      // if (!Hashtbl.mem(compiledCode, uri)) {
      //   log("Can't find compiled code for " ++ uri);
      // } else {
      //   let compiledCode = Hashtbl.find(compiledCode, uri);

      //   let sige = compiledCode.signature;
      //   let signature_items = compiledCode.signature.cmi_sign;

      //   let _ =
      //     List.map(
      //       (si: Types.signature_item) => {
      //         switch (si) {
      //         | TSigType(ident, decl, recv) => log("TSigType " ++ ident.name)
      //         | TSigValue(ident, desc) => log("TSigValue " ++ ident.name)
      //         | TSigTypeExt(_) => log("TSigTypeExt")
      //         | TSigModule(_) => log("TSigModule")
      //         | TSigModType(_) => log("TSigModType")
      //         }
      //       },
      //       signature_items,
      //     );

      //   // let values: list((Ident.t, Types.value_description)) =
      //   //   Env.get_all_values(log, compiledCode.env);

      //   // log("we have " ++ string_of_int(List.length(values)) ++ " values");

      //   // let matches =
      //   //   List.filter_map(
      //   //     ((i: Ident.t, l: Types.value_description)) => {
      //   //       let vk = l.val_kind;

      //   //       if (i.name == text) {
      //   //         log("!!!matching on " ++ i.name);
      //   //         // switch (vk) {
      //   //         // | TValReg => log("TValReg")
      //   //         // | TValPrim(_) => log("TValPrim")
      //   //         // | TValUnbound(_) => log("TValUnbound")
      //   //         // | TValConstructor(_) => log("TValConstructor")
      //   //         // };

      //   //         log("full path " ++ Utils.print_path(l.val_fullpath));

      //   //         let tt = Env.find_type(l.val_fullpath, compiledCode.env);

      //   //         log(
      //   //           "Type has "
      //   //           ++ string_of_int(List.length(tt.type_params))
      //   //           ++ " params",
      //   //         );

      //   //         Some(Utils.lens_sig(l.val_type));
      //   //       } else {
      //   //         None;
      //   //       };
      //   //     },
      //   //     values,
      //   //   );

      //   let matches = [];
      //   Rpc.sendSignature(log, stdout, id, matches);
      // };

      log("lets compile the module");
      let cpOpt =
        compile_typed(log, "/Users/marcus/Projects/grain/stdlib/result.gr");

      switch (cpOpt) {
      | None => log("Couldn't compile module")
      | Some(program) =>
        log("Ready to generate docs");

        Comments.setup_comments(program.comments);

        let env = program.env;
        let signature_items = program.signature.cmi_sign;

        //   let signature_items = compiledCode.signature.cmi_sign;

        let _ =
          List.map(
            (si: Types.signature_item) => {
              switch (si) {
              | TSigType(ident, decl, recv) => log("TSigType " ++ ident.name)
              | TSigValue(ident, desc) =>
                log("TSigValue " ++ ident.name);
                if (ident.name == "expect") {
                  let docblock = Docblock.for_signature_item(~env, si);
                  switch (docblock) {
                  | Some(docblock) =>
                    log("Found a matching docblock");

                    //let sig =

                    let sign =
                      docblock.module_name
                      ++ ":"
                      ++ docblock.name
                      ++ " = "
                      ++ docblock.type_sig;

                    let pms =
                      List.fold_left(
                        (acc, a) =>
                          switch ((a: Grain_diagnostics.Comments.Attribute.t)) {
                          | Param(p) =>
                            acc ++ p.attr_name ++ " " ++ p.attr_desc ++ " "
                          | _ => acc
                          },
                        "",
                        docblock.attributes,
                      );

                    Rpc.sendSignature(log, stdout, id, [pms]);

                  | None => log("Found no matching doc block")
                  };
                } else {
                  ();
                };
              | TSigTypeExt(_) => log("TSigTypeExt")
              | TSigModule(_) => log("TSigModule")
              | TSigModType(_) => log("TSigModType")
              }
            },
            signature_items,
          );

        ();
      };

      ();
    };
  | _ => log("!!!! params missing")
  };
};
