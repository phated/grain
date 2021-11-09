open Grain_typed;

let getTextDocumenUriAndPosition = json => {
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

  (uri, line, char);
};

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
  // Rpc.sendRefreshLenses(log, stdout);
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
  switch (getTextDocumenUriAndPosition(json)) {
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
  switch (getTextDocumenUriAndPosition(json)) {
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

let signatureHelp = (log, id, json, compiledCode) => {
  log("@@@@!!!@@@@@!!!@@  signature help requested");
  switch (getTextDocumenUriAndPosition(json)) {
  | (Some(uri), Some(line), Some(char)) =>
    let ln = line + 1;

    log("looking for code for " ++ uri);
    if (!Hashtbl.mem(compiledCode, uri)) {
      log("Can't find compiled code for " ++ uri);
    } else {
      let compiledCode = Hashtbl.find(compiledCode, uri);

      let fnsigs = [];
      Rpc.sendSignature(log, stdout, id, fnsigs);
    };
  | _ => log("!!!! params missing")
  };
};
