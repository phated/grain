open Grain;
open Compile;
open Grain_parsing;
open Grain_utils;
open Grain_typed;
open Grain_diagnostics;
open Sourcetree;
open Lsp_types;

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#hoverParams
module RequestParams = {
  [@deriving yojson({strict: false})]
  type t = {
    [@key "textDocument"]
    text_document: Protocol.text_document_identifier,
    position: Protocol.position,
  };
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#hover
module ResponseResult = {
  [@deriving yojson]
  type markup_content = {
    kind: string,
    value: string,
  };

  [@deriving yojson]
  type t = {
    contents: markup_content,
    range: Protocol.range,
  };
};

let loc_to_range = (pos: Location.t): Protocol.range => {
  let (_, startline, startchar, _) =
    Locations.get_raw_pos_info(pos.loc_start);
  let (_, endline, endchar) =
    Grain_parsing.Location.get_pos_info(pos.loc_end);

  {
    range_start: {
      line: startline - 1,
      character: startchar,
    },
    range_end: {
      line: endline - 1,
      character: endchar,
    },
  };
};

// We need to use the "grain-type" markdown syntax to have correct coloring on hover items
let grain_type_code_block = Markdown.code_block(~syntax="grain-type");
// Used for module hovers
let grain_code_block = Markdown.code_block(~syntax="grain");

let send_hover = (~id: Protocol.message_id, ~range: Protocol.range, signature) => {
  Protocol.response(
    ~id,
    ResponseResult.to_yojson({
      contents: {
        kind: "markdown",
        value: signature,
      },
      range,
    }),
  );
};

let send_no_result = (~id: Protocol.message_id) => {
  Protocol.response(~id, `Null);
};

let module_lens = (~program: Typedtree.typed_program, p: Path.t) => {
  let vals = Modules.get_exports(p, program);
  let signatures =
    List.map(
      (v: Modules.export) =>
        switch (v.kind) {
        | Function
        | Value => Format.sprintf("let %s", v.signature)
        | Record
        | Enum
        | Abstract
        | Exception => v.signature
        },
      vals,
    );
  grain_code_block(String.concat("\n", signatures));
};

let value_lens = (desc: Types.value_description) => {
  grain_type_code_block(Printtyp.string_of_type_scheme(desc.val_type));
};

let expression_lens = (e: Typedtree.expression) => {
  switch (e.exp_desc) {
  | TExpRecord(fields) =>
    let (path, _, decl) =
      Ctype.extract_concrete_typedecl(e.exp_env, e.exp_type);
    grain_code_block(
      Printtyp.string_of_type_declaration(~ident=Path.head(path), decl),
    );
  | _ => grain_type_code_block(Printtyp.string_of_type_scheme(e.exp_type))
  };
};

let pattern_lens = (p: Typedtree.pattern) => {
  grain_type_code_block(Printtyp.string_of_type_scheme(p.pat_type));
};

let type_lens = (ty: Typedtree.core_type) => {
  grain_type_code_block(Printtyp.string_of_type_scheme(ty.ctyp_type));
};

let declaration_lens = (decl: Typedtree.data_declaration) => {
  grain_type_code_block(
    Printtyp.string_of_type_declaration(~ident=decl.data_id, decl.data_type),
  );
};

let process =
    (
      ~id: Protocol.message_id,
      ~compiled_code: Hashtbl.t(Protocol.uri, code),
      ~documents: Hashtbl.t(Protocol.uri, string),
      params: RequestParams.t,
    ) => {
  switch (Hashtbl.find_opt(compiled_code, params.text_document.uri)) {
  | None => ()
  | Some({program, sourcetree}) =>
    let results = Sourcetree.query(params.position, sourcetree);
    switch (results) {
    | [Value(loc, desc), ..._] =>
      send_hover(~id, ~range=loc_to_range(loc), value_lens(desc))
    | [Expression(exp), ..._] =>
      send_hover(
        ~id,
        ~range=loc_to_range(exp.exp_loc),
        expression_lens(exp),
      )
    | [Pattern(pat), ..._] =>
      send_hover(~id, ~range=loc_to_range(pat.pat_loc), pattern_lens(pat))
    | [Type(ty), ..._] =>
      send_hover(~id, ~range=loc_to_range(ty.ctyp_loc), type_lens(ty))
    | [Declaration(decl), ..._] =>
      send_hover(
        ~id,
        ~range=loc_to_range(decl.data_loc),
        declaration_lens(decl),
      )
    | [Module(path, loc), ..._] =>
      send_hover(~id, ~range=loc_to_range(loc), module_lens(~program, path))
    | _ => send_no_result(~id)
    };
  };
};