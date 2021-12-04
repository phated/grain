let initialize = () => {
  let rootPath = ".";
  Files.mkdirp(rootPath ++ "/lsp");
  Log.set_location(rootPath ++ "/lsp/debug.log");
  Log.log("Hello - from " ++ Sys.executable_name);

  let _ = Mainserver.run(Log.log);
  ();
};
