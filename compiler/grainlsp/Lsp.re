let initialize = () => {
  let rootPath = ".";
  Files.mkdirp(rootPath ++ "/lsp");
  Log.setLocation(rootPath ++ "/lsp/debug.log");
  Log.log("Hello - from " ++ Sys.executable_name);

  let _ = Mainserver.run(Log.log);
  ();
};