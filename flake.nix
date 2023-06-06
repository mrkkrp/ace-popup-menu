{
  inputs = {
    emacs-package-flake.url = "github:mrkkrp/emacs-package-flake";
  };
  outputs = { self, emacs-package-flake }:
    emacs-package-flake.lib.mkOutputs {
      name = "ace-popup-menu";
      srcDir = ./.;
      deps = ["avy-menu"];
    };
}
