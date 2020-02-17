let
  pkgs = import <nixpkgs> {};
  source = pkgs.lib.sourceByRegex ./. [
    "^ace-popup-menu\.el$"
  ];
in
pkgs.stdenv.mkDerivation {
  name = "ace-popup-menu";
  src = source;
  buildInputs = [
    (pkgs.emacs26WithPackages (epkgs: [epkgs.avy-menu]))
  ];
  buildPhase = ''
    emacs -L . --batch -f batch-byte-compile *.el
  '';
  installPhase = ''
    LISPDIR=$out/share/emacs/site-lisp
    install -d $LISPDIR
    install *.el *.elc $LISPDIR
  '';
  # checkPhase = ''
  #   emacs -L . --batch --eval "(progn (require 'ert-runner) (ert-run-tests t (lambda (x) nil)))"
  # '';
  # doCheck = true;
}
