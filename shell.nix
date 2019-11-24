{ pkgs ? import <nixpkgs> {} }:

let
  sources = import ./nix/sources.nix;
  cl-org-mode = sources.cl-org-mode;
  version = "someversion";
  quicklisp = builtins.fetchurl "https://beta.quicklisp.org/quicklisp.lisp";
  autoload = ''
  #-quicklisp
  (let ((quicklisp-init ".quicklisp/setup.lisp"))
    (when (probe-file quicklisp-init)
      (load quicklisp-init)))



  (ql:register-local-projects)
  (ql:quickload :orgmark)
  '';

in with pkgs; mkShell {
  name = "foo";
  buildInputs = [ sbcl ];
  shellHook = ''

    ln -sf $PWD .quicklisp/local-projects/
    ln -sf ${cl-org-mode} .quicklisp/local-projects/

    if ! [ -d ".quicklisp" ]; then
      touch .sbclrc
      sbcl --load ${quicklisp} \
           --eval "(quicklisp-quickstart:install :path \"$PWD/.quicklisp/\")" \
           --quit
    fi

    echo '${autoload}' > .sbclrc
    alias sbcl='\sbcl --userinit .sbclrc'
  '';
}
