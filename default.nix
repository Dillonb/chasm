with import <nixpkgs> {};
stdenv.mkDerivation {
    name = "chasm-dev";
    buildInputs = [ ocaml capstone ];
}