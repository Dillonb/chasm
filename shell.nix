{ pkgs ? import <nixpkgs> {} }:
let
  buildInputs = [
    pkgs.ocaml
    pkgs.opam
    pkgs.capstone
    pkgs.glibc
  ];
in

pkgs.mkShell {
  buildInputs = buildInputs;
  CPATH = pkgs.lib.makeSearchPathOutput "dev" "include" (buildInputs);
}
