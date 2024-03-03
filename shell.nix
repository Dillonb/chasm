{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.capstone
    pkgs.ocaml
    pkgs.opam
    pkgs.bashInteractive
  ];
}
