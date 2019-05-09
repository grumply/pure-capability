{ mkDerivation, base, mtl, pure-state, transformers, template-haskell, stdenv }:
mkDerivation {
  pname = "pure-capability";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure-state mtl transformers template-haskell ];
  homepage = "github.com/grumply/pure-capability";
  description = "A derivation approach for record-of-function–style extensible capabilities.";
  license = stdenv.lib.licenses.bsd3;
}
