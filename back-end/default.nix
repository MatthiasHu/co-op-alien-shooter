{ mkDerivation, stdenv, base, bytestring, containers, websockets
}:
mkDerivation {
  pname = "co-op-alien-shooter";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring containers websockets
  ];
  description = "co-op alien shooter back-end";
  license = stdenv.lib.licenses.gpl3;
}
