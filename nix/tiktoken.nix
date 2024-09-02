{ mkDerivation, base, base64, bytestring, containers, deepseq
, filepath, lib, megaparsec, pcre-light, quickcheck-instances
, raw-strings-qq, tasty, tasty-bench, tasty-quickcheck
, tasty-silver, text, unordered-containers
}:
mkDerivation {
  pname = "tiktoken";
  version = "1.0.3";
  sha256 = "46d619129e267935711131bb1bbd59a2257898c5a89013259a39cad772f2c343";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base base64 bytestring containers deepseq filepath megaparsec
    pcre-light raw-strings-qq text unordered-containers
  ];
  testHaskellDepends = [
    base bytestring quickcheck-instances tasty tasty-quickcheck
    tasty-silver text
  ];
  benchmarkHaskellDepends = [
    base bytestring deepseq filepath tasty-bench
  ];
  description = "Haskell implementation of tiktoken";
  license = lib.licenses.bsd3;
}
