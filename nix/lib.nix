{ lib, system, writeText }:
let updateAttrByPath = path: update: lib.updateManyAttrsByPath [{ inherit path update; }];
in
rec {
  makeBuilt = path:
    let inherit (builtins) abort getFlake;
      lock = lib.importJSON path;
      versionWithBuilt = version: {
        built =
          if version.original."static_source:type" == "git" && version.locked."static_version:type" == "git_version" then
            let
              flake = getFlake "${version.original.url}?ref=${version.original.ref}&rev=${version.locked.rev}";
              final =
                lib.attrByPath
                  version.original.attribute
                  (abort "Specified attribute not found")
                  flake.packages."${system}";
            in
            final.outPath
          else
            abort "Only Git sources are currently supported";
      };
      lockWithBuilt = lib.mapAttrs (name: map versionWithBuilt);
    in
    updateAttrByPath
      [ "locks" ]
      lockWithBuilt
      lock;

  builtToJSON = path: builtins.toJSON (makeBuilt path);
  builtToJSONFile = path: writeText "memento.built.json" (builtins.toJSON (makeBuilt path));
}
