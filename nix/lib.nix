{ lib, system }:
let updateAttrByPath = path: update: lib.updateManyAttrsByPath [{ inherit path update; }];
in
rec {
  loadStatics = path:
    let inherit (builtins) abort getFlake;
        lock = lib.importJSON path;
        versionWithBuilt = version: version // {
            built =
              if version.original."static_source:type" == "git" && version.locked."static_version:type" == "git_version" then
                (getFlake "${version.original.url}?ref=${version.original.ref}&rev=${version.locked.rev}").packages."${system}"."${version.original.attribute}".outPath
              else
                abort "Only Git sources are currently supported";
          };
        lockWithBuilt = lib.mapAttrs (name: map versionWithBuilt);
    in
      updateAttrByPath
        ["locks"]
        lockWithBuilt
        lock;

  builtStaticsToJSON = path: builtins.toJSON (loadStatics path);
}
