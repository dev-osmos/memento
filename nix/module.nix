{ config, lib, pkgs, ... }:
let
  inherit (lib) mkIf mkOption types;
  cfg = config.memento;
  inherit (pkgs.callPackage ./lib.nix {}) builtStaticsToJSON;
in
{
  options.memento = {
    lockPath = mkOption {
      type = types.nullOr types.pathInStore;
      default = null;
    };
  };
  config = mkIf (cfg.lockPath != null) {
    systemd.services.memento-save-lock = {
      enable = true;
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart =
          let
            lockFile = pkgs.writeText "memento.lock.json" (builtStaticsToJSON cfg.lockPath);
          in
            pkgs.writeShellScript "memento-save-lock" ''
              mkdir -p /nix/var/nix/gcroots/memento/
              ln -sf ${lockFile} /nix/var/nix/gcroots/memento/memento.lock.json
            '';
        RemainAfterExit = "yes";
      };
    };
  };
}
