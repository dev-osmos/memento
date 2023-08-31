{ config, lib, pkgs, ... }:
let
  inherit (lib) mkOption types;
  cfg = config.memento;
  inherit (import ./lib.nix) builtStaticsToJSON;
in
{
  options.memento = {
    lockPath = mkOption {
      type = types.nullOr types.package;
      default = null;
    };
  };
  config = mkIf (cfg.lockPath != null) {
    systemd.services.memento-save-lock = {
      enable = true;
      serviceConfig = {
        Type = "oneshot";
        ExecStart = pkgs.writeShellScript "memento-save-lock" ''
          mkdir -p /nix/var/nix/gcroots/memento/
          cat << EOF > /nix/var/nix/gcroots/memento/memento.lock.json
          ${builtStaticsToJSON cfg.lockPath}
          EOF
        '';
      };
    };
  };
}
