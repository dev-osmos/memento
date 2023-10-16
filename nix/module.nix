{ config, lib, pkgs, ... }:
let
  inherit (lib) mkIf mkOption types;
  cfg = config.memento;
  inherit (pkgs.callPackage ./lib.nix { }) builtToJSONFile;
in
{
  options.memento = {
    etc = mkOption {
      type = types.nullOr types.pathInStore;
      default = null;
      description = "Path to directory that contains `memento.json` and `memento.lock.json`";
    };
  };
  config = mkIf (cfg.etc != null) {
    systemd.services.memento-system-upgrade = {
      enable = true;
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart =
          let
            built = builtToJSONFile "${cfg.etc}/memento.lock.json";
          in
          "${pkgs.memento}/bin/mto system upgrade --new-etc ${cfg.etc} --new-built ${built}";
        RemainAfterExit = "yes";
        Restart = "no";
      };
    };
  };
}
