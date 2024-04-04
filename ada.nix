{ config, lib, options, pkgs, ... }:

{ options.services.ada = {
    enable = lib.mkEnableOption "ada";

    openAIKeyFile = lib.mkOption {
      type = lib.types.path;
    };

    slackKeyFile = lib.mkOption {
      type = lib.types.path;
    };

    slackSigningSecretFile = lib.mkOption {
      type = lib.types.path;
    };

    store = lib.mkOption {
      type = lib.types.path;
    };

    port = lib.mkOption {
      type = lib.types.nullOr lib.types.port;

      default = null;
    };

    debug = lib.mkOption {
      type = lib.types.nullOr lib.types.bool;

      default = null;
    };
  };

  config = {
    nixpkgs.overlays = [ (import ./overlay.nix) ];

    systemd.services.ada = lib.mkIf config.services.ada.enable {
      wantedBy = [ "multi-user.target" ];

      path = [ pkgs.ada ];

      script =
        let
          options = {
            inherit (config.services.ada) port store;
          };
        in
          ''
            ada${lib.cli.toGNUCommandLineShell { } options} --openai-key "$(< ${lib.escapeShellArg config.services.ada.openAIKeyFile})" --slack-api-key "$(< ${lib.escapeShellArg config.services.ada.slackKeyFile})" --slack-signing-secret "$(< ${lib.escapeShellArg config.services.ada.slackSigningSecretFile})"
          '';
    };
  };
}
