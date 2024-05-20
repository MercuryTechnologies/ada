{ config, lib, options, pkgs, ... }:

{ options.services.ada = {
    enable = lib.mkEnableOption "ada";

    chat-model = lib.mkOption {
      type = lib.types.str;
    };

    embedding-model = lib.mkOption {
      type = lib.types.str;
    };

    openAIKeyFile = lib.mkOption {
      type = lib.types.path;
    };

    slackKeyFile = lib.mkOption {
      type = lib.types.path;
    };

    slackSigningSecretFile = lib.mkOption {
      type = lib.types.path;
    };

    getDXKeyFile = lib.mkOption {
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
          adaOptions = {
            inherit (config.services.ada) chat-model embedding-model store;
          };

          queryOptions = {
            inherit (config.services.ada) port debug;
          };
        in
          ''
            ada --openai-key "$(< ${lib.escapeShellArg config.services.ada.openAIKeyFile})" ${lib.cli.toGNUCommandLineShell { } adaOptions} query ${lib.cli.toGNUCommandLineShell { } queryOptions} --slack-api-key "$(< ${lib.escapeShellArg config.services.ada.slackKeyFile})" --slack-signing-secret "$(< ${lib.escapeShellArg config.services.ada.slackSigningSecretFile})" --getdx-api-key "$(< ${lib.escapeShellArg config.services.ada.getDXKeyFile})"
          '';
    };
  };
}
